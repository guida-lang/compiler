module Type.Solve exposing (run)

import AST.Canonical as Can
import Array exposing (Array)
import Data.IO as IO exposing (IO)
import Data.Map as Dict exposing (Dict)
import Data.Name as Name
import Data.NonEmptyList as NE
import Elm.Kernel exposing (Chunk(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Reporting.Annotation as A
import Reporting.Error.Type as Error
import Reporting.Render.Type as RT
import Reporting.Render.Type.Localizer as L
import Type.Error as ET
import Type.Occurs as Occurs
import Type.Type as Type exposing (Constraint(..), Type, nextMark)
import Type.Unify as Unify
import Type.UnionFind as UF exposing (Content, Descriptor(..), Mark, Variable)
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- RUN SOLVER


run : Constraint -> IO (Result (NE.Nonempty Error.Error) (Dict Name.Name Can.Annotation))
run constraint =
    IO.mVectorReplicate (Encode.list UF.variableEncoder) 8 []
        |> IO.bind
            (\pools ->
                solve Dict.empty Type.outermostRank pools emptyState constraint
                    |> IO.bind
                        (\(State env _ errors) ->
                            case errors of
                                [] ->
                                    Utils.mapTraverse compare Type.toAnnotation env
                                        |> IO.fmap Ok

                                e :: es ->
                                    IO.pure (Err (NE.Nonempty e es))
                        )
            )


emptyState : State
emptyState =
    State Dict.empty (Type.nextMark Type.noMark) []



-- SOLVER


type alias Env =
    Dict Name.Name Variable


type alias Pools =
    IO.IORef (Array (Maybe (List Variable)))


type State
    = State Env Mark (List Error.Error)


solve : Env -> Int -> Pools -> State -> Constraint -> IO State
solve env rank pools ((State _ sMark sErrors) as state) constraint =
    case constraint of
        CTrue ->
            IO.pure state

        CSaveTheEnvironment ->
            IO.pure (State env sMark sErrors)

        CEqual region category tipe expectation ->
            typeToVariable rank pools tipe
                |> IO.bind
                    (\actual ->
                        expectedToVariable rank pools expectation
                            |> IO.bind
                                (\expected ->
                                    Unify.unify actual expected
                                        |> IO.bind
                                            (\answer ->
                                                case answer of
                                                    Unify.AnswerOk vars ->
                                                        introduce rank pools vars
                                                            |> IO.fmap (\_ -> state)

                                                    Unify.AnswerErr vars actualType expectedType ->
                                                        introduce rank pools vars
                                                            |> IO.fmap
                                                                (\_ ->
                                                                    addError state <|
                                                                        Error.BadExpr region category actualType <|
                                                                            Error.typeReplace expectation expectedType
                                                                )
                                            )
                                )
                    )

        CLocal region name expectation ->
            makeCopy rank pools (Utils.find name env)
                |> IO.bind
                    (\actual ->
                        expectedToVariable rank pools expectation
                            |> IO.bind
                                (\expected ->
                                    Unify.unify actual expected
                                        |> IO.bind
                                            (\answer ->
                                                case answer of
                                                    Unify.AnswerOk vars ->
                                                        introduce rank pools vars
                                                            |> IO.fmap (\_ -> state)

                                                    Unify.AnswerErr vars actualType expectedType ->
                                                        introduce rank pools vars
                                                            |> IO.fmap
                                                                (\_ ->
                                                                    addError state <|
                                                                        Error.BadExpr region (Error.Local name) actualType <|
                                                                            Error.typeReplace expectation expectedType
                                                                )
                                            )
                                )
                    )

        CForeign region name (Can.Forall freeVars srcType) expectation ->
            srcTypeToVariable rank pools freeVars srcType
                |> IO.bind
                    (\actual ->
                        expectedToVariable rank pools expectation
                            |> IO.bind
                                (\expected ->
                                    Unify.unify actual expected
                                        |> IO.bind
                                            (\answer ->
                                                case answer of
                                                    Unify.AnswerOk vars ->
                                                        introduce rank pools vars
                                                            |> IO.fmap (\_ -> state)

                                                    Unify.AnswerErr vars actualType expectedType ->
                                                        introduce rank pools vars
                                                            |> IO.fmap
                                                                (\_ ->
                                                                    addError state <|
                                                                        Error.BadExpr region (Error.Foreign name) actualType <|
                                                                            Error.typeReplace expectation expectedType
                                                                )
                                            )
                                )
                    )

        CPattern region category tipe expectation ->
            typeToVariable rank pools tipe
                |> IO.bind
                    (\actual ->
                        patternExpectationToVariable rank pools expectation
                            |> IO.bind
                                (\expected ->
                                    Unify.unify actual expected
                                        |> IO.bind
                                            (\answer ->
                                                case answer of
                                                    Unify.AnswerOk vars ->
                                                        introduce rank pools vars
                                                            |> IO.fmap (\_ -> state)

                                                    Unify.AnswerErr vars actualType expectedType ->
                                                        introduce rank pools vars
                                                            |> IO.fmap
                                                                (\_ ->
                                                                    addError state <|
                                                                        Error.BadPattern region
                                                                            category
                                                                            actualType
                                                                            (Error.ptypeReplace expectation expectedType)
                                                                )
                                            )
                                )
                    )

        CAnd constraints ->
            Utils.ioFoldM (solve env rank pools) state constraints

        CLet [] flexs _ headerCon CTrue ->
            introduce rank pools flexs
                |> IO.bind (\_ -> solve env rank pools state headerCon)

        CLet [] [] header headerCon subCon ->
            solve env rank pools state headerCon
                |> IO.bind
                    (\state1 ->
                        Utils.mapTraverse compare (A.traverse (typeToVariable rank pools)) header
                            |> IO.bind
                                (\locals ->
                                    let
                                        newEnv =
                                            Dict.union compare env (Dict.map (\_ -> A.toValue) locals)
                                    in
                                    solve newEnv rank pools state1 subCon
                                        |> IO.bind
                                            (\state2 ->
                                                Utils.ioFoldM occurs state2 (Dict.toList locals)
                                            )
                                )
                    )

        CLet rigids flexs header headerCon subCon ->
            let
                -- work in the next pool to localize header
                nextRank =
                    rank + 1
            in
            IO.mVectorLength pools
                |> IO.bind
                    (\poolsLength ->
                        (if nextRank < poolsLength then
                            IO.pure pools

                         else
                            IO.mVectorGrow (Decode.list UF.variableDecoder)
                                (Encode.list UF.variableEncoder)
                                pools
                                poolsLength
                        )
                            |> IO.bind
                                (\nextPools ->
                                    let
                                        -- introduce variables
                                        vars =
                                            rigids ++ flexs
                                    in
                                    Utils.forM_ vars
                                        (\var ->
                                            UF.modify var <|
                                                \(Descriptor content _ mark copy) ->
                                                    Descriptor content nextRank mark copy
                                        )
                                        |> IO.bind
                                            (\_ ->
                                                IO.mVectorWrite (Decode.list UF.variableDecoder) (Encode.list UF.variableEncoder) nextPools nextRank vars
                                                    |> IO.bind
                                                        (\_ ->
                                                            -- run solver in next pool
                                                            Utils.mapTraverse compare (A.traverse (typeToVariable nextRank nextPools)) header
                                                                |> IO.bind
                                                                    (\locals ->
                                                                        solve env nextRank nextPools state headerCon
                                                                            |> IO.bind
                                                                                (\(State savedEnv mark errors) ->
                                                                                    let
                                                                                        youngMark =
                                                                                            mark

                                                                                        visitMark =
                                                                                            nextMark youngMark

                                                                                        finalMark =
                                                                                            nextMark visitMark
                                                                                    in
                                                                                    -- pop pool
                                                                                    generalize youngMark visitMark nextRank nextPools
                                                                                        |> IO.bind
                                                                                            (\_ ->
                                                                                                IO.mVectorWrite (Decode.list UF.variableDecoder) (Encode.list UF.variableEncoder) nextPools nextRank []
                                                                                                    |> IO.bind
                                                                                                        (\_ ->
                                                                                                            -- check that things went well
                                                                                                            Utils.mapM_ isGeneric rigids
                                                                                                                |> IO.bind
                                                                                                                    (\_ ->
                                                                                                                        let
                                                                                                                            newEnv =
                                                                                                                                Dict.union compare env (Dict.map (\_ -> A.toValue) locals)

                                                                                                                            tempState =
                                                                                                                                State savedEnv finalMark errors
                                                                                                                        in
                                                                                                                        solve newEnv rank nextPools tempState subCon
                                                                                                                            |> IO.bind
                                                                                                                                (\newState ->
                                                                                                                                    Utils.ioFoldM occurs newState (Dict.toList locals)
                                                                                                                                )
                                                                                                                    )
                                                                                                        )
                                                                                            )
                                                                                )
                                                                    )
                                                        )
                                            )
                                )
                    )



-- Check that a variable has rank == noRank, meaning that it can be generalized.


isGeneric : Variable -> IO ()
isGeneric var =
    UF.get var
        |> IO.bind
            (\(Descriptor _ rank _ _) ->
                if rank == Type.noRank then
                    IO.pure ()

                else
                    Type.toErrorType var
                        |> IO.bind
                            (\tipe ->
                                crash <|
                                    "You ran into a compiler bug. Here are some details for the developers:\n\n"
                                        ++ "    "
                                        ++ Debug.toString (ET.toDoc L.empty RT.None tipe)
                                        ++ " [rank = "
                                        ++ String.fromInt rank
                                        ++ "]\n\n"
                                        ++ "Please create an <http://sscce.org/> and then report it\nat <https://github.com/elm/compiler/issues>\n\n"
                            )
            )



-- EXPECTATIONS TO VARIABLE


expectedToVariable : Int -> Pools -> Error.Expected Type -> IO Variable
expectedToVariable rank pools expectation =
    typeToVariable rank pools <|
        case expectation of
            Error.NoExpectation tipe ->
                tipe

            Error.FromContext _ _ tipe ->
                tipe

            Error.FromAnnotation _ _ _ tipe ->
                tipe


patternExpectationToVariable : Int -> Pools -> Error.PExpected Type -> IO Variable
patternExpectationToVariable rank pools expectation =
    typeToVariable rank pools <|
        case expectation of
            Error.PNoExpectation tipe ->
                tipe

            Error.PFromContext _ _ tipe ->
                tipe



-- ERROR HELPERS


addError : State -> Error.Error -> State
addError (State savedEnv rank errors) err =
    State savedEnv rank (err :: errors)



-- OCCURS CHECK


occurs : State -> ( Name.Name, A.Located Variable ) -> IO State
occurs state ( name, A.At region variable ) =
    Occurs.occurs variable
        |> IO.bind
            (\hasOccurred ->
                if hasOccurred then
                    Type.toErrorType variable
                        |> IO.bind
                            (\errorType ->
                                UF.get variable
                                    |> IO.bind
                                        (\(Descriptor _ rank mark copy) ->
                                            UF.set variable (Descriptor UF.Error rank mark copy)
                                                |> IO.fmap (\_ -> addError state (Error.InfiniteType region name errorType))
                                        )
                            )

                else
                    IO.pure state
            )



-- GENERALIZE


{-| Every variable has rank less than or equal to the maxRank of the pool.
This sorts variables into the young and old pools accordingly.
-}
generalize : Mark -> Mark -> Int -> Pools -> IO ()
generalize youngMark visitMark youngRank pools =
    IO.mVectorRead (Decode.list UF.variableDecoder) pools youngRank
        |> IO.bind
            (\youngVars ->
                poolToRankTable youngMark youngRank youngVars
                    |> IO.bind
                        (\rankTable ->
                            -- get the ranks right for each entry.
                            -- start at low ranks so that we only have to pass
                            -- over the information once.
                            IO.vectorImapM_ (Decode.list UF.variableDecoder)
                                (\rank table ->
                                    Utils.mapM_ (adjustRank youngMark visitMark rank) table
                                )
                                rankTable
                                |> IO.bind
                                    (\_ ->
                                        -- For variables that have rank lowerer than youngRank, register them in
                                        -- the appropriate old pool if they are not redundant.
                                        IO.vectorForM_ (Decode.list UF.variableDecoder)
                                            (IO.vectorUnsafeInit rankTable)
                                            (\vars ->
                                                Utils.forM_ vars
                                                    (\var ->
                                                        UF.redundant var
                                                            |> IO.bind
                                                                (\isRedundant ->
                                                                    if isRedundant then
                                                                        IO.pure ()

                                                                    else
                                                                        UF.get var
                                                                            |> IO.bind
                                                                                (\(Descriptor _ rank _ _) ->
                                                                                    IO.mVectorModify (Decode.list UF.variableDecoder) (Encode.list UF.variableEncoder) pools ((::) var) rank
                                                                                )
                                                                )
                                                    )
                                            )
                                            |> IO.bind
                                                (\_ ->
                                                    -- For variables with rank youngRank
                                                    --   If rank < youngRank: register in oldPool
                                                    --   otherwise generalize
                                                    IO.vectorUnsafeLast (Decode.list UF.variableDecoder) rankTable
                                                        |> IO.bind
                                                            (\lastRankTable ->
                                                                Utils.forM_ lastRankTable <|
                                                                    \var ->
                                                                        UF.redundant var
                                                                            |> IO.bind
                                                                                (\isRedundant ->
                                                                                    if isRedundant then
                                                                                        IO.pure ()

                                                                                    else
                                                                                        UF.get var
                                                                                            |> IO.bind
                                                                                                (\(Descriptor content rank mark copy) ->
                                                                                                    if rank < youngRank then
                                                                                                        IO.mVectorModify (Decode.list UF.variableDecoder) (Encode.list UF.variableEncoder) pools ((::) var) rank

                                                                                                    else
                                                                                                        UF.set var <| Descriptor content Type.noRank mark copy
                                                                                                )
                                                                                )
                                                            )
                                                )
                                    )
                        )
            )


poolToRankTable : Mark -> Int -> List Variable -> IO (IO.IORef (Array (Maybe (List Variable))))
poolToRankTable youngMark youngRank youngInhabitants =
    IO.mVectorReplicate (Encode.list UF.variableEncoder) (youngRank + 1) []
        |> IO.bind
            (\mutableTable ->
                -- Sort the youngPool variables into buckets by rank.
                Utils.forM_ youngInhabitants
                    (\var ->
                        UF.get var
                            |> IO.bind
                                (\(Descriptor content rank _ copy) ->
                                    UF.set var (Descriptor content rank youngMark copy)
                                        |> IO.bind
                                            (\_ ->
                                                IO.mVectorModify (Decode.list UF.variableDecoder) (Encode.list UF.variableEncoder) mutableTable ((::) var) rank
                                            )
                                )
                    )
                    |> IO.bind (\_ -> IO.vectorUnsafeFreeze mutableTable)
            )



-- ADJUST RANK
--
-- Adjust variable ranks such that ranks never increase as you move deeper.
-- This way the outermost rank is representative of the entire structure.
--


adjustRank : Mark -> Mark -> Int -> Variable -> IO Int
adjustRank youngMark visitMark groupRank var =
    UF.get var
        |> IO.bind
            (\(Descriptor content rank mark copy) ->
                if mark == youngMark then
                    -- Set the variable as marked first because it may be cyclic.
                    UF.set var (Descriptor content rank visitMark copy)
                        |> IO.bind
                            (\_ ->
                                adjustRankContent youngMark visitMark groupRank content
                                    |> IO.bind
                                        (\maxRank ->
                                            UF.set var (Descriptor content maxRank visitMark copy)
                                                |> IO.fmap (\_ -> maxRank)
                                        )
                            )

                else if mark == visitMark then
                    IO.pure rank

                else
                    let
                        minRank =
                            min groupRank rank
                    in
                    -- TODO how can minRank ever be groupRank?
                    UF.set var (Descriptor content minRank visitMark copy)
                        |> IO.fmap (\_ -> minRank)
            )


adjustRankContent : Mark -> Mark -> Int -> Content -> IO Int
adjustRankContent youngMark visitMark groupRank content =
    let
        go =
            adjustRank youngMark visitMark groupRank
    in
    case content of
        UF.FlexVar _ ->
            IO.pure groupRank

        UF.FlexSuper _ _ ->
            IO.pure groupRank

        UF.RigidVar _ ->
            IO.pure groupRank

        UF.RigidSuper _ _ ->
            IO.pure groupRank

        UF.Structure flatType ->
            case flatType of
                UF.App1 _ _ args ->
                    Utils.ioFoldM (\rank arg -> IO.fmap (max rank) (go arg)) Type.outermostRank args

                UF.Fun1 arg result ->
                    IO.pure max
                        |> IO.apply (go arg)
                        |> IO.apply (go result)

                UF.EmptyRecord1 ->
                    -- THEORY: an empty record never needs to get generalized
                    IO.pure Type.outermostRank

                UF.Record1 fields extension ->
                    go extension
                        |> IO.bind
                            (\extRank ->
                                Utils.ioDictFoldM (\rank field -> IO.fmap (max rank) (go field)) extRank fields
                            )

                UF.Unit1 ->
                    -- THEORY: a unit never needs to get generalized
                    IO.pure Type.outermostRank

                UF.Tuple1 a b maybeC ->
                    go a
                        |> IO.bind
                            (\ma ->
                                go b
                                    |> IO.bind
                                        (\mb ->
                                            case maybeC of
                                                Nothing ->
                                                    IO.pure (max ma mb)

                                                Just c ->
                                                    go c
                                                        |> IO.fmap (max (max ma mb))
                                        )
                            )

        UF.Alias _ _ args _ ->
            -- THEORY: anything in the realVar would be outermostRank
            Utils.ioFoldM (\rank ( _, argVar ) -> IO.fmap (max rank) (go argVar)) Type.outermostRank args

        UF.Error ->
            IO.pure groupRank



-- REGISTER VARIABLES


introduce : Int -> Pools -> List Variable -> IO ()
introduce rank pools variables =
    IO.mVectorModify
        (Decode.list UF.variableDecoder)
        (Encode.list UF.variableEncoder)
        pools
        (\a -> variables ++ a)
        rank
        |> IO.bind
            (\_ ->
                Utils.forM_ variables
                    (\var ->
                        UF.modify var <|
                            \(Descriptor content _ mark copy) ->
                                Descriptor content rank mark copy
                    )
            )



-- TYPE TO VARIABLE


typeToVariable : Int -> Pools -> Type -> IO Variable
typeToVariable rank pools tipe =
    typeToVar rank pools Dict.empty tipe



-- PERF working with @mgriffith we noticed that a 784 line entry in a `let` was
-- causing a ~1.5 second slowdown. Moving it to the top-level to be a function
-- saved all that time. The slowdown seems to manifest in `typeToVar` and in
-- `register` in particular. Have not explored further yet. Top-level definitions
-- are recommended in cases like this anyway, so there is at least a safety
-- valve for now.
--


typeToVar : Int -> Pools -> Dict Name.Name Variable -> Type -> IO Variable
typeToVar rank pools aliasDict tipe =
    let
        go =
            typeToVar rank pools aliasDict
    in
    case tipe of
        Type.VarN v ->
            IO.pure v

        Type.AppN home name args ->
            Utils.listTraverse go args
                |> IO.bind
                    (\argVars ->
                        register rank pools (UF.Structure (UF.App1 home name argVars))
                    )

        Type.FunN a b ->
            go a
                |> IO.bind
                    (\aVar ->
                        go b
                            |> IO.bind
                                (\bVar ->
                                    register rank pools (UF.Structure (UF.Fun1 aVar bVar))
                                )
                    )

        Type.AliasN home name args aliasType ->
            Utils.listTraverse (Utils.tupleTraverse go) args
                |> IO.bind
                    (\argVars ->
                        typeToVar rank pools (Dict.fromList compare argVars) aliasType
                            |> IO.bind
                                (\aliasVar ->
                                    register rank pools (UF.Alias home name argVars aliasVar)
                                )
                    )

        Type.PlaceHolder name ->
            IO.pure (Utils.find name aliasDict)

        Type.RecordN fields ext ->
            Utils.mapTraverse compare go fields
                |> IO.bind
                    (\fieldVars ->
                        go ext
                            |> IO.bind
                                (\extVar ->
                                    register rank pools (UF.Structure (UF.Record1 fieldVars extVar))
                                )
                    )

        Type.EmptyRecordN ->
            register rank pools emptyRecord1

        Type.UnitN ->
            register rank pools unit1

        Type.TupleN a b c ->
            go a
                |> IO.bind
                    (\aVar ->
                        go b
                            |> IO.bind
                                (\bVar ->
                                    Utils.maybeTraverse go c
                                        |> IO.bind
                                            (\cVar ->
                                                register rank pools (UF.Structure (UF.Tuple1 aVar bVar cVar))
                                            )
                                )
                    )


register : Int -> Pools -> Content -> IO Variable
register rank pools content =
    UF.fresh (Descriptor content rank Type.noMark Nothing)
        |> IO.bind
            (\var ->
                IO.mVectorModify (Decode.list UF.variableDecoder) (Encode.list UF.variableEncoder) pools ((::) var) rank
                    |> IO.fmap (\_ -> var)
            )


emptyRecord1 : Content
emptyRecord1 =
    UF.Structure UF.EmptyRecord1


unit1 : Content
unit1 =
    UF.Structure UF.Unit1



-- SOURCE TYPE TO VARIABLE


srcTypeToVariable : Int -> Pools -> Dict Name.Name () -> Can.Type -> IO Variable
srcTypeToVariable rank pools freeVars srcType =
    let
        nameToContent name =
            if Name.isNumberType name then
                UF.FlexSuper UF.Number (Just name)

            else if Name.isComparableType name then
                UF.FlexSuper UF.Comparable (Just name)

            else if Name.isAppendableType name then
                UF.FlexSuper UF.Appendable (Just name)

            else if Name.isCompappendType name then
                UF.FlexSuper UF.CompAppend (Just name)

            else
                UF.FlexVar (Just name)

        makeVar name _ =
            UF.fresh (Descriptor (nameToContent name) rank Type.noMark Nothing)
    in
    Utils.mapTraverseWithKey compare makeVar freeVars
        |> IO.bind
            (\flexVars ->
                IO.mVectorModify (Decode.list UF.variableDecoder) (Encode.list UF.variableEncoder) pools (\a -> Dict.values flexVars ++ a) rank
                    |> IO.bind (\_ -> srcTypeToVar rank pools flexVars srcType)
            )


srcTypeToVar : Int -> Pools -> Dict Name.Name Variable -> Can.Type -> IO Variable
srcTypeToVar rank pools flexVars srcType =
    let
        go =
            srcTypeToVar rank pools flexVars
    in
    case srcType of
        Can.TLambda argument result ->
            go argument
                |> IO.bind
                    (\argVar ->
                        go result
                            |> IO.bind
                                (\resultVar ->
                                    register rank pools (UF.Structure (UF.Fun1 argVar resultVar))
                                )
                    )

        Can.TVar name ->
            IO.pure (Utils.find name flexVars)

        Can.TType home name args ->
            Utils.listTraverse go args
                |> IO.bind
                    (\argVars ->
                        register rank pools (UF.Structure (UF.App1 home name argVars))
                    )

        Can.TRecord fields maybeExt ->
            Utils.mapTraverse compare (srcFieldTypeToVar rank pools flexVars) fields
                |> IO.bind
                    (\fieldVars ->
                        (case maybeExt of
                            Nothing ->
                                register rank pools emptyRecord1

                            Just ext ->
                                IO.pure (Utils.find ext flexVars)
                        )
                            |> IO.bind
                                (\extVar ->
                                    register rank pools (UF.Structure (UF.Record1 fieldVars extVar))
                                )
                    )

        Can.TUnit ->
            register rank pools unit1

        Can.TTuple a b c ->
            go a
                |> IO.bind
                    (\aVar ->
                        go b
                            |> IO.bind
                                (\bVar ->
                                    Utils.maybeTraverse go c
                                        |> IO.bind
                                            (\cVar ->
                                                register rank pools (UF.Structure (UF.Tuple1 aVar bVar cVar))
                                            )
                                )
                    )

        Can.TAlias home name args aliasType ->
            Utils.listTraverse (Utils.tupleTraverse go) args
                |> IO.bind
                    (\argVars ->
                        (case aliasType of
                            Can.Holey tipe ->
                                srcTypeToVar rank pools (Dict.fromList compare argVars) tipe

                            Can.Filled tipe ->
                                go tipe
                        )
                            |> IO.bind
                                (\aliasVar ->
                                    register rank pools (UF.Alias home name argVars aliasVar)
                                )
                    )


srcFieldTypeToVar : Int -> Pools -> Dict Name.Name Variable -> Can.FieldType -> IO Variable
srcFieldTypeToVar rank pools flexVars (Can.FieldType _ srcTipe) =
    srcTypeToVar rank pools flexVars srcTipe



-- COPY


makeCopy : Int -> Pools -> Variable -> IO Variable
makeCopy rank pools var =
    makeCopyHelp rank pools var
        |> IO.bind
            (\copy ->
                restore var
                    |> IO.fmap (\_ -> copy)
            )


makeCopyHelp : Int -> Pools -> Variable -> IO Variable
makeCopyHelp maxRank pools variable =
    UF.get variable
        |> IO.bind
            (\(Descriptor content rank _ maybeCopy) ->
                case maybeCopy of
                    Just copy ->
                        IO.pure copy

                    Nothing ->
                        if rank /= Type.noRank then
                            IO.pure variable

                        else
                            let
                                makeDescriptor c =
                                    Descriptor c maxRank Type.noMark Nothing
                            in
                            UF.fresh (makeDescriptor content)
                                |> IO.bind
                                    (\copy ->
                                        IO.mVectorModify (Decode.list UF.variableDecoder) (Encode.list UF.variableEncoder) pools ((::) copy) maxRank
                                            |> IO.bind
                                                (\_ ->
                                                    -- Link the original variable to the new variable. This lets us
                                                    -- avoid making multiple copies of the variable we are instantiating.
                                                    --
                                                    -- Need to do this before recursively copying to avoid looping.
                                                    UF.set variable (Descriptor content rank Type.noMark (Just copy))
                                                        |> IO.bind
                                                            (\_ ->
                                                                -- Now we recursively copy the content of the variable.
                                                                -- We have already marked the variable as copied, so we
                                                                -- will not repeat this work or crawl this variable again.
                                                                case content of
                                                                    UF.Structure term ->
                                                                        traverseFlatType (makeCopyHelp maxRank pools) term
                                                                            |> IO.bind
                                                                                (\newTerm ->
                                                                                    UF.set copy (makeDescriptor (UF.Structure newTerm))
                                                                                        |> IO.fmap (\_ -> copy)
                                                                                )

                                                                    UF.FlexVar _ ->
                                                                        IO.pure copy

                                                                    UF.FlexSuper _ _ ->
                                                                        IO.pure copy

                                                                    UF.RigidVar name ->
                                                                        UF.set copy (makeDescriptor (UF.FlexVar (Just name)))
                                                                            |> IO.fmap (\_ -> copy)

                                                                    UF.RigidSuper super name ->
                                                                        UF.set copy (makeDescriptor (UF.FlexSuper super (Just name)))
                                                                            |> IO.fmap (\_ -> copy)

                                                                    UF.Alias home name args realType ->
                                                                        Utils.mapM (Utils.tupleTraverse (makeCopyHelp maxRank pools)) args
                                                                            |> IO.bind
                                                                                (\newArgs ->
                                                                                    makeCopyHelp maxRank pools realType
                                                                                        |> IO.bind
                                                                                            (\newRealType ->
                                                                                                UF.set copy (makeDescriptor (UF.Alias home name newArgs newRealType))
                                                                                                    |> IO.fmap (\_ -> copy)
                                                                                            )
                                                                                )

                                                                    UF.Error ->
                                                                        IO.pure copy
                                                            )
                                                )
                                    )
            )



-- RESTORE


restore : Variable -> IO ()
restore variable =
    UF.get variable
        |> IO.bind
            (\(Descriptor content _ _ maybeCopy) ->
                case maybeCopy of
                    Nothing ->
                        IO.pure ()

                    Just _ ->
                        UF.set variable (Descriptor content Type.noRank Type.noMark Nothing)
                            |> IO.bind (\_ -> restoreContent content)
            )


restoreContent : Content -> IO ()
restoreContent content =
    case content of
        UF.FlexVar _ ->
            IO.pure ()

        UF.FlexSuper _ _ ->
            IO.pure ()

        UF.RigidVar _ ->
            IO.pure ()

        UF.RigidSuper _ _ ->
            IO.pure ()

        UF.Structure term ->
            case term of
                UF.App1 _ _ args ->
                    Utils.mapM_ restore args

                UF.Fun1 arg result ->
                    restore arg
                        |> IO.bind (\_ -> restore result)

                UF.EmptyRecord1 ->
                    IO.pure ()

                UF.Record1 fields ext ->
                    Utils.mapM_ restore (Dict.values fields)
                        |> IO.bind (\_ -> restore ext)

                UF.Unit1 ->
                    IO.pure ()

                UF.Tuple1 a b maybeC ->
                    restore a
                        |> IO.bind (\_ -> restore b)
                        |> IO.bind
                            (\_ ->
                                case maybeC of
                                    Nothing ->
                                        IO.pure ()

                                    Just c ->
                                        restore c
                            )

        UF.Alias _ _ args var ->
            Utils.mapM_ restore (List.map Tuple.second args)
                |> IO.bind (\_ -> restore var)

        UF.Error ->
            IO.pure ()



-- TRAVERSE FLAT TYPE


traverseFlatType : (Variable -> IO Variable) -> UF.FlatType -> IO UF.FlatType
traverseFlatType f flatType =
    case flatType of
        UF.App1 home name args ->
            IO.fmap (UF.App1 home name) (Utils.listTraverse f args)

        UF.Fun1 a b ->
            IO.pure UF.Fun1
                |> IO.apply (f a)
                |> IO.apply (f b)

        UF.EmptyRecord1 ->
            IO.pure UF.EmptyRecord1

        UF.Record1 fields ext ->
            IO.pure UF.Record1
                |> IO.apply (Utils.mapTraverse compare f fields)
                |> IO.apply (f ext)

        UF.Unit1 ->
            IO.pure UF.Unit1

        UF.Tuple1 a b cs ->
            IO.pure UF.Tuple1
                |> IO.apply (f a)
                |> IO.apply (f b)
                |> IO.apply (Utils.maybeTraverse f cs)
