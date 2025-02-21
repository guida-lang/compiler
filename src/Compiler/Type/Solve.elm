module Compiler.Type.Solve exposing (run)

import Array exposing (Array)
import Compiler.AST.Canonical as Can
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as Doc
import Compiler.Reporting.Error.Type as Error
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Type.Error as ET
import Compiler.Type.Occurs as Occurs
import Compiler.Type.Type as Type exposing (Constraint(..), Type, nextMark)
import Compiler.Type.Unify as Unify
import Compiler.Type.UnionFind as UF
import Data.IORef exposing (IORef)
import Data.Map as Dict exposing (Dict)
import Data.Vector as Vector
import Data.Vector.Mutable as MVector
import System.TypeCheck.IO as IO exposing (Content, Descriptor(..), IO, Mark, Variable)
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- RUN SOLVER


run : Constraint -> IO (Result (NE.Nonempty Error.Error) (Dict String Name.Name Can.Annotation))
run constraint =
    MVector.replicate 8 []
        |> IO.bind
            (\pools ->
                solve Dict.empty Type.outermostRank pools emptyState constraint
                    |> IO.bind
                        (\(State env _ errors) ->
                            case errors of
                                [] ->
                                    IO.traverseMap identity compare Type.toAnnotation env
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
    Dict String Name.Name Variable


type alias Pools =
    IORef (Array (Maybe (List Variable)))


type State
    = State Env Mark (List Error.Error)


solve : Env -> Int -> Pools -> State -> Constraint -> IO State
solve env rank pools state constraint =
    IO.loop solveHelp ( ( env, rank ), ( pools, state ), ( constraint, identity ) )


solveHelp : ( ( Env, Int ), ( Pools, State ), ( Type.Constraint, IO State -> IO State ) ) -> IO (IO.Step ( ( Env, Int ), ( Pools, State ), ( Type.Constraint, IO State -> IO State ) ) State)
solveHelp ( ( env, rank ), ( pools, (State _ sMark sErrors) as state ), ( constraint, cont ) ) =
    case constraint of
        CTrue ->
            IO.fmap IO.Done <| cont <| IO.pure state

        CSaveTheEnvironment ->
            IO.fmap IO.Done <| cont <| IO.pure (State env sMark sErrors)

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
                                                            |> IO.bind (\_ -> IO.fmap IO.Done <| cont <| IO.pure state)

                                                    Unify.AnswerErr vars actualType expectedType ->
                                                        introduce rank pools vars
                                                            |> IO.bind
                                                                (\_ ->
                                                                    IO.fmap IO.Done <|
                                                                        cont <|
                                                                            IO.pure <|
                                                                                addError state <|
                                                                                    Error.BadExpr region category actualType <|
                                                                                        Error.typeReplace expectation expectedType
                                                                )
                                            )
                                )
                    )

        CLocal region name expectation ->
            makeCopy rank pools (Utils.find identity name env)
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
                                                            |> IO.bind (\_ -> IO.fmap IO.Done <| cont <| IO.pure state)

                                                    Unify.AnswerErr vars actualType expectedType ->
                                                        introduce rank pools vars
                                                            |> IO.bind
                                                                (\_ ->
                                                                    IO.fmap IO.Done <|
                                                                        cont <|
                                                                            IO.pure <|
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
                                                            |> IO.bind (\_ -> IO.fmap IO.Done <| cont <| IO.pure state)

                                                    Unify.AnswerErr vars actualType expectedType ->
                                                        introduce rank pools vars
                                                            |> IO.bind
                                                                (\_ ->
                                                                    IO.fmap IO.Done <|
                                                                        cont <|
                                                                            IO.pure <|
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
                                                            |> IO.bind (\_ -> IO.fmap IO.Done <| cont <| IO.pure state)

                                                    Unify.AnswerErr vars actualType expectedType ->
                                                        introduce rank pools vars
                                                            |> IO.bind
                                                                (\_ ->
                                                                    IO.fmap IO.Done <|
                                                                        cont <|
                                                                            IO.pure <|
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
            IO.fmap IO.Done <| cont <| IO.foldM (solve env rank pools) state constraints

        CLet [] flexs _ headerCon CTrue ->
            introduce rank pools flexs
                |> IO.fmap (\_ -> IO.Loop ( ( env, rank ), ( pools, state ), ( headerCon, cont ) ))

        CLet [] [] header headerCon subCon ->
            solve env rank pools state headerCon
                |> IO.bind
                    (\state1 ->
                        IO.traverseMap identity compare (A.traverse (typeToVariable rank pools)) header
                            |> IO.fmap
                                (\locals ->
                                    let
                                        newEnv : Env
                                        newEnv =
                                            Dict.union env (Dict.map (\_ -> A.toValue) locals)
                                    in
                                    IO.Loop
                                        ( ( newEnv, rank )
                                        , ( pools, state1 )
                                        , ( subCon
                                          , IO.bind
                                                (\state2 ->
                                                    IO.foldM occurs state2 (Dict.toList compare locals)
                                                )
                                                >> cont
                                          )
                                        )
                                )
                    )

        CLet rigids flexs header headerCon subCon ->
            let
                -- work in the next pool to localize header
                nextRank : Int
                nextRank =
                    rank + 1
            in
            MVector.length pools
                |> IO.bind
                    (\poolsLength ->
                        (if nextRank < poolsLength then
                            IO.pure pools

                         else
                            MVector.grow pools poolsLength
                        )
                            |> IO.bind
                                (\nextPools ->
                                    let
                                        -- introduce variables
                                        vars : List Variable
                                        vars =
                                            rigids ++ flexs
                                    in
                                    IO.forM_ vars
                                        (\var ->
                                            UF.modify var <|
                                                \(Descriptor content _ mark copy) ->
                                                    Descriptor content nextRank mark copy
                                        )
                                        |> IO.bind
                                            (\_ ->
                                                MVector.write nextPools nextRank vars
                                                    |> IO.bind
                                                        (\_ ->
                                                            -- run solver in next pool
                                                            IO.traverseMap identity compare (A.traverse (typeToVariable nextRank nextPools)) header
                                                                |> IO.bind
                                                                    (\locals ->
                                                                        solve env nextRank nextPools state headerCon
                                                                            |> IO.bind
                                                                                (\(State savedEnv mark errors) ->
                                                                                    let
                                                                                        youngMark : Mark
                                                                                        youngMark =
                                                                                            mark

                                                                                        visitMark : Mark
                                                                                        visitMark =
                                                                                            nextMark youngMark

                                                                                        finalMark : Mark
                                                                                        finalMark =
                                                                                            nextMark visitMark
                                                                                    in
                                                                                    -- pop pool
                                                                                    generalize youngMark visitMark nextRank nextPools
                                                                                        |> IO.bind
                                                                                            (\_ ->
                                                                                                MVector.write nextPools nextRank []
                                                                                                    |> IO.bind
                                                                                                        (\_ ->
                                                                                                            -- check that things went well
                                                                                                            IO.mapM_ isGeneric rigids
                                                                                                                |> IO.fmap
                                                                                                                    (\_ ->
                                                                                                                        let
                                                                                                                            newEnv : Env
                                                                                                                            newEnv =
                                                                                                                                Dict.union env (Dict.map (\_ -> A.toValue) locals)

                                                                                                                            tempState : State
                                                                                                                            tempState =
                                                                                                                                State savedEnv finalMark errors
                                                                                                                        in
                                                                                                                        IO.Loop
                                                                                                                            ( ( newEnv, rank )
                                                                                                                            , ( nextPools, tempState )
                                                                                                                            , ( subCon
                                                                                                                              , IO.bind
                                                                                                                                    (\newState ->
                                                                                                                                        IO.foldM occurs newState (Dict.toList compare locals)
                                                                                                                                    )
                                                                                                                                    >> cont
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
                                        ++ Doc.toString (ET.toDoc L.empty RT.None tipe)
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
                                            UF.set variable (Descriptor IO.Error rank mark copy)
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
    MVector.read pools youngRank
        |> IO.bind
            (\youngVars ->
                poolToRankTable youngMark youngRank youngVars
                    |> IO.bind
                        (\rankTable ->
                            -- get the ranks right for each entry.
                            -- start at low ranks so that we only have to pass
                            -- over the information once.
                            Vector.imapM_
                                (\rank table ->
                                    IO.mapM_ (adjustRank youngMark visitMark rank) table
                                )
                                rankTable
                                |> IO.bind
                                    (\_ ->
                                        -- For variables that have rank lowerer than youngRank, register them in
                                        -- the appropriate old pool if they are not redundant.
                                        Vector.forM_ (Vector.unsafeInit rankTable)
                                            (\vars ->
                                                IO.forM_ vars
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
                                                                                    MVector.modify pools ((::) var) rank
                                                                                )
                                                                )
                                                    )
                                            )
                                            |> IO.bind
                                                (\_ ->
                                                    -- For variables with rank youngRank
                                                    --   If rank < youngRank: register in oldPool
                                                    --   otherwise generalize
                                                    Vector.unsafeLast rankTable
                                                        |> IO.bind
                                                            (\lastRankTable ->
                                                                IO.forM_ lastRankTable <|
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
                                                                                                        MVector.modify pools ((::) var) rank

                                                                                                    else
                                                                                                        UF.set var <| Descriptor content Type.noRank mark copy
                                                                                                )
                                                                                )
                                                            )
                                                )
                                    )
                        )
            )


poolToRankTable : Mark -> Int -> List Variable -> IO (IORef (Array (Maybe (List Variable))))
poolToRankTable youngMark youngRank youngInhabitants =
    MVector.replicate (youngRank + 1) []
        |> IO.bind
            (\mutableTable ->
                -- Sort the youngPool variables into buckets by rank.
                IO.forM_ youngInhabitants
                    (\var ->
                        UF.get var
                            |> IO.bind
                                (\(Descriptor content rank _ copy) ->
                                    UF.set var (Descriptor content rank youngMark copy)
                                        |> IO.bind
                                            (\_ ->
                                                MVector.modify mutableTable ((::) var) rank
                                            )
                                )
                    )
                    |> IO.bind (\_ -> Vector.unsafeFreeze mutableTable)
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
                        minRank : Int
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
        go : Variable -> IO Int
        go =
            adjustRank youngMark visitMark groupRank
    in
    case content of
        IO.FlexVar _ ->
            IO.pure groupRank

        IO.FlexSuper _ _ ->
            IO.pure groupRank

        IO.RigidVar _ ->
            IO.pure groupRank

        IO.RigidSuper _ _ ->
            IO.pure groupRank

        IO.Structure flatType ->
            case flatType of
                IO.App1 _ _ args ->
                    IO.foldM (\rank arg -> IO.fmap (max rank) (go arg)) Type.outermostRank args

                IO.Fun1 arg result ->
                    IO.pure max
                        |> IO.apply (go arg)
                        |> IO.apply (go result)

                IO.EmptyRecord1 ->
                    -- THEORY: an empty record never needs to get generalized
                    IO.pure Type.outermostRank

                IO.Record1 fields extension ->
                    go extension
                        |> IO.bind
                            (\extRank ->
                                IO.foldMDict compare (\rank field -> IO.fmap (max rank) (go field)) extRank fields
                            )

                IO.Unit1 ->
                    -- THEORY: a unit never needs to get generalized
                    IO.pure Type.outermostRank

                IO.Tuple1 a b maybeC ->
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

        IO.Alias _ _ args _ ->
            -- THEORY: anything in the realVar would be outermostRank
            IO.foldM (\rank ( _, argVar ) -> IO.fmap (max rank) (go argVar)) Type.outermostRank args

        IO.Error ->
            IO.pure groupRank



-- REGISTER VARIABLES


introduce : Int -> Pools -> List Variable -> IO ()
introduce rank pools variables =
    MVector.modify pools
        (\a -> variables ++ a)
        rank
        |> IO.bind
            (\_ ->
                IO.forM_ variables
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


typeToVar : Int -> Pools -> Dict String Name.Name Variable -> Type -> IO Variable
typeToVar rank pools aliasDict tipe =
    let
        go : Type -> IO Variable
        go =
            typeToVar rank pools aliasDict
    in
    case tipe of
        Type.VarN v ->
            IO.pure v

        Type.AppN home name args ->
            IO.traverseList go args
                |> IO.bind
                    (\argVars ->
                        register rank pools (IO.Structure (IO.App1 home name argVars))
                    )

        Type.FunN a b ->
            go a
                |> IO.bind
                    (\aVar ->
                        go b
                            |> IO.bind
                                (\bVar ->
                                    register rank pools (IO.Structure (IO.Fun1 aVar bVar))
                                )
                    )

        Type.AliasN home name args aliasType ->
            IO.traverseList (IO.traverseTuple go) args
                |> IO.bind
                    (\argVars ->
                        typeToVar rank pools (Dict.fromList identity argVars) aliasType
                            |> IO.bind
                                (\aliasVar ->
                                    register rank pools (IO.Alias home name argVars aliasVar)
                                )
                    )

        Type.PlaceHolder name ->
            IO.pure (Utils.find identity name aliasDict)

        Type.RecordN fields ext ->
            IO.traverseMap identity compare go fields
                |> IO.bind
                    (\fieldVars ->
                        go ext
                            |> IO.bind
                                (\extVar ->
                                    register rank pools (IO.Structure (IO.Record1 fieldVars extVar))
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
                                    IO.traverseMaybe go c
                                        |> IO.bind
                                            (\cVar ->
                                                register rank pools (IO.Structure (IO.Tuple1 aVar bVar cVar))
                                            )
                                )
                    )


register : Int -> Pools -> Content -> IO Variable
register rank pools content =
    UF.fresh (Descriptor content rank Type.noMark Nothing)
        |> IO.bind
            (\var ->
                MVector.modify pools ((::) var) rank
                    |> IO.fmap (\_ -> var)
            )


emptyRecord1 : Content
emptyRecord1 =
    IO.Structure IO.EmptyRecord1


unit1 : Content
unit1 =
    IO.Structure IO.Unit1



-- SOURCE TYPE TO VARIABLE


srcTypeToVariable : Int -> Pools -> Dict String Name.Name () -> Can.Type -> IO Variable
srcTypeToVariable rank pools freeVars srcType =
    let
        nameToContent : Name.Name -> Content
        nameToContent name =
            if Name.isNumberType name then
                IO.FlexSuper IO.Number (Just name)

            else if Name.isComparableType name then
                IO.FlexSuper IO.Comparable (Just name)

            else if Name.isAppendableType name then
                IO.FlexSuper IO.Appendable (Just name)

            else if Name.isCompappendType name then
                IO.FlexSuper IO.CompAppend (Just name)

            else
                IO.FlexVar (Just name)

        makeVar : Name.Name -> b -> IO Variable
        makeVar name _ =
            UF.fresh (Descriptor (nameToContent name) rank Type.noMark Nothing)
    in
    IO.traverseMapWithKey identity compare makeVar freeVars
        |> IO.bind
            (\flexVars ->
                MVector.modify pools (\a -> Dict.values compare flexVars ++ a) rank
                    |> IO.bind (\_ -> srcTypeToVar rank pools flexVars srcType)
            )


srcTypeToVar : Int -> Pools -> Dict String Name.Name Variable -> Can.Type -> IO Variable
srcTypeToVar rank pools flexVars srcType =
    let
        go : Can.Type -> IO Variable
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
                                    register rank pools (IO.Structure (IO.Fun1 argVar resultVar))
                                )
                    )

        Can.TVar name ->
            IO.pure (Utils.find identity name flexVars)

        Can.TType home name args ->
            IO.traverseList go args
                |> IO.bind
                    (\argVars ->
                        register rank pools (IO.Structure (IO.App1 home name argVars))
                    )

        Can.TRecord fields maybeExt ->
            IO.traverseMap identity compare (srcFieldTypeToVar rank pools flexVars) fields
                |> IO.bind
                    (\fieldVars ->
                        (case maybeExt of
                            Nothing ->
                                register rank pools emptyRecord1

                            Just ext ->
                                IO.pure (Utils.find identity ext flexVars)
                        )
                            |> IO.bind
                                (\extVar ->
                                    register rank pools (IO.Structure (IO.Record1 fieldVars extVar))
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
                                    IO.traverseMaybe go c
                                        |> IO.bind
                                            (\cVar ->
                                                register rank pools (IO.Structure (IO.Tuple1 aVar bVar cVar))
                                            )
                                )
                    )

        Can.TAlias home name args aliasType ->
            IO.traverseList (IO.traverseTuple go) args
                |> IO.bind
                    (\argVars ->
                        (case aliasType of
                            Can.Holey tipe ->
                                srcTypeToVar rank pools (Dict.fromList identity argVars) tipe

                            Can.Filled tipe ->
                                go tipe
                        )
                            |> IO.bind
                                (\aliasVar ->
                                    register rank pools (IO.Alias home name argVars aliasVar)
                                )
                    )


srcFieldTypeToVar : Int -> Pools -> Dict String Name.Name Variable -> Can.FieldType -> IO Variable
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
                                makeDescriptor : Content -> Descriptor
                                makeDescriptor c =
                                    Descriptor c maxRank Type.noMark Nothing
                            in
                            UF.fresh (makeDescriptor content)
                                |> IO.bind
                                    (\copy ->
                                        MVector.modify pools ((::) copy) maxRank
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
                                                                    IO.Structure term ->
                                                                        traverseFlatType (makeCopyHelp maxRank pools) term
                                                                            |> IO.bind
                                                                                (\newTerm ->
                                                                                    UF.set copy (makeDescriptor (IO.Structure newTerm))
                                                                                        |> IO.fmap (\_ -> copy)
                                                                                )

                                                                    IO.FlexVar _ ->
                                                                        IO.pure copy

                                                                    IO.FlexSuper _ _ ->
                                                                        IO.pure copy

                                                                    IO.RigidVar name ->
                                                                        UF.set copy (makeDescriptor (IO.FlexVar (Just name)))
                                                                            |> IO.fmap (\_ -> copy)

                                                                    IO.RigidSuper super name ->
                                                                        UF.set copy (makeDescriptor (IO.FlexSuper super (Just name)))
                                                                            |> IO.fmap (\_ -> copy)

                                                                    IO.Alias home name args realType ->
                                                                        IO.mapM (IO.traverseTuple (makeCopyHelp maxRank pools)) args
                                                                            |> IO.bind
                                                                                (\newArgs ->
                                                                                    makeCopyHelp maxRank pools realType
                                                                                        |> IO.bind
                                                                                            (\newRealType ->
                                                                                                UF.set copy (makeDescriptor (IO.Alias home name newArgs newRealType))
                                                                                                    |> IO.fmap (\_ -> copy)
                                                                                            )
                                                                                )

                                                                    IO.Error ->
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
        IO.FlexVar _ ->
            IO.pure ()

        IO.FlexSuper _ _ ->
            IO.pure ()

        IO.RigidVar _ ->
            IO.pure ()

        IO.RigidSuper _ _ ->
            IO.pure ()

        IO.Structure term ->
            case term of
                IO.App1 _ _ args ->
                    IO.mapM_ restore args

                IO.Fun1 arg result ->
                    restore arg
                        |> IO.bind (\_ -> restore result)

                IO.EmptyRecord1 ->
                    IO.pure ()

                IO.Record1 fields ext ->
                    IO.mapM_ restore (Dict.values compare fields)
                        |> IO.bind (\_ -> restore ext)

                IO.Unit1 ->
                    IO.pure ()

                IO.Tuple1 a b maybeC ->
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

        IO.Alias _ _ args var ->
            IO.mapM_ restore (List.map Tuple.second args)
                |> IO.bind (\_ -> restore var)

        IO.Error ->
            IO.pure ()



-- TRAVERSE FLAT TYPE


traverseFlatType : (Variable -> IO Variable) -> IO.FlatType -> IO IO.FlatType
traverseFlatType f flatType =
    case flatType of
        IO.App1 home name args ->
            IO.fmap (IO.App1 home name) (IO.traverseList f args)

        IO.Fun1 a b ->
            IO.pure IO.Fun1
                |> IO.apply (f a)
                |> IO.apply (f b)

        IO.EmptyRecord1 ->
            IO.pure IO.EmptyRecord1

        IO.Record1 fields ext ->
            IO.pure IO.Record1
                |> IO.apply (IO.traverseMap identity compare f fields)
                |> IO.apply (f ext)

        IO.Unit1 ->
            IO.pure IO.Unit1

        IO.Tuple1 a b cs ->
            IO.pure IO.Tuple1
                |> IO.apply (f a)
                |> IO.apply (f b)
                |> IO.apply (IO.traverseMaybe f cs)
