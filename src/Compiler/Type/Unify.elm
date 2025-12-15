module Compiler.Type.Unify exposing
    ( Answer(..)
    , unify
    )

import Compiler.Data.Name as Name
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Type.Error as Error
import Compiler.Type.Occurs as Occurs
import Compiler.Type.Type as Type
import Compiler.Type.UnionFind as UF
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)
import Utils.Main as Utils



-- UNIFY


type Answer
    = AnswerOk (List IO.Variable)
    | AnswerErr (List IO.Variable) Error.Type Error.Type


unify : Target -> IO.Variable -> IO.Variable -> IO Answer
unify target v1 v2 =
    case guardedUnify target v1 v2 of
        Unify k ->
            k []
                |> IO.bind
                    (\result ->
                        case result of
                            Ok (UnifyOk vars ()) ->
                                onSuccess vars ()

                            Err (UnifyErr vars ()) ->
                                Type.toErrorType v1
                                    |> IO.bind
                                        (\t1 ->
                                            Type.toErrorType v2
                                                |> IO.bind
                                                    (\t2 ->
                                                        UF.union v1 v2 errorDescriptor
                                                            |> IO.fmap (\_ -> AnswerErr vars t1 t2)
                                                    )
                                        )
                    )


onSuccess : List IO.Variable -> () -> IO Answer
onSuccess vars () =
    IO.pure (AnswerOk vars)


errorDescriptor : IO.Descriptor
errorDescriptor =
    IO.Descriptor IO.Error Type.noRank Type.noMark Nothing



-- CPS UNIFIER


type Unify a
    = Unify (List IO.Variable -> IO (Result UnifyErr (UnifyOk a)))


type UnifyOk a
    = UnifyOk (List IO.Variable) a


type UnifyErr
    = UnifyErr (List IO.Variable) ()


fmap : (a -> b) -> Unify a -> Unify b
fmap func (Unify kv) =
    Unify <|
        \vars ->
            IO.fmap
                (Result.map
                    (\(UnifyOk vars1 value) ->
                        UnifyOk vars1 (func value)
                    )
                )
                (kv vars)


pure : a -> Unify a
pure a =
    Unify (\vars -> IO.pure (Ok (UnifyOk vars a)))


bind : (a -> Unify b) -> Unify a -> Unify b
bind callback (Unify ka) =
    Unify <|
        \vars ->
            IO.bind
                (\result ->
                    case result of
                        Ok (UnifyOk vars1 a) ->
                            case callback a of
                                Unify kb ->
                                    kb vars1

                        Err err ->
                            IO.pure (Err err)
                )
                (ka vars)


register : IO IO.Variable -> Unify IO.Variable
register mkVar =
    Unify
        (\vars ->
            IO.fmap
                (\var ->
                    Ok (UnifyOk (var :: vars) var)
                )
                mkVar
        )


mismatch : Unify a
mismatch =
    Unify (\vars -> IO.pure (Err (UnifyErr vars ())))



-- UNIFICATION HELPERS


type Context
    = Context IO.Variable IO.Descriptor IO.Variable IO.Descriptor


reorient : Context -> Context
reorient (Context var1 desc1 var2 desc2) =
    Context var2 desc2 var1 desc1



-- MERGE
-- merge : Context -> UF.Content -> Unify ( UF.Point UF.Descriptor, UF.Point UF.Descriptor )


merge : Context -> IO.Content -> Unify ()
merge (Context var1 (IO.Descriptor _ rank1 _ _) var2 (IO.Descriptor _ rank2 _ _)) content =
    Unify
        (\vars ->
            UF.union var1 var2 (IO.Descriptor content (min rank1 rank2) Type.noMark Nothing)
                |> IO.fmap (Ok << UnifyOk vars)
        )


fresh : Context -> IO.Content -> Unify IO.Variable
fresh (Context _ (IO.Descriptor _ rank1 _ _) _ (IO.Descriptor _ rank2 _ _)) content =
    register <|
        UF.fresh <|
            IO.Descriptor content (min rank1 rank2) Type.noMark Nothing



-- ACTUALLY UNIFY THINGS


guardedUnify : Target -> IO.Variable -> IO.Variable -> Unify ()
guardedUnify target left right =
    Unify
        (\vars ->
            UF.equivalent left right
                |> IO.bind
                    (\equivalent ->
                        if equivalent then
                            IO.pure (Ok (UnifyOk vars ()))

                        else
                            UF.get left
                                |> IO.bind
                                    (\leftDesc ->
                                        UF.get right
                                            |> IO.bind
                                                (\rightDesc ->
                                                    case actuallyUnify target (Context left leftDesc right rightDesc) of
                                                        Unify k ->
                                                            k vars
                                                )
                                    )
                    )
        )


subUnify : Target -> IO.Variable -> IO.Variable -> Unify ()
subUnify target var1 var2 =
    guardedUnify target var1 var2


subUnifyTuple : Target -> List IO.Variable -> List IO.Variable -> Context -> IO.Content -> Unify ()
subUnifyTuple target cs zs context otherContent =
    case ( cs, zs ) of
        ( [], [] ) ->
            merge context otherContent

        ( c :: restCs, z :: restZs ) ->
            subUnify target c z
                |> bind (\_ -> subUnifyTuple target restCs restZs context otherContent)

        _ ->
            mismatch


actuallyUnify : Target -> Context -> Unify ()
actuallyUnify target ((Context _ (IO.Descriptor firstContent _ _ _) _ (IO.Descriptor secondContent _ _ _)) as context) =
    case firstContent of
        IO.FlexVar _ ->
            unifyFlex context firstContent secondContent

        IO.FlexSuper super _ ->
            unifyFlexSuper target context super firstContent secondContent

        IO.RigidVar _ ->
            unifyRigid context Nothing firstContent secondContent

        IO.RigidSuper super _ ->
            unifyRigid context (Just super) firstContent secondContent

        IO.Alias home name args realVar ->
            unifyAlias target context home name args realVar secondContent

        IO.Structure flatType ->
            unifyStructure target context flatType firstContent secondContent

        IO.Error ->
            -- If there was an error, just pretend it is okay. This lets us avoid
            -- "cascading" errors where one problem manifests as multiple message.
            merge context IO.Error



-- UNIFY FLEXIBLE VARIABLES


unifyFlex : Context -> IO.Content -> IO.Content -> Unify ()
unifyFlex context content otherContent =
    case otherContent of
        IO.Error ->
            merge context IO.Error

        IO.FlexVar maybeName ->
            merge context <|
                case maybeName of
                    Nothing ->
                        content

                    Just _ ->
                        otherContent

        IO.FlexSuper _ _ ->
            merge context otherContent

        IO.RigidVar _ ->
            merge context otherContent

        IO.RigidSuper _ _ ->
            merge context otherContent

        IO.Alias _ _ _ _ ->
            merge context otherContent

        IO.Structure _ ->
            merge context otherContent



-- UNIFY RIGID VARIABLES


unifyRigid : Context -> Maybe IO.SuperType -> IO.Content -> IO.Content -> Unify ()
unifyRigid context maybeSuper content otherContent =
    case otherContent of
        IO.FlexVar _ ->
            merge context content

        IO.FlexSuper otherSuper _ ->
            case maybeSuper of
                Just super ->
                    if combineRigidSupers super otherSuper then
                        merge context content

                    else
                        mismatch

                Nothing ->
                    mismatch

        IO.RigidVar _ ->
            mismatch

        IO.RigidSuper _ _ ->
            mismatch

        IO.Alias _ _ _ _ ->
            mismatch

        IO.Structure _ ->
            mismatch

        IO.Error ->
            merge context IO.Error



-- UNIFY SUPER VARIABLES


unifyFlexSuper : Target -> Context -> IO.SuperType -> IO.Content -> IO.Content -> Unify ()
unifyFlexSuper target ((Context first _ _ _) as context) super content otherContent =
    case otherContent of
        IO.Structure flatType ->
            unifyFlexSuperStructure target context super flatType

        IO.RigidVar _ ->
            mismatch

        IO.RigidSuper otherSuper _ ->
            if combineRigidSupers otherSuper super then
                merge context otherContent

            else
                mismatch

        IO.FlexVar _ ->
            merge context content

        IO.FlexSuper otherSuper _ ->
            case super of
                IO.Number ->
                    case otherSuper of
                        IO.Number ->
                            merge context content

                        IO.Comparable ->
                            merge context content

                        IO.Appendable ->
                            mismatch

                        IO.CompAppend ->
                            mismatch

                IO.Comparable ->
                    case otherSuper of
                        IO.Comparable ->
                            merge context otherContent

                        IO.Number ->
                            merge context otherContent

                        IO.Appendable ->
                            merge context <| Type.unnamedFlexSuper IO.CompAppend

                        IO.CompAppend ->
                            merge context otherContent

                IO.Appendable ->
                    case otherSuper of
                        IO.Appendable ->
                            merge context otherContent

                        IO.Comparable ->
                            merge context <| Type.unnamedFlexSuper IO.CompAppend

                        IO.CompAppend ->
                            merge context otherContent

                        IO.Number ->
                            mismatch

                IO.CompAppend ->
                    case otherSuper of
                        IO.Comparable ->
                            merge context content

                        IO.Appendable ->
                            merge context content

                        IO.CompAppend ->
                            merge context content

                        IO.Number ->
                            mismatch

        IO.Alias _ _ _ realVar ->
            subUnify target first realVar

        IO.Error ->
            merge context IO.Error


combineRigidSupers : IO.SuperType -> IO.SuperType -> Bool
combineRigidSupers rigid flex =
    rigid
        == flex
        || (rigid == IO.Number && flex == IO.Comparable)
        || (rigid == IO.CompAppend && (flex == IO.Comparable || flex == IO.Appendable))


atomMatchesSuper : Target -> IO.SuperType -> IO.Canonical -> Name.Name -> Bool
atomMatchesSuper target super home name =
    case super of
        IO.Number ->
            isNumber target home name

        IO.Comparable ->
            isNumber target home name || Error.isString target home name || Error.isChar target home name

        IO.Appendable ->
            Error.isString target home name

        IO.CompAppend ->
            Error.isString target home name


isNumber : Target -> IO.Canonical -> Name.Name -> Bool
isNumber target home name =
    (home == ModuleName.basics target)
        && (name == Name.int || name == Name.float)


unifyFlexSuperStructure : Target -> Context -> IO.SuperType -> IO.FlatType -> Unify ()
unifyFlexSuperStructure target context super flatType =
    case flatType of
        IO.App1 home name [] ->
            if atomMatchesSuper target super home name then
                merge context (IO.Structure flatType)

            else
                mismatch

        IO.App1 home name [ variable ] ->
            if (home == ModuleName.list target) && name == Name.list then
                case super of
                    IO.Number ->
                        mismatch

                    IO.Appendable ->
                        merge context (IO.Structure flatType)

                    IO.Comparable ->
                        comparableOccursCheck context
                            |> bind (\_ -> unifyComparableRecursive target variable)
                            |> bind (\_ -> merge context (IO.Structure flatType))

                    IO.CompAppend ->
                        comparableOccursCheck context
                            |> bind (\_ -> unifyComparableRecursive target variable)
                            |> bind (\_ -> merge context (IO.Structure flatType))

            else
                mismatch

        IO.Tuple1 a b cs ->
            case super of
                IO.Number ->
                    mismatch

                IO.Appendable ->
                    mismatch

                IO.Comparable ->
                    List.foldl (\var _ -> unifyComparableRecursive target var) (comparableOccursCheck context) (a :: b :: cs)
                        |> bind (\_ -> merge context (IO.Structure flatType))

                IO.CompAppend ->
                    mismatch

        _ ->
            mismatch



-- TODO: is there some way to avoid doing this?
-- Do type classes require occurs checks?


comparableOccursCheck : Context -> Unify ()
comparableOccursCheck (Context _ _ var _) =
    Unify
        (\vars ->
            Occurs.occurs var
                |> IO.fmap
                    (\hasOccurred ->
                        if hasOccurred then
                            Err (UnifyErr vars ())

                        else
                            Ok (UnifyOk vars ())
                    )
        )


unifyComparableRecursive : Target -> IO.Variable -> Unify ()
unifyComparableRecursive target var =
    register
        (UF.get var
            |> IO.bind
                (\(IO.Descriptor _ rank _ _) ->
                    UF.fresh (IO.Descriptor (Type.unnamedFlexSuper IO.Comparable) rank Type.noMark Nothing)
                )
        )
        |> bind (\compVar -> guardedUnify target compVar var)



-- UNIFY ALIASES


unifyAlias : Target -> Context -> IO.Canonical -> Name.Name -> List ( Name.Name, IO.Variable ) -> IO.Variable -> IO.Content -> Unify ()
unifyAlias target ((Context _ _ second _) as context) home name args realVar otherContent =
    case otherContent of
        IO.FlexVar _ ->
            merge context (IO.Alias home name args realVar)

        IO.FlexSuper _ _ ->
            subUnify target realVar second

        IO.RigidVar _ ->
            subUnify target realVar second

        IO.RigidSuper _ _ ->
            subUnify target realVar second

        IO.Alias otherHome otherName otherArgs otherRealVar ->
            if name == otherName && home == otherHome then
                Unify
                    (\vars ->
                        unifyAliasArgs target vars args otherArgs
                            |> IO.bind
                                (\res ->
                                    case res of
                                        Ok (UnifyOk vars1 ()) ->
                                            case merge context otherContent of
                                                Unify k ->
                                                    k vars1

                                        Err err ->
                                            IO.pure (Err err)
                                )
                    )

            else
                subUnify target realVar otherRealVar

        IO.Structure _ ->
            subUnify target realVar second

        IO.Error ->
            merge context IO.Error


unifyAliasArgs : Target -> List IO.Variable -> List ( Name.Name, IO.Variable ) -> List ( Name.Name, IO.Variable ) -> IO (Result UnifyErr (UnifyOk ()))
unifyAliasArgs target vars args1 args2 =
    case args1 of
        ( _, arg1 ) :: others1 ->
            case args2 of
                ( _, arg2 ) :: others2 ->
                    case subUnify target arg1 arg2 of
                        Unify k ->
                            k vars
                                |> IO.bind
                                    (\res1 ->
                                        case res1 of
                                            Ok (UnifyOk vs ()) ->
                                                unifyAliasArgs target vs others1 others2

                                            Err (UnifyErr vs ()) ->
                                                unifyAliasArgs target vs others1 others2
                                                    |> IO.fmap
                                                        (\res2 ->
                                                            case res2 of
                                                                Ok (UnifyOk vs_ ()) ->
                                                                    Err (UnifyErr vs_ ())

                                                                Err err ->
                                                                    Err err
                                                        )
                                    )

                _ ->
                    IO.pure (Err (UnifyErr vars ()))

        [] ->
            case args2 of
                [] ->
                    IO.pure (Ok (UnifyOk vars ()))

                _ ->
                    IO.pure (Err (UnifyErr vars ()))



-- UNIFY STRUCTURES


unifyStructure : Target -> Context -> IO.FlatType -> IO.Content -> IO.Content -> Unify ()
unifyStructure target ((Context first _ second _) as context) flatType content otherContent =
    case otherContent of
        IO.FlexVar _ ->
            merge context content

        IO.FlexSuper super _ ->
            unifyFlexSuperStructure target (reorient context) super flatType

        IO.RigidVar _ ->
            mismatch

        IO.RigidSuper _ _ ->
            mismatch

        IO.Alias _ _ _ realVar ->
            subUnify target first realVar

        IO.Structure otherFlatType ->
            case ( flatType, otherFlatType ) of
                ( IO.App1 home name args, IO.App1 otherHome otherName otherArgs ) ->
                    if home == otherHome && name == otherName then
                        Unify
                            (\vars ->
                                unifyArgs target vars args otherArgs
                                    |> IO.bind
                                        (\unifiedArgs ->
                                            case unifiedArgs of
                                                Ok (UnifyOk vars1 ()) ->
                                                    case merge context otherContent of
                                                        Unify k ->
                                                            k vars1

                                                Err err ->
                                                    IO.pure (Err err)
                                        )
                            )

                    else
                        mismatch

                ( IO.Fun1 arg1 res1, IO.Fun1 arg2 res2 ) ->
                    subUnify target arg1 arg2
                        |> bind (\_ -> subUnify target res1 res2)
                        |> bind (\_ -> merge context otherContent)

                ( IO.EmptyRecord1, IO.EmptyRecord1 ) ->
                    merge context otherContent

                ( IO.Record1 fields ext, IO.EmptyRecord1 ) ->
                    if Dict.isEmpty fields then
                        subUnify target ext second

                    else
                        mismatch

                ( IO.EmptyRecord1, IO.Record1 fields ext ) ->
                    if Dict.isEmpty fields then
                        subUnify target first ext

                    else
                        mismatch

                ( IO.Record1 fields1 ext1, IO.Record1 fields2 ext2 ) ->
                    Unify
                        (\vars ->
                            gatherFields fields1 ext1
                                |> IO.bind
                                    (\structure1 ->
                                        gatherFields fields2 ext2
                                            |> IO.bind
                                                (\structure2 ->
                                                    case unifyRecord target context structure1 structure2 of
                                                        Unify k ->
                                                            k vars
                                                )
                                    )
                        )

                ( IO.Tuple1 a b cs, IO.Tuple1 x y zs ) ->
                    subUnify target a x
                        |> bind (\_ -> subUnify target b y)
                        |> bind (\_ -> subUnifyTuple target cs zs context otherContent)

                ( IO.Unit1, IO.Unit1 ) ->
                    merge context otherContent

                _ ->
                    mismatch

        IO.Error ->
            merge context IO.Error



-- UNIFY ARGS


unifyArgs : Target -> List IO.Variable -> List IO.Variable -> List IO.Variable -> IO (Result UnifyErr (UnifyOk ()))
unifyArgs target vars args1 args2 =
    case args1 of
        arg1 :: others1 ->
            case args2 of
                arg2 :: others2 ->
                    case subUnify target arg1 arg2 of
                        Unify k ->
                            k vars
                                |> IO.bind
                                    (\result ->
                                        case result of
                                            Ok (UnifyOk vs ()) ->
                                                unifyArgs target vs others1 others2

                                            Err (UnifyErr vs ()) ->
                                                unifyArgs target vs others1 others2
                                                    |> IO.fmap
                                                        (Result.andThen
                                                            (\(UnifyOk vs_ ()) ->
                                                                Err (UnifyErr vs_ ())
                                                            )
                                                        )
                                    )

                _ ->
                    IO.pure (Err (UnifyErr vars ()))

        [] ->
            case args2 of
                [] ->
                    IO.pure (Ok (UnifyOk vars ()))

                _ ->
                    IO.pure (Err (UnifyErr vars ()))



-- UNIFY RECORDS


unifyRecord : Target -> Context -> RecordStructure -> RecordStructure -> Unify ()
unifyRecord target context (RecordStructure fields1 ext1) (RecordStructure fields2 ext2) =
    let
        sharedFields : Dict String Name.Name ( IO.Variable, IO.Variable )
        sharedFields =
            Utils.mapIntersectionWith identity compare Tuple.pair fields1 fields2

        uniqueFields1 : Dict String Name.Name IO.Variable
        uniqueFields1 =
            Dict.diff fields1 fields2

        uniqueFields2 : Dict String Name.Name IO.Variable
        uniqueFields2 =
            Dict.diff fields2 fields1
    in
    if Dict.isEmpty uniqueFields1 then
        if Dict.isEmpty uniqueFields2 then
            subUnify target ext1 ext2
                |> bind (\_ -> unifySharedFields target context sharedFields Dict.empty ext1)

        else
            fresh context (IO.Structure (IO.Record1 uniqueFields2 ext2))
                |> bind
                    (\subRecord ->
                        subUnify target ext1 subRecord
                            |> bind (\_ -> unifySharedFields target context sharedFields Dict.empty subRecord)
                    )

    else if Dict.isEmpty uniqueFields2 then
        fresh context (IO.Structure (IO.Record1 uniqueFields1 ext1))
            |> bind
                (\subRecord ->
                    subUnify target subRecord ext2
                        |> bind (\_ -> unifySharedFields target context sharedFields Dict.empty subRecord)
                )

    else
        let
            otherFields : Dict String Name.Name IO.Variable
            otherFields =
                Dict.union uniqueFields1 uniqueFields2
        in
        fresh context Type.unnamedFlexVar
            |> bind
                (\ext ->
                    fresh context (IO.Structure (IO.Record1 uniqueFields1 ext))
                        |> bind
                            (\sub1 ->
                                fresh context (IO.Structure (IO.Record1 uniqueFields2 ext))
                                    |> bind
                                        (\sub2 ->
                                            subUnify target ext1 sub2
                                                |> bind (\_ -> subUnify target sub1 ext2)
                                                |> bind (\_ -> unifySharedFields target context sharedFields otherFields ext)
                                        )
                            )
                )


unifySharedFields : Target -> Context -> Dict String Name.Name ( IO.Variable, IO.Variable ) -> Dict String Name.Name IO.Variable -> IO.Variable -> Unify ()
unifySharedFields target context sharedFields otherFields ext =
    traverseMaybe identity compare (unifyField target) sharedFields
        |> bind
            (\matchingFields ->
                if Dict.size sharedFields == Dict.size matchingFields then
                    merge context (IO.Structure (IO.Record1 (Dict.union matchingFields otherFields) ext))

                else
                    mismatch
            )


traverseMaybe : (a -> comparable) -> (a -> a -> Order) -> (a -> b -> Unify (Maybe c)) -> Dict comparable a b -> Unify (Dict comparable a c)
traverseMaybe toComparable keyComparison func =
    Dict.foldl keyComparison
        (\a b ->
            bind
                (\acc ->
                    fmap
                        (\maybeC ->
                            maybeC
                                |> Maybe.map (\c -> Dict.insert toComparable a c acc)
                                |> Maybe.withDefault acc
                        )
                        (func a b)
                )
        )
        (pure Dict.empty)


unifyField : Target -> Name.Name -> ( IO.Variable, IO.Variable ) -> Unify (Maybe IO.Variable)
unifyField target _ ( actual, expected ) =
    Unify
        (\vars ->
            case subUnify target actual expected of
                Unify k ->
                    k vars
                        |> IO.fmap
                            (\result ->
                                case result of
                                    Ok (UnifyOk vs ()) ->
                                        Ok (UnifyOk vs (Just actual))

                                    Err (UnifyErr vs ()) ->
                                        Ok (UnifyOk vs Nothing)
                            )
        )



-- GATHER RECORD STRUCTURE


type RecordStructure
    = RecordStructure (Dict String Name.Name IO.Variable) IO.Variable


gatherFields : Dict String Name.Name IO.Variable -> IO.Variable -> IO RecordStructure
gatherFields fields variable =
    UF.get variable
        |> IO.bind
            (\(IO.Descriptor content _ _ _) ->
                case content of
                    IO.Structure (IO.Record1 subFields subExt) ->
                        gatherFields (Dict.union fields subFields) subExt

                    IO.Alias _ _ _ var ->
                        -- TODO may be dropping useful alias info here
                        gatherFields fields var

                    _ ->
                        IO.pure (RecordStructure fields variable)
            )
