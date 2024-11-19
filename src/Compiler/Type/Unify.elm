module Compiler.Type.Unify exposing
    ( Answer(..)
    , unify
    )

import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Type.Error as Error
import Compiler.Type.Occurs as Occurs
import Compiler.Type.Type as Type
import Compiler.Type.UnionFind as UF
import Data.IO as IO exposing (IO)
import Data.Map as Dict exposing (Dict)
import Utils.Main as Utils



-- UNIFY


type Answer
    = AnswerOk (List Type.Variable)
    | AnswerErr (List Type.Variable) Error.Type Error.Type


unify : Type.Variable -> Type.Variable -> IO Answer
unify v1 v2 =
    case guardedUnify v1 v2 of
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
                                                        UF.union Type.descriptorEncoder v1 v2 errorDescriptor
                                                            |> IO.fmap (\_ -> AnswerErr vars t1 t2)
                                                    )
                                        )
                    )


onSuccess : List Type.Variable -> () -> IO Answer
onSuccess vars () =
    IO.pure (AnswerOk vars)


errorDescriptor : Type.Descriptor
errorDescriptor =
    Type.Descriptor Type.Error Type.noRank Type.noMark Nothing



-- CPS UNIFIER


type Unify a
    = Unify (List Type.Variable -> IO (Result UnifyErr (UnifyOk a)))


type UnifyOk a
    = UnifyOk (List Type.Variable) a


type UnifyErr
    = UnifyErr (List Type.Variable) ()


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


register : IO Type.Variable -> Unify Type.Variable
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
    = Context Type.Variable Type.Descriptor Type.Variable Type.Descriptor


reorient : Context -> Context
reorient (Context var1 desc1 var2 desc2) =
    Context var2 desc2 var1 desc1



-- MERGE
-- merge : Context -> UF.Content -> Unify ( UF.Point UF.Descriptor, UF.Point UF.Descriptor )


merge : Context -> Type.Content -> Unify ()
merge (Context var1 (Type.Descriptor _ rank1 _ _) var2 (Type.Descriptor _ rank2 _ _)) content =
    Unify
        (\vars ->
            UF.union Type.descriptorEncoder var1 var2 (Type.Descriptor content (min rank1 rank2) Type.noMark Nothing)
                |> IO.fmap (Ok << UnifyOk vars)
        )


fresh : Context -> Type.Content -> Unify Type.Variable
fresh (Context _ (Type.Descriptor _ rank1 _ _) _ (Type.Descriptor _ rank2 _ _)) content =
    register <|
        UF.fresh Type.descriptorEncoder <|
            Type.Descriptor content (min rank1 rank2) Type.noMark Nothing



-- ACTUALLY UNIFY THINGS


guardedUnify : Type.Variable -> Type.Variable -> Unify ()
guardedUnify left right =
    Unify
        (\vars ->
            UF.equivalent left right
                |> IO.bind
                    (\equivalent ->
                        if equivalent then
                            IO.pure (Ok (UnifyOk vars ()))

                        else
                            UF.get Type.descriptorDecoder left
                                |> IO.bind
                                    (\leftDesc ->
                                        UF.get Type.descriptorDecoder right
                                            |> IO.bind
                                                (\rightDesc ->
                                                    case actuallyUnify (Context left leftDesc right rightDesc) of
                                                        Unify k ->
                                                            k vars
                                                )
                                    )
                    )
        )


subUnify : Type.Variable -> Type.Variable -> Unify ()
subUnify var1 var2 =
    guardedUnify var1 var2


actuallyUnify : Context -> Unify ()
actuallyUnify ((Context _ (Type.Descriptor firstContent _ _ _) _ (Type.Descriptor secondContent _ _ _)) as context) =
    case firstContent of
        Type.FlexVar _ ->
            unifyFlex context firstContent secondContent

        Type.FlexSuper super _ ->
            unifyFlexSuper context super firstContent secondContent

        Type.RigidVar _ ->
            unifyRigid context Nothing firstContent secondContent

        Type.RigidSuper super _ ->
            unifyRigid context (Just super) firstContent secondContent

        Type.Alias home name args realVar ->
            unifyAlias context home name args realVar secondContent

        Type.Structure flatType ->
            unifyStructure context flatType firstContent secondContent

        Type.Error ->
            -- If there was an error, just pretend it is okay. This lets us avoid
            -- "cascading" errors where one problem manifests as multiple message.
            merge context Type.Error



-- UNIFY FLEXIBLE VARIABLES


unifyFlex : Context -> Type.Content -> Type.Content -> Unify ()
unifyFlex context content otherContent =
    case otherContent of
        Type.Error ->
            merge context Type.Error

        Type.FlexVar maybeName ->
            merge context <|
                case maybeName of
                    Nothing ->
                        content

                    Just _ ->
                        otherContent

        Type.FlexSuper _ _ ->
            merge context otherContent

        Type.RigidVar _ ->
            merge context otherContent

        Type.RigidSuper _ _ ->
            merge context otherContent

        Type.Alias _ _ _ _ ->
            merge context otherContent

        Type.Structure _ ->
            merge context otherContent



-- UNIFY RIGID VARIABLES


unifyRigid : Context -> Maybe Type.SuperType -> Type.Content -> Type.Content -> Unify ()
unifyRigid context maybeSuper content otherContent =
    case otherContent of
        Type.FlexVar _ ->
            merge context content

        Type.FlexSuper otherSuper _ ->
            case maybeSuper of
                Just super ->
                    if combineRigidSupers super otherSuper then
                        merge context content

                    else
                        mismatch

                Nothing ->
                    mismatch

        Type.RigidVar _ ->
            mismatch

        Type.RigidSuper _ _ ->
            mismatch

        Type.Alias _ _ _ _ ->
            mismatch

        Type.Structure _ ->
            mismatch

        Type.Error ->
            merge context Type.Error



-- UNIFY SUPER VARIABLES


unifyFlexSuper : Context -> Type.SuperType -> Type.Content -> Type.Content -> Unify ()
unifyFlexSuper ((Context first _ _ _) as context) super content otherContent =
    case otherContent of
        Type.Structure flatType ->
            unifyFlexSuperStructure context super flatType

        Type.RigidVar _ ->
            mismatch

        Type.RigidSuper otherSuper _ ->
            if combineRigidSupers otherSuper super then
                merge context otherContent

            else
                mismatch

        Type.FlexVar _ ->
            merge context content

        Type.FlexSuper otherSuper _ ->
            case super of
                Type.Number ->
                    case otherSuper of
                        Type.Number ->
                            merge context content

                        Type.Comparable ->
                            merge context content

                        Type.Appendable ->
                            mismatch

                        Type.CompAppend ->
                            mismatch

                Type.Comparable ->
                    case otherSuper of
                        Type.Comparable ->
                            merge context otherContent

                        Type.Number ->
                            merge context otherContent

                        Type.Appendable ->
                            merge context <| Type.unnamedFlexSuper Type.CompAppend

                        Type.CompAppend ->
                            merge context otherContent

                Type.Appendable ->
                    case otherSuper of
                        Type.Appendable ->
                            merge context otherContent

                        Type.Comparable ->
                            merge context <| Type.unnamedFlexSuper Type.CompAppend

                        Type.CompAppend ->
                            merge context otherContent

                        Type.Number ->
                            mismatch

                Type.CompAppend ->
                    case otherSuper of
                        Type.Comparable ->
                            merge context content

                        Type.Appendable ->
                            merge context content

                        Type.CompAppend ->
                            merge context content

                        Type.Number ->
                            mismatch

        Type.Alias _ _ _ realVar ->
            subUnify first realVar

        Type.Error ->
            merge context Type.Error


combineRigidSupers : Type.SuperType -> Type.SuperType -> Bool
combineRigidSupers rigid flex =
    rigid
        == flex
        || (rigid == Type.Number && flex == Type.Comparable)
        || (rigid == Type.CompAppend && (flex == Type.Comparable || flex == Type.Appendable))


atomMatchesSuper : Type.SuperType -> ModuleName.Canonical -> Name.Name -> Bool
atomMatchesSuper super home name =
    case super of
        Type.Number ->
            isNumber home name

        Type.Comparable ->
            isNumber home name || Error.isString home name || Error.isChar home name

        Type.Appendable ->
            Error.isString home name

        Type.CompAppend ->
            Error.isString home name


isNumber : ModuleName.Canonical -> Name.Name -> Bool
isNumber home name =
    (home == ModuleName.basics)
        && (name == Name.int || name == Name.float)


unifyFlexSuperStructure : Context -> Type.SuperType -> Type.FlatType -> Unify ()
unifyFlexSuperStructure context super flatType =
    case flatType of
        Type.App1 home name [] ->
            if atomMatchesSuper super home name then
                merge context (Type.Structure flatType)

            else
                mismatch

        Type.App1 home name [ variable ] ->
            if home == ModuleName.list && name == Name.list then
                case super of
                    Type.Number ->
                        mismatch

                    Type.Appendable ->
                        merge context (Type.Structure flatType)

                    Type.Comparable ->
                        comparableOccursCheck context
                            |> bind (\_ -> unifyComparableRecursive variable)
                            |> bind (\_ -> merge context (Type.Structure flatType))

                    Type.CompAppend ->
                        comparableOccursCheck context
                            |> bind (\_ -> unifyComparableRecursive variable)
                            |> bind (\_ -> merge context (Type.Structure flatType))

            else
                mismatch

        Type.Tuple1 a b maybeC ->
            case super of
                Type.Number ->
                    mismatch

                Type.Appendable ->
                    mismatch

                Type.Comparable ->
                    comparableOccursCheck context
                        |> bind (\_ -> unifyComparableRecursive a)
                        |> bind (\_ -> unifyComparableRecursive b)
                        |> bind
                            (\_ ->
                                case maybeC of
                                    Nothing ->
                                        pure ()

                                    Just c ->
                                        unifyComparableRecursive c
                            )
                        |> bind (\_ -> merge context (Type.Structure flatType))

                Type.CompAppend ->
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


unifyComparableRecursive : Type.Variable -> Unify ()
unifyComparableRecursive var =
    register
        (UF.get Type.descriptorDecoder var
            |> IO.bind
                (\(Type.Descriptor _ rank _ _) ->
                    UF.fresh Type.descriptorEncoder (Type.Descriptor (Type.unnamedFlexSuper Type.Comparable) rank Type.noMark Nothing)
                )
        )
        |> bind (\compVar -> guardedUnify compVar var)



-- UNIFY ALIASES


unifyAlias : Context -> ModuleName.Canonical -> Name.Name -> List ( Name.Name, Type.Variable ) -> Type.Variable -> Type.Content -> Unify ()
unifyAlias ((Context _ _ second _) as context) home name args realVar otherContent =
    case otherContent of
        Type.FlexVar _ ->
            merge context (Type.Alias home name args realVar)

        Type.FlexSuper _ _ ->
            subUnify realVar second

        Type.RigidVar _ ->
            subUnify realVar second

        Type.RigidSuper _ _ ->
            subUnify realVar second

        Type.Alias otherHome otherName otherArgs otherRealVar ->
            if name == otherName && home == otherHome then
                Unify
                    (\vars ->
                        unifyAliasArgs vars args otherArgs
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
                subUnify realVar otherRealVar

        Type.Structure _ ->
            subUnify realVar second

        Type.Error ->
            merge context Type.Error


unifyAliasArgs : List Type.Variable -> List ( Name.Name, Type.Variable ) -> List ( Name.Name, Type.Variable ) -> IO (Result UnifyErr (UnifyOk ()))
unifyAliasArgs vars args1 args2 =
    case args1 of
        ( _, arg1 ) :: others1 ->
            case args2 of
                ( _, arg2 ) :: others2 ->
                    case subUnify arg1 arg2 of
                        Unify k ->
                            k vars
                                |> IO.bind
                                    (\res1 ->
                                        case res1 of
                                            Ok (UnifyOk vs ()) ->
                                                unifyAliasArgs vs others1 others2

                                            Err (UnifyErr vs ()) ->
                                                unifyAliasArgs vs others1 others2
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


unifyStructure : Context -> Type.FlatType -> Type.Content -> Type.Content -> Unify ()
unifyStructure ((Context first _ second _) as context) flatType content otherContent =
    case otherContent of
        Type.FlexVar _ ->
            merge context content

        Type.FlexSuper super _ ->
            unifyFlexSuperStructure (reorient context) super flatType

        Type.RigidVar _ ->
            mismatch

        Type.RigidSuper _ _ ->
            mismatch

        Type.Alias _ _ _ realVar ->
            subUnify first realVar

        Type.Structure otherFlatType ->
            case ( flatType, otherFlatType ) of
                ( Type.App1 home name args, Type.App1 otherHome otherName otherArgs ) ->
                    if home == otherHome && name == otherName then
                        Unify
                            (\vars ->
                                unifyArgs vars args otherArgs
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

                ( Type.Fun1 arg1 res1, Type.Fun1 arg2 res2 ) ->
                    subUnify arg1 arg2
                        |> bind (\_ -> subUnify res1 res2)
                        |> bind (\_ -> merge context otherContent)

                ( Type.EmptyRecord1, Type.EmptyRecord1 ) ->
                    merge context otherContent

                ( Type.Record1 fields ext, Type.EmptyRecord1 ) ->
                    if Dict.isEmpty fields then
                        subUnify ext second

                    else
                        mismatch

                ( Type.EmptyRecord1, Type.Record1 fields ext ) ->
                    if Dict.isEmpty fields then
                        subUnify first ext

                    else
                        mismatch

                ( Type.Record1 fields1 ext1, Type.Record1 fields2 ext2 ) ->
                    Unify
                        (\vars ->
                            gatherFields fields1 ext1
                                |> IO.bind
                                    (\structure1 ->
                                        gatherFields fields2 ext2
                                            |> IO.bind
                                                (\structure2 ->
                                                    case unifyRecord context structure1 structure2 of
                                                        Unify k ->
                                                            k vars
                                                )
                                    )
                        )

                ( Type.Tuple1 a b Nothing, Type.Tuple1 x y Nothing ) ->
                    subUnify a x
                        |> bind (\_ -> subUnify b y)
                        |> bind (\_ -> merge context otherContent)

                ( Type.Tuple1 a b (Just c), Type.Tuple1 x y (Just z) ) ->
                    subUnify a x
                        |> bind (\_ -> subUnify b y)
                        |> bind (\_ -> subUnify c z)
                        |> bind (\_ -> merge context otherContent)

                ( Type.Unit1, Type.Unit1 ) ->
                    merge context otherContent

                _ ->
                    mismatch

        Type.Error ->
            merge context Type.Error



-- UNIFY ARGS


unifyArgs : List Type.Variable -> List Type.Variable -> List Type.Variable -> IO (Result UnifyErr (UnifyOk ()))
unifyArgs vars args1 args2 =
    case args1 of
        arg1 :: others1 ->
            case args2 of
                arg2 :: others2 ->
                    case subUnify arg1 arg2 of
                        Unify k ->
                            k vars
                                |> IO.bind
                                    (\result ->
                                        case result of
                                            Ok (UnifyOk vs ()) ->
                                                unifyArgs vs others1 others2

                                            Err (UnifyErr vs ()) ->
                                                unifyArgs vs others1 others2
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


unifyRecord : Context -> RecordStructure -> RecordStructure -> Unify ()
unifyRecord context (RecordStructure fields1 ext1) (RecordStructure fields2 ext2) =
    let
        sharedFields : Dict Name.Name ( Type.Variable, Type.Variable )
        sharedFields =
            Utils.mapIntersectionWith compare Tuple.pair fields1 fields2

        uniqueFields1 : Dict Name.Name Type.Variable
        uniqueFields1 =
            Dict.diff fields1 fields2

        uniqueFields2 : Dict Name.Name Type.Variable
        uniqueFields2 =
            Dict.diff fields2 fields1
    in
    if Dict.isEmpty uniqueFields1 then
        if Dict.isEmpty uniqueFields2 then
            subUnify ext1 ext2
                |> bind (\_ -> unifySharedFields context sharedFields Dict.empty ext1)

        else
            fresh context (Type.Structure (Type.Record1 uniqueFields2 ext2))
                |> bind
                    (\subRecord ->
                        subUnify ext1 subRecord
                            |> bind (\_ -> unifySharedFields context sharedFields Dict.empty subRecord)
                    )

    else if Dict.isEmpty uniqueFields2 then
        fresh context (Type.Structure (Type.Record1 uniqueFields1 ext1))
            |> bind
                (\subRecord ->
                    subUnify subRecord ext2
                        |> bind (\_ -> unifySharedFields context sharedFields Dict.empty subRecord)
                )

    else
        let
            otherFields : Dict Name.Name Type.Variable
            otherFields =
                Dict.union compare uniqueFields1 uniqueFields2
        in
        fresh context Type.unnamedFlexVar
            |> bind
                (\ext ->
                    fresh context (Type.Structure (Type.Record1 uniqueFields1 ext))
                        |> bind
                            (\sub1 ->
                                fresh context (Type.Structure (Type.Record1 uniqueFields2 ext))
                                    |> bind
                                        (\sub2 ->
                                            subUnify ext1 sub2
                                                |> bind (\_ -> subUnify sub1 ext2)
                                                |> bind (\_ -> unifySharedFields context sharedFields otherFields ext)
                                        )
                            )
                )


unifySharedFields : Context -> Dict Name.Name ( Type.Variable, Type.Variable ) -> Dict Name.Name Type.Variable -> Type.Variable -> Unify ()
unifySharedFields context sharedFields otherFields ext =
    traverseMaybe compare unifyField sharedFields
        |> bind
            (\matchingFields ->
                if Dict.size sharedFields == Dict.size matchingFields then
                    merge context (Type.Structure (Type.Record1 (Dict.union compare matchingFields otherFields) ext))

                else
                    mismatch
            )


traverseMaybe : (a -> a -> Order) -> (a -> b -> Unify (Maybe c)) -> Dict a b -> Unify (Dict a c)
traverseMaybe keyComparison func =
    Dict.foldl
        (\a b ->
            bind
                (\acc ->
                    fmap
                        (\maybeC ->
                            maybeC
                                |> Maybe.map (\c -> Dict.insert keyComparison a c acc)
                                |> Maybe.withDefault acc
                        )
                        (func a b)
                )
        )
        (pure Dict.empty)


unifyField : Name.Name -> ( Type.Variable, Type.Variable ) -> Unify (Maybe Type.Variable)
unifyField _ ( actual, expected ) =
    Unify
        (\vars ->
            case subUnify actual expected of
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
    = RecordStructure (Dict Name.Name Type.Variable) Type.Variable


gatherFields : Dict Name.Name Type.Variable -> Type.Variable -> IO RecordStructure
gatherFields fields variable =
    UF.get Type.descriptorDecoder variable
        |> IO.bind
            (\(Type.Descriptor content _ _ _) ->
                case content of
                    Type.Structure (Type.Record1 subFields subExt) ->
                        gatherFields (Dict.union compare fields subFields) subExt

                    Type.Alias _ _ _ var ->
                        -- TODO may be dropping useful alias info here
                        gatherFields fields var

                    _ ->
                        IO.pure (RecordStructure fields variable)
            )
