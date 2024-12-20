module Compiler.Elm.Compiler.Type.Extract exposing
    ( fromDependencyInterface
    , fromInterface
    , fromMsg
    , fromType
    , merge
    , mergeMany
    , typesDecoder
    , typesEncoder
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Type as T
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Data.Map as Dict
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Types as T
import Utils.Main as Utils



-- EXTRACTION


fromType : T.CASTC_Type -> T.CECT_Type
fromType astType =
    Tuple.second (run (extract astType))


extract : T.CASTC_Type -> Extractor T.CECT_Type
extract astType =
    case astType of
        T.CASTC_TLambda arg result ->
            pure T.CECT_Lambda
                |> apply (extract arg)
                |> apply (extract result)

        T.CASTC_TVar x ->
            pure (T.CECT_Var x)

        T.CASTC_TType home name args ->
            addUnion (T.CASTO_Global home name) (T.CECT_Type (toPublicName home name))
                |> apply (traverse extract args)

        T.CASTC_TRecord fields ext ->
            traverse (tupleTraverse extract) (Can.fieldsToList fields)
                |> fmap (\efields -> T.CECT_Record efields ext)

        T.CASTC_TUnit ->
            pure T.CECT_Unit

        T.CASTC_TTuple a b maybeC ->
            pure T.CECT_Tuple
                |> apply (extract a)
                |> apply (extract b)
                |> apply (traverse extract (Maybe.toList maybeC))

        T.CASTC_TAlias home name args aliasType ->
            addAlias (T.CASTO_Global home name) ()
                |> bind
                    (\_ ->
                        extract (Type.dealias args aliasType)
                            |> bind
                                (\_ ->
                                    fmap (T.CECT_Type (toPublicName home name))
                                        (traverse (extract << Tuple.second) args)
                                )
                    )


toPublicName : T.CEMN_Canonical -> T.CDN_Name -> T.CDN_Name
toPublicName (T.CEMN_Canonical _ home) name =
    Name.sepBy '.' home name



-- TRANSITIVELY AVAILABLE TYPES


mergeMany : List T.CECTE_Types -> T.CECTE_Types
mergeMany listOfTypes =
    case listOfTypes of
        [] ->
            T.CECTE_Types Dict.empty

        t :: ts ->
            List.foldr merge t ts


merge : T.CECTE_Types -> T.CECTE_Types -> T.CECTE_Types
merge (T.CECTE_Types types1) (T.CECTE_Types types2) =
    T.CECTE_Types (Dict.union types1 types2)


fromInterface : T.CEMN_Raw -> T.CEI_Interface -> T.CECTE_Types
fromInterface name (T.CEI_Interface pkg _ unions aliases _) =
    T.CECTE_Types <|
        Dict.singleton ModuleName.toComparableCanonical (T.CEMN_Canonical pkg name) <|
            T.CECTE_Types_ (Dict.map (\_ -> I.extractUnion) unions) (Dict.map (\_ -> I.extractAlias) aliases)


fromDependencyInterface : T.CEMN_Canonical -> T.CEI_DependencyInterface -> T.CECTE_Types
fromDependencyInterface home di =
    T.CECTE_Types
        (Dict.singleton ModuleName.toComparableCanonical home <|
            case di of
                T.CEI_Public (T.CEI_Interface _ _ unions aliases _) ->
                    T.CECTE_Types_ (Dict.map (\_ -> I.extractUnion) unions) (Dict.map (\_ -> I.extractAlias) aliases)

                T.CEI_Private _ unions aliases ->
                    T.CECTE_Types_ unions aliases
        )



-- EXTRACT MODEL, MSG, AND ANY TRANSITIVE DEPENDENCIES


fromMsg : T.CECTE_Types -> T.CASTC_Type -> T.DebugMetadata
fromMsg types message =
    let
        ( msgDeps, msgType ) =
            run (extract message)

        ( aliases, unions ) =
            extractTransitive types noDeps msgDeps
    in
    T.DebugMetadata msgType aliases unions


extractTransitive : T.CECTE_Types -> Deps -> Deps -> ( List T.Alias, List T.Union )
extractTransitive types (Deps seenAliases seenUnions) (Deps nextAliases nextUnions) =
    let
        aliases : EverySet (List String) T.CASTO_Global
        aliases =
            EverySet.diff nextAliases seenAliases

        unions : EverySet (List String) T.CASTO_Global
        unions =
            EverySet.diff nextUnions seenUnions
    in
    if EverySet.isEmpty aliases && EverySet.isEmpty unions then
        ( [], [] )

    else
        let
            ( newDeps, ( resultAlias, resultUnion ) ) =
                run
                    (pure Tuple.pair
                        |> apply (traverse (extractAlias types) (EverySet.toList Opt.compareGlobal aliases))
                        |> apply (traverse (extractUnion types) (EverySet.toList Opt.compareGlobal unions))
                    )

            oldDeps : Deps
            oldDeps =
                Deps (EverySet.union seenAliases nextAliases) (EverySet.union seenUnions nextUnions)

            ( remainingResultAlias, remainingResultUnion ) =
                extractTransitive types oldDeps newDeps
        in
        ( resultAlias ++ remainingResultAlias, resultUnion ++ remainingResultUnion )


extractAlias : T.CECTE_Types -> T.CASTO_Global -> Extractor T.Alias
extractAlias (T.CECTE_Types dict) (T.CASTO_Global home name) =
    let
        (T.CASTC_Alias args aliasType) =
            Utils.find ModuleName.toComparableCanonical home dict
                |> (\(T.CECTE_Types_ _ aliasInfo) -> aliasInfo)
                |> Utils.find identity name
    in
    fmap (T.Alias (toPublicName home name) args) (extract aliasType)


extractUnion : T.CECTE_Types -> T.CASTO_Global -> Extractor T.Union
extractUnion (T.CECTE_Types dict) (T.CASTO_Global home name) =
    if name == Name.list && home == ModuleName.list then
        pure <| T.Union (toPublicName home name) [ "a" ] []

    else
        let
            pname : T.CDN_Name
            pname =
                toPublicName home name

            (T.CASTC_Union vars ctors _ _) =
                Utils.find ModuleName.toComparableCanonical home dict
                    |> (\(T.CECTE_Types_ unionInfo _) -> unionInfo)
                    |> Utils.find identity name
        in
        fmap (T.Union pname vars) (traverse extractCtor ctors)


extractCtor : T.CASTC_Ctor -> Extractor ( T.CDN_Name, List T.CECT_Type )
extractCtor (T.CASTC_Ctor ctor _ _ args) =
    fmap (Tuple.pair ctor) (traverse extract args)



-- DEPS


type Deps
    = Deps (EverySet (List String) T.CASTO_Global) (EverySet (List String) T.CASTO_Global)


noDeps : Deps
noDeps =
    Deps EverySet.empty EverySet.empty



-- EXTRACTOR


type Extractor a
    = Extractor (EverySet (List String) T.CASTO_Global -> EverySet (List String) T.CASTO_Global -> EResult a)


type EResult a
    = EResult (EverySet (List String) T.CASTO_Global) (EverySet (List String) T.CASTO_Global) a


run : Extractor a -> ( Deps, a )
run (Extractor k) =
    case k EverySet.empty EverySet.empty of
        EResult aliases unions value ->
            ( Deps aliases unions, value )


addAlias : T.CASTO_Global -> a -> Extractor a
addAlias alias value =
    Extractor <|
        \aliases unions ->
            EResult (EverySet.insert Opt.toComparableGlobal alias aliases) unions value


addUnion : T.CASTO_Global -> a -> Extractor a
addUnion union value =
    Extractor <|
        \aliases unions ->
            EResult aliases (EverySet.insert Opt.toComparableGlobal union unions) value


fmap : (a -> b) -> Extractor a -> Extractor b
fmap func (Extractor k) =
    Extractor <|
        \aliases unions ->
            case k aliases unions of
                EResult a1 u1 value ->
                    EResult a1 u1 (func value)


pure : a -> Extractor a
pure value =
    Extractor (\aliases unions -> EResult aliases unions value)


apply : Extractor a -> Extractor (a -> b) -> Extractor b
apply (Extractor kv) (Extractor kf) =
    Extractor <|
        \aliases unions ->
            case kf aliases unions of
                EResult a1 u1 func ->
                    case kv a1 u1 of
                        EResult a2 u2 value ->
                            EResult a2 u2 (func value)


bind : (a -> Extractor b) -> Extractor a -> Extractor b
bind callback (Extractor ka) =
    Extractor <|
        \aliases unions ->
            case ka aliases unions of
                EResult a1 u1 value ->
                    case callback value of
                        Extractor kb ->
                            kb a1 u1


traverse : (a -> Extractor b) -> List a -> Extractor (List b)
traverse f =
    List.foldr (\a -> bind (\c -> fmap (\va -> va :: c) (f a)))
        (pure [])


tupleTraverse : (b -> Extractor c) -> ( a, b ) -> Extractor ( a, c )
tupleTraverse f ( a, b ) =
    fmap (Tuple.pair a) (f b)



-- ENCODERS and DECODERS


typesEncoder : T.CECTE_Types -> Encode.Value
typesEncoder (T.CECTE_Types types) =
    E.assocListDict ModuleName.compareCanonical ModuleName.canonicalEncoder types_Encoder types


typesDecoder : Decode.Decoder T.CECTE_Types
typesDecoder =
    Decode.map T.CECTE_Types (D.assocListDict ModuleName.toComparableCanonical ModuleName.canonicalDecoder types_Decoder)


types_Encoder : T.CECTE_Types_ -> Encode.Value
types_Encoder (T.CECTE_Types_ unionInfo aliasInfo) =
    Encode.object
        [ ( "type", Encode.string "Types_" )
        , ( "unionInfo", E.assocListDict compare Encode.string Can.unionEncoder unionInfo )
        , ( "aliasInfo", E.assocListDict compare Encode.string Can.aliasEncoder aliasInfo )
        ]


types_Decoder : Decode.Decoder T.CECTE_Types_
types_Decoder =
    Decode.map2 T.CECTE_Types_
        (Decode.field "unionInfo" (D.assocListDict identity Decode.string Can.unionDecoder))
        (Decode.field "aliasInfo" (D.assocListDict identity Decode.string Can.aliasDecoder))
