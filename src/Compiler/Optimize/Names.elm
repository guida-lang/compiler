module Compiler.Optimize.Names exposing
    ( Tracker
    , bind
    , fmap
    , generate
    , mapTraverse
    , pure
    , registerCtor
    , registerDebug
    , registerField
    , registerFieldDict
    , registerFieldList
    , registerGlobal
    , registerKernel
    , run
    , traverse
    )

import Compiler.AST.Optimized as Opt
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Types as T
import Utils.Main as Utils



-- GENERATOR


type Tracker a
    = Tracker
        (Int
         -> EverySet (List String) T.CASTO_Global
         -> Dict String T.CDN_Name Int
         -> TResult a
        )


type TResult a
    = TResult Int (EverySet (List String) T.CASTO_Global) (Dict String T.CDN_Name Int) a


run : Tracker a -> ( EverySet (List String) T.CASTO_Global, Dict String T.CDN_Name Int, a )
run (Tracker k) =
    case k 0 EverySet.empty Dict.empty of
        TResult _ deps fields value ->
            ( deps, fields, value )


generate : Tracker T.CDN_Name
generate =
    Tracker <|
        \uid deps fields ->
            TResult (uid + 1) deps fields (Name.fromVarIndex uid)


registerKernel : T.CDN_Name -> a -> Tracker a
registerKernel home value =
    Tracker <|
        \uid deps fields ->
            TResult uid (EverySet.insert Opt.toComparableGlobal (Opt.toKernelGlobal home) deps) fields value


registerGlobal : T.CEMN_Canonical -> T.CDN_Name -> Tracker T.CASTO_Expr
registerGlobal home name =
    Tracker <|
        \uid deps fields ->
            let
                global : T.CASTO_Global
                global =
                    T.CASTO_Global home name
            in
            TResult uid (EverySet.insert Opt.toComparableGlobal global deps) fields (T.CASTO_VarGlobal global)


registerDebug : T.CDN_Name -> T.CEMN_Canonical -> T.CRA_Region -> Tracker T.CASTO_Expr
registerDebug name home region =
    Tracker <|
        \uid deps fields ->
            let
                global : T.CASTO_Global
                global =
                    T.CASTO_Global ModuleName.debug name
            in
            TResult uid (EverySet.insert Opt.toComparableGlobal global deps) fields (T.CASTO_VarDebug name home region Nothing)


registerCtor : T.CEMN_Canonical -> T.CDN_Name -> T.CDI_ZeroBased -> T.CASTC_CtorOpts -> Tracker T.CASTO_Expr
registerCtor home name index opts =
    Tracker <|
        \uid deps fields ->
            let
                global : T.CASTO_Global
                global =
                    T.CASTO_Global home name

                newDeps : EverySet (List String) T.CASTO_Global
                newDeps =
                    EverySet.insert Opt.toComparableGlobal global deps
            in
            case opts of
                T.CASTC_Normal ->
                    TResult uid newDeps fields (T.CASTO_VarGlobal global)

                T.CASTC_Enum ->
                    TResult uid newDeps fields <|
                        case name of
                            "True" ->
                                if home == ModuleName.basics then
                                    T.CASTO_Bool True

                                else
                                    T.CASTO_VarEnum global index

                            "False" ->
                                if home == ModuleName.basics then
                                    T.CASTO_Bool False

                                else
                                    T.CASTO_VarEnum global index

                            _ ->
                                T.CASTO_VarEnum global index

                T.CASTC_Unbox ->
                    TResult uid (EverySet.insert Opt.toComparableGlobal identity newDeps) fields (T.CASTO_VarBox global)


identity : T.CASTO_Global
identity =
    T.CASTO_Global ModuleName.basics Name.identity_


registerField : T.CDN_Name -> a -> Tracker a
registerField name value =
    Tracker <|
        \uid d fields ->
            TResult uid d (Utils.mapInsertWith Basics.identity (+) name 1 fields) value


registerFieldDict : Dict String T.CDN_Name v -> a -> Tracker a
registerFieldDict newFields value =
    Tracker <|
        \uid d fields ->
            TResult uid
                d
                (Utils.mapUnionWith Basics.identity compare (+) fields (Dict.map (\_ -> toOne) newFields))
                value


toOne : a -> Int
toOne _ =
    1


registerFieldList : List T.CDN_Name -> a -> Tracker a
registerFieldList names value =
    Tracker <|
        \uid deps fields ->
            TResult uid deps (List.foldr addOne fields names) value


addOne : T.CDN_Name -> Dict String T.CDN_Name Int -> Dict String T.CDN_Name Int
addOne name fields =
    Utils.mapInsertWith Basics.identity (+) name 1 fields



-- INSTANCES


fmap : (a -> b) -> Tracker a -> Tracker b
fmap func (Tracker kv) =
    Tracker <|
        \n d f ->
            case kv n d f of
                TResult n1 d1 f1 value ->
                    TResult n1 d1 f1 (func value)


pure : a -> Tracker a
pure value =
    Tracker (\n d f -> TResult n d f value)


bind : (a -> Tracker b) -> Tracker a -> Tracker b
bind callback (Tracker k) =
    Tracker <|
        \n d f ->
            case k n d f of
                TResult n1 d1 f1 a ->
                    case callback a of
                        Tracker kb ->
                            kb n1 d1 f1


traverse : (a -> Tracker b) -> List a -> Tracker (List b)
traverse func =
    List.foldl (\a -> bind (\acc -> fmap (\b -> acc ++ [ b ]) (func a))) (pure [])


mapTraverse : (k -> comparable) -> (k -> k -> Order) -> (a -> Tracker b) -> Dict comparable k a -> Tracker (Dict comparable k b)
mapTraverse toComparable keyComparison func =
    Dict.foldl keyComparison (\k a -> bind (\c -> fmap (\va -> Dict.insert toComparable k va c) (func a))) (pure Dict.empty)
