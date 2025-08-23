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

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import System.TypeCheck.IO as IO
import Utils.Main as Utils



-- GENERATOR


type Tracker a
    = Tracker
        (Int
         -> EverySet (List String) Opt.Global
         -> Dict String Name Int
         -> TResult a
        )


type TResult a
    = TResult Int (EverySet (List String) Opt.Global) (Dict String Name Int) a


run : Tracker a -> ( EverySet (List String) Opt.Global, Dict String Name Int, a )
run (Tracker k) =
    case k 0 EverySet.empty Dict.empty of
        TResult _ deps fields value ->
            ( deps, fields, value )


generate : Tracker Name
generate =
    Tracker <|
        \uid deps fields ->
            TResult (uid + 1) deps fields (Name.fromVarIndex uid)


registerKernel : Name -> a -> Tracker a
registerKernel home value =
    Tracker <|
        \uid deps fields ->
            TResult uid (EverySet.insert Opt.toComparableGlobal (Opt.toKernelGlobal home) deps) fields value


registerGlobal : IO.Canonical -> Name -> Tracker Opt.Expr
registerGlobal home name =
    Tracker <|
        \uid deps fields ->
            let
                global : Opt.Global
                global =
                    Opt.Global home name
            in
            TResult uid (EverySet.insert Opt.toComparableGlobal global deps) fields (Opt.VarGlobal global)


registerDebug : Name -> IO.Canonical -> A.Region -> Tracker Opt.Expr
registerDebug name home region =
    Tracker <|
        \uid deps fields ->
            let
                global : Opt.Global
                global =
                    Opt.Global ModuleName.debug name
            in
            TResult uid (EverySet.insert Opt.toComparableGlobal global deps) fields (Opt.VarDebug name home region Nothing)


registerCtor : IO.Canonical -> Name -> Index.ZeroBased -> Can.CtorOpts -> Tracker Opt.Expr
registerCtor home name index opts =
    Tracker <|
        \uid deps fields ->
            let
                global : Opt.Global
                global =
                    Opt.Global home name

                newDeps : EverySet (List String) Opt.Global
                newDeps =
                    EverySet.insert Opt.toComparableGlobal global deps
            in
            case opts of
                Can.Normal ->
                    TResult uid newDeps fields (Opt.VarGlobal global)

                Can.Enum ->
                    TResult uid newDeps fields <|
                        case name of
                            "True" ->
                                if home == ModuleName.basics then
                                    Opt.Bool True

                                else
                                    Opt.VarEnum global index

                            "False" ->
                                if home == ModuleName.basics then
                                    Opt.Bool False

                                else
                                    Opt.VarEnum global index

                            _ ->
                                Opt.VarEnum global index

                Can.Unbox ->
                    TResult uid (EverySet.insert Opt.toComparableGlobal identity newDeps) fields (Opt.VarBox global)


identity : Opt.Global
identity =
    Opt.Global ModuleName.basics Name.identity_


registerField : Name -> a -> Tracker a
registerField name value =
    Tracker <|
        \uid d fields ->
            TResult uid d (Utils.mapInsertWith Basics.identity (+) name 1 fields) value


registerFieldDict : Dict String Name v -> a -> Tracker a
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


registerFieldList : List Name -> a -> Tracker a
registerFieldList names value =
    Tracker <|
        \uid deps fields ->
            TResult uid deps (List.foldr addOne fields names) value


addOne : Name -> Dict String Name Int -> Dict String Name Int
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
