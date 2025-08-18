module Compiler.Canonicalize.Environment.Foreign exposing (FResult, createInitialEnv)

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO
import Utils.Crash exposing (crash)
import Utils.Main as Utils


type alias FResult i w a =
    R.RResult i w Error.Error a


createInitialEnv : IO.Canonical -> Dict String ModuleName.Raw I.Interface -> List Src.Import -> FResult i w Env.Env
createInitialEnv home ifaces imports =
    Utils.foldM (addImport ifaces) emptyState (toSafeImports home imports)
        |> R.fmap
            (\{ vars, types, ctors, binops, q_vars, q_types, q_ctors } ->
                Env.Env home
                    (Dict.map (\_ -> infoToVar) vars)
                    types
                    ctors
                    binops
                    q_vars
                    q_types
                    q_ctors
            )


infoToVar : Env.Info Can.Annotation -> Env.Var
infoToVar info =
    case info of
        Env.Specific home tipe ->
            Env.Foreign home tipe

        Env.Ambiguous h hs ->
            Env.Foreigns h hs



-- STATE


type alias State =
    { vars : Env.Exposed Can.Annotation
    , types : Env.Exposed Env.Type
    , ctors : Env.Exposed Env.Ctor
    , binops : Env.Exposed Env.Binop
    , q_vars : Env.Qualified Can.Annotation
    , q_types : Env.Qualified Env.Type
    , q_ctors : Env.Qualified Env.Ctor
    }


emptyState : State
emptyState =
    State Dict.empty emptyTypes Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty


emptyTypes : Env.Exposed Env.Type
emptyTypes =
    Dict.fromList identity [ ( "List", Env.Specific ModuleName.list (Env.Union 1 ModuleName.list) ) ]



-- TO SAFE IMPORTS


toSafeImports : IO.Canonical -> List Src.Import -> List Src.Import
toSafeImports (IO.Canonical package _) imports =
    if Pkg.isKernel package then
        List.filter isNormal imports

    else
        imports


isNormal : Src.Import -> Bool
isNormal (Src.Import ( _, A.At _ name ) maybeAlias _) =
    if Name.isKernel name then
        case maybeAlias of
            Nothing ->
                False

            Just _ ->
                crash "kernel imports cannot use `as`"

    else
        True



-- ADD IMPORTS


addImport : Dict String ModuleName.Raw I.Interface -> State -> Src.Import -> FResult i w State
addImport ifaces state (Src.Import ( _, A.At _ name ) maybeAlias ( _, exposing_ )) =
    let
        (I.Interface pkg defs unions aliases binops) =
            Utils.find identity name ifaces

        prefix : Name
        prefix =
            Maybe.withDefault name (Maybe.map Src.c2Value maybeAlias)

        home : IO.Canonical
        home =
            IO.Canonical pkg name

        rawTypeInfo : Dict String Name ( Env.Type, Env.Exposed Env.Ctor )
        rawTypeInfo =
            Dict.union
                (Dict.toList compare unions
                    |> List.filterMap (\( k, a ) -> Maybe.map (Tuple.pair k) (unionToType home k a))
                    |> Dict.fromList identity
                )
                (Dict.toList compare aliases
                    |> List.filterMap (\( k, a ) -> Maybe.map (Tuple.pair k) (aliasToType home k a))
                    |> Dict.fromList identity
                )

        vars : Dict String Name (Env.Info Can.Annotation)
        vars =
            Dict.map (\_ -> Env.Specific home) defs

        types : Dict String Name (Env.Info Env.Type)
        types =
            Dict.map (\_ -> Env.Specific home << Tuple.first) rawTypeInfo

        ctors : Env.Exposed Env.Ctor
        ctors =
            Dict.foldr compare (\_ -> addExposed << Tuple.second) Dict.empty rawTypeInfo

        qvs2 : Env.Qualified Can.Annotation
        qvs2 =
            addQualified prefix vars state.q_vars

        qts2 : Env.Qualified Env.Type
        qts2 =
            addQualified prefix types state.q_types

        qcs2 : Env.Qualified Env.Ctor
        qcs2 =
            addQualified prefix ctors state.q_ctors
    in
    case exposing_ of
        Src.Open _ _ ->
            let
                vs2 : Env.Exposed Can.Annotation
                vs2 =
                    addExposed state.vars vars

                ts2 : Env.Exposed Env.Type
                ts2 =
                    addExposed state.types types

                cs2 : Env.Exposed Env.Ctor
                cs2 =
                    addExposed state.ctors ctors

                bs2 : Env.Exposed Env.Binop
                bs2 =
                    addExposed state.binops (Dict.map (binopToBinop home) binops)
            in
            R.ok (State vs2 ts2 cs2 bs2 qvs2 qts2 qcs2)

        Src.Explicit (A.At _ exposedList) ->
            Utils.foldM
                (addExposedValue home vars rawTypeInfo binops)
                (State state.vars state.types state.ctors state.binops qvs2 qts2 qcs2)
                (List.map Src.c2Value exposedList)


addExposed : Env.Exposed a -> Env.Exposed a -> Env.Exposed a
addExposed =
    Utils.mapUnionWith identity compare Env.mergeInfo


addQualified : Name -> Env.Exposed a -> Env.Qualified a -> Env.Qualified a
addQualified prefix exposed qualified =
    Utils.mapInsertWith identity addExposed prefix exposed qualified



-- UNION


unionToType : IO.Canonical -> Name -> I.Union -> Maybe ( Env.Type, Env.Exposed Env.Ctor )
unionToType home name union =
    Maybe.map (unionToTypeHelp home name) (I.toPublicUnion union)


unionToTypeHelp : IO.Canonical -> Name -> Can.Union -> ( Env.Type, Env.Exposed Env.Ctor )
unionToTypeHelp home name ((Can.Union vars ctors _ _) as union) =
    let
        addCtor : Can.Ctor -> Dict String Name (Env.Info Env.Ctor) -> Dict String Name (Env.Info Env.Ctor)
        addCtor (Can.Ctor ctor index _ args) dict =
            Dict.insert identity ctor (Env.Specific home (Env.Ctor home name union index args)) dict
    in
    ( Env.Union (List.length vars) home
    , List.foldl addCtor Dict.empty ctors
    )



-- ALIAS


aliasToType : IO.Canonical -> Name -> I.Alias -> Maybe ( Env.Type, Env.Exposed Env.Ctor )
aliasToType home name alias =
    Maybe.map (aliasToTypeHelp home name) (I.toPublicAlias alias)


aliasToTypeHelp : IO.Canonical -> Name -> Can.Alias -> ( Env.Type, Env.Exposed Env.Ctor )
aliasToTypeHelp home name (Can.Alias vars tipe) =
    ( Env.Alias (List.length vars) home vars tipe
    , case tipe of
        Can.TRecord fields Nothing ->
            let
                avars : List ( Name, Can.Type )
                avars =
                    List.map (\var -> ( var, Can.TVar var )) vars

                alias_ : Can.Type
                alias_ =
                    List.foldr
                        (\( _, t1 ) t2 -> Can.TLambda t1 t2)
                        (Can.TAlias home name avars (Can.Filled tipe))
                        (Can.fieldsToList fields)
            in
            Dict.singleton identity name (Env.Specific home (Env.RecordCtor home vars alias_))

        _ ->
            Dict.empty
    )



-- BINOP


binopToBinop : IO.Canonical -> Name -> I.Binop -> Env.Info Env.Binop
binopToBinop home op (I.Binop name annotation associativity precedence) =
    Env.Specific home (Env.Binop op home name annotation associativity precedence)



-- ADD EXPOSED VALUE


addExposedValue :
    IO.Canonical
    -> Env.Exposed Can.Annotation
    -> Dict String Name ( Env.Type, Env.Exposed Env.Ctor )
    -> Dict String Name I.Binop
    -> State
    -> Src.Exposed
    -> FResult i w State
addExposedValue home vars types binops state exposed =
    case exposed of
        Src.Lower (A.At region name) ->
            case Dict.get identity name vars of
                Just info ->
                    R.ok { state | vars = Utils.mapInsertWith identity Env.mergeInfo name info state.vars }

                Nothing ->
                    R.throw (Error.ImportExposingNotFound region home name (Dict.keys compare vars))

        Src.Upper (A.At region name) ( _, privacy ) ->
            case privacy of
                Src.Private ->
                    case Dict.get identity name types of
                        Just ( tipe, ctors ) ->
                            case tipe of
                                Env.Union _ _ ->
                                    let
                                        ts2 : Dict String Name (Env.Info Env.Type)
                                        ts2 =
                                            Dict.insert identity name (Env.Specific home tipe) state.types
                                    in
                                    R.ok { state | types = ts2 }

                                Env.Alias _ _ _ _ ->
                                    let
                                        ts2 : Dict String Name (Env.Info Env.Type)
                                        ts2 =
                                            Dict.insert identity name (Env.Specific home tipe) state.types

                                        cs2 : Env.Exposed Env.Ctor
                                        cs2 =
                                            addExposed state.ctors ctors
                                    in
                                    R.ok { state | types = ts2, ctors = cs2 }

                        Nothing ->
                            case checkForCtorMistake name types of
                                tipe :: _ ->
                                    R.throw <| Error.ImportCtorByName region name tipe

                                [] ->
                                    R.throw <| Error.ImportExposingNotFound region home name (Dict.keys compare types)

                Src.Public dotDotRegion ->
                    case Dict.get identity name types of
                        Just ( tipe, ctors ) ->
                            case tipe of
                                Env.Union _ _ ->
                                    let
                                        ts2 : Dict String Name (Env.Info Env.Type)
                                        ts2 =
                                            Dict.insert identity name (Env.Specific home tipe) state.types

                                        cs2 : Env.Exposed Env.Ctor
                                        cs2 =
                                            addExposed state.ctors ctors
                                    in
                                    R.ok { state | types = ts2, ctors = cs2 }

                                Env.Alias _ _ _ _ ->
                                    R.throw (Error.ImportOpenAlias dotDotRegion name)

                        Nothing ->
                            R.throw (Error.ImportExposingNotFound region home name (Dict.keys compare types))

        Src.Operator region op ->
            case Dict.get identity op binops of
                Just binop ->
                    let
                        bs2 : Dict String Name (Env.Info Env.Binop)
                        bs2 =
                            Dict.insert identity op (binopToBinop home op binop) state.binops
                    in
                    R.ok { state | binops = bs2 }

                Nothing ->
                    R.throw (Error.ImportExposingNotFound region home op (Dict.keys compare binops))


checkForCtorMistake : Name -> Dict String Name ( Env.Type, Env.Exposed Env.Ctor ) -> List Name
checkForCtorMistake givenName types =
    let
        addMatches : a -> ( b, Dict String Name (Env.Info Env.Ctor) ) -> List Name -> List Name
        addMatches _ ( _, exposedCtors ) matches =
            Dict.foldr compare addMatch matches exposedCtors

        addMatch : Name -> Env.Info Env.Ctor -> List Name -> List Name
        addMatch ctorName info matches =
            if ctorName /= givenName then
                matches

            else
                case info of
                    Env.Specific _ (Env.Ctor _ tipeName _ _ _) ->
                        tipeName :: matches

                    Env.Specific _ (Env.RecordCtor _ _ _) ->
                        matches

                    Env.Ambiguous _ _ ->
                        matches
    in
    Dict.foldr compare addMatches [] types
