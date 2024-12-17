module Compiler.Canonicalize.Environment.Foreign exposing (FResult, createInitialEnv)

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Data.Name as Name
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


createInitialEnv : IO.CEMN_Canonical -> Dict String ModuleName.CEMN_Raw I.CEI_Interface -> List Src.CASTS_Import -> FResult i w Env.Env
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


infoToVar : Env.Info Can.CASTC_Annotation -> Env.Var
infoToVar info =
    case info of
        Env.Specific home tipe ->
            Env.Foreign home tipe

        Env.Ambiguous h hs ->
            Env.Foreigns h hs



-- STATE


type alias State =
    { vars : Env.Exposed Can.CASTC_Annotation
    , types : Env.Exposed Env.Type
    , ctors : Env.Exposed Env.Ctor
    , binops : Env.Exposed Env.Binop
    , q_vars : Env.Qualified Can.CASTC_Annotation
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


toSafeImports : IO.CEMN_Canonical -> List Src.CASTS_Import -> List Src.CASTS_Import
toSafeImports (IO.CEMN_Canonical package _) imports =
    if Pkg.isKernel package then
        List.filter isNormal imports

    else
        imports


isNormal : Src.CASTS_Import -> Bool
isNormal (Src.CASTS_Import (A.CRA_At _ name) maybeAlias _) =
    if Name.isKernel name then
        case maybeAlias of
            Nothing ->
                False

            Just _ ->
                crash "kernel imports cannot use `as`"

    else
        True



-- ADD IMPORTS


addImport : Dict String ModuleName.CEMN_Raw I.CEI_Interface -> State -> Src.CASTS_Import -> FResult i w State
addImport ifaces state (Src.CASTS_Import (A.CRA_At _ name) maybeAlias exposing_) =
    let
        (I.CEI_Interface pkg defs unions aliases binops) =
            Utils.find identity name ifaces

        prefix : Name.CDN_Name
        prefix =
            Maybe.withDefault name maybeAlias

        home : IO.CEMN_Canonical
        home =
            IO.CEMN_Canonical pkg name

        rawTypeInfo : Dict String Name.CDN_Name ( Env.Type, Env.Exposed Env.Ctor )
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

        vars : Dict String Name.CDN_Name (Env.Info Can.CASTC_Annotation)
        vars =
            Dict.map (\_ -> Env.Specific home) defs

        types : Dict String Name.CDN_Name (Env.Info Env.Type)
        types =
            Dict.map (\_ -> Env.Specific home << Tuple.first) rawTypeInfo

        ctors : Env.Exposed Env.Ctor
        ctors =
            Dict.foldr compare (\_ -> addExposed << Tuple.second) Dict.empty rawTypeInfo

        qvs2 : Env.Qualified Can.CASTC_Annotation
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
        Src.CASTS_Open ->
            let
                vs2 : Env.Exposed Can.CASTC_Annotation
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

        Src.CASTS_Explicit exposedList ->
            Utils.foldM
                (addExposedValue home vars rawTypeInfo binops)
                (State state.vars state.types state.ctors state.binops qvs2 qts2 qcs2)
                exposedList


addExposed : Env.Exposed a -> Env.Exposed a -> Env.Exposed a
addExposed =
    Utils.mapUnionWith identity compare Env.mergeInfo


addQualified : Name.CDN_Name -> Env.Exposed a -> Env.Qualified a -> Env.Qualified a
addQualified prefix exposed qualified =
    Utils.mapInsertWith identity addExposed prefix exposed qualified



-- UNION


unionToType : IO.CEMN_Canonical -> Name.CDN_Name -> I.CEI_Union -> Maybe ( Env.Type, Env.Exposed Env.Ctor )
unionToType home name union =
    Maybe.map (unionToTypeHelp home name) (I.toPublicUnion union)


unionToTypeHelp : IO.CEMN_Canonical -> Name.CDN_Name -> Can.CASTC_Union -> ( Env.Type, Env.Exposed Env.Ctor )
unionToTypeHelp home name ((Can.CASTC_Union vars ctors _ _) as union) =
    let
        addCtor : Can.CASTC_Ctor -> Dict String Name.CDN_Name (Env.Info Env.Ctor) -> Dict String Name.CDN_Name (Env.Info Env.Ctor)
        addCtor (Can.CASTC_Ctor ctor index _ args) dict =
            Dict.insert identity ctor (Env.Specific home (Env.Ctor home name union index args)) dict
    in
    ( Env.Union (List.length vars) home
    , List.foldl addCtor Dict.empty ctors
    )



-- ALIAS


aliasToType : IO.CEMN_Canonical -> Name.CDN_Name -> I.CEI_Alias -> Maybe ( Env.Type, Env.Exposed Env.Ctor )
aliasToType home name alias =
    Maybe.map (aliasToTypeHelp home name) (I.toPublicAlias alias)


aliasToTypeHelp : IO.CEMN_Canonical -> Name.CDN_Name -> Can.CASTC_Alias -> ( Env.Type, Env.Exposed Env.Ctor )
aliasToTypeHelp home name (Can.CASTC_Alias vars tipe) =
    ( Env.Alias (List.length vars) home vars tipe
    , case tipe of
        Can.CASTC_TRecord fields Nothing ->
            let
                avars : List ( Name.CDN_Name, Can.CASTC_Type )
                avars =
                    List.map (\var -> ( var, Can.CASTC_TVar var )) vars

                alias_ : Can.CASTC_Type
                alias_ =
                    List.foldr
                        (\( _, t1 ) t2 -> Can.CASTC_TLambda t1 t2)
                        (Can.CASTC_TAlias home name avars (Can.CASTC_Filled tipe))
                        (Can.fieldsToList fields)
            in
            Dict.singleton identity name (Env.Specific home (Env.RecordCtor home vars alias_))

        _ ->
            Dict.empty
    )



-- BINOP


binopToBinop : IO.CEMN_Canonical -> Name.CDN_Name -> I.CEI_Binop -> Env.Info Env.Binop
binopToBinop home op (I.CEI_Binop name annotation associativity precedence) =
    Env.Specific home (Env.Binop op home name annotation associativity precedence)



-- ADD EXPOSED VALUE


addExposedValue :
    IO.CEMN_Canonical
    -> Env.Exposed Can.CASTC_Annotation
    -> Dict String Name.CDN_Name ( Env.Type, Env.Exposed Env.Ctor )
    -> Dict String Name.CDN_Name I.CEI_Binop
    -> State
    -> Src.CASTS_Exposed
    -> FResult i w State
addExposedValue home vars types binops state exposed =
    case exposed of
        Src.CASTS_Lower (A.CRA_At region name) ->
            case Dict.get identity name vars of
                Just info ->
                    R.ok { state | vars = Utils.mapInsertWith identity Env.mergeInfo name info state.vars }

                Nothing ->
                    R.throw (Error.ImportExposingNotFound region home name (Dict.keys compare vars))

        Src.CASTS_Upper (A.CRA_At region name) privacy ->
            case privacy of
                Src.CASTS_Private ->
                    case Dict.get identity name types of
                        Just ( tipe, ctors ) ->
                            case tipe of
                                Env.Union _ _ ->
                                    let
                                        ts2 : Dict String Name.CDN_Name (Env.Info Env.Type)
                                        ts2 =
                                            Dict.insert identity name (Env.Specific home tipe) state.types
                                    in
                                    R.ok { state | types = ts2 }

                                Env.Alias _ _ _ _ ->
                                    let
                                        ts2 : Dict String Name.CDN_Name (Env.Info Env.Type)
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

                Src.CASTS_Public dotDotRegion ->
                    case Dict.get identity name types of
                        Just ( tipe, ctors ) ->
                            case tipe of
                                Env.Union _ _ ->
                                    let
                                        ts2 : Dict String Name.CDN_Name (Env.Info Env.Type)
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

        Src.CASTS_Operator region op ->
            case Dict.get identity op binops of
                Just binop ->
                    let
                        bs2 : Dict String Name.CDN_Name (Env.Info Env.Binop)
                        bs2 =
                            Dict.insert identity op (binopToBinop home op binop) state.binops
                    in
                    R.ok { state | binops = bs2 }

                Nothing ->
                    R.throw (Error.ImportExposingNotFound region home op (Dict.keys compare binops))


checkForCtorMistake : Name.CDN_Name -> Dict String Name.CDN_Name ( Env.Type, Env.Exposed Env.Ctor ) -> List Name.CDN_Name
checkForCtorMistake givenName types =
    let
        addMatches : a -> ( b, Dict String Name.CDN_Name (Env.Info Env.Ctor) ) -> List Name.CDN_Name -> List Name.CDN_Name
        addMatches _ ( _, exposedCtors ) matches =
            Dict.foldr compare addMatch matches exposedCtors

        addMatch : Name.CDN_Name -> Env.Info Env.Ctor -> List Name.CDN_Name -> List Name.CDN_Name
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
