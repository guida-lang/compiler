module Compiler.Optimize.Module exposing (Annotations, MResult, optimize)

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Type as Type
import Compiler.Canonicalize.Effects as Effects
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Optimize.Expression as Expr
import Compiler.Optimize.Names as Names
import Compiler.Optimize.Port as Port
import Compiler.Reporting.Error.Main as E
import Compiler.Reporting.Result as R
import Compiler.Reporting.Warning as W
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Types as T
import Utils.Main as Utils



-- OPTIMIZE


type alias MResult i w a =
    R.RResult i w E.CREM_Error a


type alias Annotations =
    Dict String T.CDN_Name T.CASTC_Annotation


optimize : Annotations -> Can.Module -> MResult i (List W.Warning) T.CASTO_LocalGraph
optimize annotations (Can.Module home _ _ decls unions aliases _ effects) =
    addDecls home annotations decls <|
        addEffects home effects <|
            addUnions home unions <|
                addAliases home aliases <|
                    T.CASTO_LocalGraph Nothing Dict.empty Dict.empty



-- UNION


type alias Nodes =
    Dict (List String) T.CASTO_Global T.CASTO_Node


addUnions : T.CEMN_Canonical -> Dict String T.CDN_Name T.CASTC_Union -> T.CASTO_LocalGraph -> T.CASTO_LocalGraph
addUnions home unions (T.CASTO_LocalGraph main nodes fields) =
    T.CASTO_LocalGraph main (Dict.foldr compare (\_ -> addUnion home) nodes unions) fields


addUnion : T.CEMN_Canonical -> T.CASTC_Union -> Nodes -> Nodes
addUnion home (T.CASTC_Union _ ctors _ opts) nodes =
    List.foldl (addCtorNode home opts) nodes ctors


addCtorNode : T.CEMN_Canonical -> T.CASTC_CtorOpts -> T.CASTC_Ctor -> Nodes -> Nodes
addCtorNode home opts (T.CASTC_Ctor name index numArgs _) nodes =
    let
        node : T.CASTO_Node
        node =
            case opts of
                T.CASTC_Normal ->
                    T.CASTO_Ctor index numArgs

                T.CASTC_Unbox ->
                    T.CASTO_Box

                T.CASTC_Enum ->
                    T.CASTO_Enum index
    in
    Dict.insert Opt.toComparableGlobal (T.CASTO_Global home name) node nodes



-- ALIAS


addAliases : T.CEMN_Canonical -> Dict String T.CDN_Name T.CASTC_Alias -> T.CASTO_LocalGraph -> T.CASTO_LocalGraph
addAliases home aliases graph =
    Dict.foldr compare (addAlias home) graph aliases


addAlias : T.CEMN_Canonical -> T.CDN_Name -> T.CASTC_Alias -> T.CASTO_LocalGraph -> T.CASTO_LocalGraph
addAlias home name (T.CASTC_Alias _ tipe) ((T.CASTO_LocalGraph main nodes fieldCounts) as graph) =
    case tipe of
        T.CASTC_TRecord fields Nothing ->
            let
                function : T.CASTO_Expr
                function =
                    T.CASTO_Function (List.map Tuple.first (Can.fieldsToList fields)) <|
                        T.CASTO_Record <|
                            Dict.map (\field _ -> T.CASTO_VarLocal field) fields

                node : T.CASTO_Node
                node =
                    T.CASTO_Define function EverySet.empty
            in
            T.CASTO_LocalGraph
                main
                (Dict.insert Opt.toComparableGlobal (T.CASTO_Global home name) node nodes)
                (Dict.foldr compare addRecordCtorField fieldCounts fields)

        _ ->
            graph


addRecordCtorField : T.CDN_Name -> T.CASTC_FieldType -> Dict String T.CDN_Name Int -> Dict String T.CDN_Name Int
addRecordCtorField name _ fields =
    Utils.mapInsertWith identity (+) name 1 fields



-- ADD EFFECTS


addEffects : T.CEMN_Canonical -> Can.Effects -> T.CASTO_LocalGraph -> T.CASTO_LocalGraph
addEffects home effects ((T.CASTO_LocalGraph main nodes fields) as graph) =
    case effects of
        Can.NoEffects ->
            graph

        Can.Ports ports ->
            Dict.foldr compare (addPort home) graph ports

        Can.Manager _ _ _ manager ->
            let
                fx : T.CASTO_Global
                fx =
                    T.CASTO_Global home "$fx$"

                cmd : T.CASTO_Global
                cmd =
                    T.CASTO_Global home "command"

                sub : T.CASTO_Global
                sub =
                    T.CASTO_Global home "subscription"

                link : T.CASTO_Node
                link =
                    T.CASTO_Link fx

                newNodes : Dict (List String) T.CASTO_Global T.CASTO_Node
                newNodes =
                    case manager of
                        Can.Cmd _ ->
                            Dict.insert Opt.toComparableGlobal cmd link <|
                                Dict.insert Opt.toComparableGlobal fx (T.CASTO_Manager T.CASTO_Cmd) nodes

                        Can.Sub _ ->
                            Dict.insert Opt.toComparableGlobal sub link <|
                                Dict.insert Opt.toComparableGlobal fx (T.CASTO_Manager T.CASTO_Sub) nodes

                        Can.Fx _ _ ->
                            Dict.insert Opt.toComparableGlobal cmd link <|
                                Dict.insert Opt.toComparableGlobal sub link <|
                                    Dict.insert Opt.toComparableGlobal fx (T.CASTO_Manager T.CASTO_Fx) nodes
            in
            T.CASTO_LocalGraph main newNodes fields


addPort : T.CEMN_Canonical -> T.CDN_Name -> Can.Port -> T.CASTO_LocalGraph -> T.CASTO_LocalGraph
addPort home name port_ graph =
    case port_ of
        Can.Incoming { payload } ->
            let
                ( deps, fields, decoder ) =
                    Names.run (Port.toDecoder payload)

                node : T.CASTO_Node
                node =
                    T.CASTO_PortIncoming decoder deps
            in
            addToGraph (T.CASTO_Global home name) node fields graph

        Can.Outgoing { payload } ->
            let
                ( deps, fields, encoder ) =
                    Names.run (Port.toEncoder payload)

                node : T.CASTO_Node
                node =
                    T.CASTO_PortOutgoing encoder deps
            in
            addToGraph (T.CASTO_Global home name) node fields graph



-- HELPER


addToGraph : T.CASTO_Global -> T.CASTO_Node -> Dict String T.CDN_Name Int -> T.CASTO_LocalGraph -> T.CASTO_LocalGraph
addToGraph name node fields (T.CASTO_LocalGraph main nodes fieldCounts) =
    T.CASTO_LocalGraph
        main
        (Dict.insert Opt.toComparableGlobal name node nodes)
        (Utils.mapUnionWith identity compare (+) fields fieldCounts)



-- ADD DECLS


addDecls : T.CEMN_Canonical -> Annotations -> Can.Decls -> T.CASTO_LocalGraph -> MResult i (List W.Warning) T.CASTO_LocalGraph
addDecls home annotations decls graph =
    case decls of
        Can.Declare def subDecls ->
            addDef home annotations def graph
                |> R.bind (addDecls home annotations subDecls)

        Can.DeclareRec d ds subDecls ->
            let
                defs : List Can.Def
                defs =
                    d :: ds
            in
            case findMain defs of
                Nothing ->
                    addDecls home annotations subDecls (addRecDefs home defs graph)

                Just region ->
                    R.throw <| E.CREM_BadCycle region (defToName d) (List.map defToName ds)

        Can.SaveTheEnvironment ->
            R.ok graph


findMain : List Can.Def -> Maybe T.CRA_Region
findMain defs =
    case defs of
        [] ->
            Nothing

        def :: rest ->
            case def of
                Can.Def (T.CRA_At region name) _ _ ->
                    if name == Name.main_ then
                        Just region

                    else
                        findMain rest

                Can.TypedDef (T.CRA_At region name) _ _ _ _ ->
                    if name == Name.main_ then
                        Just region

                    else
                        findMain rest


defToName : Can.Def -> T.CDN_Name
defToName def =
    case def of
        Can.Def (T.CRA_At _ name) _ _ ->
            name

        Can.TypedDef (T.CRA_At _ name) _ _ _ _ ->
            name



-- ADD DEFS


addDef : T.CEMN_Canonical -> Annotations -> Can.Def -> T.CASTO_LocalGraph -> MResult i (List W.Warning) T.CASTO_LocalGraph
addDef home annotations def graph =
    case def of
        Can.Def (T.CRA_At region name) args body ->
            let
                (T.CASTC_Forall _ tipe) =
                    Utils.find identity name annotations
            in
            addDefHelp region annotations home name args body graph
                |> R.then_ (R.warn (W.MissingTypeAnnotation region name tipe))

        Can.TypedDef (T.CRA_At region name) _ typedArgs body _ ->
            addDefHelp region annotations home name (List.map Tuple.first typedArgs) body graph


addDefHelp : T.CRA_Region -> Annotations -> T.CEMN_Canonical -> T.CDN_Name -> List Can.Pattern -> Can.Expr -> T.CASTO_LocalGraph -> MResult i w T.CASTO_LocalGraph
addDefHelp region annotations home name args body ((T.CASTO_LocalGraph _ nodes fieldCounts) as graph) =
    if name /= Name.main_ then
        R.ok (addDefNode home name args body EverySet.empty graph)

    else
        let
            (T.CASTC_Forall _ tipe) =
                Utils.find identity name annotations

            addMain : ( EverySet (List String) T.CASTO_Global, Dict String T.CDN_Name Int, T.CASTO_Main ) -> T.CASTO_LocalGraph
            addMain ( deps, fields, main ) =
                addDefNode home name args body deps <|
                    T.CASTO_LocalGraph (Just main) nodes (Utils.mapUnionWith identity compare (+) fields fieldCounts)
        in
        case Type.deepDealias tipe of
            T.CASTC_TType hm nm [ _ ] ->
                if hm == ModuleName.virtualDom && nm == Name.node then
                    R.ok <| addMain <| Names.run <| Names.registerKernel Name.virtualDom T.CASTO_Static

                else
                    R.throw (E.CREM_BadType region tipe)

            T.CASTC_TType hm nm [ flags, _, message ] ->
                if hm == ModuleName.platform && nm == Name.program then
                    case Effects.checkPayload flags of
                        Ok () ->
                            R.ok <| addMain <| Names.run <| Names.fmap (T.CASTO_Dynamic message) <| Port.toFlagsDecoder flags

                        Err ( subType, invalidPayload ) ->
                            R.throw (E.CREM_BadFlags region subType invalidPayload)

                else
                    R.throw (E.CREM_BadType region tipe)

            _ ->
                R.throw (E.CREM_BadType region tipe)


addDefNode : T.CEMN_Canonical -> T.CDN_Name -> List Can.Pattern -> Can.Expr -> EverySet (List String) T.CASTO_Global -> T.CASTO_LocalGraph -> T.CASTO_LocalGraph
addDefNode home name args body mainDeps graph =
    let
        ( deps, fields, def ) =
            Names.run <|
                case args of
                    [] ->
                        Expr.optimize EverySet.empty body

                    _ ->
                        Expr.destructArgs args
                            |> Names.bind
                                (\( argNames, destructors ) ->
                                    Expr.optimize EverySet.empty body
                                        |> Names.fmap
                                            (\obody ->
                                                T.CASTO_Function argNames <|
                                                    List.foldr T.CASTO_Destruct obody destructors
                                            )
                                )
    in
    addToGraph (T.CASTO_Global home name) (T.CASTO_Define def (EverySet.union deps mainDeps)) fields graph



-- ADD RECURSIVE DEFS


type State
    = State
        { values : List ( T.CDN_Name, T.CASTO_Expr )
        , functions : List T.CASTO_Def
        }


addRecDefs : T.CEMN_Canonical -> List Can.Def -> T.CASTO_LocalGraph -> T.CASTO_LocalGraph
addRecDefs home defs (T.CASTO_LocalGraph main nodes fieldCounts) =
    let
        names : List T.CDN_Name
        names =
            List.reverse (List.map toName defs)

        cycleName : T.CASTO_Global
        cycleName =
            T.CASTO_Global home (Name.fromManyNames names)

        cycle : EverySet String T.CDN_Name
        cycle =
            List.foldr addValueName EverySet.empty defs

        links : Dict (List String) T.CASTO_Global T.CASTO_Node
        links =
            List.foldr (addLink home (T.CASTO_Link cycleName)) Dict.empty defs

        ( deps, fields, State { values, functions } ) =
            Names.run <|
                List.foldl (\def -> Names.bind (\state -> addRecDef cycle state def))
                    (Names.pure (State { values = [], functions = [] }))
                    defs
    in
    T.CASTO_LocalGraph
        main
        (Dict.insert Opt.toComparableGlobal cycleName (T.CASTO_Cycle names values functions deps) (Dict.union links nodes))
        (Utils.mapUnionWith identity compare (+) fields fieldCounts)


toName : Can.Def -> T.CDN_Name
toName def =
    case def of
        Can.Def (T.CRA_At _ name) _ _ ->
            name

        Can.TypedDef (T.CRA_At _ name) _ _ _ _ ->
            name


addValueName : Can.Def -> EverySet String T.CDN_Name -> EverySet String T.CDN_Name
addValueName def names =
    case def of
        Can.Def (T.CRA_At _ name) args _ ->
            if List.isEmpty args then
                EverySet.insert identity name names

            else
                names

        Can.TypedDef (T.CRA_At _ name) _ args _ _ ->
            if List.isEmpty args then
                EverySet.insert identity name names

            else
                names


addLink : T.CEMN_Canonical -> T.CASTO_Node -> Can.Def -> Dict (List String) T.CASTO_Global T.CASTO_Node -> Dict (List String) T.CASTO_Global T.CASTO_Node
addLink home link def links =
    case def of
        Can.Def (T.CRA_At _ name) _ _ ->
            Dict.insert Opt.toComparableGlobal (T.CASTO_Global home name) link links

        Can.TypedDef (T.CRA_At _ name) _ _ _ _ ->
            Dict.insert Opt.toComparableGlobal (T.CASTO_Global home name) link links



-- ADD RECURSIVE DEFS


addRecDef : EverySet String T.CDN_Name -> State -> Can.Def -> Names.Tracker State
addRecDef cycle state def =
    case def of
        Can.Def (T.CRA_At _ name) args body ->
            addRecDefHelp cycle state name args body

        Can.TypedDef (T.CRA_At _ name) _ args body _ ->
            addRecDefHelp cycle state name (List.map Tuple.first args) body


addRecDefHelp : EverySet String T.CDN_Name -> State -> T.CDN_Name -> List Can.Pattern -> Can.Expr -> Names.Tracker State
addRecDefHelp cycle (State { values, functions }) name args body =
    case args of
        [] ->
            Expr.optimize cycle body
                |> Names.fmap
                    (\obody ->
                        State
                            { values = ( name, obody ) :: values
                            , functions = functions
                            }
                    )

        _ :: _ ->
            Expr.optimizePotentialTailCall cycle name args body
                |> Names.fmap
                    (\odef ->
                        State
                            { values = values
                            , functions = odef :: functions
                            }
                    )
