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
    R.RResult i w E.Error a


type alias Annotations =
    Dict String T.CDN_Name T.CASTC_Annotation


optimize : Annotations -> Can.Module -> MResult i (List W.Warning) Opt.LocalGraph
optimize annotations (Can.Module home _ _ decls unions aliases _ effects) =
    addDecls home annotations decls <|
        addEffects home effects <|
            addUnions home unions <|
                addAliases home aliases <|
                    Opt.LocalGraph Nothing Dict.empty Dict.empty



-- UNION


type alias Nodes =
    Dict (List String) Opt.Global Opt.Node


addUnions : T.CEMN_Canonical -> Dict String T.CDN_Name T.CASTC_Union -> Opt.LocalGraph -> Opt.LocalGraph
addUnions home unions (Opt.LocalGraph main nodes fields) =
    Opt.LocalGraph main (Dict.foldr compare (\_ -> addUnion home) nodes unions) fields


addUnion : T.CEMN_Canonical -> T.CASTC_Union -> Nodes -> Nodes
addUnion home (T.CASTC_Union _ ctors _ opts) nodes =
    List.foldl (addCtorNode home opts) nodes ctors


addCtorNode : T.CEMN_Canonical -> T.CASTC_CtorOpts -> T.CASTC_Ctor -> Nodes -> Nodes
addCtorNode home opts (T.CASTC_Ctor name index numArgs _) nodes =
    let
        node : Opt.Node
        node =
            case opts of
                T.CASTC_Normal ->
                    Opt.Ctor index numArgs

                T.CASTC_Unbox ->
                    Opt.Box

                T.CASTC_Enum ->
                    Opt.Enum index
    in
    Dict.insert Opt.toComparableGlobal (Opt.Global home name) node nodes



-- ALIAS


addAliases : T.CEMN_Canonical -> Dict String T.CDN_Name T.CASTC_Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAliases home aliases graph =
    Dict.foldr compare (addAlias home) graph aliases


addAlias : T.CEMN_Canonical -> T.CDN_Name -> T.CASTC_Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAlias home name (T.CASTC_Alias _ tipe) ((Opt.LocalGraph main nodes fieldCounts) as graph) =
    case tipe of
        T.CASTC_TRecord fields Nothing ->
            let
                function : Opt.Expr
                function =
                    Opt.Function (List.map Tuple.first (Can.fieldsToList fields)) <|
                        Opt.Record <|
                            Dict.map (\field _ -> Opt.VarLocal field) fields

                node : Opt.Node
                node =
                    Opt.Define function EverySet.empty
            in
            Opt.LocalGraph
                main
                (Dict.insert Opt.toComparableGlobal (Opt.Global home name) node nodes)
                (Dict.foldr compare addRecordCtorField fieldCounts fields)

        _ ->
            graph


addRecordCtorField : T.CDN_Name -> T.CASTC_FieldType -> Dict String T.CDN_Name Int -> Dict String T.CDN_Name Int
addRecordCtorField name _ fields =
    Utils.mapInsertWith identity (+) name 1 fields



-- ADD EFFECTS


addEffects : T.CEMN_Canonical -> Can.Effects -> Opt.LocalGraph -> Opt.LocalGraph
addEffects home effects ((Opt.LocalGraph main nodes fields) as graph) =
    case effects of
        Can.NoEffects ->
            graph

        Can.Ports ports ->
            Dict.foldr compare (addPort home) graph ports

        Can.Manager _ _ _ manager ->
            let
                fx : Opt.Global
                fx =
                    Opt.Global home "$fx$"

                cmd : Opt.Global
                cmd =
                    Opt.Global home "command"

                sub : Opt.Global
                sub =
                    Opt.Global home "subscription"

                link : Opt.Node
                link =
                    Opt.Link fx

                newNodes : Dict (List String) Opt.Global Opt.Node
                newNodes =
                    case manager of
                        Can.Cmd _ ->
                            Dict.insert Opt.toComparableGlobal cmd link <|
                                Dict.insert Opt.toComparableGlobal fx (Opt.Manager Opt.Cmd) nodes

                        Can.Sub _ ->
                            Dict.insert Opt.toComparableGlobal sub link <|
                                Dict.insert Opt.toComparableGlobal fx (Opt.Manager Opt.Sub) nodes

                        Can.Fx _ _ ->
                            Dict.insert Opt.toComparableGlobal cmd link <|
                                Dict.insert Opt.toComparableGlobal sub link <|
                                    Dict.insert Opt.toComparableGlobal fx (Opt.Manager Opt.Fx) nodes
            in
            Opt.LocalGraph main newNodes fields


addPort : T.CEMN_Canonical -> T.CDN_Name -> Can.Port -> Opt.LocalGraph -> Opt.LocalGraph
addPort home name port_ graph =
    case port_ of
        Can.Incoming { payload } ->
            let
                ( deps, fields, decoder ) =
                    Names.run (Port.toDecoder payload)

                node : Opt.Node
                node =
                    Opt.PortIncoming decoder deps
            in
            addToGraph (Opt.Global home name) node fields graph

        Can.Outgoing { payload } ->
            let
                ( deps, fields, encoder ) =
                    Names.run (Port.toEncoder payload)

                node : Opt.Node
                node =
                    Opt.PortOutgoing encoder deps
            in
            addToGraph (Opt.Global home name) node fields graph



-- HELPER


addToGraph : Opt.Global -> Opt.Node -> Dict String T.CDN_Name Int -> Opt.LocalGraph -> Opt.LocalGraph
addToGraph name node fields (Opt.LocalGraph main nodes fieldCounts) =
    Opt.LocalGraph
        main
        (Dict.insert Opt.toComparableGlobal name node nodes)
        (Utils.mapUnionWith identity compare (+) fields fieldCounts)



-- ADD DECLS


addDecls : T.CEMN_Canonical -> Annotations -> Can.Decls -> Opt.LocalGraph -> MResult i (List W.Warning) Opt.LocalGraph
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
                    R.throw <| E.BadCycle region (defToName d) (List.map defToName ds)

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


addDef : T.CEMN_Canonical -> Annotations -> Can.Def -> Opt.LocalGraph -> MResult i (List W.Warning) Opt.LocalGraph
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


addDefHelp : T.CRA_Region -> Annotations -> T.CEMN_Canonical -> T.CDN_Name -> List Can.Pattern -> Can.Expr -> Opt.LocalGraph -> MResult i w Opt.LocalGraph
addDefHelp region annotations home name args body ((Opt.LocalGraph _ nodes fieldCounts) as graph) =
    if name /= Name.main_ then
        R.ok (addDefNode home name args body EverySet.empty graph)

    else
        let
            (T.CASTC_Forall _ tipe) =
                Utils.find identity name annotations

            addMain : ( EverySet (List String) Opt.Global, Dict String T.CDN_Name Int, Opt.Main ) -> Opt.LocalGraph
            addMain ( deps, fields, main ) =
                addDefNode home name args body deps <|
                    Opt.LocalGraph (Just main) nodes (Utils.mapUnionWith identity compare (+) fields fieldCounts)
        in
        case Type.deepDealias tipe of
            T.CASTC_TType hm nm [ _ ] ->
                if hm == ModuleName.virtualDom && nm == Name.node then
                    R.ok <| addMain <| Names.run <| Names.registerKernel Name.virtualDom Opt.Static

                else
                    R.throw (E.BadType region tipe)

            T.CASTC_TType hm nm [ flags, _, message ] ->
                if hm == ModuleName.platform && nm == Name.program then
                    case Effects.checkPayload flags of
                        Ok () ->
                            R.ok <| addMain <| Names.run <| Names.fmap (Opt.Dynamic message) <| Port.toFlagsDecoder flags

                        Err ( subType, invalidPayload ) ->
                            R.throw (E.BadFlags region subType invalidPayload)

                else
                    R.throw (E.BadType region tipe)

            _ ->
                R.throw (E.BadType region tipe)


addDefNode : T.CEMN_Canonical -> T.CDN_Name -> List Can.Pattern -> Can.Expr -> EverySet (List String) Opt.Global -> Opt.LocalGraph -> Opt.LocalGraph
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
                                                Opt.Function argNames <|
                                                    List.foldr Opt.Destruct obody destructors
                                            )
                                )
    in
    addToGraph (Opt.Global home name) (Opt.Define def (EverySet.union deps mainDeps)) fields graph



-- ADD RECURSIVE DEFS


type State
    = State
        { values : List ( T.CDN_Name, Opt.Expr )
        , functions : List Opt.Def
        }


addRecDefs : T.CEMN_Canonical -> List Can.Def -> Opt.LocalGraph -> Opt.LocalGraph
addRecDefs home defs (Opt.LocalGraph main nodes fieldCounts) =
    let
        names : List T.CDN_Name
        names =
            List.reverse (List.map toName defs)

        cycleName : Opt.Global
        cycleName =
            Opt.Global home (Name.fromManyNames names)

        cycle : EverySet String T.CDN_Name
        cycle =
            List.foldr addValueName EverySet.empty defs

        links : Dict (List String) Opt.Global Opt.Node
        links =
            List.foldr (addLink home (Opt.Link cycleName)) Dict.empty defs

        ( deps, fields, State { values, functions } ) =
            Names.run <|
                List.foldl (\def -> Names.bind (\state -> addRecDef cycle state def))
                    (Names.pure (State { values = [], functions = [] }))
                    defs
    in
    Opt.LocalGraph
        main
        (Dict.insert Opt.toComparableGlobal cycleName (Opt.Cycle names values functions deps) (Dict.union links nodes))
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


addLink : T.CEMN_Canonical -> Opt.Node -> Can.Def -> Dict (List String) Opt.Global Opt.Node -> Dict (List String) Opt.Global Opt.Node
addLink home link def links =
    case def of
        Can.Def (T.CRA_At _ name) _ _ ->
            Dict.insert Opt.toComparableGlobal (Opt.Global home name) link links

        Can.TypedDef (T.CRA_At _ name) _ _ _ _ ->
            Dict.insert Opt.toComparableGlobal (Opt.Global home name) link links



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
