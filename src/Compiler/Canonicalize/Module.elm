module Compiler.Canonicalize.Module exposing (MResult, canonicalize)

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Effects as Effects
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Environment.Foreign as Foreign
import Compiler.Canonicalize.Environment.Local as Local
import Compiler.Canonicalize.Expression as Expr
import Compiler.Canonicalize.Pattern as Pattern
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Index as Index
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Result as R
import Compiler.Reporting.Warning as W
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Types as T
import Utils.Crash exposing (crash)



-- RESULT


type alias MResult i w a =
    R.RResult i w T.CREC_Error a



-- MODULES


canonicalize : T.CEP_Name -> Dict String T.CEMN_Raw T.CEI_Interface -> T.CASTS_Module -> MResult i (List W.Warning) Can.Module
canonicalize pkg ifaces ((T.CASTS_Module _ exports docs imports values _ _ binops effects) as modul) =
    let
        home : T.CEMN_Canonical
        home =
            T.CEMN_Canonical pkg (Src.getName modul)

        cbinops : Dict String T.CDN_Name Can.Binop
        cbinops =
            Dict.fromList identity (List.map canonicalizeBinop binops)
    in
    Foreign.createInitialEnv home ifaces imports
        |> R.bind (Local.add modul)
        |> R.bind
            (\( env, cunions, caliases ) ->
                canonicalizeValues env values
                    |> R.bind
                        (\cvalues ->
                            Effects.canonicalize env values cunions effects
                                |> R.bind
                                    (\ceffects ->
                                        canonicalizeExports values cunions caliases cbinops ceffects exports
                                            |> R.fmap
                                                (\cexports ->
                                                    Can.Module home cexports docs cvalues cunions caliases cbinops ceffects
                                                )
                                    )
                        )
            )



-- CANONICALIZE BINOP


canonicalizeBinop : T.CRA_Located T.CASTS_Infix -> ( T.CDN_Name, Can.Binop )
canonicalizeBinop (T.CRA_At _ (T.CASTS_Infix op associativity precedence func)) =
    ( op, Can.Binop_ associativity precedence func )



-- DECLARATIONS / CYCLE DETECTION
--
-- There are two phases of cycle detection:
--
-- 1. Detect cycles using ALL dependencies => needed for type inference
-- 2. Detect cycles using DIRECT dependencies => nonterminating recursion


canonicalizeValues : Env.Env -> List (T.CRA_Located T.CASTS_Value) -> MResult i (List W.Warning) T.CASTC_Decls
canonicalizeValues env values =
    R.traverse (toNodeOne env) values
        |> R.bind (\nodes -> detectCycles (Graph.stronglyConnComp nodes))


detectCycles : List (Graph.SCC NodeTwo) -> MResult i w T.CASTC_Decls
detectCycles sccs =
    case sccs of
        [] ->
            R.ok T.CASTC_SaveTheEnvironment

        scc :: otherSccs ->
            case scc of
                Graph.AcyclicSCC ( def, _, _ ) ->
                    R.fmap (T.CASTC_Declare def) (detectCycles otherSccs)

                Graph.CyclicSCC subNodes ->
                    R.traverse detectBadCycles (Graph.stronglyConnComp subNodes)
                        |> R.bind
                            (\defs ->
                                case defs of
                                    [] ->
                                        detectCycles otherSccs

                                    d :: ds ->
                                        R.fmap (T.CASTC_DeclareRec d ds) (detectCycles otherSccs)
                            )


detectBadCycles : Graph.SCC T.CASTC_Def -> MResult i w T.CASTC_Def
detectBadCycles scc =
    case scc of
        Graph.AcyclicSCC def ->
            R.ok def

        Graph.CyclicSCC [] ->
            crash "The definition of Data.Graph.SCC should not allow empty CyclicSCC!"

        Graph.CyclicSCC (def :: defs) ->
            let
                (T.CRA_At region name) =
                    extractDefName def

                names : List T.CDN_Name
                names =
                    List.map (A.toValue << extractDefName) defs
            in
            R.throw (T.CREC_RecursiveDecl region name names)


extractDefName : T.CASTC_Def -> T.CRA_Located T.CDN_Name
extractDefName def =
    case def of
        T.CASTC_Def name _ _ ->
            name

        T.CASTC_TypedDef name _ _ _ _ ->
            name



-- DECLARATIONS / CYCLE DETECTION SETUP
--
-- toNodeOne and toNodeTwo set up nodes for the two cycle detection phases.
--
-- Phase one nodes track ALL dependencies.
-- This allows us to find cyclic values for type inference.


type alias NodeOne =
    ( NodeTwo, T.CDN_Name, List T.CDN_Name )



-- Phase two nodes track DIRECT dependencies.
-- This allows us to detect cycles that definitely do not terminate.


type alias NodeTwo =
    ( T.CASTC_Def, T.CDN_Name, List T.CDN_Name )


toNodeOne : Env.Env -> T.CRA_Located T.CASTS_Value -> MResult i (List W.Warning) NodeOne
toNodeOne env (T.CRA_At _ (T.CASTS_Value ((T.CRA_At _ name) as aname) srcArgs body maybeType)) =
    case maybeType of
        Nothing ->
            Pattern.verify (T.CREC_DPFuncArgs name)
                (R.traverse (Pattern.canonicalize env) srcArgs)
                |> R.bind
                    (\( args, argBindings ) ->
                        Env.addLocals argBindings env
                            |> R.bind
                                (\newEnv ->
                                    Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv body)
                                        |> R.fmap
                                            (\( cbody, freeLocals ) ->
                                                let
                                                    def : T.CASTC_Def
                                                    def =
                                                        T.CASTC_Def aname args cbody
                                                in
                                                ( toNodeTwo name srcArgs def freeLocals
                                                , name
                                                , Dict.keys compare freeLocals
                                                )
                                            )
                                )
                    )

        Just srcType ->
            Type.toAnnotation env srcType
                |> R.bind
                    (\(T.CASTC_Forall freeVars tipe) ->
                        Pattern.verify (T.CREC_DPFuncArgs name)
                            (Expr.gatherTypedArgs env name srcArgs tipe Index.first [])
                            |> R.bind
                                (\( ( args, resultType ), argBindings ) ->
                                    Env.addLocals argBindings env
                                        |> R.bind
                                            (\newEnv ->
                                                Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv body)
                                                    |> R.fmap
                                                        (\( cbody, freeLocals ) ->
                                                            let
                                                                def : T.CASTC_Def
                                                                def =
                                                                    T.CASTC_TypedDef aname freeVars args cbody resultType
                                                            in
                                                            ( toNodeTwo name srcArgs def freeLocals
                                                            , name
                                                            , Dict.keys compare freeLocals
                                                            )
                                                        )
                                            )
                                )
                    )


toNodeTwo : T.CDN_Name -> List arg -> T.CASTC_Def -> Expr.FreeLocals -> NodeTwo
toNodeTwo name args def freeLocals =
    case args of
        [] ->
            ( def, name, Dict.foldr compare addDirects [] freeLocals )

        _ ->
            ( def, name, [] )


addDirects : T.CDN_Name -> Expr.Uses -> List T.CDN_Name -> List T.CDN_Name
addDirects name (Expr.Uses { direct }) directDeps =
    if direct > 0 then
        name :: directDeps

    else
        directDeps



-- CANONICALIZE EXPORTS


canonicalizeExports :
    List (T.CRA_Located T.CASTS_Value)
    -> Dict String T.CDN_Name union
    -> Dict String T.CDN_Name alias
    -> Dict String T.CDN_Name binop
    -> Can.Effects
    -> T.CRA_Located T.CASTS_Exposing
    -> MResult i w Can.Exports
canonicalizeExports values unions aliases binops effects (T.CRA_At region exposing_) =
    case exposing_ of
        T.CASTS_Open ->
            R.ok (Can.ExportEverything region)

        T.CASTS_Explicit exposeds ->
            let
                names : Dict String T.CDN_Name ()
                names =
                    Dict.fromList identity (List.map valueToName values)
            in
            R.traverse (checkExposed names unions aliases binops effects) exposeds
                |> R.bind
                    (\infos ->
                        Dups.detect T.CREC_ExportDuplicate (Dups.unions infos)
                            |> R.fmap Can.Export
                    )


valueToName : T.CRA_Located T.CASTS_Value -> ( T.CDN_Name, () )
valueToName (T.CRA_At _ (T.CASTS_Value (T.CRA_At _ name) _ _ _)) =
    ( name, () )


checkExposed :
    Dict String T.CDN_Name value
    -> Dict String T.CDN_Name union
    -> Dict String T.CDN_Name alias
    -> Dict String T.CDN_Name binop
    -> Can.Effects
    -> T.CASTS_Exposed
    -> MResult i w (Dups.Tracker (T.CRA_Located Can.Export))
checkExposed values unions aliases binops effects exposed =
    case exposed of
        T.CASTS_Lower (T.CRA_At region name) ->
            if Dict.member identity name values then
                ok name region Can.ExportValue

            else
                case checkPorts effects name of
                    Nothing ->
                        ok name region Can.ExportPort

                    Just ports ->
                        R.throw (T.CREC_ExportNotFound region T.CREC_BadVar name (ports ++ Dict.keys compare values))

        T.CASTS_Operator region name ->
            if Dict.member identity name binops then
                ok name region Can.ExportBinop

            else
                R.throw (T.CREC_ExportNotFound region T.CREC_BadOp name (Dict.keys compare binops))

        T.CASTS_Upper (T.CRA_At region name) (T.CASTS_Public dotDotRegion) ->
            if Dict.member identity name unions then
                ok name region Can.ExportUnionOpen

            else if Dict.member identity name aliases then
                R.throw (T.CREC_ExportOpenAlias dotDotRegion name)

            else
                R.throw (T.CREC_ExportNotFound region T.CREC_BadType name (Dict.keys compare unions ++ Dict.keys compare aliases))

        T.CASTS_Upper (T.CRA_At region name) T.CASTS_Private ->
            if Dict.member identity name unions then
                ok name region Can.ExportUnionClosed

            else if Dict.member identity name aliases then
                ok name region Can.ExportAlias

            else
                R.throw (T.CREC_ExportNotFound region T.CREC_BadType name (Dict.keys compare unions ++ Dict.keys compare aliases))


checkPorts : Can.Effects -> T.CDN_Name -> Maybe (List T.CDN_Name)
checkPorts effects name =
    case effects of
        Can.NoEffects ->
            Just []

        Can.Ports ports ->
            if Dict.member identity name ports then
                Nothing

            else
                Just (Dict.keys compare ports)

        Can.Manager _ _ _ _ ->
            Just []


ok : T.CDN_Name -> T.CRA_Region -> Can.Export -> MResult i w (Dups.Tracker (T.CRA_Located Can.Export))
ok name region export =
    R.ok (Dups.one name region (T.CRA_At region export))
