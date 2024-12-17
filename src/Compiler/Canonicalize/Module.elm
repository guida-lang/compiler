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
import Compiler.Data.Name as Name
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Compiler.Reporting.Warning as W
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO
import Utils.Crash exposing (crash)



-- RESULT


type alias MResult i w a =
    R.RResult i w Error.Error a



-- MODULES


canonicalize : Pkg.CEP_Name -> Dict String ModuleName.CEMN_Raw I.CEI_Interface -> Src.CASTS_Module -> MResult i (List W.Warning) Can.Module
canonicalize pkg ifaces ((Src.CASTS_Module _ exports docs imports values _ _ binops effects) as modul) =
    let
        home : IO.CEMN_Canonical
        home =
            IO.CEMN_Canonical pkg (Src.getName modul)

        cbinops : Dict String Name.CDN_Name Can.Binop
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


canonicalizeBinop : A.CRA_Located Src.CASTS_Infix -> ( Name.CDN_Name, Can.Binop )
canonicalizeBinop (A.CRA_At _ (Src.CASTS_Infix op associativity precedence func)) =
    ( op, Can.Binop_ associativity precedence func )



-- DECLARATIONS / CYCLE DETECTION
--
-- There are two phases of cycle detection:
--
-- 1. Detect cycles using ALL dependencies => needed for type inference
-- 2. Detect cycles using DIRECT dependencies => nonterminating recursion


canonicalizeValues : Env.Env -> List (A.CRA_Located Src.CASTS_Value) -> MResult i (List W.Warning) Can.Decls
canonicalizeValues env values =
    R.traverse (toNodeOne env) values
        |> R.bind (\nodes -> detectCycles (Graph.stronglyConnComp nodes))


detectCycles : List (Graph.SCC NodeTwo) -> MResult i w Can.Decls
detectCycles sccs =
    case sccs of
        [] ->
            R.ok Can.SaveTheEnvironment

        scc :: otherSccs ->
            case scc of
                Graph.AcyclicSCC ( def, _, _ ) ->
                    R.fmap (Can.Declare def) (detectCycles otherSccs)

                Graph.CyclicSCC subNodes ->
                    R.traverse detectBadCycles (Graph.stronglyConnComp subNodes)
                        |> R.bind
                            (\defs ->
                                case defs of
                                    [] ->
                                        detectCycles otherSccs

                                    d :: ds ->
                                        R.fmap (Can.DeclareRec d ds) (detectCycles otherSccs)
                            )


detectBadCycles : Graph.SCC Can.Def -> MResult i w Can.Def
detectBadCycles scc =
    case scc of
        Graph.AcyclicSCC def ->
            R.ok def

        Graph.CyclicSCC [] ->
            crash "The definition of Data.Graph.SCC should not allow empty CyclicSCC!"

        Graph.CyclicSCC (def :: defs) ->
            let
                (A.CRA_At region name) =
                    extractDefName def

                names : List Name.CDN_Name
                names =
                    List.map (A.toValue << extractDefName) defs
            in
            R.throw (Error.RecursiveDecl region name names)


extractDefName : Can.Def -> A.CRA_Located Name.CDN_Name
extractDefName def =
    case def of
        Can.Def name _ _ ->
            name

        Can.TypedDef name _ _ _ _ ->
            name



-- DECLARATIONS / CYCLE DETECTION SETUP
--
-- toNodeOne and toNodeTwo set up nodes for the two cycle detection phases.
--
-- Phase one nodes track ALL dependencies.
-- This allows us to find cyclic values for type inference.


type alias NodeOne =
    ( NodeTwo, Name.CDN_Name, List Name.CDN_Name )



-- Phase two nodes track DIRECT dependencies.
-- This allows us to detect cycles that definitely do not terminate.


type alias NodeTwo =
    ( Can.Def, Name.CDN_Name, List Name.CDN_Name )


toNodeOne : Env.Env -> A.CRA_Located Src.CASTS_Value -> MResult i (List W.Warning) NodeOne
toNodeOne env (A.CRA_At _ (Src.CASTS_Value ((A.CRA_At _ name) as aname) srcArgs body maybeType)) =
    case maybeType of
        Nothing ->
            Pattern.verify (Error.DPFuncArgs name)
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
                                                    def : Can.Def
                                                    def =
                                                        Can.Def aname args cbody
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
                    (\(Can.CASTC_Forall freeVars tipe) ->
                        Pattern.verify (Error.DPFuncArgs name)
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
                                                                def : Can.Def
                                                                def =
                                                                    Can.TypedDef aname freeVars args cbody resultType
                                                            in
                                                            ( toNodeTwo name srcArgs def freeLocals
                                                            , name
                                                            , Dict.keys compare freeLocals
                                                            )
                                                        )
                                            )
                                )
                    )


toNodeTwo : Name.CDN_Name -> List arg -> Can.Def -> Expr.FreeLocals -> NodeTwo
toNodeTwo name args def freeLocals =
    case args of
        [] ->
            ( def, name, Dict.foldr compare addDirects [] freeLocals )

        _ ->
            ( def, name, [] )


addDirects : Name.CDN_Name -> Expr.Uses -> List Name.CDN_Name -> List Name.CDN_Name
addDirects name (Expr.Uses { direct }) directDeps =
    if direct > 0 then
        name :: directDeps

    else
        directDeps



-- CANONICALIZE EXPORTS


canonicalizeExports :
    List (A.CRA_Located Src.CASTS_Value)
    -> Dict String Name.CDN_Name union
    -> Dict String Name.CDN_Name alias
    -> Dict String Name.CDN_Name binop
    -> Can.Effects
    -> A.CRA_Located Src.CASTS_Exposing
    -> MResult i w Can.Exports
canonicalizeExports values unions aliases binops effects (A.CRA_At region exposing_) =
    case exposing_ of
        Src.CASTS_Open ->
            R.ok (Can.ExportEverything region)

        Src.CASTS_Explicit exposeds ->
            let
                names : Dict String Name.CDN_Name ()
                names =
                    Dict.fromList identity (List.map valueToName values)
            in
            R.traverse (checkExposed names unions aliases binops effects) exposeds
                |> R.bind
                    (\infos ->
                        Dups.detect Error.ExportDuplicate (Dups.unions infos)
                            |> R.fmap Can.Export
                    )


valueToName : A.CRA_Located Src.CASTS_Value -> ( Name.CDN_Name, () )
valueToName (A.CRA_At _ (Src.CASTS_Value (A.CRA_At _ name) _ _ _)) =
    ( name, () )


checkExposed :
    Dict String Name.CDN_Name value
    -> Dict String Name.CDN_Name union
    -> Dict String Name.CDN_Name alias
    -> Dict String Name.CDN_Name binop
    -> Can.Effects
    -> Src.CASTS_Exposed
    -> MResult i w (Dups.Tracker (A.CRA_Located Can.Export))
checkExposed values unions aliases binops effects exposed =
    case exposed of
        Src.CASTS_Lower (A.CRA_At region name) ->
            if Dict.member identity name values then
                ok name region Can.ExportValue

            else
                case checkPorts effects name of
                    Nothing ->
                        ok name region Can.ExportPort

                    Just ports ->
                        R.throw (Error.ExportNotFound region Error.BadVar name (ports ++ Dict.keys compare values))

        Src.CASTS_Operator region name ->
            if Dict.member identity name binops then
                ok name region Can.ExportBinop

            else
                R.throw (Error.ExportNotFound region Error.BadOp name (Dict.keys compare binops))

        Src.CASTS_Upper (A.CRA_At region name) (Src.CASTS_Public dotDotRegion) ->
            if Dict.member identity name unions then
                ok name region Can.ExportUnionOpen

            else if Dict.member identity name aliases then
                R.throw (Error.ExportOpenAlias dotDotRegion name)

            else
                R.throw (Error.ExportNotFound region Error.BadType name (Dict.keys compare unions ++ Dict.keys compare aliases))

        Src.CASTS_Upper (A.CRA_At region name) Src.CASTS_Private ->
            if Dict.member identity name unions then
                ok name region Can.ExportUnionClosed

            else if Dict.member identity name aliases then
                ok name region Can.ExportAlias

            else
                R.throw (Error.ExportNotFound region Error.BadType name (Dict.keys compare unions ++ Dict.keys compare aliases))


checkPorts : Can.Effects -> Name.CDN_Name -> Maybe (List Name.CDN_Name)
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


ok : Name.CDN_Name -> A.CRA_Region -> Can.Export -> MResult i w (Dups.Tracker (A.CRA_Located Can.Export))
ok name region export =
    R.ok (Dups.one name region (A.CRA_At region export))
