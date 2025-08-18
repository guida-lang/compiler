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
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
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


canonicalize : Pkg.Name -> Dict String ModuleName.Raw I.Interface -> Src.Module -> MResult i (List W.Warning) Can.Module
canonicalize pkg ifaces ((Src.Module syntaxVersion _ exports docs imports values _ _ binops effects) as modul) =
    let
        home : IO.Canonical
        home =
            IO.Canonical pkg (Src.getName modul)

        cbinops : Dict String Name Can.Binop
        cbinops =
            Dict.fromList identity (List.map canonicalizeBinop binops)
    in
    Foreign.createInitialEnv home ifaces imports
        |> R.bind (Local.add modul)
        |> R.bind
            (\( env, cunions, caliases ) ->
                canonicalizeValues syntaxVersion env values
                    |> R.bind
                        (\cvalues ->
                            Effects.canonicalize syntaxVersion env values cunions effects
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


canonicalizeBinop : A.Located Src.Infix -> ( Name, Can.Binop )
canonicalizeBinop (A.At _ (Src.Infix ( _, op ) ( _, associativity ) ( _, precedence ) ( _, func ))) =
    ( op, Can.Binop_ associativity precedence func )



-- DECLARATIONS / CYCLE DETECTION
--
-- There are two phases of cycle detection:
--
-- 1. Detect cycles using ALL dependencies => needed for type inference
-- 2. Detect cycles using DIRECT dependencies => nonterminating recursion
--


canonicalizeValues : SyntaxVersion -> Env.Env -> List (A.Located Src.Value) -> MResult i (List W.Warning) Can.Decls
canonicalizeValues syntaxVersion env values =
    R.traverse (toNodeOne syntaxVersion env) values
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
                (A.At region name) =
                    extractDefName def

                names : List Name
                names =
                    List.map (A.toValue << extractDefName) defs
            in
            R.throw (Error.RecursiveDecl region name names)


extractDefName : Can.Def -> A.Located Name
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
    ( NodeTwo, Name.Name, List Name.Name )



-- Phase two nodes track DIRECT dependencies.
-- This allows us to detect cycles that definitely do not terminate.


type alias NodeTwo =
    ( Can.Def, Name, List Name )


toNodeOne : SyntaxVersion -> Env.Env -> A.Located Src.Value -> MResult i (List W.Warning) NodeOne
toNodeOne syntaxVersion env (A.At _ (Src.Value _ ( _, (A.At _ name) as aname ) srcArgs ( _, body ) maybeType)) =
    case maybeType of
        Nothing ->
            Pattern.verify (Error.DPFuncArgs name)
                (R.traverse (Pattern.canonicalize syntaxVersion env) (List.map Src.c1Value srcArgs))
                |> R.bind
                    (\( args, argBindings ) ->
                        Env.addLocals argBindings env
                            |> R.bind
                                (\newEnv ->
                                    Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize syntaxVersion newEnv body)
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

        Just ( _, ( _, srcType ) ) ->
            Type.toAnnotation syntaxVersion env srcType
                |> R.bind
                    (\(Can.Forall freeVars tipe) ->
                        Pattern.verify (Error.DPFuncArgs name)
                            (Expr.gatherTypedArgs syntaxVersion env name (List.map Src.c1Value srcArgs) tipe Index.first [])
                            |> R.bind
                                (\( ( args, resultType ), argBindings ) ->
                                    Env.addLocals argBindings env
                                        |> R.bind
                                            (\newEnv ->
                                                Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize syntaxVersion newEnv body)
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


toNodeTwo : Name -> List arg -> Can.Def -> Expr.FreeLocals -> NodeTwo
toNodeTwo name args def freeLocals =
    case args of
        [] ->
            ( def, name, Dict.foldr compare addDirects [] freeLocals )

        _ ->
            ( def, name, [] )


addDirects : Name -> Expr.Uses -> List Name -> List Name
addDirects name (Expr.Uses { direct }) directDeps =
    if direct > 0 then
        name :: directDeps

    else
        directDeps



-- CANONICALIZE EXPORTS


canonicalizeExports :
    List (A.Located Src.Value)
    -> Dict String Name union
    -> Dict String Name alias
    -> Dict String Name binop
    -> Can.Effects
    -> A.Located Src.Exposing
    -> MResult i w Can.Exports
canonicalizeExports values unions aliases binops effects (A.At region exposing_) =
    case exposing_ of
        Src.Open _ _ ->
            R.ok (Can.ExportEverything region)

        Src.Explicit (A.At _ exposeds) ->
            let
                names : Dict String Name ()
                names =
                    Dict.fromList identity (List.map valueToName values)
            in
            R.traverse (checkExposed names unions aliases binops effects) (List.map Src.c2Value exposeds)
                |> R.bind
                    (\infos ->
                        Dups.detect Error.ExportDuplicate (Dups.unions infos)
                            |> R.fmap Can.Export
                    )


valueToName : A.Located Src.Value -> ( Name, () )
valueToName (A.At _ (Src.Value _ ( _, A.At _ name ) _ _ _)) =
    ( name, () )


checkExposed :
    Dict String Name value
    -> Dict String Name union
    -> Dict String Name alias
    -> Dict String Name binop
    -> Can.Effects
    -> Src.Exposed
    -> MResult i w (Dups.Tracker (A.Located Can.Export))
checkExposed values unions aliases binops effects exposed =
    case exposed of
        Src.Lower (A.At region name) ->
            if Dict.member identity name values then
                ok name region Can.ExportValue

            else
                case checkPorts effects name of
                    Nothing ->
                        ok name region Can.ExportPort

                    Just ports ->
                        R.throw (Error.ExportNotFound region Error.BadVar name (ports ++ Dict.keys compare values))

        Src.Operator region name ->
            if Dict.member identity name binops then
                ok name region Can.ExportBinop

            else
                R.throw (Error.ExportNotFound region Error.BadOp name (Dict.keys compare binops))

        Src.Upper (A.At region name) ( _, Src.Public dotDotRegion ) ->
            if Dict.member identity name unions then
                ok name region Can.ExportUnionOpen

            else if Dict.member identity name aliases then
                R.throw (Error.ExportOpenAlias dotDotRegion name)

            else
                R.throw (Error.ExportNotFound region Error.BadType name (Dict.keys compare unions ++ Dict.keys compare aliases))

        Src.Upper (A.At region name) ( _, Src.Private ) ->
            if Dict.member identity name unions then
                ok name region Can.ExportUnionClosed

            else if Dict.member identity name aliases then
                ok name region Can.ExportAlias

            else
                R.throw (Error.ExportNotFound region Error.BadType name (Dict.keys compare unions ++ Dict.keys compare aliases))


checkPorts : Can.Effects -> Name -> Maybe (List Name)
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


ok : Name -> A.Region -> Can.Export -> MResult i w (Dups.Tracker (A.Located Can.Export))
ok name region export =
    R.ok (Dups.one name region (A.At region export))
