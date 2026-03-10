module Compiler.Canonicalize.Module exposing (MResult, canonicalize)

import Builder.Stuff as Stuff
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
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.Compiler.Imports as DefaultImports
import Compiler.Guida.Interface as I
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Compiler.Reporting.Warning as W
import Data.Graph as Graph
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import System.TypeCheck.IO as IO
import Utils.Crash exposing (crash)



-- RESULT


type alias MResult i w a =
    R.RResult i w Error.Error a



-- USED MODULES TRACKING


type alias UsedModules =
    EverySet.EverySet String ModuleName.Raw


trackModule : ModuleName.Raw -> UsedModules -> UsedModules
trackModule moduleName used =
    EverySet.insert identity moduleName used


trackCanonical : IO.Canonical -> UsedModules -> UsedModules
trackCanonical (IO.Canonical _ moduleName) used =
    trackModule moduleName used



-- MODULES


canonicalize : Target -> Stuff.Root -> Pkg.Name -> Dict String ModuleName.Raw I.Interface -> Src.Module -> MResult UsedModules (List W.Warning) Can.Module
canonicalize target root pkg ifaces ((Src.Module syntaxVersion _ exports docs imports values _ _ binops effects) as modul) =
    let
        home : IO.Canonical
        home =
            IO.Canonical pkg (Src.getName modul)

        cbinops : Dict String Name Can.Binop
        cbinops =
            Dict.fromList identity (List.map canonicalizeBinop binops)
    in
    Foreign.createInitialEnv target home ifaces imports
        |> R.bind (Local.add target modul)
        |> R.bind
            (\( env, cunions, caliases ) ->
                canonicalizeValues target root syntaxVersion env values
                    |> R.bind
                        (\cvalues ->
                            Effects.canonicalize target syntaxVersion env values cunions effects
                                |> R.bind
                                    (\ceffects ->
                                        canonicalizeExports values cunions caliases cbinops ceffects exports
                                            |> R.fmap
                                                (\cexports ->
                                                    Can.Module home cexports docs cvalues cunions caliases cbinops ceffects
                                                )
                                            |> R.bind
                                                (\canModule ->
                                                    extractUsedModules canModule
                                                        |> R.bind
                                                            (\() ->
                                                                generateUnusedImportWarnings target imports
                                                                    |> R.fmap (\() -> canModule)
                                                            )
                                                )
                                    )
                        )
            )



-- EXTRACT USED MODULES FROM CANONICAL AST


extractUsedModules : Can.Module -> MResult UsedModules (List W.Warning) ()
extractUsedModules (Can.Module _ _ _ decls unions aliases _ effects) =
    extractUsedFromDecls decls
        |> R.bind (\() -> extractUsedFromUnions unions)
        |> R.bind (\() -> extractUsedFromAliases aliases)
        |> R.bind (\() -> extractUsedFromEffects effects)


extractUsedFromDecls : Can.Decls -> MResult UsedModules (List W.Warning) ()
extractUsedFromDecls decls =
    case decls of
        Can.SaveTheEnvironment ->
            R.ok ()

        Can.Declare def rest ->
            extractUsedFromDef def
                |> R.bind (\() -> extractUsedFromDecls rest)

        Can.DeclareRec def defs rest ->
            extractUsedFromDef def
                |> R.bind (\() -> R.traverse extractUsedFromDef defs)
                |> R.bind (\_ -> extractUsedFromDecls rest)


extractUsedFromDef : Can.Def -> MResult UsedModules (List W.Warning) ()
extractUsedFromDef def =
    case def of
        Can.Def _ patterns expr ->
            R.traverse extractUsedFromPattern patterns
                |> R.bind (\_ -> extractUsedFromExpr expr)

        Can.TypedDef _ _ patternsAndTypes expr tipe ->
            R.traverse (\( p, t ) -> extractUsedFromPattern p |> R.bind (\() -> extractUsedFromType t)) patternsAndTypes
                |> R.bind (\_ -> extractUsedFromExpr expr)
                |> R.bind (\() -> extractUsedFromType tipe)


extractUsedFromExpr : Can.Expr -> MResult UsedModules (List W.Warning) ()
extractUsedFromExpr (A.At _ expr_) =
    case expr_ of
        Can.VarLocal _ ->
            R.ok ()

        Can.VarTopLevel _ _ ->
            R.ok ()

        Can.VarKernel _ _ ->
            R.ok ()

        Can.VarForeign home _ _ ->
            trackHome home

        Can.VarCtor _ home _ _ _ ->
            trackHome home

        Can.VarDebug home _ _ ->
            trackHome home

        Can.VarOperator _ home _ _ ->
            trackHome home

        Can.Chr _ ->
            R.ok ()

        Can.Str _ ->
            R.ok ()

        Can.Int _ ->
            R.ok ()

        Can.Float _ ->
            R.ok ()

        Can.List exprs ->
            R.traverse extractUsedFromExpr exprs
                |> R.fmap (\_ -> ())

        Can.Negate expr ->
            extractUsedFromExpr expr

        Can.Binop _ home _ _ left right ->
            trackHome home
                |> R.bind (\() -> extractUsedFromExpr left)
                |> R.bind (\() -> extractUsedFromExpr right)

        Can.Lambda patterns body ->
            R.traverse extractUsedFromPattern patterns
                |> R.bind (\_ -> extractUsedFromExpr body)

        Can.Call func args ->
            extractUsedFromExpr func
                |> R.bind (\() -> R.traverse extractUsedFromExpr args)
                |> R.fmap (\_ -> ())

        Can.If branches final ->
            R.traverse (\( cond, branch ) -> extractUsedFromExpr cond |> R.bind (\() -> extractUsedFromExpr branch)) branches
                |> R.bind (\_ -> extractUsedFromExpr final)

        Can.Let def body ->
            extractUsedFromDef def
                |> R.bind (\() -> extractUsedFromExpr body)

        Can.LetRec defs body ->
            R.traverse extractUsedFromDef defs
                |> R.bind (\_ -> extractUsedFromExpr body)

        Can.LetDestruct pattern expr body ->
            extractUsedFromPattern pattern
                |> R.bind (\() -> extractUsedFromExpr expr)
                |> R.bind (\() -> extractUsedFromExpr body)

        Can.Case expr branches ->
            extractUsedFromExpr expr
                |> R.bind (\() -> R.traverse extractUsedFromCaseBranch branches)
                |> R.fmap (\_ -> ())

        Can.Accessor _ ->
            R.ok ()

        Can.Access expr _ ->
            extractUsedFromExpr expr

        Can.Update expr fields ->
            let
                extractFromFieldUpdate ( _, Can.FieldUpdate _ e ) =
                    extractUsedFromExpr e
            in
            extractUsedFromExpr expr
                |> R.bind (\() -> R.traverse extractFromFieldUpdate (Dict.toList A.compareLocated fields))
                |> R.fmap (\_ -> ())

        Can.Record fields ->
            R.traverse (\( _, expr ) -> extractUsedFromExpr expr) (Dict.toList A.compareLocated fields)
                |> R.fmap (\_ -> ())

        Can.Unit ->
            R.ok ()

        Can.Tuple a b rest ->
            extractUsedFromExpr a
                |> R.bind (\() -> extractUsedFromExpr b)
                |> R.bind (\() -> R.traverse extractUsedFromExpr rest)
                |> R.fmap (\_ -> ())

        Can.Shader _ _ ->
            R.ok ()


extractUsedFromCaseBranch : Can.CaseBranch -> MResult UsedModules (List W.Warning) ()
extractUsedFromCaseBranch (Can.CaseBranch pattern expr) =
    extractUsedFromPattern pattern
        |> R.bind (\() -> extractUsedFromExpr expr)


extractUsedFromPattern : Can.Pattern -> MResult UsedModules (List W.Warning) ()
extractUsedFromPattern (A.At _ pattern_) =
    case pattern_ of
        Can.PAnything ->
            R.ok ()

        Can.PVar _ ->
            R.ok ()

        Can.PRecord _ ->
            R.ok ()

        Can.PAlias subPattern _ ->
            extractUsedFromPattern subPattern

        Can.PUnit ->
            R.ok ()

        Can.PTuple a b rest ->
            extractUsedFromPattern a
                |> R.bind (\() -> extractUsedFromPattern b)
                |> R.bind (\() -> R.traverse extractUsedFromPattern rest)
                |> R.fmap (\_ -> ())

        Can.PList patterns ->
            R.traverse extractUsedFromPattern patterns
                |> R.fmap (\_ -> ())

        Can.PCons head tail ->
            extractUsedFromPattern head
                |> R.bind (\() -> extractUsedFromPattern tail)

        Can.PBool _ _ ->
            R.ok ()

        Can.PChr _ ->
            R.ok ()

        Can.PStr _ _ ->
            R.ok ()

        Can.PInt _ ->
            R.ok ()

        Can.PCtor { home, args } ->
            trackHome home
                |> R.bind (\() -> R.traverse (\(Can.PatternCtorArg _ _ p) -> extractUsedFromPattern p) args)
                |> R.fmap (\_ -> ())


extractUsedFromType : Can.Type -> MResult UsedModules (List W.Warning) ()
extractUsedFromType tipe =
    case tipe of
        Can.TLambda arg result ->
            extractUsedFromType arg
                |> R.bind (\() -> extractUsedFromType result)

        Can.TVar _ ->
            R.ok ()

        Can.TType home _ args ->
            trackHome home
                |> R.bind (\() -> R.traverse extractUsedFromType args)
                |> R.fmap (\_ -> ())

        Can.TRecord fields _ ->
            R.traverse extractUsedFromFieldType (Dict.values compare fields)
                |> R.fmap (\_ -> ())

        Can.TUnit ->
            R.ok ()

        Can.TTuple a b rest ->
            extractUsedFromType a
                |> R.bind (\() -> extractUsedFromType b)
                |> R.bind (\() -> R.traverse extractUsedFromType rest)
                |> R.fmap (\_ -> ())

        Can.TAlias home _ args aliasType ->
            trackHome home
                |> R.bind (\() -> R.traverse (\( _, t ) -> extractUsedFromType t) args)
                |> R.fmap (\_ -> ())
                |> R.bind (\() -> extractUsedFromAliasType aliasType)


extractUsedFromFieldType : Can.FieldType -> MResult UsedModules (List W.Warning) ()
extractUsedFromFieldType (Can.FieldType _ tipe) =
    extractUsedFromType tipe


extractUsedFromAliasType : Can.AliasType -> MResult UsedModules (List W.Warning) ()
extractUsedFromAliasType aliasType =
    case aliasType of
        Can.Holey tipe ->
            extractUsedFromType tipe

        Can.Filled tipe ->
            extractUsedFromType tipe


extractUsedFromUnions : Dict String Name Can.Union -> MResult UsedModules (List W.Warning) ()
extractUsedFromUnions unions =
    R.traverse extractUsedFromUnion (Dict.values compare unions)
        |> R.fmap (\_ -> ())


extractUsedFromUnion : Can.Union -> MResult UsedModules (List W.Warning) ()
extractUsedFromUnion (Can.Union _ ctors _ _) =
    R.traverse extractUsedFromCtor ctors
        |> R.fmap (\_ -> ())


extractUsedFromCtor : Can.Ctor -> MResult UsedModules (List W.Warning) ()
extractUsedFromCtor (Can.Ctor _ _ _ types) =
    R.traverse extractUsedFromType types
        |> R.fmap (\_ -> ())


extractUsedFromAliases : Dict String Name Can.Alias -> MResult UsedModules (List W.Warning) ()
extractUsedFromAliases aliases =
    R.traverse extractUsedFromAlias (Dict.values compare aliases)
        |> R.fmap (\_ -> ())


extractUsedFromAlias : Can.Alias -> MResult UsedModules (List W.Warning) ()
extractUsedFromAlias (Can.Alias _ tipe) =
    extractUsedFromType tipe


extractUsedFromEffects : Can.Effects -> MResult UsedModules (List W.Warning) ()
extractUsedFromEffects effects =
    case effects of
        Can.NoEffects ->
            R.ok ()

        Can.Ports ports ->
            R.traverse extractUsedFromPort (Dict.values compare ports)
                |> R.fmap (\_ -> ())

        Can.Manager _ _ _ manager ->
            extractUsedFromManager manager


extractUsedFromPort : Can.Port -> MResult UsedModules (List W.Warning) ()
extractUsedFromPort portDef =
    case portDef of
        Can.Incoming { payload } ->
            extractUsedFromType payload

        Can.Outgoing { payload } ->
            extractUsedFromType payload


extractUsedFromManager : Can.Manager -> MResult UsedModules (List W.Warning) ()
extractUsedFromManager manager =
    case manager of
        Can.Cmd _ ->
            R.ok ()

        Can.Sub _ ->
            R.ok ()

        Can.Fx _ _ ->
            R.ok ()


trackHome : IO.Canonical -> MResult UsedModules (List W.Warning) ()
trackHome home =
    R.modifyInfo (trackCanonical home)



-- GENERATE UNUSED IMPORT WARNINGS


generateUnusedImportWarnings : Target -> List Src.Import -> MResult UsedModules (List W.Warning) ()
generateUnusedImportWarnings target imports =
    R.getInfo
        |> R.bind
            (\usedModules ->
                let
                    importsToCheck : List Src.Import
                    importsToCheck =
                        removeImplicitDefaults target imports

                    unusedImports : List ( ModuleName.Raw, A.Region )
                    unusedImports =
                        List.filterMap (checkImportUsed usedModules) importsToCheck
                in
                R.traverse warnUnusedImport unusedImports
                    |> R.fmap (\_ -> ())
            )


removeImplicitDefaults : Target -> List Src.Import -> List Src.Import
removeImplicitDefaults target imports =
    let
        defaultNames : EverySet.EverySet String ModuleName.Raw
        defaultNames =
            EverySet.fromList identity (List.map Src.getImportName (List.map Src.c1Value (DefaultImports.defaults target)))
    in
    List.filter (isExplicitImport defaultNames) imports


isExplicitImport : EverySet.EverySet String ModuleName.Raw -> Src.Import -> Bool
isExplicitImport defaultNames (Src.Import ( _, A.At (A.Region (A.Position startRow _) _) name ) _ _) =
    not (EverySet.member identity name defaultNames && startRow <= 1)


checkImportUsed : UsedModules -> Src.Import -> Maybe ( ModuleName.Raw, A.Region )
checkImportUsed usedModules (Src.Import ( _, A.At region name ) _ _) =
    if EverySet.member identity name usedModules then
        Nothing

    else
        Just ( name, region )


warnUnusedImport : ( ModuleName.Raw, A.Region ) -> MResult UsedModules (List W.Warning) ()
warnUnusedImport ( name, region ) =
    R.warn (W.UnusedImport region name)



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


canonicalizeValues : Target -> Stuff.Root -> SyntaxVersion -> Env.Env -> List (A.Located Src.Value) -> MResult i (List W.Warning) Can.Decls
canonicalizeValues target root syntaxVersion env values =
    R.traverse (toNodeOne target root syntaxVersion env) values
        |> R.bind (\nodes -> detectCycles target (Graph.stronglyConnComp nodes))


detectCycles : Target -> List (Graph.SCC NodeTwo) -> MResult i w Can.Decls
detectCycles target sccs =
    case sccs of
        [] ->
            R.ok Can.SaveTheEnvironment

        scc :: otherSccs ->
            case scc of
                Graph.AcyclicSCC ( def, _, _ ) ->
                    R.fmap (Can.Declare def) (detectCycles target otherSccs)

                Graph.CyclicSCC subNodes ->
                    R.traverse (detectBadCycles target) (Graph.stronglyConnComp subNodes)
                        |> R.bind
                            (\defs ->
                                case defs of
                                    [] ->
                                        detectCycles target otherSccs

                                    d :: ds ->
                                        R.fmap (Can.DeclareRec d ds) (detectCycles target otherSccs)
                            )


detectBadCycles : Target -> Graph.SCC Can.Def -> MResult i w Can.Def
detectBadCycles target scc =
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
            R.throw (Error.RecursiveDecl target region name names)


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


toNodeOne : Target -> Stuff.Root -> SyntaxVersion -> Env.Env -> A.Located Src.Value -> MResult i (List W.Warning) NodeOne
toNodeOne target root syntaxVersion env (A.At _ (Src.Value _ ( _, (A.At _ name) as aname ) srcArgs ( _, body ) maybeType)) =
    case maybeType of
        Nothing ->
            Pattern.verify (Error.DPFuncArgs name)
                (R.traverse (Pattern.canonicalize target syntaxVersion env) (List.map Src.c1Value srcArgs))
                |> R.bind
                    (\( args, argBindings ) ->
                        Env.addLocals target argBindings env
                            |> R.bind
                                (\newEnv ->
                                    Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize target root syntaxVersion newEnv body)
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
            Type.toAnnotation target syntaxVersion env srcType
                |> R.bind
                    (\(Can.Forall freeVars tipe) ->
                        Pattern.verify (Error.DPFuncArgs name)
                            (Expr.gatherTypedArgs target syntaxVersion env name (List.map Src.c1Value srcArgs) tipe Index.first [])
                            |> R.bind
                                (\( ( args, resultType ), argBindings ) ->
                                    Env.addLocals target argBindings env
                                        |> R.bind
                                            (\newEnv ->
                                                Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize target root syntaxVersion newEnv body)
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
