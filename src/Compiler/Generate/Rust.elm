module Compiler.Generate.Rust exposing
    ( Mains
    , generate
    )

import Basics.Extra exposing (flip)
import Compiler.AST.Optimized as Opt
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Generate.Mode as Mode
import Compiler.Generate.Rust.Builder as Rust
import Compiler.Generate.Rust.Expression as Expr
import Compiler.Generate.Rust.Functions as Functions
import Compiler.Generate.Rust.Name as RustName
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Rust.Kernel as K
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import System.TypeCheck.IO as IO
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- GENERATE


type alias Graph =
    Dict (List String) Opt.Global Opt.Node


type alias Mains =
    Dict (List String) IO.Canonical Opt.Main


prelude : String
prelude =
    Functions.functions


generate : Target -> Mode.Mode -> Opt.GlobalGraph -> Mains -> String
generate target mode (Opt.GlobalGraph graph _) mains =
    let
        state : State
        state =
            Dict.foldr ModuleName.compareCanonical (addMain target mode graph) emptyState mains
    in
    prelude
        ++ stateToBuilder state
        ++ toMainExports target mode mains
        ++ "fn main() {"
        ++ perfNote target mode
        ++ "}"


addMain : Target -> Mode.Mode -> Graph -> IO.Canonical -> Opt.Main -> State -> State
addMain target mode graph home _ state =
    addGlobal target mode graph state (Opt.Global home "main")


perfNote : Target -> Mode.Mode -> String
perfNote target mode =
    case mode of
        Mode.Prod _ ->
            ""

        Mode.Dev Nothing ->
            "println!(\"Compiled in DEV mode. Follow the advice at "
                ++ D.makeNakedLink target "optimize"
                ++ " for better performance and smaller assets.\");"

        Mode.Dev (Just _) ->
            "println!(\"Compiled in DEBUG mode. Follow the advice at "
                ++ D.makeNakedLink target "optimize"
                ++ " for better performance and smaller assets.\");"


type State
    = State Rust.Builder (EverySet (List String) Opt.Global)


emptyState : State
emptyState =
    State (Rust.emptyBuilder 0) EverySet.empty


stateToBuilder : State -> String
stateToBuilder (State (Rust.Builder revKernels code _ _ _) _) =
    prependBuilders revKernels code


prependBuilders : List String -> String -> String
prependBuilders revBuilders monolith =
    List.foldl (\b m -> b ++ m) monolith revBuilders


addGlobal : Target -> Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal target mode graph ((State builder seen) as state) global =
    if EverySet.member Opt.toComparableGlobal global seen then
        state

    else
        addGlobalHelp target mode graph global <|
            State builder (EverySet.insert Opt.toComparableGlobal global seen)


addGlobalHelp : Target -> Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp target mode graph ((Opt.Global home _) as global) state =
    let
        addDeps : EverySet (List String) Opt.Global -> State -> State
        addDeps deps someState =
            let
                sortedDeps : List Opt.Global
                sortedDeps =
                    -- This is required given that it looks like `Data.Set.union` sorts its elements
                    List.sortWith Opt.compareGlobal (EverySet.toList Opt.compareGlobal deps)
            in
            List.foldl (flip (addGlobal target mode graph)) someState sortedDeps
    in
    case Utils.find Opt.toComparableGlobal global graph of
        Opt.Define expr deps ->
            addStmt (addDeps deps state)
                (var global (Expr.generate target mode home expr))

        Opt.TrackedDefine region expr deps ->
            addStmt (addDeps deps state)
                (trackedVar region global (Expr.generate target mode home expr))

        Opt.DefineTailFunc region argNames body deps ->
            let
                (Opt.Global _ name) =
                    global
            in
            addStmt (addDeps deps state)
                (trackedVar region global (Expr.generateTailDef target mode home name argNames body))

        Opt.Ctor index arity ->
            addStmt state
                (var global (Expr.generateCtor target mode global index arity))

        Opt.Link linkedGlobal ->
            addGlobal target mode graph state linkedGlobal

        Opt.Cycle names values functions deps ->
            addStmt (addDeps deps state)
                (generateCycle target mode global names values functions)

        Opt.Manager effectsType ->
            generateManager target mode graph global effectsType state

        Opt.Kernel _ rustChunks deps ->
            if isDebugger global && not (Mode.isDebug mode) then
                state

            else
                addKernel (addDeps deps state) (generateKernel mode rustChunks)

        Opt.Enum index ->
            addStmt state
                (generateEnum target mode global index)

        Opt.Box ->
            addStmt (addGlobal target mode graph state (identity_ target))
                (generateBox target mode global)

        Opt.PortIncoming decoder deps ->
            addStmt (addDeps deps state)
                (generatePort target mode global "incomingPort" decoder)

        Opt.PortOutgoing encoder deps ->
            addStmt (addDeps deps state)
                (generatePort target mode global "outgoingPort" encoder)


addStmt : State -> Rust.Stmt -> State
addStmt (State builder seen) stmt =
    State (Rust.stmtToBuilder stmt builder) seen


addKernel : State -> String -> State
addKernel (State builder seen) kernel =
    State (Rust.addKernel kernel builder) seen


var : Opt.Global -> Expr.Code -> Rust.Stmt
var (Opt.Global home name) code =
    Rust.Var (RustName.fromGlobal home name) (Expr.codeToExpr code)


trackedVar : A.Region -> Opt.Global -> Expr.Code -> Rust.Stmt
trackedVar (A.Region startPos _) (Opt.Global home name) code =
    Rust.TrackedVar home startPos (RustName.fromGlobalHumanReadable home name) (RustName.fromGlobal home name) (Expr.codeToExpr code)


isDebugger : Opt.Global -> Bool
isDebugger (Opt.Global (IO.Canonical _ home) _) =
    home == Name.debugger



-- GENERATE CYCLES


generateCycle : Target -> Mode.Mode -> Opt.Global -> List Name.Name -> List ( Name.Name, Opt.Expr ) -> List Opt.Def -> Rust.Stmt
generateCycle target mode (Opt.Global ((IO.Canonical _ module_) as home) _) names values functions =
    Rust.Block
        [ Rust.Block <| List.map (generateCycleFunc target mode home) functions
        , Rust.Block <| List.map (generateSafeCycle target mode home) values
        , case List.map (generateRealCycle home) values of
            [] ->
                Rust.EmptyStmt

            (_ :: _) as realBlock ->
                case mode of
                    Mode.Prod _ ->
                        Rust.Block realBlock

                    Mode.Dev _ ->
                        Rust.Try (Rust.Block realBlock) RustName.dollar <|
                            Rust.Throw <|
                                Rust.ExprString <|
                                    "Some top-level definitions from `"
                                        ++ module_
                                        ++ "` are causing infinite recursion:\\n"
                                        ++ drawCycle names
                                        ++ "\\n\\nThese errors are very tricky, so read "
                                        ++ D.makeNakedLink target "bad-recursion"
                                        ++ " to learn how to fix it!"
        ]


generateCycleFunc : Target -> Mode.Mode -> IO.Canonical -> Opt.Def -> Rust.Stmt
generateCycleFunc target mode home def =
    case def of
        Opt.Def _ name expr ->
            Rust.Var (RustName.fromGlobal home name) (Expr.codeToExpr (Expr.generate target mode home expr))

        Opt.TailDef _ name args expr ->
            Rust.Var (RustName.fromGlobal home name) (Expr.codeToExpr (Expr.generateTailDef target mode home name args expr))


generateSafeCycle : Target -> Mode.Mode -> IO.Canonical -> ( Name.Name, Opt.Expr ) -> Rust.Stmt
generateSafeCycle target mode home ( name, expr ) =
    Rust.FunctionStmt (RustName.fromCycle home name) [] <|
        Expr.codeToStmtList (Expr.generate target mode home expr)


generateRealCycle : IO.Canonical -> ( Name.Name, expr ) -> Rust.Stmt
generateRealCycle home ( name, _ ) =
    let
        safeName : RustName.Name
        safeName =
            RustName.fromCycle home name

        realName : RustName.Name
        realName =
            RustName.fromGlobal home name
    in
    Rust.Block
        [ Rust.Var realName (Rust.ExprCall (Rust.ExprRef safeName) [])
        , Rust.ExprStmt <|
            Rust.ExprAssign (Rust.LRef safeName) <|
                Rust.ExprFunction Nothing [] [ Rust.Return (Rust.ExprRef realName) ]
        ]


drawCycle : List Name.Name -> String
drawCycle names =
    let
        topLine : String
        topLine =
            "\\n  ┌─────┐"

        nameLine : String -> String
        nameLine name =
            "\\n  │    " ++ name

        midLine : String
        midLine =
            "\\n  │     ↓"

        bottomLine : String
        bottomLine =
            "\\n  └─────┘"
    in
    String.concat (topLine :: List.intersperse midLine (List.map nameLine names) ++ [ bottomLine ])


generateKernel : Mode.Mode -> List K.Chunk -> String
generateKernel mode chunks =
    List.foldr (addChunk mode) "" chunks


addChunk : Mode.Mode -> K.Chunk -> String -> String
addChunk mode chunk builder =
    case chunk of
        K.Rust rust ->
            rust ++ builder

        K.GuidaVar home name ->
            RustName.fromGlobal home name ++ builder

        K.JsVar home name ->
            RustName.fromKernel home name ++ builder

        K.GuidaField name ->
            Expr.generateField mode name ++ builder

        K.JsField int ->
            RustName.fromInt int ++ builder

        K.JsEnum int ->
            String.fromInt int ++ builder

        K.Debug ->
            case mode of
                Mode.Dev _ ->
                    builder

                Mode.Prod _ ->
                    "_UNUSED" ++ builder

        K.Prod ->
            case mode of
                Mode.Dev _ ->
                    "_UNUSED" ++ builder

                Mode.Prod _ ->
                    builder



-- GENERATE ENUM


generateEnum : Target -> Mode.Mode -> Opt.Global -> Index.ZeroBased -> Rust.Stmt
generateEnum target mode ((Opt.Global home name) as global) index =
    Rust.Var (RustName.fromGlobal home name) <|
        case mode of
            Mode.Dev _ ->
                Expr.codeToExpr (Expr.generateCtor target mode global index 0)

            Mode.Prod _ ->
                Rust.ExprInt (Index.toMachine index)



-- GENERATE BOX


generateBox : Target -> Mode.Mode -> Opt.Global -> Rust.Stmt
generateBox target mode ((Opt.Global home name) as global) =
    Rust.Var (RustName.fromGlobal home name) <|
        case mode of
            Mode.Dev _ ->
                Expr.codeToExpr (Expr.generateCtor target mode global Index.first 1)

            Mode.Prod _ ->
                Rust.ExprRef (RustName.fromGlobal (ModuleName.basics target) Name.identity_)


identity_ : Target -> Opt.Global
identity_ target =
    Opt.Global (ModuleName.basics target) Name.identity_



-- GENERATE PORTS


generatePort : Target -> Mode.Mode -> Opt.Global -> Name.Name -> Opt.Expr -> Rust.Stmt
generatePort target mode (Opt.Global home name) makePort converter =
    Rust.Var (RustName.fromGlobal home name) <|
        Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.platform makePort))
            [ Rust.ExprString name
            , Expr.codeToExpr (Expr.generate target mode home converter)
            ]



-- GENERATE MANAGER


generateManager : Target -> Mode.Mode -> Graph -> Opt.Global -> Opt.EffectsType -> State -> State
generateManager target mode graph (Opt.Global ((IO.Canonical _ moduleName) as home) _) effectsType state =
    let
        managerLVar : Rust.LValue
        managerLVar =
            Rust.LBracket
                (Rust.ExprRef (RustName.fromKernel Name.platform "effectManagers"))
                (Rust.ExprString moduleName)

        ( deps, args, stmts ) =
            generateManagerHelp home effectsType

        createManager : Rust.Stmt
        createManager =
            Rust.ExprStmt <|
                Rust.ExprAssign managerLVar <|
                    Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.platform "createManager")) args
    in
    addStmt (List.foldl (flip (addGlobal target mode graph)) state deps) <|
        Rust.Block (createManager :: stmts)


generateLeaf : IO.Canonical -> Name.Name -> Rust.Stmt
generateLeaf ((IO.Canonical _ moduleName) as home) name =
    Rust.Var (RustName.fromGlobal home name) <|
        Rust.ExprCall leaf [ Rust.ExprString moduleName ]


leaf : Rust.Expr
leaf =
    Rust.ExprRef (RustName.fromKernel Name.platform "leaf")


generateManagerHelp : IO.Canonical -> Opt.EffectsType -> ( List Opt.Global, List Rust.Expr, List Rust.Stmt )
generateManagerHelp home effectsType =
    let
        dep : Name.Name -> Opt.Global
        dep name =
            Opt.Global home name

        ref : Name.Name -> Rust.Expr
        ref name =
            Rust.ExprRef (RustName.fromGlobal home name)
    in
    case effectsType of
        Opt.Cmd ->
            ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap" ]
            , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap" ]
            , [ generateLeaf home "command" ]
            )

        Opt.Sub ->
            ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "subMap" ]
            , [ ref "init", ref "onEffects", ref "onSelfMsg", Rust.ExprInt 0, ref "subMap" ]
            , [ generateLeaf home "subscription" ]
            )

        Opt.Fx ->
            ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap", dep "subMap" ]
            , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap", ref "subMap" ]
            , [ generateLeaf home "command"
              , generateLeaf home "subscription"
              ]
            )



-- MAIN EXPORTS


toMainExports : Target -> Mode.Mode -> Mains -> String
toMainExports target mode mains =
    let
        export : RustName.Name
        export =
            RustName.fromKernel Name.platform "export"

        exports : String
        exports =
            generateExports target mode (Dict.foldr ModuleName.compareCanonical addToTrie emptyTrie mains)
    in
    export ++ "(" ++ exports ++ ");"


generateExports : Target -> Mode.Mode -> Trie -> String
generateExports target mode (Trie maybeMain subs) =
    let
        starter : String -> String
        starter end =
            case maybeMain of
                Nothing ->
                    "{"

                Just ( home, main ) ->
                    let
                        (Rust.Builder _ code _ _ _) =
                            Rust.exprToBuilder (Expr.generateMain target mode home main) (Rust.emptyBuilder 0)
                    in
                    "{\"init\":"
                        ++ code
                        ++ end
    in
    case Dict.toList compare subs of
        [] ->
            starter "" ++ "}"

        ( name, subTrie ) :: otherSubTries ->
            starter ","
                ++ "\""
                ++ name
                ++ "\":"
                ++ generateExports target mode subTrie
                ++ List.foldl (flip (addSubTrie target mode)) "}" otherSubTries


addSubTrie : Target -> Mode.Mode -> String -> ( Name.Name, Trie ) -> String
addSubTrie target mode end ( name, trie ) =
    ",'" ++ name ++ "':" ++ generateExports target mode trie ++ end



-- BUILD TRIES


type Trie
    = Trie (Maybe ( IO.Canonical, Opt.Main )) (Dict String Name.Name Trie)


emptyTrie : Trie
emptyTrie =
    Trie Nothing Dict.empty


addToTrie : IO.Canonical -> Opt.Main -> Trie -> Trie
addToTrie ((IO.Canonical _ moduleName) as home) main trie =
    merge trie <| segmentsToTrie home (Name.splitDots moduleName) main


segmentsToTrie : IO.Canonical -> List Name.Name -> Opt.Main -> Trie
segmentsToTrie home segments main =
    case segments of
        [] ->
            Trie (Just ( home, main )) Dict.empty

        segment :: otherSegments ->
            Trie Nothing (Dict.singleton identity segment (segmentsToTrie home otherSegments main))


merge : Trie -> Trie -> Trie
merge (Trie main1 subs1) (Trie main2 subs2) =
    Trie
        (checkedMerge main1 main2)
        (Utils.mapUnionWith identity compare merge subs1 subs2)


checkedMerge : Maybe a -> Maybe a -> Maybe a
checkedMerge a b =
    case ( a, b ) of
        ( Nothing, main ) ->
            main

        ( main, Nothing ) ->
            main

        ( Just _, Just _ ) ->
            crash "cannot have two modules with the same name"
