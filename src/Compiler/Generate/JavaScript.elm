module Compiler.Generate.JavaScript exposing
    ( Mains
    , generate
    , generateForRepl
    , generateForReplEndpoint
    )

import Basics.Extra exposing (flip)
import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.Kernel as K
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Generate.JavaScript.Builder as JS
import Compiler.Generate.JavaScript.Expression as Expr
import Compiler.Generate.JavaScript.Functions as Functions
import Compiler.Generate.JavaScript.Name as JsName
import Compiler.Generate.Mode as Mode
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Json.Encode as Encode
import Maybe.Extra as Maybe
import System.TypeCheck.IO as IO
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- GENERATE


type alias Graph =
    Dict Opt.Global Opt.Node


type alias Mains =
    Dict IO.Canonical Opt.Main


generate : Mode.Mode -> Opt.GlobalGraph -> Mains -> String
generate mode (Opt.GlobalGraph graph _) mains =
    let
        state : State
        state =
            Dict.foldr (addMain mode graph) emptyState mains
    in
    "(function(scope){\n'use strict';"
        ++ Functions.functions
        ++ perfNote mode
        ++ stateToBuilder state
        ++ toMainExports mode mains
        ++ "}(this));"


addMain : Mode.Mode -> Graph -> IO.Canonical -> Opt.Main -> State -> State
addMain mode graph home _ state =
    addGlobal mode graph state (Opt.Global home "main")


perfNote : Mode.Mode -> String
perfNote mode =
    case mode of
        Mode.Prod _ ->
            ""

        Mode.Dev Nothing ->
            "console.warn('Compiled in DEV mode. Follow the advice at "
                ++ D.makeNakedLink "optimize"
                ++ " for better performance and smaller assets.');"

        Mode.Dev (Just _) ->
            "console.warn('Compiled in DEBUG mode. Follow the advice at "
                ++ D.makeNakedLink "optimize"
                ++ " for better performance and smaller assets.');"


generateForRepl : Bool -> L.Localizer -> Opt.GlobalGraph -> IO.Canonical -> Name.Name -> Can.Annotation -> String
generateForRepl ansi localizer (Opt.GlobalGraph graph _) home name (Can.Forall _ tipe) =
    let
        mode : Mode.Mode
        mode =
            Mode.Dev Nothing

        debugState : State
        debugState =
            addGlobal mode graph emptyState (Opt.Global ModuleName.debug "toString")

        evalState : State
        evalState =
            addGlobal mode graph debugState (Opt.Global home name)
    in
    "process.on('uncaughtException', function(err) { process.stderr.write(err.toString() + '\\n'); process.exit(1); });"
        ++ Functions.functions
        ++ stateToBuilder evalState
        ++ print ansi localizer home name tipe


print : Bool -> L.Localizer -> IO.Canonical -> Name.Name -> Can.Type -> String
print ansi localizer home name tipe =
    let
        value : JsName.Name
        value =
            JsName.fromGlobal home name

        toString : JsName.Name
        toString =
            JsName.fromKernel Name.debug "toAnsiString"

        tipeDoc : D.Doc
        tipeDoc =
            RT.canToDoc localizer RT.None tipe

        bool : String
        bool =
            if ansi then
                "true"

            else
                "false"
    in
    "var _value = "
        ++ toString
        ++ "("
        ++ bool
        ++ ", "
        ++ value
        ++ ");\nvar _type = "
        ++ Encode.encode 0 (Encode.string (D.toString tipeDoc))
        ++ ";\nfunction _print(t) { console.log(_value + ("
        ++ bool
        ++ " ? '\\x1b[90m' + t + '\\x1b[0m' : t)); }\nif (_value.length + 3 + _type.length >= 80 || _type.indexOf('\\n') >= 0) {\n    _print('\\n    : ' + _type.split('\\n').join('\\n      '));\n} else {\n    _print(' : ' + _type);\n}\n"



-- GENERATE FOR REPL ENDPOINT


generateForReplEndpoint : L.Localizer -> Opt.GlobalGraph -> IO.Canonical -> Maybe Name.Name -> Can.Annotation -> String
generateForReplEndpoint localizer (Opt.GlobalGraph graph _) home maybeName (Can.Forall _ tipe) =
    let
        name : Name.Name
        name =
            Maybe.unwrap Name.replValueToPrint identity maybeName

        mode : Mode.Mode
        mode =
            Mode.Dev Nothing

        debugState : State
        debugState =
            addGlobal mode graph emptyState (Opt.Global ModuleName.debug "toString")

        evalState : State
        evalState =
            addGlobal mode graph debugState (Opt.Global home name)
    in
    Functions.functions
        ++ stateToBuilder evalState
        ++ postMessage localizer home maybeName tipe


postMessage : L.Localizer -> IO.Canonical -> Maybe Name.Name -> Can.Type -> String
postMessage localizer home maybeName tipe =
    let
        name : Name.Name
        name =
            Maybe.unwrap Name.replValueToPrint identity maybeName

        value : JsName.Name
        value =
            JsName.fromGlobal home name

        toString : JsName.Name
        toString =
            JsName.fromKernel Name.debug "toAnsiString"

        tipeDoc : D.Doc
        tipeDoc =
            RT.canToDoc localizer RT.None tipe

        toName : String -> String
        toName n =
            "\"" ++ n ++ "\""
    in
    "self.postMessage({\n  name: "
        ++ Maybe.unwrap "null" toName maybeName
        ++ ",\n  value: "
        ++ toString
        ++ "(true, "
        ++ value
        ++ "),\n  type: "
        ++ D.toString tipeDoc
        ++ "\n});\n"


type State
    = State (List String) (List String) (EverySet Opt.Global)


emptyState : State
emptyState =
    State [] [] EverySet.empty


stateToBuilder : State -> String
stateToBuilder (State revKernels revBuilders _) =
    prependBuilders revKernels (prependBuilders revBuilders "")


prependBuilders : List String -> String -> String
prependBuilders revBuilders monolith =
    List.foldl (\b m -> b ++ m) monolith revBuilders


addGlobal : Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal mode graph ((State revKernels builders seen) as state) global =
    if EverySet.member global seen then
        state

    else
        addGlobalHelp mode graph global <|
            State revKernels builders (EverySet.insert Opt.compareGlobal global seen)


addGlobalHelp : Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp mode graph global state =
    let
        addDeps : EverySet Opt.Global -> State -> State
        addDeps deps someState =
            let
                sortedDeps : List Opt.Global
                sortedDeps =
                    -- This is required given that it looks like `Data.Set.union` sorts its elements
                    List.sortWith Opt.compareGlobal (EverySet.toList deps)
            in
            List.foldl (flip (addGlobal mode graph)) someState sortedDeps
    in
    case Utils.find global graph of
        Opt.Define expr deps ->
            addStmt (addDeps deps state)
                (var global (Expr.generate mode expr))

        Opt.DefineTailFunc argNames body deps ->
            addStmt (addDeps deps state)
                (let
                    (Opt.Global _ name) =
                        global
                 in
                 var global (Expr.generateTailDef mode name argNames body)
                )

        Opt.Ctor index arity ->
            addStmt state
                (var global (Expr.generateCtor mode global index arity))

        Opt.Link linkedGlobal ->
            addGlobal mode graph state linkedGlobal

        Opt.Cycle names values functions deps ->
            addStmt (addDeps deps state)
                (generateCycle mode global names values functions)

        Opt.Manager effectsType ->
            generateManager mode graph global effectsType state

        Opt.Kernel chunks deps ->
            if isDebugger global && not (Mode.isDebug mode) then
                state

            else
                addKernel (addDeps deps state) (generateKernel mode chunks)

        Opt.Enum index ->
            addStmt state
                (generateEnum mode global index)

        Opt.Box ->
            addStmt (addGlobal mode graph state identity_)
                (generateBox mode global)

        Opt.PortIncoming decoder deps ->
            addStmt (addDeps deps state)
                (generatePort mode global "incomingPort" decoder)

        Opt.PortOutgoing encoder deps ->
            addStmt (addDeps deps state)
                (generatePort mode global "outgoingPort" encoder)


addStmt : State -> JS.Stmt -> State
addStmt state stmt =
    addBuilder state (JS.stmtToBuilder stmt)


addBuilder : State -> String -> State
addBuilder (State revKernels revBuilders seen) builder =
    State revKernels (builder :: revBuilders) seen


addKernel : State -> String -> State
addKernel (State revKernels revBuilders seen) kernel =
    State (kernel :: revKernels) revBuilders seen


var : Opt.Global -> Expr.Code -> JS.Stmt
var (Opt.Global home name) code =
    JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr code)


isDebugger : Opt.Global -> Bool
isDebugger (Opt.Global (IO.Canonical _ home) _) =
    home == Name.debugger



-- GENERATE CYCLES


generateCycle : Mode.Mode -> Opt.Global -> List Name.Name -> List ( Name.Name, Opt.Expr ) -> List Opt.Def -> JS.Stmt
generateCycle mode (Opt.Global ((IO.Canonical _ module_) as home) _) names values functions =
    JS.Block
        [ JS.Block <| List.map (generateCycleFunc mode home) functions
        , JS.Block <| List.map (generateSafeCycle mode home) values
        , case List.map (generateRealCycle home) values of
            [] ->
                JS.EmptyStmt

            (_ :: _) as realBlock ->
                case mode of
                    Mode.Prod _ ->
                        JS.Block realBlock

                    Mode.Dev _ ->
                        JS.Try (JS.Block realBlock) JsName.dollar <|
                            JS.Throw <|
                                JS.ExprString <|
                                    "Some top-level definitions from `"
                                        ++ module_
                                        ++ "` are causing infinite recursion:\\n"
                                        ++ drawCycle names
                                        ++ "\\n\\nThese errors are very tricky, so read "
                                        ++ D.makeNakedLink "bad-recursion"
                                        ++ " to learn how to fix it!"
        ]


generateCycleFunc : Mode.Mode -> IO.Canonical -> Opt.Def -> JS.Stmt
generateCycleFunc mode home def =
    case def of
        Opt.Def name expr ->
            JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.generate mode expr))

        Opt.TailDef name args expr ->
            JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.generateTailDef mode name args expr))


generateSafeCycle : Mode.Mode -> IO.Canonical -> ( Name.Name, Opt.Expr ) -> JS.Stmt
generateSafeCycle mode home ( name, expr ) =
    JS.FunctionStmt (JsName.fromCycle home name) [] <|
        Expr.codeToStmtList (Expr.generate mode expr)


generateRealCycle : IO.Canonical -> ( Name.Name, expr ) -> JS.Stmt
generateRealCycle home ( name, _ ) =
    let
        safeName : JsName.Name
        safeName =
            JsName.fromCycle home name

        realName : JsName.Name
        realName =
            JsName.fromGlobal home name
    in
    JS.Block
        [ JS.Var realName (JS.ExprCall (JS.ExprRef safeName) [])
        , JS.ExprStmt <|
            JS.ExprAssign (JS.LRef safeName) <|
                JS.ExprFunction Nothing [] [ JS.Return (JS.ExprRef realName) ]
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
        K.JS javascript ->
            javascript ++ builder

        K.ElmVar home name ->
            JsName.fromGlobal home name ++ builder

        K.JsVar home name ->
            JsName.fromKernel home name ++ builder

        K.ElmField name ->
            Expr.generateField mode name ++ builder

        K.JsField int ->
            JsName.fromInt int ++ builder

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


generateEnum : Mode.Mode -> Opt.Global -> Index.ZeroBased -> JS.Stmt
generateEnum mode ((Opt.Global home name) as global) index =
    JS.Var (JsName.fromGlobal home name) <|
        case mode of
            Mode.Dev _ ->
                Expr.codeToExpr (Expr.generateCtor mode global index 0)

            Mode.Prod _ ->
                JS.ExprInt (Index.toMachine index)



-- GENERATE BOX


generateBox : Mode.Mode -> Opt.Global -> JS.Stmt
generateBox mode ((Opt.Global home name) as global) =
    JS.Var (JsName.fromGlobal home name) <|
        case mode of
            Mode.Dev _ ->
                Expr.codeToExpr (Expr.generateCtor mode global Index.first 1)

            Mode.Prod _ ->
                JS.ExprRef (JsName.fromGlobal ModuleName.basics Name.identity_)


identity_ : Opt.Global
identity_ =
    Opt.Global ModuleName.basics Name.identity_



-- GENERATE PORTS


generatePort : Mode.Mode -> Opt.Global -> Name.Name -> Opt.Expr -> JS.Stmt
generatePort mode (Opt.Global home name) makePort converter =
    JS.Var (JsName.fromGlobal home name) <|
        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.platform makePort))
            [ JS.ExprString name
            , Expr.codeToExpr (Expr.generate mode converter)
            ]



-- GENERATE MANAGER


generateManager : Mode.Mode -> Graph -> Opt.Global -> Opt.EffectsType -> State -> State
generateManager mode graph (Opt.Global ((IO.Canonical _ moduleName) as home) _) effectsType state =
    let
        managerLVar : JS.LValue
        managerLVar =
            JS.LBracket
                (JS.ExprRef (JsName.fromKernel Name.platform "effectManagers"))
                (JS.ExprString moduleName)

        ( deps, args, stmts ) =
            generateManagerHelp home effectsType

        createManager : JS.Stmt
        createManager =
            JS.ExprStmt <|
                JS.ExprAssign managerLVar <|
                    JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.platform "createManager")) args
    in
    addStmt (List.foldl (flip (addGlobal mode graph)) state deps) <|
        JS.Block (createManager :: stmts)


generateLeaf : IO.Canonical -> Name.Name -> JS.Stmt
generateLeaf ((IO.Canonical _ moduleName) as home) name =
    JS.Var (JsName.fromGlobal home name) <|
        JS.ExprCall leaf [ JS.ExprString moduleName ]


leaf : JS.Expr
leaf =
    JS.ExprRef (JsName.fromKernel Name.platform "leaf")


generateManagerHelp : IO.Canonical -> Opt.EffectsType -> ( List Opt.Global, List JS.Expr, List JS.Stmt )
generateManagerHelp home effectsType =
    let
        dep : Name.Name -> Opt.Global
        dep name =
            Opt.Global home name

        ref : Name.Name -> JS.Expr
        ref name =
            JS.ExprRef (JsName.fromGlobal home name)
    in
    case effectsType of
        Opt.Cmd ->
            ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap" ]
            , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap" ]
            , [ generateLeaf home "command" ]
            )

        Opt.Sub ->
            ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "subMap" ]
            , [ ref "init", ref "onEffects", ref "onSelfMsg", JS.ExprInt 0, ref "subMap" ]
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


toMainExports : Mode.Mode -> Mains -> String
toMainExports mode mains =
    let
        export : JsName.Name
        export =
            JsName.fromKernel Name.platform "export"

        exports : String
        exports =
            generateExports mode (Dict.foldr addToTrie emptyTrie mains)
    in
    export ++ "(" ++ exports ++ ");"


generateExports : Mode.Mode -> Trie -> String
generateExports mode (Trie maybeMain subs) =
    let
        starter : String -> String
        starter end =
            case maybeMain of
                Nothing ->
                    "{"

                Just ( home, main ) ->
                    "{'init':"
                        ++ JS.exprToBuilder (Expr.generateMain mode home main)
                        ++ end
    in
    case Dict.toList subs of
        [] ->
            starter "" ++ "}"

        ( name, subTrie ) :: otherSubTries ->
            starter ","
                ++ "'"
                ++ name
                ++ "':"
                ++ generateExports mode subTrie
                ++ List.foldl (flip (addSubTrie mode)) "}" otherSubTries


addSubTrie : Mode.Mode -> String -> ( Name.Name, Trie ) -> String
addSubTrie mode end ( name, trie ) =
    ",'" ++ name ++ "':" ++ generateExports mode trie ++ end



-- BUILD TRIES


type Trie
    = Trie (Maybe ( IO.Canonical, Opt.Main )) (Dict Name.Name Trie)


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
            Trie Nothing (Dict.singleton segment (segmentsToTrie home otherSegments main))


merge : Trie -> Trie -> Trie
merge (Trie main1 subs1) (Trie main2 subs2) =
    Trie
        (checkedMerge main1 main2)
        (Utils.mapUnionWith compare merge subs1 subs2)


checkedMerge : Maybe a -> Maybe a -> Maybe a
checkedMerge a b =
    case ( a, b ) of
        ( Nothing, main ) ->
            main

        ( main, Nothing ) ->
            main

        ( Just _, Just _ ) ->
            crash "cannot have two modules with the same name"
