module Terminal.Repl exposing
    ( CategorizedInput(..)
    , Flags(..)
    , Input(..)
    , Lines(..)
    , Output(..)
    , Prefill(..)
    , run
    )

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.Generate as Generate
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.Name as N
import Compiler.Elm.Constraint as C
import Compiler.Elm.Licenses as Licenses
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Parse.Declaration as PD
import Compiler.Parse.Expression as PE
import Compiler.Parse.Module as PM
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Parse.Space as PS
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Parse.Type as PT
import Compiler.Parse.Variable as PV
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Syntax as ES
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Control.Monad.State.Strict as State
import Data.Map as Map exposing (Dict)
import Dict
import List.Extra as List
import Maybe.Extra as Maybe
import Prelude
import System.Exit as Exit
import System.IO as IO
import System.Process as Process
import Task exposing (Task)
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- RUN


type Flags
    = Flags (Maybe FilePath) Bool


run : () -> Flags -> Task Never ()
run () flags =
    printWelcomeMessage
        |> Task.bind (\_ -> initSettings)
        |> Task.bind
            (\settings ->
                initEnv flags
                    |> Task.bind
                        (\env ->
                            let
                                looper : M Exit.ExitCode
                                looper =
                                    Utils.replRunInputT settings (Utils.replWithInterrupt (loop env IO.initialReplState))
                            in
                            State.evalStateT looper IO.initialReplState
                                |> Task.bind (\exitCode -> Exit.exitWith exitCode)
                        )
            )



-- WELCOME


printWelcomeMessage : Task Never ()
printWelcomeMessage =
    let
        vsn : String
        vsn =
            V.toChars V.compiler

        title : D.Doc
        title =
            D.fromChars "Guida"
                |> D.plus (D.fromChars vsn)

        dashes : String
        dashes =
            String.repeat (70 - String.length vsn) "-"
    in
    D.toAnsi IO.stdout <|
        D.vcat
            [ D.black (D.fromChars "----")
                |> D.plus (D.dullcyan title)
                |> D.plus (D.black (D.fromChars dashes))
            , D.black (D.fromChars "Say :help for help and :exit to exit! More at ")
                |> D.a (D.fromChars (D.makeLink "repl"))
            , D.black (D.fromChars "--------------------------------------------------------------------------------")
            , D.fromChars ""
            ]



-- ENV


type Env
    = Env FilePath FilePath Bool


initEnv : Flags -> Task Never Env
initEnv (Flags maybeAlternateInterpreter noColors) =
    getRoot
        |> Task.bind
            (\root ->
                getInterpreter maybeAlternateInterpreter
                    |> Task.fmap
                        (\interpreter ->
                            Env root interpreter (not noColors)
                        )
            )



-- LOOP


type Outcome
    = Loop IO.ReplState
    | End Exit.ExitCode


type alias M a =
    State.StateT IO.ReplState a


loop : Env -> IO.ReplState -> Utils.ReplInputT Exit.ExitCode
loop env state =
    read
        |> Task.bind
            (\input ->
                Utils.liftIOInputT (eval env state input)
                    |> Task.bind
                        (\outcome ->
                            case outcome of
                                Loop loopState ->
                                    Utils.liftInputT (State.put loopState)
                                        |> Task.bind (\_ -> loop env loopState)

                                End exitCode ->
                                    Task.pure exitCode
                        )
            )



-- READ


type Input
    = Import ModuleName.Raw String
    | Type N.Name String
    | Port
    | Decl N.Name String
    | Expr String
      --
    | Reset
    | Exit
    | Skip
    | Help (Maybe String)


read : Utils.ReplInputT Input
read =
    Utils.replGetInputLine "> "
        |> Task.bind
            (\maybeLine ->
                case maybeLine of
                    Nothing ->
                        Task.pure Exit

                    Just chars ->
                        let
                            lines : Lines
                            lines =
                                Lines (stripLegacyBackslash chars) []
                        in
                        case categorize lines of
                            Done input ->
                                Task.pure input

                            Continue p ->
                                readMore lines p
            )


readMore : Lines -> Prefill -> Utils.ReplInputT Input
readMore previousLines prefill =
    Utils.replGetInputLineWithInitial "| " ( renderPrefill prefill, "" )
        |> Task.bind
            (\input ->
                case input of
                    Nothing ->
                        Task.pure Skip

                    Just chars ->
                        let
                            lines : Lines
                            lines =
                                addLine (stripLegacyBackslash chars) previousLines
                        in
                        case categorize lines of
                            Done doneInput ->
                                Task.pure doneInput

                            Continue p ->
                                readMore lines p
            )



-- For compatibility with 0.19.0 such that readers of "Programming Elm" by @jfairbank
-- can get through the REPL section successfully.
--
-- TODO: remove stripLegacyBackslash in next MAJOR release
--


stripLegacyBackslash : String -> String
stripLegacyBackslash chars =
    case String.toList chars of
        [] ->
            ""

        (_ :: _) as charsList ->
            if Prelude.last charsList == '\\' then
                String.fromList (Prelude.init charsList)

            else
                chars


type Prefill
    = Indent
    | DefStart N.Name


renderPrefill : Prefill -> String
renderPrefill lineStart =
    case lineStart of
        Indent ->
            "  "

        DefStart name ->
            name ++ " "



-- LINES


type Lines
    = Lines String (List String)


addLine : String -> Lines -> Lines
addLine line (Lines x xs) =
    Lines line (x :: xs)


isBlank : Lines -> Bool
isBlank (Lines prev rev) =
    List.isEmpty rev && String.all ((==) ' ') prev


isSingleLine : Lines -> Bool
isSingleLine (Lines _ rev) =
    List.isEmpty rev


endsWithBlankLine : Lines -> Bool
endsWithBlankLine (Lines prev _) =
    String.all ((==) ' ') prev


linesToByteString : Lines -> String
linesToByteString (Lines prev rev) =
    Utils.unlines (List.reverse (prev :: rev))


getFirstLine : Lines -> String
getFirstLine (Lines x xs) =
    case xs of
        [] ->
            x

        y :: ys ->
            getFirstLine (Lines y ys)



-- CATEGORIZE INPUT


type CategorizedInput
    = Done Input
    | Continue Prefill


categorize : Lines -> CategorizedInput
categorize lines =
    if isBlank lines then
        Done Skip

    else if startsWithColon lines then
        Done (toCommand lines)

    else if startsWithKeyword "import" lines then
        attemptImport lines

    else
        attemptDeclOrExpr lines


attemptImport : Lines -> CategorizedInput
attemptImport lines =
    let
        src : String
        src =
            linesToByteString lines

        parser : P.Parser () (Src.C1 Src.Import)
        parser =
            P.specialize (\_ _ _ -> ()) PM.chompImport
    in
    case P.fromByteString parser (\_ _ -> ()) src of
        Ok ( _, Src.Import ( _, A.At _ name ) _ _ ) ->
            Done (Import name src)

        Err () ->
            ifFail lines (Import "ERR" src)


ifFail : Lines -> Input -> CategorizedInput
ifFail lines input =
    if endsWithBlankLine lines then
        Done input

    else
        Continue Indent


ifDone : Lines -> Input -> CategorizedInput
ifDone lines input =
    if isSingleLine lines || endsWithBlankLine lines then
        Done input

    else
        Continue Indent


attemptDeclOrExpr : Lines -> CategorizedInput
attemptDeclOrExpr lines =
    let
        src : String
        src =
            linesToByteString lines

        declParser : P.Parser ( Row, Col ) ( PD.Decl, A.Position )
        declParser =
            P.specialize (toDeclPosition src) (P.fmap (Tuple.mapFirst Src.c2Value) (PD.declaration SV.Guida))
    in
    case P.fromByteString declParser Tuple.pair src of
        Ok ( decl, _ ) ->
            case decl of
                PD.Value _ (A.At _ (Src.Value _ ( _, A.At _ name ) _ _ _)) ->
                    ifDone lines (Decl name src)

                PD.Union _ (A.At _ (Src.Union ( _, A.At _ name ) _ _)) ->
                    ifDone lines (Type name src)

                PD.Alias _ (A.At _ (Src.Alias _ ( _, A.At _ name ) _ _)) ->
                    ifDone lines (Type name src)

                PD.Port _ _ ->
                    Done Port

        Err declPosition ->
            if startsWithKeyword "type" lines then
                ifFail lines (Type "ERR" src)

            else if startsWithKeyword "port" lines then
                Done Port

            else
                let
                    exprParser : P.Parser ( Row, Col ) ( Src.C1 Src.Expr, A.Position )
                    exprParser =
                        P.specialize (toExprPosition src) (PE.expression SV.Guida)
                in
                case P.fromByteString exprParser Tuple.pair src of
                    Ok _ ->
                        ifDone lines (Expr src)

                    Err exprPosition ->
                        if exprPosition >= declPosition then
                            ifFail lines (Expr src)

                        else
                            case P.fromByteString annotation (\_ _ -> ()) src of
                                Ok name ->
                                    Continue (DefStart name)

                                Err () ->
                                    ifFail lines (Decl "ERR" src)


startsWithColon : Lines -> Bool
startsWithColon lines =
    case List.dropWhile ((==) ' ') (String.toList (getFirstLine lines)) of
        [] ->
            False

        c :: _ ->
            c == ':'


toCommand : Lines -> Input
toCommand lines =
    case String.fromList <| List.drop 1 <| List.dropWhile ((==) ' ') (String.toList (getFirstLine lines)) of
        "reset" ->
            Reset

        "exit" ->
            Exit

        "quit" ->
            Exit

        "help" ->
            Help Nothing

        rest ->
            Help (Just (String.fromList (List.takeWhile ((/=) ' ') (String.toList rest))))


startsWithKeyword : String -> Lines -> Bool
startsWithKeyword keyword lines =
    let
        line : String
        line =
            getFirstLine lines
    in
    String.startsWith keyword line
        && (case List.drop (String.length keyword) (String.toList line) of
                [] ->
                    True

                c :: _ ->
                    not (Char.isAlphaNum c)
           )


toExprPosition : String -> ES.Expr -> Row -> Col -> ( Row, Col )
toExprPosition src expr row col =
    let
        decl : ES.Decl
        decl =
            ES.DeclDef N.replValueToPrint (ES.DeclDefBody expr row col) row col
    in
    toDeclPosition src decl row col


toDeclPosition : String -> ES.Decl -> Row -> Col -> ( Row, Col )
toDeclPosition src decl r c =
    let
        err : ES.Error
        err =
            ES.ParseError (ES.Declarations decl r c)

        report : Report.Report
        report =
            ES.toReport (Code.toSource src) err

        (Report.Report _ (A.Region (A.Position row col) _) _ _) =
            report
    in
    ( row, col )


annotation : P.Parser () N.Name
annotation =
    let
        err : Row -> Col -> ()
        err _ _ =
            ()

        err_ : x -> Row -> Col -> ()
        err_ _ _ _ =
            ()
    in
    PV.lower err
        |> P.bind
            (\name ->
                PS.chompAndCheckIndent err_ err
                    |> P.bind (\_ -> P.word1 ':' err)
                    |> P.bind (\_ -> PS.chompAndCheckIndent err_ err)
                    |> P.bind (\_ -> P.specialize err_ (PT.expression []))
                    |> P.bind (\_ -> PS.checkFreshLine err)
                    |> P.fmap (\_ -> name)
            )



-- EVAL


eval : Env -> IO.ReplState -> Input -> Task Never Outcome
eval env ((IO.ReplState imports types decls) as state) input =
    case input of
        Skip ->
            Task.pure (Loop state)

        Exit ->
            Task.pure (End Exit.ExitSuccess)

        Reset ->
            IO.putStrLn "<reset>"
                |> Task.fmap (\_ -> Loop IO.initialReplState)

        Help maybeUnknownCommand ->
            IO.putStrLn (toHelpMessage maybeUnknownCommand)
                |> Task.fmap (\_ -> Loop state)

        Import name src ->
            let
                newState : IO.ReplState
                newState =
                    IO.ReplState (Dict.insert name src imports) types decls
            in
            Task.fmap Loop (attemptEval env state newState OutputNothing)

        Type name src ->
            let
                newState : IO.ReplState
                newState =
                    IO.ReplState imports (Dict.insert name src types) decls
            in
            Task.fmap Loop (attemptEval env state newState OutputNothing)

        Port ->
            IO.putStrLn "I cannot handle port declarations."
                |> Task.fmap (\_ -> Loop state)

        Decl name src ->
            let
                newState : IO.ReplState
                newState =
                    IO.ReplState imports types (Dict.insert name src decls)
            in
            Task.fmap Loop (attemptEval env state newState (OutputDecl name))

        Expr src ->
            Task.fmap Loop (attemptEval env state state (OutputExpr src))



-- ATTEMPT EVAL


type Output
    = OutputNothing
    | OutputDecl N.Name
    | OutputExpr String


attemptEval : Env -> IO.ReplState -> IO.ReplState -> Output -> Task Never IO.ReplState
attemptEval (Env root interpreter ansi) oldState newState output =
    BW.withScope
        (\scope ->
            Stuff.withRootLock root
                (Task.run
                    (Task.eio Exit.ReplBadDetails
                        (Details.load Reporting.silent scope root)
                        |> Task.bind
                            (\details ->
                                Task.eio identity
                                    (Build.fromRepl root details (toByteString newState output))
                                    |> Task.bind
                                        (\artifacts ->
                                            Utils.maybeTraverseTask (Task.mapError Exit.ReplBadGenerate << Generate.repl root details ansi artifacts) (toPrintName output)
                                        )
                            )
                    )
                )
        )
        |> Task.bind
            (\result ->
                case result of
                    Err exit ->
                        Exit.toStderr (Exit.replToReport exit)
                            |> Task.fmap (\_ -> oldState)

                    Ok Nothing ->
                        Task.pure newState

                    Ok (Just javascript) ->
                        interpret interpreter javascript
                            |> Task.fmap
                                (\exitCode ->
                                    case exitCode of
                                        Exit.ExitSuccess ->
                                            newState

                                        Exit.ExitFailure _ ->
                                            oldState
                                )
            )


interpret : FilePath -> String -> Task Never Exit.ExitCode
interpret interpreter javascript =
    let
        createProcess : { cmdspec : Process.CmdSpec, std_out : Process.StdStream, std_err : Process.StdStream, std_in : Process.StdStream }
        createProcess =
            Process.proc interpreter []
                |> (\cp -> { cp | std_in = Process.CreatePipe })
    in
    Process.withCreateProcess createProcess <|
        \stdinHandle _ _ handle ->
            case stdinHandle of
                Just stdin ->
                    Utils.builderHPutBuilder stdin javascript
                        |> Task.bind (\_ -> IO.hClose stdin)
                        |> Task.bind (\_ -> Process.waitForProcess handle)

                Nothing ->
                    crash "not implemented"



-- TO BYTESTRING


toByteString : IO.ReplState -> Output -> String
toByteString (IO.ReplState imports types decls) output =
    String.concat
        [ "module "
        , N.replModule
        , " exposing (..)\n"
        , Dict.foldr (\_ -> (++)) "" imports
        , Dict.foldr (\_ -> (++)) "" types
        , Dict.foldr (\_ -> (++)) "" decls
        , outputToBuilder output
        ]


outputToBuilder : Output -> String
outputToBuilder output =
    N.replValueToPrint
        ++ " ="
        ++ (case output of
                OutputNothing ->
                    " ()\n"

                OutputDecl _ ->
                    " ()\n"

                OutputExpr expr ->
                    List.foldr (\line rest -> "\n  " ++ line ++ rest) "\n" (Utils.lines expr)
           )



-- TO PRINT NAME


toPrintName : Output -> Maybe N.Name
toPrintName output =
    case output of
        OutputNothing ->
            Nothing

        OutputDecl name ->
            Just name

        OutputExpr _ ->
            Just N.replValueToPrint



-- HELP MESSAGES


toHelpMessage : Maybe String -> String
toHelpMessage maybeBadCommand =
    case maybeBadCommand of
        Nothing ->
            genericHelpMessage

        Just command ->
            "I do not recognize the :" ++ command ++ " command. " ++ genericHelpMessage


genericHelpMessage : String
genericHelpMessage =
    "Valid commands include:\n\n  :exit    Exit the REPL\n  :help    Show this information\n  :reset   Clear all previous imports and definitions\n\nMore info at " ++ D.makeLink "repl" ++ "\n"



-- GET ROOT


getRoot : Task Never FilePath
getRoot =
    Stuff.findRoot
        |> Task.bind
            (\maybeRoot ->
                case maybeRoot of
                    Just root ->
                        Task.pure root

                    Nothing ->
                        Stuff.getReplCache
                            |> Task.bind
                                (\cache ->
                                    let
                                        root : String
                                        root =
                                            cache ++ "/tmp"
                                    in
                                    Utils.dirCreateDirectoryIfMissing True (root ++ "/src")
                                        |> Task.bind
                                            (\_ ->
                                                Outline.write root <|
                                                    Outline.Pkg <|
                                                        Outline.PkgOutline
                                                            Pkg.dummyName
                                                            Outline.defaultSummary
                                                            Licenses.bsd3
                                                            V.one
                                                            (Outline.ExposedList [])
                                                            defaultDeps
                                                            Map.empty
                                                            C.defaultElm
                                            )
                                        |> Task.fmap (\_ -> root)
                                )
            )


defaultDeps : Dict ( String, String ) Pkg.Name C.Constraint
defaultDeps =
    Map.fromList identity
        [ ( Pkg.core, C.anything )
        , ( Pkg.json, C.anything )
        , ( Pkg.html, C.anything )
        ]



-- GET INTERPRETER


getInterpreter : Maybe String -> Task Never FilePath
getInterpreter maybeName =
    case maybeName of
        Just name ->
            getInterpreterHelp name (Utils.dirFindExecutable name)

        Nothing ->
            getInterpreterHelp "node` or `nodejs" <|
                (Utils.dirFindExecutable "node"
                    |> Task.bind
                        (\exe1 ->
                            Utils.dirFindExecutable "nodejs"
                                |> Task.fmap (\exe2 -> Maybe.or exe1 exe2)
                        )
                )


getInterpreterHelp : String -> Task Never (Maybe FilePath) -> Task Never FilePath
getInterpreterHelp name findExe =
    findExe
        |> Task.bind
            (\maybePath ->
                case maybePath of
                    Just path ->
                        Task.pure path

                    Nothing ->
                        IO.hPutStrLn IO.stderr (exeNotFound name)
                            |> Task.bind (\_ -> Exit.exitFailure)
            )


exeNotFound : String -> String
exeNotFound name =
    "The REPL relies on node.js to execute JavaScript code outside the browser.\n"
        ++ "I could not find executable `"
        ++ name
        ++ "` on your PATH though!\n\n"
        ++ "You can install node.js from <http://nodejs.org/>. If it is already installed\n"
        ++ "but has a different name, use the --interpreter flag."



-- SETTINGS


initSettings : Task Never Utils.ReplSettings
initSettings =
    Stuff.getReplCache
        |> Task.fmap
            (\cache ->
                Utils.ReplSettings
                    { historyFile = Just (cache ++ "/history")
                    , autoAddHistory = True
                    , complete = Utils.replCompleteWord Nothing " \n" lookupCompletions
                    }
            )


lookupCompletions : String -> M (List Utils.ReplCompletion)
lookupCompletions string =
    State.get
        |> State.fmap
            (\(IO.ReplState imports types decls) ->
                addMatches string False decls <|
                    addMatches string False types <|
                        addMatches string True imports <|
                            addMatches string False commands []
            )


commands : Dict.Dict N.Name ()
commands =
    Dict.fromList
        [ ( ":exit", () )
        , ( ":quit", () )
        , ( ":reset", () )
        , ( ":help", () )
        ]


addMatches : String -> Bool -> Dict.Dict N.Name v -> List Utils.ReplCompletion -> List Utils.ReplCompletion
addMatches string isFinished dict completions =
    Dict.foldr (addMatch string isFinished) completions dict


addMatch : String -> Bool -> N.Name -> v -> List Utils.ReplCompletion -> List Utils.ReplCompletion
addMatch string isFinished name _ completions =
    let
        suggestion : String
        suggestion =
            String.fromList (N.toChars name)
    in
    if String.startsWith string suggestion then
        Utils.ReplCompletion suggestion suggestion isFinished :: completions

    else
        completions
