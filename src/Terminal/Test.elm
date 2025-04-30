module Terminal.Test exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Generate as Generate
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Constraint as C
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Regex exposing (Regex)
import System.Exit as Exit
import System.IO as IO exposing (IO)
import System.Process as Process
import Terminal.Make as Make
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath)



-- RUN


type alias Task a =
    Task.Task Exit.Test a


run : List String -> () -> IO ()
run paths flags =
    Stuff.findRoot
        |> IO.bind
            (\maybeRoot ->
                Reporting.attemptWithStyle style Exit.testToReport <|
                    case maybeRoot of
                        Just root ->
                            runHelp root paths flags

                        Nothing ->
                            IO.pure (Err Exit.TestNoOutline)
            )


runHelp : String -> List String -> () -> IO (Result Exit.Test ())
runHelp root testFileGlobs () =
    Stuff.withRootLock root <|
        Task.run <|
            (Utils.dirCreateDirectoryIfMissing True (Stuff.testDir root)
                |> Task.io
                |> Task.bind
                    (\_ ->
                        Task.eio Exit.TestBadOutline (Outline.read root)
                            |> Task.bind
                                (\baseOutline ->
                                    Task.io (Utils.dirDoesDirectoryExist "tests")
                                        |> Task.bind
                                            (\testsDirExists ->
                                                Task.eio Exit.TestBadRegistry Solver.initEnv
                                                    |> Task.bind
                                                        (\env ->
                                                            case baseOutline of
                                                                Outline.App (Outline.AppOutline elm srcDirs depsDirect depsTrans testDirect testTrans) ->
                                                                    let
                                                                        addOptionalTests =
                                                                            if testsDirExists then
                                                                                NE.cons (Outline.RelativeSrcDir "tests")

                                                                            else
                                                                                identity

                                                                        newSrcDirs =
                                                                            srcDirs
                                                                                -- TODO/FIXME we shouldn't need to install elm-test...
                                                                                |> NE.cons (Outline.RelativeSrcDir "node_modules/elm-test/elm/src")
                                                                                |> addOptionalTests
                                                                                |> NE.map
                                                                                    (\srcDir ->
                                                                                        case srcDir of
                                                                                            Outline.AbsoluteSrcDir _ ->
                                                                                                srcDir

                                                                                            Outline.RelativeSrcDir path ->
                                                                                                Outline.RelativeSrcDir ("../../../" ++ path)
                                                                                    )
                                                                                |> NE.cons (Outline.RelativeSrcDir "src")
                                                                    in
                                                                    Outline.AppOutline elm newSrcDirs (Dict.union depsDirect testDirect) (Dict.union depsTrans testTrans) Dict.empty Dict.empty
                                                                        |> makeAppPlan env ( "elm", "core" )
                                                                        |> Task.bind (makeAppPlan env ( "elm", "json" ))
                                                                        |> Task.bind (makeAppPlan env ( "elm", "time" ))
                                                                        |> Task.bind (makeAppPlan env ( "elm", "random" ))
                                                                        -- TODO changes should only be done to the `tests/elm.json` in case the top level `elm.json` had changes! This will improve performance!
                                                                        |> Task.bind (attemptChanges root env << Outline.App)

                                                                Outline.Pkg outline ->
                                                                    -- TODO
                                                                    -- makePkgPlan env pkg outline
                                                                    --     |> Task.bind (\changes -> attemptChanges root env baseOutline changes)
                                                                    Debug.todo "TODO: makePkgPlan"
                                                        )
                                            )
                                )
                            |> Task.bind
                                (\_ ->
                                    let
                                        paths =
                                            case testFileGlobs of
                                                [] ->
                                                    [ root ++ "/tests" ]

                                                _ ->
                                                    testFileGlobs
                                    in
                                    resolveElmFiles paths
                                        |> IO.bind
                                            (\resolvedInputFiles ->
                                                case resolvedInputFiles of
                                                    Ok inputFiles ->
                                                        inputFiles
                                                            |> Utils.listTraverse
                                                                (\inputFile ->
                                                                    case List.filter (\path -> String.startsWith path inputFile) paths of
                                                                        [] ->
                                                                            Debug.todo "TODO: handle empty paths"

                                                                        [ _ ] ->
                                                                            extractExposedPossiblyTests inputFile
                                                                                |> IO.fmap (Maybe.map (Tuple.pair inputFile))

                                                                        _ ->
                                                                            Debug.todo "TODO: handle multiple paths"
                                                                )

                                                    Err _ ->
                                                        -- TODO
                                                        IO.pure []
                                            )
                                        |> IO.fmap (List.filterMap identity)
                                        |> IO.bind
                                            (\exposedList ->
                                                let
                                                    testModules =
                                                        List.map
                                                            (\( _, ( moduleName, possiblyTests ) ) ->
                                                                { moduleName = moduleName
                                                                , possiblyTests = possiblyTests
                                                                }
                                                            )
                                                            exposedList
                                                in
                                                Utils.dirCreateDirectoryIfMissing True (Stuff.testDir root ++ "/src/Test/Generated")
                                                    |> IO.bind
                                                        (\_ ->
                                                            IO.writeString (Stuff.testDir root ++ "/src/Test/Generated/Main.elm") (testGeneratedMain testModules testFileGlobs (List.map Tuple.first exposedList))
                                                        )
                                                    |> IO.bind (\_ -> Reporting.terminal)
                                                    |> IO.bind
                                                        (\terminalStyle ->
                                                            Reporting.attemptWithStyle terminalStyle Exit.testToReport <|
                                                                Utils.dirWithCurrentDirectory (Stuff.testDir root)
                                                                    (runMake (Stuff.testDir root) "src/Test/Generated/Main.elm")
                                                        )
                                                    |> IO.bind
                                                        (\content ->
                                                            IO.hPutStrLn IO.stdout "Starting tests"
                                                                |> IO.bind
                                                                    (\_ ->
                                                                        getInterpreter
                                                                            |> IO.bind
                                                                                (\interpreter ->
                                                                                    let
                                                                                        finalContent =
                                                                                            before
                                                                                                ++ "\nvar Elm = (function(module) {\n"
                                                                                                ++ addKernelTestChecking content
                                                                                                ++ "\nreturn this.Elm;\n})({});\n"
                                                                                                ++ after
                                                                                    in
                                                                                    interpret interpreter finalContent
                                                                                )
                                                                    )
                                                        )
                                            )
                                        |> Task.io
                                )
                            |> Task.fmap (\_ -> ())
                    )
            )


interpret : FilePath -> String -> IO Exit.ExitCode
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
                        |> IO.bind (\_ -> IO.hClose stdin)
                        |> IO.bind (\_ -> Process.waitForProcess handle)

                Nothing ->
                    crash "not implemented"


testVariantDefinition : Regex
testVariantDefinition =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith { caseInsensitive = False, multiline = True }
            "^var\\s+\\$elm_explorations\\$test\\$Test\\$Internal\\$(?:ElmTestVariant__\\w+|UnitTest|FuzzTest|Labeled|Skipped|Only|Batch)\\s*=\\s*(?:\\w+\\(\\s*)?function\\s*\\([\\w, ]*\\)\\s*\\{\\s*return *\\{"


checkDefinition : Regex
checkDefinition =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith { caseInsensitive = False, multiline = True }
            "^(var\\s+\\$author\\$project\\$Test\\$Runner\\$Node\\$check)\\s*=\\s*\\$author\\$project\\$Test\\$Runner\\$Node\\$checkHelperReplaceMe___;?$"


addKernelTestChecking : String -> String
addKernelTestChecking content =
    "var __elmTestSymbol = Symbol(\"elmTestSymbol\");\n"
        ++ (content
                |> Regex.replace testVariantDefinition (\{ match } -> match ++ "__elmTestSymbol: __elmTestSymbol, ")
                |> Regex.replaceAtMost 1
                    checkDefinition
                    (\{ submatches } ->
                        case submatches of
                            (Just firstSubmatch) :: _ ->
                                firstSubmatch ++ " = value => value && value.__elmTestSymbol === __elmTestSymbol ? $elm$core$Maybe$Just(value) : $elm$core$Maybe$Nothing;"

                            _ ->
                                crash "addKernelTestChecking: no submatches found"
                    )
           )


before : String
before =
    """// Apply Node polyfills as necessary.
var window = {
  Date: Date,
  addEventListener: function () {},
  removeEventListener: function () {},
};

var location = {
  href: '',
  host: '',
  hostname: '',
  protocol: '',
  origin: '',
  port: '',
  pathname: '',
  search: '',
  hash: '',
  username: '',
  password: '',
};
var document = { body: {}, createTextNode: function () {}, location: location };

if (typeof FileList === 'undefined') {
  FileList = function () {};
}

if (typeof File === 'undefined') {
  File = function () {};
}

if (typeof XMLHttpRequest === 'undefined') {
  XMLHttpRequest = function () {
    return {
      addEventListener: function () {},
      open: function () {},
      send: function () {},
    };
  };

  var oldConsoleWarn = console.warn;
  console.warn = function () {
    if (
      arguments.length === 1 &&
      arguments[0].indexOf('Compiled in DEV mode') === 0
    )
      return;
    return oldConsoleWarn.apply(console, arguments);
  };
}

if (typeof FormData === 'undefined') {
  FormData = function () {
    this._data = [];
  };
  FormData.prototype.append = function () {
    this._data.push(Array.prototype.slice.call(arguments));
  };
}
"""


after : String
after =
    """// Run the Elm app.
var app = Elm.Test.Generated.Main.init({ flags: Date.now() });

var report = 'console';

var nextResultToPrint = null;
var results = new Map();
var failures = 0;
var todos = [];
var testsToRun = -1;
var startingTime = Date.now();

function printResult(result) {
    switch (report) {
        case 'console':
            switch (result.type) {
                case 'begin':
                    console.log(makeWindowsSafe(result.output));
                    break;
                case 'complete':
                    switch (result.status) {
                        case 'pass':
                            // passed tests should be printed only if they contain distributionReport
                            if (result.distributionReport !== undefined) {
                                console.log(makeWindowsSafe(result.distributionReport));
                            }
                            break;
                        case 'todo':
                            // todos will be shown in the SUMMARY only.
                            break;
                        case 'fail':
                            console.log(makeWindowsSafe(result.failure));
                            break;
                        default:
                            throw new Error(`Unexpected result.status: ${result.status}`);
                    }
                    break;
                case 'summary':
                    console.log(makeWindowsSafe(result.summary));
                    break;
                default:
                    throw new Error(`Unexpected result.type: ${result.type}`);
            }
            break;

        case 'json':
            console.log(JSON.stringify(result));
            break;

        case 'junit':
            // JUnit does everything at once in SUMMARY, elsewhere
            break;
    }
}

function flushResults() {
    // Only print any results if we're ready - that is, nextResultToPrint
    // is no longer null. (BEGIN changes it from null to 0.)
    if (nextResultToPrint !== null) {
        var result = results.get(nextResultToPrint);

        while (
            // If there are no more results to print, then we're done.
            nextResultToPrint < testsToRun &&
            // Otherwise, keep going until we have no result available to print.
            typeof result !== 'undefined'
        ) {
            printResult(result);
            nextResultToPrint++;
            result = results.get(nextResultToPrint);
        }
    }
}

function handleResults(response) {
    // TODO print progress bar - e.g. "Running test 5 of 20" on a bar!
    // -- yikes, be careful though...test the scenario where test
    // authors put Debug.log in their tests - does that mess
    // everything up re: the line feed? Seems like it would...
    // ...so maybe a bar is not best. Can we do better? Hm.
    // Maybe the answer is to print the thing, then Immediately
    // backtrack the line feed, so that if someone else does more
    // logging, it will overwrite our status update and that's ok?

    Object.keys(response.results).forEach(function (index) {
        var result = response.results[index];
        results.set(parseInt(index), result);

        switch (report) {
            case 'console':
                switch (result.status) {
                    case 'pass':
                        // It's a PASS; no need to take any action.
                        break;
                    case 'todo':
                        todos.push(result);
                        break;
                    case 'fail':
                        failures++;
                        break;
                    default:
                        throw new Error(`Unexpected result.status: ${result.status}`);
                }
                break;
            case 'junit':
                if (typeof result.failure !== 'undefined') {
                    failures++;
                }
                break;
            case 'json':
                if (result.status === 'fail') {
                    failures++;
                } else if (result.status === 'todo') {
                    todos.push({ labels: result.labels, todo: result.failures[0] });
                }
                break;
        }
    });

    flushResults();
}

function makeWindowsSafe(text) {
    return process.platform === 'win32' ? windowsify(text) : text;
}

// Fix Windows Unicode problems. Credit to https://github.com/sindresorhus/figures for the Windows compat idea!
var windowsSubstitutions = [
    [/[↓✗►]/g, '>'],
    [/╵│╷╹┃╻/g, '|'],
    [/═/g, '='],
    [/▔/g, '-'],
    [/✔/g, '√'],
];

function windowsify(str) {
    return windowsSubstitutions.reduce(function (result /*: string */, sub) {
        return result.replace(sub[0], sub[1]);
    }, str);
}

// Use ports for inter-process communication.
app.ports.elmTestPort__send.subscribe(function (msg) {
    var response = JSON.parse(msg);

    switch (response.type) {
        case 'FINISHED':
            handleResults(response);

            // Print the summmary.
            app.ports.elmTestPort__receive.send(
                {
                    type: 'SUMMARY',
                    duration: Date.now() - startingTime,
                    failures: failures,
                    todos: todos,
                }
            );

            break;
        case 'SUMMARY':
            flushResults();

            if (response.exitCode === 1) {
                // The tests could not even run. At the time of this writing, the
                // only case is “No exposed values of type Test found”. That
                // _could_ have been caught at compile time, but the current
                // architecture needs to actually run the JS to figure out which
                // exposed values are of type Test. That’s why this type of
                // response is handled differently than others.
                console.error(response.message);
            } else {
                printResult(response.message);

                if (report === 'junit') {
                    var xml = response.message;
                    var values = Array.from(results.values());

                    xml.testsuite.testcase = xml.testsuite.testcase.concat(values);

                    // The XmlBuilder by default does not remove characters that are
                    // invalid in XML, like backspaces. However, we can pass it an
                    // `invalidCharReplacement` option to tell it how to handle
                    // those characters, rather than crashing. In an attempt to
                    // retain useful information in the output, we try and output a
                    // hex-encoded unicode codepoint for the invalid character. For
                    // example, the start of a terminal escape (`\u{001B}` in Elm) will be output as a
                    // literal `\u{001B}`.
                    var invalidCharReplacement = function (char) {
                        return (
                            '\\\\u{' +
                            char.codePointAt(0).toString(16).padStart(4, '0') +
                            '}'
                        );
                    };

                    console.log(
                        XmlBuilder.create(xml, {
                            invalidCharReplacement: invalidCharReplacement,
                        }).end()
                    );
                }
            }

            // resolve(response.exitCode);
            break;
        case 'BEGIN':
            testsToRun = response.testCount;

            // TODO
            // if (!Report.isMachineReadable(report)) {
            //     var headline = 'elm-test ' + elmTestVersion;
            //     var bar = '-'.repeat(headline.length);

            //     console.log('\\n' + headline + '\\n' + bar + '\\n');
            // }

            printResult(response.message);

            // Now we're ready to print results!
            nextResultToPrint = 0;

            flushResults();

            break;
        case 'RESULTS':
            handleResults(response);

            break;
        case 'ERROR':
            throw new Error(response.message);
        default:
            throw new Error(
                'Unrecognized message from worker:' + response.type
            );
    }
});

app.ports.elmTestPort__receive.send({ type: 'TEST', index: -1 });"""


testGeneratedMain :
    List
        { moduleName : String
        , possiblyTests : List String
        }
    -> List String
    -> List String
    -> String
testGeneratedMain testModules testFileGlobs testFilePaths =
    let
        imports =
            List.map (\mod -> "import " ++ mod.moduleName) testModules

        possiblyTestsList =
            List.map makeModuleTuple testModules
    in
    -- TODO fix runs, report and seed entries!
    """module Test.Generated.Main exposing (main)

""" ++ String.join "\n" imports ++ """

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 342047891320010
        , processes = 1
        , globs =
            """ ++ indentAllButFirstLine 12 (List.map (Encode.encode 0 << Encode.string) testFileGlobs) ++ """
        , paths =
            """ ++ indentAllButFirstLine 12 (List.map (Encode.encode 0 << Encode.string) testFilePaths) ++ """
        }
        """ ++ indentAllButFirstLine 8 possiblyTestsList


indentAllButFirstLine : Int -> List String -> String
indentAllButFirstLine indent list =
    "[ "
        ++ String.join ("\n" ++ String.repeat indent " " ++ ", ") list
        ++ "\n"
        ++ String.repeat indent " "
        ++ "]"


makeModuleTuple : { moduleName : String, possiblyTests : List String } -> String
makeModuleTuple mod =
    let
        list =
            List.map (\test -> "Test.Runner.Node.check " ++ mod.moduleName ++ "." ++ test)
                mod.possiblyTests
    in
    "( \""
        ++ mod.moduleName
        ++ "\"\n"
        ++ String.repeat 10 " "
        ++ ", "
        ++ indentAllButFirstLine 12 list
        ++ "\n"
        ++ String.repeat 10 " "
        ++ ")"



-- GET INFORMATION


style : Reporting.Style
style =
    Reporting.silent


extractExposedPossiblyTests : String -> IO (Maybe ( String, List String ))
extractExposedPossiblyTests path =
    File.readUtf8 path
        |> IO.bind
            (\bytes ->
                case Parse.fromByteString (SV.fileSyntaxVersion path) Parse.Application bytes of
                    Ok (Src.Module _ (Just (A.At _ name)) (A.At _ exposing_) _ _ _ _ _ _ _) ->
                        let
                            exposed =
                                case exposing_ of
                                    Src.Open ->
                                        Debug.todo "TODO: Open"

                                    Src.Explicit exposedList ->
                                        List.filterMap
                                            (\exposedValue ->
                                                case exposedValue of
                                                    Src.Lower (A.At _ lowerName) ->
                                                        Just lowerName

                                                    Src.Upper _ _ ->
                                                        Nothing

                                                    Src.Operator _ _ ->
                                                        Nothing
                                            )
                                            exposedList
                        in
                        IO.pure (Just ( name, exposed ))

                    _ ->
                        IO.pure Nothing
            )



-- COMMAND LINE


type FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


stat : FilePath -> IO FileType
stat path =
    Utils.dirDoesFileExist path
        |> IO.bind
            (\isFile ->
                Utils.dirDoesDirectoryExist path
                    |> IO.fmap
                        (\isDirectory ->
                            case ( isFile, isDirectory ) of
                                ( True, _ ) ->
                                    IsFile

                                ( _, True ) ->
                                    IsDirectory

                                ( False, False ) ->
                                    DoesNotExist
                        )
            )



-- RESOLVE FILES


type Error
    = FileDoesNotExist FilePath
    | NoElmFiles FilePath


resolveFile : FilePath -> IO (Result Error (List FilePath))
resolveFile path =
    stat path
        |> IO.bind
            (\fileType ->
                case fileType of
                    IsFile ->
                        IO.pure (Ok [ path ])

                    IsDirectory ->
                        findAllElmFiles path
                            |> IO.fmap
                                (\elmFiles ->
                                    case elmFiles of
                                        [] ->
                                            Err (NoElmFiles path)

                                        _ ->
                                            Ok elmFiles
                                )

                    DoesNotExist ->
                        IO.pure (Err (FileDoesNotExist path))
            )


resolveElmFiles : List FilePath -> IO (Result (List Error) (List FilePath))
resolveElmFiles inputFiles =
    IO.mapM resolveFile inputFiles
        |> IO.fmap collectErrors
        |> IO.fmap
            (\result ->
                case result of
                    Err ls ->
                        Err ls

                    Ok files ->
                        Ok (List.concat files)
            )


collectErrors : List (Result e v) -> Result (List e) (List v)
collectErrors =
    List.foldl
        (\next acc ->
            case ( next, acc ) of
                ( Err e, Ok _ ) ->
                    Err [ e ]

                ( Err e, Err es ) ->
                    Err (e :: es)

                ( Ok v, Ok vs ) ->
                    Ok (v :: vs)

                ( Ok _, Err es ) ->
                    Err es
        )
        (Ok [])



-- FILESYSTEM


collectFiles : (a -> IO (List a)) -> a -> IO (List a)
collectFiles children root =
    children root
        |> IO.bind (\xs -> IO.mapM (collectFiles children) xs)
        |> IO.fmap (\subChildren -> root :: List.concat subChildren)


listDir : FilePath -> IO (List FilePath)
listDir path =
    Utils.dirListDirectory path
        |> IO.fmap (List.map (\file -> path ++ "/" ++ file))


fileList : FilePath -> IO (List FilePath)
fileList =
    let
        children : FilePath -> IO (List FilePath)
        children path =
            if isSkippable path then
                IO.pure []

            else
                Utils.dirDoesDirectoryExist path
                    |> IO.bind
                        (\directory ->
                            if directory then
                                listDir path

                            else
                                IO.pure []
                        )
    in
    collectFiles children


isSkippable : FilePath -> Bool
isSkippable path =
    List.any identity
        [ hasFilename "elm-stuff" path
        , hasFilename "node_modules" path
        , hasFilename ".git" path
        ]


hasExtension : String -> FilePath -> Bool
hasExtension ext path =
    ext == Utils.fpTakeExtension path


findAllElmFiles : FilePath -> IO (List FilePath)
findAllElmFiles inputFile =
    fileList inputFile
        |> IO.fmap (List.filter (hasExtension ".elm"))


hasFilename : String -> FilePath -> Bool
hasFilename name path =
    name == Utils.fpTakeFileName path


{-| FROM INSTALL
-}



-- ATTEMPT CHANGES


attemptChanges : FilePath -> Solver.Env -> Outline.Outline -> Task ()
attemptChanges root env newOutline =
    Task.eio Exit.TestBadDetails <|
        BW.withScope
            (\scope ->
                Outline.write (Stuff.testDir root) newOutline
                    |> IO.bind (\_ -> Details.verifyInstall scope root env newOutline)
            )



-- MAKE APP PLAN


makeAppPlan : Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task Outline.AppOutline
makeAppPlan (Solver.Env cache _ connection registry) pkg ((Outline.AppOutline elmVersion sourceDirs direct indirect testDirect testIndirect) as outline) =
    if Dict.member identity pkg direct then
        Task.pure outline

    else
        -- is it already indirect?
        case Dict.get identity pkg indirect of
            Just vsn ->
                Task.pure <|
                    Outline.AppOutline elmVersion
                        sourceDirs
                        (Dict.insert identity pkg vsn direct)
                        (Dict.remove identity pkg indirect)
                        testDirect
                        testIndirect

            Nothing ->
                -- is it already a test dependency?
                case Dict.get identity pkg testDirect of
                    Just vsn ->
                        Task.pure <|
                            Outline.AppOutline elmVersion
                                sourceDirs
                                (Dict.insert identity pkg vsn direct)
                                indirect
                                (Dict.remove identity pkg testDirect)
                                testIndirect

                    Nothing ->
                        -- is it already an indirect test dependency?
                        case Dict.get identity pkg testIndirect of
                            Just vsn ->
                                Task.pure <|
                                    Outline.AppOutline elmVersion
                                        sourceDirs
                                        (Dict.insert identity pkg vsn direct)
                                        indirect
                                        testDirect
                                        (Dict.remove identity pkg testIndirect)

                            Nothing ->
                                -- finally try to add it from scratch
                                case Registry.getVersions_ pkg registry of
                                    Err _ ->
                                        case connection of
                                            Solver.Online _ ->
                                                -- Task.throw (Exit.InstallUnknownPackageOnline pkg suggestions)
                                                Task.throw Exit.TestNoOutline

                                            Solver.Offline ->
                                                -- Task.throw (Exit.InstallUnknownPackageOffline pkg suggestions)
                                                Task.throw Exit.TestNoOutline

                                    Ok _ ->
                                        Task.io (Solver.addToApp cache connection registry pkg outline False)
                                            |> Task.bind
                                                (\result ->
                                                    case result of
                                                        Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                                            Task.pure app

                                                        Solver.NoSolution ->
                                                            -- Task.throw (Exit.InstallNoOnlineAppSolution pkg)
                                                            Task.throw Exit.TestNoOutline

                                                        Solver.NoOfflineSolution ->
                                                            -- Task.throw (Exit.InstallNoOfflineAppSolution pkg)
                                                            Task.throw Exit.TestNoOutline

                                                        Solver.SolverErr exit ->
                                                            -- Task.throw (Exit.InstallHadSolverTrouble exit)
                                                            Task.throw Exit.TestNoOutline
                                                )



-- MAKE PACKAGE PLAN
-- makePkgPlan : Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Bool -> Task (Changes C.Constraint)
-- makePkgPlan (Solver.Env cache _ connection registry) pkg (Outline.PkgOutline name summary license version exposed deps test elmVersion) forTest =
--     if forTest then
--         if Dict.member identity pkg test then
--             Task.pure AlreadyInstalled
--         else
--             -- try to add a new dependency
--             case Registry.getVersions_ pkg registry of
--                 Err suggestions ->
--                     case connection of
--                         Solver.Online _ ->
--                             Task.throw (Exit.InstallUnknownPackageOnline pkg suggestions)
--                         Solver.Offline ->
--                             Task.throw (Exit.InstallUnknownPackageOffline pkg suggestions)
--                 Ok (Registry.KnownVersions _ _) ->
--                     let
--                         cons : Dict ( String, String ) Pkg.Name C.Constraint
--                         cons =
--                             Dict.insert identity pkg C.anything test
--                     in
--                     Task.io (Solver.verify cache connection registry cons)
--                         |> Task.bind
--                             (\result ->
--                                 case result of
--                                     Solver.SolverOk solution ->
--                                         let
--                                             (Solver.Details vsn _) =
--                                                 Utils.find identity pkg solution
--                                             con : C.Constraint
--                                             con =
--                                                 C.untilNextMajor vsn
--                                             newTest : Dict ( String, String ) Pkg.Name C.Constraint
--                                             newTest =
--                                                 Dict.insert identity pkg con test
--                                             changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
--                                             changes =
--                                                 detectChanges test newTest
--                                             news : Dict ( String, String ) Pkg.Name C.Constraint
--                                             news =
--                                                 Utils.mapMapMaybe identity Pkg.compareName keepNew changes
--                                         in
--                                         Task.pure <|
--                                             Changes changes <|
--                                                 Outline.Pkg <|
--                                                     Outline.PkgOutline name
--                                                         summary
--                                                         license
--                                                         version
--                                                         exposed
--                                                         deps
--                                                         (addNews (Just pkg) news test)
--                                                         elmVersion
--                                     Solver.NoSolution ->
--                                         Task.throw (Exit.InstallNoOnlinePkgSolution pkg)
--                                     Solver.NoOfflineSolution ->
--                                         Task.throw (Exit.InstallNoOfflinePkgSolution pkg)
--                                     Solver.SolverErr exit ->
--                                         Task.throw (Exit.InstallHadSolverTrouble exit)
--                             )
--     else if Dict.member identity pkg deps then
--         Task.pure AlreadyInstalled
--     else
--         -- is already in test dependencies?
--         case Dict.get identity pkg test of
--             Just con ->
--                 Task.pure <|
--                     PromoteTest <|
--                         Outline.Pkg <|
--                             Outline.PkgOutline name
--                                 summary
--                                 license
--                                 version
--                                 exposed
--                                 (Dict.insert identity pkg con deps)
--                                 (Dict.remove identity pkg test)
--                                 elmVersion
--             Nothing ->
--                 -- try to add a new dependency
--                 case Registry.getVersions_ pkg registry of
--                     Err suggestions ->
--                         case connection of
--                             Solver.Online _ ->
--                                 Task.throw (Exit.InstallUnknownPackageOnline pkg suggestions)
--                             Solver.Offline ->
--                                 Task.throw (Exit.InstallUnknownPackageOffline pkg suggestions)
--                     Ok (Registry.KnownVersions _ _) ->
--                         let
--                             old : Dict ( String, String ) Pkg.Name C.Constraint
--                             old =
--                                 Dict.union deps test
--                             cons : Dict ( String, String ) Pkg.Name C.Constraint
--                             cons =
--                                 Dict.insert identity pkg C.anything old
--                         in
--                         Task.io (Solver.verify cache connection registry cons)
--                             |> Task.bind
--                                 (\result ->
--                                     case result of
--                                         Solver.SolverOk solution ->
--                                             let
--                                                 (Solver.Details vsn _) =
--                                                     Utils.find identity pkg solution
--                                                 con : C.Constraint
--                                                 con =
--                                                     C.untilNextMajor vsn
--                                                 new : Dict ( String, String ) Pkg.Name C.Constraint
--                                                 new =
--                                                     Dict.insert identity pkg con old
--                                                 changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
--                                                 changes =
--                                                     detectChanges old new
--                                                 news : Dict ( String, String ) Pkg.Name C.Constraint
--                                                 news =
--                                                     Utils.mapMapMaybe identity Pkg.compareName keepNew changes
--                                             in
--                                             Task.pure <|
--                                                 Changes changes <|
--                                                     Outline.Pkg <|
--                                                         Outline.PkgOutline name
--                                                             summary
--                                                             license
--                                                             version
--                                                             exposed
--                                                             (addNews (Just pkg) news deps)
--                                                             (addNews Nothing news test)
--                                                             elmVersion
--                                         Solver.NoSolution ->
--                                             Task.throw (Exit.InstallNoOnlinePkgSolution pkg)
--                                         Solver.NoOfflineSolution ->
--                                             Task.throw (Exit.InstallNoOfflinePkgSolution pkg)
--                                         Solver.SolverErr exit ->
--                                             Task.throw (Exit.InstallHadSolverTrouble exit)
--                                 )


addNews : Maybe Pkg.Name -> Dict ( String, String ) Pkg.Name C.Constraint -> Dict ( String, String ) Pkg.Name C.Constraint -> Dict ( String, String ) Pkg.Name C.Constraint
addNews pkg new old =
    Dict.merge compare
        (Dict.insert identity)
        (\k _ n -> Dict.insert identity k n)
        (\k c acc ->
            if Just k == pkg then
                Dict.insert identity k c acc

            else
                acc
        )
        old
        new
        Dict.empty



-- CHANGES


type Change a
    = Insert a
    | Change a a
    | Remove a


detectChanges : Dict ( String, String ) Pkg.Name a -> Dict ( String, String ) Pkg.Name a -> Dict ( String, String ) Pkg.Name (Change a)
detectChanges old new =
    Dict.merge compare
        (\k v -> Dict.insert identity k (Remove v))
        (\k oldElem newElem acc ->
            case keepChange k oldElem newElem of
                Just change ->
                    Dict.insert identity k change acc

                Nothing ->
                    acc
        )
        (\k v -> Dict.insert identity k (Insert v))
        old
        new
        Dict.empty


keepChange : k -> v -> v -> Maybe (Change v)
keepChange _ old new =
    if old == new then
        Nothing

    else
        Just (Change old new)


keepNew : Change a -> Maybe a
keepNew change =
    case change of
        Insert a ->
            Just a

        Change _ a ->
            Just a

        Remove _ ->
            Nothing



-- GET INTERPRETER


getInterpreter : IO FilePath
getInterpreter =
    getInterpreterHelp "node` or `nodejs" <|
        (Utils.dirFindExecutable "node"
            |> IO.bind
                (\exe1 ->
                    Utils.dirFindExecutable "nodejs"
                        |> IO.fmap (\exe2 -> Maybe.or exe1 exe2)
                )
        )


getInterpreterHelp : String -> IO (Maybe FilePath) -> IO FilePath
getInterpreterHelp name findExe =
    findExe
        |> IO.bind
            (\maybePath ->
                case maybePath of
                    Just path ->
                        IO.pure path

                    Nothing ->
                        IO.hPutStrLn IO.stderr (exeNotFound name)
                            |> IO.bind (\_ -> Exit.exitFailure)
            )


exeNotFound : String -> String
exeNotFound name =
    "The TEST relies on node.js to execute JavaScript code outside the browser.\n"
        ++ "I could not find executable `"
        ++ name
        ++ "` on your PATH though!\n\n"
        ++ "You can install node.js from <http://nodejs.org/>. If it is already installed\n"
        ++ "but has a different name, use the --interpreter flag."


{-| FROM MAKE
-}
runMake : String -> String -> IO (Result Exit.Test String)
runMake root path =
    BW.withScope
        (\scope ->
            Task.run <|
                (Task.eio Exit.TestBadDetails (Details.load style scope root)
                    |> Task.bind
                        (\details ->
                            buildPaths root details (NE.Nonempty path [])
                                |> Task.bind
                                    (\artifacts ->
                                        toBuilder 0 root details artifacts
                                    )
                        )
                )
        )


buildPaths : FilePath -> Details.Details -> NE.Nonempty FilePath -> Task Build.Artifacts
buildPaths root details paths =
    Task.eio Exit.TestCannotBuild <|
        Build.fromPaths style root details paths



-- TO BUILDER


toBuilder : Int -> FilePath -> Details.Details -> Build.Artifacts -> Task String
toBuilder leadingLines root details artifacts =
    Task.mapError Exit.TestBadGenerate <|
        Generate.dev False leadingLines root details artifacts
