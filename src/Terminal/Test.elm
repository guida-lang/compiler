module Terminal.Test exposing
    ( Flags(..)
    , Report(..)
    , format
    , parseReport
    , run
    )

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
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.Name exposing (Name)
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Constraint as C
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Regex exposing (Regex)
import System.Exit as Exit
import System.IO as IO
import System.Process as Process
import Task exposing (Task)
import Terminal.Terminal.Internal exposing (Parser(..))
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as TE



-- RUN


type Flags
    = Flags (Maybe Int) (Maybe Int) (Maybe Report)


run : List String -> Flags -> Task Never ()
run paths flags =
    Stuff.findRoot
        |> TE.bind
            (\maybeRoot ->
                Reporting.attemptWithStyle style Exit.testToReport <|
                    case maybeRoot of
                        Just root ->
                            runHelp root paths flags

                        Nothing ->
                            TE.pure (Err Exit.TestNoOutline)
            )


runHelp : String -> List String -> Flags -> Task Never (Result Exit.Test ())
runHelp root testFileGlobs flags =
    Stuff.withRootLock root <|
        TE.toResult <|
            (Utils.dirCreateDirectoryIfMissing True (Stuff.testDir root)
                |> TE.bind (\_ -> Utils.nodeGetDirname)
                |> TE.io
                |> TE.bind
                    (\nodeDirname ->
                        TE.eio Exit.TestBadOutline (Outline.read root)
                            |> TE.bind
                                (\baseOutline ->
                                    TE.io (Utils.dirDoesDirectoryExist "tests")
                                        |> TE.bind
                                            (\testsDirExists ->
                                                TE.eio Exit.TestBadRegistry Solver.initEnv
                                                    |> TE.bind
                                                        (\env ->
                                                            let
                                                                addOptionalTests : NE.Nonempty Outline.SrcDir -> NE.Nonempty Outline.SrcDir
                                                                addOptionalTests =
                                                                    if testsDirExists then
                                                                        NE.cons (Outline.RelativeSrcDir "tests")

                                                                    else
                                                                        identity

                                                                newSrcDirs : NE.Nonempty Outline.SrcDir -> NE.Nonempty Outline.SrcDir
                                                                newSrcDirs srcDirs =
                                                                    srcDirs
                                                                        |> addOptionalTests
                                                                        |> NE.map
                                                                            (\srcDir ->
                                                                                case srcDir of
                                                                                    Outline.AbsoluteSrcDir _ ->
                                                                                        srcDir

                                                                                    Outline.RelativeSrcDir path ->
                                                                                        Outline.RelativeSrcDir ("../../../" ++ path)
                                                                            )
                                                                        |> NE.cons (Outline.AbsoluteSrcDir (Utils.fpCombine nodeDirname "../libraries/test/src"))
                                                                        |> NE.cons (Outline.RelativeSrcDir "src")
                                                            in
                                                            case baseOutline of
                                                                Outline.App (Outline.AppOutline elm srcDirs depsDirect depsTrans testDirect testTrans) ->
                                                                    Outline.AppOutline elm (newSrcDirs srcDirs) (Dict.union depsDirect testDirect) (Dict.union depsTrans testTrans) Dict.empty Dict.empty
                                                                        |> makeAppPlan env Pkg.core
                                                                        |> TE.bind (makeAppPlan env Pkg.json)
                                                                        |> TE.bind (makeAppPlan env Pkg.time)
                                                                        |> TE.bind (makeAppPlan env Pkg.random)
                                                                        -- TODO changes should only be done to the `tests/elm.json` in case the top level `elm.json` had changes! This will improve performance!
                                                                        |> TE.bind (attemptChanges root env)

                                                                Outline.Pkg (Outline.PkgOutline _ _ _ _ _ deps test _) ->
                                                                    Outline.AppOutline V.elmCompiler (newSrcDirs (NE.singleton (Outline.RelativeSrcDir "src"))) Dict.empty Dict.empty Dict.empty Dict.empty
                                                                        |> makePkgPlan env (Dict.union deps test)
                                                                        |> TE.bind (makeAppPlan env Pkg.core)
                                                                        |> TE.bind (makeAppPlan env Pkg.json)
                                                                        |> TE.bind (makeAppPlan env Pkg.time)
                                                                        |> TE.bind (makeAppPlan env Pkg.random)
                                                                        -- TODO changes should only be done to the `tests/elm.json` in case the top level `elm.json` had changes! This will improve performance!
                                                                        |> TE.bind (attemptChanges root env)
                                                        )
                                            )
                                )
                            |> TE.bind
                                (\_ ->
                                    let
                                        paths : List String
                                        paths =
                                            case testFileGlobs of
                                                [] ->
                                                    [ root ++ "/tests" ]

                                                _ ->
                                                    testFileGlobs
                                    in
                                    resolveElmFiles paths
                                        |> TE.bind
                                            (\resolvedInputFiles ->
                                                case resolvedInputFiles of
                                                    Ok inputFiles ->
                                                        inputFiles
                                                            |> Utils.listTraverse
                                                                (\inputFile ->
                                                                    case List.filter (\path -> String.startsWith path inputFile) paths of
                                                                        _ :: [] ->
                                                                            extractExposedPossiblyTests inputFile
                                                                                |> TE.fmap (Maybe.map (Tuple.pair inputFile))

                                                                        _ ->
                                                                            TE.pure Nothing
                                                                )

                                                    Err _ ->
                                                        TE.pure []
                                            )
                                        |> TE.fmap (List.filterMap identity)
                                        |> TE.bind
                                            (\exposedList ->
                                                Utils.dirCreateDirectoryIfMissing True (Stuff.testDir root ++ "/src/Test/Generated")
                                                    |> TE.bind
                                                        (\_ ->
                                                            let
                                                                testModules : List { moduleName : String, possiblyTests : List String }
                                                                testModules =
                                                                    List.map
                                                                        (\( _, ( moduleName, possiblyTests ) ) ->
                                                                            { moduleName = moduleName
                                                                            , possiblyTests = possiblyTests
                                                                            }
                                                                        )
                                                                        exposedList
                                                            in
                                                            testGeneratedMain testModules testFileGlobs (List.map Tuple.first exposedList) flags
                                                        )
                                                    |> TE.bind (IO.writeString (Stuff.testDir root ++ "/src/Test/Generated/Main.elm"))
                                                    |> TE.bind (\_ -> Reporting.terminal)
                                                    |> TE.bind
                                                        (\terminalStyle ->
                                                            Reporting.attemptWithStyle terminalStyle Exit.testToReport <|
                                                                Utils.dirWithCurrentDirectory (Stuff.testDir root)
                                                                    (runMake (Stuff.testDir root) "src/Test/Generated/Main.elm")
                                                        )
                                                    |> TE.bind
                                                        (\content ->
                                                            IO.hPutStrLn IO.stdout "Starting tests"
                                                                |> TE.bind
                                                                    (\_ ->
                                                                        getInterpreter
                                                                            |> TE.bind
                                                                                (\interpreter ->
                                                                                    let
                                                                                        finalContent : String
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
                                        |> TE.io
                                )
                            |> TE.fmap (\_ -> ())
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
                        |> TE.bind (\_ -> IO.hClose stdin)
                        |> TE.bind (\_ -> Process.waitForProcess handle)

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

            if (!isMachineReadable(report)) {
                var headline = 'elm-test """ ++ V.toChars V.elmCompiler ++ """';
                var bar = '-'.repeat(headline.length);

                console.log('\\n' + headline + '\\n' + bar + '\\n');
            }

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

function isMachineReadable(report) {
  switch (report) {
    case 'json':
    case 'junit':
      return true;
    case 'console':
      return false;
  }
}

app.ports.elmTestPort__receive.send({ type: 'TEST', index: -1 });"""


testGeneratedMain :
    List
        { moduleName : String
        , possiblyTests : List String
        }
    -> List String
    -> List String
    -> Flags
    -> Task Never String
testGeneratedMain testModules testFileGlobs testFilePaths (Flags maybeSeed maybeRuns report) =
    let
        seedIO : Task Never Int
        seedIO =
            case maybeSeed of
                Just seedValue ->
                    TE.pure seedValue

                Nothing ->
                    Utils.nodeMathRandom
                        |> TE.fmap (\seedRandom -> floor (seedRandom * 407199254740991) + 1000)

        imports : List String
        imports =
            List.map (\mod -> "import " ++ mod.moduleName) testModules

        possiblyTestsList : List String
        possiblyTestsList =
            List.map makeModuleTuple testModules
    in
    seedIO
        |> TE.fmap
            (\seedValue ->
                """module Test.Generated.Main exposing (main)

""" ++ String.join "\n" imports ++ """

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = """ ++ String.fromInt (Maybe.withDefault 100 maybeRuns) ++ """
        , report = """ ++ generateElmReportVariant report ++ """
        , seed = """ ++ String.fromInt seedValue ++ """
        , processes = 1
        , globs =
            """ ++ indentAllButFirstLine 12 (List.map (Encode.encode 0 << Encode.string) testFileGlobs) ++ """
        , paths =
            """ ++ indentAllButFirstLine 12 (List.map (Encode.encode 0 << Encode.string) testFilePaths) ++ """
        }
        """ ++ indentAllButFirstLine 8 possiblyTestsList
            )


indentAllButFirstLine : Int -> List String -> String
indentAllButFirstLine indent list =
    case list of
        [] ->
            "[]"

        head :: rest ->
            "[ "
                ++ head
                ++ String.concat (List.map (\entry -> "\n" ++ String.repeat indent " " ++ ", " ++ entry) rest)
                ++ "\n"
                ++ String.repeat indent " "
                ++ "]"


makeModuleTuple : { moduleName : String, possiblyTests : List String } -> String
makeModuleTuple mod =
    let
        list : List String
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


generateElmReportVariant : Maybe Report -> String
generateElmReportVariant maybeReport =
    case maybeReport of
        Just Json ->
            "JsonReport"

        Just JUnit ->
            "JUnitReport"

        _ ->
            "ConsoleReport UseColor"



-- GET INFORMATION


style : Reporting.Style
style =
    Reporting.silent


extractExposedPossiblyTests : String -> Task Never (Maybe ( String, List String ))
extractExposedPossiblyTests path =
    File.readUtf8 path
        |> TE.bind
            (\bytes ->
                case Parse.fromByteString (SV.fileSyntaxVersion path) Parse.Application bytes of
                    Ok (Src.Module _ (Just (A.At _ name)) (A.At _ exposing_) _ _ _ _ _ _ _) ->
                        let
                            exposed : List Name
                            exposed =
                                case exposing_ of
                                    Src.Open ->
                                        []

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
                        TE.pure (Just ( name, exposed ))

                    _ ->
                        TE.pure Nothing
            )



-- COMMAND LINE


type FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


stat : FilePath -> Task Never FileType
stat path =
    Utils.dirDoesFileExist path
        |> TE.bind
            (\isFile ->
                Utils.dirDoesDirectoryExist path
                    |> TE.fmap
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


resolveFile : FilePath -> Task Never (Result Error (List FilePath))
resolveFile path =
    stat path
        |> TE.bind
            (\fileType ->
                case fileType of
                    IsFile ->
                        TE.pure (Ok [ path ])

                    IsDirectory ->
                        findAllGuidaAndElmFiles path
                            |> TE.fmap
                                (\elmFiles ->
                                    case elmFiles of
                                        [] ->
                                            Err (NoElmFiles path)

                                        _ ->
                                            Ok elmFiles
                                )

                    DoesNotExist ->
                        TE.pure (Err (FileDoesNotExist path))
            )


resolveElmFiles : List FilePath -> Task Never (Result (List Error) (List FilePath))
resolveElmFiles inputFiles =
    TE.mapM resolveFile inputFiles
        |> TE.fmap collectErrors
        |> TE.fmap
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


collectFiles : (a -> Task Never (List a)) -> a -> Task Never (List a)
collectFiles children root =
    children root
        |> TE.bind (\xs -> TE.mapM (collectFiles children) xs)
        |> TE.fmap (\subChildren -> root :: List.concat subChildren)


listDir : FilePath -> Task Never (List FilePath)
listDir path =
    Utils.dirListDirectory path
        |> TE.fmap (List.map (\file -> path ++ "/" ++ file))


fileList : FilePath -> Task Never (List FilePath)
fileList =
    let
        children : FilePath -> Task Never (List FilePath)
        children path =
            if isSkippable path then
                TE.pure []

            else
                Utils.dirDoesDirectoryExist path
                    |> TE.bind
                        (\directory ->
                            if directory then
                                listDir path

                            else
                                TE.pure []
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


findAllGuidaAndElmFiles : FilePath -> Task Never (List FilePath)
findAllGuidaAndElmFiles inputFile =
    fileList inputFile
        |> TE.fmap (List.filter (\path -> hasExtension ".guida" path || hasExtension ".elm" path))


hasFilename : String -> FilePath -> Bool
hasFilename name path =
    name == Utils.fpTakeFileName path


{-| FROM INSTALL
-}



-- ATTEMPT CHANGES


attemptChanges : FilePath -> Solver.Env -> Outline.AppOutline -> Task Exit.Test ()
attemptChanges root env appOutline =
    TE.eio Exit.TestBadDetails <|
        BW.withScope
            (\scope ->
                let
                    newOutline : Outline.Outline
                    newOutline =
                        Outline.App appOutline
                in
                Outline.write (Stuff.testDir root) newOutline
                    |> TE.bind (\_ -> Details.verifyInstall scope root env newOutline)
            )



-- MAKE APP PLAN


makeAppPlan : Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task Exit.Test Outline.AppOutline
makeAppPlan (Solver.Env cache _ connection registry) pkg ((Outline.AppOutline elmVersion sourceDirs direct indirect testDirect testIndirect) as outline) =
    if Dict.member identity pkg direct then
        TE.pure outline

    else
        -- is it already indirect?
        case Dict.get identity pkg indirect of
            Just vsn ->
                TE.pure <|
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
                        TE.pure <|
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
                                TE.pure <|
                                    Outline.AppOutline elmVersion
                                        sourceDirs
                                        (Dict.insert identity pkg vsn direct)
                                        indirect
                                        testDirect
                                        (Dict.remove identity pkg testIndirect)

                            Nothing ->
                                -- finally try to add it from scratch
                                case Registry.getVersions_ pkg registry of
                                    Err suggestions ->
                                        case connection of
                                            Solver.Online _ ->
                                                TE.throw (Exit.TestUnknownPackageOnline pkg suggestions)

                                            Solver.Offline ->
                                                TE.throw (Exit.TestUnknownPackageOffline pkg suggestions)

                                    Ok _ ->
                                        TE.io (Solver.addToApp cache connection registry pkg outline False)
                                            |> TE.bind
                                                (\result ->
                                                    case result of
                                                        Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                                            TE.pure app

                                                        Solver.NoSolution ->
                                                            TE.throw (Exit.TestNoOnlineAppSolution pkg)

                                                        Solver.NoOfflineSolution ->
                                                            TE.throw (Exit.TestNoOfflineAppSolution pkg)

                                                        Solver.SolverErr exit ->
                                                            TE.throw (Exit.TestHadSolverTrouble exit)
                                                )



-- MAKE PACKAGE PLAN


makePkgPlan : Solver.Env -> Dict ( String, String ) Pkg.Name C.Constraint -> Outline.AppOutline -> Task Exit.Test Outline.AppOutline
makePkgPlan env cons outline =
    makePkgPlanHelp env (Dict.toList Pkg.compareName cons) outline


makePkgPlanHelp : Solver.Env -> List ( Pkg.Name, C.Constraint ) -> Outline.AppOutline -> Task Exit.Test Outline.AppOutline
makePkgPlanHelp ((Solver.Env cache _ connection registry) as env) cons outline =
    case cons of
        [] ->
            TE.pure outline

        ( pkg, con ) :: remainingCons ->
            TE.io (Solver.addToTestApp cache connection registry pkg con outline)
                |> TE.bind
                    (\result ->
                        case result of
                            Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                makePkgPlanHelp env remainingCons app

                            Solver.NoSolution ->
                                TE.throw (Exit.TestNoOnlinePkgSolution pkg)

                            Solver.NoOfflineSolution ->
                                TE.throw (Exit.TestNoOfflinePkgSolution pkg)

                            Solver.SolverErr exit ->
                                TE.throw (Exit.TestHadSolverTrouble exit)
                    )



-- GET INTERPRETER


getInterpreter : Task Never FilePath
getInterpreter =
    getInterpreterHelp "node` or `nodejs" <|
        (Utils.dirFindExecutable "node"
            |> TE.bind
                (\exe1 ->
                    Utils.dirFindExecutable "nodejs"
                        |> TE.fmap (\exe2 -> Maybe.or exe1 exe2)
                )
        )


getInterpreterHelp : String -> Task Never (Maybe FilePath) -> Task Never FilePath
getInterpreterHelp name findExe =
    findExe
        |> TE.bind
            (\maybePath ->
                case maybePath of
                    Just path ->
                        TE.pure path

                    Nothing ->
                        IO.hPutStrLn IO.stderr (exeNotFound name)
                            |> TE.bind (\_ -> Exit.exitFailure)
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
runMake : String -> String -> Task Never (Result Exit.Test String)
runMake root path =
    BW.withScope
        (\scope ->
            TE.toResult <|
                (TE.eio Exit.TestBadDetails (Details.load style scope root)
                    |> TE.bind
                        (\details ->
                            buildPaths root details (NE.Nonempty path [])
                                |> TE.bind
                                    (\artifacts ->
                                        toBuilder 0 root details artifacts
                                    )
                        )
                )
        )


buildPaths : FilePath -> Details.Details -> NE.Nonempty FilePath -> Task Exit.Test Build.Artifacts
buildPaths root details paths =
    TE.eio Exit.TestCannotBuild <|
        Build.fromPaths style root details paths



-- TO BUILDER


toBuilder : Int -> FilePath -> Details.Details -> Build.Artifacts -> Task Exit.Test String
toBuilder leadingLines root details artifacts =
    TE.mapError Exit.TestBadGenerate <|
        Generate.dev False leadingLines root details artifacts



-- PARSERS


type Report
    = Json
    | JUnit
    | Console


format : Parser
format =
    Parser
        { singular = "format"
        , plural = "formats"
        , suggest = \_ -> TE.pure []
        , examples = \_ -> TE.pure [ "json", "junit", "console" ]
        }


parseReport : String -> Maybe Report
parseReport report =
    case report of
        "json" ->
            Just Json

        "junit" ->
            Just JUnit

        "console" ->
            Just Console

        _ ->
            Nothing
