module Terminal.Make exposing
    ( Flags(..)
    , Output(..)
    , ReportType(..)
    , docsFile
    , output
    , parseDocsFile
    , parseOutput
    , parseReportType
    , reportType
    , run
    )

{-| Top-level `guida make` command driver.

This module parses CLI flags, chooses reporting styles, invokes the
builder for either package-exposed modules or explicit file paths, and
then converts build artifacts into generated output (JavaScript,
HTML, or documentation). It contains helpers for handling warnings,
report rendering, and writing generated files to disk.

-}

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.File as File
import Builder.Generate as Generate
import Builder.Guida.Details as Details
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help as Help
import Builder.Stuff as Stuff
import Compiler.AST.Optimized as Opt
import Compiler.Data.NonEmptyList as NE
import Compiler.Generate.Html as Html
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Warning as W
import Maybe.Extra as Maybe
import System.Exit as Exit
import Task exposing (Task)
import Terminal.Terminal.Internal exposing (Parser(..))
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- FLAGS


{-| Whether compiler warnings are enabled and whether they are treated as
errors. Use `NoWarnings` to suppress all warnings. `Warnings Bool` turns
warnings on; the `Bool` indicates whether warnings should be considered
fatal (treated as errors) when `True`.
-}
type Warnings
    = NoWarnings
    | Warnings Bool


{-| Encodes the raw set of flags parsed from the CLI for `guida make`.
The fields correspond to: `debug`, `optimize`, `sourcemaps`,
`noWarnings`, `denyWarnings`, an optional `Output`, an optional
`ReportType`, and an optional docs file path.
-}
type Flags
    = Flags Bool Bool Bool Bool Bool (Maybe Output) (Maybe ReportType) (Maybe String)


{-| Where the user asked the compiler to write its output.

  - `JS String` writes a JavaScript bundle to the given path.
  - `Html String` writes an HTML file to the given path.
  - `DevNull` discards output (e.g. `/dev/null`).

-}
type Output
    = JS String
    | Html String
    | DevNull


{-| The format to emit error and build reports in. `Json` produces
machine-readable output suitable for editor integration. Human-oriented
terminal output is the default when no report type is specified.
-}
type ReportType
    = Json



-- RUN


{-| Run the `make` command.

     This function is the top-level entry point when `guida make` is invoked.
     It performs the following steps:

     1. Resolve the desired `Reporting.Style` based on the `--report` flag.
     2. Locate the project root with `Stuff.findRoot`. If no root is found,
         the function returns a terminal-friendly `MakeNoOutline` error.
     3. When a root is found, call `runHelp` inside `Reporting.attemptWithStyle`
         to ensure errors are rendered with the selected style.
     4. `runHelp` acquires a project-level lock, loads project details and then
         delegates to either `buildExposed` (package builds) or `buildPaths`
         (file-based builds). After the build, generation and file writes may
         occur (writing JS/HTML/docs) depending on flags such as `--output`
         and `--docs`. Warnings and report styles are honored throughout.

     The returned `Task` encapsulates all I/O and error handling; it never
     throws exceptions directly but instead returns structured `Exit.Make`
     results via `Reporting.attemptWithStyle`.

-}
run : List String -> Flags -> Task Never ()
run paths ((Flags _ _ _ _ _ _ report _) as flags) =
    getStyle report
        |> Task.andThen
            (\style ->
                Stuff.findRoot
                    |> Task.andThen
                        (\maybeRoot ->
                            Reporting.attemptWithStyle style Exit.makeToReport <|
                                case maybeRoot of
                                    Just root ->
                                        runHelp root paths style flags

                                    Nothing ->
                                        Task.succeed (Err Exit.MakeNoOutline)
                        )
            )


{-| Run the make workflow once the project root is known.
It acquires a build lock, converts CLI flags into compiler settings,
then either builds all exposed modules or the specific input paths.
The result is a build report or a terminal-friendly failure code.
-}
runHelp : Stuff.Root -> List String -> Reporting.Style -> Flags -> Task Never (Result Exit.Make ())
runHelp root paths style (Flags debug optimize withSourceMaps noWarnings denyWarnings maybeOutput _ maybeDocs) =
    BW.withScope
        (\scope ->
            Stuff.withRootLock (Stuff.rootPath root) <|
                Task.run <|
                    (getMode debug optimize
                        |> Task.andThen
                            (\desiredMode ->
                                getWarnings noWarnings denyWarnings
                                    |> Task.andThen
                                        (\warnings ->
                                            Task.eio Exit.MakeBadDetails (Details.load style scope root)
                                                |> Task.andThen
                                                    (\details ->
                                                        case paths of
                                                            [] ->
                                                                getExposed root details
                                                                    |> Task.andThen (\exposed -> buildExposed style root details maybeDocs exposed)

                                                            p :: ps ->
                                                                buildPaths style root details warnings (NE.Nonempty p ps)
                                                                    |> Task.andThen
                                                                        (\artifacts ->
                                                                            Task.io (reportWarnings warnings root artifacts)
                                                                                |> Task.andThen
                                                                                    (\_ ->
                                                                                        case maybeOutput of
                                                                                            Nothing ->
                                                                                                case getMains artifacts of
                                                                                                    [] ->
                                                                                                        Task.succeed ()

                                                                                                    [ name ] ->
                                                                                                        toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                                                                            |> Task.andThen
                                                                                                                (\builder ->
                                                                                                                    generate style "index.html" (Html.sandwich (Stuff.rootToTarget root) name builder) (NE.Nonempty name [])
                                                                                                                )

                                                                                                    name :: names ->
                                                                                                        toBuilder withSourceMaps 0 root details desiredMode artifacts
                                                                                                            |> Task.andThen
                                                                                                                (\builder ->
                                                                                                                    generate style "guida.js" builder (NE.Nonempty name names)
                                                                                                                )

                                                                                            Just DevNull ->
                                                                                                Task.succeed ()

                                                                                            Just (JS target) ->
                                                                                                case getNoMains artifacts of
                                                                                                    [] ->
                                                                                                        toBuilder withSourceMaps 0 root details desiredMode artifacts
                                                                                                            |> Task.andThen
                                                                                                                (\builder ->
                                                                                                                    generate style target builder (Build.getRootNames artifacts)
                                                                                                                )

                                                                                                    name :: names ->
                                                                                                        Task.fail (Exit.MakeNonMainFilesIntoJavaScript name names)

                                                                                            Just (Html target) ->
                                                                                                hasOneMain artifacts
                                                                                                    |> Task.andThen
                                                                                                        (\name ->
                                                                                                            toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                                                                                |> Task.andThen
                                                                                                                    (\builder ->
                                                                                                                        generate style target (Html.sandwich (Stuff.rootToTarget root) name builder) (NE.Nonempty name [])
                                                                                                                    )
                                                                                                        )
                                                                                    )
                                                                        )
                                                    )
                                        )
                            )
                    )
        )



-- GET INFORMATION


{-| Choose the reporting style for compiler output.
If the user requests JSON reports, the build uses machine-readable reporting.
Otherwise it defaults to the terminal-friendly style.
-}
getStyle : Maybe ReportType -> Task Never Reporting.Style
getStyle report =
    case report of
        Nothing ->
            Reporting.terminal

        Just Json ->
            Task.succeed Reporting.json


{-| Convert debug/optimize flags into a concrete generation mode.
These options are mutually exclusive: the command either builds in debug
mode, optimized production mode, or a plain development mode.
-}
getMode : Bool -> Bool -> Task Exit.Make DesiredMode
getMode debug optimize =
    case ( debug, optimize ) of
        ( True, True ) ->
            Task.fail Exit.MakeCannotOptimizeAndDebug

        ( True, False ) ->
            Task.succeed Debug

        ( False, False ) ->
            Task.succeed Dev

        ( False, True ) ->
            Task.succeed Prod


{-| Convert warning suppression and denial flags into a warning policy.
This determines whether warnings are printed and whether they fail the build.
-}
getWarnings : Bool -> Bool -> Task Exit.Make Warnings
getWarnings noWarnings denyWarnings =
    case ( noWarnings, denyWarnings ) of
        ( True, True ) ->
            Task.fail Exit.MakeCannotSuppressAndDenyWarnings

        ( True, False ) ->
            Task.succeed NoWarnings

        ( False, False ) ->
            Task.succeed (Warnings False)

        ( False, True ) ->
            Task.succeed (Warnings True)


{-| Determine the list of exposed modules for a package build.
When no file paths are provided, `guida make` builds the exposed modules
from the package outline. Applications must specify file names instead.
-}
getExposed : Stuff.Root -> Details.Details -> Task Exit.Make (NE.Nonempty ModuleName.Raw)
getExposed root (Details.Details _ validOutline _ _ _ _) =
    case validOutline of
        Details.ValidApp _ ->
            Task.fail Exit.MakeAppNeedsFileNames

        Details.ValidPkg _ exposed _ ->
            case exposed of
                [] ->
                    Task.fail
                        (case root of
                            Stuff.GuidaRoot _ ->
                                Exit.MakeGuidaPkgNeedsExposing

                            Stuff.ElmRoot _ _ ->
                                Exit.MakeElmPkgNeedsExposing
                        )

                m :: ms ->
                    Task.succeed (NE.Nonempty m ms)



-- BUILD PROJECTS


{-| Build the exposed modules of a package.
This is used when `guida make` is run without explicit file arguments.
`maybeDocs` can also enable generation of package documentation JSON.
-}
buildExposed : Reporting.Style -> Stuff.Root -> Details.Details -> Maybe FilePath -> NE.Nonempty ModuleName.Raw -> Task Exit.Make ()
buildExposed style root details maybeDocs exposed =
    let
        docsGoal : Build.DocsGoal ()
        docsGoal =
            Maybe.unwrap Build.ignoreDocs Build.writeDocs maybeDocs
    in
    Task.eio Exit.MakeCannotBuild <|
        Build.fromExposed BD.unit
            BE.unit
            style
            root
            details
            docsGoal
            exposed


{-| Build the specific file paths provided by the user.
This produces compilation artifacts that can later be rendered to JS/HTML.
Warnings are handled according to the current warning policy.
-}
buildPaths : Reporting.Style -> Stuff.Root -> Details.Details -> Warnings -> NE.Nonempty FilePath -> Task Exit.Make Build.Artifacts
buildPaths style root details warnings paths =
    let
        ( suppressWarnings, denyWarnings ) =
            case warnings of
                NoWarnings ->
                    ( True, False )

                Warnings deny ->
                    ( False, deny )
    in
    Task.eio Exit.MakeCannotBuild <|
        Build.fromPaths style root details suppressWarnings denyWarnings paths



-- GET MAINS


{-| Extract the main modules from the built artifacts.
These are used to determine whether the build can generate HTML or JS output.
-}
getMains : Build.Artifacts -> List ModuleName.Raw
getMains (Build.Artifacts _ _ _ roots modules) =
    List.filterMap (getMain modules) (NE.toList roots)


{-| For a given list of built modules and a root description, return the
module name when that root declares or contains a `main` function.
This is used to select entry points for generated HTML or JS bundles.
-}
getMain : List Build.Module -> Build.Root -> Maybe ModuleName.Raw
getMain modules root =
    case root of
        Build.Inside name ->
            if List.any (isMain name) modules then
                Just name

            else
                Nothing

        Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
            maybeMain
                |> Maybe.map (\_ -> name)


{-| Check whether a given `Build.Module` provides a `main` definition
and matches the requested target module name.
-}
isMain : ModuleName.Raw -> Build.Module -> Bool
isMain targetName modul =
    case modul of
        Build.Fresh name _ (Opt.LocalGraph maybeMain _ _) ->
            Maybe.isJust maybeMain && name == targetName

        Build.Cached name mainIsDefined _ ->
            mainIsDefined && name == targetName



-- HAS ONE MAIN


{-| Ensure the build targets exactly one main module for HTML output.
If the user requested HTML output but multiple roots were provided, this fails.
-}
hasOneMain : Build.Artifacts -> Task Exit.Make ModuleName.Raw
hasOneMain (Build.Artifacts _ _ _ roots modules) =
    case roots of
        NE.Nonempty root [] ->
            Task.mio Exit.MakeNoMain (Task.succeed <| getMain modules root)

        NE.Nonempty _ (_ :: _) ->
            Task.fail Exit.MakeMultipleFilesIntoHtml



-- GET MAINLESS


{-| For a completed build, return the list of root modules that do not
have a `main` value. These roots are candidates for "mainless" errors
when the user requested output that requires a main (e.g. generating
HTML without specifying a main).
-}
getNoMains : Build.Artifacts -> List ModuleName.Raw
getNoMains (Build.Artifacts _ _ _ roots modules) =
    List.filterMap (getNoMain modules) (NE.toList roots)


{-| Determine whether a single root is mainless.
For `Inside` roots this checks in-project modules. For `Outside`
explicit roots it inspects the compiled object graph for a recorded
main entry.
-}
getNoMain : List Build.Module -> Build.Root -> Maybe ModuleName.Raw
getNoMain modules root =
    case root of
        Build.Inside name ->
            if List.any (isMain name) modules then
                Nothing

            else
                Just name

        Build.Outside name _ (Opt.LocalGraph maybeMain _ _) ->
            case maybeMain of
                Just _ ->
                    Nothing

                Nothing ->
                    Just name



-- WARNINGS


{-| Print warnings after a build if warnings are enabled.
When warnings are denied, this may also fail the process after reporting them.
-}
reportWarnings : Warnings -> Stuff.Root -> Build.Artifacts -> Task Never ()
reportWarnings warnings root (Build.Artifacts warnList _ _ _ _) =
    case warnings of
        NoWarnings ->
            Task.succeed ()

        Warnings denyWarnings ->
            if List.isEmpty warnList then
                Task.succeed ()

            else
                let
                    rootPath : FilePath
                    rootPath =
                        Stuff.rootPath root

                    target : Target
                    target =
                        Stuff.rootToTarget root
                in
                Utils.listTraverse (warningToDoc target rootPath) warnList
                    |> Task.mapError never
                    |> Task.andThen
                        (\docs ->
                            Task.io (Help.toStderr (D.vcat (docs ++ [ D.fromChars "" ])))
                                |> Task.andThen
                                    (\_ ->
                                        if denyWarnings then
                                            Exit.exitFailure

                                        else
                                            Task.succeed ()
                                    )
                        )


{-| Convert a single module warning list into a renderable document.
This maps compiler warnings into `Report.Report` values and then
formats them into a single `D.Doc` suitable for writing to stderr.
-}
warningToDoc : Target -> FilePath -> W.Module -> Task Never D.Doc
warningToDoc target rootPath { absolutePath, source, warnings } =
    let
        reports : List Report.Report
        reports =
            List.map (W.toReport target L.empty (Code.toSource source)) warnings
    in
    Task.succeed (D.vcat (List.map (reportToDoc rootPath absolutePath) reports))


{-| Render a single `Report.Report` into a document with a header and
the report body. The `rootPath` is used to relativize the file path in
the message bar for readability.
-}
reportToDoc : FilePath -> FilePath -> Report.Report -> D.Doc
reportToDoc rootPath absolutePath (Report.Report title _ _ message) =
    D.vcat
        [ toMessageBar title (Utils.fpMakeRelative rootPath absolutePath)
        , D.fromChars ""
        , message
        , D.fromChars ""
        ]


{-| Create a compact header line for a titled message.
The bar includes the title and a truncated file path aligned to a
fixed width for consistent terminal appearance.
-}
toMessageBar : String -> String -> D.Doc
toMessageBar title filePath =
    let
        usedSpace : Int
        usedSpace =
            4 + String.length title + 1 + String.length filePath
    in
    D.yellow <|
        D.fromChars <|
            "-- "
                ++ title
                ++ " "
                ++ String.repeat (max 1 (80 - usedSpace)) "-"
                ++ " "
                ++ filePath



-- GENERATE


{-| Write a generated bundle or HTML file to disk and report success.
The builder string is the output text and `names` describes the built modules.
-}
generate : Reporting.Style -> FilePath -> String -> NE.Nonempty ModuleName.Raw -> Task Exit.Make ()
generate style target builder names =
    Task.io
        (Utils.dirCreateDirectoryIfMissing True (Utils.fpTakeDirectory target)
            |> Task.andThen (\_ -> File.writeUtf8 target builder)
            |> Task.andThen (\_ -> Reporting.reportGenerate style names target)
        )



-- TO BUILDER


{-| Generation mode selected from CLI flags.

`Debug` produces instrumented output for debugging, `Dev` is the
default developer-friendly output, and `Prod` enables optimizations for
production bundles.

-}
type DesiredMode
    = Debug
    | Dev
    | Prod


{-| Convert build artifacts into a final builder string.
This chooses between debug, development, or production generation modes.
-}
toBuilder : Bool -> Int -> Stuff.Root -> Details.Details -> DesiredMode -> Build.Artifacts -> Task Exit.Make String
toBuilder withSourceMaps leadingLines root details desiredMode artifacts =
    Task.mapError Exit.MakeBadGenerate <|
        case desiredMode of
            Debug ->
                Generate.debug withSourceMaps leadingLines root details artifacts

            Dev ->
                Generate.dev withSourceMaps leadingLines root details artifacts

            Prod ->
                Generate.prod withSourceMaps leadingLines root details artifacts



-- PARSERS


{-| Command-line parser descriptor for the `--report` option. The
parser advertises allowed values and example usage shown in help text.
-}
reportType : Parser
reportType =
    Parser
        { singular = "report type"
        , plural = "report types"
        , suggest = \_ -> Task.succeed [ "json" ]
        , examples = \_ -> Task.succeed [ "json" ]
        }


{-| Parse a textual `ReportType` value from CLI input.
Currently accepts `json` to select machine-readable reporting.
-}
parseReportType : String -> Maybe ReportType
parseReportType string =
    if string == "json" then
        Just Json

    else
        Nothing


{-| Command-line parser descriptor for the `--output` option. It is
used to provide examples and help text for valid output targets.
-}
output : Parser
output =
    Parser
        { singular = "output file"
        , plural = "output files"
        , suggest = \_ -> Task.succeed []
        , examples = \_ -> Task.succeed [ "guida.js", "index.html", "/dev/null" ]
        }


{-| Parse an `Output` target from a user-supplied path. Supports HTML,
JavaScript, and special null devices to discard output.
-}
parseOutput : String -> Maybe Output
parseOutput name =
    if isDevNull name then
        Just DevNull

    else if hasExt ".html" name then
        Just (Html name)

    else if hasExt ".js" name then
        Just (JS name)

    else
        Nothing


docsFile : Parser
docsFile =
    Parser
        { singular = "json file"
        , plural = "json files"
        , suggest = \_ -> Task.succeed []
        , examples = \_ -> Task.succeed [ "docs.json", "documentation.json" ]
        }


{-| Command-line parser descriptor for the `--docs` option. Provides
help text and examples for specifying the documentation JSON file.
-}
parseDocsFile : String -> Maybe String
parseDocsFile name =
    if hasExt ".json" name then
        Just name

    else
        Nothing


{-| Check whether a path string ends with the given extension and is
longer than the extension itself. This avoids treating ".html" as a
valid file name.
-}
hasExt : String -> String -> Bool
hasExt ext path =
    Utils.fpTakeExtension path == ext && String.length path > String.length ext


{-| Recognize common platform-specific "null" file paths used to
discard output. This covers POSIX `/dev/null` and Windows `NUL`.
-}
isDevNull : String -> Bool
isDevNull name =
    name == "/dev/null" || name == "NUL" || name == "<|null"
