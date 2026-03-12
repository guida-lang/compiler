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


type Warnings
    = NoWarnings
    | Warnings Bool


type Flags
    = Flags Bool Bool Bool Bool Bool (Maybe Output) (Maybe ReportType) (Maybe String)


type Output
    = JS String
    | Html String
    | DevNull


type ReportType
    = Json



-- RUN


run : List String -> Flags -> Task Never ()
run paths ((Flags _ _ _ _ _ _ report _) as flags) =
    getStyle report
        |> Task.bind
            (\style ->
                Stuff.findRoot
                    |> Task.bind
                        (\maybeRoot ->
                            Reporting.attemptWithStyle style Exit.makeToReport <|
                                case maybeRoot of
                                    Just root ->
                                        runHelp root paths style flags

                                    Nothing ->
                                        Task.pure (Err Exit.MakeNoOutline)
                        )
            )


runHelp : Stuff.Root -> List String -> Reporting.Style -> Flags -> Task Never (Result Exit.Make ())
runHelp root paths style (Flags debug optimize withSourceMaps noWarnings denyWarnings maybeOutput _ maybeDocs) =
    BW.withScope
        (\scope ->
            Stuff.withRootLock (Stuff.rootPath root) <|
                Task.run <|
                    (getMode debug optimize
                        |> Task.bind
                            (\desiredMode ->
                                getWarnings noWarnings denyWarnings
                                    |> Task.bind
                                        (\warnings ->
                                            Task.eio Exit.MakeBadDetails (Details.load style scope root)
                                                |> Task.bind
                                                    (\details ->
                                                        case paths of
                                                            [] ->
                                                                getExposed root details
                                                                    |> Task.bind (\exposed -> buildExposed style root details maybeDocs exposed)

                                                            p :: ps ->
                                                                buildPaths style root details warnings (NE.Nonempty p ps)
                                                                    |> Task.bind
                                                                        (\artifacts ->
                                                                            Task.io (reportWarnings warnings root artifacts)
                                                                                |> Task.bind
                                                                                    (\_ ->
                                                                                        case maybeOutput of
                                                                                            Nothing ->
                                                                                                case getMains artifacts of
                                                                                                    [] ->
                                                                                                        Task.pure ()

                                                                                                    [ name ] ->
                                                                                                        toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                                                                            |> Task.bind
                                                                                                                (\builder ->
                                                                                                                    generate style "index.html" (Html.sandwich (Stuff.rootToTarget root) name builder) (NE.Nonempty name [])
                                                                                                                )

                                                                                                    name :: names ->
                                                                                                        toBuilder withSourceMaps 0 root details desiredMode artifacts
                                                                                                            |> Task.bind
                                                                                                                (\builder ->
                                                                                                                    generate style "guida.js" builder (NE.Nonempty name names)
                                                                                                                )

                                                                                            Just DevNull ->
                                                                                                Task.pure ()

                                                                                            Just (JS target) ->
                                                                                                case getNoMains artifacts of
                                                                                                    [] ->
                                                                                                        toBuilder withSourceMaps 0 root details desiredMode artifacts
                                                                                                            |> Task.bind
                                                                                                                (\builder ->
                                                                                                                    generate style target builder (Build.getRootNames artifacts)
                                                                                                                )

                                                                                                    name :: names ->
                                                                                                        Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)

                                                                                            Just (Html target) ->
                                                                                                hasOneMain artifacts
                                                                                                    |> Task.bind
                                                                                                        (\name ->
                                                                                                            toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                                                                                |> Task.bind
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


getStyle : Maybe ReportType -> Task Never Reporting.Style
getStyle report =
    case report of
        Nothing ->
            Reporting.terminal

        Just Json ->
            Task.pure Reporting.json


getMode : Bool -> Bool -> Task Exit.Make DesiredMode
getMode debug optimize =
    case ( debug, optimize ) of
        ( True, True ) ->
            Task.throw Exit.MakeCannotOptimizeAndDebug

        ( True, False ) ->
            Task.pure Debug

        ( False, False ) ->
            Task.pure Dev

        ( False, True ) ->
            Task.pure Prod


getWarnings : Bool -> Bool -> Task Exit.Make Warnings
getWarnings noWarnings denyWarnings =
    case ( noWarnings, denyWarnings ) of
        ( True, True ) ->
            Task.throw Exit.MakeCannotSuppressAndDenyWarnings

        ( True, False ) ->
            Task.pure NoWarnings

        ( False, False ) ->
            Task.pure (Warnings False)

        ( False, True ) ->
            Task.pure (Warnings True)


getExposed : Stuff.Root -> Details.Details -> Task Exit.Make (NE.Nonempty ModuleName.Raw)
getExposed root (Details.Details _ validOutline _ _ _ _) =
    case validOutline of
        Details.ValidApp _ ->
            Task.throw Exit.MakeAppNeedsFileNames

        Details.ValidPkg _ exposed _ ->
            case exposed of
                [] ->
                    Task.throw
                        (case root of
                            Stuff.GuidaRoot _ ->
                                Exit.MakeGuidaPkgNeedsExposing

                            Stuff.ElmRoot _ _ ->
                                Exit.MakeElmPkgNeedsExposing
                        )

                m :: ms ->
                    Task.pure (NE.Nonempty m ms)



-- BUILD PROJECTS


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


getMains : Build.Artifacts -> List ModuleName.Raw
getMains (Build.Artifacts _ _ _ roots modules) =
    List.filterMap (getMain modules) (NE.toList roots)


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


isMain : ModuleName.Raw -> Build.Module -> Bool
isMain targetName modul =
    case modul of
        Build.Fresh name _ (Opt.LocalGraph maybeMain _ _) ->
            Maybe.isJust maybeMain && name == targetName

        Build.Cached name mainIsDefined _ ->
            mainIsDefined && name == targetName



-- HAS ONE MAIN


hasOneMain : Build.Artifacts -> Task Exit.Make ModuleName.Raw
hasOneMain (Build.Artifacts _ _ _ roots modules) =
    case roots of
        NE.Nonempty root [] ->
            Task.mio Exit.MakeNoMain (Task.pure <| getMain modules root)

        NE.Nonempty _ (_ :: _) ->
            Task.throw Exit.MakeMultipleFilesIntoHtml



-- GET MAINLESS


getNoMains : Build.Artifacts -> List ModuleName.Raw
getNoMains (Build.Artifacts _ _ _ roots modules) =
    List.filterMap (getNoMain modules) (NE.toList roots)


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


reportWarnings : Warnings -> Stuff.Root -> Build.Artifacts -> Task Never ()
reportWarnings warnings root (Build.Artifacts warnList _ _ _ _) =
    case warnings of
        NoWarnings ->
            Task.pure ()

        Warnings denyWarnings ->
            if List.isEmpty warnList then
                Task.pure ()

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
                    |> Task.bind
                        (\docs ->
                            Task.io (Help.toStderr (D.vcat (docs ++ [ D.fromChars "" ])))
                                |> Task.bind
                                    (\_ ->
                                        if denyWarnings then
                                            Exit.exitFailure

                                        else
                                            Task.pure ()
                                    )
                        )


warningToDoc : Target -> FilePath -> W.Module -> Task Never D.Doc
warningToDoc target rootPath { absolutePath, source, warnings } =
    let
        reports : List Report.Report
        reports =
            List.map (W.toReport target L.empty (Code.toSource source)) warnings
    in
    Task.pure (D.vcat (List.map (reportToDoc rootPath absolutePath) reports))


reportToDoc : FilePath -> FilePath -> Report.Report -> D.Doc
reportToDoc rootPath absolutePath (Report.Report title _ _ message) =
    D.vcat
        [ toMessageBar title (Utils.fpMakeRelative rootPath absolutePath)
        , D.fromChars ""
        , message
        , D.fromChars ""
        ]


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


generate : Reporting.Style -> FilePath -> String -> NE.Nonempty ModuleName.Raw -> Task Exit.Make ()
generate style target builder names =
    Task.io
        (Utils.dirCreateDirectoryIfMissing True (Utils.fpTakeDirectory target)
            |> Task.bind (\_ -> File.writeUtf8 target builder)
            |> Task.bind (\_ -> Reporting.reportGenerate style names target)
        )



-- TO BUILDER


type DesiredMode
    = Debug
    | Dev
    | Prod


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


reportType : Parser
reportType =
    Parser
        { singular = "report type"
        , plural = "report types"
        , suggest = \_ -> Task.pure [ "json" ]
        , examples = \_ -> Task.pure [ "json" ]
        }


parseReportType : String -> Maybe ReportType
parseReportType string =
    if string == "json" then
        Just Json

    else
        Nothing


output : Parser
output =
    Parser
        { singular = "output file"
        , plural = "output files"
        , suggest = \_ -> Task.pure []
        , examples = \_ -> Task.pure [ "guida.js", "index.html", "/dev/null" ]
        }


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
        , suggest = \_ -> Task.pure []
        , examples = \_ -> Task.pure [ "docs.json", "documentation.json" ]
        }


parseDocsFile : String -> Maybe String
parseDocsFile name =
    if hasExt ".json" name then
        Just name

    else
        Nothing


hasExt : String -> String -> Bool
hasExt ext path =
    Utils.fpTakeExtension path == ext && String.length path > String.length ext


isDevNull : String -> Bool
isDevNull name =
    name == "/dev/null" || name == "NUL" || name == "<|null"
