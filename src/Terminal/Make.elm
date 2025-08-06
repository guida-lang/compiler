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
import Builder.Elm.Details as Details
import Builder.File as File
import Builder.Generate as Generate
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Utils.Task.Extra as TE
import Builder.Stuff as Stuff
import Compiler.AST.Optimized as Opt
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Generate.Html as Html
import Maybe.Extra as Maybe
import Utils.Task.Extra as TE
import Task exposing (Task)
import Terminal.Terminal.Internal exposing (Parser(..))
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Main as Utils exposing (FilePath)



-- FLAGS


type Flags
    = Flags Bool Bool Bool (Maybe Output) (Maybe ReportType) (Maybe String)


type Output
    = JS String
    | Html String
    | DevNull


type ReportType
    = Json



-- RUN


run : List String -> Flags -> Task Never ()
run paths ((Flags _ _ _ _ report _) as flags) =
    getStyle report
        |> TE.bind
            (\style ->
                Stuff.findRoot
                    |> TE.bind
                        (\maybeRoot ->
                            Reporting.attemptWithStyle style Exit.makeToReport <|
                                case maybeRoot of
                                    Just root ->
                                        runHelp root paths style flags

                                    Nothing ->
                                        TE.pure (Err Exit.MakeNoOutline)
                        )
            )


runHelp : String -> List String -> Reporting.Style -> Flags -> Task Never (Result Exit.Make ())
runHelp root paths style (Flags debug optimize withSourceMaps maybeOutput _ maybeDocs) =
    BW.withScope
        (\scope ->
            Stuff.withRootLock root <|
                TE.toResult <|
                    (getMode debug optimize
                        |> TE.bind
                            (\desiredMode ->
                                TE.eio Exit.MakeBadDetails (Details.load style scope root)
                                    |> TE.bind
                                        (\details ->
                                            case paths of
                                                [] ->
                                                    getExposed details
                                                        |> TE.bind (\exposed -> buildExposed style root details maybeDocs exposed)

                                                p :: ps ->
                                                    buildPaths style root details (NE.Nonempty p ps)
                                                        |> TE.bind
                                                            (\artifacts ->
                                                                case maybeOutput of
                                                                    Nothing ->
                                                                        case getMains artifacts of
                                                                            [] ->
                                                                                TE.pure ()

                                                                            [ name ] ->
                                                                                toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                                                    |> TE.bind
                                                                                        (\builder ->
                                                                                            generate style "index.html" (Html.sandwich name builder) (NE.Nonempty name [])
                                                                                        )

                                                                            name :: names ->
                                                                                toBuilder withSourceMaps 0 root details desiredMode artifacts
                                                                                    |> TE.bind
                                                                                        (\builder ->
                                                                                            generate style "elm.js" builder (NE.Nonempty name names)
                                                                                        )

                                                                    Just DevNull ->
                                                                        TE.pure ()

                                                                    Just (JS target) ->
                                                                        case getNoMains artifacts of
                                                                            [] ->
                                                                                toBuilder withSourceMaps 0 root details desiredMode artifacts
                                                                                    |> TE.bind
                                                                                        (\builder ->
                                                                                            generate style target builder (Build.getRootNames artifacts)
                                                                                        )

                                                                            name :: names ->
                                                                                TE.throw (Exit.MakeNonMainFilesIntoJavaScript name names)

                                                                    Just (Html target) ->
                                                                        hasOneMain artifacts
                                                                            |> TE.bind
                                                                                (\name ->
                                                                                    toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                                                        |> TE.bind
                                                                                            (\builder ->
                                                                                                generate style target (Html.sandwich name builder) (NE.Nonempty name [])
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
            TE.pure Reporting.json


getMode : Bool -> Bool -> Task Exit.Make DesiredMode
getMode debug optimize =
    case ( debug, optimize ) of
        ( True, True ) ->
            TE.throw Exit.MakeCannotOptimizeAndDebug

        ( True, False ) ->
            TE.pure Debug

        ( False, False ) ->
            TE.pure Dev

        ( False, True ) ->
            TE.pure Prod


getExposed : Details.Details -> Task Exit.Make (NE.Nonempty ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
    case validOutline of
        Details.ValidApp _ ->
            TE.throw Exit.MakeAppNeedsFileNames

        Details.ValidPkg _ exposed _ ->
            case exposed of
                [] ->
                    TE.throw Exit.MakePkgNeedsExposing

                m :: ms ->
                    TE.pure (NE.Nonempty m ms)



-- BUILD PROJECTS


buildExposed : Reporting.Style -> FilePath -> Details.Details -> Maybe FilePath -> NE.Nonempty ModuleName.Raw -> Task Exit.Make ()
buildExposed style root details maybeDocs exposed =
    let
        docsGoal : Build.DocsGoal ()
        docsGoal =
            Maybe.unwrap Build.ignoreDocs Build.writeDocs maybeDocs
    in
    TE.eio Exit.MakeCannotBuild <|
        Build.fromExposed BD.unit
            BE.unit
            style
            root
            details
            docsGoal
            exposed


buildPaths : Reporting.Style -> FilePath -> Details.Details -> NE.Nonempty FilePath -> Task Exit.Make Build.Artifacts
buildPaths style root details paths =
    TE.eio Exit.MakeCannotBuild <|
        Build.fromPaths style root details paths



-- GET MAINS


getMains : Build.Artifacts -> List ModuleName.Raw
getMains (Build.Artifacts _ _ roots modules) =
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
hasOneMain (Build.Artifacts _ _ roots modules) =
    case roots of
        NE.Nonempty root [] ->
            TE.mio Exit.MakeNoMain (TE.pure <| getMain modules root)

        NE.Nonempty _ (_ :: _) ->
            TE.throw Exit.MakeMultipleFilesIntoHtml



-- GET MAINLESS


getNoMains : Build.Artifacts -> List ModuleName.Raw
getNoMains (Build.Artifacts _ _ roots modules) =
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



-- GENERATE


generate : Reporting.Style -> FilePath -> String -> NE.Nonempty ModuleName.Raw -> Task Exit.Make ()
generate style target builder names =
    TE.io
        (Utils.dirCreateDirectoryIfMissing True (Utils.fpTakeDirectory target)
            |> TE.bind (\_ -> File.writeUtf8 target builder)
            |> TE.bind (\_ -> Reporting.reportGenerate style names target)
        )



-- TO BUILDER


type DesiredMode
    = Debug
    | Dev
    | Prod


toBuilder : Bool -> Int -> FilePath -> Details.Details -> DesiredMode -> Build.Artifacts -> Task Exit.Make String
toBuilder withSourceMaps leadingLines root details desiredMode artifacts =
    TE.mapError Exit.MakeBadGenerate <|
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
        , suggest = \_ -> TE.pure [ "json" ]
        , examples = \_ -> TE.pure [ "json" ]
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
        , suggest = \_ -> TE.pure []
        , examples = \_ -> TE.pure [ "elm.js", "index.html", "/dev/null" ]
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
        , suggest = \_ -> TE.pure []
        , examples = \_ -> TE.pure [ "docs.json", "documentation.json" ]
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
