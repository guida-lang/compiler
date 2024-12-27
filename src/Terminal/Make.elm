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
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.Data.NonEmptyList as NE
import Compiler.Generate.Html as Html
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import System.IO as IO
import Terminal.Terminal.Internal exposing (Parser(..))
import Types as T exposing (IO)
import Utils.Main as Utils



-- FLAGS


type Flags
    = Flags Bool Bool (Maybe Output) (Maybe ReportType) (Maybe String)


type Output
    = JS String
    | Html String
    | DevNull


type ReportType
    = Json



-- RUN


type alias Task a =
    Task.Task Exit.Make a


run : List String -> Flags -> IO ()
run paths ((Flags _ _ _ report _) as flags) =
    getStyle report
        |> IO.bind
            (\style ->
                Stuff.findRoot
                    |> IO.bind
                        (\maybeRoot ->
                            Reporting.attemptWithStyle style Exit.makeToReport <|
                                case maybeRoot of
                                    Just root ->
                                        runHelp root paths style flags

                                    Nothing ->
                                        IO.pure (Err Exit.MakeNoOutline)
                        )
            )


runHelp : String -> List String -> Reporting.Style -> Flags -> IO (Result Exit.Make ())
runHelp root paths style (Flags debug optimize maybeOutput _ maybeDocs) =
    BW.withScope
        (\scope ->
            Stuff.withRootLock root <|
                Task.run <|
                    (getMode debug optimize
                        |> Task.bind
                            (\desiredMode ->
                                Task.eio Exit.MakeBadDetails (Details.load style scope root)
                                    |> Task.bind
                                        (\details ->
                                            case paths of
                                                [] ->
                                                    getExposed details
                                                        |> Task.bind (\exposed -> buildExposed style root details maybeDocs exposed)

                                                p :: ps ->
                                                    buildPaths style root details (NE.Nonempty p ps)
                                                        |> Task.bind
                                                            (\artifacts ->
                                                                case maybeOutput of
                                                                    Nothing ->
                                                                        case getMains artifacts of
                                                                            [] ->
                                                                                Task.pure ()

                                                                            [ name ] ->
                                                                                toBuilder root details desiredMode artifacts
                                                                                    |> Task.bind
                                                                                        (\builder ->
                                                                                            generate style "index.html" (Html.sandwich name builder) (NE.Nonempty name [])
                                                                                        )

                                                                            name :: names ->
                                                                                toBuilder root details desiredMode artifacts
                                                                                    |> Task.bind
                                                                                        (\builder ->
                                                                                            generate style "elm.js" builder (NE.Nonempty name names)
                                                                                        )

                                                                    Just DevNull ->
                                                                        Task.pure ()

                                                                    Just (JS target) ->
                                                                        case getNoMains artifacts of
                                                                            [] ->
                                                                                toBuilder root details desiredMode artifacts
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
                                                                                    toBuilder root details desiredMode artifacts
                                                                                        |> Task.bind
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


getStyle : Maybe ReportType -> IO Reporting.Style
getStyle report =
    case report of
        Nothing ->
            Reporting.terminal

        Just Json ->
            IO.pure Reporting.json


getMode : Bool -> Bool -> Task DesiredMode
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


getExposed : Details.Details -> Task (NE.Nonempty T.CEMN_Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
    case validOutline of
        Details.ValidApp _ ->
            Task.throw Exit.MakeAppNeedsFileNames

        Details.ValidPkg _ exposed _ ->
            case exposed of
                [] ->
                    Task.throw Exit.MakePkgNeedsExposing

                m :: ms ->
                    Task.pure (NE.Nonempty m ms)



-- BUILD PROJECTS


buildExposed : Reporting.Style -> T.FilePath -> Details.Details -> Maybe T.FilePath -> NE.Nonempty T.CEMN_Raw -> Task ()
buildExposed style root details maybeDocs exposed =
    let
        docsGoal : Build.DocsGoal ()
        docsGoal =
            Maybe.unwrap Build.ignoreDocs Build.writeDocs maybeDocs
    in
    Task.eio Exit.MakeCannotBuild <|
        Build.fromExposed (Decode.succeed ()) (\_ -> Encode.object []) style root details docsGoal exposed


buildPaths : Reporting.Style -> T.FilePath -> Details.Details -> NE.Nonempty T.FilePath -> Task T.BB_Artifacts
buildPaths style root details paths =
    Task.eio Exit.MakeCannotBuild <|
        Build.fromPaths style root details paths



-- GET MAINS


getMains : T.BB_Artifacts -> List T.CEMN_Raw
getMains (T.BB_Artifacts _ _ roots modules) =
    List.filterMap (getMain modules) (NE.toList roots)


getMain : List T.BB_Module -> T.BB_Root -> Maybe T.CEMN_Raw
getMain modules root =
    case root of
        T.BB_Inside name ->
            if List.any (isMain name) modules then
                Just name

            else
                Nothing

        T.BB_Outside name _ (T.CASTO_LocalGraph maybeMain _ _) ->
            maybeMain
                |> Maybe.map (\_ -> name)


isMain : T.CEMN_Raw -> T.BB_Module -> Bool
isMain targetName modul =
    case modul of
        T.BB_Fresh name _ (T.CASTO_LocalGraph maybeMain _ _) ->
            Maybe.isJust maybeMain && name == targetName

        T.BB_Cached name mainIsDefined _ ->
            mainIsDefined && name == targetName



-- HAS ONE MAIN


hasOneMain : T.BB_Artifacts -> Task T.CEMN_Raw
hasOneMain (T.BB_Artifacts _ _ roots modules) =
    case roots of
        NE.Nonempty root [] ->
            Task.mio Exit.MakeNoMain (IO.pure <| getMain modules root)

        NE.Nonempty _ (_ :: _) ->
            Task.throw Exit.MakeMultipleFilesIntoHtml



-- GET MAINLESS


getNoMains : T.BB_Artifacts -> List T.CEMN_Raw
getNoMains (T.BB_Artifacts _ _ roots modules) =
    List.filterMap (getNoMain modules) (NE.toList roots)


getNoMain : List T.BB_Module -> T.BB_Root -> Maybe T.CEMN_Raw
getNoMain modules root =
    case root of
        T.BB_Inside name ->
            if List.any (isMain name) modules then
                Nothing

            else
                Just name

        T.BB_Outside name _ (T.CASTO_LocalGraph maybeMain _ _) ->
            case maybeMain of
                Just _ ->
                    Nothing

                Nothing ->
                    Just name



-- GENERATE


generate : Reporting.Style -> T.FilePath -> String -> NE.Nonempty T.CEMN_Raw -> Task ()
generate style target builder names =
    Task.io
        (Utils.dirCreateDirectoryIfMissing True (Utils.fpTakeDirectory target)
            |> IO.bind (\_ -> File.writeBuilder target builder)
            |> IO.bind (\_ -> Reporting.reportGenerate style names target)
        )



-- TO BUILDER


type DesiredMode
    = Debug
    | Dev
    | Prod


toBuilder : T.FilePath -> Details.Details -> DesiredMode -> T.BB_Artifacts -> Task String
toBuilder root details desiredMode artifacts =
    Task.mapError Exit.MakeBadGenerate <|
        case desiredMode of
            Debug ->
                Generate.debug root details artifacts

            Dev ->
                Generate.dev root details artifacts

            Prod ->
                Generate.prod root details artifacts



-- PARSERS


reportType : Parser
reportType =
    Parser
        { singular = "report type"
        , plural = "report types"
        , suggest = \_ -> IO.pure [ "json" ]
        , examples = \_ -> IO.pure [ "json" ]
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
        , suggest = \_ -> IO.pure []
        , examples = \_ -> IO.pure [ "elm.js", "index.html", "/dev/null" ]
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
        , suggest = \_ -> IO.pure []
        , examples = \_ -> IO.pure [ "docs.json", "documentation.json" ]
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
