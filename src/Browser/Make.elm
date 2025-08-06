module Browser.Make exposing
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
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath)



-- FLAGS


type Flags
    = Flags Bool Bool Bool


type Output
    = JS String
    | Html String
    | DevNull


type ReportType
    = Json



-- RUN


run : String -> Flags -> Task Never (Result Exit.Make String)
run path flags =
    Stuff.findRoot
        |> TE.bind
            (\maybeRoot ->
                case maybeRoot of
                    Just root ->
                        runHelp root path flags

                    Nothing ->
                        TE.pure (Err Exit.MakeNoOutline)
            )


runHelp : String -> String -> Flags -> Task Never (Result Exit.Make String)
runHelp root path (Flags debug optimize withSourceMaps) =
    BW.withScope
        (\scope ->
            Stuff.withRootLock root <|
                TE.toResult <|
                    (getMode debug optimize
                        |> TE.bind
                            (\desiredMode ->
                                let
                                    style : Reporting.Style
                                    style =
                                        Reporting.json
                                in
                                TE.eio Exit.MakeBadDetails (Details.load style scope root)
                                    |> TE.bind
                                        (\details ->
                                            buildPaths style root details (NE.Nonempty path [])
                                                |> TE.bind
                                                    (\artifacts ->
                                                        case getMains artifacts of
                                                            [] ->
                                                                -- TE.pure ()
                                                                crash "No main!"

                                                            [ name ] ->
                                                                toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                                    |> TE.bind (TE.pure << Html.sandwich name)

                                                            _ ->
                                                                crash "TODO"
                                                    )
                                        )
                            )
                    )
        )



-- GET INFORMATION


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



-- BUILD PROJECTS


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
