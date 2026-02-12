module API.Diagnostics exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Generate as Generate
import Builder.Guida.Details as Details
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Data.NonEmptyList as NE
import Compiler.Generate.Html as Html
import Task exposing (Task)
import Terminal.Terminal.Internal exposing (Parser(..))
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- RUN


run : String -> Task Never (Result Exit.Make ())
run path =
    Stuff.findRoot
        |> Task.bind
            (\maybeRoot ->
                case maybeRoot of
                    Just root ->
                        runHelp root path

                    Nothing ->
                        Task.pure (Err Exit.MakeNoOutline)
            )


runHelp : Stuff.Root -> String -> Task Never (Result Exit.Make ())
runHelp root path =
    BW.withScope
        (\scope ->
            Stuff.withRootLock (Stuff.rootPath root) <|
                Task.run <|
                    let
                        style : Reporting.Style
                        style =
                            -- Reporting.json
                            Reporting.silent
                    in
                    Task.eio Exit.MakeBadDetails (Details.load style scope root)
                        |> Task.bind
                            (\details ->
                                buildPaths style root details (NE.Nonempty path [])
                                    |> Task.bind
                                        (\artifacts ->
                                            toBuilder False Html.leadingLines root details artifacts
                                                |> Task.map (\_ -> ())
                                        )
                            )
        )



-- BUILD PROJECTS


buildPaths : Reporting.Style -> Stuff.Root -> Details.Details -> NE.Nonempty FilePath -> Task Exit.Make Build.Artifacts
buildPaths style root details paths =
    Task.eio Exit.MakeCannotBuild <|
        Build.fromPaths style root details paths



-- TO BUILDER


toBuilder : Bool -> Int -> Stuff.Root -> Details.Details -> Build.Artifacts -> Task Exit.Make String
toBuilder withSourceMaps leadingLines root details artifacts =
    Task.mapError Exit.MakeBadGenerate <|
        Generate.prod withSourceMaps leadingLines root details artifacts
