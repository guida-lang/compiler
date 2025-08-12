module Browser.Uninstall exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Deps.Solver as Solver
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Elm.Constraint as C
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Data.Map as Dict exposing (Dict)
import System.IO as IO
import Task exposing (Task)
import Utils.Main exposing (FilePath)
import Utils.Task.Extra as Task



-- RUN


run : Pkg.Name -> Task Never ()
run pkg =
    Reporting.attempt Exit.uninstallToReport
        (Stuff.findRoot
            |> Task.bind
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            Task.pure (Err Exit.UninstallNoOutline)

                        Just root ->
                            Task.run
                                (Task.eio Exit.UninstallBadRegistry Solver.initEnv
                                    |> Task.bind
                                        (\env ->
                                            Task.eio Exit.UninstallBadOutline (Outline.read root)
                                                |> Task.bind
                                                    (\oldOutline ->
                                                        case oldOutline of
                                                            Outline.App outline ->
                                                                makeAppPlan env pkg outline
                                                                    |> Task.bind (\changes -> attemptChanges root env oldOutline changes)

                                                            Outline.Pkg outline ->
                                                                makePkgPlan pkg outline
                                                                    |> Task.bind (\changes -> attemptChanges root env oldOutline changes)
                                                    )
                                        )
                                )
                )
        )



-- ATTEMPT CHANGES


type Changes vsn
    = AlreadyNotPresent
    | Changes Outline.Outline


attemptChanges : String -> Solver.Env -> Outline.Outline -> Changes a -> Task Exit.Uninstall ()
attemptChanges root env oldOutline changes =
    case changes of
        AlreadyNotPresent ->
            Task.io (IO.putStrLn "It is not currently installed!")

        Changes newOutline ->
            attemptChangesHelp root env oldOutline newOutline


attemptChangesHelp : FilePath -> Solver.Env -> Outline.Outline -> Outline.Outline -> Task Exit.Uninstall ()
attemptChangesHelp root env oldOutline newOutline =
    Task.eio Exit.UninstallBadDetails <|
        BW.withScope
            (\scope ->
                Outline.write root newOutline
                    |> Task.bind (\_ -> Details.verifyInstall scope root env newOutline)
                    |> Task.bind
                        (\result ->
                            case result of
                                Err exit ->
                                    Outline.write root oldOutline
                                        |> Task.fmap (\_ -> Err exit)

                                Ok () ->
                                    IO.putStrLn "Success!"
                                        |> Task.fmap (\_ -> Ok ())
                        )
            )



-- MAKE APP PLAN


makeAppPlan : Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task Exit.Uninstall (Changes V.Version)
makeAppPlan (Solver.Env cache _ connection registry) pkg ((Outline.AppOutline _ _ direct _ testDirect _) as outline) =
    case Dict.get identity pkg (Dict.union direct testDirect) of
        Just _ ->
            Task.io (Solver.removeFromApp cache connection registry pkg outline)
                |> Task.bind
                    (\result ->
                        case result of
                            Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                Task.pure (Changes (Outline.App app))

                            Solver.NoSolution ->
                                Task.throw (Exit.UninstallNoOnlineAppSolution pkg)

                            Solver.NoOfflineSolution ->
                                Task.throw (Exit.UninstallNoOfflineAppSolution pkg)

                            Solver.SolverErr exit ->
                                Task.throw (Exit.UninstallHadSolverTrouble exit)
                    )

        Nothing ->
            Task.pure AlreadyNotPresent



-- MAKE PACKAGE PLAN


makePkgPlan : Pkg.Name -> Outline.PkgOutline -> Task Exit.Uninstall (Changes C.Constraint)
makePkgPlan pkg (Outline.PkgOutline name summary license version exposed deps test elmVersion) =
    let
        old : Dict ( String, String ) Pkg.Name C.Constraint
        old =
            Dict.union deps test
    in
    if Dict.member identity pkg old then
        Task.pure <|
            Changes <|
                Outline.Pkg <|
                    Outline.PkgOutline name
                        summary
                        license
                        version
                        exposed
                        (Dict.remove identity pkg deps)
                        (Dict.remove identity pkg test)
                        elmVersion

    else
        Task.pure AlreadyNotPresent
