module API.Uninstall exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Deps.Solver as Solver
import Builder.Guida.Details as Details
import Builder.Guida.Outline as Outline
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Guida.Constraint as C
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Data.Map as Dict exposing (Dict)
import Task exposing (Task)
import Utils.System.IO as IO
import Utils.Task.Extra as Task



-- RUN


run : Pkg.Name -> Task Never ()
run pkg =
    Reporting.attempt Exit.uninstallToReport
        (Stuff.findRoot
            |> Task.andThen
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            Task.succeed (Err Exit.UninstallNoOutline)

                        Just root ->
                            Task.run
                                (Task.eio Exit.UninstallBadRegistry Solver.initEnv
                                    |> Task.andThen
                                        (\env ->
                                            Task.eio Exit.UninstallBadOutline (Outline.read root)
                                                |> Task.andThen
                                                    (\oldOutline ->
                                                        case oldOutline of
                                                            Outline.App outline ->
                                                                makeAppPlan root env pkg outline
                                                                    |> Task.andThen (\changes -> attemptChanges root env oldOutline changes)

                                                            Outline.Pkg outline ->
                                                                makePkgPlan pkg outline
                                                                    |> Task.andThen (\changes -> attemptChanges root env oldOutline changes)
                                                    )
                                        )
                                )
                )
        )



-- ATTEMPT CHANGES


type Changes vsn
    = AlreadyNotPresent
    | Changes Outline.Outline


attemptChanges : Stuff.Root -> Solver.Env -> Outline.Outline -> Changes a -> Task Exit.Uninstall ()
attemptChanges root env oldOutline changes =
    case changes of
        AlreadyNotPresent ->
            Task.io (IO.putStrLn "It is not currently installed!")

        Changes newOutline ->
            attemptChangesHelp root env oldOutline newOutline


attemptChangesHelp : Stuff.Root -> Solver.Env -> Outline.Outline -> Outline.Outline -> Task Exit.Uninstall ()
attemptChangesHelp root env oldOutline newOutline =
    Task.eio Exit.UninstallBadDetails <|
        BW.withScope
            (\scope ->
                Outline.write root newOutline
                    |> Task.andThen (\_ -> Details.verifyInstall scope root env newOutline)
                    |> Task.andThen
                        (\result ->
                            case result of
                                Err exit ->
                                    Outline.write root oldOutline
                                        |> Task.map (\_ -> Err exit)

                                Ok () ->
                                    IO.putStrLn "Success!"
                                        |> Task.map (\_ -> Ok ())
                        )
            )



-- MAKE APP PLAN


makeAppPlan : Stuff.Root -> Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task Exit.Uninstall (Changes V.Version)
makeAppPlan root (Solver.Env cache _ connection registry) pkg outline =
    case outline of
        Outline.GuidaAppOutline _ _ direct _ testDirect _ ->
            case Dict.get identity pkg (Dict.union direct testDirect) of
                Just _ ->
                    Task.io (Solver.removeFromApp cache connection registry pkg outline)
                        |> Task.andThen
                            (\result ->
                                case result of
                                    Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                        Task.succeed (Changes (Outline.App app))

                                    Solver.NoSolution ->
                                        Task.fail (Exit.UninstallGuidaNoOnlineAppSolution pkg)

                                    Solver.NoOfflineSolution ->
                                        Task.fail (Exit.UninstallGuidaNoOfflineAppSolution (Stuff.rootPath root) pkg)

                                    Solver.SolverErr exit ->
                                        Task.fail (Exit.UninstallHadSolverTrouble exit)
                            )

                Nothing ->
                    Task.succeed AlreadyNotPresent

        Outline.ElmAppOutline _ _ direct _ testDirect _ ->
            case Dict.get identity pkg (Dict.union direct testDirect) of
                Just _ ->
                    Task.io (Solver.removeFromApp cache connection registry pkg outline)
                        |> Task.andThen
                            (\result ->
                                case result of
                                    Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                        Task.succeed (Changes (Outline.App app))

                                    Solver.NoSolution ->
                                        Task.fail (Exit.UninstallElmNoOnlineAppSolution pkg)

                                    Solver.NoOfflineSolution ->
                                        Task.fail (Exit.UninstallElmNoOfflineAppSolution (Stuff.rootPath root) pkg)

                                    Solver.SolverErr exit ->
                                        Task.fail (Exit.UninstallHadSolverTrouble exit)
                            )

                Nothing ->
                    Task.succeed AlreadyNotPresent



-- MAKE PACKAGE PLAN


makePkgPlan : Pkg.Name -> Outline.PkgOutline -> Task Exit.Uninstall (Changes C.Constraint)
makePkgPlan pkg outline =
    case outline of
        Outline.GuidaPkgOutline name summary license version exposed deps test elmVersion ->
            let
                old : Dict ( String, String ) Pkg.Name C.Constraint
                old =
                    Dict.union deps test
            in
            if Dict.member identity pkg old then
                Task.succeed <|
                    Changes <|
                        Outline.Pkg <|
                            Outline.GuidaPkgOutline name
                                summary
                                license
                                version
                                exposed
                                (Dict.remove identity pkg deps)
                                (Dict.remove identity pkg test)
                                elmVersion

            else
                Task.succeed AlreadyNotPresent

        Outline.ElmPkgOutline name summary license version exposed deps test elmVersion ->
            let
                old : Dict ( String, String ) Pkg.Name C.Constraint
                old =
                    Dict.union deps test
            in
            if Dict.member identity pkg old then
                Task.succeed <|
                    Changes <|
                        Outline.Pkg <|
                            Outline.ElmPkgOutline name
                                summary
                                license
                                version
                                exposed
                                (Dict.remove identity pkg deps)
                                (Dict.remove identity pkg test)
                                elmVersion

            else
                Task.succeed AlreadyNotPresent
