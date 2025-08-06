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
import Utils.Task.Extra as TE



-- RUN


run : Pkg.Name -> Task Never ()
run pkg =
    Reporting.attempt Exit.uninstallToReport
        (Stuff.findRoot
            |> TE.bind
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            TE.pure (Err Exit.UninstallNoOutline)

                        Just root ->
                            TE.toResult
                                (TE.eio Exit.UninstallBadRegistry Solver.initEnv
                                    |> TE.bind
                                        (\env ->
                                            TE.eio Exit.UninstallBadOutline (Outline.read root)
                                                |> TE.bind
                                                    (\oldOutline ->
                                                        case oldOutline of
                                                            Outline.App outline ->
                                                                makeAppPlan env pkg outline
                                                                    |> TE.bind (\changes -> attemptChanges root env oldOutline changes)

                                                            Outline.Pkg outline ->
                                                                makePkgPlan pkg outline
                                                                    |> TE.bind (\changes -> attemptChanges root env oldOutline changes)
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
            TE.io (IO.putStrLn "It is not currently installed!")

        Changes newOutline ->
            attemptChangesHelp root env oldOutline newOutline


attemptChangesHelp : FilePath -> Solver.Env -> Outline.Outline -> Outline.Outline -> Task Exit.Uninstall ()
attemptChangesHelp root env oldOutline newOutline =
    TE.eio Exit.UninstallBadDetails <|
        BW.withScope
            (\scope ->
                Outline.write root newOutline
                    |> TE.bind (\_ -> Details.verifyInstall scope root env newOutline)
                    |> TE.bind
                        (\result ->
                            case result of
                                Err exit ->
                                    Outline.write root oldOutline
                                        |> TE.fmap (\_ -> Err exit)

                                Ok () ->
                                    IO.putStrLn "Success!"
                                        |> TE.fmap (\_ -> Ok ())
                        )
            )



-- MAKE APP PLAN


makeAppPlan : Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task Exit.Uninstall (Changes V.Version)
makeAppPlan (Solver.Env cache _ connection registry) pkg ((Outline.AppOutline _ _ direct _ testDirect _) as outline) =
    case Dict.get identity pkg (Dict.union direct testDirect) of
        Just _ ->
            TE.io (Solver.removeFromApp cache connection registry pkg outline)
                |> TE.bind
                    (\result ->
                        case result of
                            Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                TE.pure (Changes (Outline.App app))

                            Solver.NoSolution ->
                                TE.throw (Exit.UninstallNoOnlineAppSolution pkg)

                            Solver.NoOfflineSolution ->
                                TE.throw (Exit.UninstallNoOfflineAppSolution pkg)

                            Solver.SolverErr exit ->
                                TE.throw (Exit.UninstallHadSolverTrouble exit)
                    )

        Nothing ->
            TE.pure AlreadyNotPresent



-- MAKE PACKAGE PLAN


makePkgPlan : Pkg.Name -> Outline.PkgOutline -> Task Exit.Uninstall (Changes C.Constraint)
makePkgPlan pkg (Outline.PkgOutline name summary license version exposed deps test elmVersion) =
    let
        old : Dict ( String, String ) Pkg.Name C.Constraint
        old =
            Dict.union deps test
    in
    if Dict.member identity pkg old then
        TE.pure <|
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
        TE.pure AlreadyNotPresent
