module Browser.Install exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as WasRepTask
import Builder.Stuff as Stuff
import Compiler.Elm.Constraint as C
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Data.Map as Dict exposing (Dict)
import System.IO as IO
import Task exposing (Task)
import Utils.Main as Utils exposing (FilePath)



-- RUN


run : Pkg.Name -> Task Never ()
run pkg =
    Reporting.attempt Exit.installToReport
        (Stuff.findRoot
            |> IO.bind
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            IO.pure (Err Exit.InstallNoOutline)

                        Just root ->
                            WasRepTask.run
                                (WasRepTask.eio Exit.InstallBadRegistry Solver.initEnv
                                    |> WasRepTask.bind
                                        (\env ->
                                            WasRepTask.eio Exit.InstallBadOutline (Outline.read root)
                                                |> WasRepTask.bind
                                                    (\oldOutline ->
                                                        case oldOutline of
                                                            Outline.App outline ->
                                                                makeAppPlan env pkg outline
                                                                    |> WasRepTask.bind (\changes -> attemptChanges root env oldOutline V.toChars changes)

                                                            Outline.Pkg outline ->
                                                                makePkgPlan env pkg outline
                                                                    |> WasRepTask.bind (\changes -> attemptChanges root env oldOutline C.toChars changes)
                                                    )
                                        )
                                )
                )
        )



-- ATTEMPT CHANGES


type Changes vsn
    = AlreadyInstalled
    | PromoteTest Outline.Outline
    | PromoteIndirect Outline.Outline
    | Changes Outline.Outline


attemptChanges : String -> Solver.Env -> Outline.Outline -> (a -> String) -> Changes a -> Task Exit.Install ()
attemptChanges root env oldOutline _ changes =
    case changes of
        AlreadyInstalled ->
            WasRepTask.io (IO.putStrLn "It is already installed!")

        PromoteIndirect newOutline ->
            attemptChangesHelp root env oldOutline newOutline

        PromoteTest newOutline ->
            attemptChangesHelp root env oldOutline newOutline

        Changes newOutline ->
            attemptChangesHelp root env oldOutline newOutline


attemptChangesHelp : FilePath -> Solver.Env -> Outline.Outline -> Outline.Outline -> Task Exit.Install ()
attemptChangesHelp root env oldOutline newOutline =
    WasRepTask.eio Exit.InstallBadDetails <|
        BW.withScope
            (\scope ->
                Outline.write root newOutline
                    |> IO.bind (\_ -> Details.verifyInstall scope root env newOutline)
                    |> IO.bind
                        (\result ->
                            case result of
                                Err exit ->
                                    Outline.write root oldOutline
                                        |> IO.fmap (\_ -> Err exit)

                                Ok () ->
                                    IO.putStrLn "Success!"
                                        |> IO.fmap (\_ -> Ok ())
                        )
            )



-- MAKE APP PLAN


makeAppPlan : Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task Exit.Install (Changes V.Version)
makeAppPlan (Solver.Env cache _ connection registry) pkg ((Outline.AppOutline elmVersion sourceDirs direct indirect testDirect testIndirect) as outline) =
    if Dict.member identity pkg direct then
        WasRepTask.pure AlreadyInstalled

    else
        -- is it already indirect?
        case Dict.get identity pkg indirect of
            Just vsn ->
                WasRepTask.pure <|
                    PromoteIndirect <|
                        Outline.App <|
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
                        WasRepTask.pure <|
                            PromoteTest <|
                                Outline.App <|
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
                                WasRepTask.pure <|
                                    PromoteTest <|
                                        Outline.App <|
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
                                                WasRepTask.throw (Exit.InstallUnknownPackageOnline pkg suggestions)

                                            Solver.Offline ->
                                                WasRepTask.throw (Exit.InstallUnknownPackageOffline pkg suggestions)

                                    Ok _ ->
                                        WasRepTask.io (Solver.addToApp cache connection registry pkg outline False)
                                            |> WasRepTask.bind
                                                (\result ->
                                                    case result of
                                                        Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                                            WasRepTask.pure (Changes (Outline.App app))

                                                        Solver.NoSolution ->
                                                            WasRepTask.throw (Exit.InstallNoOnlineAppSolution pkg)

                                                        Solver.NoOfflineSolution ->
                                                            WasRepTask.throw (Exit.InstallNoOfflineAppSolution pkg)

                                                        Solver.SolverErr exit ->
                                                            WasRepTask.throw (Exit.InstallHadSolverTrouble exit)
                                                )



-- MAKE PACKAGE PLAN


makePkgPlan : Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Task Exit.Install (Changes C.Constraint)
makePkgPlan (Solver.Env cache _ connection registry) pkg (Outline.PkgOutline name summary license version exposed deps test elmVersion) =
    if Dict.member identity pkg deps then
        WasRepTask.pure AlreadyInstalled

    else
        -- is already in test dependencies?
        case Dict.get identity pkg test of
            Just con ->
                WasRepTask.pure <|
                    PromoteTest <|
                        Outline.Pkg <|
                            Outline.PkgOutline name
                                summary
                                license
                                version
                                exposed
                                (Dict.insert identity pkg con deps)
                                (Dict.remove identity pkg test)
                                elmVersion

            Nothing ->
                -- try to add a new dependency
                case Registry.getVersions_ pkg registry of
                    Err suggestions ->
                        case connection of
                            Solver.Online _ ->
                                WasRepTask.throw (Exit.InstallUnknownPackageOnline pkg suggestions)

                            Solver.Offline ->
                                WasRepTask.throw (Exit.InstallUnknownPackageOffline pkg suggestions)

                    Ok (Registry.KnownVersions _ _) ->
                        let
                            old : Dict ( String, String ) Pkg.Name C.Constraint
                            old =
                                Dict.union deps test

                            cons : Dict ( String, String ) Pkg.Name C.Constraint
                            cons =
                                Dict.insert identity pkg C.anything old
                        in
                        WasRepTask.io (Solver.verify cache connection registry cons)
                            |> WasRepTask.bind
                                (\result ->
                                    case result of
                                        Solver.SolverOk solution ->
                                            let
                                                (Solver.Details vsn _) =
                                                    Utils.find identity pkg solution

                                                con : C.Constraint
                                                con =
                                                    C.untilNextMajor vsn

                                                new : Dict ( String, String ) Pkg.Name C.Constraint
                                                new =
                                                    Dict.insert identity pkg con old

                                                changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
                                                changes =
                                                    detectChanges old new

                                                news : Dict ( String, String ) Pkg.Name C.Constraint
                                                news =
                                                    Utils.mapMapMaybe identity Pkg.compareName keepNew changes
                                            in
                                            WasRepTask.pure <|
                                                Changes <|
                                                    Outline.Pkg <|
                                                        Outline.PkgOutline name
                                                            summary
                                                            license
                                                            version
                                                            exposed
                                                            (addNews (Just pkg) news deps)
                                                            (addNews Nothing news test)
                                                            elmVersion

                                        Solver.NoSolution ->
                                            WasRepTask.throw (Exit.InstallNoOnlinePkgSolution pkg)

                                        Solver.NoOfflineSolution ->
                                            WasRepTask.throw (Exit.InstallNoOfflinePkgSolution pkg)

                                        Solver.SolverErr exit ->
                                            WasRepTask.throw (Exit.InstallHadSolverTrouble exit)
                                )


addNews : Maybe Pkg.Name -> Dict ( String, String ) Pkg.Name C.Constraint -> Dict ( String, String ) Pkg.Name C.Constraint -> Dict ( String, String ) Pkg.Name C.Constraint
addNews pkg new old =
    Dict.merge compare
        (Dict.insert identity)
        (\k _ n -> Dict.insert identity k n)
        (\k c acc ->
            if Just k == pkg then
                Dict.insert identity k c acc

            else
                acc
        )
        old
        new
        Dict.empty



-- CHANGES


type Change a
    = Insert a
    | Change a a
    | Remove a


detectChanges : Dict ( String, String ) Pkg.Name a -> Dict ( String, String ) Pkg.Name a -> Dict ( String, String ) Pkg.Name (Change a)
detectChanges old new =
    Dict.merge compare
        (\k v -> Dict.insert identity k (Remove v))
        (\k oldElem newElem acc ->
            case keepChange k oldElem newElem of
                Just change ->
                    Dict.insert identity k change acc

                Nothing ->
                    acc
        )
        (\k v -> Dict.insert identity k (Insert v))
        old
        new
        Dict.empty


keepChange : k -> v -> v -> Maybe (Change v)
keepChange _ old new =
    if old == new then
        Nothing

    else
        Just (Change old new)


keepNew : Change a -> Maybe a
keepNew change =
    case change of
        Insert a ->
            Just a

        Change _ a ->
            Just a

        Remove _ ->
            Nothing
