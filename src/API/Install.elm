module API.Install exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Guida.Details as Details
import Builder.Guida.Outline as Outline
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Generate.Target as Target
import Compiler.Guida.Constraint as C
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Data.Map as Dict exposing (Dict)
import System.IO as IO
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- RUN


run : Pkg.Name -> Task Never ()
run pkg =
    Reporting.attempt Exit.installToReport
        (Stuff.findRoot
            |> Task.bind
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            Task.pure (Err Exit.InstallNoOutline)

                        Just root ->
                            Task.run
                                (Task.eio Exit.InstallBadRegistry Solver.initEnv
                                    |> Task.bind
                                        (\env ->
                                            Task.eio Exit.InstallBadOutline (Outline.read root)
                                                |> Task.bind
                                                    (\oldOutline ->
                                                        case oldOutline of
                                                            Outline.App outline ->
                                                                makeAppPlan root env pkg outline
                                                                    |> Task.bind (\changes -> attemptChanges root env oldOutline changes)

                                                            Outline.Pkg outline ->
                                                                makePkgPlan root env pkg outline
                                                                    |> Task.bind (\changes -> attemptChanges root env oldOutline changes)
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


attemptChanges : Stuff.Root -> Solver.Env -> Outline.Outline -> Changes a -> Task Exit.Install ()
attemptChanges root env oldOutline changes =
    case changes of
        AlreadyInstalled ->
            Task.io (IO.putStrLn "It is already installed!")

        PromoteIndirect newOutline ->
            attemptChangesHelp root env oldOutline newOutline

        PromoteTest newOutline ->
            attemptChangesHelp root env oldOutline newOutline

        Changes newOutline ->
            attemptChangesHelp root env oldOutline newOutline


attemptChangesHelp : Stuff.Root -> Solver.Env -> Outline.Outline -> Outline.Outline -> Task Exit.Install ()
attemptChangesHelp root env oldOutline newOutline =
    Task.eio Exit.InstallBadDetails <|
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


makeAppPlan : Stuff.Root -> Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task Exit.Install (Changes V.Version)
makeAppPlan root (Solver.Env cache _ connection registry) pkg outline =
    case outline of
        Outline.GuidaAppOutline guidaVersion sourceDirs direct indirect testDirect testIndirect ->
            if Dict.member identity pkg direct then
                Task.pure AlreadyInstalled

            else
                -- is it already indirect?
                case Dict.get identity pkg indirect of
                    Just vsn ->
                        Task.pure <|
                            PromoteIndirect <|
                                Outline.App <|
                                    Outline.GuidaAppOutline guidaVersion
                                        sourceDirs
                                        (Dict.insert identity pkg vsn direct)
                                        (Dict.remove identity pkg indirect)
                                        testDirect
                                        testIndirect

                    Nothing ->
                        -- is it already a test dependency?
                        case Dict.get identity pkg testDirect of
                            Just vsn ->
                                Task.pure <|
                                    PromoteTest <|
                                        Outline.App <|
                                            Outline.GuidaAppOutline guidaVersion
                                                sourceDirs
                                                (Dict.insert identity pkg vsn direct)
                                                indirect
                                                (Dict.remove identity pkg testDirect)
                                                testIndirect

                            Nothing ->
                                -- is it already an indirect test dependency?
                                case Dict.get identity pkg testIndirect of
                                    Just vsn ->
                                        Task.pure <|
                                            PromoteTest <|
                                                Outline.App <|
                                                    Outline.GuidaAppOutline guidaVersion
                                                        sourceDirs
                                                        (Dict.insert identity pkg vsn direct)
                                                        indirect
                                                        testDirect
                                                        (Dict.remove identity pkg testIndirect)

                                    Nothing ->
                                        -- finally try to add it from scratch
                                        case Registry.getVersions_ (Registry.FilterByTarget Target.GuidaTarget) pkg registry of
                                            Err suggestions ->
                                                case connection of
                                                    Solver.Online _ ->
                                                        Task.throw (Exit.InstallUnknownPackageOnline (Stuff.rootPath root) pkg suggestions)

                                                    Solver.Offline ->
                                                        Task.throw (Exit.InstallUnknownPackageOffline (Stuff.rootPath root) pkg suggestions)

                                            Ok _ ->
                                                Task.io (Solver.addToApp cache connection registry pkg outline False)
                                                    |> Task.bind
                                                        (\result ->
                                                            case result of
                                                                Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                                                    Task.pure (Changes (Outline.App app))

                                                                Solver.NoSolution ->
                                                                    Task.throw (Exit.InstallGuidaNoOnlineAppSolution pkg)

                                                                Solver.NoOfflineSolution ->
                                                                    Task.throw (Exit.InstallGuidaNoOfflineAppSolution (Stuff.rootPath root) pkg)

                                                                Solver.SolverErr exit ->
                                                                    Task.throw (Exit.InstallHadSolverTrouble exit)
                                                        )

        Outline.ElmAppOutline elmVersion sourceDirs direct indirect testDirect testIndirect ->
            if Dict.member identity pkg direct then
                Task.pure AlreadyInstalled

            else
                -- is it already indirect?
                case Dict.get identity pkg indirect of
                    Just vsn ->
                        Task.pure <|
                            PromoteIndirect <|
                                Outline.App <|
                                    Outline.ElmAppOutline elmVersion
                                        sourceDirs
                                        (Dict.insert identity pkg vsn direct)
                                        (Dict.remove identity pkg indirect)
                                        testDirect
                                        testIndirect

                    Nothing ->
                        -- is it already a test dependency?
                        case Dict.get identity pkg testDirect of
                            Just vsn ->
                                Task.pure <|
                                    PromoteTest <|
                                        Outline.App <|
                                            Outline.ElmAppOutline elmVersion
                                                sourceDirs
                                                (Dict.insert identity pkg vsn direct)
                                                indirect
                                                (Dict.remove identity pkg testDirect)
                                                testIndirect

                            Nothing ->
                                -- is it already an indirect test dependency?
                                case Dict.get identity pkg testIndirect of
                                    Just vsn ->
                                        Task.pure <|
                                            PromoteTest <|
                                                Outline.App <|
                                                    Outline.ElmAppOutline elmVersion
                                                        sourceDirs
                                                        (Dict.insert identity pkg vsn direct)
                                                        indirect
                                                        testDirect
                                                        (Dict.remove identity pkg testIndirect)

                                    Nothing ->
                                        -- finally try to add it from scratch
                                        case Registry.getVersions_ (Registry.FilterByTarget Target.ElmTarget) pkg registry of
                                            Err suggestions ->
                                                case connection of
                                                    Solver.Online _ ->
                                                        Task.throw (Exit.InstallUnknownPackageOnline (Stuff.rootPath root) pkg suggestions)

                                                    Solver.Offline ->
                                                        Task.throw (Exit.InstallUnknownPackageOffline (Stuff.rootPath root) pkg suggestions)

                                            Ok _ ->
                                                Task.io (Solver.addToApp cache connection registry pkg outline False)
                                                    |> Task.bind
                                                        (\result ->
                                                            case result of
                                                                Solver.SolverOk (Solver.AppSolution _ _ app) ->
                                                                    Task.pure (Changes (Outline.App app))

                                                                Solver.NoSolution ->
                                                                    Task.throw (Exit.InstallElmNoOnlineAppSolution pkg)

                                                                Solver.NoOfflineSolution ->
                                                                    Task.throw (Exit.InstallGuidaNoOfflineAppSolution (Stuff.rootPath root) pkg)

                                                                Solver.SolverErr exit ->
                                                                    Task.throw (Exit.InstallHadSolverTrouble exit)
                                                        )



-- MAKE PACKAGE PLAN


makePkgPlan : Stuff.Root -> Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Task Exit.Install (Changes C.Constraint)
makePkgPlan root (Solver.Env cache _ connection registry) pkg outline =
    case outline of
        Outline.GuidaPkgOutline name summary license version exposed deps test guidaVersion ->
            if Dict.member identity pkg deps then
                Task.pure AlreadyInstalled

            else
                -- is already in test dependencies?
                case Dict.get identity pkg test of
                    Just con ->
                        Task.pure <|
                            PromoteTest <|
                                Outline.Pkg <|
                                    Outline.GuidaPkgOutline name
                                        summary
                                        license
                                        version
                                        exposed
                                        (Dict.insert identity pkg con deps)
                                        (Dict.remove identity pkg test)
                                        guidaVersion

                    Nothing ->
                        -- try to add a new dependency
                        case Registry.getVersions_ (Registry.FilterByTarget Target.GuidaTarget) pkg registry of
                            Err suggestions ->
                                case connection of
                                    Solver.Online _ ->
                                        Task.throw (Exit.InstallUnknownPackageOnline (Stuff.rootPath root) pkg suggestions)

                                    Solver.Offline ->
                                        Task.throw (Exit.InstallUnknownPackageOffline (Stuff.rootPath root) pkg suggestions)

                            Ok (Registry.KnownVersions _ _) ->
                                let
                                    old : Dict ( String, String ) Pkg.Name C.Constraint
                                    old =
                                        Dict.union deps test

                                    cons : Dict ( String, String ) Pkg.Name C.Constraint
                                    cons =
                                        Dict.insert identity pkg C.anything old
                                in
                                Task.io (Solver.verify Target.GuidaTarget cache connection registry cons)
                                    |> Task.bind
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
                                                    Task.pure <|
                                                        Changes <|
                                                            Outline.Pkg <|
                                                                Outline.GuidaPkgOutline name
                                                                    summary
                                                                    license
                                                                    version
                                                                    exposed
                                                                    (addNews (Just pkg) news deps)
                                                                    (addNews Nothing news test)
                                                                    guidaVersion

                                                Solver.NoSolution ->
                                                    Task.throw (Exit.InstallGuidaNoOnlinePkgSolution pkg)

                                                Solver.NoOfflineSolution ->
                                                    Task.throw (Exit.InstallGuidaNoOfflinePkgSolution (Stuff.rootPath root) pkg)

                                                Solver.SolverErr exit ->
                                                    Task.throw (Exit.InstallHadSolverTrouble exit)
                                        )

        Outline.ElmPkgOutline name summary license version exposed deps test elmVersion ->
            if Dict.member identity pkg deps then
                Task.pure AlreadyInstalled

            else
                -- is already in test dependencies?
                case Dict.get identity pkg test of
                    Just con ->
                        Task.pure <|
                            PromoteTest <|
                                Outline.Pkg <|
                                    Outline.ElmPkgOutline name
                                        summary
                                        license
                                        version
                                        exposed
                                        (Dict.insert identity pkg con deps)
                                        (Dict.remove identity pkg test)
                                        elmVersion

                    Nothing ->
                        -- try to add a new dependency
                        case Registry.getVersions_ (Registry.FilterByTarget Target.ElmTarget) pkg registry of
                            Err suggestions ->
                                case connection of
                                    Solver.Online _ ->
                                        Task.throw (Exit.InstallUnknownPackageOnline (Stuff.rootPath root) pkg suggestions)

                                    Solver.Offline ->
                                        Task.throw (Exit.InstallUnknownPackageOffline (Stuff.rootPath root) pkg suggestions)

                            Ok (Registry.KnownVersions _ _) ->
                                let
                                    old : Dict ( String, String ) Pkg.Name C.Constraint
                                    old =
                                        Dict.union deps test

                                    cons : Dict ( String, String ) Pkg.Name C.Constraint
                                    cons =
                                        Dict.insert identity pkg C.anything old
                                in
                                Task.io (Solver.verify Target.ElmTarget cache connection registry cons)
                                    |> Task.bind
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
                                                    Task.pure <|
                                                        Changes <|
                                                            Outline.Pkg <|
                                                                Outline.ElmPkgOutline name
                                                                    summary
                                                                    license
                                                                    version
                                                                    exposed
                                                                    (addNews (Just pkg) news deps)
                                                                    (addNews Nothing news test)
                                                                    elmVersion

                                                Solver.NoSolution ->
                                                    Task.throw (Exit.InstallElmNoOnlinePkgSolution pkg)

                                                Solver.NoOfflineSolution ->
                                                    Task.throw (Exit.InstallElmNoOfflinePkgSolution (Stuff.rootPath root) pkg)

                                                Solver.SolverErr exit ->
                                                    Task.throw (Exit.InstallHadSolverTrouble exit)
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
            case keepChange oldElem newElem of
                Just change ->
                    Dict.insert identity k change acc

                Nothing ->
                    acc
        )
        (\k v -> Dict.insert identity k (Insert v))
        old
        new
        Dict.empty


keepChange : v -> v -> Maybe (Change v)
keepChange old new =
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
