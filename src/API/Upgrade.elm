module API.Upgrade exposing (run)

import Builder.BackgroundWriter as BW
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


run : Task Never ()
run =
    Reporting.attempt Exit.upgradeToReport
        (Stuff.findRoot
            |> Task.andThen
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            Task.succeed (Err Exit.UpgradeNoOutline)

                        Just root ->
                            Task.run
                                (Task.eio Exit.UpgradeBadRegistry Solver.initEnv
                                    |> Task.andThen
                                        (\env ->
                                            Task.eio Exit.UpgradeBadOutline (Outline.read root)
                                                |> Task.andThen
                                                    (\oldOutline ->
                                                        case oldOutline of
                                                            Outline.App outline ->
                                                                makeAppPlan root env outline
                                                                    |> Task.andThen (\changes -> attemptChanges root env oldOutline changes)

                                                            Outline.Pkg outline ->
                                                                makePkgPlan root env outline
                                                                    |> Task.andThen (\changes -> attemptChanges root env oldOutline changes)
                                                    )
                                        )
                                )
                )
        )



-- ATTEMPT CHANGES


type Changes
    = AlreadyUpgraded
    | Changes Outline.Outline


attemptChanges : Stuff.Root -> Solver.Env -> Outline.Outline -> Changes -> Task Exit.Upgrade ()
attemptChanges root env oldOutline changes =
    case changes of
        AlreadyUpgraded ->
            Task.io (IO.putStrLn "Everything is already up to date!")

        Changes newOutline ->
            attemptChangesHelp root env oldOutline newOutline


attemptChangesHelp : Stuff.Root -> Solver.Env -> Outline.Outline -> Outline.Outline -> Task Exit.Upgrade ()
attemptChangesHelp root env oldOutline newOutline =
    Task.eio Exit.UpgradeBadDetails <|
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


makeAppPlan : Stuff.Root -> Solver.Env -> Outline.AppOutline -> Task Exit.Upgrade Changes
makeAppPlan root (Solver.Env cache _ connection registry) outline =
    case outline of
        Outline.GuidaAppOutline guidaVersion sourceDirs direct indirect testDirect testIndirect ->
            let
                oldAll : Dict ( String, String ) Pkg.Name V.Version
                oldAll =
                    Utils.mapUnions [ direct, indirect, testDirect, testIndirect ]

                constraints : Dict ( String, String ) Pkg.Name C.Constraint
                constraints =
                    Dict.map (\_ -> C.untilNextMajor) (Dict.union direct testDirect)
            in
            Task.io (Solver.verify Target.GuidaTarget cache connection registry constraints)
                |> Task.andThen
                    (\result ->
                        case result of
                            Solver.SolverOk details ->
                                let
                                    solved : Dict ( String, String ) Pkg.Name V.Version
                                    solved =
                                        Dict.map (\_ (Solver.Details vsn _) -> vsn) details

                                    newDirect : Dict ( String, String ) Pkg.Name V.Version
                                    newDirect =
                                        Dict.intersection Pkg.compareName solved direct

                                    newTestDirect : Dict ( String, String ) Pkg.Name V.Version
                                    newTestDirect =
                                        Dict.intersection Pkg.compareName solved testDirect

                                    newIndirect : Dict ( String, String ) Pkg.Name V.Version
                                    newIndirect =
                                        Dict.diff (getTransitive details solved (Dict.toList compare newDirect) Dict.empty) newDirect

                                    newTestIndirect : Dict ( String, String ) Pkg.Name V.Version
                                    newTestIndirect =
                                        Dict.diff solved (Utils.mapUnions [ newDirect, newIndirect, newTestDirect ])

                                    newAll : Dict ( String, String ) Pkg.Name V.Version
                                    newAll =
                                        Utils.mapUnions [ newDirect, newIndirect, newTestDirect, newTestIndirect ]
                                in
                                if Dict.isEmpty (detectChanges oldAll newAll) then
                                    Task.succeed AlreadyUpgraded

                                else
                                    Task.succeed <|
                                        Changes <|
                                            Outline.App <|
                                                Outline.GuidaAppOutline guidaVersion sourceDirs newDirect newIndirect newTestDirect newTestIndirect

                            Solver.NoSolution ->
                                Task.fail Exit.UpgradeGuidaNoOnlineSolution

                            Solver.NoOfflineSolution ->
                                Task.fail (Exit.UpgradeGuidaNoOfflineSolution (Stuff.rootPath root))

                            Solver.SolverErr exit ->
                                Task.fail (Exit.UpgradeHadSolverTrouble exit)
                    )

        Outline.ElmAppOutline elmVersion sourceDirs direct indirect testDirect testIndirect ->
            let
                oldAll : Dict ( String, String ) Pkg.Name V.Version
                oldAll =
                    Utils.mapUnions [ direct, indirect, testDirect, testIndirect ]

                constraints : Dict ( String, String ) Pkg.Name C.Constraint
                constraints =
                    Dict.map (\_ -> C.untilNextMajor) (Dict.union direct testDirect)
            in
            Task.io (Solver.verify Target.ElmTarget cache connection registry constraints)
                |> Task.andThen
                    (\result ->
                        case result of
                            Solver.SolverOk details ->
                                let
                                    solved : Dict ( String, String ) Pkg.Name V.Version
                                    solved =
                                        Dict.map (\_ (Solver.Details vsn _) -> vsn) details

                                    newDirect : Dict ( String, String ) Pkg.Name V.Version
                                    newDirect =
                                        Dict.intersection Pkg.compareName solved direct

                                    newTestDirect : Dict ( String, String ) Pkg.Name V.Version
                                    newTestDirect =
                                        Dict.intersection Pkg.compareName solved testDirect

                                    newIndirect : Dict ( String, String ) Pkg.Name V.Version
                                    newIndirect =
                                        Dict.diff (getTransitive details solved (Dict.toList compare newDirect) Dict.empty) newDirect

                                    newTestIndirect : Dict ( String, String ) Pkg.Name V.Version
                                    newTestIndirect =
                                        Dict.diff solved (Utils.mapUnions [ newDirect, newIndirect, newTestDirect ])

                                    newAll : Dict ( String, String ) Pkg.Name V.Version
                                    newAll =
                                        Utils.mapUnions [ newDirect, newIndirect, newTestDirect, newTestIndirect ]
                                in
                                if Dict.isEmpty (detectChanges oldAll newAll) then
                                    Task.succeed AlreadyUpgraded

                                else
                                    Task.succeed <|
                                        Changes <|
                                            Outline.App <|
                                                Outline.ElmAppOutline elmVersion sourceDirs newDirect newIndirect newTestDirect newTestIndirect

                            Solver.NoSolution ->
                                Task.fail Exit.UpgradeElmNoOnlineSolution

                            Solver.NoOfflineSolution ->
                                Task.fail (Exit.UpgradeElmNoOfflineSolution (Stuff.rootPath root))

                            Solver.SolverErr exit ->
                                Task.fail (Exit.UpgradeHadSolverTrouble exit)
                    )



-- MAKE PACKAGE PLAN


makePkgPlan : Stuff.Root -> Solver.Env -> Outline.PkgOutline -> Task Exit.Upgrade Changes
makePkgPlan root (Solver.Env cache _ connection registry) outline =
    case outline of
        Outline.GuidaPkgOutline name summary license version exposed deps test guidaVersion ->
            let
                oldAll : Dict ( String, String ) Pkg.Name C.Constraint
                oldAll =
                    Dict.union deps test
            in
            case toSafeConstraints oldAll of
                Err pkg ->
                    Task.fail (Exit.UpgradeCannotBuildSafeConstraint pkg)

                Ok constraints ->
                    Task.io (Solver.verify Target.GuidaTarget cache connection registry constraints)
                        |> Task.andThen
                            (\result ->
                                case result of
                                    Solver.SolverOk solved ->
                                        let
                                            newDeps : Dict ( String, String ) Pkg.Name C.Constraint
                                            newDeps =
                                                Dict.map
                                                    (\pkg oldConstraint ->
                                                        upgradeConstraint oldConstraint (Utils.find identity pkg solved)
                                                    )
                                                    deps

                                            newTest : Dict ( String, String ) Pkg.Name C.Constraint
                                            newTest =
                                                Dict.map
                                                    (\pkg oldConstraint ->
                                                        upgradeConstraint oldConstraint (Utils.find identity pkg solved)
                                                    )
                                                    test

                                            newAll : Dict ( String, String ) Pkg.Name C.Constraint
                                            newAll =
                                                Dict.union newDeps newTest
                                        in
                                        if Dict.isEmpty (detectChanges oldAll newAll) then
                                            Task.succeed AlreadyUpgraded

                                        else
                                            Task.succeed <|
                                                Changes <|
                                                    Outline.Pkg <|
                                                        Outline.GuidaPkgOutline name summary license version exposed newDeps newTest guidaVersion

                                    Solver.NoSolution ->
                                        Task.fail Exit.UpgradeGuidaNoOnlineSolution

                                    Solver.NoOfflineSolution ->
                                        Task.fail (Exit.UpgradeGuidaNoOfflineSolution (Stuff.rootPath root))

                                    Solver.SolverErr exit ->
                                        Task.fail (Exit.UpgradeHadSolverTrouble exit)
                            )

        Outline.ElmPkgOutline name summary license version exposed deps test elmVersion ->
            let
                oldAll : Dict ( String, String ) Pkg.Name C.Constraint
                oldAll =
                    Dict.union deps test
            in
            case toSafeConstraints oldAll of
                Err pkg ->
                    Task.fail (Exit.UpgradeCannotBuildSafeConstraint pkg)

                Ok constraints ->
                    Task.io (Solver.verify Target.ElmTarget cache connection registry constraints)
                        |> Task.andThen
                            (\result ->
                                case result of
                                    Solver.SolverOk solved ->
                                        let
                                            newDeps : Dict ( String, String ) Pkg.Name C.Constraint
                                            newDeps =
                                                Dict.map
                                                    (\pkg oldConstraint ->
                                                        upgradeConstraint oldConstraint (Utils.find identity pkg solved)
                                                    )
                                                    deps

                                            newTest : Dict ( String, String ) Pkg.Name C.Constraint
                                            newTest =
                                                Dict.map
                                                    (\pkg oldConstraint ->
                                                        upgradeConstraint oldConstraint (Utils.find identity pkg solved)
                                                    )
                                                    test

                                            newAll : Dict ( String, String ) Pkg.Name C.Constraint
                                            newAll =
                                                Dict.union newDeps newTest
                                        in
                                        if Dict.isEmpty (detectChanges oldAll newAll) then
                                            Task.succeed AlreadyUpgraded

                                        else
                                            Task.succeed <|
                                                Changes <|
                                                    Outline.Pkg <|
                                                        Outline.ElmPkgOutline name summary license version exposed newDeps newTest elmVersion

                                    Solver.NoSolution ->
                                        Task.fail Exit.UpgradeElmNoOnlineSolution

                                    Solver.NoOfflineSolution ->
                                        Task.fail (Exit.UpgradeElmNoOfflineSolution (Stuff.rootPath root))

                                    Solver.SolverErr exit ->
                                        Task.fail (Exit.UpgradeHadSolverTrouble exit)
                            )



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


keepChange : a -> a -> Maybe (Change a)
keepChange old new =
    if old == new then
        Nothing

    else
        Just (Change old new)


toSafeConstraints : Dict ( String, String ) Pkg.Name C.Constraint -> Result Pkg.Name (Dict ( String, String ) Pkg.Name C.Constraint)
toSafeConstraints =
    Dict.foldr compare
        (\pkg oldConstraint result ->
            case result of
                Err badPkg ->
                    Err badPkg

                Ok acc ->
                    case C.intersect oldConstraint (C.untilNextMajor (C.lowerBound oldConstraint)) of
                        Just safeConstraint ->
                            Ok (Dict.insert identity pkg safeConstraint acc)

                        Nothing ->
                            Err pkg
        )
        (Ok Dict.empty)


upgradeConstraint : C.Constraint -> Solver.Details -> C.Constraint
upgradeConstraint oldConstraint (Solver.Details version _) =
    C.intersect oldConstraint (C.untilNextMajor version)
        |> Maybe.withDefault oldConstraint


getDeps : Dict ( String, String ) Pkg.Name Solver.Details -> Pkg.Name -> Dict ( String, String ) Pkg.Name C.Constraint
getDeps details pkg =
    let
        (Solver.Details _ deps) =
            Utils.find identity pkg details
    in
    deps


getTransitive : Dict ( String, String ) Pkg.Name Solver.Details -> Dict ( String, String ) Pkg.Name V.Version -> List ( Pkg.Name, V.Version ) -> Dict ( String, String ) Pkg.Name V.Version -> Dict ( String, String ) Pkg.Name V.Version
getTransitive details solution unvisited visited =
    case unvisited of
        [] ->
            visited

        ( pkg, version ) :: infos ->
            if Dict.member identity pkg visited then
                getTransitive details solution infos visited

            else
                let
                    deps : Dict ( String, String ) Pkg.Name C.Constraint
                    deps =
                        getDeps details pkg

                    newUnvisited : List ( Pkg.Name, V.Version )
                    newUnvisited =
                        Dict.toList compare (Dict.intersection Pkg.compareName solution (Dict.diff deps visited))

                    newVisited : Dict ( String, String ) Pkg.Name V.Version
                    newVisited =
                        Dict.insert identity pkg version visited
                in
                getTransitive details solution infos <|
                    getTransitive details solution newUnvisited newVisited
