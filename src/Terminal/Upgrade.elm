module Terminal.Upgrade exposing
    ( Args(..)
    , Flags(..)
    , run
    )

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
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import System.IO as IO
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- RUN


type Args
    = NoArgs


type Flags
    = Flags Bool


run : Args -> Flags -> Task Never ()
run _ (Flags autoYes) =
    Reporting.attempt Exit.upgradeToReport <|
        (Stuff.findRoot
            |> Task.bind
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            Task.pure (Err Exit.UpgradeNoOutline)

                        Just root ->
                            Task.run
                                (Task.eio Exit.UpgradeBadRegistry Solver.initEnv
                                    |> Task.bind
                                        (\env ->
                                            Task.eio Exit.UpgradeBadOutline (Outline.read root)
                                                |> Task.bind
                                                    (\oldOutline ->
                                                        case oldOutline of
                                                            Outline.App outline ->
                                                                makeAppPlan root env outline
                                                                    |> Task.bind (\changes -> attemptChanges root env oldOutline V.toChars changes autoYes)

                                                            Outline.Pkg outline ->
                                                                makePkgPlan root env outline
                                                                    |> Task.bind (\changes -> attemptChanges root env oldOutline C.toChars changes autoYes)
                                                    )
                                        )
                                )
                )
        )



-- ATTEMPT CHANGES


type Changes vsn
    = AlreadyUpgraded
    | Changes (Dict ( String, String ) Pkg.Name (Change vsn)) Outline.Outline


attemptChanges : Stuff.Root -> Solver.Env -> Outline.Outline -> (a -> String) -> Changes a -> Bool -> Task Exit.Upgrade ()
attemptChanges root env oldOutline toChars changes autoYes =
    case changes of
        AlreadyUpgraded ->
            Task.io (IO.putStrLn "Everything is already up to date!")

        Changes changeDict newOutline ->
            let
                widths : Widths
                widths =
                    Dict.foldr compare (widen toChars) (Widths 0 0 0) changeDict

                changeDocs : ChangeDocs
                changeDocs =
                    Dict.foldr compare (addChange toChars widths) (Docs [] [] []) changeDict
            in
            attemptChangesHelp root env oldOutline newOutline autoYes <|
                D.vcat
                    [ D.fromChars "Here is my plan:"
                    , viewChangeDocs changeDocs
                    , D.fromChars ""
                    , D.fromChars ("Would you like me to update your " ++ Stuff.rootFilename root ++ " accordingly? [Y/n]: ")
                    ]


attemptChangesHelp : Stuff.Root -> Solver.Env -> Outline.Outline -> Outline.Outline -> Bool -> D.Doc -> Task Exit.Upgrade ()
attemptChangesHelp root env oldOutline newOutline autoYes question =
    Task.eio Exit.UpgradeBadDetails <|
        BW.withScope
            (\scope ->
                let
                    askQuestion : Task Never Bool
                    askQuestion =
                        if autoYes then
                            Task.pure True

                        else
                            Reporting.ask question
                in
                askQuestion
                    |> Task.bind
                        (\approved ->
                            if approved then
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

                            else
                                IO.putStrLn "Okay, I did not change anything!"
                                    |> Task.fmap (\_ -> Ok ())
                        )
            )



-- MAKE APP PLAN


makeAppPlan : Stuff.Root -> Solver.Env -> Outline.AppOutline -> Task Exit.Upgrade (Changes V.Version)
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
                |> Task.bind
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

                                    changes : Dict ( String, String ) Pkg.Name (Change V.Version)
                                    changes =
                                        detectChanges oldAll newAll
                                in
                                if Dict.isEmpty changes then
                                    Task.pure AlreadyUpgraded

                                else
                                    Task.pure <|
                                        Changes changes <|
                                            Outline.App <|
                                                Outline.GuidaAppOutline guidaVersion sourceDirs newDirect newIndirect newTestDirect newTestIndirect

                            Solver.NoSolution ->
                                Task.throw Exit.UpgradeGuidaNoOnlineSolution

                            Solver.NoOfflineSolution ->
                                Task.throw (Exit.UpgradeGuidaNoOfflineSolution (Stuff.rootPath root))

                            Solver.SolverErr exit ->
                                Task.throw (Exit.UpgradeHadSolverTrouble exit)
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
                |> Task.bind
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

                                    changes : Dict ( String, String ) Pkg.Name (Change V.Version)
                                    changes =
                                        detectChanges oldAll newAll
                                in
                                if Dict.isEmpty changes then
                                    Task.pure AlreadyUpgraded

                                else
                                    Task.pure <|
                                        Changes changes <|
                                            Outline.App <|
                                                Outline.ElmAppOutline elmVersion sourceDirs newDirect newIndirect newTestDirect newTestIndirect

                            Solver.NoSolution ->
                                Task.throw Exit.UpgradeElmNoOnlineSolution

                            Solver.NoOfflineSolution ->
                                Task.throw (Exit.UpgradeElmNoOfflineSolution (Stuff.rootPath root))

                            Solver.SolverErr exit ->
                                Task.throw (Exit.UpgradeHadSolverTrouble exit)
                    )



-- MAKE PACKAGE PLAN


makePkgPlan : Stuff.Root -> Solver.Env -> Outline.PkgOutline -> Task Exit.Upgrade (Changes C.Constraint)
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
                    Task.throw (Exit.UpgradeCannotBuildSafeConstraint pkg)

                Ok constraints ->
                    Task.io (Solver.verify Target.GuidaTarget cache connection registry constraints)
                        |> Task.bind
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

                                            changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
                                            changes =
                                                detectChanges oldAll newAll
                                        in
                                        if Dict.isEmpty changes then
                                            Task.pure AlreadyUpgraded

                                        else
                                            Task.pure <|
                                                Changes changes <|
                                                    Outline.Pkg <|
                                                        Outline.GuidaPkgOutline name summary license version exposed newDeps newTest guidaVersion

                                    Solver.NoSolution ->
                                        Task.throw Exit.UpgradeGuidaNoOnlineSolution

                                    Solver.NoOfflineSolution ->
                                        Task.throw (Exit.UpgradeGuidaNoOfflineSolution (Stuff.rootPath root))

                                    Solver.SolverErr exit ->
                                        Task.throw (Exit.UpgradeHadSolverTrouble exit)
                            )

        Outline.ElmPkgOutline name summary license version exposed deps test elmVersion ->
            let
                oldAll : Dict ( String, String ) Pkg.Name C.Constraint
                oldAll =
                    Dict.union deps test
            in
            case toSafeConstraints oldAll of
                Err pkg ->
                    Task.throw (Exit.UpgradeCannotBuildSafeConstraint pkg)

                Ok constraints ->
                    Task.io (Solver.verify Target.ElmTarget cache connection registry constraints)
                        |> Task.bind
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

                                            changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
                                            changes =
                                                detectChanges oldAll newAll
                                        in
                                        if Dict.isEmpty changes then
                                            Task.pure AlreadyUpgraded

                                        else
                                            Task.pure <|
                                                Changes changes <|
                                                    Outline.Pkg <|
                                                        Outline.ElmPkgOutline name summary license version exposed newDeps newTest elmVersion

                                    Solver.NoSolution ->
                                        Task.throw Exit.UpgradeElmNoOnlineSolution

                                    Solver.NoOfflineSolution ->
                                        Task.throw (Exit.UpgradeElmNoOfflineSolution (Stuff.rootPath root))

                                    Solver.SolverErr exit ->
                                        Task.throw (Exit.UpgradeHadSolverTrouble exit)
                            )



-- HELPERS


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



-- VIEW CHANGE DOCS


type ChangeDocs
    = Docs (List D.Doc) (List D.Doc) (List D.Doc)


viewChangeDocs : ChangeDocs -> D.Doc
viewChangeDocs (Docs inserts changes removes) =
    D.indent 2 <|
        D.vcat <|
            List.concat <|
                [ viewNonZero "Add:" inserts
                , viewNonZero "Change:" changes
                , viewNonZero "Remove:" removes
                ]


viewNonZero : String -> List D.Doc -> List D.Doc
viewNonZero title entries =
    if List.isEmpty entries then
        []

    else
        [ D.fromChars ""
        , D.fromChars title
        , D.indent 2 (D.vcat entries)
        ]



-- VIEW CHANGE


addChange : (a -> String) -> Widths -> Pkg.Name -> Change a -> ChangeDocs -> ChangeDocs
addChange toChars widths name change (Docs inserts changes removes) =
    case change of
        Insert new ->
            Docs (viewInsert toChars widths name new :: inserts) changes removes

        Change old new ->
            Docs inserts (viewChange toChars widths name old new :: changes) removes

        Remove old ->
            Docs inserts changes (viewRemove toChars widths name old :: removes)


viewInsert : (a -> String) -> Widths -> Pkg.Name -> a -> D.Doc
viewInsert toChars (Widths nameWidth leftWidth _) name new =
    viewName nameWidth name
        |> D.plus (pad leftWidth (toChars new))


viewChange : (a -> String) -> Widths -> Pkg.Name -> a -> a -> D.Doc
viewChange toChars (Widths nameWidth leftWidth rightWidth) name old new =
    D.hsep
        [ viewName nameWidth name
        , pad leftWidth (toChars old)
        , D.fromChars "=>"
        , pad rightWidth (toChars new)
        ]


viewRemove : (a -> String) -> Widths -> Pkg.Name -> a -> D.Doc
viewRemove toChars (Widths nameWidth leftWidth _) name old =
    viewName nameWidth name
        |> D.plus (pad leftWidth (toChars old))


viewName : Int -> Pkg.Name -> D.Doc
viewName width name =
    D.fill (width + 3) (D.fromPackage name)


pad : Int -> String -> D.Doc
pad width string =
    D.fromChars (String.repeat (width - String.length string) " ")
        |> D.a (D.fromChars string)



-- WIDTHS


type Widths
    = Widths Int Int Int


widen : (a -> String) -> Pkg.Name -> Change a -> Widths -> Widths
widen toChars pkg change (Widths name left right) =
    let
        toLength : a -> Int
        toLength a =
            String.length (toChars a)

        newName : Int
        newName =
            max name (String.length (Pkg.toChars pkg))
    in
    case change of
        Insert new ->
            Widths newName (max left (toLength new)) right

        Change old new ->
            Widths newName (max left (toLength old)) (max right (toLength new))

        Remove old ->
            Widths newName (max left (toLength old)) right
