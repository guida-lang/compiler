module Terminal.Uninstall exposing
    ( Args(..)
    , Flags(..)
    , run
    )

import Builder.BackgroundWriter as BW
import Builder.Deps.Registry as Registry
import Builder.Deps.Solver as Solver
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.Elm.Constraint as C
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import System.IO as IO exposing (IO)
import Utils.Main exposing (FilePath)



-- RUN


type Args
    = NoArgs
    | Uninstall Pkg.Name


type Flags
    = Flags Bool


run : Args -> Flags -> IO ()
run args (Flags autoYes) =
    Reporting.attempt Exit.uninstallToReport
        (Stuff.findRoot
            |> IO.bind
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            IO.pure (Err Exit.UninstallNoOutline)

                        Just root ->
                            case args of
                                NoArgs ->
                                    IO.pure (Err Exit.UninstallNoArgs)

                                Uninstall pkg ->
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
                                                                            |> Task.bind (\changes -> attemptChanges root env oldOutline V.toChars changes autoYes)

                                                                    Outline.Pkg outline ->
                                                                        makePkgPlan env pkg outline
                                                                            |> Task.bind (\changes -> attemptChanges root env oldOutline C.toChars changes autoYes)
                                                            )
                                                )
                                        )
                )
        )



-- ATTEMPT CHANGES


type Changes vsn
    = AlreadyUninstalled
    | DemoteTest Outline.Outline
    | DemoteIndirect Outline.Outline
    | Changes (Dict ( String, String ) Pkg.Name (Change vsn)) Outline.Outline


type alias Task a =
    Task.Task Exit.Uninstall a


attemptChanges : String -> Solver.Env -> Outline.Outline -> (a -> String) -> Changes a -> Bool -> Task ()
attemptChanges root env oldOutline toChars changes autoYes =
    case changes of
        AlreadyUninstalled ->
            Task.io (IO.putStrLn "It is already uninstalled!")

        DemoteIndirect newOutline ->
            attemptChangesHelp root env oldOutline newOutline autoYes <|
                D.vcat
                    [ D.fillSep
                        [ D.fromChars "I"
                        , D.fromChars "found"
                        , D.fromChars "it"
                        , D.fromChars "in"
                        , D.fromChars "your"
                        , D.fromChars "elm.json"
                        , D.fromChars "file,"
                        , D.fromChars "but"
                        , D.fromChars "in"
                        , D.fromChars "the"
                        , D.dullyellow (D.fromChars "\"indirect\"")
                        , D.fromChars "dependencies."
                        ]
                    , D.fillSep
                        [ D.fromChars "Should"
                        , D.fromChars "I"
                        , D.fromChars "move"
                        , D.fromChars "it"
                        , D.fromChars "into"
                        , D.green (D.fromChars "\"direct\"")
                        , D.fromChars "dependencies"
                        , D.fromChars "for"
                        , D.fromChars "more"
                        , D.fromChars "general"
                        , D.fromChars "use?"
                        , D.fromChars "[Y/n]: "
                        ]
                    ]

        DemoteTest newOutline ->
            attemptChangesHelp root env oldOutline newOutline autoYes <|
                D.vcat
                    [ D.fillSep
                        [ D.fromChars "I"
                        , D.fromChars "found"
                        , D.fromChars "it"
                        , D.fromChars "in"
                        , D.fromChars "your"
                        , D.fromChars "elm.json"
                        , D.fromChars "file,"
                        , D.fromChars "but"
                        , D.fromChars "in"
                        , D.fromChars "the"
                        , D.dullyellow (D.fromChars "\"test-dependencies\"")
                        , D.fromChars "field."
                        ]
                    , D.fillSep
                        [ D.fromChars "Should"
                        , D.fromChars "I"
                        , D.fromChars "move"
                        , D.fromChars "it"
                        , D.fromChars "into"
                        , D.green (D.fromChars "\"dependencies\"")
                        , D.fromChars "for"
                        , D.fromChars "more"
                        , D.fromChars "general"
                        , D.fromChars "use?"
                        , D.fromChars "[Y/n]: "
                        ]
                    ]

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
                    , D.fromChars "Would you like me to update your elm.json accordingly? [Y/n]: "
                    ]


attemptChangesHelp : FilePath -> Solver.Env -> Outline.Outline -> Outline.Outline -> Bool -> D.Doc -> Task ()
attemptChangesHelp root env oldOutline newOutline autoYes question =
    Task.eio Exit.UninstallBadDetails <|
        BW.withScope
            (\scope ->
                let
                    askQuestion : IO Bool
                    askQuestion =
                        if autoYes then
                            IO.pure True

                        else
                            Reporting.ask question
                in
                askQuestion
                    |> IO.bind
                        (\approved ->
                            if approved then
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

                            else
                                IO.putStrLn "Okay, I did not change anything!"
                                    |> IO.fmap (\_ -> Ok ())
                        )
            )



-- MAKE APP PLAN


makeAppPlan : Solver.Env -> Pkg.Name -> Outline.AppOutline -> Task (Changes V.Version)
makeAppPlan (Solver.Env cache _ connection registry) pkg (Outline.AppOutline elmVersion sourceDirs direct indirect testDirect testIndirect) =
    if Dict.member identity pkg direct then
        let
            constraints : Dict ( String, String ) Pkg.Name C.Constraint
            constraints =
                Dict.remove identity pkg direct
                    |> Dict.map (\_ -> C.exactly)
        in
        Task.io (Solver.verify cache connection registry constraints)
            |> Task.bind
                (\result ->
                    case result of
                        Solver.SolverOk details ->
                            let
                                solution : Dict ( String, String ) Pkg.Name V.Version
                                solution =
                                    Dict.map (\_ (Solver.Details vsn _) -> vsn) details

                                newDirect : Dict ( String, String ) Pkg.Name V.Version
                                newDirect =
                                    Dict.intersection compare solution constraints

                                newIndirect : Dict ( String, String ) Pkg.Name V.Version
                                newIndirect =
                                    Dict.diff solution constraints

                                old : Dict ( String, String ) Pkg.Name V.Version
                                old =
                                    Dict.union direct indirect

                                new : Dict ( String, String ) Pkg.Name V.Version
                                new =
                                    Dict.union (Dict.remove identity pkg direct) indirect
                            in
                            Task.pure (Changes (detectChanges old new) (Outline.App (Outline.AppOutline elmVersion sourceDirs newDirect newIndirect testDirect testIndirect)))

                        Solver.NoSolution ->
                            Task.throw (Exit.UninstallNoOnlineAppSolution pkg)

                        Solver.NoOfflineSolution ->
                            Task.throw (Exit.UninstallNoOfflineAppSolution pkg)

                        Solver.SolverErr exit ->
                            Task.throw (Exit.UninstallHadSolverTrouble exit)
                )

    else
        Task.pure AlreadyUninstalled



-- MAKE PACKAGE PLAN


makePkgPlan : Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Task (Changes C.Constraint)
makePkgPlan (Solver.Env cache _ connection registry) pkg (Outline.PkgOutline name summary license version exposed deps test elmVersion) =
    if Dict.member identity pkg deps then
        case Registry.getVersions_ pkg registry of
            Err suggestions ->
                case connection of
                    Solver.Online _ ->
                        Task.throw (Exit.UninstallUnknownPackageOnline pkg suggestions)

                    Solver.Offline ->
                        Task.throw (Exit.UninstallUnknownPackageOffline pkg suggestions)

            Ok (Registry.KnownVersions _ _) ->
                let
                    old : Dict ( String, String ) Pkg.Name C.Constraint
                    old =
                        Dict.union deps test

                    cons : Dict ( String, String ) Pkg.Name C.Constraint
                    cons =
                        Dict.remove identity pkg old
                in
                Task.io (Solver.verify cache connection registry cons)
                    |> Task.bind
                        (\result ->
                            case result of
                                Solver.SolverOk _ ->
                                    let
                                        new : Dict ( String, String ) Pkg.Name C.Constraint
                                        new =
                                            Dict.remove identity pkg old

                                        changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
                                        changes =
                                            detectChanges old new
                                    in
                                    Task.pure <|
                                        Changes changes <|
                                            Outline.Pkg <|
                                                Outline.PkgOutline name
                                                    summary
                                                    license
                                                    version
                                                    exposed
                                                    (Dict.remove identity pkg deps)
                                                    test
                                                    elmVersion

                                Solver.NoSolution ->
                                    Task.throw (Exit.UninstallNoOnlinePkgSolution pkg)

                                Solver.NoOfflineSolution ->
                                    Task.throw (Exit.UninstallNoOfflinePkgSolution pkg)

                                Solver.SolverErr exit ->
                                    Task.throw (Exit.UninstallHadSolverTrouble exit)
                        )

    else
        Task.pure AlreadyUninstalled



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
