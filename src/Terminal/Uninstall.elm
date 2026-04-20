module Terminal.Uninstall exposing
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
import Compiler.Guida.Constraint as C
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Utils.System.IO as IO
import Utils.Task.Extra as Task



-- RUN


type Args
    = NoArgs
    | Uninstall Pkg.Name


type Flags
    = Flags Bool


run : Args -> Flags -> Task Exit.ExitCode ()
run args (Flags autoYes) =
    Reporting.attempt Exit.uninstallToReport <|
        (Stuff.findRoot
            |> Task.andThen
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            Task.succeed (Err Exit.UninstallNoOutline)

                        Just root ->
                            case args of
                                NoArgs ->
                                    Task.succeed (Err Exit.UninstallNoArgs)

                                Uninstall pkg ->
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
                                                                            |> Task.andThen (\changes -> attemptChanges root env oldOutline V.toChars changes autoYes)

                                                                    Outline.Pkg outline ->
                                                                        makePkgPlan pkg outline
                                                                            |> Task.andThen (\changes -> attemptChanges root env oldOutline C.toChars changes autoYes)
                                                            )
                                                )
                                        )
                )
        )



-- ATTEMPT CHANGES


type Changes vsn
    = AlreadyNotPresent
    | Changes (Dict ( String, String ) Pkg.Name (Change vsn)) Outline.Outline


attemptChanges : Stuff.Root -> Solver.Env -> Outline.Outline -> (a -> String) -> Changes a -> Bool -> Task Exit.Uninstall ()
attemptChanges root env oldOutline toChars changes autoYes =
    case changes of
        AlreadyNotPresent ->
            Task.io (IO.putStrLn "It is not currently installed!")

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


attemptChangesHelp : Stuff.Root -> Solver.Env -> Outline.Outline -> Outline.Outline -> Bool -> D.Doc -> Task Exit.Uninstall ()
attemptChangesHelp root env oldOutline newOutline autoYes question =
    Task.eio Exit.UninstallBadDetails <|
        BW.withScope
            (\scope ->
                let
                    askQuestion : Task Never Bool
                    askQuestion =
                        if autoYes then
                            Task.succeed True

                        else
                            Reporting.ask question
                in
                askQuestion
                    |> Task.andThen
                        (\approved ->
                            if approved then
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

                            else
                                IO.putStrLn "Okay, I did not change anything!"
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
                                    Solver.SolverOk (Solver.AppSolution old new app) ->
                                        Task.succeed (Changes (detectChanges old new) (Outline.App app))

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
                                    Solver.SolverOk (Solver.AppSolution old new app) ->
                                        Task.succeed (Changes (detectChanges old new) (Outline.App app))

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
        Outline.GuidaPkgOutline name summary license version exposed deps test guidaVersion ->
            let
                old : Dict ( String, String ) Pkg.Name C.Constraint
                old =
                    Dict.union deps test
            in
            if Dict.member identity pkg old then
                let
                    new : Dict ( String, String ) Pkg.Name C.Constraint
                    new =
                        Dict.remove identity pkg old

                    changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
                    changes =
                        detectChanges old new
                in
                Task.succeed <|
                    Changes changes <|
                        Outline.Pkg <|
                            Outline.GuidaPkgOutline name
                                summary
                                license
                                version
                                exposed
                                (Dict.remove identity pkg deps)
                                (Dict.remove identity pkg test)
                                guidaVersion

            else
                Task.succeed AlreadyNotPresent

        Outline.ElmPkgOutline name summary license version exposed deps test elmVersion ->
            let
                old : Dict ( String, String ) Pkg.Name C.Constraint
                old =
                    Dict.union deps test
            in
            if Dict.member identity pkg old then
                let
                    new : Dict ( String, String ) Pkg.Name C.Constraint
                    new =
                        Dict.remove identity pkg old

                    changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
                    changes =
                        detectChanges old new
                in
                Task.succeed <|
                    Changes changes <|
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
