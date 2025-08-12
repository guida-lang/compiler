module Terminal.Install exposing
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
import Builder.Stuff as Stuff
import Compiler.Elm.Constraint as C
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import Data.Map as Dict exposing (Dict)
import System.IO as IO
import Task exposing (Task)
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- RUN


type Args
    = NoArgs
    | Install Pkg.Name


type Flags
    = Flags Bool Bool


run : Args -> Flags -> Task Never ()
run args (Flags forTest autoYes) =
    Reporting.attempt Exit.installToReport
        (Stuff.findRoot
            |> Task.bind
                (\maybeRoot ->
                    case maybeRoot of
                        Nothing ->
                            Task.pure (Err Exit.InstallNoOutline)

                        Just root ->
                            case args of
                                NoArgs ->
                                    Stuff.getElmHome
                                        |> Task.fmap (\elmHome -> Err (Exit.InstallNoArgs elmHome))

                                Install pkg ->
                                    Task.run
                                        (Task.eio Exit.InstallBadRegistry Solver.initEnv
                                            |> Task.bind
                                                (\env ->
                                                    Task.eio Exit.InstallBadOutline (Outline.read root)
                                                        |> Task.bind
                                                            (\oldOutline ->
                                                                case oldOutline of
                                                                    Outline.App outline ->
                                                                        makeAppPlan env pkg outline forTest
                                                                            |> Task.bind (\changes -> attemptChanges root env oldOutline V.toChars changes autoYes)

                                                                    Outline.Pkg outline ->
                                                                        makePkgPlan env pkg outline forTest
                                                                            |> Task.bind (\changes -> attemptChanges root env oldOutline C.toChars changes autoYes)
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
    | Changes (Dict ( String, String ) Pkg.Name (Change vsn)) Outline.Outline


attemptChanges : String -> Solver.Env -> Outline.Outline -> (a -> String) -> Changes a -> Bool -> Task Exit.Install ()
attemptChanges root env oldOutline toChars changes autoYes =
    case changes of
        AlreadyInstalled ->
            Task.io (IO.putStrLn "It is already installed!")

        PromoteIndirect newOutline ->
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

        PromoteTest newOutline ->
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


attemptChangesHelp : FilePath -> Solver.Env -> Outline.Outline -> Outline.Outline -> Bool -> D.Doc -> Task Exit.Install ()
attemptChangesHelp root env oldOutline newOutline autoYes question =
    Task.eio Exit.InstallBadDetails <|
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


makeAppPlan : Solver.Env -> Pkg.Name -> Outline.AppOutline -> Bool -> Task Exit.Install (Changes V.Version)
makeAppPlan (Solver.Env cache _ connection registry) pkg ((Outline.AppOutline elmVersion sourceDirs direct indirect testDirect testIndirect) as outline) forTest =
    if forTest then
        if Dict.member identity pkg testDirect then
            Task.pure AlreadyInstalled

        else
            (-- is it already an indirect test dependency?
             case Dict.get identity pkg testIndirect of
                Just vsn ->
                    Task.pure <|
                        PromoteTest <|
                            Outline.App <|
                                Outline.AppOutline elmVersion
                                    sourceDirs
                                    direct
                                    indirect
                                    (Dict.insert identity pkg vsn testDirect)
                                    (Dict.remove identity pkg testIndirect)

                Nothing ->
                    -- finally try to add it from scratch
                    case Registry.getVersions_ pkg registry of
                        Err suggestions ->
                            case connection of
                                Solver.Online _ ->
                                    Task.throw (Exit.InstallUnknownPackageOnline pkg suggestions)

                                Solver.Offline ->
                                    Task.throw (Exit.InstallUnknownPackageOffline pkg suggestions)

                        Ok _ ->
                            Task.io (Solver.addToApp cache connection registry pkg outline forTest)
                                |> Task.bind
                                    (\result ->
                                        case result of
                                            Solver.SolverOk (Solver.AppSolution old new app) ->
                                                Task.pure (Changes (detectChanges old new) (Outline.App app))

                                            Solver.NoSolution ->
                                                Task.throw (Exit.InstallNoOnlineAppSolution pkg)

                                            Solver.NoOfflineSolution ->
                                                Task.throw (Exit.InstallNoOfflineAppSolution pkg)

                                            Solver.SolverErr exit ->
                                                Task.throw (Exit.InstallHadSolverTrouble exit)
                                    )
            )

    else if Dict.member identity pkg direct then
        Task.pure AlreadyInstalled

    else
        -- is it already indirect?
        case Dict.get identity pkg indirect of
            Just vsn ->
                Task.pure <|
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
                        Task.pure <|
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
                                Task.pure <|
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
                                                Task.throw (Exit.InstallUnknownPackageOnline pkg suggestions)

                                            Solver.Offline ->
                                                Task.throw (Exit.InstallUnknownPackageOffline pkg suggestions)

                                    Ok _ ->
                                        Task.io (Solver.addToApp cache connection registry pkg outline forTest)
                                            |> Task.bind
                                                (\result ->
                                                    case result of
                                                        Solver.SolverOk (Solver.AppSolution old new app) ->
                                                            Task.pure (Changes (detectChanges old new) (Outline.App app))

                                                        Solver.NoSolution ->
                                                            Task.throw (Exit.InstallNoOnlineAppSolution pkg)

                                                        Solver.NoOfflineSolution ->
                                                            Task.throw (Exit.InstallNoOfflineAppSolution pkg)

                                                        Solver.SolverErr exit ->
                                                            Task.throw (Exit.InstallHadSolverTrouble exit)
                                                )



-- MAKE PACKAGE PLAN


makePkgPlan : Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Bool -> Task Exit.Install (Changes C.Constraint)
makePkgPlan (Solver.Env cache _ connection registry) pkg (Outline.PkgOutline name summary license version exposed deps test elmVersion) forTest =
    if forTest then
        if Dict.member identity pkg test then
            Task.pure AlreadyInstalled

        else
            -- try to add a new dependency
            case Registry.getVersions_ pkg registry of
                Err suggestions ->
                    case connection of
                        Solver.Online _ ->
                            Task.throw (Exit.InstallUnknownPackageOnline pkg suggestions)

                        Solver.Offline ->
                            Task.throw (Exit.InstallUnknownPackageOffline pkg suggestions)

                Ok (Registry.KnownVersions _ _) ->
                    let
                        cons : Dict ( String, String ) Pkg.Name C.Constraint
                        cons =
                            Dict.insert identity pkg C.anything test
                    in
                    Task.io (Solver.verify cache connection registry cons)
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

                                            newTest : Dict ( String, String ) Pkg.Name C.Constraint
                                            newTest =
                                                Dict.insert identity pkg con test

                                            changes : Dict ( String, String ) Pkg.Name (Change C.Constraint)
                                            changes =
                                                detectChanges test newTest

                                            news : Dict ( String, String ) Pkg.Name C.Constraint
                                            news =
                                                Utils.mapMapMaybe identity Pkg.compareName keepNew changes
                                        in
                                        Task.pure <|
                                            Changes changes <|
                                                Outline.Pkg <|
                                                    Outline.PkgOutline name
                                                        summary
                                                        license
                                                        version
                                                        exposed
                                                        deps
                                                        (addNews (Just pkg) news test)
                                                        elmVersion

                                    Solver.NoSolution ->
                                        Task.throw (Exit.InstallNoOnlinePkgSolution pkg)

                                    Solver.NoOfflineSolution ->
                                        Task.throw (Exit.InstallNoOfflinePkgSolution pkg)

                                    Solver.SolverErr exit ->
                                        Task.throw (Exit.InstallHadSolverTrouble exit)
                            )

    else if Dict.member identity pkg deps then
        Task.pure AlreadyInstalled

    else
        -- is already in test dependencies?
        case Dict.get identity pkg test of
            Just con ->
                Task.pure <|
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
                                Task.throw (Exit.InstallUnknownPackageOnline pkg suggestions)

                            Solver.Offline ->
                                Task.throw (Exit.InstallUnknownPackageOffline pkg suggestions)

                    Ok (Registry.KnownVersions _ _) ->
                        let
                            old : Dict ( String, String ) Pkg.Name C.Constraint
                            old =
                                Dict.union deps test

                            cons : Dict ( String, String ) Pkg.Name C.Constraint
                            cons =
                                Dict.insert identity pkg C.anything old
                        in
                        Task.io (Solver.verify cache connection registry cons)
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
                                                Changes changes <|
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
                                            Task.throw (Exit.InstallNoOnlinePkgSolution pkg)

                                        Solver.NoOfflineSolution ->
                                            Task.throw (Exit.InstallNoOfflinePkgSolution pkg)

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
