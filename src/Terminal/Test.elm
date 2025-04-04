module Terminal.Test exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import System.IO as IO exposing (IO)
import Utils.Main as Utils exposing (FilePath)



-- RUN


type alias Task a =
    Task.Task Exit.Make a


run : List String -> () -> IO ()
run paths flags =
    getStyle
        |> IO.bind
            (\style ->
                Stuff.findRoot
                    |> IO.bind
                        (\maybeRoot ->
                            Reporting.attemptWithStyle style Exit.makeToReport <|
                                case maybeRoot of
                                    Just root ->
                                        runHelp root paths style flags

                                    Nothing ->
                                        IO.pure (Err Exit.MakeNoOutline)
                        )
            )


runHelp : String -> List String -> Reporting.Style -> () -> IO (Result Exit.Make ())
runHelp root testFileGlobs style () =
    BW.withScope
        (\scope ->
            Stuff.withRootLock root <|
                Task.run <|
                    (Task.eio Exit.MakeBadDetails (Details.load style scope root)
                        |> Task.bind
                            (\details ->
                                -- TODO: we might want to consider wrapping the code into a task that creates and removes the folder for tests
                                -- Utils.bracket_
                                --     (Utils.dirCreateDirectoryIfMissing True dir)
                                --     (Utils.dirRemoveDirectoryRecursive dir)
                                --     (Task.run (callback dir))
                                Utils.dirCreateDirectoryIfMissing True (Stuff.testDir root)
                                    |> Task.io
                                    |> Task.bind
                                        (\_ ->
                                            let
                                                _ =
                                                    -- FIXME change these details to make it for tests
                                                    Debug.log "details" (String.left 2000 (Debug.toString details))
                                            in
                                            case Debug.log "testFileGlobs" testFileGlobs of
                                                [] ->
                                                    getExposed details
                                                        |> Task.bind
                                                            (\exposed ->
                                                                Task.eio
                                                                    (\_ ->
                                                                        -- Exit.InstallBadOutline
                                                                        Exit.MakeNoOutline
                                                                    )
                                                                    (Outline.read root)
                                                                    |> Task.bind
                                                                        (\oldOutline ->
                                                                            Utils.dirDoesDirectoryExist "tests"
                                                                                |> IO.bind
                                                                                    (\testsDirExists ->
                                                                                        let
                                                                                            newOutline =
                                                                                                case oldOutline of
                                                                                                    Outline.App (Outline.AppOutline elmVersion srcDirs depsDirect depsTrans testDirect testTrans) ->
                                                                                                        let
                                                                                                            addOptionalTests =
                                                                                                                if testsDirExists then
                                                                                                                    NE.cons (Outline.RelativeSrcDir "tests")

                                                                                                                else
                                                                                                                    identity

                                                                                                            newSrcDirs =
                                                                                                                srcDirs
                                                                                                                    -- TODO/FIXME we shouldn't need to install elm-test...
                                                                                                                    |> NE.cons (Outline.RelativeSrcDir "node_modules/elm-test/elm/src")
                                                                                                                    |> addOptionalTests
                                                                                                                    |> NE.map
                                                                                                                        (\srcDir ->
                                                                                                                            case srcDir of
                                                                                                                                Outline.AbsoluteSrcDir _ ->
                                                                                                                                    srcDir

                                                                                                                                Outline.RelativeSrcDir path ->
                                                                                                                                    Outline.RelativeSrcDir ("../../" ++ path)
                                                                                                                        )
                                                                                                                    |> NE.cons (Outline.RelativeSrcDir "src")
                                                                                                        in
                                                                                                        Outline.App (Outline.AppOutline elmVersion newSrcDirs depsDirect depsTrans testDirect testTrans)

                                                                                                    Outline.Pkg _ ->
                                                                                                        oldOutline
                                                                                        in
                                                                                        -- TODO
                                                                                        -- // Note: These are the dependencies listed in `elm/elm.json`, except
                                                                                        -- // `elm-explorations/test`. `elm/elm.json` is only used during development of
                                                                                        -- // this CLI (for editor integrations and unit tests). When running `elm-test`
                                                                                        -- // we add the `elm/` folder in the npm package as a source directory. The
                                                                                        -- // dependencies listed here and the ones in `elm/elm.json` need to be in sync.
                                                                                        -- const extra = {
                                                                                        --     'elm/core': '1.0.0 <= v < 2.0.0',
                                                                                        --     'elm/json': '1.0.0 <= v < 2.0.0',
                                                                                        --     'elm/time': '1.0.0 <= v < 2.0.0',
                                                                                        --     'elm/random': '1.0.0 <= v < 2.0.0',
                                                                                        -- };
                                                                                        Outline.write (Stuff.testDir root) newOutline
                                                                                    )
                                                                                |> Task.io
                                                                        )
                                                                    |> Task.bind
                                                                        (\_ ->
                                                                            buildExposed style root details Nothing exposed
                                                                        )
                                                            )

                                                p :: ps ->
                                                    buildPaths style root details (NE.Nonempty p ps)
                                                        |> Task.bind
                                                            (\artifacts ->
                                                                Task.eio
                                                                    (\_ ->
                                                                        -- Exit.InstallBadOutline
                                                                        Exit.MakeNoOutline
                                                                    )
                                                                    (Outline.read root)
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
                                                             -- case maybeOutput of
                                                             --     Nothing ->
                                                             --         case getMains artifacts of
                                                             --             [] ->
                                                             --                 Task.pure ()
                                                             --             [ name ] ->
                                                             --                 toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                             --                     |> Task.bind
                                                             --                         (\builder ->
                                                             --                             generate style "index.html" (Html.sandwich name builder) (NE.Nonempty name [])
                                                             --                         )
                                                             --             name :: names ->
                                                             --                 toBuilder withSourceMaps 0 root details desiredMode artifacts
                                                             --                     |> Task.bind
                                                             --                         (\builder ->
                                                             --                             generate style "elm.js" builder (NE.Nonempty name names)
                                                             --                         )
                                                             --     Just DevNull ->
                                                             --         Task.pure ()
                                                             --     Just (JS target) ->
                                                             --         case getNoMains artifacts of
                                                             --             [] ->
                                                             --                 toBuilder withSourceMaps 0 root details desiredMode artifacts
                                                             --                     |> Task.bind
                                                             --                         (\builder ->
                                                             --                             generate style target builder (Build.getRootNames artifacts)
                                                             --                         )
                                                             --             name :: names ->
                                                             --                 Task.throw (Exit.MakeNonMainFilesIntoJavaScript name names)
                                                             --     Just (Html target) ->
                                                             --         hasOneMain artifacts
                                                             --             |> Task.bind
                                                             --                 (\name ->
                                                             --                     toBuilder withSourceMaps Html.leadingLines root details desiredMode artifacts
                                                             --                         |> Task.bind
                                                             --                             (\builder ->
                                                             --                                 generate style target (Html.sandwich name builder) (NE.Nonempty name [])
                                                             --                             )
                                                             --                 )
                                                            )
                                        )
                            )
                    )
        )



-- GET INFORMATION


getStyle : IO Reporting.Style
getStyle =
    Reporting.terminal


getExposed : Details.Details -> Task (NE.Nonempty ModuleName.Raw)
getExposed (Details.Details _ validOutline _ _ _ _) =
    case Debug.log "validOutline" validOutline of
        Details.ValidApp _ ->
            -- Task.throw Exit.MakeAppNeedsFileNames
            Task.pure (NE.Nonempty "tests" [])

        Details.ValidPkg _ exposed _ ->
            case exposed of
                [] ->
                    Task.throw Exit.MakePkgNeedsExposing

                m :: ms ->
                    Task.pure (NE.Nonempty m ms)


extractExposedPossiblyTests : String -> IO (Maybe ( String, List String ))
extractExposedPossiblyTests path =
    File.readUtf8 path
        |> IO.bind
            (\bytes ->
                case Parse.fromByteString (SV.fileSyntaxVersion path) Parse.Application bytes of
                    Ok (Src.Module _ (Just (A.At _ name)) (A.At _ exposing_) _ _ _ _ _ _ _) ->
                        let
                            _ =
                                case exposing_ of
                                    Src.Open ->
                                        Debug.todo "Open"

                                    Src.Explicit exposedList ->
                                        Debug.todo "Explicit"
                        in
                        IO.pure (Just ( name, [] ))

                    _ ->
                        IO.pure Nothing
            )



-- BUILD PROJECTS


buildExposed : Reporting.Style -> FilePath -> Details.Details -> Maybe FilePath -> NE.Nonempty ModuleName.Raw -> Task ()
buildExposed style root details maybeDocs exposed =
    let
        docsGoal : Build.DocsGoal ()
        docsGoal =
            Maybe.unwrap Build.ignoreDocs Build.writeDocs maybeDocs
                |> Debug.log "docsGoal"
    in
    Task.eio Exit.MakeCannotBuild <|
        Build.fromExposed (Decode.succeed ()) (\_ -> Encode.object []) style root details docsGoal exposed


buildPaths : Reporting.Style -> FilePath -> Details.Details -> NE.Nonempty FilePath -> Task Build.Artifacts
buildPaths style root details paths =
    Task.eio Exit.MakeCannotBuild <|
        Build.fromPaths style root details paths



-- COMMAND LINE


type FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


stat : FilePath -> IO FileType
stat path =
    Utils.dirDoesFileExist path
        |> IO.bind
            (\isFile ->
                Utils.dirDoesDirectoryExist path
                    |> IO.fmap
                        (\isDirectory ->
                            case ( isFile, isDirectory ) of
                                ( True, _ ) ->
                                    IsFile

                                ( _, True ) ->
                                    IsDirectory

                                ( False, False ) ->
                                    DoesNotExist
                        )
            )



-- RESOLVE FILES


type Error
    = FileDoesNotExist FilePath
    | NoElmFiles FilePath


resolveFile : FilePath -> IO (Result Error (List FilePath))
resolveFile path =
    stat path
        |> IO.bind
            (\fileType ->
                case fileType of
                    IsFile ->
                        IO.pure (Ok [ path ])

                    IsDirectory ->
                        findAllElmFiles path
                            |> IO.fmap
                                (\elmFiles ->
                                    case elmFiles of
                                        [] ->
                                            Err (NoElmFiles path)

                                        _ ->
                                            Ok elmFiles
                                )

                    DoesNotExist ->
                        IO.pure (Err (FileDoesNotExist path))
            )


resolveElmFiles : List FilePath -> IO (Result (List Error) (List FilePath))
resolveElmFiles inputFiles =
    IO.mapM resolveFile inputFiles
        |> IO.fmap collectErrors
        |> IO.fmap
            (\result ->
                case result of
                    Err ls ->
                        Err ls

                    Ok files ->
                        Ok (List.concat files)
            )


collectErrors : List (Result e v) -> Result (List e) (List v)
collectErrors =
    List.foldl
        (\next acc ->
            case ( next, acc ) of
                ( Err e, Ok _ ) ->
                    Err [ e ]

                ( Err e, Err es ) ->
                    Err (e :: es)

                ( Ok v, Ok vs ) ->
                    Ok (v :: vs)

                ( Ok _, Err es ) ->
                    Err es
        )
        (Ok [])



-- FILESYSTEM


collectFiles : (a -> IO (List a)) -> a -> IO (List a)
collectFiles children root =
    children root
        |> IO.bind (\xs -> IO.mapM (collectFiles children) xs)
        |> IO.fmap (\subChildren -> root :: List.concat subChildren)


listDir : FilePath -> IO (List FilePath)
listDir path =
    Utils.dirListDirectory path
        |> IO.fmap (List.map (\file -> path ++ "/" ++ file))


fileList : FilePath -> IO (List FilePath)
fileList =
    let
        children : FilePath -> IO (List FilePath)
        children path =
            if isSkippable path then
                IO.pure []

            else
                Utils.dirDoesDirectoryExist path
                    |> IO.bind
                        (\directory ->
                            if directory then
                                listDir path

                            else
                                IO.pure []
                        )
    in
    collectFiles children


isSkippable : FilePath -> Bool
isSkippable path =
    List.any identity
        [ hasFilename "elm-stuff" path
        , hasFilename "node_modules" path
        , hasFilename ".git" path
        ]


hasExtension : String -> FilePath -> Bool
hasExtension ext path =
    ext == Utils.fpTakeExtension path


findAllElmFiles : FilePath -> IO (List FilePath)
findAllElmFiles inputFile =
    fileList inputFile
        |> IO.fmap (List.filter (hasExtension ".elm"))


hasFilename : String -> FilePath -> Bool
hasFilename name path =
    name == Utils.fpTakeFileName path


{-| FROM INSTALL!!!
-}



-- ATTEMPT CHANGES


attemptChanges : String -> Solver.Env -> Outline.Outline -> (a -> String) -> Changes a -> Task ()
attemptChanges root env oldOutline toChars changes =
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


attemptChangesHelp : FilePath -> Solver.Env -> Outline.Outline -> Task (Result Exit.Details ())
attemptChangesHelp root env newOutline =
    Task.eio Exit.InstallBadDetails <|
        BW.withScope
            (\scope ->
                Outline.write root newOutline
                    |> IO.bind (\_ -> Details.verifyInstall scope root env newOutline)
            )



-- MAKE APP PLAN


makeAppPlan : Solver.Env -> Pkg.Name -> Outline.AppOutline -> Bool -> Task (Changes V.Version)
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


makePkgPlan : Solver.Env -> Pkg.Name -> Outline.PkgOutline -> Bool -> Task (Changes C.Constraint)
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
