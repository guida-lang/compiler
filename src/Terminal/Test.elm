module Terminal.Test exposing (run)

import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Task as Task
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Version as V
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Data.Map as Dict
import System.IO as IO exposing (IO)
import Utils.Main as Utils exposing (FilePath)



-- RUN


run : List String -> () -> IO ()
run paths flags =
    Reporting.terminal
        |> IO.bind
            (\style ->
                Stuff.findRoot
                    |> IO.bind
                        (\maybeRoot ->
                            Reporting.attemptWithStyle style Exit.installToReport <|
                                case maybeRoot of
                                    Just root ->
                                        runHelp root paths style flags

                                    Nothing ->
                                        IO.pure (Err Exit.InstallNoOutline)
                        )
            )


runHelp : String -> List String -> Reporting.Style -> () -> IO (Result Exit.Install ())
runHelp root testFileGlobs style () =
    Task.run
        (Task.eio Exit.InstallBadOutline (Outline.read root)
            |> Task.bind
                (\rootOutline ->
                    resolveElmFiles
                        (case testFileGlobs of
                            [] ->
                                [ root ++ "/tests" ]

                            _ ->
                                testFileGlobs
                        )
                        |> IO.bind
                            (\testFilePaths ->
                                let
                                    testModules =
                                        Debug.todo "testModules"

                                    ( directs, indirects ) =
                                        -- TODO this is incorrect, needs to be fixed
                                        case rootOutline of
                                            Outline.App (Outline.AppOutline _ _ _ _ testDirect testIndirect) ->
                                                ( testDirect, testIndirect )

                                            Outline.Pkg _ ->
                                                ( Dict.empty, Dict.empty )

                                    dir =
                                        Stuff.testDir root
                                in
                                Utils.dirCreateDirectoryIfMissing True dir
                                    |> IO.bind
                                        (\_ ->
                                            Outline.write dir <|
                                                Outline.App <|
                                                    Outline.AppOutline V.elmCompiler (NE.Nonempty (Outline.RelativeSrcDir "src") []) directs indirects Dict.empty Dict.empty
                                        )
                                    |> IO.bind
                                        (\_ ->
                                            let
                                                _ =
                                                    Debug.log "rootOutline" ( rootOutline, testFilePaths )
                                            in
                                            Debug.todo "runHelp"
                                        )
                            )
                        |> Task.io
                )
        )


extractExposedPossiblyTests : String -> IO (Maybe ( String, List String ))
extractExposedPossiblyTests path =
    File.readUtf8 path
        |> IO.bind
            (\bytes ->
                case Parse.fromByteString (SV.fileSyntaxVersion path) Parse.Application bytes of
                    Ok (Src.Module (Just (A.At _ name)) (A.At _ exposing_) _ _ _ _ _ _ _) ->
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
