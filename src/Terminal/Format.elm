module Terminal.Format exposing
    ( Flags(..)
    , run
    )

import Builder.File as File
import Common.Format
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Module as M
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Json.Encode as Encode
import Result.Extra as Result
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- RUN


type Flags
    = Flags (Maybe FilePath) Bool Bool Bool


run : List String -> Flags -> Task Never ()
run paths ((Flags _ autoYes _ _) as flags) =
    resolveElmFiles paths
        |> Task.bind
            (\resolvedInputFiles ->
                case determineWhatToDoFromConfig flags resolvedInputFiles of
                    Err err ->
                        IO.hPutStrLn IO.stderr (toConsoleErrorMessage err)
                            |> Task.bind (\_ -> Exit.exitFailure)

                    Ok a ->
                        Task.pure a
            )
        |> Task.bind (\whatToDo -> doIt autoYes whatToDo)
        |> Task.bind
            (\result ->
                if result then
                    Task.pure ()

                else
                    Exit.exitFailure
            )


type WhatToDo
    = Format TransformMode
    | Validate ValidateMode


type Source
    = Stdin
    | FromFiles FilePath (List FilePath)


type Destination
    = InPlace
    | ToFile FilePath


type Mode
    = FormatMode
    | ValidateMode


determineSource : Bool -> Result (List Error) (List FilePath) -> Result ErrorMessage Source
determineSource stdin inputFiles =
    case ( stdin, inputFiles ) of
        ( _, Err fileErrors ) ->
            Err (BadInputFiles fileErrors)

        ( True, Ok [] ) ->
            Ok Stdin

        ( False, Ok [] ) ->
            Err NoInputs

        ( False, Ok (first :: rest) ) ->
            Ok (FromFiles first rest)

        ( True, Ok (_ :: _) ) ->
            Err TooManyInputs


determineDestination : Maybe FilePath -> Result ErrorMessage Destination
determineDestination output =
    case output of
        Just path ->
            Ok (ToFile path)

        Nothing ->
            Ok InPlace


determineMode : Bool -> Mode
determineMode doValidate =
    if doValidate then
        ValidateMode

    else
        FormatMode


determineWhatToDo : Source -> Destination -> Mode -> Result ErrorMessage WhatToDo
determineWhatToDo source destination mode =
    case ( mode, source, destination ) of
        ( ValidateMode, _, ToFile _ ) ->
            Err OutputAndValidate

        ( ValidateMode, Stdin, _ ) ->
            Ok (Validate ValidateStdin)

        ( ValidateMode, FromFiles first rest, _ ) ->
            Ok (Validate (ValidateFiles first rest))

        ( FormatMode, Stdin, InPlace ) ->
            Ok (Format StdinToStdout)

        ( FormatMode, Stdin, ToFile output ) ->
            Ok (Format (StdinToFile output))

        ( FormatMode, FromFiles first [], ToFile output ) ->
            Ok (Format (FileToFile first output))

        ( FormatMode, FromFiles first rest, InPlace ) ->
            Ok (Format (FilesInPlace first rest))

        ( _, FromFiles _ _, ToFile _ ) ->
            Err SingleOutputWithMultipleInputs


determineWhatToDoFromConfig : Flags -> Result (List Error) (List FilePath) -> Result ErrorMessage WhatToDo
determineWhatToDoFromConfig (Flags maybeOutput _ doValidate stdin) resolvedInputFiles =
    determineSource stdin resolvedInputFiles
        |> Result.andThen
            (\source ->
                determineDestination maybeOutput
                    |> Result.andThen
                        (\destination ->
                            determineWhatToDo source destination (determineMode doValidate)
                        )
            )


validate : ( FilePath, String ) -> Result InfoMessage ()
validate (( inputFile, inputText ) as input) =
    case format input of
        Ok modu ->
            if inputText /= modu then
                Err (FileWouldChange inputFile)

            else
                Ok ()

        Err err ->
            Err err


format : ( FilePath, String ) -> Result InfoMessage String
format ( inputFile, inputText ) =
    -- FIXME fix hardcoded projectType
    Common.Format.format (SV.fileSyntaxVersion inputFile) (M.Package Pkg.core) inputText
        |> Result.mapError
            (\_ ->
                -- FIXME show errors!
                -- let
                --     _ =
                --         Debug.log "err" err
                -- in
                ParseError inputFile []
            )


doIt : Bool -> WhatToDo -> Task Never Bool
doIt autoYes whatToDo =
    case whatToDo of
        Validate validateMode ->
            validateNoChanges validateMode

        Format transformMode ->
            applyTransformation
                ProcessingFile
                autoYes
                FilesWillBeOverwritten
                format
                transformMode



-- MESSAGES


type InfoMessage
    = ProcessingFile FilePath
    | FileWouldChange FilePath
    | ParseError FilePath (List (A.Located E.Error))
    | JsonParseError FilePath String


type PromptMessage
    = FilesWillBeOverwritten (List FilePath)


type ErrorMessage
    = BadInputFiles (List Error)
    | NoInputs
    | SingleOutputWithMultipleInputs
    | TooManyInputs
    | OutputAndValidate


showFiles : List FilePath -> String
showFiles =
    unlines << List.map (\filename -> "    " ++ filename)


toConsolePromptMessage : PromptMessage -> String
toConsolePromptMessage promptMessage =
    case promptMessage of
        FilesWillBeOverwritten filePaths ->
            unlines
                [ "This will overwrite the following files to use Elm's preferred style:"
                , ""
                , showFiles filePaths
                , "This cannot be undone! Make sure to back up these files before proceeding."
                , ""
                , "Are you sure you want to overwrite these files with formatted versions? (y/n)"
                ]


toConsoleInfoMessage : InfoMessage -> String
toConsoleInfoMessage infoMessage =
    case infoMessage of
        ProcessingFile file ->
            "Processing file " ++ file

        FileWouldChange file ->
            "File would be changed " ++ file

        ParseError inputFile errs ->
            let
                location : FilePath
                location =
                    case errs of
                        [] ->
                            inputFile

                        (A.At (A.Region (A.Position line col) _) _) :: _ ->
                            inputFile ++ ":" ++ String.fromInt line ++ ":" ++ String.fromInt col
            in
            "Unable to parse file " ++ location ++ " To see a detailed explanation, run elm make on the file."

        JsonParseError inputFile err ->
            "Unable to parse JSON file " ++ inputFile ++ "\n\n" ++ err


jsonInfoMessage : InfoMessage -> Maybe Encode.Value
jsonInfoMessage infoMessage =
    let
        fileMessage : String -> String -> Encode.Value
        fileMessage filename message =
            Encode.object
                [ ( "path", Encode.string filename )
                , ( "message", Encode.string message )
                ]
    in
    case infoMessage of
        ProcessingFile _ ->
            Nothing

        FileWouldChange file ->
            Just (fileMessage file "File is not formatted with elm-format-0.8.7 --elm-version=0.19")

        ParseError inputFile _ ->
            Just (fileMessage inputFile "Error parsing the file")

        JsonParseError inputFile _ ->
            Just (fileMessage inputFile "Error parsing the JSON file")


toConsoleErrorMessage : ErrorMessage -> String
toConsoleErrorMessage errorMessage =
    case errorMessage of
        BadInputFiles filePaths ->
            unlines
                [ "There was a problem reading one or more of the specified INPUT paths:"
                , ""
                , unlines (List.map (\fp -> "    " ++ toConsoleError fp) filePaths)
                , "Please check the given paths."
                ]

        SingleOutputWithMultipleInputs ->
            unlines
                [ "Can't write to the OUTPUT path, because multiple .elm files have been specified."
                , ""
                , "Please remove the --output argument. The .elm files in INPUT will be formatted in place."
                ]

        TooManyInputs ->
            "Too many input sources! Please only provide one of either INPUT or --stdin"

        OutputAndValidate ->
            "Cannot use --output and --validate together"

        NoInputs ->
            "No file inputs provided. Use the --stdin flag to format input from standard input."



-- COMMAND LINE


type FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


readUtf8FileWithPath : FilePath -> Task Never ( FilePath, String )
readUtf8FileWithPath filePath =
    File.readUtf8 filePath
        |> Task.fmap (Tuple.pair filePath)


stat : FilePath -> Task Never FileType
stat path =
    Utils.dirDoesFileExist path
        |> Task.bind
            (\isFile ->
                Utils.dirDoesDirectoryExist path
                    |> Task.fmap
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


getYesOrNo : Task Never Bool
getYesOrNo =
    IO.hFlush IO.stdout
        |> Task.bind
            (\_ ->
                IO.getLine
                    |> Task.bind
                        (\input ->
                            case input of
                                "y" ->
                                    Task.pure True

                                "n" ->
                                    Task.pure False

                                _ ->
                                    IO.putStr "Must type 'y' for yes or 'n' for no: "
                                        |> Task.bind (\_ -> getYesOrNo)
                        )
            )


type ValidateMode
    = ValidateStdin
    | ValidateFiles FilePath (List FilePath)



-- INFO FORMATTER


approve : Bool -> PromptMessage -> Task Never Bool
approve autoYes prompt =
    if autoYes then
        Task.pure True

    else
        putStrLn False (toConsolePromptMessage prompt)
            |> Task.bind (\_ -> getYesOrNo)


putStrLn : Bool -> String -> Task Never ()
putStrLn usingStdout =
    -- we log to stdout unless it is being used for file output (in that case, we log to stderr)
    if usingStdout then
        IO.hPutStrLn IO.stderr

    else
        IO.putStrLn


resultsToJsonString : List (Result (Maybe String) ()) -> String
resultsToJsonString results =
    let
        lines : List String
        lines =
            List.filterMap handleResult results

        handleResult : Result (Maybe String) () -> Maybe String
        handleResult result =
            case result of
                Err info ->
                    info

                Ok () ->
                    Nothing
    in
    if List.isEmpty lines then
        "[]"

    else
        "[" ++ String.join "\n," lines ++ "\n]"



-- RESOLVE FILES


type Error
    = FileDoesNotExist FilePath
    | NoElmFiles FilePath


toConsoleError : Error -> String
toConsoleError error =
    case error of
        FileDoesNotExist path ->
            path ++ ": No such file or directory"

        NoElmFiles path ->
            path ++ ": Directory does not contain any *.elm files"


resolveFile : FilePath -> Task Never (Result Error (List FilePath))
resolveFile path =
    stat path
        |> Task.bind
            (\fileType ->
                case fileType of
                    IsFile ->
                        Task.pure (Ok [ path ])

                    IsDirectory ->
                        findAllElmFiles path
                            |> Task.fmap
                                (\elmFiles ->
                                    case elmFiles of
                                        [] ->
                                            Err (NoElmFiles path)

                                        _ ->
                                            Ok elmFiles
                                )

                    DoesNotExist ->
                        Task.pure (Err (FileDoesNotExist path))
            )


resolveElmFiles : List FilePath -> Task Never (Result (List Error) (List FilePath))
resolveElmFiles inputFiles =
    Task.mapM resolveFile inputFiles
        |> Task.fmap collectErrors
        |> Task.fmap
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



-- TRANSFORM FILES


type TranformFilesResult a
    = NoChange FilePath a
    | Changed FilePath a


updateFile : TranformFilesResult String -> Task Never ()
updateFile result =
    case result of
        NoChange _ _ ->
            Task.pure ()

        Changed outputFile outputText ->
            File.writeUtf8 outputFile outputText


readStdin : Task Never ( FilePath, String )
readStdin =
    File.readStdin
        |> Task.fmap (Tuple.pair "<STDIN>")


checkChange : ( FilePath, a ) -> a -> TranformFilesResult a
checkChange ( inputFile, inputText ) outputText =
    if inputText == outputText then
        NoChange inputFile outputText

    else
        Changed inputFile outputText


readFromFile : (FilePath -> Task Never ()) -> FilePath -> Task Never ( FilePath, String )
readFromFile onProcessingFile filePath =
    onProcessingFile filePath
        |> Task.bind (\_ -> readUtf8FileWithPath filePath)


type TransformMode
    = StdinToStdout
    | StdinToFile FilePath
    | FileToStdout FilePath
    | FileToFile FilePath FilePath
    | FilesInPlace FilePath (List FilePath)


applyTransformation : (FilePath -> InfoMessage) -> Bool -> (List FilePath -> PromptMessage) -> (( FilePath, String ) -> Result InfoMessage String) -> TransformMode -> Task Never Bool
applyTransformation processingFile autoYes confirmPrompt transform mode =
    let
        usesStdout : Bool
        usesStdout =
            case mode of
                StdinToStdout ->
                    True

                StdinToFile _ ->
                    True

                FileToStdout _ ->
                    True

                FileToFile _ _ ->
                    False

                FilesInPlace _ _ ->
                    False

        onInfo : InfoMessage -> Task Never ()
        onInfo info =
            if usesStdout then
                IO.hPutStrLn IO.stderr (toConsoleInfoMessage info)

            else
                IO.putStrLn (toConsoleInfoMessage info)
    in
    case mode of
        StdinToStdout ->
            readStdin
                |> Task.bind (logErrorOr onInfo IO.putStr << transform)

        StdinToFile outputFile ->
            readStdin
                |> Task.bind (logErrorOr onInfo (File.writeUtf8 outputFile) << transform)

        FileToStdout inputFile ->
            readUtf8FileWithPath inputFile
                |> Task.bind (logErrorOr onInfo IO.putStr << transform)

        FileToFile inputFile outputFile ->
            readFromFile (onInfo << processingFile) inputFile
                |> Task.bind (logErrorOr onInfo (File.writeUtf8 outputFile) << transform)

        FilesInPlace first rest ->
            let
                formatFile : FilePath -> Task Never Bool
                formatFile file =
                    readFromFile (onInfo << processingFile) file
                        |> Task.bind (\i -> logErrorOr onInfo updateFile <| Result.map (checkChange i) (transform i))
            in
            approve autoYes (confirmPrompt (first :: rest))
                |> Task.bind
                    (\canOverwrite ->
                        if canOverwrite then
                            Task.mapM formatFile (first :: rest)
                                |> Task.fmap (List.all identity)

                        else
                            Task.pure True
                    )


validateNoChanges : ValidateMode -> Task Never Bool
validateNoChanges mode =
    let
        newValidate : FilePath -> String -> Result (Maybe String) ()
        newValidate filePath content =
            case validate ( filePath, content ) of
                Err info ->
                    Err (Maybe.map (Encode.encode 0) (jsonInfoMessage info))

                Ok value ->
                    Ok value
    in
    case mode of
        ValidateStdin ->
            readStdin
                |> Task.bind
                    (\( filePath, content ) ->
                        let
                            result : Result (Maybe String) ()
                            result =
                                newValidate filePath content
                        in
                        IO.putStrLn (resultsToJsonString [ result ])
                            |> Task.fmap (\_ -> Result.isOk result)
                    )

        ValidateFiles first rest ->
            let
                validateFile : FilePath -> Task Never (Result (Maybe String) ())
                validateFile filePath =
                    File.readUtf8 filePath
                        |> Task.fmap (newValidate filePath)
            in
            Task.mapM validateFile (first :: rest)
                |> Task.bind
                    (\results ->
                        IO.putStrLn (resultsToJsonString results)
                            |> Task.fmap (\_ -> List.all Result.isOk results)
                    )


logErrorOr : (error -> Task Never ()) -> (a -> Task Never ()) -> Result error a -> Task Never Bool
logErrorOr onInfo fn result =
    case result of
        Err message ->
            onInfo message
                |> Task.fmap (\_ -> False)

        Ok value ->
            fn value
                |> Task.fmap (\_ -> True)



-- FILESYSTEM


collectFiles : (a -> Task Never (List a)) -> a -> Task Never (List a)
collectFiles children root =
    children root
        |> Task.bind (\xs -> Task.mapM (collectFiles children) xs)
        |> Task.fmap (\subChildren -> root :: List.concat subChildren)


listDir : FilePath -> Task Never (List FilePath)
listDir path =
    Utils.dirListDirectory path
        |> Task.fmap (List.map (\file -> path ++ "/" ++ file))


fileList : FilePath -> Task Never (List FilePath)
fileList =
    let
        children : FilePath -> Task Never (List FilePath)
        children path =
            if isSkippable path then
                Task.pure []

            else
                Utils.dirDoesDirectoryExist path
                    |> Task.bind
                        (\directory ->
                            if directory then
                                listDir path

                            else
                                Task.pure []
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


findAllElmFiles : FilePath -> Task Never (List FilePath)
findAllElmFiles inputFile =
    fileList inputFile
        |> Task.fmap (List.filter (hasExtension ".elm"))


hasFilename : String -> FilePath -> Bool
hasFilename name path =
    name == Utils.fpTakeFileName path



-- PRELUDE


unlines : List String -> String
unlines =
    List.map (\line -> line ++ "\n")
        >> String.concat
