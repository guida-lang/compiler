module Terminal.Format exposing
    ( Flags(..)
    , run
    )

import Builder.File as File
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as Syntax
import Elm.Syntax.File
import ElmSyntaxParserLenient
import ElmSyntaxPrint
import Json.Encode as Encode
import List.Extra as List
import Result.Extra as Result
import System.IO as IO exposing (IO)
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (FilePath)


type Flags
    = Flags (Maybe FilePath) Bool Bool Bool


run : List String -> Flags -> IO ()
run paths flags =
    runHelp paths flags


runHelp : List String -> Flags -> IO ()
runHelp paths ((Flags _ autoYes _ _) as flags) =
    resolveElmFiles paths
        |> IO.bind
            (\resolvedInputFiles ->
                case determineWhatToDoFromConfig flags resolvedInputFiles of
                    Err NoInputs ->
                        -- FIXME Program.showUsage
                        crash "Program.showUsage"

                    Err err ->
                        -- FIXME Program.error err
                        crash "Program.error err"

                    Ok a ->
                        IO.pure a
            )
        |> IO.bind (\whatToDo -> doIt autoYes whatToDo)
        |> IO.bind
            (\result ->
                if result then
                    IO.pure ()

                else
                    -- FIXME Program.failed
                    crash "Program.failed"
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
    case parseModule input of
        Ok modu ->
            if inputText /= render modu then
                Err (FileWouldChange inputFile)

            else
                Ok ()

        Err err ->
            Err err


parseModule : ( FilePath, String ) -> Result InfoMessage Elm.Syntax.File.File
parseModule ( inputFile, inputText ) =
    case ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ inputText of
        Just modu ->
            Ok modu

        Nothing ->
            -- FIXME missings errs
            Err (ParseError inputFile [])


format : ( FilePath, String ) -> Result InfoMessage String
format input =
    Result.map render (parseModule input)


doIt : Bool -> WhatToDo -> IO Bool
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
    | ParseError FilePath (List (A.Located Syntax.Error))
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
    -- let
    --     fileMessage filename message =
    --         Aeson.pairs
    --             (mconcat
    --                 [ "path" .= (filename :: FilePath)
    --                 , "message" .= (message :: String)
    --                 ]
    --             )
    -- in
    -- case infoMessage of
    --     ProcessingFile _ ->
    --         Nothing
    --     FileWouldChange elmVersion file ->
    --         Just
    --             $ fileMessage file
    --             $ "File is not formatted with elm-format-"
    --             <> ElmFormat.Version.asString
    --             <> " --elm-version="
    --             <> show elmVersion
    --     ParseError inputFile _ ->
    --         Just $ fileMessage inputFile "Error parsing the file"
    --     JsonParseError inputFile _ ->
    --         Just $ fileMessage inputFile "Error parsing the JSON file"
    crash "jsonInfoMessage"


toConsoleErrorMessage : ErrorMessage -> String
toConsoleErrorMessage errorMessage =
    case errorMessage of
        BadInputFiles filePaths ->
            unlines
                [ "There was a problem reading one or more of the specified INPUT paths:"
                , ""
                , unlines (List.map ((++) "    " << toConsoleError) filePaths)
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
            -- FIXME error "Error case NoInputs should be handled elsewhere. Please report this issue at https://github.com/avh4/elm-format/issues"
            crash "Error case NoInputs should be handled elsewhere. Please report this issue at https://github.com/avh4/elm-format/issues"



-- COMMAND LINE


type FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


readUtf8FileWithPath : FilePath -> IO ( FilePath, String )
readUtf8FileWithPath filePath =
    File.readUtf8 filePath
        |> IO.fmap (Tuple.pair filePath)


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


getYesOrNo : IO Bool
getYesOrNo =
    IO.hFlush IO.stdout
        |> IO.bind
            (\_ ->
                IO.getLine
                    |> IO.bind
                        (\input ->
                            case input of
                                "y" ->
                                    IO.pure True

                                "n" ->
                                    IO.pure False

                                _ ->
                                    IO.putStr "Must type 'y' for yes or 'n' for no: "
                                        |> IO.bind (\_ -> getYesOrNo)
                        )
            )


type ValidateMode
    = ValidateStdin
    | ValidateFiles FilePath (List FilePath)



-- INFO FORMATTER


approve : Bool -> PromptMessage -> IO Bool
approve autoYes prompt =
    if autoYes then
        IO.pure True

    else
        putStrLn False (toConsolePromptMessage prompt)
            |> IO.bind (\_ -> getYesOrNo)


putStrLn : Bool -> String -> IO ()
putStrLn usingStdout =
    -- we log to stdout unless it is being used for file output (in that case, we log to stderr)
    if usingStdout then
        IO.hPutStrLn IO.stderr

    else
        IO.putStrLn


resultsToJsonString : List (Result (Maybe String) ()) -> String
resultsToJsonString results =
    let
        lines =
            List.filterMap handleResult results

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



-- TRANSFORM FILES


type TranformFilesResult a
    = NoChange FilePath a
    | Changed FilePath a


updateFile : TranformFilesResult String -> IO ()
updateFile result =
    case result of
        NoChange _ _ ->
            IO.pure ()

        Changed outputFile outputText ->
            File.writeUtf8 outputFile outputText


checkChange : ( FilePath, a ) -> a -> TranformFilesResult a
checkChange ( inputFile, inputText ) outputText =
    if inputText == outputText then
        NoChange inputFile outputText

    else
        Changed inputFile outputText


readFromFile : (FilePath -> IO ()) -> FilePath -> IO ( FilePath, String )
readFromFile onProcessingFile filePath =
    onProcessingFile filePath
        |> IO.bind (\_ -> readUtf8FileWithPath filePath)


type TransformMode
    = StdinToStdout
    | StdinToFile FilePath
    | FileToStdout FilePath
    | FileToFile FilePath FilePath
    | FilesInPlace FilePath (List FilePath)


applyTransformation : (FilePath -> InfoMessage) -> Bool -> (List FilePath -> PromptMessage) -> (( FilePath, String ) -> Result InfoMessage String) -> TransformMode -> IO Bool
applyTransformation processingFile autoYes confirmPrompt transform mode =
    let
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

        onInfo info =
            if usesStdout then
                IO.hPutStrLn IO.stderr (toConsoleInfoMessage info)

            else
                IO.putStrLn (toConsoleInfoMessage info)
    in
    case mode of
        StdinToStdout ->
            -- FIXME readStdin >>= logErrorOr onInfo World.writeStdout . transform
            crash "StdinToStdout"

        StdinToFile outputFile ->
            -- FIXME readStdin >>= logErrorOr onInfo (World.writeUtf8File outputFile) . transform
            crash "StdinToFile outputFile"

        FileToStdout inputFile ->
            -- FIXME World.readUtf8FileWithPath inputFile >>= logErrorOr onInfo World.writeStdout . transform
            crash "FileToStdout inputFile"

        FileToFile inputFile outputFile ->
            -- FIXME readFromFile (onInfo . processingFile) inputFile >>= logErrorOr onInfo (World.writeUtf8File outputFile) . transform
            crash "FileToFile inputFile outputFile"

        FilesInPlace first rest ->
            let
                formatFile : FilePath -> IO Bool
                formatFile file =
                    readFromFile (onInfo << processingFile) file
                        |> IO.bind (\i -> logErrorOr onInfo updateFile <| Result.map (checkChange i) (transform i))
            in
            approve autoYes (confirmPrompt (first :: rest))
                |> IO.bind
                    (\canOverwrite ->
                        if canOverwrite then
                            IO.mapM formatFile (first :: rest)
                                |> IO.fmap (List.all identity)

                        else
                            IO.pure True
                    )


validateNoChanges : ValidateMode -> IO Bool
validateNoChanges mode =
    let
        newValidate filePath content =
            case validate ( filePath, content ) of
                Err info ->
                    Err (Maybe.map (Encode.encode 0) (jsonInfoMessage info))

                Ok value ->
                    Ok value
    in
    case mode of
        ValidateStdin ->
            -- FIXME
            -- do
            --     (filePath, content) <- readStdin
            --     let result = newValidate filePath content
            --     World.putStrLn (InfoFormatter.resultsToJsonString [result])
            --     return (Either.isRight result)
            crash "ValidateStdin"

        ValidateFiles first rest ->
            let
                validateFile filePath =
                    File.readUtf8 filePath
                        |> IO.fmap (\content -> newValidate filePath content)
            in
            IO.mapM validateFile (first :: rest)
                |> IO.bind
                    (\results ->
                        IO.putStrLn (resultsToJsonString results)
                            |> IO.fmap (\_ -> List.all Result.isOk results)
                    )


logErrorOr : (error -> IO ()) -> (a -> IO ()) -> Result error a -> IO Bool
logErrorOr onInfo fn result =
    case result of
        Err message ->
            onInfo message
                |> IO.fmap (\_ -> False)

        Ok value ->
            fn value
                |> IO.fmap (\_ -> True)



-- RENDER


render : Elm.Syntax.File.File -> String
render modul =
    ElmSyntaxPrint.module_ modul
        |> ElmSyntaxPrint.toString



-- FILESYSTEM


collectFiles : (a -> IO (List a)) -> a -> IO (List a)
collectFiles children root =
    children root
        |> IO.bind (\xs -> IO.mapM (collectFiles children) xs)
        |> IO.fmap (\subChildren -> root :: List.concat subChildren)


listDir : FilePath -> IO (List FilePath)
listDir path =
    Utils.dirListDirectory path
        |> IO.fmap (List.map ((++) (path ++ "/")))


fileList : FilePath -> IO (List FilePath)
fileList =
    let
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



-- PRELUDE


unlines : List String -> String
unlines =
    List.map (\line -> line ++ "\n")
        >> String.join ""
