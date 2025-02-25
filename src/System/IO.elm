port module System.IO exposing
    ( Program, Flags, Model, Msg, Next, run
    , IO(..), ION(..), RealWorld, pure, apply, fmap, bind, mapM
    , FilePath, Handle(..)
    , stdout, stderr
    , withFile, IOMode(..)
    , hClose
    , hFileSize
    , hFlush
    , hIsTerminalDevice
    , hPutStr, hPutStrLn
    , putStr, putStrLn, getLine
    , ReplState(..), initialReplState
    , RealWorldMVar, MVarSubscriber(..)
    )

{-| Ref.: <https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html>

@docs Program, Flags, Model, Msg, Next, run


# The IO monad

@docs IO, ION, RealWorld, pure, apply, fmap, bind, mapM


# Files and handles

@docs FilePath, Handle


# Standard handles

@docs stdout, stderr


# Opening files

@docs withFile, IOMode


# Closing files

@docs hClose


# File locking

@docs hFileSize


# Buffering operations

@docs hFlush


# Terminal operations (not portable: GHC only)

@docs hIsTerminalDevice


# Text output

@docs hPutStr, hPutStrLn


# Special cases for standard input and output

@docs putStr, putStrLn, getLine


# Repl State

@docs ReplState, initialReplState


# MVars

@docs RealWorldMVar, MVarSubscriber

-}

import Array exposing (Array)
import Cmd.Extra as Cmd
import Codec.Archive.Zip as Zip
import Dict exposing (Dict)
import Json.Encode as Encode
import Utils.Crash exposing (crash)


type alias Flags =
    { args : List String
    , currentDirectory : String
    , envVars : List ( String, String )
    , homedir : FilePath
    , progName : String
    }


type alias Program =
    Platform.Program Flags Model Msg


run : IO () -> Program
run app =
    Platform.worker
        { init =
            \flags ->
                update (PureMsg 0 app)
                    { count = 1
                    , args = flags.args
                    , currentDirectory = flags.currentDirectory
                    , envVars = Dict.fromList flags.envVars
                    , homedir = flags.homedir
                    , progName = flags.progName
                    , state = initialReplState
                    , mVars = Array.empty
                    , next = Dict.empty
                    }
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ recvGetLine (\{ index, value } -> GetLineMsg index value)
                    , recvHPutStr HPutLineMsg
                    , recvWriteString WriteStringMsg
                    , recvRead (\{ index, value } -> ReadMsg index value)
                    , recvReadStdin (\{ index, value } -> ReadStdinMsg index value)
                    , recvHttpFetch (\{ index, value } -> HttpFetchMsg index value)
                    , recvGetArchive (\{ index, value } -> GetArchiveMsg index value)
                    , recvHttpUpload HttpUploadMsg
                    , recvHFlush HFlushMsg
                    , recvWithFile (\{ index, value } -> WithFileMsg index value)
                    , recvHFileSize (\{ index, value } -> HFileSizeMsg index value)
                    , recvProcWithCreateProcess (\{ index, value } -> ProcWithCreateProcessMsg index value)
                    , recvHClose HCloseMsg
                    , recvProcWaitForProcess (\{ index, value } -> ProcWaitForProcessMsg index value)
                    , recvDirFindExecutable (\{ index, value } -> DirFindExecutableMsg index value)
                    , recvReplGetInputLine (\{ index, value } -> ReplGetInputLineMsg index value)
                    , recvDirDoesFileExist (\{ index, value } -> DirDoesFileExistMsg index value)
                    , recvDirCreateDirectoryIfMissing DirCreateDirectoryIfMissingMsg
                    , recvLockFile LockFileMsg
                    , recvUnlockFile UnlockFileMsg
                    , recvDirGetModificationTime (\{ index, value } -> DirGetModificationTimeMsg index value)
                    , recvDirDoesDirectoryExist (\{ index, value } -> DirDoesDirectoryExistMsg index value)
                    , recvDirCanonicalizePath (\{ index, value } -> DirCanonicalizePathMsg index value)
                    , recvDirListDirectory (\{ index, value } -> DirListDirectoryMsg index value)
                    , recvBinaryDecodeFileOrFail (\{ index, value } -> BinaryDecodeFileOrFailMsg index value)
                    , recvWrite WriteMsg
                    , recvDirRemoveFile DirRemoveFileMsg
                    , recvDirRemoveDirectoryRecursive DirRemoveDirectoryRecursiveMsg
                    , recvDirWithCurrentDirectory DirWithCurrentDirectoryMsg
                    , recvReplGetInputLineWithInitial (\{ index, value } -> ReplGetInputLineWithInitialMsg index value)
                    ]
        }


type alias Model =
    RealWorld


type Next
    = GetLineNext (String -> IO ())
    | HPutLineNext (() -> IO ())
    | WriteStringNext (() -> IO ())
    | ReadNext (String -> IO ())
    | ReadStdinNext (String -> IO ())
    | HttpFetchNext (String -> IO ())
    | GetArchiveNext (( String, Zip.Archive ) -> IO ())
    | HttpUploadNext (() -> IO ())
    | HFlushNext (() -> IO ())
    | WithFileNext (Int -> IO ())
    | HFileSizeNext (Int -> IO ())
    | ProcWithCreateProcessNext ({ stdinHandle : Maybe Int, ph : Int } -> IO ())
    | HCloseNext (() -> IO ())
    | ProcWaitForProcessNext (Int -> IO ())
    | ExitWithNext (() -> IO ())
    | DirFindExecutableNext (Maybe FilePath -> IO ())
    | ReplGetInputLineNext (Maybe String -> IO ())
    | DirDoesFileExistNext (Bool -> IO ())
    | DirCreateDirectoryIfMissingNext (() -> IO ())
    | LockFileNext (() -> IO ())
    | UnlockFileNext (() -> IO ())
    | DirGetModificationTimeNext (Int -> IO ())
    | DirDoesDirectoryExistNext (Bool -> IO ())
    | DirCanonicalizePathNext (String -> IO ())
    | DirListDirectoryNext (List String -> IO ())
    | BinaryDecodeFileOrFailNext (Encode.Value -> IO ())
    | WriteNext (() -> IO ())
    | DirRemoveFileNext (() -> IO ())
    | DirRemoveDirectoryRecursiveNext (() -> IO ())
    | DirWithCurrentDirectoryNext (() -> IO ())
    | ReplGetInputLineWithInitialNext (Maybe String -> IO ())
    | NewEmptyMVarNext (Int -> IO ())
    | ReadMVarNext (Encode.Value -> IO ())
    | TakeMVarNext (Encode.Value -> IO ())
    | PutMVarNext (() -> IO ())


type Msg
    = PureMsg Int (IO ())
    | GetLineMsg Int String
    | HPutLineMsg Int
    | WriteStringMsg Int
    | ReadMsg Int String
    | ReadStdinMsg Int String
    | HttpFetchMsg Int String
    | GetArchiveMsg Int ( String, Zip.Archive )
    | HttpUploadMsg Int
    | HFlushMsg Int
    | WithFileMsg Int Int
    | HFileSizeMsg Int Int
    | ProcWithCreateProcessMsg Int { stdinHandle : Maybe Int, ph : Int }
    | HCloseMsg Int
    | ProcWaitForProcessMsg Int Int
    | DirFindExecutableMsg Int (Maybe FilePath)
    | ReplGetInputLineMsg Int (Maybe String)
    | DirDoesFileExistMsg Int Bool
    | DirCreateDirectoryIfMissingMsg Int
    | LockFileMsg Int
    | UnlockFileMsg Int
    | DirGetModificationTimeMsg Int Int
    | DirDoesDirectoryExistMsg Int Bool
    | DirCanonicalizePathMsg Int FilePath
    | DirListDirectoryMsg Int (List FilePath)
    | BinaryDecodeFileOrFailMsg Int Encode.Value
    | WriteMsg Int
    | DirRemoveFileMsg Int
    | DirRemoveDirectoryRecursiveMsg Int
    | DirWithCurrentDirectoryMsg Int
    | ReplGetInputLineWithInitialMsg Int (Maybe String)
    | NewEmptyMVarMsg Int Int
    | ReadMVarMsg Int Encode.Value
    | PutMVarMsg Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PureMsg index (IO fn) ->
            case fn index model of
                ( newRealWorld, Pure () ) ->
                    ( { newRealWorld | next = Dict.remove index newRealWorld.next }
                    , if index == 0 then
                        sendExitWith 0

                      else
                        Cmd.none
                    )

                ( newRealWorld, ForkIO next forkIO ) ->
                    ( { newRealWorld | count = newRealWorld.count + 1 }
                    , Cmd.batch
                        [ Cmd.perform (PureMsg index (next ()))
                        , Cmd.perform (PureMsg newRealWorld.count forkIO)
                        ]
                    )

                ( newRealWorld, GetLine next ) ->
                    ( { newRealWorld | next = Dict.insert index (GetLineNext next) model.next }, sendGetLine index )

                ( newRealWorld, HPutStr next (Handle fd) content ) ->
                    ( { newRealWorld | next = Dict.insert index (HPutLineNext next) model.next }, sendHPutStr { index = index, fd = fd, content = content } )

                ( newRealWorld, WriteString next path content ) ->
                    ( { newRealWorld | next = Dict.insert index (WriteStringNext next) model.next }, sendWriteString { index = index, path = path, content = content } )

                ( newRealWorld, Read next fd ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadNext next) model.next }, sendRead { index = index, fd = fd } )

                ( newRealWorld, ReadStdin next ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadStdinNext next) model.next }, sendReadStdin { index = index } )

                ( newRealWorld, HttpFetch next method urlStr headers ) ->
                    ( { newRealWorld | next = Dict.insert index (HttpFetchNext next) model.next }, sendHttpFetch { index = index, method = method, urlStr = urlStr, headers = headers } )

                ( newRealWorld, GetArchive next method url ) ->
                    ( { newRealWorld | next = Dict.insert index (GetArchiveNext next) model.next }, sendGetArchive { index = index, method = method, url = url } )

                ( newRealWorld, HttpUpload next urlStr headers parts ) ->
                    ( { newRealWorld | next = Dict.insert index (HttpUploadNext next) model.next }, sendHttpUpload { index = index, urlStr = urlStr, headers = headers, parts = parts } )

                ( newRealWorld, HFlush next (Handle fd) ) ->
                    ( { newRealWorld | next = Dict.insert index (HFlushNext next) model.next }, sendHFlush { index = index, fd = fd } )

                ( newRealWorld, WithFile next path mode ) ->
                    ( { newRealWorld | next = Dict.insert index (WithFileNext next) model.next }
                    , sendWithFile
                        { index = index
                        , path = path
                        , mode =
                            case mode of
                                ReadMode ->
                                    "r"

                                WriteMode ->
                                    "w"

                                AppendMode ->
                                    "a"

                                ReadWriteMode ->
                                    "w+"
                        }
                    )

                ( newRealWorld, HFileSize next (Handle fd) ) ->
                    ( { newRealWorld | next = Dict.insert index (HFileSizeNext next) model.next }, sendHFileSize { index = index, fd = fd } )

                ( newRealWorld, ProcWithCreateProcess next createProcess ) ->
                    ( { newRealWorld | next = Dict.insert index (ProcWithCreateProcessNext next) model.next }, sendProcWithCreateProcess { index = index, createProcess = createProcess } )

                ( newRealWorld, HClose next (Handle fd) ) ->
                    ( { newRealWorld | next = Dict.insert index (HCloseNext next) model.next }, sendHClose { index = index, fd = fd } )

                ( newRealWorld, ProcWaitForProcess next ph ) ->
                    ( { newRealWorld | next = Dict.insert index (ProcWaitForProcessNext next) model.next }, sendProcWaitForProcess { index = index, ph = ph } )

                ( newRealWorld, ExitWith next code ) ->
                    ( { newRealWorld | next = Dict.insert index (ExitWithNext next) model.next }, sendExitWith code )

                ( newRealWorld, DirFindExecutable next name ) ->
                    ( { newRealWorld | next = Dict.insert index (DirFindExecutableNext next) model.next }, sendDirFindExecutable { index = index, name = name } )

                ( newRealWorld, ReplGetInputLine next prompt ) ->
                    ( { newRealWorld | next = Dict.insert index (ReplGetInputLineNext next) model.next }, sendReplGetInputLine { index = index, prompt = prompt } )

                ( newRealWorld, DirDoesFileExist next filename ) ->
                    ( { newRealWorld | next = Dict.insert index (DirDoesFileExistNext next) model.next }, sendDirDoesFileExist { index = index, filename = filename } )

                ( newRealWorld, DirCreateDirectoryIfMissing next createParents filename ) ->
                    ( { newRealWorld | next = Dict.insert index (DirCreateDirectoryIfMissingNext next) model.next }, sendDirCreateDirectoryIfMissing { index = index, createParents = createParents, filename = filename } )

                ( newRealWorld, LockFile next path ) ->
                    ( { newRealWorld | next = Dict.insert index (LockFileNext next) model.next }, sendLockFile { index = index, path = path } )

                ( newRealWorld, UnlockFile next path ) ->
                    ( { newRealWorld | next = Dict.insert index (UnlockFileNext next) model.next }, sendUnlockFile { index = index, path = path } )

                ( newRealWorld, DirGetModificationTime next filename ) ->
                    ( { newRealWorld | next = Dict.insert index (DirGetModificationTimeNext next) model.next }, sendDirGetModificationTime { index = index, filename = filename } )

                ( newRealWorld, DirDoesDirectoryExist next path ) ->
                    ( { newRealWorld | next = Dict.insert index (DirDoesDirectoryExistNext next) model.next }, sendDirDoesDirectoryExist { index = index, path = path } )

                ( newRealWorld, DirCanonicalizePath next path ) ->
                    ( { newRealWorld | next = Dict.insert index (DirCanonicalizePathNext next) model.next }, sendDirCanonicalizePath { index = index, path = path } )

                ( newRealWorld, DirListDirectory next path ) ->
                    ( { newRealWorld | next = Dict.insert index (DirListDirectoryNext next) model.next }, sendDirListDirectory { index = index, path = path } )

                ( newRealWorld, BinaryDecodeFileOrFail next filename ) ->
                    ( { newRealWorld | next = Dict.insert index (BinaryDecodeFileOrFailNext next) model.next }, sendBinaryDecodeFileOrFail { index = index, filename = filename } )

                ( newRealWorld, Write next fd content ) ->
                    ( { newRealWorld | next = Dict.insert index (WriteNext next) model.next }, sendWrite { index = index, fd = fd, content = content } )

                ( newRealWorld, DirRemoveFile next path ) ->
                    ( { newRealWorld | next = Dict.insert index (DirRemoveFileNext next) model.next }, sendDirRemoveFile { index = index, path = path } )

                ( newRealWorld, DirRemoveDirectoryRecursive next path ) ->
                    ( { newRealWorld | next = Dict.insert index (DirRemoveDirectoryRecursiveNext next) model.next }, sendDirRemoveDirectoryRecursive { index = index, path = path } )

                ( newRealWorld, DirWithCurrentDirectory next path ) ->
                    ( { newRealWorld | next = Dict.insert index (DirWithCurrentDirectoryNext next) model.next }, sendDirWithCurrentDirectory { index = index, path = path } )

                ( newRealWorld, ReplGetInputLineWithInitial next prompt left right ) ->
                    ( { newRealWorld | next = Dict.insert index (ReplGetInputLineWithInitialNext next) model.next }, sendReplGetInputLineWithInitial { index = index, prompt = prompt, left = left, right = right } )

                -- MVars
                ( newRealWorld, NewEmptyMVar next value ) ->
                    update (NewEmptyMVarMsg index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext next) model.next }

                ( newRealWorld, ReadMVar next (Just value) ) ->
                    update (ReadMVarMsg index value) { newRealWorld | next = Dict.insert index (ReadMVarNext next) model.next }

                ( newRealWorld, ReadMVar next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext next) model.next }, Cmd.none )

                ( newRealWorld, TakeMVar next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg index value) { newRealWorld | next = Dict.insert index (TakeMVarNext next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, TakeMVar next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (TakeMVarNext next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, PutMVar next readIndexes (Just value) ) ->
                    ( { newRealWorld | next = Dict.insert index (PutMVarNext next) model.next }
                    , Cmd.batch
                        (List.foldl (\readIndex -> (::) (Cmd.perform (ReadMVarMsg readIndex value)))
                            [ Cmd.perform (PutMVarMsg index) ]
                            readIndexes
                        )
                    )

                ( newRealWorld, PutMVar next _ Nothing ) ->
                    update (PutMVarMsg index) { newRealWorld | next = Dict.insert index (PutMVarNext next) model.next }

        GetLineMsg index input ->
            case Dict.get index model.next of
                Just (GetLineNext fn) ->
                    update (PureMsg index (fn input)) model

                _ ->
                    crash "GetLineMsg"

        HPutLineMsg index ->
            case Dict.get index model.next of
                Just (HPutLineNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "HPutLineMsg"

        WriteStringMsg index ->
            case Dict.get index model.next of
                Just (WriteStringNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "WriteStringMsg"

        ReadMsg index value ->
            case Dict.get index model.next of
                Just (ReadNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMsg"

        ReadStdinMsg index value ->
            case Dict.get index model.next of
                Just (ReadStdinNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadStdinMsg"

        HttpFetchMsg index value ->
            case Dict.get index model.next of
                Just (HttpFetchNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "HttpFetchMsg"

        GetArchiveMsg index value ->
            case Dict.get index model.next of
                Just (GetArchiveNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "GetArchiveMsg"

        HttpUploadMsg index ->
            case Dict.get index model.next of
                Just (HttpUploadNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "HttpUploadMsg"

        HFlushMsg index ->
            case Dict.get index model.next of
                Just (HFlushNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "HFlushMsg"

        WithFileMsg index fd ->
            case Dict.get index model.next of
                Just (WithFileNext fn) ->
                    update (PureMsg index (fn fd)) model

                _ ->
                    crash "WithFileMsg"

        HFileSizeMsg index size ->
            case Dict.get index model.next of
                Just (HFileSizeNext fn) ->
                    update (PureMsg index (fn size)) model

                _ ->
                    crash "HFileSizeMsg"

        ProcWithCreateProcessMsg index value ->
            case Dict.get index model.next of
                Just (ProcWithCreateProcessNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ProcWithCreateProcessMsg"

        HCloseMsg index ->
            case Dict.get index model.next of
                Just (HCloseNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "HCloseMsg"

        ProcWaitForProcessMsg index code ->
            case Dict.get index model.next of
                Just (ProcWaitForProcessNext fn) ->
                    update (PureMsg index (fn code)) model

                _ ->
                    crash "ProcWaitForProcessMsg"

        DirFindExecutableMsg index value ->
            case Dict.get index model.next of
                Just (DirFindExecutableNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirFindExecutableMsg"

        ReplGetInputLineMsg index value ->
            case Dict.get index model.next of
                Just (ReplGetInputLineNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReplGetInputLineMsg"

        DirDoesFileExistMsg index value ->
            case Dict.get index model.next of
                Just (DirDoesFileExistNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirDoesFileExistMsg"

        DirCreateDirectoryIfMissingMsg index ->
            case Dict.get index model.next of
                Just (DirCreateDirectoryIfMissingNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "DirCreateDirectoryIfMissingMsg"

        LockFileMsg index ->
            case Dict.get index model.next of
                Just (LockFileNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "LockFileMsg"

        UnlockFileMsg index ->
            case Dict.get index model.next of
                Just (UnlockFileNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "UnlockFileMsg"

        DirGetModificationTimeMsg index value ->
            case Dict.get index model.next of
                Just (DirGetModificationTimeNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirGetModificationTimeMsg"

        DirDoesDirectoryExistMsg index value ->
            case Dict.get index model.next of
                Just (DirDoesDirectoryExistNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirDoesDirectoryExistMsg"

        DirCanonicalizePathMsg index value ->
            case Dict.get index model.next of
                Just (DirCanonicalizePathNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirCanonicalizePathMsg"

        DirListDirectoryMsg index value ->
            case Dict.get index model.next of
                Just (DirListDirectoryNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirListDirectoryMsg"

        BinaryDecodeFileOrFailMsg index value ->
            case Dict.get index model.next of
                Just (BinaryDecodeFileOrFailNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "BinaryDecodeFileOrFailMsg"

        WriteMsg index ->
            case Dict.get index model.next of
                Just (WriteNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "WriteMsg"

        DirRemoveFileMsg index ->
            case Dict.get index model.next of
                Just (DirRemoveFileNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "DirRemoveFileMsg"

        DirRemoveDirectoryRecursiveMsg index ->
            case Dict.get index model.next of
                Just (DirRemoveDirectoryRecursiveNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "DirRemoveDirectoryRecursiveMsg"

        DirWithCurrentDirectoryMsg index ->
            case Dict.get index model.next of
                Just (DirWithCurrentDirectoryNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "DirWithCurrentDirectoryMsg"

        ReplGetInputLineWithInitialMsg index value ->
            case Dict.get index model.next of
                Just (ReplGetInputLineWithInitialNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReplGetInputLineWithInitialMsg"

        NewEmptyMVarMsg index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg"

        ReadMVarMsg index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                Just (TakeMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg"

        PutMVarMsg index ->
            case Dict.get index model.next of
                Just (PutMVarNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg"


updatePutIndex : Maybe Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePutIndex maybePutIndex ( model, cmd ) =
    case maybePutIndex of
        Just putIndex ->
            update (PutMVarMsg putIndex) model
                |> Tuple.mapSecond (\putCmd -> Cmd.batch [ cmd, putCmd ])

        Nothing ->
            ( model, cmd )


port sendGetLine : Int -> Cmd msg


port recvGetLine : ({ index : Int, value : String } -> msg) -> Sub msg


port sendHPutStr : { index : Int, fd : Int, content : String } -> Cmd msg


port recvHPutStr : (Int -> msg) -> Sub msg


port sendWriteString : { index : Int, path : FilePath, content : String } -> Cmd msg


port recvWriteString : (Int -> msg) -> Sub msg


port sendRead : { index : Int, fd : String } -> Cmd msg


port recvRead : ({ index : Int, value : String } -> msg) -> Sub msg


port sendReadStdin : { index : Int } -> Cmd msg


port recvReadStdin : ({ index : Int, value : String } -> msg) -> Sub msg


port sendHttpFetch : { index : Int, method : String, urlStr : String, headers : List ( String, String ) } -> Cmd msg


port recvHttpFetch : ({ index : Int, value : String } -> msg) -> Sub msg


port sendGetArchive : { index : Int, method : String, url : String } -> Cmd msg


port recvGetArchive : ({ index : Int, value : ( String, Zip.Archive ) } -> msg) -> Sub msg


port sendHttpUpload : { index : Int, urlStr : String, headers : List ( String, String ), parts : List Encode.Value } -> Cmd msg


port recvHttpUpload : (Int -> msg) -> Sub msg


port sendHFlush : { index : Int, fd : Int } -> Cmd msg


port recvHFlush : (Int -> msg) -> Sub msg


port sendWithFile : { index : Int, path : String, mode : String } -> Cmd msg


port recvWithFile : ({ index : Int, value : Int } -> msg) -> Sub msg


port sendHFileSize : { index : Int, fd : Int } -> Cmd msg


port recvHFileSize : ({ index : Int, value : Int } -> msg) -> Sub msg


port sendProcWithCreateProcess : { index : Int, createProcess : Encode.Value } -> Cmd msg


port recvProcWithCreateProcess : ({ index : Int, value : { stdinHandle : Maybe Int, ph : Int } } -> msg) -> Sub msg


port sendHClose : { index : Int, fd : Int } -> Cmd msg


port recvHClose : (Int -> msg) -> Sub msg


port sendProcWaitForProcess : { index : Int, ph : Int } -> Cmd msg


port recvProcWaitForProcess : ({ index : Int, value : Int } -> msg) -> Sub msg


port sendExitWith : Int -> Cmd msg


port sendDirFindExecutable : { index : Int, name : FilePath } -> Cmd msg


port recvDirFindExecutable : ({ index : Int, value : Maybe FilePath } -> msg) -> Sub msg


port sendReplGetInputLine : { index : Int, prompt : String } -> Cmd msg


port recvReplGetInputLine : ({ index : Int, value : Maybe String } -> msg) -> Sub msg


port sendDirDoesFileExist : { index : Int, filename : String } -> Cmd msg


port recvDirDoesFileExist : ({ index : Int, value : Bool } -> msg) -> Sub msg


port sendDirCreateDirectoryIfMissing : { index : Int, createParents : Bool, filename : String } -> Cmd msg


port recvDirCreateDirectoryIfMissing : (Int -> msg) -> Sub msg


port sendLockFile : { index : Int, path : String } -> Cmd msg


port recvLockFile : (Int -> msg) -> Sub msg


port sendUnlockFile : { index : Int, path : String } -> Cmd msg


port recvUnlockFile : (Int -> msg) -> Sub msg


port sendDirGetModificationTime : { index : Int, filename : String } -> Cmd msg


port recvDirGetModificationTime : ({ index : Int, value : Int } -> msg) -> Sub msg


port sendDirDoesDirectoryExist : { index : Int, path : FilePath } -> Cmd msg


port recvDirDoesDirectoryExist : ({ index : Int, value : Bool } -> msg) -> Sub msg


port sendDirCanonicalizePath : { index : Int, path : FilePath } -> Cmd msg


port recvDirCanonicalizePath : ({ index : Int, value : FilePath } -> msg) -> Sub msg


port sendDirListDirectory : { index : Int, path : FilePath } -> Cmd msg


port recvDirListDirectory : ({ index : Int, value : List FilePath } -> msg) -> Sub msg


port sendBinaryDecodeFileOrFail : { index : Int, filename : FilePath } -> Cmd msg


port recvBinaryDecodeFileOrFail : ({ index : Int, value : Encode.Value } -> msg) -> Sub msg


port sendWrite : { index : Int, fd : FilePath, content : Encode.Value } -> Cmd msg


port recvWrite : (Int -> msg) -> Sub msg


port sendDirRemoveFile : { index : Int, path : FilePath } -> Cmd msg


port recvDirRemoveFile : (Int -> msg) -> Sub msg


port sendDirRemoveDirectoryRecursive : { index : Int, path : FilePath } -> Cmd msg


port recvDirRemoveDirectoryRecursive : (Int -> msg) -> Sub msg


port sendDirWithCurrentDirectory : { index : Int, path : FilePath } -> Cmd msg


port recvDirWithCurrentDirectory : (Int -> msg) -> Sub msg


port sendReplGetInputLineWithInitial : { index : Int, prompt : String, left : String, right : String } -> Cmd msg


port recvReplGetInputLineWithInitial : ({ index : Int, value : Maybe String } -> msg) -> Sub msg



-- The IO monad


type IO a
    = IO (Int -> RealWorld -> ( RealWorld, ION a ))


type ION a
    = Pure a
    | ForkIO (() -> IO a) (IO ())
    | HPutStr (() -> IO a) Handle String
    | GetLine (String -> IO a)
    | WriteString (() -> IO a) FilePath String
    | Read (String -> IO a) FilePath
    | ReadStdin (String -> IO a)
    | HttpFetch (String -> IO a) String String (List ( String, String ))
    | GetArchive (( String, Zip.Archive ) -> IO a) String String
    | HttpUpload (() -> IO a) String (List ( String, String )) (List Encode.Value)
    | HFlush (() -> IO a) Handle
    | WithFile (Int -> IO a) String IOMode
    | HFileSize (Int -> IO a) Handle
    | ProcWithCreateProcess ({ stdinHandle : Maybe Int, ph : Int } -> IO a) Encode.Value
    | HClose (() -> IO a) Handle
    | ProcWaitForProcess (Int -> IO a) Int
    | ExitWith (a -> IO a) Int
    | DirFindExecutable (Maybe FilePath -> IO a) FilePath
    | ReplGetInputLine (Maybe String -> IO a) String
    | DirDoesFileExist (Bool -> IO a) FilePath
    | DirCreateDirectoryIfMissing (() -> IO a) Bool FilePath
    | LockFile (() -> IO a) FilePath
    | UnlockFile (() -> IO a) FilePath
    | DirGetModificationTime (Int -> IO a) FilePath
    | DirDoesDirectoryExist (Bool -> IO a) FilePath
    | DirCanonicalizePath (String -> IO a) FilePath
    | DirListDirectory (List String -> IO a) FilePath
    | BinaryDecodeFileOrFail (Encode.Value -> IO a) FilePath
    | Write (() -> IO a) FilePath Encode.Value
    | DirRemoveFile (() -> IO a) FilePath
    | DirRemoveDirectoryRecursive (() -> IO a) FilePath
    | DirWithCurrentDirectory (() -> IO a) FilePath
    | ReplGetInputLineWithInitial (Maybe String -> IO a) String String String
      -- MVars
    | NewEmptyMVar (Int -> IO a) Int
    | ReadMVar (Encode.Value -> IO a) (Maybe Encode.Value)
    | TakeMVar (Encode.Value -> IO a) (Maybe Encode.Value) (Maybe Int)
    | PutMVar (() -> IO a) (List Int) (Maybe Encode.Value)


type alias RealWorld =
    { count : Int
    , args : List String
    , currentDirectory : String
    , envVars : Dict String String
    , homedir : FilePath
    , progName : String
    , state : ReplState
    , mVars : Array RealWorldMVar
    , next : Dict Int Next
    }


type alias RealWorldMVar =
    { subscribers : List MVarSubscriber
    , value : Maybe Encode.Value
    }


type MVarSubscriber
    = ReadSubscriber Int
    | TakeSubscriber Int
    | PutSubscriber Int Encode.Value


pure : a -> IO a
pure x =
    IO (\_ s -> ( s, Pure x ))


apply : IO a -> IO (a -> b) -> IO b
apply ma mf =
    bind (\f -> bind (pure << f) ma) mf


fmap : (a -> b) -> IO a -> IO b
fmap fn ma =
    bind (pure << fn) ma


bind : (a -> IO b) -> IO a -> IO b
bind f (IO ma) =
    IO
        (\index s0 ->
            case ma index s0 of
                ( s1, Pure a ) ->
                    unIO (f a) index s1

                ( s1, ForkIO next forkIO ) ->
                    ( s1, ForkIO (\() -> bind f (next ())) forkIO )

                ( s1, GetLine next ) ->
                    ( s1, GetLine (\input -> bind f (next input)) )

                ( s1, HPutStr next handle content ) ->
                    ( s1, HPutStr (\() -> bind f (next ())) handle content )

                ( s1, WriteString next path content ) ->
                    ( s1, WriteString (\() -> bind f (next ())) path content )

                ( s1, Read next fd ) ->
                    ( s1, Read (\input -> bind f (next input)) fd )

                ( s1, ReadStdin next ) ->
                    ( s1, ReadStdin (\input -> bind f (next input)) )

                ( s1, HttpFetch next method urlStr headers ) ->
                    ( s1, HttpFetch (\body -> bind f (next body)) method urlStr headers )

                ( s1, GetArchive next method url ) ->
                    ( s1, GetArchive (\body -> bind f (next body)) method url )

                ( s1, HttpUpload next urlStr headers parts ) ->
                    ( s1, HttpUpload (\() -> bind f (next ())) urlStr headers parts )

                ( s1, HFlush next handle ) ->
                    ( s1, HFlush (\() -> bind f (next ())) handle )

                ( s1, WithFile next path mode ) ->
                    ( s1, WithFile (\fd -> bind f (next fd)) path mode )

                ( s1, HFileSize next handle ) ->
                    ( s1, HFileSize (\size -> bind f (next size)) handle )

                ( s1, ProcWithCreateProcess next createProcess ) ->
                    ( s1, ProcWithCreateProcess (\data -> bind f (next data)) createProcess )

                ( s1, HClose next handle ) ->
                    ( s1, HClose (\() -> bind f (next ())) handle )

                ( s1, ProcWaitForProcess next ph ) ->
                    ( s1, ProcWaitForProcess (\code -> bind f (next code)) ph )

                ( s1, ExitWith _ code ) ->
                    ( s1, ExitWith (\_ -> crash "exitWith") code )

                ( s1, DirFindExecutable next name ) ->
                    ( s1, DirFindExecutable (\value -> bind f (next value)) name )

                ( s1, ReplGetInputLine next prompt ) ->
                    ( s1, ReplGetInputLine (\value -> bind f (next value)) prompt )

                ( s1, DirDoesFileExist next filename ) ->
                    ( s1, DirDoesFileExist (\exists -> bind f (next exists)) filename )

                ( s1, DirCreateDirectoryIfMissing next createParents filename ) ->
                    ( s1, DirCreateDirectoryIfMissing (\exists -> bind f (next exists)) createParents filename )

                ( s1, LockFile next path ) ->
                    ( s1, LockFile (\() -> bind f (next ())) path )

                ( s1, UnlockFile next path ) ->
                    ( s1, UnlockFile (\() -> bind f (next ())) path )

                ( s1, DirGetModificationTime next path ) ->
                    ( s1, DirGetModificationTime (\value -> bind f (next value)) path )

                ( s1, DirDoesDirectoryExist next path ) ->
                    ( s1, DirDoesDirectoryExist (\value -> bind f (next value)) path )

                ( s1, DirCanonicalizePath next path ) ->
                    ( s1, DirCanonicalizePath (\value -> bind f (next value)) path )

                ( s1, DirListDirectory next path ) ->
                    ( s1, DirListDirectory (\value -> bind f (next value)) path )

                ( s1, BinaryDecodeFileOrFail next filename ) ->
                    ( s1, BinaryDecodeFileOrFail (\value -> bind f (next value)) filename )

                ( s1, Write next fd content ) ->
                    ( s1, Write (\() -> bind f (next ())) fd content )

                ( s1, DirRemoveFile next path ) ->
                    ( s1, DirRemoveFile (\() -> bind f (next ())) path )

                ( s1, DirRemoveDirectoryRecursive next path ) ->
                    ( s1, DirRemoveDirectoryRecursive (\() -> bind f (next ())) path )

                ( s1, DirWithCurrentDirectory next path ) ->
                    ( s1, DirWithCurrentDirectory (\() -> bind f (next ())) path )

                ( s1, ReplGetInputLineWithInitial next prompt left right ) ->
                    ( s1, ReplGetInputLineWithInitial (\value -> bind f (next value)) prompt left right )

                ( s1, NewEmptyMVar next newValue ) ->
                    ( s1, NewEmptyMVar (\value -> bind f (next value)) newValue )

                ( s1, ReadMVar next mVarValue ) ->
                    ( s1, ReadMVar (\value -> bind f (next value)) mVarValue )

                ( s1, TakeMVar next mVarValue maybePutIndex ) ->
                    ( s1, TakeMVar (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, PutMVar next readIndexes value ) ->
                    ( s1, PutMVar (\() -> bind f (next ())) readIndexes value )
        )


unIO : IO a -> (Int -> RealWorld -> ( RealWorld, ION a ))
unIO (IO a) =
    a


mapM : (a -> IO b) -> List a -> IO (List b)
mapM f =
    List.foldr (\a -> bind (\c -> fmap (\va -> va :: c) (f a)))
        (pure [])



-- Files and handles


type alias FilePath =
    String


type Handle
    = Handle Int



-- Standard handles


stdout : Handle
stdout =
    Handle 1


stderr : Handle
stderr =
    Handle 2



-- Opening files


withFile : String -> IOMode -> (Handle -> IO a) -> IO a
withFile path mode callback =
    IO (\_ s -> ( s, WithFile pure path mode ))
        |> bind (Handle >> callback)


type IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode



-- Closing files


hClose : Handle -> IO ()
hClose handle =
    IO (\_ s -> ( s, HClose pure handle ))



-- File locking


hFileSize : Handle -> IO Int
hFileSize handle =
    IO (\_ s -> ( s, HFileSize pure handle ))



-- Buffering operations


hFlush : Handle -> IO ()
hFlush handle =
    IO (\_ s -> ( s, HFlush pure handle ))



-- Terminal operations (not portable: GHC only)


hIsTerminalDevice : Handle -> IO Bool
hIsTerminalDevice _ =
    pure True



-- Text output


hPutStr : Handle -> String -> IO ()
hPutStr handle content =
    IO (\_ s -> ( s, HPutStr pure handle content ))


hPutStrLn : Handle -> String -> IO ()
hPutStrLn handle content =
    hPutStr handle (content ++ "\n")



-- Special cases for standard input and output


putStr : String -> IO ()
putStr =
    hPutStr stdout


putStrLn : String -> IO ()
putStrLn s =
    putStr (s ++ "\n")


getLine : IO String
getLine =
    IO (\_ s -> ( s, GetLine pure ))



-- Repl State (Terminal.Repl)


type ReplState
    = ReplState (Dict String String) (Dict String String) (Dict String String)


initialReplState : ReplState
initialReplState =
    ReplState Dict.empty Dict.empty Dict.empty
