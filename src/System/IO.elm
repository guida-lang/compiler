port module System.IO exposing
    ( Program, Flags, Model, Msg, Next, run
    , IO(..), ION(..), RealWorld, pure, apply, fmap, bind
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
    , MVarSubscriber(..)
    , MVarSubscriber_Maybe_BED_Status(..)
    , MVarSubscriber_Maybe_BED_DResult(..)
    , MVarSubscriber_Maybe_CASTO_GlobalGraph(..)
    , MVarSubscriber_Maybe_CASTO_LocalGraph(..)
    , MVarSubscriber_BB_BResult(..)
    , MVarSubscriber_BB_Status(..)
    , MVarSubscriber_BB_StatusDict(..)
    , MVarSubscriber_ResultRegistryProblemEnv(..)
    , MVarSubscriber_CED_Dep(..)
    , MVarSubscriber_Maybe_CECTE_Types(..)
    , MVarSubscriber_Maybe_BB_Dependencies(..)
    )

{-| Ref.: <https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html>

@docs Program, Flags, Model, Msg, Next, run


# The IO monad

@docs IO, ION, RealWorld, pure, apply, fmap, bind


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


# MVar

@docs MVarSubscriber
@docs MVarSubscriber_Maybe_BED_Status
@docs MVarSubscriber_Maybe_BED_DResult
@docs MVarSubscriber_Maybe_CASTO_GlobalGraph
@docs MVarSubscriber_Maybe_CASTO_LocalGraph
@docs MVarSubscriber_BB_BResult
@docs MVarSubscriber_BB_Status
@docs MVarSubscriber_BB_StatusDict
@docs MVarSubscriber_ResultRegistryProblemEnv
@docs MVarSubscriber_CED_Dep
@docs MVarSubscriber_Maybe_CECTE_Types
@docs MVarSubscriber_Maybe_BB_Dependencies

-}

import Array exposing (Array)
import Codec.Archive.Zip as Zip
import Dict exposing (Dict)
import Json.Encode as Encode
import Types as T
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
                    { args = flags.args
                    , currentDirectory = flags.currentDirectory
                    , envVars = Dict.fromList flags.envVars
                    , homedir = flags.homedir
                    , progName = flags.progName
                    , state = initialReplState
                    , mVars = Array.empty
                    , mVars_Maybe_BED_Status = Array.empty
                    , mVars_Maybe_BED_DResult = Array.empty
                    , mVars_Maybe_CASTO_LocalGraph = Array.empty
                    , mVars_Maybe_CASTO_GlobalGraph = Array.empty
                    , mVars_BB_BResult = Array.empty
                    , mVars_BB_Status = Array.empty
                    , mVars_BB_StatusDict = Array.empty
                    , mVars_ResultRegistryProblemEnv = Array.empty
                    , mVars_CED_Dep = Array.empty
                    , mVars_Maybe_CECTE_Types = Array.empty
                    , mVars_Maybe_BB_Dependencies = Array.empty
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
    | BinaryDecodeFileOrFailNext (Encode.Value -> IO ())
    | WriteNext (() -> IO ())
    | DirRemoveFileNext (() -> IO ())
    | DirRemoveDirectoryRecursiveNext (() -> IO ())
    | DirWithCurrentDirectoryNext (() -> IO ())
    | ReplGetInputLineWithInitialNext (Maybe String -> IO ())
      -- MVars
    | NewEmptyMVarNext (Int -> IO ())
    | ReadMVarNext (Encode.Value -> IO ())
    | TakeMVarNext (Encode.Value -> IO ())
    | PutMVarNext (() -> IO ())
      -- MVars (Maybe T.BED_Status)
    | NewEmptyMVarNext_Maybe_BED_Status (Int -> IO ())
    | ReadMVarNext_Maybe_BED_Status (Maybe T.BED_Status -> IO ())
    | PutMVarNext_Maybe_BED_Status (() -> IO ())
      -- MVars (Maybe T.BED_DResult)
    | NewEmptyMVarNext_Maybe_BED_DResult (Int -> IO ())
    | ReadMVarNext_Maybe_BED_DResult (Maybe T.BED_DResult -> IO ())
    | PutMVarNext_Maybe_BED_DResult (() -> IO ())
      -- MVars (Maybe T.CASTO_LocalGraph)
    | NewEmptyMVarNext_Maybe_CASTO_LocalGraph (Int -> IO ())
    | ReadMVarNext_Maybe_CASTO_LocalGraph (Maybe T.CASTO_LocalGraph -> IO ())
    | PutMVarNext_Maybe_CASTO_LocalGraph (() -> IO ())
      -- MVars (Maybe T.CASTO_GlobalGraph)
    | NewEmptyMVarNext_Maybe_CASTO_GlobalGraph (Int -> IO ())
    | ReadMVarNext_Maybe_CASTO_GlobalGraph (Maybe T.CASTO_GlobalGraph -> IO ())
    | PutMVarNext_Maybe_CASTO_GlobalGraph (() -> IO ())
      -- MVars (T.BB_BResult)
    | NewEmptyMVarNext_BB_BResult (Int -> IO ())
    | ReadMVarNext_BB_BResult (T.BB_BResult -> IO ())
    | PutMVarNext_BB_BResult (() -> IO ())
      -- MVars (T.BB_Status)
    | NewEmptyMVarNext_BB_Status (Int -> IO ())
    | ReadMVarNext_BB_Status (T.BB_Status -> IO ())
    | PutMVarNext_BB_Status (() -> IO ())
      -- MVars (T.BB_StatusDict)
    | NewEmptyMVarNext_BB_StatusDict (Int -> IO ())
    | ReadMVarNext_BB_StatusDict (T.BB_StatusDict -> IO ())
    | TakeMVarNext_BB_StatusDict (T.BB_StatusDict -> IO ())
    | PutMVarNext_BB_StatusDict (() -> IO ())
      -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
    | NewEmptyMVarNext_ResultRegistryProblemEnv (Int -> IO ())
    | ReadMVarNext_ResultRegistryProblemEnv (Result T.BRE_RegistryProblem T.BDS_Env -> IO ())
    | TakeMVarNext_ResultRegistryProblemEnv (Result T.BRE_RegistryProblem T.BDS_Env -> IO ())
    | PutMVarNext_ResultRegistryProblemEnv (() -> IO ())
      -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
    | NewEmptyMVarNext_CED_Dep (Int -> IO ())
    | ReadMVarNext_CED_Dep (T.CED_Dep -> IO ())
    | TakeMVarNext_CED_Dep (T.CED_Dep -> IO ())
    | PutMVarNext_CED_Dep (() -> IO ())
      -- MVars (Maybe T.CECTE_Types)
    | NewEmptyMVarNext_Maybe_CECTE_Types (Int -> IO ())
    | ReadMVarNext_Maybe_CECTE_Types (Maybe T.CECTE_Types -> IO ())
    | TakeMVarNext_Maybe_CECTE_Types (Maybe T.CECTE_Types -> IO ())
    | PutMVarNext_Maybe_CECTE_Types (() -> IO ())
      -- MVars (Maybe T.BB_Dependencies)
    | NewEmptyMVarNext_Maybe_BB_Dependencies (Int -> IO ())
    | ReadMVarNext_Maybe_BB_Dependencies (Maybe T.BB_Dependencies -> IO ())
    | TakeMVarNext_Maybe_BB_Dependencies (Maybe T.BB_Dependencies -> IO ())
    | PutMVarNext_Maybe_BB_Dependencies (() -> IO ())


type Msg
    = PureMsg Int (IO ())
    | GetLineMsg Int String
    | HPutLineMsg Int
    | WriteStringMsg Int
    | ReadMsg Int String
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
    | BinaryDecodeFileOrFailMsg Int Encode.Value
    | WriteMsg Int
    | DirRemoveFileMsg Int
    | DirRemoveDirectoryRecursiveMsg Int
    | DirWithCurrentDirectoryMsg Int
    | ReplGetInputLineWithInitialMsg Int (Maybe String)
      -- MVars
    | NewEmptyMVarMsg Int Int
    | ReadMVarMsg Int Encode.Value
    | PutMVarMsg Int
      -- MVars (Maybe T.BED_Status)
    | NewEmptyMVarMsg_Maybe_BED_Status Int Int
    | ReadMVarMsg_Maybe_BED_Status Int (Maybe T.BED_Status)
    | PutMVarMsg_Maybe_BED_Status Int
      -- MVars (Maybe T.BED_DResult)
    | NewEmptyMVarMsg_Maybe_BED_DResult Int Int
    | ReadMVarMsg_Maybe_BED_DResult Int (Maybe T.BED_DResult)
    | PutMVarMsg_Maybe_BED_DResult Int
      -- MVars (Maybe T.CASTO_LocalGraph)
    | NewEmptyMVarMsg_Maybe_CASTO_LocalGraph Int Int
    | ReadMVarMsg_Maybe_CASTO_LocalGraph Int (Maybe T.CASTO_LocalGraph)
    | PutMVarMsg_Maybe_CASTO_LocalGraph Int
      -- MVars (Maybe T.CASTO_GlobalGraph)
    | NewEmptyMVarMsg_Maybe_CASTO_GlobalGraph Int Int
    | ReadMVarMsg_Maybe_CASTO_GlobalGraph Int (Maybe T.CASTO_GlobalGraph)
    | PutMVarMsg_Maybe_CASTO_GlobalGraph Int
      -- MVars (T.BB_BResult)
    | NewEmptyMVarMsg_BB_BResult Int Int
    | ReadMVarMsg_BB_BResult Int T.BB_BResult
    | PutMVarMsg_BB_BResult Int
      -- MVars (T.BB_Status)
    | NewEmptyMVarMsg_BB_Status Int Int
    | ReadMVarMsg_BB_Status Int T.BB_Status
    | PutMVarMsg_BB_Status Int
      -- MVars (T.BB_StatusDict)
    | NewEmptyMVarMsg_BB_StatusDict Int Int
    | ReadMVarMsg_BB_StatusDict Int T.BB_StatusDict
    | PutMVarMsg_BB_StatusDict Int
      -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
    | NewEmptyMVarMsg_ResultRegistryProblemEnv Int Int
    | ReadMVarMsg_ResultRegistryProblemEnv Int (Result T.BRE_RegistryProblem T.BDS_Env)
    | PutMVarMsg_ResultRegistryProblemEnv Int
      -- MVars (T.CED_Dep)
    | NewEmptyMVarMsg_CED_Dep Int Int
    | ReadMVarMsg_CED_Dep Int T.CED_Dep
    | PutMVarMsg_CED_Dep Int
      -- MVars (Maybe T.CECTE_Types)
    | NewEmptyMVarMsg_Maybe_CECTE_Types Int Int
    | ReadMVarMsg_Maybe_CECTE_Types Int (Maybe T.CECTE_Types)
    | PutMVarMsg_Maybe_CECTE_Types Int
      -- MVars (Maybe T.BB_Dependencies)
    | NewEmptyMVarMsg_Maybe_BB_Dependencies Int Int
    | ReadMVarMsg_Maybe_BB_Dependencies Int (Maybe T.BB_Dependencies)
    | PutMVarMsg_Maybe_BB_Dependencies Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PureMsg index (IO fn) ->
            case fn index model of
                ( newRealWorld, Pure () ) ->
                    ( newRealWorld
                    , if index == 0 then
                        sendExitWith 0

                      else
                        Cmd.none
                    )

                ( newRealWorld, ForkIO next forkIO ) ->
                    let
                        ( updatedModel, updatedCmd ) =
                            update (PureMsg index (next ())) newRealWorld
                    in
                    update (PureMsg (Dict.size updatedModel.next) forkIO) updatedModel
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ updatedCmd, cmd ])

                ( newRealWorld, GetLine next ) ->
                    ( { newRealWorld | next = Dict.insert index (GetLineNext next) model.next }, sendGetLine index )

                ( newRealWorld, HPutStr next (Handle fd) content ) ->
                    ( { newRealWorld | next = Dict.insert index (HPutLineNext next) model.next }, sendHPutStr { index = index, fd = fd, content = content } )

                ( newRealWorld, WriteString next path content ) ->
                    ( { newRealWorld | next = Dict.insert index (WriteStringNext next) model.next }, sendWriteString { index = index, path = path, content = content } )

                ( newRealWorld, Read next fd ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadNext next) model.next }, sendRead { index = index, fd = fd } )

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
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg index) { newRealWorld | next = Dict.insert index (PutMVarNext next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar next _ Nothing ) ->
                    update (PutMVarMsg index) { newRealWorld | next = Dict.insert index (PutMVarNext next) model.next }

                -- MVars (Maybe T.BED_Status)
                ( newRealWorld, NewEmptyMVar_Maybe_BED_Status next value ) ->
                    update (NewEmptyMVarMsg_Maybe_BED_Status index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_Maybe_BED_Status next) model.next }

                ( newRealWorld, ReadMVar_Maybe_BED_Status next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_BED_Status index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_BED_Status next) model.next }

                ( newRealWorld, ReadMVar_Maybe_BED_Status next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_BED_Status next) model.next }, Cmd.none )

                ( newRealWorld, PutMVar_Maybe_BED_Status next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_BED_Status readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_BED_Status index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_BED_Status next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_Maybe_BED_Status next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_BED_Status index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_BED_Status next) model.next }

                -- MVars (Maybe T.BED_DResult)
                ( newRealWorld, NewEmptyMVar_Maybe_BED_DResult next value ) ->
                    update (NewEmptyMVarMsg_Maybe_BED_DResult index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_Maybe_BED_DResult next) model.next }

                ( newRealWorld, ReadMVar_Maybe_BED_DResult next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_BED_DResult index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_BED_DResult next) model.next }

                ( newRealWorld, ReadMVar_Maybe_BED_DResult next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_BED_DResult next) model.next }, Cmd.none )

                ( newRealWorld, PutMVar_Maybe_BED_DResult next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_BED_DResult readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_BED_DResult index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_BED_DResult next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_Maybe_BED_DResult next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_BED_DResult index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_BED_DResult next) model.next }

                -- MVars (Maybe T.CASTO_LocalGraph)
                ( newRealWorld, NewEmptyMVar_Maybe_CASTO_LocalGraph next value ) ->
                    update (NewEmptyMVarMsg_Maybe_CASTO_LocalGraph index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_Maybe_CASTO_LocalGraph next) model.next }

                ( newRealWorld, ReadMVar_Maybe_CASTO_LocalGraph next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_CASTO_LocalGraph index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_CASTO_LocalGraph next) model.next }

                ( newRealWorld, ReadMVar_Maybe_CASTO_LocalGraph next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_CASTO_LocalGraph next) model.next }, Cmd.none )

                ( newRealWorld, PutMVar_Maybe_CASTO_LocalGraph next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_CASTO_LocalGraph readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_CASTO_LocalGraph index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_CASTO_LocalGraph next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_Maybe_CASTO_LocalGraph next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_CASTO_LocalGraph index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_CASTO_LocalGraph next) model.next }

                -- MVars (Maybe T.CASTO_GlobalGraph)
                ( newRealWorld, NewEmptyMVar_Maybe_CASTO_GlobalGraph next value ) ->
                    update (NewEmptyMVarMsg_Maybe_CASTO_GlobalGraph index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_Maybe_CASTO_GlobalGraph next) model.next }

                ( newRealWorld, ReadMVar_Maybe_CASTO_GlobalGraph next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_CASTO_GlobalGraph index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_CASTO_GlobalGraph next) model.next }

                ( newRealWorld, ReadMVar_Maybe_CASTO_GlobalGraph next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_CASTO_GlobalGraph next) model.next }, Cmd.none )

                ( newRealWorld, PutMVar_Maybe_CASTO_GlobalGraph next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_CASTO_GlobalGraph readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_CASTO_GlobalGraph index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_CASTO_GlobalGraph next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_Maybe_CASTO_GlobalGraph next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_CASTO_GlobalGraph index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_CASTO_GlobalGraph next) model.next }

                -- MVars (T.BB_BResult)
                ( newRealWorld, NewEmptyMVar_BB_BResult next value ) ->
                    update (NewEmptyMVarMsg_BB_BResult index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_BB_BResult next) model.next }

                ( newRealWorld, ReadMVar_BB_BResult next (Just value) ) ->
                    update (ReadMVarMsg_BB_BResult index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_BB_BResult next) model.next }

                ( newRealWorld, ReadMVar_BB_BResult next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_BB_BResult next) model.next }, Cmd.none )

                ( newRealWorld, PutMVar_BB_BResult next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BB_BResult readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BB_BResult index) { newRealWorld | next = Dict.insert index (PutMVarNext_BB_BResult next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_BB_BResult next _ Nothing ) ->
                    update (PutMVarMsg_BB_BResult index) { newRealWorld | next = Dict.insert index (PutMVarNext_BB_BResult next) model.next }

                -- MVars (T.BB_Status)
                ( newRealWorld, NewEmptyMVar_BB_Status next value ) ->
                    update (NewEmptyMVarMsg_BB_Status index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_BB_Status next) model.next }

                ( newRealWorld, ReadMVar_BB_Status next (Just value) ) ->
                    update (ReadMVarMsg_BB_Status index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_BB_Status next) model.next }

                ( newRealWorld, ReadMVar_BB_Status next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_BB_Status next) model.next }, Cmd.none )

                ( newRealWorld, PutMVar_BB_Status next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BB_Status readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BB_Status index) { newRealWorld | next = Dict.insert index (PutMVarNext_BB_Status next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_BB_Status next _ Nothing ) ->
                    update (PutMVarMsg_BB_Status index) { newRealWorld | next = Dict.insert index (PutMVarNext_BB_Status next) model.next }

                -- MVars (T.BB_StatusDict)
                ( newRealWorld, NewEmptyMVar_BB_StatusDict next value ) ->
                    update (NewEmptyMVarMsg_BB_StatusDict index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_BB_StatusDict next) model.next }

                ( newRealWorld, ReadMVar_BB_StatusDict next (Just value) ) ->
                    update (ReadMVarMsg_BB_StatusDict index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_BB_StatusDict next) model.next }

                ( newRealWorld, ReadMVar_BB_StatusDict next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_BB_StatusDict next) model.next }, Cmd.none )

                ( newRealWorld, TakeMVar_BB_StatusDict next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_BB_StatusDict index value) { newRealWorld | next = Dict.insert index (TakeMVarNext_BB_StatusDict next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, TakeMVar_BB_StatusDict next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (TakeMVarNext_BB_StatusDict next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, PutMVar_BB_StatusDict next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BB_StatusDict readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BB_StatusDict index) { newRealWorld | next = Dict.insert index (PutMVarNext_BB_StatusDict next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_BB_StatusDict next _ Nothing ) ->
                    update (PutMVarMsg_BB_StatusDict index) { newRealWorld | next = Dict.insert index (PutMVarNext_BB_StatusDict next) model.next }

                -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
                ( newRealWorld, NewEmptyMVar_ResultRegistryProblemEnv next value ) ->
                    update (NewEmptyMVarMsg_ResultRegistryProblemEnv index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_ResultRegistryProblemEnv next) model.next }

                ( newRealWorld, ReadMVar_ResultRegistryProblemEnv next (Just value) ) ->
                    update (ReadMVarMsg_ResultRegistryProblemEnv index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_ResultRegistryProblemEnv next) model.next }

                ( newRealWorld, ReadMVar_ResultRegistryProblemEnv next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_ResultRegistryProblemEnv next) model.next }, Cmd.none )

                ( newRealWorld, TakeMVar_ResultRegistryProblemEnv next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_ResultRegistryProblemEnv index value) { newRealWorld | next = Dict.insert index (TakeMVarNext_ResultRegistryProblemEnv next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, TakeMVar_ResultRegistryProblemEnv next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (TakeMVarNext_ResultRegistryProblemEnv next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, PutMVar_ResultRegistryProblemEnv next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_ResultRegistryProblemEnv readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_ResultRegistryProblemEnv index) { newRealWorld | next = Dict.insert index (PutMVarNext_ResultRegistryProblemEnv next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_ResultRegistryProblemEnv next _ Nothing ) ->
                    update (PutMVarMsg_ResultRegistryProblemEnv index) { newRealWorld | next = Dict.insert index (PutMVarNext_ResultRegistryProblemEnv next) model.next }

                -- MVars (T.CED_Dep)
                ( newRealWorld, NewEmptyMVar_CED_Dep next value ) ->
                    update (NewEmptyMVarMsg_CED_Dep index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_CED_Dep next) model.next }

                ( newRealWorld, ReadMVar_CED_Dep next (Just value) ) ->
                    update (ReadMVarMsg_CED_Dep index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_CED_Dep next) model.next }

                ( newRealWorld, ReadMVar_CED_Dep next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_CED_Dep next) model.next }, Cmd.none )

                ( newRealWorld, TakeMVar_CED_Dep next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_CED_Dep index value) { newRealWorld | next = Dict.insert index (TakeMVarNext_CED_Dep next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, TakeMVar_CED_Dep next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (TakeMVarNext_CED_Dep next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, PutMVar_CED_Dep next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_CED_Dep readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_CED_Dep index) { newRealWorld | next = Dict.insert index (PutMVarNext_CED_Dep next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_CED_Dep next _ Nothing ) ->
                    update (PutMVarMsg_CED_Dep index) { newRealWorld | next = Dict.insert index (PutMVarNext_CED_Dep next) model.next }

                -- MVars (Maybe T.CECTE_Types)
                ( newRealWorld, NewEmptyMVar_Maybe_CECTE_Types next value ) ->
                    update (NewEmptyMVarMsg_Maybe_CECTE_Types index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_Maybe_CECTE_Types next) model.next }

                ( newRealWorld, ReadMVar_Maybe_CECTE_Types next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_CECTE_Types index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_CECTE_Types next) model.next }

                ( newRealWorld, ReadMVar_Maybe_CECTE_Types next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_CECTE_Types next) model.next }, Cmd.none )

                ( newRealWorld, TakeMVar_Maybe_CECTE_Types next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_Maybe_CECTE_Types index value) { newRealWorld | next = Dict.insert index (TakeMVarNext_Maybe_CECTE_Types next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, TakeMVar_Maybe_CECTE_Types next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (TakeMVarNext_Maybe_CECTE_Types next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, PutMVar_Maybe_CECTE_Types next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_CECTE_Types readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_CECTE_Types index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_CECTE_Types next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_Maybe_CECTE_Types next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_CECTE_Types index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_CECTE_Types next) model.next }

                -- MVars (Maybe T.BB_Dependencies)
                ( newRealWorld, NewEmptyMVar_Maybe_BB_Dependencies next value ) ->
                    update (NewEmptyMVarMsg_Maybe_BB_Dependencies index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext_Maybe_BB_Dependencies next) model.next }

                ( newRealWorld, ReadMVar_Maybe_BB_Dependencies next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_BB_Dependencies index value) { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_BB_Dependencies next) model.next }

                ( newRealWorld, ReadMVar_Maybe_BB_Dependencies next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext_Maybe_BB_Dependencies next) model.next }, Cmd.none )

                ( newRealWorld, TakeMVar_Maybe_BB_Dependencies next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_Maybe_BB_Dependencies index value) { newRealWorld | next = Dict.insert index (TakeMVarNext_Maybe_BB_Dependencies next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, TakeMVar_Maybe_BB_Dependencies next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (TakeMVarNext_Maybe_BB_Dependencies next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, PutMVar_Maybe_BB_Dependencies next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_BB_Dependencies readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_BB_Dependencies index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_BB_Dependencies next) model.next })
                        readIndexes

                ( newRealWorld, PutMVar_Maybe_BB_Dependencies next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_BB_Dependencies index) { newRealWorld | next = Dict.insert index (PutMVarNext_Maybe_BB_Dependencies next) model.next }

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

        -- MVars
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

        -- MVars (Maybe T.BED_Status)
        NewEmptyMVarMsg_Maybe_BED_Status index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_Maybe_BED_Status fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_BED_Status"

        ReadMVarMsg_Maybe_BED_Status index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_Maybe_BED_Status fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_BED_Status"

        PutMVarMsg_Maybe_BED_Status index ->
            case Dict.get index model.next of
                Just (PutMVarNext_Maybe_BED_Status fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_BED_Status"

        -- MVars (Maybe T.BED_DResult)
        NewEmptyMVarMsg_Maybe_BED_DResult index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_Maybe_BED_DResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_BED_DResult"

        ReadMVarMsg_Maybe_BED_DResult index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_Maybe_BED_DResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_BED_DResult"

        PutMVarMsg_Maybe_BED_DResult index ->
            case Dict.get index model.next of
                Just (PutMVarNext_Maybe_BED_DResult fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_BED_DResult"

        -- MVars (Maybe T.CASTO_LocalGraph)
        NewEmptyMVarMsg_Maybe_CASTO_LocalGraph index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_Maybe_CASTO_LocalGraph fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_CASTO_LocalGraph"

        ReadMVarMsg_Maybe_CASTO_LocalGraph index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_Maybe_CASTO_LocalGraph fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_CASTO_LocalGraph"

        PutMVarMsg_Maybe_CASTO_LocalGraph index ->
            case Dict.get index model.next of
                Just (PutMVarNext_Maybe_CASTO_LocalGraph fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_CASTO_LocalGraph"

        -- MVars (Maybe T.CASTO_GlobalGraph)
        NewEmptyMVarMsg_Maybe_CASTO_GlobalGraph index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_Maybe_CASTO_GlobalGraph fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_CASTO_GlobalGraph"

        ReadMVarMsg_Maybe_CASTO_GlobalGraph index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_Maybe_CASTO_GlobalGraph fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_CASTO_GlobalGraph"

        PutMVarMsg_Maybe_CASTO_GlobalGraph index ->
            case Dict.get index model.next of
                Just (PutMVarNext_Maybe_CASTO_GlobalGraph fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_CASTO_GlobalGraph"

        -- MVars (T.BB_BResult)
        NewEmptyMVarMsg_BB_BResult index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_BB_BResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BB_BResult"

        ReadMVarMsg_BB_BResult index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_BB_BResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BB_BResult"

        PutMVarMsg_BB_BResult index ->
            case Dict.get index model.next of
                Just (PutMVarNext_BB_BResult fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BB_BResult"

        -- MVars (T.BB_Status)
        NewEmptyMVarMsg_BB_Status index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_BB_Status fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BB_Status"

        ReadMVarMsg_BB_Status index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_BB_Status fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BB_Status"

        PutMVarMsg_BB_Status index ->
            case Dict.get index model.next of
                Just (PutMVarNext_BB_Status fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BB_Status"

        -- MVars (T.BB_StatusDict)
        NewEmptyMVarMsg_BB_StatusDict index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_BB_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BB_StatusDict"

        ReadMVarMsg_BB_StatusDict index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_BB_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                Just (TakeMVarNext_BB_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BB_StatusDict"

        PutMVarMsg_BB_StatusDict index ->
            case Dict.get index model.next of
                Just (PutMVarNext_BB_StatusDict fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BB_StatusDict"

        -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
        NewEmptyMVarMsg_ResultRegistryProblemEnv index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_ResultRegistryProblemEnv fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_ResultRegistryProblemEnv"

        ReadMVarMsg_ResultRegistryProblemEnv index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_ResultRegistryProblemEnv fn) ->
                    update (PureMsg index (fn value)) model

                Just (TakeMVarNext_ResultRegistryProblemEnv fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_ResultRegistryProblemEnv"

        PutMVarMsg_ResultRegistryProblemEnv index ->
            case Dict.get index model.next of
                Just (PutMVarNext_ResultRegistryProblemEnv fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_ResultRegistryProblemEnv"

        -- MVars (T.CED_Dep)
        NewEmptyMVarMsg_CED_Dep index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_CED_Dep fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_CED_Dep"

        ReadMVarMsg_CED_Dep index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_CED_Dep fn) ->
                    update (PureMsg index (fn value)) model

                Just (TakeMVarNext_CED_Dep fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_CED_Dep"

        PutMVarMsg_CED_Dep index ->
            case Dict.get index model.next of
                Just (PutMVarNext_CED_Dep fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_CED_Dep"

        -- MVars (Maybe T.CECTE_Types)
        NewEmptyMVarMsg_Maybe_CECTE_Types index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_Maybe_CECTE_Types fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_CECTE_Types"

        ReadMVarMsg_Maybe_CECTE_Types index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_Maybe_CECTE_Types fn) ->
                    update (PureMsg index (fn value)) model

                Just (TakeMVarNext_Maybe_CECTE_Types fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_CECTE_Types"

        PutMVarMsg_Maybe_CECTE_Types index ->
            case Dict.get index model.next of
                Just (PutMVarNext_Maybe_CECTE_Types fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_CECTE_Types"

        -- MVars (Maybe T.BB_Dependencies)
        NewEmptyMVarMsg_Maybe_BB_Dependencies index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext_Maybe_BB_Dependencies fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_BB_Dependencies"

        ReadMVarMsg_Maybe_BB_Dependencies index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext_Maybe_BB_Dependencies fn) ->
                    update (PureMsg index (fn value)) model

                Just (TakeMVarNext_Maybe_BB_Dependencies fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_BB_Dependencies"

        PutMVarMsg_Maybe_BB_Dependencies index ->
            case Dict.get index model.next of
                Just (PutMVarNext_Maybe_BB_Dependencies fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_BB_Dependencies"


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
      -- MVars (Maybe T.BED_Status)
    | NewEmptyMVar_Maybe_BED_Status (Int -> IO a) Int
    | ReadMVar_Maybe_BED_Status (Maybe T.BED_Status -> IO a) (Maybe (Maybe T.BED_Status))
    | PutMVar_Maybe_BED_Status (() -> IO a) (List Int) (Maybe (Maybe T.BED_Status))
      -- MVars (Maybe T.BED_DResult)
    | NewEmptyMVar_Maybe_BED_DResult (Int -> IO a) Int
    | ReadMVar_Maybe_BED_DResult (Maybe T.BED_DResult -> IO a) (Maybe (Maybe T.BED_DResult))
    | PutMVar_Maybe_BED_DResult (() -> IO a) (List Int) (Maybe (Maybe T.BED_DResult))
      -- MVars (Maybe T.CASTO_LocalGraph)
    | NewEmptyMVar_Maybe_CASTO_LocalGraph (Int -> IO a) Int
    | ReadMVar_Maybe_CASTO_LocalGraph (Maybe T.CASTO_LocalGraph -> IO a) (Maybe (Maybe T.CASTO_LocalGraph))
    | PutMVar_Maybe_CASTO_LocalGraph (() -> IO a) (List Int) (Maybe (Maybe T.CASTO_LocalGraph))
      -- MVars (Maybe T.CASTO_GlobalGraph)
    | NewEmptyMVar_Maybe_CASTO_GlobalGraph (Int -> IO a) Int
    | ReadMVar_Maybe_CASTO_GlobalGraph (Maybe T.CASTO_GlobalGraph -> IO a) (Maybe (Maybe T.CASTO_GlobalGraph))
    | PutMVar_Maybe_CASTO_GlobalGraph (() -> IO a) (List Int) (Maybe (Maybe T.CASTO_GlobalGraph))
      -- MVars (T.BB_BResult)
    | NewEmptyMVar_BB_BResult (Int -> IO a) Int
    | ReadMVar_BB_BResult (T.BB_BResult -> IO a) (Maybe T.BB_BResult)
    | PutMVar_BB_BResult (() -> IO a) (List Int) (Maybe T.BB_BResult)
      -- MVars (T.BB_Status)
    | NewEmptyMVar_BB_Status (Int -> IO a) Int
    | ReadMVar_BB_Status (T.BB_Status -> IO a) (Maybe T.BB_Status)
    | PutMVar_BB_Status (() -> IO a) (List Int) (Maybe T.BB_Status)
      -- MVars (T.BB_StatusDict)
    | NewEmptyMVar_BB_StatusDict (Int -> IO a) Int
    | ReadMVar_BB_StatusDict (T.BB_StatusDict -> IO a) (Maybe T.BB_StatusDict)
    | TakeMVar_BB_StatusDict (T.BB_StatusDict -> IO a) (Maybe T.BB_StatusDict) (Maybe Int)
    | PutMVar_BB_StatusDict (() -> IO a) (List Int) (Maybe T.BB_StatusDict)
      -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
    | NewEmptyMVar_ResultRegistryProblemEnv (Int -> IO a) Int
    | ReadMVar_ResultRegistryProblemEnv (Result T.BRE_RegistryProblem T.BDS_Env -> IO a) (Maybe (Result T.BRE_RegistryProblem T.BDS_Env))
    | TakeMVar_ResultRegistryProblemEnv (Result T.BRE_RegistryProblem T.BDS_Env -> IO a) (Maybe (Result T.BRE_RegistryProblem T.BDS_Env)) (Maybe Int)
    | PutMVar_ResultRegistryProblemEnv (() -> IO a) (List Int) (Maybe (Result T.BRE_RegistryProblem T.BDS_Env))
      -- MVars (T.CED_Dep)
    | NewEmptyMVar_CED_Dep (Int -> IO a) Int
    | ReadMVar_CED_Dep (T.CED_Dep -> IO a) (Maybe T.CED_Dep)
    | TakeMVar_CED_Dep (T.CED_Dep -> IO a) (Maybe T.CED_Dep) (Maybe Int)
    | PutMVar_CED_Dep (() -> IO a) (List Int) (Maybe T.CED_Dep)
      -- MVars (Maybe T.CECTE_Types)
    | NewEmptyMVar_Maybe_CECTE_Types (Int -> IO a) Int
    | ReadMVar_Maybe_CECTE_Types (Maybe T.CECTE_Types -> IO a) (Maybe (Maybe T.CECTE_Types))
    | TakeMVar_Maybe_CECTE_Types (Maybe T.CECTE_Types -> IO a) (Maybe (Maybe T.CECTE_Types)) (Maybe Int)
    | PutMVar_Maybe_CECTE_Types (() -> IO a) (List Int) (Maybe (Maybe T.CECTE_Types))
      -- MVars (Maybe T.BB_Dependencies)
    | NewEmptyMVar_Maybe_BB_Dependencies (Int -> IO a) Int
    | ReadMVar_Maybe_BB_Dependencies (Maybe T.BB_Dependencies -> IO a) (Maybe (Maybe T.BB_Dependencies))
    | TakeMVar_Maybe_BB_Dependencies (Maybe T.BB_Dependencies -> IO a) (Maybe (Maybe T.BB_Dependencies)) (Maybe Int)
    | PutMVar_Maybe_BB_Dependencies (() -> IO a) (List Int) (Maybe (Maybe T.BB_Dependencies))


type alias RealWorld =
    { args : List String
    , currentDirectory : String
    , envVars : Dict String String
    , homedir : FilePath
    , progName : String
    , state : ReplState
    , mVars : Array { subscribers : List MVarSubscriber, value : Maybe Encode.Value }
    , mVars_Maybe_BED_Status : Array { subscribers : List MVarSubscriber_Maybe_BED_Status, value : Maybe (Maybe T.BED_Status) }
    , mVars_Maybe_BED_DResult : Array { subscribers : List MVarSubscriber_Maybe_BED_DResult, value : Maybe (Maybe T.BED_DResult) }
    , mVars_Maybe_CASTO_LocalGraph : Array { subscribers : List MVarSubscriber_Maybe_CASTO_LocalGraph, value : Maybe (Maybe T.CASTO_LocalGraph) }
    , mVars_Maybe_CASTO_GlobalGraph : Array { subscribers : List MVarSubscriber_Maybe_CASTO_GlobalGraph, value : Maybe (Maybe T.CASTO_GlobalGraph) }
    , mVars_BB_BResult : Array { subscribers : List MVarSubscriber_BB_BResult, value : Maybe T.BB_BResult }
    , mVars_BB_Status : Array { subscribers : List MVarSubscriber_BB_Status, value : Maybe T.BB_Status }
    , mVars_BB_StatusDict : Array { subscribers : List MVarSubscriber_BB_StatusDict, value : Maybe T.BB_StatusDict }
    , mVars_ResultRegistryProblemEnv : Array { subscribers : List MVarSubscriber_ResultRegistryProblemEnv, value : Maybe (Result T.BRE_RegistryProblem T.BDS_Env) }
    , mVars_CED_Dep : Array { subscribers : List MVarSubscriber_CED_Dep, value : Maybe T.CED_Dep }
    , mVars_Maybe_CECTE_Types : Array { subscribers : List MVarSubscriber_Maybe_CECTE_Types, value : Maybe (Maybe T.CECTE_Types) }
    , mVars_Maybe_BB_Dependencies : Array { subscribers : List MVarSubscriber_Maybe_BB_Dependencies, value : Maybe (Maybe T.BB_Dependencies) }
    , next : Dict Int Next
    }


type MVarSubscriber
    = ReadMVarSubscriber Int
    | TakeMVarSubscriber Int
    | PutMVarSubscriber Int Encode.Value


type MVarSubscriber_Maybe_BED_Status
    = ReadMVarSubscriber_Maybe_BED_Status Int
    | TakeMVarSubscriber_Maybe_BED_Status Int
    | PutMVarSubscriber_Maybe_BED_Status Int (Maybe T.BED_Status)


type MVarSubscriber_Maybe_BED_DResult
    = ReadMVarSubscriber_Maybe_BED_DResult Int
    | TakeMVarSubscriber_Maybe_BED_DResult Int
    | PutMVarSubscriber_Maybe_BED_DResult Int (Maybe T.BED_DResult)


type MVarSubscriber_Maybe_CASTO_LocalGraph
    = ReadMVarSubscriber_Maybe_CASTO_LocalGraph Int
    | TakeMVarSubscriber_Maybe_CASTO_LocalGraph Int
    | PutMVarSubscriber_Maybe_CASTO_LocalGraph Int (Maybe T.CASTO_LocalGraph)


type MVarSubscriber_Maybe_CASTO_GlobalGraph
    = ReadMVarSubscriber_Maybe_CASTO_GlobalGraph Int
    | TakeMVarSubscriber_Maybe_CASTO_GlobalGraph Int
    | PutMVarSubscriber_Maybe_CASTO_GlobalGraph Int (Maybe T.CASTO_GlobalGraph)


type MVarSubscriber_BB_BResult
    = ReadMVarSubscriber_BB_BResult Int
    | TakeMVarSubscriber_BB_BResult Int
    | PutMVarSubscriber_BB_BResult Int T.BB_BResult


type MVarSubscriber_BB_Status
    = ReadMVarSubscriber_BB_Status Int
    | TakeMVarSubscriber_BB_Status Int
    | PutMVarSubscriber_BB_Status Int T.BB_Status


type MVarSubscriber_BB_StatusDict
    = ReadMVarSubscriber_BB_StatusDict Int
    | TakeMVarSubscriber_BB_StatusDict Int
    | PutMVarSubscriber_BB_StatusDict Int T.BB_StatusDict


type MVarSubscriber_ResultRegistryProblemEnv
    = ReadMVarSubscriber_ResultRegistryProblemEnv Int
    | TakeMVarSubscriber_ResultRegistryProblemEnv Int
    | PutMVarSubscriber_ResultRegistryProblemEnv Int (Result T.BRE_RegistryProblem T.BDS_Env)


type MVarSubscriber_CED_Dep
    = ReadMVarSubscriber_CED_Dep Int
    | TakeMVarSubscriber_CED_Dep Int
    | PutMVarSubscriber_CED_Dep Int T.CED_Dep


type MVarSubscriber_Maybe_CECTE_Types
    = ReadMVarSubscriber_Maybe_CECTE_Types Int
    | TakeMVarSubscriber_Maybe_CECTE_Types Int
    | PutMVarSubscriber_Maybe_CECTE_Types Int (Maybe T.CECTE_Types)


type MVarSubscriber_Maybe_BB_Dependencies
    = ReadMVarSubscriber_Maybe_BB_Dependencies Int
    | TakeMVarSubscriber_Maybe_BB_Dependencies Int
    | PutMVarSubscriber_Maybe_BB_Dependencies Int (Maybe T.BB_Dependencies)


pure : a -> IO a
pure x =
    IO (\_ s -> ( s, Pure x ))


apply : IO a -> IO (a -> b) -> IO b
apply ma mf =
    bind (\f -> bind (\a -> pure (f a)) ma) mf


fmap : (a -> b) -> IO a -> IO b
fmap fn ma =
    bind (\a -> pure (fn a)) ma


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

                -- MVars
                ( s1, NewEmptyMVar next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar next mVarValue ) ->
                    ( s1, ReadMVar (\value -> bind f (next value)) mVarValue )

                ( s1, TakeMVar next mVarValue maybePutIndex ) ->
                    ( s1, TakeMVar (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, PutMVar next readIndexes value ) ->
                    ( s1, PutMVar (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.BED_Status)
                ( s1, NewEmptyMVar_Maybe_BED_Status next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_Maybe_BED_Status (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_Maybe_BED_Status next mVarValue ) ->
                    ( s1, ReadMVar_Maybe_BED_Status (\value -> bind f (next value)) mVarValue )

                ( s1, PutMVar_Maybe_BED_Status next readIndexes value ) ->
                    ( s1, PutMVar_Maybe_BED_Status (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.BED_DResult)
                ( s1, NewEmptyMVar_Maybe_BED_DResult next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_Maybe_BED_DResult (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_Maybe_BED_DResult next mVarValue ) ->
                    ( s1, ReadMVar_Maybe_BED_DResult (\value -> bind f (next value)) mVarValue )

                ( s1, PutMVar_Maybe_BED_DResult next readIndexes value ) ->
                    ( s1, PutMVar_Maybe_BED_DResult (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.CASTO_LocalGraph)
                ( s1, NewEmptyMVar_Maybe_CASTO_LocalGraph next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_Maybe_CASTO_LocalGraph (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_Maybe_CASTO_LocalGraph next mVarValue ) ->
                    ( s1, ReadMVar_Maybe_CASTO_LocalGraph (\value -> bind f (next value)) mVarValue )

                ( s1, PutMVar_Maybe_CASTO_LocalGraph next readIndexes value ) ->
                    ( s1, PutMVar_Maybe_CASTO_LocalGraph (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.CASTO_GlobalGraph)
                ( s1, NewEmptyMVar_Maybe_CASTO_GlobalGraph next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_Maybe_CASTO_GlobalGraph (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_Maybe_CASTO_GlobalGraph next mVarValue ) ->
                    ( s1, ReadMVar_Maybe_CASTO_GlobalGraph (\value -> bind f (next value)) mVarValue )

                ( s1, PutMVar_Maybe_CASTO_GlobalGraph next readIndexes value ) ->
                    ( s1, PutMVar_Maybe_CASTO_GlobalGraph (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BB_BResult)
                ( s1, NewEmptyMVar_BB_BResult next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_BB_BResult (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_BB_BResult next mVarValue ) ->
                    ( s1, ReadMVar_BB_BResult (\value -> bind f (next value)) mVarValue )

                ( s1, PutMVar_BB_BResult next readIndexes value ) ->
                    ( s1, PutMVar_BB_BResult (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BB_Status)
                ( s1, NewEmptyMVar_BB_Status next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_BB_Status (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_BB_Status next mVarValue ) ->
                    ( s1, ReadMVar_BB_Status (\value -> bind f (next value)) mVarValue )

                ( s1, PutMVar_BB_Status next readIndexes value ) ->
                    ( s1, PutMVar_BB_Status (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BB_StatusDict)
                ( s1, NewEmptyMVar_BB_StatusDict next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_BB_StatusDict (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_BB_StatusDict next mVarValue ) ->
                    ( s1, ReadMVar_BB_StatusDict (\value -> bind f (next value)) mVarValue )

                ( s1, TakeMVar_BB_StatusDict next mVarValue maybePutIndex ) ->
                    ( s1, TakeMVar_BB_StatusDict (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, PutMVar_BB_StatusDict next readIndexes value ) ->
                    ( s1, PutMVar_BB_StatusDict (\() -> bind f (next ())) readIndexes value )

                -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
                ( s1, NewEmptyMVar_ResultRegistryProblemEnv next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_ResultRegistryProblemEnv (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_ResultRegistryProblemEnv next mVarValue ) ->
                    ( s1, ReadMVar_ResultRegistryProblemEnv (\value -> bind f (next value)) mVarValue )

                ( s1, TakeMVar_ResultRegistryProblemEnv next mVarValue maybePutIndex ) ->
                    ( s1, TakeMVar_ResultRegistryProblemEnv (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, PutMVar_ResultRegistryProblemEnv next readIndexes value ) ->
                    ( s1, PutMVar_ResultRegistryProblemEnv (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.CED_Dep)
                ( s1, NewEmptyMVar_CED_Dep next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_CED_Dep (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_CED_Dep next mVarValue ) ->
                    ( s1, ReadMVar_CED_Dep (\value -> bind f (next value)) mVarValue )

                ( s1, TakeMVar_CED_Dep next mVarValue maybePutIndex ) ->
                    ( s1, TakeMVar_CED_Dep (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, PutMVar_CED_Dep next readIndexes value ) ->
                    ( s1, PutMVar_CED_Dep (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.CECTE_Types)
                ( s1, NewEmptyMVar_Maybe_CECTE_Types next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_Maybe_CECTE_Types (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_Maybe_CECTE_Types next mVarValue ) ->
                    ( s1, ReadMVar_Maybe_CECTE_Types (\value -> bind f (next value)) mVarValue )

                ( s1, TakeMVar_Maybe_CECTE_Types next mVarValue maybePutIndex ) ->
                    ( s1, TakeMVar_Maybe_CECTE_Types (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, PutMVar_Maybe_CECTE_Types next readIndexes value ) ->
                    ( s1, PutMVar_Maybe_CECTE_Types (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.BB_Dependencies)
                ( s1, NewEmptyMVar_Maybe_BB_Dependencies next emptyMVarIndex ) ->
                    ( s1, NewEmptyMVar_Maybe_BB_Dependencies (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, ReadMVar_Maybe_BB_Dependencies next mVarValue ) ->
                    ( s1, ReadMVar_Maybe_BB_Dependencies (\value -> bind f (next value)) mVarValue )

                ( s1, TakeMVar_Maybe_BB_Dependencies next mVarValue maybePutIndex ) ->
                    ( s1, TakeMVar_Maybe_BB_Dependencies (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, PutMVar_Maybe_BB_Dependencies next readIndexes value ) ->
                    ( s1, PutMVar_Maybe_BB_Dependencies (\() -> bind f (next ())) readIndexes value )
        )


unIO : IO a -> (Int -> RealWorld -> ( RealWorld, ION a ))
unIO (IO a) =
    a



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
