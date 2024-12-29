port module System.IO exposing
    ( Program, Flags, Model, Msg, run
    , pure, apply, fmap, bind
    , stdout, stderr
    , withFile
    , hClose
    , hFileSize
    , hFlush
    , hIsTerminalDevice
    , hPutStr, hPutStrLn
    , putStr, putStrLn, getLine
    , initialReplState
    )

{-| Ref.: <https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html>

@docs Program, Flags, Model, Msg, run


# The IO monad

@docs pure, apply, fmap, bind


# Standard handles

@docs stdout, stderr


# Opening files

@docs withFile


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

@docs initialReplState

-}

import Array
import Data.Map as Map
import Dict
import Json.Encode as Encode
import Types as T
import Utils.Crash exposing (crash)


type alias Flags =
    { args : List String
    , currentDirectory : String
    , envVars : List ( String, String )
    , homedir : T.FilePath
    , progName : String
    }


type alias Program =
    Platform.Program Flags Model Msg


run : T.IO () -> Program
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
                    , mVars_DictNameMVarDep = Array.empty
                    , mVars_DictRawMVarMaybeDResult = Array.empty
                    , mVars_ListMVar = Array.empty
                    , mVars_BB_CachedInterface = Array.empty
                    , mVars_BED_StatusDict = Array.empty
                    , mVars_Unit = Array.empty
                    , mVars_Manager = Array.empty
                    , mVars_BB_ResultDict = Array.empty
                    , mVars_StreamResultBMsgBResultDocumentation = Array.empty
                    , mVars_ChItemResultBMsgBResultDocumentation = Array.empty
                    , mVars_StreamResultBMsgBResultUnit = Array.empty
                    , mVars_ChItemResultBMsgBResultUnit = Array.empty
                    , mVars_StreamResultBMsgBResultArtifacts = Array.empty
                    , mVars_ChItemResultBMsgBResultArtifacts = Array.empty
                    , mVars_ResultBMsgBResultArtifacts = Array.empty
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
    T.RealWorld


type Msg
    = PureMsg Int (T.IO ())
    | GetLineMsg Int String
    | HPutLineMsg Int
    | WriteStringMsg Int
    | ReadMsg Int String
    | HttpFetchMsg Int String
    | GetArchiveMsg Int ( String, T.CAZ_Archive )
    | HttpUploadMsg Int
    | HFlushMsg Int
    | WithFileMsg Int Int
    | HFileSizeMsg Int Int
    | ProcWithCreateProcessMsg Int { stdinHandle : Maybe Int, ph : Int }
    | HCloseMsg Int
    | ProcWaitForProcessMsg Int Int
    | DirFindExecutableMsg Int (Maybe T.FilePath)
    | ReplGetInputLineMsg Int (Maybe String)
    | DirDoesFileExistMsg Int Bool
    | DirCreateDirectoryIfMissingMsg Int
    | LockFileMsg Int
    | UnlockFileMsg Int
    | DirGetModificationTimeMsg Int Int
    | DirDoesDirectoryExistMsg Int Bool
    | DirCanonicalizePathMsg Int T.FilePath
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
      -- MVars (Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep)
    | NewEmptyMVarMsg_DictNameMVarDep Int Int
    | ReadMVarMsg_DictNameMVarDep Int (Map.Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep)
    | PutMVarMsg_DictNameMVarDep Int
      -- MVars (Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult)
    | NewEmptyMVarMsg_DictRawMVarMaybeDResult Int Int
    | ReadMVarMsg_DictRawMVarMaybeDResult Int (Map.Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult)
    | PutMVarMsg_DictRawMVarMaybeDResult Int
      -- MVars (List T.MVar_Unit)
    | NewEmptyMVarMsg_ListMVar Int Int
    | ReadMVarMsg_ListMVar Int (List T.MVar_Unit)
    | PutMVarMsg_ListMVar Int
      -- MVars (T.BB_CachedInterface)
    | NewEmptyMVarMsg_BB_CachedInterface Int Int
    | ReadMVarMsg_BB_CachedInterface Int T.BB_CachedInterface
    | PutMVarMsg_BB_CachedInterface Int
      -- MVars (T.BED_StatusDict)
    | NewEmptyMVarMsg_BED_StatusDict Int Int
    | ReadMVarMsg_BED_StatusDict Int T.BED_StatusDict
    | PutMVarMsg_BED_StatusDict Int
      -- MVars (Unit)
    | NewEmptyMVarMsg_Unit Int Int
    | ReadMVarMsg_Unit Int ()
    | PutMVarMsg_Unit Int
      -- MVars (T.BH_Manager)
    | NewEmptyMVarMsg_Manager Int Int
    | ReadMVarMsg_Manager Int T.BH_Manager
    | PutMVarMsg_Manager Int
      -- MVars (T.BB_ResultDict)
    | NewEmptyMVarMsg_BB_ResultDict Int Int
    | ReadMVarMsg_BB_ResultDict Int T.BB_ResultDict
    | PutMVarMsg_BB_ResultDict Int
      -- MVars (T.MVar_ChItemResultBMsgBResultDocumentation)
    | NewEmptyMVarMsg_StreamResultBMsgBResultDocumentation Int Int
    | ReadMVarMsg_StreamResultBMsgBResultDocumentation Int T.MVar_ChItemResultBMsgBResultDocumentation
    | PutMVarMsg_StreamResultBMsgBResultDocumentation Int
      -- MVars (T.ChItem_ResultBMsgBResultDocumentation)
    | NewEmptyMVarMsg_ChItemResultBMsgBResultDocumentation Int Int
    | ReadMVarMsg_ChItemResultBMsgBResultDocumentation Int T.ChItem_ResultBMsgBResultDocumentation
    | PutMVarMsg_ChItemResultBMsgBResultDocumentation Int
      -- MVars (T.MVar_ChItemResultBMsgBResultUnit)
    | NewEmptyMVarMsg_StreamResultBMsgBResultUnit Int Int
    | ReadMVarMsg_StreamResultBMsgBResultUnit Int T.MVar_ChItemResultBMsgBResultUnit
    | PutMVarMsg_StreamResultBMsgBResultUnit Int
      -- MVars (T.ChItem_ResultBMsgBResultUnit)
    | NewEmptyMVarMsg_ChItemResultBMsgBResultUnit Int Int
    | ReadMVarMsg_ChItemResultBMsgBResultUnit Int T.ChItem_ResultBMsgBResultUnit
    | PutMVarMsg_ChItemResultBMsgBResultUnit Int
      -- MVars (T.MVar_ChItemResultBMsgBResultArtifacts)
    | NewEmptyMVarMsg_StreamResultBMsgBResultArtifacts Int Int
    | ReadMVarMsg_StreamResultBMsgBResultArtifacts Int T.MVar_ChItemResultBMsgBResultArtifacts
    | PutMVarMsg_StreamResultBMsgBResultArtifacts Int
      -- MVars (T.ChItem_ResultBMsgBResultArtifacts)
    | NewEmptyMVarMsg_ChItemResultBMsgBResultArtifacts Int Int
    | ReadMVarMsg_ChItemResultBMsgBResultArtifacts Int T.ChItem_ResultBMsgBResultArtifacts
    | PutMVarMsg_ChItemResultBMsgBResultArtifacts Int
      -- MVars (Result T.BR_BMsg (T.BR_BResult T.BB_Artifacts))
    | NewEmptyMVarMsg_ResultBMsgBResultArtifacts Int Int
    | ReadMVarMsg_ResultBMsgBResultArtifacts Int (Result T.BR_BMsg (T.BR_BResult T.BB_Artifacts))
    | PutMVarMsg_ResultBMsgBResultArtifacts Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PureMsg index (T.IO fn) ->
            case fn index model of
                ( newRealWorld, T.Pure () ) ->
                    ( newRealWorld
                    , if index == 0 then
                        sendExitWith 0

                      else
                        Cmd.none
                    )

                ( newRealWorld, T.ForkIO next forkIO ) ->
                    let
                        ( updatedModel, updatedCmd ) =
                            update (PureMsg index (next ())) newRealWorld
                    in
                    update (PureMsg (Dict.size updatedModel.next) forkIO) updatedModel
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ updatedCmd, cmd ])

                ( newRealWorld, T.GetLine next ) ->
                    ( { newRealWorld | next = Dict.insert index (T.GetLineNext next) model.next }, sendGetLine index )

                ( newRealWorld, T.HPutStr next (T.Handle fd) content ) ->
                    ( { newRealWorld | next = Dict.insert index (T.HPutLineNext next) model.next }, sendHPutStr { index = index, fd = fd, content = content } )

                ( newRealWorld, T.WriteString next path content ) ->
                    ( { newRealWorld | next = Dict.insert index (T.WriteStringNext next) model.next }, sendWriteString { index = index, path = path, content = content } )

                ( newRealWorld, T.Read next fd ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadNext next) model.next }, sendRead { index = index, fd = fd } )

                ( newRealWorld, T.HttpFetch next method urlStr headers ) ->
                    ( { newRealWorld | next = Dict.insert index (T.HttpFetchNext next) model.next }, sendHttpFetch { index = index, method = method, urlStr = urlStr, headers = headers } )

                ( newRealWorld, T.GetArchive next method url ) ->
                    ( { newRealWorld | next = Dict.insert index (T.GetArchiveNext next) model.next }, sendGetArchive { index = index, method = method, url = url } )

                ( newRealWorld, T.HttpUpload next urlStr headers parts ) ->
                    ( { newRealWorld | next = Dict.insert index (T.HttpUploadNext next) model.next }, sendHttpUpload { index = index, urlStr = urlStr, headers = headers, parts = parts } )

                ( newRealWorld, T.HFlush next (T.Handle fd) ) ->
                    ( { newRealWorld | next = Dict.insert index (T.HFlushNext next) model.next }, sendHFlush { index = index, fd = fd } )

                ( newRealWorld, T.WithFile next path mode ) ->
                    ( { newRealWorld | next = Dict.insert index (T.WithFileNext next) model.next }
                    , sendWithFile
                        { index = index
                        , path = path
                        , mode =
                            case mode of
                                T.ReadMode ->
                                    "r"

                                T.WriteMode ->
                                    "w"

                                T.AppendMode ->
                                    "a"

                                T.ReadWriteMode ->
                                    "w+"
                        }
                    )

                ( newRealWorld, T.HFileSize next (T.Handle fd) ) ->
                    ( { newRealWorld | next = Dict.insert index (T.HFileSizeNext next) model.next }, sendHFileSize { index = index, fd = fd } )

                ( newRealWorld, T.ProcWithCreateProcess next createProcess ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ProcWithCreateProcessNext next) model.next }, sendProcWithCreateProcess { index = index, createProcess = createProcess } )

                ( newRealWorld, T.HClose next (T.Handle fd) ) ->
                    ( { newRealWorld | next = Dict.insert index (T.HCloseNext next) model.next }, sendHClose { index = index, fd = fd } )

                ( newRealWorld, T.ProcWaitForProcess next ph ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ProcWaitForProcessNext next) model.next }, sendProcWaitForProcess { index = index, ph = ph } )

                ( newRealWorld, T.ExitWith next code ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ExitWithNext next) model.next }, sendExitWith code )

                ( newRealWorld, T.DirFindExecutable next name ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirFindExecutableNext next) model.next }, sendDirFindExecutable { index = index, name = name } )

                ( newRealWorld, T.ReplGetInputLine next prompt ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReplGetInputLineNext next) model.next }, sendReplGetInputLine { index = index, prompt = prompt } )

                ( newRealWorld, T.DirDoesFileExist next filename ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirDoesFileExistNext next) model.next }, sendDirDoesFileExist { index = index, filename = filename } )

                ( newRealWorld, T.DirCreateDirectoryIfMissing next createParents filename ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirCreateDirectoryIfMissingNext next) model.next }, sendDirCreateDirectoryIfMissing { index = index, createParents = createParents, filename = filename } )

                ( newRealWorld, T.LockFile next path ) ->
                    ( { newRealWorld | next = Dict.insert index (T.LockFileNext next) model.next }, sendLockFile { index = index, path = path } )

                ( newRealWorld, T.UnlockFile next path ) ->
                    ( { newRealWorld | next = Dict.insert index (T.UnlockFileNext next) model.next }, sendUnlockFile { index = index, path = path } )

                ( newRealWorld, T.DirGetModificationTime next filename ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirGetModificationTimeNext next) model.next }, sendDirGetModificationTime { index = index, filename = filename } )

                ( newRealWorld, T.DirDoesDirectoryExist next path ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirDoesDirectoryExistNext next) model.next }, sendDirDoesDirectoryExist { index = index, path = path } )

                ( newRealWorld, T.DirCanonicalizePath next path ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirCanonicalizePathNext next) model.next }, sendDirCanonicalizePath { index = index, path = path } )

                ( newRealWorld, T.BinaryDecodeFileOrFail next filename ) ->
                    ( { newRealWorld | next = Dict.insert index (T.BinaryDecodeFileOrFailNext next) model.next }, sendBinaryDecodeFileOrFail { index = index, filename = filename } )

                ( newRealWorld, T.Write next fd content ) ->
                    ( { newRealWorld | next = Dict.insert index (T.WriteNext next) model.next }, sendWrite { index = index, fd = fd, content = content } )

                ( newRealWorld, T.DirRemoveFile next path ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirRemoveFileNext next) model.next }, sendDirRemoveFile { index = index, path = path } )

                ( newRealWorld, T.DirRemoveDirectoryRecursive next path ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirRemoveDirectoryRecursiveNext next) model.next }, sendDirRemoveDirectoryRecursive { index = index, path = path } )

                ( newRealWorld, T.DirWithCurrentDirectory next path ) ->
                    ( { newRealWorld | next = Dict.insert index (T.DirWithCurrentDirectoryNext next) model.next }, sendDirWithCurrentDirectory { index = index, path = path } )

                ( newRealWorld, T.ReplGetInputLineWithInitial next prompt left right ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReplGetInputLineWithInitialNext next) model.next }, sendReplGetInputLineWithInitial { index = index, prompt = prompt, left = left, right = right } )

                -- MVars
                ( newRealWorld, T.NewEmptyMVar next value ) ->
                    update (NewEmptyMVarMsg index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext next) model.next }

                ( newRealWorld, T.ReadMVar next (Just value) ) ->
                    update (ReadMVarMsg index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext next) model.next }

                ( newRealWorld, T.ReadMVar next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg index) { newRealWorld | next = Dict.insert index (T.PutMVarNext next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar next _ Nothing ) ->
                    update (PutMVarMsg index) { newRealWorld | next = Dict.insert index (T.PutMVarNext next) model.next }

                -- MVars (Maybe T.BED_Status)
                ( newRealWorld, T.NewEmptyMVar_Maybe_BED_Status next value ) ->
                    update (NewEmptyMVarMsg_Maybe_BED_Status index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_Maybe_BED_Status next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_BED_Status next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_BED_Status index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_BED_Status next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_BED_Status next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_BED_Status next) model.next }, Cmd.none )

                ( newRealWorld, T.PutMVar_Maybe_BED_Status next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_BED_Status readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_BED_Status index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_BED_Status next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_Maybe_BED_Status next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_BED_Status index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_BED_Status next) model.next }

                -- MVars (Maybe T.BED_DResult)
                ( newRealWorld, T.NewEmptyMVar_Maybe_BED_DResult next value ) ->
                    update (NewEmptyMVarMsg_Maybe_BED_DResult index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_Maybe_BED_DResult next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_BED_DResult next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_BED_DResult index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_BED_DResult next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_BED_DResult next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_BED_DResult next) model.next }, Cmd.none )

                ( newRealWorld, T.PutMVar_Maybe_BED_DResult next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_BED_DResult readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_BED_DResult index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_BED_DResult next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_Maybe_BED_DResult next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_BED_DResult index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_BED_DResult next) model.next }

                -- MVars (Maybe T.CASTO_LocalGraph)
                ( newRealWorld, T.NewEmptyMVar_Maybe_CASTO_LocalGraph next value ) ->
                    update (NewEmptyMVarMsg_Maybe_CASTO_LocalGraph index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_Maybe_CASTO_LocalGraph next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_CASTO_LocalGraph next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_CASTO_LocalGraph index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_CASTO_LocalGraph next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_CASTO_LocalGraph next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_CASTO_LocalGraph next) model.next }, Cmd.none )

                ( newRealWorld, T.PutMVar_Maybe_CASTO_LocalGraph next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_CASTO_LocalGraph readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_CASTO_LocalGraph index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_CASTO_LocalGraph next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_Maybe_CASTO_LocalGraph next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_CASTO_LocalGraph index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_CASTO_LocalGraph next) model.next }

                -- MVars (Maybe T.CASTO_GlobalGraph)
                ( newRealWorld, T.NewEmptyMVar_Maybe_CASTO_GlobalGraph next value ) ->
                    update (NewEmptyMVarMsg_Maybe_CASTO_GlobalGraph index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_Maybe_CASTO_GlobalGraph next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_CASTO_GlobalGraph next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_CASTO_GlobalGraph index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_CASTO_GlobalGraph next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_CASTO_GlobalGraph next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_CASTO_GlobalGraph next) model.next }, Cmd.none )

                ( newRealWorld, T.PutMVar_Maybe_CASTO_GlobalGraph next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_CASTO_GlobalGraph readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_CASTO_GlobalGraph index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_CASTO_GlobalGraph next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_Maybe_CASTO_GlobalGraph next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_CASTO_GlobalGraph index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_CASTO_GlobalGraph next) model.next }

                -- MVars (T.BB_BResult)
                ( newRealWorld, T.NewEmptyMVar_BB_BResult next value ) ->
                    update (NewEmptyMVarMsg_BB_BResult index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_BB_BResult next) model.next }

                ( newRealWorld, T.ReadMVar_BB_BResult next (Just value) ) ->
                    update (ReadMVarMsg_BB_BResult index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_BResult next) model.next }

                ( newRealWorld, T.ReadMVar_BB_BResult next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_BResult next) model.next }, Cmd.none )

                ( newRealWorld, T.PutMVar_BB_BResult next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BB_BResult readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BB_BResult index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_BResult next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_BB_BResult next _ Nothing ) ->
                    update (PutMVarMsg_BB_BResult index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_BResult next) model.next }

                -- MVars (T.BB_Status)
                ( newRealWorld, T.NewEmptyMVar_BB_Status next value ) ->
                    update (NewEmptyMVarMsg_BB_Status index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_BB_Status next) model.next }

                ( newRealWorld, T.ReadMVar_BB_Status next (Just value) ) ->
                    update (ReadMVarMsg_BB_Status index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_Status next) model.next }

                ( newRealWorld, T.ReadMVar_BB_Status next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_Status next) model.next }, Cmd.none )

                ( newRealWorld, T.PutMVar_BB_Status next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BB_Status readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BB_Status index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_Status next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_BB_Status next _ Nothing ) ->
                    update (PutMVarMsg_BB_Status index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_Status next) model.next }

                -- MVars (T.BB_StatusDict)
                ( newRealWorld, T.NewEmptyMVar_BB_StatusDict next value ) ->
                    update (NewEmptyMVarMsg_BB_StatusDict index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_BB_StatusDict next) model.next }

                ( newRealWorld, T.ReadMVar_BB_StatusDict next (Just value) ) ->
                    update (ReadMVarMsg_BB_StatusDict index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_StatusDict next) model.next }

                ( newRealWorld, T.ReadMVar_BB_StatusDict next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_StatusDict next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_BB_StatusDict next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_BB_StatusDict index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_BB_StatusDict next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_BB_StatusDict next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_BB_StatusDict next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_BB_StatusDict next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BB_StatusDict readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BB_StatusDict index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_StatusDict next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_BB_StatusDict next _ Nothing ) ->
                    update (PutMVarMsg_BB_StatusDict index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_StatusDict next) model.next }

                -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
                ( newRealWorld, T.NewEmptyMVar_ResultRegistryProblemEnv next value ) ->
                    update (NewEmptyMVarMsg_ResultRegistryProblemEnv index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_ResultRegistryProblemEnv next) model.next }

                ( newRealWorld, T.ReadMVar_ResultRegistryProblemEnv next (Just value) ) ->
                    update (ReadMVarMsg_ResultRegistryProblemEnv index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ResultRegistryProblemEnv next) model.next }

                ( newRealWorld, T.ReadMVar_ResultRegistryProblemEnv next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ResultRegistryProblemEnv next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_ResultRegistryProblemEnv next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_ResultRegistryProblemEnv index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ResultRegistryProblemEnv next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_ResultRegistryProblemEnv next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ResultRegistryProblemEnv next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_ResultRegistryProblemEnv next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_ResultRegistryProblemEnv readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_ResultRegistryProblemEnv index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ResultRegistryProblemEnv next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_ResultRegistryProblemEnv next _ Nothing ) ->
                    update (PutMVarMsg_ResultRegistryProblemEnv index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ResultRegistryProblemEnv next) model.next }

                -- MVars (T.CED_Dep)
                ( newRealWorld, T.NewEmptyMVar_CED_Dep next value ) ->
                    update (NewEmptyMVarMsg_CED_Dep index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_CED_Dep next) model.next }

                ( newRealWorld, T.ReadMVar_CED_Dep next (Just value) ) ->
                    update (ReadMVarMsg_CED_Dep index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_CED_Dep next) model.next }

                ( newRealWorld, T.ReadMVar_CED_Dep next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_CED_Dep next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_CED_Dep next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_CED_Dep index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_CED_Dep next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_CED_Dep next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_CED_Dep next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_CED_Dep next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_CED_Dep readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_CED_Dep index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_CED_Dep next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_CED_Dep next _ Nothing ) ->
                    update (PutMVarMsg_CED_Dep index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_CED_Dep next) model.next }

                -- MVars (Maybe T.CECTE_Types)
                ( newRealWorld, T.NewEmptyMVar_Maybe_CECTE_Types next value ) ->
                    update (NewEmptyMVarMsg_Maybe_CECTE_Types index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_Maybe_CECTE_Types next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_CECTE_Types next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_CECTE_Types index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_CECTE_Types next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_CECTE_Types next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_CECTE_Types next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_Maybe_CECTE_Types next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_Maybe_CECTE_Types index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_Maybe_CECTE_Types next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_Maybe_CECTE_Types next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_Maybe_CECTE_Types next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_Maybe_CECTE_Types next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_CECTE_Types readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_CECTE_Types index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_CECTE_Types next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_Maybe_CECTE_Types next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_CECTE_Types index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_CECTE_Types next) model.next }

                -- MVars (Maybe T.BB_Dependencies)
                ( newRealWorld, T.NewEmptyMVar_Maybe_BB_Dependencies next value ) ->
                    update (NewEmptyMVarMsg_Maybe_BB_Dependencies index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_Maybe_BB_Dependencies next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_BB_Dependencies next (Just value) ) ->
                    update (ReadMVarMsg_Maybe_BB_Dependencies index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_BB_Dependencies next) model.next }

                ( newRealWorld, T.ReadMVar_Maybe_BB_Dependencies next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Maybe_BB_Dependencies next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_Maybe_BB_Dependencies next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_Maybe_BB_Dependencies index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_Maybe_BB_Dependencies next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_Maybe_BB_Dependencies next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_Maybe_BB_Dependencies next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_Maybe_BB_Dependencies next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Maybe_BB_Dependencies readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Maybe_BB_Dependencies index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_BB_Dependencies next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_Maybe_BB_Dependencies next _ Nothing ) ->
                    update (PutMVarMsg_Maybe_BB_Dependencies index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Maybe_BB_Dependencies next) model.next }

                -- MVars (Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep)
                ( newRealWorld, T.NewEmptyMVar_DictNameMVarDep next value ) ->
                    update (NewEmptyMVarMsg_DictNameMVarDep index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_DictNameMVarDep next) model.next }

                ( newRealWorld, T.ReadMVar_DictNameMVarDep next (Just value) ) ->
                    update (ReadMVarMsg_DictNameMVarDep index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_DictNameMVarDep next) model.next }

                ( newRealWorld, T.ReadMVar_DictNameMVarDep next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_DictNameMVarDep next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_DictNameMVarDep next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_DictNameMVarDep index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_DictNameMVarDep next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_DictNameMVarDep next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_DictNameMVarDep next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_DictNameMVarDep next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_DictNameMVarDep readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_DictNameMVarDep index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_DictNameMVarDep next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_DictNameMVarDep next _ Nothing ) ->
                    update (PutMVarMsg_DictNameMVarDep index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_DictNameMVarDep next) model.next }

                -- MVars (Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult)
                ( newRealWorld, T.NewEmptyMVar_DictRawMVarMaybeDResult next value ) ->
                    update (NewEmptyMVarMsg_DictRawMVarMaybeDResult index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_DictRawMVarMaybeDResult next) model.next }

                ( newRealWorld, T.ReadMVar_DictRawMVarMaybeDResult next (Just value) ) ->
                    update (ReadMVarMsg_DictRawMVarMaybeDResult index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_DictRawMVarMaybeDResult next) model.next }

                ( newRealWorld, T.ReadMVar_DictRawMVarMaybeDResult next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_DictRawMVarMaybeDResult next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_DictRawMVarMaybeDResult next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_DictRawMVarMaybeDResult index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_DictRawMVarMaybeDResult next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_DictRawMVarMaybeDResult next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_DictRawMVarMaybeDResult next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_DictRawMVarMaybeDResult next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_DictRawMVarMaybeDResult readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_DictRawMVarMaybeDResult index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_DictRawMVarMaybeDResult next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_DictRawMVarMaybeDResult next _ Nothing ) ->
                    update (PutMVarMsg_DictRawMVarMaybeDResult index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_DictRawMVarMaybeDResult next) model.next }

                -- MVars (List T.MVar_Unit)
                ( newRealWorld, T.NewEmptyMVar_ListMVar next value ) ->
                    update (NewEmptyMVarMsg_ListMVar index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_ListMVar next) model.next }

                ( newRealWorld, T.ReadMVar_ListMVar next (Just value) ) ->
                    update (ReadMVarMsg_ListMVar index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ListMVar next) model.next }

                ( newRealWorld, T.ReadMVar_ListMVar next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ListMVar next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_ListMVar next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_ListMVar index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ListMVar next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_ListMVar next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ListMVar next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_ListMVar next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_ListMVar readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_ListMVar index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ListMVar next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_ListMVar next _ Nothing ) ->
                    update (PutMVarMsg_ListMVar index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ListMVar next) model.next }

                -- MVars (T.BB_CachedInterface)
                ( newRealWorld, T.NewEmptyMVar_BB_CachedInterface next value ) ->
                    update (NewEmptyMVarMsg_BB_CachedInterface index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_BB_CachedInterface next) model.next }

                ( newRealWorld, T.ReadMVar_BB_CachedInterface next (Just value) ) ->
                    update (ReadMVarMsg_BB_CachedInterface index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_CachedInterface next) model.next }

                ( newRealWorld, T.ReadMVar_BB_CachedInterface next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_CachedInterface next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_BB_CachedInterface next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_BB_CachedInterface index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_BB_CachedInterface next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_BB_CachedInterface next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_BB_CachedInterface next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_BB_CachedInterface next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BB_CachedInterface readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BB_CachedInterface index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_CachedInterface next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_BB_CachedInterface next _ Nothing ) ->
                    update (PutMVarMsg_BB_CachedInterface index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_CachedInterface next) model.next }

                -- MVars (T.BED_StatusDict)
                ( newRealWorld, T.NewEmptyMVar_BED_StatusDict next value ) ->
                    update (NewEmptyMVarMsg_BED_StatusDict index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_BED_StatusDict next) model.next }

                ( newRealWorld, T.ReadMVar_BED_StatusDict next (Just value) ) ->
                    update (ReadMVarMsg_BED_StatusDict index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BED_StatusDict next) model.next }

                ( newRealWorld, T.ReadMVar_BED_StatusDict next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BED_StatusDict next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_BED_StatusDict next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_BED_StatusDict index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_BED_StatusDict next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_BED_StatusDict next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_BED_StatusDict next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_BED_StatusDict next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BED_StatusDict readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BED_StatusDict index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BED_StatusDict next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_BED_StatusDict next _ Nothing ) ->
                    update (PutMVarMsg_BED_StatusDict index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BED_StatusDict next) model.next }

                -- MVars (Unit)
                ( newRealWorld, T.NewEmptyMVar_Unit next value ) ->
                    update (NewEmptyMVarMsg_Unit index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_Unit next) model.next }

                ( newRealWorld, T.ReadMVar_Unit next (Just value) ) ->
                    update (ReadMVarMsg_Unit index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Unit next) model.next }

                ( newRealWorld, T.ReadMVar_Unit next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Unit next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_Unit next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_Unit index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_Unit next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_Unit next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_Unit next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_Unit next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Unit readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Unit index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Unit next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_Unit next _ Nothing ) ->
                    update (PutMVarMsg_Unit index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Unit next) model.next }

                -- MVars (T.BH_Manager)
                ( newRealWorld, T.NewEmptyMVar_Manager next value ) ->
                    update (NewEmptyMVarMsg_Manager index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_Manager next) model.next }

                ( newRealWorld, T.ReadMVar_Manager next (Just value) ) ->
                    update (ReadMVarMsg_Manager index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Manager next) model.next }

                ( newRealWorld, T.ReadMVar_Manager next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_Manager next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_Manager next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_Manager index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_Manager next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_Manager next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_Manager next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_Manager next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_Manager readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_Manager index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Manager next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_Manager next _ Nothing ) ->
                    update (PutMVarMsg_Manager index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_Manager next) model.next }

                -- MVars (T.BB_ResultDict)
                ( newRealWorld, T.NewEmptyMVar_BB_ResultDict next value ) ->
                    update (NewEmptyMVarMsg_BB_ResultDict index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_BB_ResultDict next) model.next }

                ( newRealWorld, T.ReadMVar_BB_ResultDict next (Just value) ) ->
                    update (ReadMVarMsg_BB_ResultDict index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_ResultDict next) model.next }

                ( newRealWorld, T.ReadMVar_BB_ResultDict next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_BB_ResultDict next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_BB_ResultDict next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_BB_ResultDict index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_BB_ResultDict next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_BB_ResultDict next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_BB_ResultDict next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_BB_ResultDict next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_BB_ResultDict readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_BB_ResultDict index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_ResultDict next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_BB_ResultDict next _ Nothing ) ->
                    update (PutMVarMsg_BB_ResultDict index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_BB_ResultDict next) model.next }

                -- MVars (T.MVar_StreamResultBMsgBResultDocumentation)
                ( newRealWorld, T.NewEmptyMVar_StreamResultBMsgBResultDocumentation next value ) ->
                    update (NewEmptyMVarMsg_StreamResultBMsgBResultDocumentation index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_StreamResultBMsgBResultDocumentation next) model.next }

                ( newRealWorld, T.ReadMVar_StreamResultBMsgBResultDocumentation next (Just value) ) ->
                    update (ReadMVarMsg_StreamResultBMsgBResultDocumentation index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_StreamResultBMsgBResultDocumentation next) model.next }

                ( newRealWorld, T.ReadMVar_StreamResultBMsgBResultDocumentation next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_StreamResultBMsgBResultDocumentation next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_StreamResultBMsgBResultDocumentation next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_StreamResultBMsgBResultDocumentation index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_StreamResultBMsgBResultDocumentation next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_StreamResultBMsgBResultDocumentation next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_StreamResultBMsgBResultDocumentation next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_StreamResultBMsgBResultDocumentation next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_StreamResultBMsgBResultDocumentation readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_StreamResultBMsgBResultDocumentation index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_StreamResultBMsgBResultDocumentation next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_StreamResultBMsgBResultDocumentation next _ Nothing ) ->
                    update (PutMVarMsg_StreamResultBMsgBResultDocumentation index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_StreamResultBMsgBResultDocumentation next) model.next }

                -- MVars (T.MVar_ChItemResultBMsgBResultDocumentation)
                ( newRealWorld, T.NewEmptyMVar_ChItemResultBMsgBResultDocumentation next value ) ->
                    update (NewEmptyMVarMsg_ChItemResultBMsgBResultDocumentation index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_ChItemResultBMsgBResultDocumentation next) model.next }

                ( newRealWorld, T.ReadMVar_ChItemResultBMsgBResultDocumentation next (Just value) ) ->
                    update (ReadMVarMsg_ChItemResultBMsgBResultDocumentation index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ChItemResultBMsgBResultDocumentation next) model.next }

                ( newRealWorld, T.ReadMVar_ChItemResultBMsgBResultDocumentation next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ChItemResultBMsgBResultDocumentation next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_ChItemResultBMsgBResultDocumentation next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_ChItemResultBMsgBResultDocumentation index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ChItemResultBMsgBResultDocumentation next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_ChItemResultBMsgBResultDocumentation next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ChItemResultBMsgBResultDocumentation next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_ChItemResultBMsgBResultDocumentation next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_ChItemResultBMsgBResultDocumentation readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_ChItemResultBMsgBResultDocumentation index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ChItemResultBMsgBResultDocumentation next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_ChItemResultBMsgBResultDocumentation next _ Nothing ) ->
                    update (PutMVarMsg_ChItemResultBMsgBResultDocumentation index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ChItemResultBMsgBResultDocumentation next) model.next }

                -- MVars (T.MVar_StreamResultBMsgBResultUnit)
                ( newRealWorld, T.NewEmptyMVar_StreamResultBMsgBResultUnit next value ) ->
                    update (NewEmptyMVarMsg_StreamResultBMsgBResultUnit index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_StreamResultBMsgBResultUnit next) model.next }

                ( newRealWorld, T.ReadMVar_StreamResultBMsgBResultUnit next (Just value) ) ->
                    update (ReadMVarMsg_StreamResultBMsgBResultUnit index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_StreamResultBMsgBResultUnit next) model.next }

                ( newRealWorld, T.ReadMVar_StreamResultBMsgBResultUnit next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_StreamResultBMsgBResultUnit next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_StreamResultBMsgBResultUnit next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_StreamResultBMsgBResultUnit index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_StreamResultBMsgBResultUnit next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_StreamResultBMsgBResultUnit next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_StreamResultBMsgBResultUnit next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_StreamResultBMsgBResultUnit next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_StreamResultBMsgBResultUnit readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_StreamResultBMsgBResultUnit index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_StreamResultBMsgBResultUnit next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_StreamResultBMsgBResultUnit next _ Nothing ) ->
                    update (PutMVarMsg_StreamResultBMsgBResultUnit index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_StreamResultBMsgBResultUnit next) model.next }

                -- MVars (T.MVar_ChItemResultBMsgBResultUnit)
                ( newRealWorld, T.NewEmptyMVar_ChItemResultBMsgBResultUnit next value ) ->
                    update (NewEmptyMVarMsg_ChItemResultBMsgBResultUnit index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_ChItemResultBMsgBResultUnit next) model.next }

                ( newRealWorld, T.ReadMVar_ChItemResultBMsgBResultUnit next (Just value) ) ->
                    update (ReadMVarMsg_ChItemResultBMsgBResultUnit index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ChItemResultBMsgBResultUnit next) model.next }

                ( newRealWorld, T.ReadMVar_ChItemResultBMsgBResultUnit next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ChItemResultBMsgBResultUnit next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_ChItemResultBMsgBResultUnit next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_ChItemResultBMsgBResultUnit index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ChItemResultBMsgBResultUnit next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_ChItemResultBMsgBResultUnit next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ChItemResultBMsgBResultUnit next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_ChItemResultBMsgBResultUnit next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_ChItemResultBMsgBResultUnit readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_ChItemResultBMsgBResultUnit index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ChItemResultBMsgBResultUnit next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_ChItemResultBMsgBResultUnit next _ Nothing ) ->
                    update (PutMVarMsg_ChItemResultBMsgBResultUnit index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ChItemResultBMsgBResultUnit next) model.next }

                -- MVars (T.MVar_StreamResultBMsgBResultArtifacts)
                ( newRealWorld, T.NewEmptyMVar_StreamResultBMsgBResultArtifacts next value ) ->
                    update (NewEmptyMVarMsg_StreamResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_StreamResultBMsgBResultArtifacts next) model.next }

                ( newRealWorld, T.ReadMVar_StreamResultBMsgBResultArtifacts next (Just value) ) ->
                    update (ReadMVarMsg_StreamResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_StreamResultBMsgBResultArtifacts next) model.next }

                ( newRealWorld, T.ReadMVar_StreamResultBMsgBResultArtifacts next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_StreamResultBMsgBResultArtifacts next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_StreamResultBMsgBResultArtifacts next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_StreamResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_StreamResultBMsgBResultArtifacts next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_StreamResultBMsgBResultArtifacts next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_StreamResultBMsgBResultArtifacts next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_StreamResultBMsgBResultArtifacts next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_StreamResultBMsgBResultArtifacts readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_StreamResultBMsgBResultArtifacts index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_StreamResultBMsgBResultArtifacts next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_StreamResultBMsgBResultArtifacts next _ Nothing ) ->
                    update (PutMVarMsg_StreamResultBMsgBResultArtifacts index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_StreamResultBMsgBResultArtifacts next) model.next }

                -- MVars (T.MVar_ChItemResultBMsgBResultArtifacts)
                ( newRealWorld, T.NewEmptyMVar_ChItemResultBMsgBResultArtifacts next value ) ->
                    update (NewEmptyMVarMsg_ChItemResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_ChItemResultBMsgBResultArtifacts next) model.next }

                ( newRealWorld, T.ReadMVar_ChItemResultBMsgBResultArtifacts next (Just value) ) ->
                    update (ReadMVarMsg_ChItemResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ChItemResultBMsgBResultArtifacts next) model.next }

                ( newRealWorld, T.ReadMVar_ChItemResultBMsgBResultArtifacts next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ChItemResultBMsgBResultArtifacts next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_ChItemResultBMsgBResultArtifacts next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_ChItemResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ChItemResultBMsgBResultArtifacts next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_ChItemResultBMsgBResultArtifacts next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ChItemResultBMsgBResultArtifacts next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_ChItemResultBMsgBResultArtifacts next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_ChItemResultBMsgBResultArtifacts readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_ChItemResultBMsgBResultArtifacts index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ChItemResultBMsgBResultArtifacts next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_ChItemResultBMsgBResultArtifacts next _ Nothing ) ->
                    update (PutMVarMsg_ChItemResultBMsgBResultArtifacts index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ChItemResultBMsgBResultArtifacts next) model.next }

                -- MVars (Result T.BR_BMsg (T.BR_BResult T.BB_Artifacts))
                ( newRealWorld, T.NewEmptyMVar_ResultBMsgBResultArtifacts next value ) ->
                    update (NewEmptyMVarMsg_ResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.NewEmptyMVarNext_ResultBMsgBResultArtifacts next) model.next }

                ( newRealWorld, T.ReadMVar_ResultBMsgBResultArtifacts next (Just value) ) ->
                    update (ReadMVarMsg_ResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ResultBMsgBResultArtifacts next) model.next }

                ( newRealWorld, T.ReadMVar_ResultBMsgBResultArtifacts next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (T.ReadMVarNext_ResultBMsgBResultArtifacts next) model.next }, Cmd.none )

                ( newRealWorld, T.TakeMVar_ResultBMsgBResultArtifacts next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg_ResultBMsgBResultArtifacts index value) { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ResultBMsgBResultArtifacts next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.TakeMVar_ResultBMsgBResultArtifacts next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (T.TakeMVarNext_ResultBMsgBResultArtifacts next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, T.PutMVar_ResultBMsgBResultArtifacts next readIndexes (Just value) ) ->
                    List.foldl
                        (\readIndex ( updatedModel, updateCmd ) ->
                            update (ReadMVarMsg_ResultBMsgBResultArtifacts readIndex value) updatedModel
                                |> Tuple.mapSecond (\cmd -> Cmd.batch [ updateCmd, cmd ])
                        )
                        (update (PutMVarMsg_ResultBMsgBResultArtifacts index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ResultBMsgBResultArtifacts next) model.next })
                        readIndexes

                ( newRealWorld, T.PutMVar_ResultBMsgBResultArtifacts next _ Nothing ) ->
                    update (PutMVarMsg_ResultBMsgBResultArtifacts index) { newRealWorld | next = Dict.insert index (T.PutMVarNext_ResultBMsgBResultArtifacts next) model.next }

        GetLineMsg index input ->
            case Dict.get index model.next of
                Just (T.GetLineNext fn) ->
                    update (PureMsg index (fn input)) model

                _ ->
                    crash "GetLineMsg"

        HPutLineMsg index ->
            case Dict.get index model.next of
                Just (T.HPutLineNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "HPutLineMsg"

        WriteStringMsg index ->
            case Dict.get index model.next of
                Just (T.WriteStringNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "WriteStringMsg"

        ReadMsg index value ->
            case Dict.get index model.next of
                Just (T.ReadNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMsg"

        HttpFetchMsg index value ->
            case Dict.get index model.next of
                Just (T.HttpFetchNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "HttpFetchMsg"

        GetArchiveMsg index value ->
            case Dict.get index model.next of
                Just (T.GetArchiveNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "GetArchiveMsg"

        HttpUploadMsg index ->
            case Dict.get index model.next of
                Just (T.HttpUploadNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "HttpUploadMsg"

        HFlushMsg index ->
            case Dict.get index model.next of
                Just (T.HFlushNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "HFlushMsg"

        WithFileMsg index fd ->
            case Dict.get index model.next of
                Just (T.WithFileNext fn) ->
                    update (PureMsg index (fn fd)) model

                _ ->
                    crash "WithFileMsg"

        HFileSizeMsg index size ->
            case Dict.get index model.next of
                Just (T.HFileSizeNext fn) ->
                    update (PureMsg index (fn size)) model

                _ ->
                    crash "HFileSizeMsg"

        ProcWithCreateProcessMsg index value ->
            case Dict.get index model.next of
                Just (T.ProcWithCreateProcessNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ProcWithCreateProcessMsg"

        HCloseMsg index ->
            case Dict.get index model.next of
                Just (T.HCloseNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "HCloseMsg"

        ProcWaitForProcessMsg index code ->
            case Dict.get index model.next of
                Just (T.ProcWaitForProcessNext fn) ->
                    update (PureMsg index (fn code)) model

                _ ->
                    crash "ProcWaitForProcessMsg"

        DirFindExecutableMsg index value ->
            case Dict.get index model.next of
                Just (T.DirFindExecutableNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirFindExecutableMsg"

        ReplGetInputLineMsg index value ->
            case Dict.get index model.next of
                Just (T.ReplGetInputLineNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReplGetInputLineMsg"

        DirDoesFileExistMsg index value ->
            case Dict.get index model.next of
                Just (T.DirDoesFileExistNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirDoesFileExistMsg"

        DirCreateDirectoryIfMissingMsg index ->
            case Dict.get index model.next of
                Just (T.DirCreateDirectoryIfMissingNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "DirCreateDirectoryIfMissingMsg"

        LockFileMsg index ->
            case Dict.get index model.next of
                Just (T.LockFileNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "LockFileMsg"

        UnlockFileMsg index ->
            case Dict.get index model.next of
                Just (T.UnlockFileNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "UnlockFileMsg"

        DirGetModificationTimeMsg index value ->
            case Dict.get index model.next of
                Just (T.DirGetModificationTimeNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirGetModificationTimeMsg"

        DirDoesDirectoryExistMsg index value ->
            case Dict.get index model.next of
                Just (T.DirDoesDirectoryExistNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirDoesDirectoryExistMsg"

        DirCanonicalizePathMsg index value ->
            case Dict.get index model.next of
                Just (T.DirCanonicalizePathNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "DirCanonicalizePathMsg"

        BinaryDecodeFileOrFailMsg index value ->
            case Dict.get index model.next of
                Just (T.BinaryDecodeFileOrFailNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "BinaryDecodeFileOrFailMsg"

        WriteMsg index ->
            case Dict.get index model.next of
                Just (T.WriteNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "WriteMsg"

        DirRemoveFileMsg index ->
            case Dict.get index model.next of
                Just (T.DirRemoveFileNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "DirRemoveFileMsg"

        DirRemoveDirectoryRecursiveMsg index ->
            case Dict.get index model.next of
                Just (T.DirRemoveDirectoryRecursiveNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "DirRemoveDirectoryRecursiveMsg"

        DirWithCurrentDirectoryMsg index ->
            case Dict.get index model.next of
                Just (T.DirWithCurrentDirectoryNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "DirWithCurrentDirectoryMsg"

        ReplGetInputLineWithInitialMsg index value ->
            case Dict.get index model.next of
                Just (T.ReplGetInputLineWithInitialNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReplGetInputLineWithInitialMsg"

        -- MVars
        NewEmptyMVarMsg index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg"

        ReadMVarMsg index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg"

        PutMVarMsg index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg"

        -- MVars (Maybe T.BED_Status)
        NewEmptyMVarMsg_Maybe_BED_Status index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_Maybe_BED_Status fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_BED_Status"

        ReadMVarMsg_Maybe_BED_Status index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_Maybe_BED_Status fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_BED_Status"

        PutMVarMsg_Maybe_BED_Status index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_Maybe_BED_Status fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_BED_Status"

        -- MVars (Maybe T.BED_DResult)
        NewEmptyMVarMsg_Maybe_BED_DResult index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_Maybe_BED_DResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_BED_DResult"

        ReadMVarMsg_Maybe_BED_DResult index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_Maybe_BED_DResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_BED_DResult"

        PutMVarMsg_Maybe_BED_DResult index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_Maybe_BED_DResult fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_BED_DResult"

        -- MVars (Maybe T.CASTO_LocalGraph)
        NewEmptyMVarMsg_Maybe_CASTO_LocalGraph index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_Maybe_CASTO_LocalGraph fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_CASTO_LocalGraph"

        ReadMVarMsg_Maybe_CASTO_LocalGraph index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_Maybe_CASTO_LocalGraph fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_CASTO_LocalGraph"

        PutMVarMsg_Maybe_CASTO_LocalGraph index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_Maybe_CASTO_LocalGraph fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_CASTO_LocalGraph"

        -- MVars (Maybe T.CASTO_GlobalGraph)
        NewEmptyMVarMsg_Maybe_CASTO_GlobalGraph index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_Maybe_CASTO_GlobalGraph fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_CASTO_GlobalGraph"

        ReadMVarMsg_Maybe_CASTO_GlobalGraph index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_Maybe_CASTO_GlobalGraph fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_CASTO_GlobalGraph"

        PutMVarMsg_Maybe_CASTO_GlobalGraph index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_Maybe_CASTO_GlobalGraph fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_CASTO_GlobalGraph"

        -- MVars (T.BB_BResult)
        NewEmptyMVarMsg_BB_BResult index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_BB_BResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BB_BResult"

        ReadMVarMsg_BB_BResult index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_BB_BResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BB_BResult"

        PutMVarMsg_BB_BResult index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_BB_BResult fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BB_BResult"

        -- MVars (T.BB_Status)
        NewEmptyMVarMsg_BB_Status index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_BB_Status fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BB_Status"

        ReadMVarMsg_BB_Status index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_BB_Status fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BB_Status"

        PutMVarMsg_BB_Status index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_BB_Status fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BB_Status"

        -- MVars (T.BB_StatusDict)
        NewEmptyMVarMsg_BB_StatusDict index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_BB_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BB_StatusDict"

        ReadMVarMsg_BB_StatusDict index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_BB_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_BB_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BB_StatusDict"

        PutMVarMsg_BB_StatusDict index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_BB_StatusDict fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BB_StatusDict"

        -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
        NewEmptyMVarMsg_ResultRegistryProblemEnv index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_ResultRegistryProblemEnv fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_ResultRegistryProblemEnv"

        ReadMVarMsg_ResultRegistryProblemEnv index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_ResultRegistryProblemEnv fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_ResultRegistryProblemEnv fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_ResultRegistryProblemEnv"

        PutMVarMsg_ResultRegistryProblemEnv index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_ResultRegistryProblemEnv fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_ResultRegistryProblemEnv"

        -- MVars (T.CED_Dep)
        NewEmptyMVarMsg_CED_Dep index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_CED_Dep fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_CED_Dep"

        ReadMVarMsg_CED_Dep index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_CED_Dep fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_CED_Dep fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_CED_Dep"

        PutMVarMsg_CED_Dep index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_CED_Dep fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_CED_Dep"

        -- MVars (Maybe T.CECTE_Types)
        NewEmptyMVarMsg_Maybe_CECTE_Types index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_Maybe_CECTE_Types fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_CECTE_Types"

        ReadMVarMsg_Maybe_CECTE_Types index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_Maybe_CECTE_Types fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_Maybe_CECTE_Types fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_CECTE_Types"

        PutMVarMsg_Maybe_CECTE_Types index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_Maybe_CECTE_Types fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_CECTE_Types"

        -- MVars (Maybe T.BB_Dependencies)
        NewEmptyMVarMsg_Maybe_BB_Dependencies index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_Maybe_BB_Dependencies fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Maybe_BB_Dependencies"

        ReadMVarMsg_Maybe_BB_Dependencies index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_Maybe_BB_Dependencies fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_Maybe_BB_Dependencies fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Maybe_BB_Dependencies"

        PutMVarMsg_Maybe_BB_Dependencies index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_Maybe_BB_Dependencies fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Maybe_BB_Dependencies"

        -- MVars (Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep)
        NewEmptyMVarMsg_DictNameMVarDep index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_DictNameMVarDep fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_DictNameMVarDep"

        ReadMVarMsg_DictNameMVarDep index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_DictNameMVarDep fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_DictNameMVarDep fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_DictNameMVarDep"

        PutMVarMsg_DictNameMVarDep index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_DictNameMVarDep fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_DictNameMVarDep"

        -- MVars (Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult)
        NewEmptyMVarMsg_DictRawMVarMaybeDResult index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_DictRawMVarMaybeDResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_DictRawMVarMaybeDResult"

        ReadMVarMsg_DictRawMVarMaybeDResult index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_DictRawMVarMaybeDResult fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_DictRawMVarMaybeDResult fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_DictRawMVarMaybeDResult"

        PutMVarMsg_DictRawMVarMaybeDResult index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_DictRawMVarMaybeDResult fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_DictRawMVarMaybeDResult"

        -- MVars (List T.MVar_Unit)
        NewEmptyMVarMsg_ListMVar index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_ListMVar fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_ListMVar"

        ReadMVarMsg_ListMVar index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_ListMVar fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_ListMVar fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_ListMVar"

        PutMVarMsg_ListMVar index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_ListMVar fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_ListMVar"

        -- MVars (T.BB_CachedInterface)
        NewEmptyMVarMsg_BB_CachedInterface index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_BB_CachedInterface fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BB_CachedInterface"

        ReadMVarMsg_BB_CachedInterface index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_BB_CachedInterface fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_BB_CachedInterface fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BB_CachedInterface"

        PutMVarMsg_BB_CachedInterface index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_BB_CachedInterface fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BB_CachedInterface"

        -- MVars (T.BED_StatusDict)
        NewEmptyMVarMsg_BED_StatusDict index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_BED_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BED_StatusDict"

        ReadMVarMsg_BED_StatusDict index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_BED_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_BED_StatusDict fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BED_StatusDict"

        PutMVarMsg_BED_StatusDict index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_BED_StatusDict fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BED_StatusDict"

        -- MVars (Unit)
        NewEmptyMVarMsg_Unit index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_Unit fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Unit"

        ReadMVarMsg_Unit index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_Unit fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_Unit fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Unit"

        PutMVarMsg_Unit index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_Unit fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Unit"

        -- MVars (T.BH_Manager)
        NewEmptyMVarMsg_Manager index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_Manager fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_Manager"

        ReadMVarMsg_Manager index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_Manager fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_Manager fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_Manager"

        PutMVarMsg_Manager index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_Manager fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_Manager"

        -- MVars (T.BB_ResultDict)
        NewEmptyMVarMsg_BB_ResultDict index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_BB_ResultDict fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_BB_ResultDict"

        ReadMVarMsg_BB_ResultDict index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_BB_ResultDict fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_BB_ResultDict fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_BB_ResultDict"

        PutMVarMsg_BB_ResultDict index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_BB_ResultDict fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_BB_ResultDict"

        -- MVars (MVar_StreamResultBMsgBResultDocumentation)
        NewEmptyMVarMsg_StreamResultBMsgBResultDocumentation index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_StreamResultBMsgBResultDocumentation fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_StreamResultBMsgBResultDocumentation"

        ReadMVarMsg_StreamResultBMsgBResultDocumentation index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_StreamResultBMsgBResultDocumentation fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_StreamResultBMsgBResultDocumentation fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_StreamResultBMsgBResultDocumentation"

        PutMVarMsg_StreamResultBMsgBResultDocumentation index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_StreamResultBMsgBResultDocumentation fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_StreamResultBMsgBResultDocumentation"

        -- MVars (MVar_ChItemResultBMsgBResultDocumentation)
        NewEmptyMVarMsg_ChItemResultBMsgBResultDocumentation index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_ChItemResultBMsgBResultDocumentation fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_ChItemResultBMsgBResultDocumentation"

        ReadMVarMsg_ChItemResultBMsgBResultDocumentation index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_ChItemResultBMsgBResultDocumentation fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_ChItemResultBMsgBResultDocumentation fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_ChItemResultBMsgBResultDocumentation"

        PutMVarMsg_ChItemResultBMsgBResultDocumentation index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_ChItemResultBMsgBResultDocumentation fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_ChItemResultBMsgBResultDocumentation"

        -- MVars (MVar_StreamResultBMsgBResultUnit)
        NewEmptyMVarMsg_StreamResultBMsgBResultUnit index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_StreamResultBMsgBResultUnit fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_StreamResultBMsgBResultUnit"

        ReadMVarMsg_StreamResultBMsgBResultUnit index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_StreamResultBMsgBResultUnit fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_StreamResultBMsgBResultUnit fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_StreamResultBMsgBResultUnit"

        PutMVarMsg_StreamResultBMsgBResultUnit index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_StreamResultBMsgBResultUnit fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_StreamResultBMsgBResultUnit"

        -- MVars (MVar_ChItemResultBMsgBResultUnit)
        NewEmptyMVarMsg_ChItemResultBMsgBResultUnit index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_ChItemResultBMsgBResultUnit fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_ChItemResultBMsgBResultUnit"

        ReadMVarMsg_ChItemResultBMsgBResultUnit index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_ChItemResultBMsgBResultUnit fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_ChItemResultBMsgBResultUnit fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_ChItemResultBMsgBResultUnit"

        PutMVarMsg_ChItemResultBMsgBResultUnit index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_ChItemResultBMsgBResultUnit fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_ChItemResultBMsgBResultUnit"

        -- MVars (MVar_StreamResultBMsgBResultArtifacts)
        NewEmptyMVarMsg_StreamResultBMsgBResultArtifacts index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_StreamResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_StreamResultBMsgBResultArtifacts"

        ReadMVarMsg_StreamResultBMsgBResultArtifacts index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_StreamResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_StreamResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_StreamResultBMsgBResultArtifacts"

        PutMVarMsg_StreamResultBMsgBResultArtifacts index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_StreamResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_StreamResultBMsgBResultArtifacts"

        -- MVars (MVar_ChItemResultBMsgBResultArtifacts)
        NewEmptyMVarMsg_ChItemResultBMsgBResultArtifacts index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_ChItemResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_ChItemResultBMsgBResultArtifacts"

        ReadMVarMsg_ChItemResultBMsgBResultArtifacts index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_ChItemResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_ChItemResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_ChItemResultBMsgBResultArtifacts"

        PutMVarMsg_ChItemResultBMsgBResultArtifacts index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_ChItemResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_ChItemResultBMsgBResultArtifacts"

        -- MVars (Result BR_BMsg (BR_BResult BB_Artifacts))
        NewEmptyMVarMsg_ResultBMsgBResultArtifacts index value ->
            case Dict.get index model.next of
                Just (T.NewEmptyMVarNext_ResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg_ResultBMsgBResultArtifacts"

        ReadMVarMsg_ResultBMsgBResultArtifacts index value ->
            case Dict.get index model.next of
                Just (T.ReadMVarNext_ResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                Just (T.TakeMVarNext_ResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg_ResultBMsgBResultArtifacts"

        PutMVarMsg_ResultBMsgBResultArtifacts index ->
            case Dict.get index model.next of
                Just (T.PutMVarNext_ResultBMsgBResultArtifacts fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg_ResultBMsgBResultArtifacts"


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


port sendWriteString : { index : Int, path : T.FilePath, content : String } -> Cmd msg


port recvWriteString : (Int -> msg) -> Sub msg


port sendRead : { index : Int, fd : String } -> Cmd msg


port recvRead : ({ index : Int, value : String } -> msg) -> Sub msg


port sendHttpFetch : { index : Int, method : String, urlStr : String, headers : List ( String, String ) } -> Cmd msg


port recvHttpFetch : ({ index : Int, value : String } -> msg) -> Sub msg


port sendGetArchive : { index : Int, method : String, url : String } -> Cmd msg


port recvGetArchive : ({ index : Int, value : ( String, T.CAZ_Archive ) } -> msg) -> Sub msg


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


port sendDirFindExecutable : { index : Int, name : T.FilePath } -> Cmd msg


port recvDirFindExecutable : ({ index : Int, value : Maybe T.FilePath } -> msg) -> Sub msg


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


port sendDirDoesDirectoryExist : { index : Int, path : T.FilePath } -> Cmd msg


port recvDirDoesDirectoryExist : ({ index : Int, value : Bool } -> msg) -> Sub msg


port sendDirCanonicalizePath : { index : Int, path : T.FilePath } -> Cmd msg


port recvDirCanonicalizePath : ({ index : Int, value : T.FilePath } -> msg) -> Sub msg


port sendBinaryDecodeFileOrFail : { index : Int, filename : T.FilePath } -> Cmd msg


port recvBinaryDecodeFileOrFail : ({ index : Int, value : Encode.Value } -> msg) -> Sub msg


port sendWrite : { index : Int, fd : T.FilePath, content : Encode.Value } -> Cmd msg


port recvWrite : (Int -> msg) -> Sub msg


port sendDirRemoveFile : { index : Int, path : T.FilePath } -> Cmd msg


port recvDirRemoveFile : (Int -> msg) -> Sub msg


port sendDirRemoveDirectoryRecursive : { index : Int, path : T.FilePath } -> Cmd msg


port recvDirRemoveDirectoryRecursive : (Int -> msg) -> Sub msg


port sendDirWithCurrentDirectory : { index : Int, path : T.FilePath } -> Cmd msg


port recvDirWithCurrentDirectory : (Int -> msg) -> Sub msg


port sendReplGetInputLineWithInitial : { index : Int, prompt : String, left : String, right : String } -> Cmd msg


port recvReplGetInputLineWithInitial : ({ index : Int, value : Maybe String } -> msg) -> Sub msg



-- The IO monad


pure : a -> T.IO a
pure x =
    T.IO (\_ s -> ( s, T.Pure x ))


apply : T.IO a -> T.IO (a -> b) -> T.IO b
apply ma mf =
    bind (\f -> bind (\a -> pure (f a)) ma) mf


fmap : (a -> b) -> T.IO a -> T.IO b
fmap fn ma =
    bind (\a -> pure (fn a)) ma


bind : (a -> T.IO b) -> T.IO a -> T.IO b
bind f (T.IO ma) =
    T.IO
        (\index s0 ->
            case ma index s0 of
                ( s1, T.Pure a ) ->
                    unIO (f a) index s1

                ( s1, T.ForkIO next forkIO ) ->
                    ( s1, T.ForkIO (\() -> bind f (next ())) forkIO )

                ( s1, T.GetLine next ) ->
                    ( s1, T.GetLine (\input -> bind f (next input)) )

                ( s1, T.HPutStr next handle content ) ->
                    ( s1, T.HPutStr (\() -> bind f (next ())) handle content )

                ( s1, T.WriteString next path content ) ->
                    ( s1, T.WriteString (\() -> bind f (next ())) path content )

                ( s1, T.Read next fd ) ->
                    ( s1, T.Read (\input -> bind f (next input)) fd )

                ( s1, T.HttpFetch next method urlStr headers ) ->
                    ( s1, T.HttpFetch (\body -> bind f (next body)) method urlStr headers )

                ( s1, T.GetArchive next method url ) ->
                    ( s1, T.GetArchive (\body -> bind f (next body)) method url )

                ( s1, T.HttpUpload next urlStr headers parts ) ->
                    ( s1, T.HttpUpload (\() -> bind f (next ())) urlStr headers parts )

                ( s1, T.HFlush next handle ) ->
                    ( s1, T.HFlush (\() -> bind f (next ())) handle )

                ( s1, T.WithFile next path mode ) ->
                    ( s1, T.WithFile (\fd -> bind f (next fd)) path mode )

                ( s1, T.HFileSize next handle ) ->
                    ( s1, T.HFileSize (\size -> bind f (next size)) handle )

                ( s1, T.ProcWithCreateProcess next createProcess ) ->
                    ( s1, T.ProcWithCreateProcess (\data -> bind f (next data)) createProcess )

                ( s1, T.HClose next handle ) ->
                    ( s1, T.HClose (\() -> bind f (next ())) handle )

                ( s1, T.ProcWaitForProcess next ph ) ->
                    ( s1, T.ProcWaitForProcess (\code -> bind f (next code)) ph )

                ( s1, T.ExitWith _ code ) ->
                    ( s1, T.ExitWith (\_ -> crash "exitWith") code )

                ( s1, T.DirFindExecutable next name ) ->
                    ( s1, T.DirFindExecutable (\value -> bind f (next value)) name )

                ( s1, T.ReplGetInputLine next prompt ) ->
                    ( s1, T.ReplGetInputLine (\value -> bind f (next value)) prompt )

                ( s1, T.DirDoesFileExist next filename ) ->
                    ( s1, T.DirDoesFileExist (\exists -> bind f (next exists)) filename )

                ( s1, T.DirCreateDirectoryIfMissing next createParents filename ) ->
                    ( s1, T.DirCreateDirectoryIfMissing (\exists -> bind f (next exists)) createParents filename )

                ( s1, T.LockFile next path ) ->
                    ( s1, T.LockFile (\() -> bind f (next ())) path )

                ( s1, T.UnlockFile next path ) ->
                    ( s1, T.UnlockFile (\() -> bind f (next ())) path )

                ( s1, T.DirGetModificationTime next path ) ->
                    ( s1, T.DirGetModificationTime (\value -> bind f (next value)) path )

                ( s1, T.DirDoesDirectoryExist next path ) ->
                    ( s1, T.DirDoesDirectoryExist (\value -> bind f (next value)) path )

                ( s1, T.DirCanonicalizePath next path ) ->
                    ( s1, T.DirCanonicalizePath (\value -> bind f (next value)) path )

                ( s1, T.BinaryDecodeFileOrFail next filename ) ->
                    ( s1, T.BinaryDecodeFileOrFail (\value -> bind f (next value)) filename )

                ( s1, T.Write next fd content ) ->
                    ( s1, T.Write (\() -> bind f (next ())) fd content )

                ( s1, T.DirRemoveFile next path ) ->
                    ( s1, T.DirRemoveFile (\() -> bind f (next ())) path )

                ( s1, T.DirRemoveDirectoryRecursive next path ) ->
                    ( s1, T.DirRemoveDirectoryRecursive (\() -> bind f (next ())) path )

                ( s1, T.DirWithCurrentDirectory next path ) ->
                    ( s1, T.DirWithCurrentDirectory (\() -> bind f (next ())) path )

                ( s1, T.ReplGetInputLineWithInitial next prompt left right ) ->
                    ( s1, T.ReplGetInputLineWithInitial (\value -> bind f (next value)) prompt left right )

                -- MVars
                ( s1, T.NewEmptyMVar next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar next mVarValue ) ->
                    ( s1, T.ReadMVar (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar next readIndexes value ) ->
                    ( s1, T.PutMVar (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.BED_Status)
                ( s1, T.NewEmptyMVar_Maybe_BED_Status next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_Maybe_BED_Status (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_Maybe_BED_Status next mVarValue ) ->
                    ( s1, T.ReadMVar_Maybe_BED_Status (\value -> bind f (next value)) mVarValue )

                ( s1, T.PutMVar_Maybe_BED_Status next readIndexes value ) ->
                    ( s1, T.PutMVar_Maybe_BED_Status (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.BED_DResult)
                ( s1, T.NewEmptyMVar_Maybe_BED_DResult next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_Maybe_BED_DResult (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_Maybe_BED_DResult next mVarValue ) ->
                    ( s1, T.ReadMVar_Maybe_BED_DResult (\value -> bind f (next value)) mVarValue )

                ( s1, T.PutMVar_Maybe_BED_DResult next readIndexes value ) ->
                    ( s1, T.PutMVar_Maybe_BED_DResult (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.CASTO_LocalGraph)
                ( s1, T.NewEmptyMVar_Maybe_CASTO_LocalGraph next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_Maybe_CASTO_LocalGraph (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_Maybe_CASTO_LocalGraph next mVarValue ) ->
                    ( s1, T.ReadMVar_Maybe_CASTO_LocalGraph (\value -> bind f (next value)) mVarValue )

                ( s1, T.PutMVar_Maybe_CASTO_LocalGraph next readIndexes value ) ->
                    ( s1, T.PutMVar_Maybe_CASTO_LocalGraph (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.CASTO_GlobalGraph)
                ( s1, T.NewEmptyMVar_Maybe_CASTO_GlobalGraph next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_Maybe_CASTO_GlobalGraph (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_Maybe_CASTO_GlobalGraph next mVarValue ) ->
                    ( s1, T.ReadMVar_Maybe_CASTO_GlobalGraph (\value -> bind f (next value)) mVarValue )

                ( s1, T.PutMVar_Maybe_CASTO_GlobalGraph next readIndexes value ) ->
                    ( s1, T.PutMVar_Maybe_CASTO_GlobalGraph (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BB_BResult)
                ( s1, T.NewEmptyMVar_BB_BResult next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_BB_BResult (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_BB_BResult next mVarValue ) ->
                    ( s1, T.ReadMVar_BB_BResult (\value -> bind f (next value)) mVarValue )

                ( s1, T.PutMVar_BB_BResult next readIndexes value ) ->
                    ( s1, T.PutMVar_BB_BResult (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BB_Status)
                ( s1, T.NewEmptyMVar_BB_Status next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_BB_Status (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_BB_Status next mVarValue ) ->
                    ( s1, T.ReadMVar_BB_Status (\value -> bind f (next value)) mVarValue )

                ( s1, T.PutMVar_BB_Status next readIndexes value ) ->
                    ( s1, T.PutMVar_BB_Status (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BB_StatusDict)
                ( s1, T.NewEmptyMVar_BB_StatusDict next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_BB_StatusDict (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_BB_StatusDict next mVarValue ) ->
                    ( s1, T.ReadMVar_BB_StatusDict (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_BB_StatusDict next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_BB_StatusDict (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_BB_StatusDict next readIndexes value ) ->
                    ( s1, T.PutMVar_BB_StatusDict (\() -> bind f (next ())) readIndexes value )

                -- MVars (Result T.BRE_RegistryProblem T.BDS_Env)
                ( s1, T.NewEmptyMVar_ResultRegistryProblemEnv next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_ResultRegistryProblemEnv (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_ResultRegistryProblemEnv next mVarValue ) ->
                    ( s1, T.ReadMVar_ResultRegistryProblemEnv (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_ResultRegistryProblemEnv next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_ResultRegistryProblemEnv (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_ResultRegistryProblemEnv next readIndexes value ) ->
                    ( s1, T.PutMVar_ResultRegistryProblemEnv (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.CED_Dep)
                ( s1, T.NewEmptyMVar_CED_Dep next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_CED_Dep (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_CED_Dep next mVarValue ) ->
                    ( s1, T.ReadMVar_CED_Dep (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_CED_Dep next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_CED_Dep (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_CED_Dep next readIndexes value ) ->
                    ( s1, T.PutMVar_CED_Dep (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.CECTE_Types)
                ( s1, T.NewEmptyMVar_Maybe_CECTE_Types next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_Maybe_CECTE_Types (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_Maybe_CECTE_Types next mVarValue ) ->
                    ( s1, T.ReadMVar_Maybe_CECTE_Types (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_Maybe_CECTE_Types next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_Maybe_CECTE_Types (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_Maybe_CECTE_Types next readIndexes value ) ->
                    ( s1, T.PutMVar_Maybe_CECTE_Types (\() -> bind f (next ())) readIndexes value )

                -- MVars (Maybe T.BB_Dependencies)
                ( s1, T.NewEmptyMVar_Maybe_BB_Dependencies next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_Maybe_BB_Dependencies (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_Maybe_BB_Dependencies next mVarValue ) ->
                    ( s1, T.ReadMVar_Maybe_BB_Dependencies (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_Maybe_BB_Dependencies next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_Maybe_BB_Dependencies (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_Maybe_BB_Dependencies next readIndexes value ) ->
                    ( s1, T.PutMVar_Maybe_BB_Dependencies (\() -> bind f (next ())) readIndexes value )

                -- MVars (Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep)
                ( s1, T.NewEmptyMVar_DictNameMVarDep next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_DictNameMVarDep (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_DictNameMVarDep next mVarValue ) ->
                    ( s1, T.ReadMVar_DictNameMVarDep (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_DictNameMVarDep next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_DictNameMVarDep (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_DictNameMVarDep next readIndexes value ) ->
                    ( s1, T.PutMVar_DictNameMVarDep (\() -> bind f (next ())) readIndexes value )

                -- MVars (Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult)
                ( s1, T.NewEmptyMVar_DictRawMVarMaybeDResult next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_DictRawMVarMaybeDResult (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_DictRawMVarMaybeDResult next mVarValue ) ->
                    ( s1, T.ReadMVar_DictRawMVarMaybeDResult (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_DictRawMVarMaybeDResult next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_DictRawMVarMaybeDResult (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_DictRawMVarMaybeDResult next readIndexes value ) ->
                    ( s1, T.PutMVar_DictRawMVarMaybeDResult (\() -> bind f (next ())) readIndexes value )

                -- MVars (List T.MVar_Unit)
                ( s1, T.NewEmptyMVar_ListMVar next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_ListMVar (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_ListMVar next mVarValue ) ->
                    ( s1, T.ReadMVar_ListMVar (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_ListMVar next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_ListMVar (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_ListMVar next readIndexes value ) ->
                    ( s1, T.PutMVar_ListMVar (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BB_CachedInterface)
                ( s1, T.NewEmptyMVar_BB_CachedInterface next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_BB_CachedInterface (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_BB_CachedInterface next mVarValue ) ->
                    ( s1, T.ReadMVar_BB_CachedInterface (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_BB_CachedInterface next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_BB_CachedInterface (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_BB_CachedInterface next readIndexes value ) ->
                    ( s1, T.PutMVar_BB_CachedInterface (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BED_StatusDict)
                ( s1, T.NewEmptyMVar_BED_StatusDict next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_BED_StatusDict (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_BED_StatusDict next mVarValue ) ->
                    ( s1, T.ReadMVar_BED_StatusDict (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_BED_StatusDict next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_BED_StatusDict (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_BED_StatusDict next readIndexes value ) ->
                    ( s1, T.PutMVar_BED_StatusDict (\() -> bind f (next ())) readIndexes value )

                -- MVars (Unit)
                ( s1, T.NewEmptyMVar_Unit next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_Unit (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_Unit next mVarValue ) ->
                    ( s1, T.ReadMVar_Unit (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_Unit next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_Unit (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_Unit next readIndexes value ) ->
                    ( s1, T.PutMVar_Unit (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BH_Manager)
                ( s1, T.NewEmptyMVar_Manager next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_Manager (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_Manager next mVarValue ) ->
                    ( s1, T.ReadMVar_Manager (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_Manager next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_Manager (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_Manager next readIndexes value ) ->
                    ( s1, T.PutMVar_Manager (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.BB_ResultDict)
                ( s1, T.NewEmptyMVar_BB_ResultDict next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_BB_ResultDict (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_BB_ResultDict next mVarValue ) ->
                    ( s1, T.ReadMVar_BB_ResultDict (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_BB_ResultDict next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_BB_ResultDict (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_BB_ResultDict next readIndexes value ) ->
                    ( s1, T.PutMVar_BB_ResultDict (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.MVar_StreamResultBMsgBResultDocumentation)
                ( s1, T.NewEmptyMVar_StreamResultBMsgBResultDocumentation next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_StreamResultBMsgBResultDocumentation (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_StreamResultBMsgBResultDocumentation next mVarValue ) ->
                    ( s1, T.ReadMVar_StreamResultBMsgBResultDocumentation (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_StreamResultBMsgBResultDocumentation next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_StreamResultBMsgBResultDocumentation (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_StreamResultBMsgBResultDocumentation next readIndexes value ) ->
                    ( s1, T.PutMVar_StreamResultBMsgBResultDocumentation (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.MVar_ChItemResultBMsgBResultDocumentation)
                ( s1, T.NewEmptyMVar_ChItemResultBMsgBResultDocumentation next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_ChItemResultBMsgBResultDocumentation (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_ChItemResultBMsgBResultDocumentation next mVarValue ) ->
                    ( s1, T.ReadMVar_ChItemResultBMsgBResultDocumentation (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_ChItemResultBMsgBResultDocumentation next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_ChItemResultBMsgBResultDocumentation (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_ChItemResultBMsgBResultDocumentation next readIndexes value ) ->
                    ( s1, T.PutMVar_ChItemResultBMsgBResultDocumentation (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.MVar_StreamResultBMsgBResultUnit)
                ( s1, T.NewEmptyMVar_StreamResultBMsgBResultUnit next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_StreamResultBMsgBResultUnit (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_StreamResultBMsgBResultUnit next mVarValue ) ->
                    ( s1, T.ReadMVar_StreamResultBMsgBResultUnit (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_StreamResultBMsgBResultUnit next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_StreamResultBMsgBResultUnit (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_StreamResultBMsgBResultUnit next readIndexes value ) ->
                    ( s1, T.PutMVar_StreamResultBMsgBResultUnit (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.MVar_ChItemResultBMsgBResultUnit)
                ( s1, T.NewEmptyMVar_ChItemResultBMsgBResultUnit next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_ChItemResultBMsgBResultUnit (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_ChItemResultBMsgBResultUnit next mVarValue ) ->
                    ( s1, T.ReadMVar_ChItemResultBMsgBResultUnit (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_ChItemResultBMsgBResultUnit next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_ChItemResultBMsgBResultUnit (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_ChItemResultBMsgBResultUnit next readIndexes value ) ->
                    ( s1, T.PutMVar_ChItemResultBMsgBResultUnit (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.MVar_StreamResultBMsgBResultArtifacts)
                ( s1, T.NewEmptyMVar_StreamResultBMsgBResultArtifacts next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_StreamResultBMsgBResultArtifacts (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_StreamResultBMsgBResultArtifacts next mVarValue ) ->
                    ( s1, T.ReadMVar_StreamResultBMsgBResultArtifacts (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_StreamResultBMsgBResultArtifacts next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_StreamResultBMsgBResultArtifacts (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_StreamResultBMsgBResultArtifacts next readIndexes value ) ->
                    ( s1, T.PutMVar_StreamResultBMsgBResultArtifacts (\() -> bind f (next ())) readIndexes value )

                -- MVars (T.MVar_ChItemResultBMsgBResultArtifacts)
                ( s1, T.NewEmptyMVar_ChItemResultBMsgBResultArtifacts next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_ChItemResultBMsgBResultArtifacts (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_ChItemResultBMsgBResultArtifacts next mVarValue ) ->
                    ( s1, T.ReadMVar_ChItemResultBMsgBResultArtifacts (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_ChItemResultBMsgBResultArtifacts next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_ChItemResultBMsgBResultArtifacts (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_ChItemResultBMsgBResultArtifacts next readIndexes value ) ->
                    ( s1, T.PutMVar_ChItemResultBMsgBResultArtifacts (\() -> bind f (next ())) readIndexes value )

                -- MVars (Result T.BR_BMsg (T.BR_BResult T.BB_Artifacts))
                ( s1, T.NewEmptyMVar_ResultBMsgBResultArtifacts next emptyMVarIndex ) ->
                    ( s1, T.NewEmptyMVar_ResultBMsgBResultArtifacts (\value -> bind f (next value)) emptyMVarIndex )

                ( s1, T.ReadMVar_ResultBMsgBResultArtifacts next mVarValue ) ->
                    ( s1, T.ReadMVar_ResultBMsgBResultArtifacts (\value -> bind f (next value)) mVarValue )

                ( s1, T.TakeMVar_ResultBMsgBResultArtifacts next mVarValue maybePutIndex ) ->
                    ( s1, T.TakeMVar_ResultBMsgBResultArtifacts (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, T.PutMVar_ResultBMsgBResultArtifacts next readIndexes value ) ->
                    ( s1, T.PutMVar_ResultBMsgBResultArtifacts (\() -> bind f (next ())) readIndexes value )
        )


unIO : T.IO a -> (Int -> T.RealWorld -> ( T.RealWorld, T.ION a ))
unIO (T.IO a) =
    a



-- Standard handles


stdout : T.Handle
stdout =
    T.Handle 1


stderr : T.Handle
stderr =
    T.Handle 2



-- Opening files


withFile : String -> T.IOMode -> (T.Handle -> T.IO a) -> T.IO a
withFile path mode callback =
    T.IO (\_ s -> ( s, T.WithFile pure path mode ))
        |> bind (T.Handle >> callback)



-- Closing files


hClose : T.Handle -> T.IO ()
hClose handle =
    T.IO (\_ s -> ( s, T.HClose pure handle ))



-- File locking


hFileSize : T.Handle -> T.IO Int
hFileSize handle =
    T.IO (\_ s -> ( s, T.HFileSize pure handle ))



-- Buffering operations


hFlush : T.Handle -> T.IO ()
hFlush handle =
    T.IO (\_ s -> ( s, T.HFlush pure handle ))



-- Terminal operations (not portable: GHC only)


hIsTerminalDevice : T.Handle -> T.IO Bool
hIsTerminalDevice _ =
    pure True



-- Text output


hPutStr : T.Handle -> String -> T.IO ()
hPutStr handle content =
    T.IO (\_ s -> ( s, T.HPutStr pure handle content ))


hPutStrLn : T.Handle -> String -> T.IO ()
hPutStrLn handle content =
    hPutStr handle (content ++ "\n")



-- Special cases for standard input and output


putStr : String -> T.IO ()
putStr =
    hPutStr stdout


putStrLn : String -> T.IO ()
putStrLn s =
    putStr (s ++ "\n")


getLine : T.IO String
getLine =
    T.IO (\_ s -> ( s, T.GetLine pure ))



-- Repl State (Terminal.Repl)


initialReplState : T.ReplState
initialReplState =
    T.ReplState Dict.empty Dict.empty Dict.empty
