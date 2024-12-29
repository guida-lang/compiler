module Types exposing (..)

{-| -}

import Array exposing (Array)
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore exposing (OneOrMore)
import Data.Map as Map
import Data.Set exposing (EverySet)
import Dict exposing (Dict)
import Json.Encode as Encode
import Time



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
    | GetArchive (( String, CAZ_Archive ) -> IO a) String String
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
      -- MVars (Maybe BED_Status)
    | NewEmptyMVar_Maybe_BED_Status (Int -> IO a) Int
    | ReadMVar_Maybe_BED_Status (Maybe BED_Status -> IO a) (Maybe (Maybe BED_Status))
    | PutMVar_Maybe_BED_Status (() -> IO a) (List Int) (Maybe (Maybe BED_Status))
      -- MVars (Maybe BED_DResult)
    | NewEmptyMVar_Maybe_BED_DResult (Int -> IO a) Int
    | ReadMVar_Maybe_BED_DResult (Maybe BED_DResult -> IO a) (Maybe (Maybe BED_DResult))
    | PutMVar_Maybe_BED_DResult (() -> IO a) (List Int) (Maybe (Maybe BED_DResult))
      -- MVars (Maybe CASTO_LocalGraph)
    | NewEmptyMVar_Maybe_CASTO_LocalGraph (Int -> IO a) Int
    | ReadMVar_Maybe_CASTO_LocalGraph (Maybe CASTO_LocalGraph -> IO a) (Maybe (Maybe CASTO_LocalGraph))
    | PutMVar_Maybe_CASTO_LocalGraph (() -> IO a) (List Int) (Maybe (Maybe CASTO_LocalGraph))
      -- MVars (Maybe CASTO_GlobalGraph)
    | NewEmptyMVar_Maybe_CASTO_GlobalGraph (Int -> IO a) Int
    | ReadMVar_Maybe_CASTO_GlobalGraph (Maybe CASTO_GlobalGraph -> IO a) (Maybe (Maybe CASTO_GlobalGraph))
    | PutMVar_Maybe_CASTO_GlobalGraph (() -> IO a) (List Int) (Maybe (Maybe CASTO_GlobalGraph))
      -- MVars (Result BRE_BuildProjectProblem BB_RootInfo)
    | NewEmptyMVar_Result_BuildProjectProblem_RootInfo (Int -> IO a) Int
    | ReadMVar_Result_BuildProjectProblem_RootInfo (Result BRE_BuildProjectProblem BB_RootInfo -> IO a) (Maybe (Result BRE_BuildProjectProblem BB_RootInfo))
    | PutMVar_Result_BuildProjectProblem_RootInfo (() -> IO a) (List Int) (Maybe (Result BRE_BuildProjectProblem BB_RootInfo))
      -- MVars (Maybe BB_Dep)
    | NewEmptyMVar_MaybeDep (Int -> IO a) Int
    | ReadMVar_MaybeDep (Maybe BB_Dep -> IO a) (Maybe (Maybe BB_Dep))
    | PutMVar_MaybeDep (() -> IO a) (List Int) (Maybe (Maybe BB_Dep))
      -- MVars (BB_RootResult)
    | NewEmptyMVar_BB_RootResult (Int -> IO a) Int
    | ReadMVar_BB_RootResult (BB_RootResult -> IO a) (Maybe BB_RootResult)
    | PutMVar_BB_RootResult (() -> IO a) (List Int) (Maybe BB_RootResult)
      -- MVars (BB_RootStatus)
    | NewEmptyMVar_BB_RootStatus (Int -> IO a) Int
    | ReadMVar_BB_RootStatus (BB_RootStatus -> IO a) (Maybe BB_RootStatus)
    | PutMVar_BB_RootStatus (() -> IO a) (List Int) (Maybe BB_RootStatus)
      -- MVars (BB_BResult)
    | NewEmptyMVar_BB_BResult (Int -> IO a) Int
    | ReadMVar_BB_BResult (BB_BResult -> IO a) (Maybe BB_BResult)
    | PutMVar_BB_BResult (() -> IO a) (List Int) (Maybe BB_BResult)
      -- MVars (BB_Status)
    | NewEmptyMVar_BB_Status (Int -> IO a) Int
    | ReadMVar_BB_Status (BB_Status -> IO a) (Maybe BB_Status)
    | PutMVar_BB_Status (() -> IO a) (List Int) (Maybe BB_Status)
      -- MVars (BB_StatusDict)
    | NewEmptyMVar_BB_StatusDict (Int -> IO a) Int
    | ReadMVar_BB_StatusDict (BB_StatusDict -> IO a) (Maybe BB_StatusDict)
    | TakeMVar_BB_StatusDict (BB_StatusDict -> IO a) (Maybe BB_StatusDict) (Maybe Int)
    | PutMVar_BB_StatusDict (() -> IO a) (List Int) (Maybe BB_StatusDict)
      -- MVars (Result BRE_RegistryProblem BDS_Env)
    | NewEmptyMVar_ResultRegistryProblemEnv (Int -> IO a) Int
    | ReadMVar_ResultRegistryProblemEnv (Result BRE_RegistryProblem BDS_Env -> IO a) (Maybe (Result BRE_RegistryProblem BDS_Env))
    | TakeMVar_ResultRegistryProblemEnv (Result BRE_RegistryProblem BDS_Env -> IO a) (Maybe (Result BRE_RegistryProblem BDS_Env)) (Maybe Int)
    | PutMVar_ResultRegistryProblemEnv (() -> IO a) (List Int) (Maybe (Result BRE_RegistryProblem BDS_Env))
      -- MVars (CED_Dep)
    | NewEmptyMVar_CED_Dep (Int -> IO a) Int
    | ReadMVar_CED_Dep (CED_Dep -> IO a) (Maybe CED_Dep)
    | TakeMVar_CED_Dep (CED_Dep -> IO a) (Maybe CED_Dep) (Maybe Int)
    | PutMVar_CED_Dep (() -> IO a) (List Int) (Maybe CED_Dep)
      -- MVars (Maybe CECTE_Types)
    | NewEmptyMVar_Maybe_CECTE_Types (Int -> IO a) Int
    | ReadMVar_Maybe_CECTE_Types (Maybe CECTE_Types -> IO a) (Maybe (Maybe CECTE_Types))
    | TakeMVar_Maybe_CECTE_Types (Maybe CECTE_Types -> IO a) (Maybe (Maybe CECTE_Types)) (Maybe Int)
    | PutMVar_Maybe_CECTE_Types (() -> IO a) (List Int) (Maybe (Maybe CECTE_Types))
      -- MVars (Maybe BB_Dependencies)
    | NewEmptyMVar_Maybe_BB_Dependencies (Int -> IO a) Int
    | ReadMVar_Maybe_BB_Dependencies (Maybe BB_Dependencies -> IO a) (Maybe (Maybe BB_Dependencies))
    | TakeMVar_Maybe_BB_Dependencies (Maybe BB_Dependencies -> IO a) (Maybe (Maybe BB_Dependencies)) (Maybe Int)
    | PutMVar_Maybe_BB_Dependencies (() -> IO a) (List Int) (Maybe (Maybe BB_Dependencies))
      -- MVars (Dict ( String, String ) CEP_Name MVar_CED_Dep)
    | NewEmptyMVar_DictNameMVarDep (Int -> IO a) Int
    | ReadMVar_DictNameMVarDep (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep -> IO a) (Maybe (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep))
    | TakeMVar_DictNameMVarDep (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep -> IO a) (Maybe (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep)) (Maybe Int)
    | PutMVar_DictNameMVarDep (() -> IO a) (List Int) (Maybe (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep))
      -- MVars (Dict String CEMN_Raw MVar_Maybe_BED_DResult)
    | NewEmptyMVar_DictRawMVarMaybeDResult (Int -> IO a) Int
    | ReadMVar_DictRawMVarMaybeDResult (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult -> IO a) (Maybe (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult))
    | TakeMVar_DictRawMVarMaybeDResult (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult -> IO a) (Maybe (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult)) (Maybe Int)
    | PutMVar_DictRawMVarMaybeDResult (() -> IO a) (List Int) (Maybe (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult))
      -- MVars (List (MVar_Unit))
    | NewEmptyMVar_ListMVar (Int -> IO a) Int
    | ReadMVar_ListMVar (List MVar_Unit -> IO a) (Maybe (List MVar_Unit))
    | TakeMVar_ListMVar (List MVar_Unit -> IO a) (Maybe (List MVar_Unit)) (Maybe Int)
    | PutMVar_ListMVar (() -> IO a) (List Int) (Maybe (List MVar_Unit))
      -- MVars (BB_CachedInterface)
    | NewEmptyMVar_BB_CachedInterface (Int -> IO a) Int
    | ReadMVar_BB_CachedInterface (BB_CachedInterface -> IO a) (Maybe BB_CachedInterface)
    | TakeMVar_BB_CachedInterface (BB_CachedInterface -> IO a) (Maybe BB_CachedInterface) (Maybe Int)
    | PutMVar_BB_CachedInterface (() -> IO a) (List Int) (Maybe BB_CachedInterface)
      -- MVars (BED_StatusDict)
    | NewEmptyMVar_BED_StatusDict (Int -> IO a) Int
    | ReadMVar_BED_StatusDict (BED_StatusDict -> IO a) (Maybe BED_StatusDict)
    | TakeMVar_BED_StatusDict (BED_StatusDict -> IO a) (Maybe BED_StatusDict) (Maybe Int)
    | PutMVar_BED_StatusDict (() -> IO a) (List Int) (Maybe BED_StatusDict)
      -- MVars (Unit)
    | NewEmptyMVar_Unit (Int -> IO a) Int
    | ReadMVar_Unit (() -> IO a) (Maybe ())
    | TakeMVar_Unit (() -> IO a) (Maybe ()) (Maybe Int)
    | PutMVar_Unit (() -> IO a) (List Int) (Maybe ())
      -- MVars (BH_Manager)
    | NewEmptyMVar_Manager (Int -> IO a) Int
    | ReadMVar_Manager (BH_Manager -> IO a) (Maybe BH_Manager)
    | TakeMVar_Manager (BH_Manager -> IO a) (Maybe BH_Manager) (Maybe Int)
    | PutMVar_Manager (() -> IO a) (List Int) (Maybe BH_Manager)
      -- MVars (BB_ResultDict)
    | NewEmptyMVar_BB_ResultDict (Int -> IO a) Int
    | ReadMVar_BB_ResultDict (BB_ResultDict -> IO a) (Maybe BB_ResultDict)
    | TakeMVar_BB_ResultDict (BB_ResultDict -> IO a) (Maybe BB_ResultDict) (Maybe Int)
    | PutMVar_BB_ResultDict (() -> IO a) (List Int) (Maybe BB_ResultDict)
      -- MVars (MVar_ChItem_Maybe_DMsg)
    | NewEmptyMVar_Stream_Maybe_DMsg (Int -> IO a) Int
    | ReadMVar_Stream_Maybe_DMsg (MVar_ChItem_Maybe_DMsg -> IO a) (Maybe MVar_ChItem_Maybe_DMsg)
    | TakeMVar_Stream_Maybe_DMsg (MVar_ChItem_Maybe_DMsg -> IO a) (Maybe MVar_ChItem_Maybe_DMsg) (Maybe Int)
    | PutMVar_Stream_Maybe_DMsg (() -> IO a) (List Int) (Maybe MVar_ChItem_Maybe_DMsg)
      -- MVars (ChItem_Maybe_DMsg)
    | NewEmptyMVar_ChItem_Maybe_DMsg (Int -> IO a) Int
    | ReadMVar_ChItem_Maybe_DMsg (ChItem_Maybe_DMsg -> IO a) (Maybe ChItem_Maybe_DMsg)
    | TakeMVar_ChItem_Maybe_DMsg (ChItem_Maybe_DMsg -> IO a) (Maybe ChItem_Maybe_DMsg) (Maybe Int)
    | PutMVar_ChItem_Maybe_DMsg (() -> IO a) (List Int) (Maybe ChItem_Maybe_DMsg)
      -- MVars (MVar_ChItemResultBMsgBResultDocumentation)
    | NewEmptyMVar_StreamResultBMsgBResultDocumentation (Int -> IO a) Int
    | ReadMVar_StreamResultBMsgBResultDocumentation (MVar_ChItemResultBMsgBResultDocumentation -> IO a) (Maybe MVar_ChItemResultBMsgBResultDocumentation)
    | TakeMVar_StreamResultBMsgBResultDocumentation (MVar_ChItemResultBMsgBResultDocumentation -> IO a) (Maybe MVar_ChItemResultBMsgBResultDocumentation) (Maybe Int)
    | PutMVar_StreamResultBMsgBResultDocumentation (() -> IO a) (List Int) (Maybe MVar_ChItemResultBMsgBResultDocumentation)
      -- MVars (ChItem_ResultBMsgBResultDocumentation)
    | NewEmptyMVar_ChItemResultBMsgBResultDocumentation (Int -> IO a) Int
    | ReadMVar_ChItemResultBMsgBResultDocumentation (ChItem_ResultBMsgBResultDocumentation -> IO a) (Maybe ChItem_ResultBMsgBResultDocumentation)
    | TakeMVar_ChItemResultBMsgBResultDocumentation (ChItem_ResultBMsgBResultDocumentation -> IO a) (Maybe ChItem_ResultBMsgBResultDocumentation) (Maybe Int)
    | PutMVar_ChItemResultBMsgBResultDocumentation (() -> IO a) (List Int) (Maybe ChItem_ResultBMsgBResultDocumentation)
      -- MVars (MVar_ChItemResultBMsgBResultUnit)
    | NewEmptyMVar_StreamResultBMsgBResultUnit (Int -> IO a) Int
    | ReadMVar_StreamResultBMsgBResultUnit (MVar_ChItemResultBMsgBResultUnit -> IO a) (Maybe MVar_ChItemResultBMsgBResultUnit)
    | TakeMVar_StreamResultBMsgBResultUnit (MVar_ChItemResultBMsgBResultUnit -> IO a) (Maybe MVar_ChItemResultBMsgBResultUnit) (Maybe Int)
    | PutMVar_StreamResultBMsgBResultUnit (() -> IO a) (List Int) (Maybe MVar_ChItemResultBMsgBResultUnit)
      -- MVars (ChItem_ResultBMsgBResultUnit)
    | NewEmptyMVar_ChItemResultBMsgBResultUnit (Int -> IO a) Int
    | ReadMVar_ChItemResultBMsgBResultUnit (ChItem_ResultBMsgBResultUnit -> IO a) (Maybe ChItem_ResultBMsgBResultUnit)
    | TakeMVar_ChItemResultBMsgBResultUnit (ChItem_ResultBMsgBResultUnit -> IO a) (Maybe ChItem_ResultBMsgBResultUnit) (Maybe Int)
    | PutMVar_ChItemResultBMsgBResultUnit (() -> IO a) (List Int) (Maybe ChItem_ResultBMsgBResultUnit)
      -- MVars (MVar_ChItemResultBMsgBResultArtifacts)
    | NewEmptyMVar_StreamResultBMsgBResultArtifacts (Int -> IO a) Int
    | ReadMVar_StreamResultBMsgBResultArtifacts (MVar_ChItemResultBMsgBResultArtifacts -> IO a) (Maybe MVar_ChItemResultBMsgBResultArtifacts)
    | TakeMVar_StreamResultBMsgBResultArtifacts (MVar_ChItemResultBMsgBResultArtifacts -> IO a) (Maybe MVar_ChItemResultBMsgBResultArtifacts) (Maybe Int)
    | PutMVar_StreamResultBMsgBResultArtifacts (() -> IO a) (List Int) (Maybe MVar_ChItemResultBMsgBResultArtifacts)
      -- MVars (ChItem_ResultBMsgBResultArtifacts)
    | NewEmptyMVar_ChItemResultBMsgBResultArtifacts (Int -> IO a) Int
    | ReadMVar_ChItemResultBMsgBResultArtifacts (ChItem_ResultBMsgBResultArtifacts -> IO a) (Maybe ChItem_ResultBMsgBResultArtifacts)
    | TakeMVar_ChItemResultBMsgBResultArtifacts (ChItem_ResultBMsgBResultArtifacts -> IO a) (Maybe ChItem_ResultBMsgBResultArtifacts) (Maybe Int)
    | PutMVar_ChItemResultBMsgBResultArtifacts (() -> IO a) (List Int) (Maybe ChItem_ResultBMsgBResultArtifacts)
      -- MVars (Result BR_BMsg (BR_BResult BB_Artifacts))
    | NewEmptyMVar_ResultBMsgBResultArtifacts (Int -> IO a) Int
    | ReadMVar_ResultBMsgBResultArtifacts (Result BR_BMsg (BR_BResult BB_Artifacts) -> IO a) (Maybe (Result BR_BMsg (BR_BResult BB_Artifacts)))
    | TakeMVar_ResultBMsgBResultArtifacts (Result BR_BMsg (BR_BResult BB_Artifacts) -> IO a) (Maybe (Result BR_BMsg (BR_BResult BB_Artifacts))) (Maybe Int)
    | PutMVar_ResultBMsgBResultArtifacts (() -> IO a) (List Int) (Maybe (Result BR_BMsg (BR_BResult BB_Artifacts)))


type alias RealWorld =
    { args : List String
    , currentDirectory : String
    , envVars : Dict String String
    , homedir : FilePath
    , progName : String
    , state : ReplState
    , mVars : Array { subscribers : List MVarSubscriber, value : Maybe Encode.Value }
    , mVars_Maybe_BED_Status : Array { subscribers : List MVarSubscriber_Maybe_BED_Status, value : Maybe (Maybe BED_Status) }
    , mVars_Maybe_BED_DResult : Array { subscribers : List MVarSubscriber_Maybe_BED_DResult, value : Maybe (Maybe BED_DResult) }
    , mVars_Maybe_CASTO_LocalGraph : Array { subscribers : List MVarSubscriber_Maybe_CASTO_LocalGraph, value : Maybe (Maybe CASTO_LocalGraph) }
    , mVars_Maybe_CASTO_GlobalGraph : Array { subscribers : List MVarSubscriber_Maybe_CASTO_GlobalGraph, value : Maybe (Maybe CASTO_GlobalGraph) }
    , mVars_Result_BuildProjectProblem_RootInfo : Array { subscribers : List MVarSubscriber_Result_BuildProjectProblem_RootInfo, value : Maybe (Result BRE_BuildProjectProblem BB_RootInfo) }
    , mVars_MaybeDep : Array { subscribers : List MVarSubscriber_MaybeDep, value : Maybe (Maybe BB_Dep) }
    , mVars_BB_RootResult : Array { subscribers : List MVarSubscriber_BB_RootResult, value : Maybe BB_RootResult }
    , mVars_BB_RootStatus : Array { subscribers : List MVarSubscriber_BB_RootStatus, value : Maybe BB_RootStatus }
    , mVars_BB_BResult : Array { subscribers : List MVarSubscriber_BB_BResult, value : Maybe BB_BResult }
    , mVars_BB_Status : Array { subscribers : List MVarSubscriber_BB_Status, value : Maybe BB_Status }
    , mVars_BB_StatusDict : Array { subscribers : List MVarSubscriber_BB_StatusDict, value : Maybe BB_StatusDict }
    , mVars_ResultRegistryProblemEnv : Array { subscribers : List MVarSubscriber_ResultRegistryProblemEnv, value : Maybe (Result BRE_RegistryProblem BDS_Env) }
    , mVars_CED_Dep : Array { subscribers : List MVarSubscriber_CED_Dep, value : Maybe CED_Dep }
    , mVars_Maybe_CECTE_Types : Array { subscribers : List MVarSubscriber_Maybe_CECTE_Types, value : Maybe (Maybe CECTE_Types) }
    , mVars_Maybe_BB_Dependencies : Array { subscribers : List MVarSubscriber_Maybe_BB_Dependencies, value : Maybe (Maybe BB_Dependencies) }
    , mVars_DictNameMVarDep : Array { subscribers : List MVarSubscriber_DictNameMVarDep, value : Maybe (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep) }
    , mVars_DictRawMVarMaybeDResult : Array { subscribers : List MVarSubscriber_DictRawMVarMaybeDResult, value : Maybe (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult) }
    , mVars_ListMVar : Array { subscribers : List MVarSubscriber_ListMVar, value : Maybe (List MVar_Unit) }
    , mVars_BB_CachedInterface : Array { subscribers : List MVarSubscriber_BB_CachedInterface, value : Maybe BB_CachedInterface }
    , mVars_BED_StatusDict : Array { subscribers : List MVarSubscriber_BED_StatusDict, value : Maybe BED_StatusDict }
    , mVars_Unit : Array { subscribers : List MVarSubscriber_Unit, value : Maybe () }
    , mVars_Manager : Array { subscribers : List MVarSubscriber_Manager, value : Maybe BH_Manager }
    , mVars_BB_ResultDict : Array { subscribers : List MVarSubscriber_BB_ResultDict, value : Maybe BB_ResultDict }
    , mVars_Stream_Maybe_DMsg : Array { subscribers : List MVarSubscriber_Stream_Maybe_DMsg, value : Maybe MVar_ChItem_Maybe_DMsg }
    , mVars_ChItem_Maybe_DMsg : Array { subscribers : List MVarSubscriber_ChItem_Maybe_DMsg, value : Maybe ChItem_Maybe_DMsg }
    , mVars_StreamResultBMsgBResultDocumentation : Array { subscribers : List MVarSubscriber_StreamResultBMsgBResultDocumentation, value : Maybe MVar_ChItemResultBMsgBResultDocumentation }
    , mVars_ChItemResultBMsgBResultDocumentation : Array { subscribers : List MVarSubscriber_ChItemResultBMsgBResultDocumentation, value : Maybe ChItem_ResultBMsgBResultDocumentation }
    , mVars_StreamResultBMsgBResultUnit : Array { subscribers : List MVarSubscriber_StreamResultBMsgBResultUnit, value : Maybe MVar_ChItemResultBMsgBResultUnit }
    , mVars_ChItemResultBMsgBResultUnit : Array { subscribers : List MVarSubscriber_ChItemResultBMsgBResultUnit, value : Maybe ChItem_ResultBMsgBResultUnit }
    , mVars_StreamResultBMsgBResultArtifacts : Array { subscribers : List MVarSubscriber_StreamResultBMsgBResultArtifacts, value : Maybe MVar_ChItemResultBMsgBResultArtifacts }
    , mVars_ChItemResultBMsgBResultArtifacts : Array { subscribers : List MVarSubscriber_ChItemResultBMsgBResultArtifacts, value : Maybe ChItem_ResultBMsgBResultArtifacts }
    , mVars_ResultBMsgBResultArtifacts : Array { subscribers : List MVarSubscriber_ResultBMsgBResultArtifacts, value : Maybe (Result BR_BMsg (BR_BResult BB_Artifacts)) }
    , next : Dict Int Next
    }


type MVarSubscriber
    = ReadMVarSubscriber Int
    | TakeMVarSubscriber Int
    | PutMVarSubscriber Int Encode.Value


type MVarSubscriber_Maybe_BED_Status
    = ReadMVarSubscriber_Maybe_BED_Status Int
    | TakeMVarSubscriber_Maybe_BED_Status Int
    | PutMVarSubscriber_Maybe_BED_Status Int (Maybe BED_Status)


type MVarSubscriber_Maybe_BED_DResult
    = ReadMVarSubscriber_Maybe_BED_DResult Int
    | TakeMVarSubscriber_Maybe_BED_DResult Int
    | PutMVarSubscriber_Maybe_BED_DResult Int (Maybe BED_DResult)


type MVarSubscriber_Maybe_CASTO_LocalGraph
    = ReadMVarSubscriber_Maybe_CASTO_LocalGraph Int
    | TakeMVarSubscriber_Maybe_CASTO_LocalGraph Int
    | PutMVarSubscriber_Maybe_CASTO_LocalGraph Int (Maybe CASTO_LocalGraph)


type MVarSubscriber_Maybe_CASTO_GlobalGraph
    = ReadMVarSubscriber_Maybe_CASTO_GlobalGraph Int
    | TakeMVarSubscriber_Maybe_CASTO_GlobalGraph Int
    | PutMVarSubscriber_Maybe_CASTO_GlobalGraph Int (Maybe CASTO_GlobalGraph)


type MVarSubscriber_Result_BuildProjectProblem_RootInfo
    = ReadMVarSubscriber_Result_BuildProjectProblem_RootInfo Int
    | TakeMVarSubscriber_Result_BuildProjectProblem_RootInfo Int
    | PutMVarSubscriber_Result_BuildProjectProblem_RootInfo Int (Result BRE_BuildProjectProblem BB_RootInfo)


type MVarSubscriber_MaybeDep
    = ReadMVarSubscriber_MaybeDep Int
    | TakeMVarSubscriber_MaybeDep Int
    | PutMVarSubscriber_MaybeDep Int (Maybe BB_Dep)


type MVarSubscriber_BB_RootResult
    = ReadMVarSubscriber_BB_RootResult Int
    | TakeMVarSubscriber_BB_RootResult Int
    | PutMVarSubscriber_BB_RootResult Int BB_RootResult


type MVarSubscriber_BB_RootStatus
    = ReadMVarSubscriber_BB_RootStatus Int
    | TakeMVarSubscriber_BB_RootStatus Int
    | PutMVarSubscriber_BB_RootStatus Int BB_RootStatus


type MVarSubscriber_BB_BResult
    = ReadMVarSubscriber_BB_BResult Int
    | TakeMVarSubscriber_BB_BResult Int
    | PutMVarSubscriber_BB_BResult Int BB_BResult


type MVarSubscriber_BB_Status
    = ReadMVarSubscriber_BB_Status Int
    | TakeMVarSubscriber_BB_Status Int
    | PutMVarSubscriber_BB_Status Int BB_Status


type MVarSubscriber_BB_StatusDict
    = ReadMVarSubscriber_BB_StatusDict Int
    | TakeMVarSubscriber_BB_StatusDict Int
    | PutMVarSubscriber_BB_StatusDict Int BB_StatusDict


type MVarSubscriber_ResultRegistryProblemEnv
    = ReadMVarSubscriber_ResultRegistryProblemEnv Int
    | TakeMVarSubscriber_ResultRegistryProblemEnv Int
    | PutMVarSubscriber_ResultRegistryProblemEnv Int (Result BRE_RegistryProblem BDS_Env)


type MVarSubscriber_CED_Dep
    = ReadMVarSubscriber_CED_Dep Int
    | TakeMVarSubscriber_CED_Dep Int
    | PutMVarSubscriber_CED_Dep Int CED_Dep


type MVarSubscriber_Maybe_CECTE_Types
    = ReadMVarSubscriber_Maybe_CECTE_Types Int
    | TakeMVarSubscriber_Maybe_CECTE_Types Int
    | PutMVarSubscriber_Maybe_CECTE_Types Int (Maybe CECTE_Types)


type MVarSubscriber_Maybe_BB_Dependencies
    = ReadMVarSubscriber_Maybe_BB_Dependencies Int
    | TakeMVarSubscriber_Maybe_BB_Dependencies Int
    | PutMVarSubscriber_Maybe_BB_Dependencies Int (Maybe BB_Dependencies)


type MVarSubscriber_DictNameMVarDep
    = ReadMVarSubscriber_DictNameMVarDep Int
    | TakeMVarSubscriber_DictNameMVarDep Int
    | PutMVarSubscriber_DictNameMVarDep Int (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep)


type MVarSubscriber_DictRawMVarMaybeDResult
    = ReadMVarSubscriber_DictRawMVarMaybeDResult Int
    | TakeMVarSubscriber_DictRawMVarMaybeDResult Int
    | PutMVarSubscriber_DictRawMVarMaybeDResult Int (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult)


type MVarSubscriber_ListMVar
    = ReadMVarSubscriber_ListMVar Int
    | TakeMVarSubscriber_ListMVar Int
    | PutMVarSubscriber_ListMVar Int (List MVar_Unit)


type MVarSubscriber_BB_CachedInterface
    = ReadMVarSubscriber_BB_CachedInterface Int
    | TakeMVarSubscriber_BB_CachedInterface Int
    | PutMVarSubscriber_BB_CachedInterface Int BB_CachedInterface


type MVarSubscriber_BED_StatusDict
    = ReadMVarSubscriber_BED_StatusDict Int
    | TakeMVarSubscriber_BED_StatusDict Int
    | PutMVarSubscriber_BED_StatusDict Int BED_StatusDict


type MVarSubscriber_Manager
    = ReadMVarSubscriber_Manager Int
    | TakeMVarSubscriber_Manager Int
    | PutMVarSubscriber_Manager Int BH_Manager


type MVarSubscriber_BB_ResultDict
    = ReadMVarSubscriber_BB_ResultDict Int
    | TakeMVarSubscriber_BB_ResultDict Int
    | PutMVarSubscriber_BB_ResultDict Int BB_ResultDict


type MVarSubscriber_Unit
    = ReadMVarSubscriber_Unit Int
    | TakeMVarSubscriber_Unit Int
    | PutMVarSubscriber_Unit Int ()


type MVarSubscriber_Stream_Maybe_DMsg
    = ReadMVarSubscriber_Stream_Maybe_DMsg Int
    | TakeMVarSubscriber_Stream_Maybe_DMsg Int
    | PutMVarSubscriber_Stream_Maybe_DMsg Int MVar_ChItem_Maybe_DMsg


type MVarSubscriber_ChItem_Maybe_DMsg
    = ReadMVarSubscriber_ChItem_Maybe_DMsg Int
    | TakeMVarSubscriber_ChItem_Maybe_DMsg Int
    | PutMVarSubscriber_ChItem_Maybe_DMsg Int ChItem_Maybe_DMsg


type MVarSubscriber_StreamResultBMsgBResultDocumentation
    = ReadMVarSubscriber_StreamResultBMsgBResultDocumentation Int
    | TakeMVarSubscriber_StreamResultBMsgBResultDocumentation Int
    | PutMVarSubscriber_StreamResultBMsgBResultDocumentation Int MVar_ChItemResultBMsgBResultDocumentation


type MVarSubscriber_ChItemResultBMsgBResultDocumentation
    = ReadMVarSubscriber_ChItemResultBMsgBResultDocumentation Int
    | TakeMVarSubscriber_ChItemResultBMsgBResultDocumentation Int
    | PutMVarSubscriber_ChItemResultBMsgBResultDocumentation Int ChItem_ResultBMsgBResultDocumentation


type MVarSubscriber_StreamResultBMsgBResultUnit
    = ReadMVarSubscriber_StreamResultBMsgBResultUnit Int
    | TakeMVarSubscriber_StreamResultBMsgBResultUnit Int
    | PutMVarSubscriber_StreamResultBMsgBResultUnit Int MVar_ChItemResultBMsgBResultUnit


type MVarSubscriber_ChItemResultBMsgBResultUnit
    = ReadMVarSubscriber_ChItemResultBMsgBResultUnit Int
    | TakeMVarSubscriber_ChItemResultBMsgBResultUnit Int
    | PutMVarSubscriber_ChItemResultBMsgBResultUnit Int ChItem_ResultBMsgBResultUnit


type MVarSubscriber_StreamResultBMsgBResultArtifacts
    = ReadMVarSubscriber_StreamResultBMsgBResultArtifacts Int
    | TakeMVarSubscriber_StreamResultBMsgBResultArtifacts Int
    | PutMVarSubscriber_StreamResultBMsgBResultArtifacts Int MVar_ChItemResultBMsgBResultArtifacts


type MVarSubscriber_ChItemResultBMsgBResultArtifacts
    = ReadMVarSubscriber_ChItemResultBMsgBResultArtifacts Int
    | TakeMVarSubscriber_ChItemResultBMsgBResultArtifacts Int
    | PutMVarSubscriber_ChItemResultBMsgBResultArtifacts Int ChItem_ResultBMsgBResultArtifacts


type MVarSubscriber_ResultBMsgBResultArtifacts
    = ReadMVarSubscriber_ResultBMsgBResultArtifacts Int
    | TakeMVarSubscriber_ResultBMsgBResultArtifacts Int
    | PutMVarSubscriber_ResultBMsgBResultArtifacts Int (Result BR_BMsg (BR_BResult BB_Artifacts))


type Next
    = GetLineNext (String -> IO ())
    | HPutLineNext (() -> IO ())
    | WriteStringNext (() -> IO ())
    | ReadNext (String -> IO ())
    | HttpFetchNext (String -> IO ())
    | GetArchiveNext (( String, CAZ_Archive ) -> IO ())
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
      -- MVars (Maybe BED_Status)
    | NewEmptyMVarNext_Maybe_BED_Status (Int -> IO ())
    | ReadMVarNext_Maybe_BED_Status (Maybe BED_Status -> IO ())
    | PutMVarNext_Maybe_BED_Status (() -> IO ())
      -- MVars (Maybe BED_DResult)
    | NewEmptyMVarNext_Maybe_BED_DResult (Int -> IO ())
    | ReadMVarNext_Maybe_BED_DResult (Maybe BED_DResult -> IO ())
    | PutMVarNext_Maybe_BED_DResult (() -> IO ())
      -- MVars (Maybe CASTO_LocalGraph)
    | NewEmptyMVarNext_Maybe_CASTO_LocalGraph (Int -> IO ())
    | ReadMVarNext_Maybe_CASTO_LocalGraph (Maybe CASTO_LocalGraph -> IO ())
    | PutMVarNext_Maybe_CASTO_LocalGraph (() -> IO ())
      -- MVars (Maybe CASTO_GlobalGraph)
    | NewEmptyMVarNext_Maybe_CASTO_GlobalGraph (Int -> IO ())
    | ReadMVarNext_Maybe_CASTO_GlobalGraph (Maybe CASTO_GlobalGraph -> IO ())
    | PutMVarNext_Maybe_CASTO_GlobalGraph (() -> IO ())
      -- MVars (Result BRE_BuildProjectProblem BB_RootInfo)
    | NewEmptyMVarNext_Result_BuildProjectProblem_RootInfo (Int -> IO ())
    | ReadMVarNext_Result_BuildProjectProblem_RootInfo (Result BRE_BuildProjectProblem BB_RootInfo -> IO ())
    | PutMVarNext_Result_BuildProjectProblem_RootInfo (() -> IO ())
      -- MVars (Maybe BB_Dep)
    | NewEmptyMVarNext_MaybeDep (Int -> IO ())
    | ReadMVarNext_MaybeDep (Maybe BB_Dep -> IO ())
    | PutMVarNext_MaybeDep (() -> IO ())
      -- MVars (BB_RootResult)
    | NewEmptyMVarNext_BB_RootResult (Int -> IO ())
    | ReadMVarNext_BB_RootResult (BB_RootResult -> IO ())
    | PutMVarNext_BB_RootResult (() -> IO ())
      -- MVars (BB_RootStatus)
    | NewEmptyMVarNext_BB_RootStatus (Int -> IO ())
    | ReadMVarNext_BB_RootStatus (BB_RootStatus -> IO ())
    | PutMVarNext_BB_RootStatus (() -> IO ())
      -- MVars (BB_BResult)
    | NewEmptyMVarNext_BB_BResult (Int -> IO ())
    | ReadMVarNext_BB_BResult (BB_BResult -> IO ())
    | PutMVarNext_BB_BResult (() -> IO ())
      -- MVars (BB_Status)
    | NewEmptyMVarNext_BB_Status (Int -> IO ())
    | ReadMVarNext_BB_Status (BB_Status -> IO ())
    | PutMVarNext_BB_Status (() -> IO ())
      -- MVars (BB_StatusDict)
    | NewEmptyMVarNext_BB_StatusDict (Int -> IO ())
    | ReadMVarNext_BB_StatusDict (BB_StatusDict -> IO ())
    | TakeMVarNext_BB_StatusDict (BB_StatusDict -> IO ())
    | PutMVarNext_BB_StatusDict (() -> IO ())
      -- MVars (Result BRE_RegistryProblem BDS_Env)
    | NewEmptyMVarNext_ResultRegistryProblemEnv (Int -> IO ())
    | ReadMVarNext_ResultRegistryProblemEnv (Result BRE_RegistryProblem BDS_Env -> IO ())
    | TakeMVarNext_ResultRegistryProblemEnv (Result BRE_RegistryProblem BDS_Env -> IO ())
    | PutMVarNext_ResultRegistryProblemEnv (() -> IO ())
      -- MVars (Result BRE_RegistryProblem BDS_Env)
    | NewEmptyMVarNext_CED_Dep (Int -> IO ())
    | ReadMVarNext_CED_Dep (CED_Dep -> IO ())
    | TakeMVarNext_CED_Dep (CED_Dep -> IO ())
    | PutMVarNext_CED_Dep (() -> IO ())
      -- MVars (Maybe CECTE_Types)
    | NewEmptyMVarNext_Maybe_CECTE_Types (Int -> IO ())
    | ReadMVarNext_Maybe_CECTE_Types (Maybe CECTE_Types -> IO ())
    | TakeMVarNext_Maybe_CECTE_Types (Maybe CECTE_Types -> IO ())
    | PutMVarNext_Maybe_CECTE_Types (() -> IO ())
      -- MVars (Maybe BB_Dependencies)
    | NewEmptyMVarNext_Maybe_BB_Dependencies (Int -> IO ())
    | ReadMVarNext_Maybe_BB_Dependencies (Maybe BB_Dependencies -> IO ())
    | TakeMVarNext_Maybe_BB_Dependencies (Maybe BB_Dependencies -> IO ())
    | PutMVarNext_Maybe_BB_Dependencies (() -> IO ())
      -- MVars (Dict ( String, String ) CEP_Name MVar_CED_Dep)
    | NewEmptyMVarNext_DictNameMVarDep (Int -> IO ())
    | ReadMVarNext_DictNameMVarDep (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep -> IO ())
    | TakeMVarNext_DictNameMVarDep (Map.Dict ( String, String ) CEP_Name MVar_CED_Dep -> IO ())
    | PutMVarNext_DictNameMVarDep (() -> IO ())
      -- MVars (Dict String CEMN_Raw MVar_Maybe_BED_DResult)
    | NewEmptyMVarNext_DictRawMVarMaybeDResult (Int -> IO ())
    | ReadMVarNext_DictRawMVarMaybeDResult (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult -> IO ())
    | TakeMVarNext_DictRawMVarMaybeDResult (Map.Dict String CEMN_Raw MVar_Maybe_BED_DResult -> IO ())
    | PutMVarNext_DictRawMVarMaybeDResult (() -> IO ())
      -- MVars (List (MVar_Unit))
    | NewEmptyMVarNext_ListMVar (Int -> IO ())
    | ReadMVarNext_ListMVar (List MVar_Unit -> IO ())
    | TakeMVarNext_ListMVar (List MVar_Unit -> IO ())
    | PutMVarNext_ListMVar (() -> IO ())
      -- MVars (BB_CachedInterface)
    | NewEmptyMVarNext_BB_CachedInterface (Int -> IO ())
    | ReadMVarNext_BB_CachedInterface (BB_CachedInterface -> IO ())
    | TakeMVarNext_BB_CachedInterface (BB_CachedInterface -> IO ())
    | PutMVarNext_BB_CachedInterface (() -> IO ())
      -- MVars (BED_StatusDict)
    | NewEmptyMVarNext_BED_StatusDict (Int -> IO ())
    | ReadMVarNext_BED_StatusDict (BED_StatusDict -> IO ())
    | TakeMVarNext_BED_StatusDict (BED_StatusDict -> IO ())
    | PutMVarNext_BED_StatusDict (() -> IO ())
      -- MVars (Unit)
    | NewEmptyMVarNext_Unit (Int -> IO ())
    | ReadMVarNext_Unit (() -> IO ())
    | TakeMVarNext_Unit (() -> IO ())
    | PutMVarNext_Unit (() -> IO ())
      -- MVars (BH_Manager)
    | NewEmptyMVarNext_Manager (Int -> IO ())
    | ReadMVarNext_Manager (BH_Manager -> IO ())
    | TakeMVarNext_Manager (BH_Manager -> IO ())
    | PutMVarNext_Manager (() -> IO ())
      -- MVars (BB_ResultDict)
    | NewEmptyMVarNext_BB_ResultDict (Int -> IO ())
    | ReadMVarNext_BB_ResultDict (BB_ResultDict -> IO ())
    | TakeMVarNext_BB_ResultDict (BB_ResultDict -> IO ())
    | PutMVarNext_BB_ResultDict (() -> IO ())
      -- MVars (MVar_ChItem_Maybe_DMsg)
    | NewEmptyMVarNext_Stream_Maybe_DMsg (Int -> IO ())
    | ReadMVarNext_Stream_Maybe_DMsg (MVar_ChItem_Maybe_DMsg -> IO ())
    | TakeMVarNext_Stream_Maybe_DMsg (MVar_ChItem_Maybe_DMsg -> IO ())
    | PutMVarNext_Stream_Maybe_DMsg (() -> IO ())
      -- MVars (ChItem_Maybe_DMsg)
    | NewEmptyMVarNext_ChItem_Maybe_DMsg (Int -> IO ())
    | ReadMVarNext_ChItem_Maybe_DMsg (ChItem_Maybe_DMsg -> IO ())
    | TakeMVarNext_ChItem_Maybe_DMsg (ChItem_Maybe_DMsg -> IO ())
    | PutMVarNext_ChItem_Maybe_DMsg (() -> IO ())
      -- MVars (MVar_ChItemResultBMsgBResultDocumentation)
    | NewEmptyMVarNext_StreamResultBMsgBResultDocumentation (Int -> IO ())
    | ReadMVarNext_StreamResultBMsgBResultDocumentation (MVar_ChItemResultBMsgBResultDocumentation -> IO ())
    | TakeMVarNext_StreamResultBMsgBResultDocumentation (MVar_ChItemResultBMsgBResultDocumentation -> IO ())
    | PutMVarNext_StreamResultBMsgBResultDocumentation (() -> IO ())
      -- MVars (ChItem_ResultBMsgBResultDocumentation)
    | NewEmptyMVarNext_ChItemResultBMsgBResultDocumentation (Int -> IO ())
    | ReadMVarNext_ChItemResultBMsgBResultDocumentation (ChItem_ResultBMsgBResultDocumentation -> IO ())
    | TakeMVarNext_ChItemResultBMsgBResultDocumentation (ChItem_ResultBMsgBResultDocumentation -> IO ())
    | PutMVarNext_ChItemResultBMsgBResultDocumentation (() -> IO ())
      -- MVars (MVar_ChItemResultBMsgBResultUnit)
    | NewEmptyMVarNext_StreamResultBMsgBResultUnit (Int -> IO ())
    | ReadMVarNext_StreamResultBMsgBResultUnit (MVar_ChItemResultBMsgBResultUnit -> IO ())
    | TakeMVarNext_StreamResultBMsgBResultUnit (MVar_ChItemResultBMsgBResultUnit -> IO ())
    | PutMVarNext_StreamResultBMsgBResultUnit (() -> IO ())
      -- MVars (ChItem_ResultBMsgBResultUnit)
    | NewEmptyMVarNext_ChItemResultBMsgBResultUnit (Int -> IO ())
    | ReadMVarNext_ChItemResultBMsgBResultUnit (ChItem_ResultBMsgBResultUnit -> IO ())
    | TakeMVarNext_ChItemResultBMsgBResultUnit (ChItem_ResultBMsgBResultUnit -> IO ())
    | PutMVarNext_ChItemResultBMsgBResultUnit (() -> IO ())
      -- MVars (MVar_ChItemResultBMsgBResultArtifacts)
    | NewEmptyMVarNext_StreamResultBMsgBResultArtifacts (Int -> IO ())
    | ReadMVarNext_StreamResultBMsgBResultArtifacts (MVar_ChItemResultBMsgBResultArtifacts -> IO ())
    | TakeMVarNext_StreamResultBMsgBResultArtifacts (MVar_ChItemResultBMsgBResultArtifacts -> IO ())
    | PutMVarNext_StreamResultBMsgBResultArtifacts (() -> IO ())
      -- MVars (ChItem_ResultBMsgBResultArtifacts)
    | NewEmptyMVarNext_ChItemResultBMsgBResultArtifacts (Int -> IO ())
    | ReadMVarNext_ChItemResultBMsgBResultArtifacts (ChItem_ResultBMsgBResultArtifacts -> IO ())
    | TakeMVarNext_ChItemResultBMsgBResultArtifacts (ChItem_ResultBMsgBResultArtifacts -> IO ())
    | PutMVarNext_ChItemResultBMsgBResultArtifacts (() -> IO ())
      -- MVars (Result BR_BMsg (BR_BResult BB_Artifacts))
    | NewEmptyMVarNext_ResultBMsgBResultArtifacts (Int -> IO ())
    | ReadMVarNext_ResultBMsgBResultArtifacts (Result BR_BMsg (BR_BResult BB_Artifacts) -> IO ())
    | TakeMVarNext_ResultBMsgBResultArtifacts (Result BR_BMsg (BR_BResult BB_Artifacts) -> IO ())
    | PutMVarNext_ResultBMsgBResultArtifacts (() -> IO ())


type IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode



-- Repl State (Terminal.Repl)


type ReplState
    = ReplState (Dict String String) (Dict String String) (Dict String String)



-- Files and handles


{-| GHC.IO
-}
type alias FilePath =
    String


type Handle
    = Handle Int



-- CRAWL


{-| FIXME Builder.Elm.Details
-}
type alias BED_StatusDict =
    Map.Dict String CEMN_Raw MVar_Maybe_BED_Status


{-| FIXME Builder.Elm.Details
-}
type BED_Status
    = BED_SLocal BED_DocsStatus (Map.Dict String CEMN_Raw ()) CASTS_Module
    | BED_SForeign CEI_Interface
    | BED_SKernelLocal (List CEK_Chunk)
    | BED_SKernelForeign



-- MAKE DOCS


{-| FIXME Builder.Elm.Details
-}
type BED_DocsStatus
    = BED_DocsNeeded
    | BED_DocsNotNeeded



-- TYPES


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Annotation
    = CASTC_Forall CASTC_FreeVars CASTC_Type


{-| FIXME Compiler.AST.Canonical
-}
type alias CASTC_FreeVars =
    Map.Dict String CDN_Name ()


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Type
    = CASTC_TLambda CASTC_Type CASTC_Type
    | CASTC_TVar CDN_Name
    | CASTC_TType CEMN_Canonical CDN_Name (List CASTC_Type)
    | CASTC_TRecord (Map.Dict String CDN_Name CASTC_FieldType) (Maybe CDN_Name)
    | CASTC_TUnit
    | CASTC_TTuple CASTC_Type CASTC_Type (Maybe CASTC_Type)
    | CASTC_TAlias CEMN_Canonical CDN_Name (List ( CDN_Name, CASTC_Type )) CASTC_AliasType


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_AliasType
    = CASTC_Holey CASTC_Type
    | CASTC_Filled CASTC_Type


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_FieldType
    = CASTC_FieldType Int CASTC_Type



-- MODULES


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Alias
    = CASTC_Alias (List CDN_Name) CASTC_Type


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Union
    = CASTC_Union
        (List CDN_Name)
        (List CASTC_Ctor)
        -- CACHE numAlts for exhaustiveness checking
        Int
        -- CACHE which optimizations are available
        CASTC_CtorOpts


type CASTC_CtorOpts
    = CASTC_Normal
    | CASTC_Enum
    | CASTC_Unbox


type CASTC_Ctor
    = CASTC_Ctor CDN_Name CDI_ZeroBased Int (List CASTC_Type) -- CACHE length args



-- PATTERN


{-| FIXME Compiler.AST.Source
-}
type alias CASTS_Pattern =
    CRA_Located CASTS_Pattern_


{-| FIXME Compiler.AST.Source
-}
type CASTS_Pattern_
    = CASTS_PAnything
    | CASTS_PVar CDN_Name
    | CASTS_PRecord (List (CRA_Located CDN_Name))
    | CASTS_PAlias CASTS_Pattern (CRA_Located CDN_Name)
    | CASTS_PUnit
    | CASTS_PTuple CASTS_Pattern CASTS_Pattern (List CASTS_Pattern)
    | CASTS_PCtor CRA_Region CDN_Name (List CASTS_Pattern)
    | CASTS_PCtorQual CRA_Region CDN_Name CDN_Name (List CASTS_Pattern)
    | CASTS_PList (List CASTS_Pattern)
    | CASTS_PCons CASTS_Pattern CASTS_Pattern
    | CASTS_PChr String
    | CASTS_PStr String
    | CASTS_PInt Int



-- TYPE


{-| FIXME Compiler.AST.Source
-}
type alias CASTS_Type =
    CRA_Located CASTS_Type_


{-| FIXME Compiler.AST.Source
-}
type CASTS_Type_
    = CASTS_TLambda CASTS_Type CASTS_Type
    | CASTS_TVar CDN_Name
    | CASTS_TType CRA_Region CDN_Name (List CASTS_Type)
    | CASTS_TTypeQual CRA_Region CDN_Name CDN_Name (List CASTS_Type)
    | CASTS_TRecord (List ( CRA_Located CDN_Name, CASTS_Type )) (Maybe (CRA_Located CDN_Name))
    | CASTS_TUnit
    | CASTS_TTuple CASTS_Type CASTS_Type (List CASTS_Type)



-- EXPRESSIONS


type alias CASTS_Expr =
    CRA_Located CASTS_Expr_


type CASTS_Expr_
    = CASTS_Chr String
    | CASTS_Str String
    | CASTS_Int Int
    | CASTS_Float Float
    | CASTS_Var CASTS_VarType CDN_Name
    | CASTS_VarQual CASTS_VarType CDN_Name CDN_Name
    | CASTS_List (List CASTS_Expr)
    | CASTS_Op CDN_Name
    | CASTS_Negate CASTS_Expr
    | CASTS_Binops (List ( CASTS_Expr, CRA_Located CDN_Name )) CASTS_Expr
    | CASTS_Lambda (List CASTS_Pattern) CASTS_Expr
    | CASTS_Call CASTS_Expr (List CASTS_Expr)
    | CASTS_If (List ( CASTS_Expr, CASTS_Expr )) CASTS_Expr
    | CASTS_Let (List (CRA_Located CASTS_Def)) CASTS_Expr
    | CASTS_Case CASTS_Expr (List ( CASTS_Pattern, CASTS_Expr ))
    | CASTS_Accessor CDN_Name
    | CASTS_Access CASTS_Expr (CRA_Located CDN_Name)
    | CASTS_Update (CRA_Located CDN_Name) (List ( CRA_Located CDN_Name, CASTS_Expr ))
    | CASTS_Record (List ( CRA_Located CDN_Name, CASTS_Expr ))
    | CASTS_Unit
    | CASTS_Tuple CASTS_Expr CASTS_Expr (List CASTS_Expr)
    | CASTS_Shader CASTUS_Source CASTUS_Types


type CASTS_VarType
    = CASTS_LowVar
    | CASTS_CapVar



-- DEFINITIONS


type CASTS_Def
    = CASTS_Define (CRA_Located CDN_Name) (List CASTS_Pattern) CASTS_Expr (Maybe CASTS_Type)
    | CASTS_Destruct CASTS_Pattern CASTS_Expr



-- MODULE


{-| FIXME Compiler.AST.Source
-}
type CASTS_Module
    = CASTS_Module (Maybe (CRA_Located CDN_Name)) (CRA_Located CASTS_Exposing) CASTS_Docs (List CASTS_Import) (List (CRA_Located CASTS_Value)) (List (CRA_Located CASTS_Union)) (List (CRA_Located CASTS_Alias)) (List (CRA_Located CASTS_Infix)) CASTS_Effects


{-| FIXME Compiler.AST.Source
-}
type CASTS_Import
    = CASTS_Import (CRA_Located CDN_Name) (Maybe CDN_Name) CASTS_Exposing


{-| FIXME Compiler.AST.Source
-}
type CASTS_Value
    = CASTS_Value (CRA_Located CDN_Name) (List CASTS_Pattern) CASTS_Expr (Maybe CASTS_Type)


{-| FIXME Compiler.AST.Source
-}
type CASTS_Union
    = CASTS_Union (CRA_Located CDN_Name) (List (CRA_Located CDN_Name)) (List ( CRA_Located CDN_Name, List CASTS_Type ))


{-| FIXME Compiler.AST.Source
-}
type CASTS_Alias
    = CASTS_Alias (CRA_Located CDN_Name) (List (CRA_Located CDN_Name)) CASTS_Type


{-| FIXME Compiler.AST.Source
-}
type CASTS_Infix
    = CASTS_Infix CDN_Name CASTUB_Associativity CASTUB_Precedence CDN_Name


{-| FIXME Compiler.AST.Source
-}
type CASTS_Port
    = CASTS_Port (CRA_Located CDN_Name) CASTS_Type


{-| FIXME Compiler.AST.Source
-}
type CASTS_Effects
    = CASTS_NoEffects
    | CASTS_Ports (List CASTS_Port)
    | CASTS_Manager CRA_Region CASTS_Manager


{-| FIXME Compiler.AST.Source
-}
type CASTS_Manager
    = CASTS_Cmd (CRA_Located CDN_Name)
    | CASTS_Sub (CRA_Located CDN_Name)
    | CASTS_Fx (CRA_Located CDN_Name) (CRA_Located CDN_Name)


type CASTS_Docs
    = CASTS_NoDocs CRA_Region
    | CASTS_YesDocs CASTS_Comment (List ( CDN_Name, CASTS_Comment ))


type CASTS_Comment
    = CASTS_Comment CPP_Snippet



-- EXPOSING


{-| FIXME Compiler.AST.Source
-}
type CASTS_Exposing
    = CASTS_Open
    | CASTS_Explicit (List CASTS_Exposed)


{-| FIXME Compiler.AST.Source
-}
type CASTS_Exposed
    = CASTS_Lower (CRA_Located CDN_Name)
    | CASTS_Upper (CRA_Located CDN_Name) CASTS_Privacy
    | CASTS_Operator CRA_Region CDN_Name


{-| FIXME Compiler.AST.Source
-}
type CASTS_Privacy
    = CASTS_Public CRA_Region
    | CASTS_Private



-- BINOP STUFF


{-| FIXME Compiler.AST.Utils.Binop
-}
type alias CASTUB_Precedence =
    Int


{-| FIXME Compiler.AST.Utils.Binop
-}
type CASTUB_Associativity
    = CASTUB_Left
    | CASTUB_Non
    | CASTUB_Right



-- NAME


{-| FIXME Compiler.Data.Name
-}
type alias CDN_Name =
    String



-- INTERFACE


{-| FIXME Compiler.Elm.Interface
-}
type CEI_Interface
    = CEI_Interface CEP_Name (Map.Dict String CDN_Name CASTC_Annotation) (Map.Dict String CDN_Name CEI_Union) (Map.Dict String CDN_Name CEI_Alias) (Map.Dict String CDN_Name CEI_Binop)


{-| FIXME Compiler.Elm.Interface
-}
type CEI_Union
    = CEI_OpenUnion CASTC_Union
    | CEI_ClosedUnion CASTC_Union
    | CEI_PrivateUnion CASTC_Union


{-| FIXME Compiler.Elm.Interface
-}
type CEI_Alias
    = CEI_PublicAlias CASTC_Alias
    | CEI_PrivateAlias CASTC_Alias


{-| FIXME Compiler.Elm.Interface
-}
type CEI_Binop
    = CEI_Binop CDN_Name CASTC_Annotation CASTUB_Associativity CASTUB_Precedence



-- CHUNK


{-| FIXME Compiler.Elm.Kernel
-}
type CEK_Chunk
    = CEK_JS String
    | CEK_ElmVar CEMN_Canonical CDN_Name
    | CEK_JsVar CDN_Name CDN_Name
    | CEK_ElmField CDN_Name
    | CEK_JsField Int
    | CEK_JsEnum Int
    | CEK_Debug
    | CEK_Prod



-- RAW


{-| FIXME Compiler.Elm.ModuleName
-}
type alias CEMN_Raw =
    CDN_Name



-- PACKAGE NAMES


{-| FIXME Compiler.Elm.Package

This has been simplified from `Name Author Project` as part of the work for
`System.TypeCheck.IO`.

-}
type alias CEP_Name =
    ( CEP_Author, CEP_Project )


{-| FIXME Compiler.Elm.Package
-}
type alias CEP_Author =
    String


{-| FIXME Compiler.Elm.Package
-}
type alias CEP_Project =
    String



-- LOCATED


{-| FIXME Compiler.Reporting.Annotation
-}
type CRA_Located a
    = CRA_At CRA_Region a -- PERF see if unpacking region is helpful



-- POSITION


{-| FIXME Compiler.Reporting.Annotation
-}
type CRA_Position
    = CRA_Position Int Int



-- REGION


{-| FIXME Compiler.Reporting.Annotation
-}
type CRA_Region
    = CRA_Region CRA_Position CRA_Position



-- CANONICAL


{-| FIXME Compiler.Elm.ModuleName
-}
type CEMN_Canonical
    = CEMN_Canonical ( String, String ) String



-- ZERO BASED


{-| FIXME Compiler.Data.Index
-}
type CDI_ZeroBased
    = CDI_ZeroBased Int



-- SOURCE


{-| FIXME Compiler.AST.Utils.Shader
-}
type CASTUS_Source
    = CASTUS_Source String



-- TYPES


{-| FIXME Compiler.AST.Utils.Shader
-}
type CASTUS_Types
    = CASTUS_Types (Map.Dict String CDN_Name CASTUS_Type) (Map.Dict String CDN_Name CASTUS_Type) (Map.Dict String CDN_Name CASTUS_Type)


{-| FIXME Compiler.AST.Utils.Shader
-}
type CASTUS_Type
    = CASTUS_Int
    | CASTUS_Float
    | CASTUS_V2
    | CASTUS_V3
    | CASTUS_V4
    | CASTUS_M4
    | CASTUS_Texture



-- PARSER


{-| FIXME Compiler.Parse.Primitives
-}
type alias CPP_Row =
    Int


{-| FIXME Compiler.Parse.Primitives
-}
type alias CPP_Col =
    Int



-- FROM SNIPPET


{-| FIXME Compiler.Parse.Primitives
-}
type CPP_Snippet
    = CPP_Snippet
        { fptr : String
        , offset : Int
        , length : Int
        , offRow : CPP_Row
        , offCol : CPP_Col
        }



-- Control.Concurrent.MVar


{-| FIXME Utils.Main
-}
type MVar a
    = MVar Int


{-| FIXME Utils.Main
-}
type MVar_Maybe_BED_Status
    = MVar_Maybe_BED_Status Int


{-| FIXME Utils.Main
-}
type MVar_Maybe_BED_DResult
    = MVar_Maybe_BED_DResult Int


{-| FIXME Utils.Main
-}
type MVar_Maybe_CASTO_LocalGraph
    = MVar_Maybe_CASTO_LocalGraph Int


{-| FIXME Utils.Main
-}
type MVar_Maybe_CASTO_GlobalGraph
    = MVar_Maybe_CASTO_GlobalGraph Int


{-| FIXME Utils.Main
-}
type MVar_Result_BuildProjectProblem_RootInfo
    = MVar_Result_BuildProjectProblem_RootInfo Int


{-| FIXME Utils.Main
-}
type MVar_MaybeDep
    = MVar_MaybeDep Int


{-| FIXME Utils.Main
-}
type MVar_BB_RootResult
    = MVar_BB_RootResult Int


{-| FIXME Utils.Main
-}
type MVar_BB_RootStatus
    = MVar_BB_RootStatus Int


{-| FIXME Utils.Main
-}
type MVar_BB_BResult
    = MVar_BB_BResult Int


{-| FIXME Utils.Main
-}
type MVar_BB_Status
    = MVar_BB_Status Int


{-| FIXME Utils.Main
-}
type MVar_BB_StatusDict
    = MVar_BB_StatusDict Int


{-| FIXME Utils.Main
-}
type MVar_ResultRegistryProblemEnv
    = MVar_ResultRegistryProblemEnv Int


{-| FIXME Utils.Main
-}
type MVar_CED_Dep
    = MVar_CED_Dep Int


{-| FIXME Utils.Main
-}
type MVar_Maybe_CECTE_Types
    = MVar_Maybe_CECTE_Types Int


{-| FIXME Utils.Main
-}
type MVar_Maybe_BB_Dependencies
    = MVar_Maybe_BB_Dependencies Int


{-| FIXME Utils.Main
-}
type MVar_DictNameMVarDep
    = MVar_DictNameMVarDep Int


{-| FIXME Utils.Main
-}
type MVar_DictRawMVarMaybeDResult
    = MVar_DictRawMVarMaybeDResult Int


{-| FIXME Utils.Main
-}
type MVar_ListMVar
    = MVar_ListMVar Int


{-| FIXME Utils.Main
-}
type MVar_BB_CachedInterface
    = MVar_BB_CachedInterface Int


{-| FIXME Utils.Main
-}
type MVar_BED_StatusDict
    = MVar_BED_StatusDict Int


{-| FIXME Utils.Main
-}
type MVar_Unit
    = MVar_Unit Int


{-| FIXME Utils.Main
-}
type MVar_Manager
    = MVar_Manager Int


{-| FIXME Utils.Main
-}
type MVar_BB_ResultDict
    = MVar_BB_ResultDict Int


{-| FIXME Utils.Main
-}
type MVar_Stream_Maybe_DMsg
    = MVar_Stream_Maybe_DMsg Int


{-| FIXME Utils.Main
-}
type MVar_ChItem_Maybe_DMsg
    = MVar_ChItem_Maybe_DMsg Int


{-| FIXME Utils.Main
-}
type MVar_StreamResultBMsgBResultDocumentation
    = MVar_StreamResultBMsgBResultDocumentation Int


{-| FIXME Utils.Main
-}
type MVar_ChItemResultBMsgBResultDocumentation
    = MVar_ChItemResultBMsgBResultDocumentation Int


{-| FIXME Utils.Main
-}
type MVar_StreamResultBMsgBResultUnit
    = MVar_StreamResultBMsgBResultUnit Int


{-| FIXME Utils.Main
-}
type MVar_ChItemResultBMsgBResultUnit
    = MVar_ChItemResultBMsgBResultUnit Int


{-| FIXME Utils.Main
-}
type MVar_StreamResultBMsgBResultArtifacts
    = MVar_StreamResultBMsgBResultArtifacts Int


{-| FIXME Utils.Main
-}
type MVar_ChItemResultBMsgBResultArtifacts
    = MVar_ChItemResultBMsgBResultArtifacts Int


{-| FIXME Utils.Main
-}
type MVar_ResultBMsgBResultArtifacts
    = MVar_ResultBMsgBResultArtifacts Int



-- Control.Concurrent.Chan


{-| FIXME Utils.Main
-}
type alias Stream a =
    MVar (ChItem a)


{-| FIXME Utils.Main
-}
type ChItem a
    = ChItem a (Stream a)


{-| FIXME Utils.Main
-}
type ChItem_Maybe_DMsg
    = ChItem_Maybe_DMsg (Maybe BR_DMsg) MVar_ChItem_Maybe_DMsg


{-| FIXME Utils.Main
-}
type ChItem_ResultBMsgBResultDocumentation
    = ChItem_ResultBMsgBResultDocumentation (Result BR_BMsg (BR_BResult CED_Documentation)) MVar_ChItemResultBMsgBResultDocumentation


{-| FIXME Utils.Main
-}
type ChItem_ResultBMsgBResultUnit
    = ChItem_ResultBMsgBResultUnit (Result BR_BMsg (BR_BResult ())) MVar_ChItemResultBMsgBResultUnit


{-| FIXME Utils.Main
-}
type ChItem_ResultBMsgBResultArtifacts
    = ChItem_ResultBMsgBResultArtifacts (Result BR_BMsg (BR_BResult BB_Artifacts)) MVar_ChItemResultBMsgBResultArtifacts



-- EXPRESSIONS


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Expr
    = CASTO_Bool Bool
    | CASTO_Chr String
    | CASTO_Str String
    | CASTO_Int Int
    | CASTO_Float Float
    | CASTO_VarLocal CDN_Name
    | CASTO_VarGlobal CASTO_Global
    | CASTO_VarEnum CASTO_Global CDI_ZeroBased
    | CASTO_VarBox CASTO_Global
    | CASTO_VarCycle CEMN_Canonical CDN_Name
    | CASTO_VarDebug CDN_Name CEMN_Canonical CRA_Region (Maybe CDN_Name)
    | CASTO_VarKernel CDN_Name CDN_Name
    | CASTO_List (List CASTO_Expr)
    | CASTO_Function (List CDN_Name) CASTO_Expr
    | CASTO_Call CASTO_Expr (List CASTO_Expr)
    | CASTO_TailCall CDN_Name (List ( CDN_Name, CASTO_Expr ))
    | CASTO_If (List ( CASTO_Expr, CASTO_Expr )) CASTO_Expr
    | CASTO_Let CASTO_Def CASTO_Expr
    | CASTO_Destruct CASTO_Destructor CASTO_Expr
    | CASTO_Case CDN_Name CDN_Name (CASTO_Decider CASTO_Choice) (List ( Int, CASTO_Expr ))
    | CASTO_Accessor CDN_Name
    | CASTO_Access CASTO_Expr CDN_Name
    | CASTO_Update CASTO_Expr (Map.Dict String CDN_Name CASTO_Expr)
    | CASTO_Record (Map.Dict String CDN_Name CASTO_Expr)
    | CASTO_Unit
    | CASTO_Tuple CASTO_Expr CASTO_Expr (Maybe CASTO_Expr)
    | CASTO_Shader CASTUS_Source (EverySet String CDN_Name) (EverySet String CDN_Name)


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Global
    = CASTO_Global CEMN_Canonical CDN_Name



-- DEFINITIONS


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Def
    = CASTO_Def CDN_Name CASTO_Expr
    | CASTO_TailDef CDN_Name (List CDN_Name) CASTO_Expr


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Destructor
    = CASTO_Destructor CDN_Name CASTO_Path


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Path
    = CASTO_Index CDI_ZeroBased CASTO_Path
    | CASTO_Field CDN_Name CASTO_Path
    | CASTO_Unbox CASTO_Path
    | CASTO_Root CDN_Name



-- BRANCHING


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Decider a
    = CASTO_Leaf a
    | CASTO_Chain (List ( CODT_Path, CODT_Test )) (CASTO_Decider a) (CASTO_Decider a)
    | CASTO_FanOut CODT_Path (List ( CODT_Test, CASTO_Decider a )) (CASTO_Decider a)


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Choice
    = CASTO_Inline CASTO_Expr
    | CASTO_Jump Int



-- OBJECT GRAPH


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_GlobalGraph
    = CASTO_GlobalGraph (Map.Dict (List String) CASTO_Global CASTO_Node) (Map.Dict String CDN_Name Int)


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_LocalGraph
    = CASTO_LocalGraph
        (Maybe CASTO_Main)
        -- PERF profile switching Global to Name
        (Map.Dict (List String) CASTO_Global CASTO_Node)
        (Map.Dict String CDN_Name Int)


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Main
    = CASTO_Static
    | CASTO_Dynamic CASTC_Type CASTO_Expr


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_Node
    = CASTO_Define CASTO_Expr (EverySet (List String) CASTO_Global)
    | CASTO_DefineTailFunc (List CDN_Name) CASTO_Expr (EverySet (List String) CASTO_Global)
    | CASTO_Ctor CDI_ZeroBased Int
    | CASTO_Enum CDI_ZeroBased
    | CASTO_Box
    | CASTO_Link CASTO_Global
    | CASTO_Cycle (List CDN_Name) (List ( CDN_Name, CASTO_Expr )) (List CASTO_Def) (EverySet (List String) CASTO_Global)
    | CASTO_Manager CASTO_EffectsType
    | CASTO_Kernel (List CEK_Chunk) (EverySet (List String) CASTO_Global)
    | CASTO_PortIncoming CASTO_Expr (EverySet (List String) CASTO_Global)
    | CASTO_PortOutgoing CASTO_Expr (EverySet (List String) CASTO_Global)


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_EffectsType
    = CASTO_Cmd
    | CASTO_Sub
    | CASTO_Fx



-- DECISION TREES


{-| FIXME Compiler.Optimize.DecisionTree
-}
type CODT_Test
    = CODT_IsCtor CEMN_Canonical CDN_Name CDI_ZeroBased Int CASTC_CtorOpts
    | CODT_IsCons
    | CODT_IsNil
    | CODT_IsTuple
    | CODT_IsInt Int
    | CODT_IsChr String
    | CODT_IsStr String
    | CODT_IsBool Bool


{-| FIXME Compiler.Optimize.DecisionTree
-}
type CODT_Path
    = CODT_Index CDI_ZeroBased CODT_Path
    | CODT_Unbox CODT_Path
    | CODT_Empty



-- CHECK MODULE


{-| FIXME Builder.Build
-}
type alias BB_ResultDict =
    Map.Dict String CEMN_Raw MVar_BB_BResult


{-| FIXME Builder.Build
-}
type BB_BResult
    = BB_RNew BED_Local CEI_Interface CASTO_LocalGraph (Maybe CED_Module)
    | BB_RSame BED_Local CEI_Interface CASTO_LocalGraph (Maybe CED_Module)
    | BB_RCached Bool BED_BuildID MVar_BB_CachedInterface
    | BB_RNotFound CREI_Problem
    | BB_RProblem CRE_Module
    | BB_RBlocked
    | BB_RForeign CEI_Interface
    | BB_RKernel


{-| FIXME Builder.Build
-}
type BB_CachedInterface
    = BB_Unneeded
    | BB_Loaded CEI_Interface
    | BB_Corrupted



-- CRAWL


{-| FIXME Builder.Build
-}
type alias BB_StatusDict =
    Map.Dict String CEMN_Raw MVar_BB_Status


{-| FIXME Builder.Build
-}
type BB_Status
    = BB_SCached BED_Local
    | BB_SChanged BED_Local String CASTS_Module BB_DocsNeed
    | BB_SBadImport CREI_Problem
    | BB_SBadSyntax FilePath BF_Time String CRES_Error
    | BB_SForeign CEP_Name
    | BB_SKernel


{-| FIXME Builder.Build
-}
type BB_DocsNeed
    = BB_DocsNeed Bool



-- TIME


{-| FIXME Builder.File
-}
type BF_Time
    = BF_Time Time.Posix



-- DETAILS


{-| FIXME Builder.Elm.Details
-}
type alias BED_BuildID =
    Int



-- NOTE: we need two ways to detect if a file must be recompiled:
--
-- (1) _time is the modification time from the last time we compiled the file.
-- By checking EQUALITY with the current modification time, we can detect file
-- saves and `git checkout` of previous versions. Both need a recompile.
--
-- (2) _lastChange is the BuildID from the last time a new interface file was
-- generated, and _lastCompile is the BuildID from the last time the file was
-- compiled. These may be different if a file is recompiled but the interface
-- stayed the same. When the _lastCompile is LESS THAN the _lastChange of any
-- imports, we need to recompile. This can happen when a project has multiple
-- entrypoints and some modules are compiled less often than their imports.
--


{-| FIXME Builder.Elm.Details
-}
type BED_Local
    = BED_Local FilePath BF_Time (List CEMN_Raw) Bool BED_BuildID BED_BuildID



-- DOCUMENTATION


{-| FIXME Compiler.Elm.Docs
-}
type alias CED_Documentation =
    Map.Dict String CDN_Name CED_Module


{-| FIXME Compiler.Elm.Docs
-}
type CED_Module
    = CED_Module CDN_Name CED_Comment (Map.Dict String CDN_Name CED_Union) (Map.Dict String CDN_Name CED_Alias) (Map.Dict String CDN_Name CED_Value) (Map.Dict String CDN_Name CED_Binop)


{-| FIXME Compiler.Elm.Docs
-}
type alias CED_Comment =
    String


{-| FIXME Compiler.Elm.Docs
-}
type CED_Alias
    = CED_Alias CED_Comment (List CDN_Name) CECT_Type


{-| FIXME Compiler.Elm.Docs
-}
type CED_Union
    = CED_Union CED_Comment (List CDN_Name) (List ( CDN_Name, List CECT_Type ))


{-| FIXME Compiler.Elm.Docs
-}
type CED_Value
    = CED_Value CED_Comment CECT_Type


{-| FIXME Compiler.Elm.Docs
-}
type CED_Binop
    = CED_Binop CED_Comment CECT_Type CASTUB_Associativity CASTUB_Precedence



-- TYPES


{-| FIXME Compiler.Elm.Compiler.Type
-}
type CECT_Type
    = CECT_Lambda CECT_Type CECT_Type
    | CECT_Var CDN_Name
    | CECT_Type CDN_Name (List CECT_Type)
    | CECT_Record (List ( CDN_Name, CECT_Type )) (Maybe CDN_Name)
    | CECT_Unit
    | CECT_Tuple CECT_Type CECT_Type (List CECT_Type)



-- PATTERN


{-| FIXME Compiler.Nitpick.PatternMatches
-}
type CNPM_Pattern
    = CNPM_Anything
    | CNPM_Literal CNPM_Literal
    | CNPM_Ctor CASTC_Union CDN_Name (List CNPM_Pattern)


{-| FIXME Compiler.Nitpick.PatternMatches
-}
type CNPM_Literal
    = CNPM_Chr String
    | CNPM_Str String
    | CNPM_Int Int



-- ERROR


{-| FIXME Compiler.Nitpick.PatternMatches
-}
type CNPM_Error
    = CNPM_Incomplete CRA_Region CNPM_Context (List CNPM_Pattern)
    | CNPM_Redundant CRA_Region CRA_Region Int


{-| FIXME Compiler.Nitpick.PatternMatches
-}
type CNPM_Context
    = CNPM_BadArg
    | CNPM_BadDestruct
    | CNPM_BadCase



-- OPERATOR


{-| FIXME Compiler.Parse.Symbol
-}
type CPS_BadOperator
    = CPS_BadDot
    | CPS_BadPipe
    | CPS_BadArrow
    | CPS_BadEquals
    | CPS_BadHasType



-- MODULE


{-| FIXME Compiler.Reporting.Error
-}
type alias CRE_Module =
    { name : CEMN_Raw
    , absolutePath : String
    , modificationTime : BF_Time
    , source : String
    , error : CRE_Error
    }



-- ERRORS


{-| FIXME Compiler.Reporting.Error
-}
type CRE_Error
    = CRE_BadSyntax CRES_Error
    | CRE_BadImports (NE.Nonempty CREI_Error)
    | CRE_BadNames (OneOrMore CREC_Error)
    | CRE_BadTypes CRRTL_Localizer (NE.Nonempty CRET_Error)
    | CRE_BadMains CRRTL_Localizer (OneOrMore CREM_Error)
    | CRE_BadPatterns (NE.Nonempty CNPM_Error)
    | CRE_BadDocs CRED_Error



-- CANONICALIZATION ERRORS


{-| FIXME Compiler.Reporting.Error.Canonicalize
-}
type CREC_Error
    = CREC_AnnotationTooShort CRA_Region CDN_Name CDI_ZeroBased Int
    | CREC_AmbiguousVar CRA_Region (Maybe CDN_Name) CDN_Name CEMN_Canonical (OneOrMore CEMN_Canonical)
    | CREC_AmbiguousType CRA_Region (Maybe CDN_Name) CDN_Name CEMN_Canonical (OneOrMore CEMN_Canonical)
    | CREC_AmbiguousVariant CRA_Region (Maybe CDN_Name) CDN_Name CEMN_Canonical (OneOrMore CEMN_Canonical)
    | CREC_AmbiguousBinop CRA_Region CDN_Name CEMN_Canonical (OneOrMore CEMN_Canonical)
    | CREC_BadArity CRA_Region CREC_BadArityContext CDN_Name Int Int
    | CREC_Binop CRA_Region CDN_Name CDN_Name
    | CREC_DuplicateDecl CDN_Name CRA_Region CRA_Region
    | CREC_DuplicateType CDN_Name CRA_Region CRA_Region
    | CREC_DuplicateCtor CDN_Name CRA_Region CRA_Region
    | CREC_DuplicateBinop CDN_Name CRA_Region CRA_Region
    | CREC_DuplicateField CDN_Name CRA_Region CRA_Region
    | CREC_DuplicateAliasArg CDN_Name CDN_Name CRA_Region CRA_Region
    | CREC_DuplicateUnionArg CDN_Name CDN_Name CRA_Region CRA_Region
    | CREC_DuplicatePattern CREC_DuplicatePatternContext CDN_Name CRA_Region CRA_Region
    | CREC_EffectNotFound CRA_Region CDN_Name
    | CREC_EffectFunctionNotFound CRA_Region CDN_Name
    | CREC_ExportDuplicate CDN_Name CRA_Region CRA_Region
    | CREC_ExportNotFound CRA_Region CREC_VarKind CDN_Name (List CDN_Name)
    | CREC_ExportOpenAlias CRA_Region CDN_Name
    | CREC_ImportCtorByName CRA_Region CDN_Name CDN_Name
    | CREC_ImportNotFound CRA_Region CDN_Name (List CEMN_Canonical)
    | CREC_ImportOpenAlias CRA_Region CDN_Name
    | CREC_ImportExposingNotFound CRA_Region CEMN_Canonical CDN_Name (List CDN_Name)
    | CREC_NotFoundVar CRA_Region (Maybe CDN_Name) CDN_Name CREC_PossibleNames
    | CREC_NotFoundType CRA_Region (Maybe CDN_Name) CDN_Name CREC_PossibleNames
    | CREC_NotFoundVariant CRA_Region (Maybe CDN_Name) CDN_Name CREC_PossibleNames
    | CREC_NotFoundBinop CRA_Region CDN_Name (EverySet String CDN_Name)
    | CREC_PatternHasRecordCtor CRA_Region CDN_Name
    | CREC_PortPayloadInvalid CRA_Region CDN_Name CASTC_Type CREC_InvalidPayload
    | CREC_PortTypeInvalid CRA_Region CDN_Name CREC_PortProblem
    | CREC_RecursiveAlias CRA_Region CDN_Name (List CDN_Name) CASTS_Type (List CDN_Name)
    | CREC_RecursiveDecl CRA_Region CDN_Name (List CDN_Name)
    | CREC_RecursiveLet (CRA_Located CDN_Name) (List CDN_Name)
    | CREC_Shadowing CDN_Name CRA_Region CRA_Region
    | CREC_TupleLargerThanThree CRA_Region
    | CREC_TypeVarsUnboundInUnion CRA_Region CDN_Name (List CDN_Name) ( CDN_Name, CRA_Region ) (List ( CDN_Name, CRA_Region ))
    | CREC_TypeVarsMessedUpInAlias CRA_Region CDN_Name (List CDN_Name) (List ( CDN_Name, CRA_Region )) (List ( CDN_Name, CRA_Region ))


{-| FIXME Compiler.Reporting.Error.Canonicalize
-}
type CREC_BadArityContext
    = CREC_TypeArity
    | CREC_PatternArity


{-| FIXME Compiler.Reporting.Error.Canonicalize
-}
type CREC_DuplicatePatternContext
    = CREC_DPLambdaArgs
    | CREC_DPFuncArgs CDN_Name
    | CREC_DPCaseBranch
    | CREC_DPLetBinding
    | CREC_DPDestruct


{-| FIXME Compiler.Reporting.Error.Canonicalize
-}
type CREC_InvalidPayload
    = CREC_ExtendedRecord
    | CREC_Function
    | CREC_TypeVariable CDN_Name
    | CREC_UnsupportedType CDN_Name


{-| FIXME Compiler.Reporting.Error.Canonicalize
-}
type CREC_PortProblem
    = CREC_CmdNoArg
    | CREC_CmdExtraArgs Int
    | CREC_CmdBadMsg
    | CREC_SubBad
    | CREC_NotCmdOrSub


{-| FIXME Compiler.Reporting.Error.Canonicalize
-}
type alias CREC_PossibleNames =
    { locals : EverySet String CDN_Name
    , quals : Map.Dict String CDN_Name (EverySet String CDN_Name)
    }



-- KIND


{-| FIXME Compiler.Reporting.Error.Canonicalize
-}
type CREC_VarKind
    = CREC_BadOp
    | CREC_BadVar
    | CREC_BadPattern
    | CREC_BadType


{-| FIXME Compiler.Reporting.Error.Docs
-}
type CRED_Error
    = CRED_NoDocs CRA_Region
    | CRED_ImplicitExposing CRA_Region
    | CRED_SyntaxProblem CRED_SyntaxProblem
    | CRED_NameProblems (NE.Nonempty CRED_NameProblem)
    | CRED_DefProblems (NE.Nonempty CRED_DefProblem)


{-| FIXME Compiler.Reporting.Error.Docs
-}
type CRED_SyntaxProblem
    = CRED_Op CPP_Row CPP_Col
    | CRED_OpBad CPS_BadOperator CPP_Row CPP_Col
    | CRED_Name CPP_Row CPP_Col
    | CRED_Space CRES_Space CPP_Row CPP_Col
    | CRED_Comma CPP_Row CPP_Col
    | CRED_BadEnd CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Docs
-}
type CRED_NameProblem
    = CRED_NameDuplicate CDN_Name CRA_Region CRA_Region
    | CRED_NameOnlyInDocs CDN_Name CRA_Region
    | CRED_NameOnlyInExports CDN_Name CRA_Region


{-| FIXME Compiler.Reporting.Error.Docs
-}
type CRED_DefProblem
    = CRED_NoComment CDN_Name CRA_Region
    | CRED_NoAnnotation CDN_Name CRA_Region



-- ERROR


{-| FIXME Compiler.Reporting.Error.Import
-}
type CREI_Error
    = CREI_Error CRA_Region CEMN_Raw (EverySet String CEMN_Raw) CREI_Problem


{-| FIXME Compiler.Reporting.Error.Import
-}
type CREI_Problem
    = CREI_NotFound
    | CREI_Ambiguous String (List String) CEP_Name (List CEP_Name)
    | CREI_AmbiguousLocal String String (List String)
    | CREI_AmbiguousForeign CEP_Name CEP_Name (List CEP_Name)



-- ERROR


{-| FIXME Compiler.Reporting.Error.Main
-}
type CREM_Error
    = CREM_BadType CRA_Region CASTC_Type
    | CREM_BadCycle CRA_Region CDN_Name (List CDN_Name)
    | CREM_BadFlags CRA_Region CASTC_Type CREC_InvalidPayload



-- ALL SYNTAX ERRORS


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Error
    = CRES_ModuleNameUnspecified CEMN_Raw
    | CRES_ModuleNameMismatch CEMN_Raw (CRA_Located CEMN_Raw)
    | CRES_UnexpectedPort CRA_Region
    | CRES_NoPorts CRA_Region
    | CRES_NoPortsInPackage (CRA_Located CDN_Name)
    | CRES_NoPortModulesInPackage CRA_Region
    | CRES_NoEffectsOutsideKernel CRA_Region
    | CRES_ParseError CRES_Module


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Module
    = CRES_ModuleSpace CRES_Space CPP_Row CPP_Col
    | CRES_ModuleBadEnd CPP_Row CPP_Col
      --
    | CRES_ModuleProblem CPP_Row CPP_Col
    | CRES_ModuleName CPP_Row CPP_Col
    | CRES_ModuleExposing CRES_Exposing CPP_Row CPP_Col
      --
    | CRES_PortModuleProblem CPP_Row CPP_Col
    | CRES_PortModuleName CPP_Row CPP_Col
    | CRES_PortModuleExposing CRES_Exposing CPP_Row CPP_Col
      --
    | CRES_Effect CPP_Row CPP_Col
      --
    | CRES_FreshLine CPP_Row CPP_Col
      --
    | CRES_ImportStart CPP_Row CPP_Col
    | CRES_ImportName CPP_Row CPP_Col
    | CRES_ImportAs CPP_Row CPP_Col
    | CRES_ImportAlias CPP_Row CPP_Col
    | CRES_ImportExposing CPP_Row CPP_Col
    | CRES_ImportExposingList CRES_Exposing CPP_Row CPP_Col
    | CRES_ImportEnd CPP_Row CPP_Col -- different based on col=1 or if greater
      --
    | CRES_ImportIndentName CPP_Row CPP_Col
    | CRES_ImportIndentAlias CPP_Row CPP_Col
    | CRES_ImportIndentExposingList CPP_Row CPP_Col
      --
    | CRES_Infix CPP_Row CPP_Col
      --
    | CRES_Declarations CRES_Decl CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Exposing
    = CRES_ExposingSpace CRES_Space CPP_Row CPP_Col
    | CRES_ExposingStart CPP_Row CPP_Col
    | CRES_ExposingValue CPP_Row CPP_Col
    | CRES_ExposingOperator CPP_Row CPP_Col
    | CRES_ExposingOperatorReserved CPS_BadOperator CPP_Row CPP_Col
    | CRES_ExposingOperatorRightParen CPP_Row CPP_Col
    | CRES_ExposingTypePrivacy CPP_Row CPP_Col
    | CRES_ExposingEnd CPP_Row CPP_Col
      --
    | CRES_ExposingIndentEnd CPP_Row CPP_Col
    | CRES_ExposingIndentValue CPP_Row CPP_Col



-- DECLARATIONS


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Decl
    = CRES_DeclStart CPP_Row CPP_Col
    | CRES_DeclSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_Port CRES_Port CPP_Row CPP_Col
    | CRES_DeclType CRES_DeclType CPP_Row CPP_Col
    | CRES_DeclDef CDN_Name CRES_DeclDef CPP_Row CPP_Col
      --
    | CRES_DeclFreshLineAfterDocComment CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_DeclDef
    = CRES_DeclDefSpace CRES_Space CPP_Row CPP_Col
    | CRES_DeclDefEquals CPP_Row CPP_Col
    | CRES_DeclDefType CRES_Type CPP_Row CPP_Col
    | CRES_DeclDefArg CRES_Pattern CPP_Row CPP_Col
    | CRES_DeclDefBody CRES_Expr CPP_Row CPP_Col
    | CRES_DeclDefNameRepeat CPP_Row CPP_Col
    | CRES_DeclDefNameMatch CDN_Name CPP_Row CPP_Col
      --
    | CRES_DeclDefIndentType CPP_Row CPP_Col
    | CRES_DeclDefIndentEquals CPP_Row CPP_Col
    | CRES_DeclDefIndentBody CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Port
    = CRES_PortSpace CRES_Space CPP_Row CPP_Col
    | CRES_PortName CPP_Row CPP_Col
    | CRES_PortColon CPP_Row CPP_Col
    | CRES_PortType CRES_Type CPP_Row CPP_Col
    | CRES_PortIndentName CPP_Row CPP_Col
    | CRES_PortIndentColon CPP_Row CPP_Col
    | CRES_PortIndentType CPP_Row CPP_Col



-- TYPE DECLARATIONS


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_DeclType
    = CRES_DT_Space CRES_Space CPP_Row CPP_Col
    | CRES_DT_Name CPP_Row CPP_Col
    | CRES_DT_Alias CRES_TypeAlias CPP_Row CPP_Col
    | CRES_DT_Union CRES_CustomType CPP_Row CPP_Col
      --
    | CRES_DT_IndentName CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_TypeAlias
    = CRES_AliasSpace CRES_Space CPP_Row CPP_Col
    | CRES_AliasName CPP_Row CPP_Col
    | CRES_AliasEquals CPP_Row CPP_Col
    | CRES_AliasBody CRES_Type CPP_Row CPP_Col
      --
    | CRES_AliasIndentEquals CPP_Row CPP_Col
    | CRES_AliasIndentBody CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_CustomType
    = CRES_CT_Space CRES_Space CPP_Row CPP_Col
    | CRES_CT_Name CPP_Row CPP_Col
    | CRES_CT_Equals CPP_Row CPP_Col
    | CRES_CT_Bar CPP_Row CPP_Col
    | CRES_CT_Variant CPP_Row CPP_Col
    | CRES_CT_VariantArg CRES_Type CPP_Row CPP_Col
      --
    | CRES_CT_IndentEquals CPP_Row CPP_Col
    | CRES_CT_IndentBar CPP_Row CPP_Col
    | CRES_CT_IndentAfterBar CPP_Row CPP_Col
    | CRES_CT_IndentAfterEquals CPP_Row CPP_Col



-- EXPRESSIONS


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Expr
    = CRES_Let CRES_Let CPP_Row CPP_Col
    | CRES_Case CRES_Case CPP_Row CPP_Col
    | CRES_If CRES_If CPP_Row CPP_Col
    | CRES_List CRES_List_ CPP_Row CPP_Col
    | CRES_Record CRES_Record CPP_Row CPP_Col
    | CRES_Tuple CRES_Tuple CPP_Row CPP_Col
    | CRES_Func CRES_Func CPP_Row CPP_Col
      --
    | CRES_Dot CPP_Row CPP_Col
    | CRES_Access CPP_Row CPP_Col
    | CRES_OperatorRight CDN_Name CPP_Row CPP_Col
    | CRES_OperatorReserved CPS_BadOperator CPP_Row CPP_Col
      --
    | CRES_Start CPP_Row CPP_Col
    | CRES_Char CRES_Char CPP_Row CPP_Col
    | CRES_String_ CRES_String_ CPP_Row CPP_Col
    | CRES_Number CRES_Number CPP_Row CPP_Col
    | CRES_Space CRES_Space CPP_Row CPP_Col
    | CRES_EndlessShader CPP_Row CPP_Col
    | CRES_ShaderProblem String CPP_Row CPP_Col
    | CRES_IndentOperatorRight CDN_Name CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Record
    = CRES_RecordOpen CPP_Row CPP_Col
    | CRES_RecordEnd CPP_Row CPP_Col
    | CRES_RecordField CPP_Row CPP_Col
    | CRES_RecordEquals CPP_Row CPP_Col
    | CRES_RecordExpr CRES_Expr CPP_Row CPP_Col
    | CRES_RecordSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_RecordIndentOpen CPP_Row CPP_Col
    | CRES_RecordIndentEnd CPP_Row CPP_Col
    | CRES_RecordIndentField CPP_Row CPP_Col
    | CRES_RecordIndentEquals CPP_Row CPP_Col
    | CRES_RecordIndentExpr CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Tuple
    = CRES_TupleExpr CRES_Expr CPP_Row CPP_Col
    | CRES_TupleSpace CRES_Space CPP_Row CPP_Col
    | CRES_TupleEnd CPP_Row CPP_Col
    | CRES_TupleOperatorClose CPP_Row CPP_Col
    | CRES_TupleOperatorReserved CPS_BadOperator CPP_Row CPP_Col
      --
    | CRES_TupleIndentExpr1 CPP_Row CPP_Col
    | CRES_TupleIndentExprN CPP_Row CPP_Col
    | CRES_TupleIndentEnd CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_List_
    = CRES_ListSpace CRES_Space CPP_Row CPP_Col
    | CRES_ListOpen CPP_Row CPP_Col
    | CRES_ListExpr CRES_Expr CPP_Row CPP_Col
    | CRES_ListEnd CPP_Row CPP_Col
      --
    | CRES_ListIndentOpen CPP_Row CPP_Col
    | CRES_ListIndentEnd CPP_Row CPP_Col
    | CRES_ListIndentExpr CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Func
    = CRES_FuncSpace CRES_Space CPP_Row CPP_Col
    | CRES_FuncArg CRES_Pattern CPP_Row CPP_Col
    | CRES_FuncBody CRES_Expr CPP_Row CPP_Col
    | CRES_FuncArrow CPP_Row CPP_Col
      --
    | CRES_FuncIndentArg CPP_Row CPP_Col
    | CRES_FuncIndentArrow CPP_Row CPP_Col
    | CRES_FuncIndentBody CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Case
    = CRES_CaseSpace CRES_Space CPP_Row CPP_Col
    | CRES_CaseOf CPP_Row CPP_Col
    | CRES_CasePattern CRES_Pattern CPP_Row CPP_Col
    | CRES_CaseArrow CPP_Row CPP_Col
    | CRES_CaseExpr CRES_Expr CPP_Row CPP_Col
    | CRES_CaseBranch CRES_Expr CPP_Row CPP_Col
      --
    | CRES_CaseIndentOf CPP_Row CPP_Col
    | CRES_CaseIndentExpr CPP_Row CPP_Col
    | CRES_CaseIndentPattern CPP_Row CPP_Col
    | CRES_CaseIndentArrow CPP_Row CPP_Col
    | CRES_CaseIndentBranch CPP_Row CPP_Col
    | CRES_CasePatternAlignment Int CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_If
    = CRES_IfSpace CRES_Space CPP_Row CPP_Col
    | CRES_IfThen CPP_Row CPP_Col
    | CRES_IfElse CPP_Row CPP_Col
    | CRES_IfElseBranchStart CPP_Row CPP_Col
      --
    | CRES_IfCondition CRES_Expr CPP_Row CPP_Col
    | CRES_IfThenBranch CRES_Expr CPP_Row CPP_Col
    | CRES_IfElseBranch CRES_Expr CPP_Row CPP_Col
      --
    | CRES_IfIndentCondition CPP_Row CPP_Col
    | CRES_IfIndentThen CPP_Row CPP_Col
    | CRES_IfIndentThenBranch CPP_Row CPP_Col
    | CRES_IfIndentElseBranch CPP_Row CPP_Col
    | CRES_IfIndentElse CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Let
    = CRES_LetSpace CRES_Space CPP_Row CPP_Col
    | CRES_LetIn CPP_Row CPP_Col
    | CRES_LetDefAlignment Int CPP_Row CPP_Col
    | CRES_LetDefName CPP_Row CPP_Col
    | CRES_LetDef CDN_Name CRES_Def CPP_Row CPP_Col
    | CRES_LetDestruct CRES_Destruct CPP_Row CPP_Col
    | CRES_LetBody CRES_Expr CPP_Row CPP_Col
    | CRES_LetIndentDef CPP_Row CPP_Col
    | CRES_LetIndentIn CPP_Row CPP_Col
    | CRES_LetIndentBody CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Def
    = CRES_DefSpace CRES_Space CPP_Row CPP_Col
    | CRES_DefType CRES_Type CPP_Row CPP_Col
    | CRES_DefNameRepeat CPP_Row CPP_Col
    | CRES_DefNameMatch CDN_Name CPP_Row CPP_Col
    | CRES_DefArg CRES_Pattern CPP_Row CPP_Col
    | CRES_DefEquals CPP_Row CPP_Col
    | CRES_DefBody CRES_Expr CPP_Row CPP_Col
    | CRES_DefIndentEquals CPP_Row CPP_Col
    | CRES_DefIndentType CPP_Row CPP_Col
    | CRES_DefIndentBody CPP_Row CPP_Col
    | CRES_DefAlignment Int CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Destruct
    = CRES_DestructSpace CRES_Space CPP_Row CPP_Col
    | CRES_DestructPattern CRES_Pattern CPP_Row CPP_Col
    | CRES_DestructEquals CPP_Row CPP_Col
    | CRES_DestructBody CRES_Expr CPP_Row CPP_Col
    | CRES_DestructIndentEquals CPP_Row CPP_Col
    | CRES_DestructIndentBody CPP_Row CPP_Col



-- PATTERNS


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Pattern
    = CRES_PRecord CRES_PRecord CPP_Row CPP_Col
    | CRES_PTuple CRES_PTuple CPP_Row CPP_Col
    | CRES_PList CRES_PList CPP_Row CPP_Col
      --
    | CRES_PStart CPP_Row CPP_Col
    | CRES_PChar CRES_Char CPP_Row CPP_Col
    | CRES_PString CRES_String_ CPP_Row CPP_Col
    | CRES_PNumber CRES_Number CPP_Row CPP_Col
    | CRES_PFloat Int CPP_Row CPP_Col
    | CRES_PAlias CPP_Row CPP_Col
    | CRES_PWildcardNotVar CDN_Name Int CPP_Row CPP_Col
    | CRES_PSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_PIndentStart CPP_Row CPP_Col
    | CRES_PIndentAlias CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_PRecord
    = CRES_PRecordOpen CPP_Row CPP_Col
    | CRES_PRecordEnd CPP_Row CPP_Col
    | CRES_PRecordField CPP_Row CPP_Col
    | CRES_PRecordSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_PRecordIndentOpen CPP_Row CPP_Col
    | CRES_PRecordIndentEnd CPP_Row CPP_Col
    | CRES_PRecordIndentField CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_PTuple
    = CRES_PTupleOpen CPP_Row CPP_Col
    | CRES_PTupleEnd CPP_Row CPP_Col
    | CRES_PTupleExpr CRES_Pattern CPP_Row CPP_Col
    | CRES_PTupleSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_PTupleIndentEnd CPP_Row CPP_Col
    | CRES_PTupleIndentExpr1 CPP_Row CPP_Col
    | CRES_PTupleIndentExprN CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_PList
    = CRES_PListOpen CPP_Row CPP_Col
    | CRES_PListEnd CPP_Row CPP_Col
    | CRES_PListExpr CRES_Pattern CPP_Row CPP_Col
    | CRES_PListSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_PListIndentOpen CPP_Row CPP_Col
    | CRES_PListIndentEnd CPP_Row CPP_Col
    | CRES_PListIndentExpr CPP_Row CPP_Col



-- TYPES


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Type
    = CRES_TRecord CRES_TRecord CPP_Row CPP_Col
    | CRES_TTuple CRES_TTuple CPP_Row CPP_Col
      --
    | CRES_TStart CPP_Row CPP_Col
    | CRES_TSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_TIndentStart CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_TRecord
    = CRES_TRecordOpen CPP_Row CPP_Col
    | CRES_TRecordEnd CPP_Row CPP_Col
      --
    | CRES_TRecordField CPP_Row CPP_Col
    | CRES_TRecordColon CPP_Row CPP_Col
    | CRES_TRecordType CRES_Type CPP_Row CPP_Col
      --
    | CRES_TRecordSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_TRecordIndentOpen CPP_Row CPP_Col
    | CRES_TRecordIndentField CPP_Row CPP_Col
    | CRES_TRecordIndentColon CPP_Row CPP_Col
    | CRES_TRecordIndentType CPP_Row CPP_Col
    | CRES_TRecordIndentEnd CPP_Row CPP_Col


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_TTuple
    = CRES_TTupleOpen CPP_Row CPP_Col
    | CRES_TTupleEnd CPP_Row CPP_Col
    | CRES_TTupleType CRES_Type CPP_Row CPP_Col
    | CRES_TTupleSpace CRES_Space CPP_Row CPP_Col
      --
    | CRES_TTupleIndentType1 CPP_Row CPP_Col
    | CRES_TTupleIndentTypeN CPP_Row CPP_Col
    | CRES_TTupleIndentEnd CPP_Row CPP_Col



-- LITERALS


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Char
    = CRES_CharEndless
    | CRES_CharEscape CRES_Escape
    | CRES_CharNotString Int


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_String_
    = CRES_StringEndless_Single
    | CRES_StringEndless_Multi
    | CRES_StringEscape CRES_Escape


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Escape
    = CRES_EscapeUnknown
    | CRES_BadUnicodeFormat Int
    | CRES_BadUnicodeCode Int
    | CRES_BadUnicodeLength Int Int Int


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Number
    = CRES_NumberEnd
    | CRES_NumberDot Int
    | CRES_NumberHexDigit
    | CRES_NumberNoLeadingZero



-- MISC


{-| FIXME Compiler.Reporting.Error.Syntax
-}
type CRES_Space
    = CRES_HasTab
    | CRES_EndlessMultiComment



-- ERRORS


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_Error
    = CRET_BadExpr CRA_Region CRET_Category CTE_Type (CRET_Expected CTE_Type)
    | CRET_BadPattern CRA_Region CRET_PCategory CTE_Type (CRET_PExpected CTE_Type)
    | CRET_InfiniteType CRA_Region CDN_Name CTE_Type



-- EXPRESSION EXPECTATIONS


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_Expected tipe
    = CRET_NoExpectation tipe
    | CRET_FromContext CRA_Region CRET_Context tipe
    | CRET_FromAnnotation CDN_Name Int CRET_SubContext tipe


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_Context
    = CRET_ListEntry CDI_ZeroBased
    | CRET_Negate
    | CRET_OpLeft CDN_Name
    | CRET_OpRight CDN_Name
    | CRET_IfCondition
    | CRET_IfBranch CDI_ZeroBased
    | CRET_CaseBranch CDI_ZeroBased
    | CRET_CallArity CRET_MaybeName Int
    | CRET_CallArg CRET_MaybeName CDI_ZeroBased
    | CRET_RecordAccess CRA_Region (Maybe CDN_Name) CRA_Region CDN_Name
    | CRET_RecordUpdateKeys CDN_Name (Map.Dict String CDN_Name CASTC_FieldUpdate)
    | CRET_RecordUpdateValue CDN_Name
    | CRET_Destructure


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_SubContext
    = CRET_TypedIfBranch CDI_ZeroBased
    | CRET_TypedCaseBranch CDI_ZeroBased
    | CRET_TypedBody


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_MaybeName
    = CRET_FuncName CDN_Name
    | CRET_CtorName CDN_Name
    | CRET_OpName CDN_Name
    | CRET_NoName


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_Category
    = CRET_List
    | CRET_Number
    | CRET_Float
    | CRET_String
    | CRET_Char
    | CRET_If
    | CRET_Case
    | CRET_CallResult CRET_MaybeName
    | CRET_Lambda
    | CRET_Accessor CDN_Name
    | CRET_Access CDN_Name
    | CRET_Record
    | CRET_Tuple
    | CRET_Unit
    | CRET_Shader
    | CRET_Effects
    | CRET_Local CDN_Name
    | CRET_Foreign CDN_Name



-- PATTERN EXPECTATIONS


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_PExpected tipe
    = CRET_PNoExpectation tipe
    | CRET_PFromContext CRA_Region CRET_PContext tipe


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_PContext
    = CRET_PTypedArg CDN_Name CDI_ZeroBased
    | CRET_PCaseMatch CDI_ZeroBased
    | CRET_PCtorArg CDN_Name CDI_ZeroBased
    | CRET_PListEntry CDI_ZeroBased
    | CRET_PTail


{-| FIXME Compiler.Reporting.Error.Type
-}
type CRET_PCategory
    = CRET_PRecord
    | CRET_PUnit
    | CRET_PTuple
    | CRET_PList
    | CRET_PCtor CDN_Name
    | CRET_PInt
    | CRET_PStr
    | CRET_PChr
    | CRET_PBool



-- LOCALIZER


{-| FIXME Compiler.Reporting.Render.Type.Localizer
-}
type CRRTL_Localizer
    = CRRTL_Localizer (Map.Dict String CDN_Name CRRTL_Import)


{-| FIXME Compiler.Reporting.Render.Type.Localizer
-}
type alias CRRTL_Import =
    { alias : Maybe CDN_Name
    , exposing_ : CRRTL_Exposing
    }


{-| FIXME Compiler.Reporting.Render.Type.Localizer
-}
type CRRTL_Exposing
    = CRRTL_All
    | CRRTL_Only (EverySet String CDN_Name)



-- ERROR TYPES


{-| FIXME Compiler.Type.Error
-}
type CTE_Type
    = CTE_Lambda CTE_Type CTE_Type (List CTE_Type)
    | CTE_Infinite
    | CTE_Error
    | CTE_FlexVar CDN_Name
    | CTE_FlexSuper CTE_Super CDN_Name
    | CTE_RigidVar CDN_Name
    | CTE_RigidSuper CTE_Super CDN_Name
    | CTE_Type CEMN_Canonical CDN_Name (List CTE_Type)
    | CTE_Record (Map.Dict String CDN_Name CTE_Type) CTE_Extension
    | CTE_Unit
    | CTE_Tuple CTE_Type CTE_Type (Maybe CTE_Type)
    | CTE_Alias CEMN_Canonical CDN_Name (List ( CDN_Name, CTE_Type )) CTE_Type


{-| FIXME Compiler.Type.Error
-}
type CTE_Super
    = CTE_Number
    | CTE_Comparable
    | CTE_Appendable
    | CTE_CompAppend


{-| FIXME Compiler.Type.Error
-}
type CTE_Extension
    = CTE_Closed
    | CTE_FlexOpen CDN_Name
    | CTE_RigidOpen CDN_Name



-- EXPRESSIONS


{-| FIXME Compiler.AST.Canonical
-}
type alias CASTC_Expr =
    CRA_Located CASTC_Expr_



-- CACHE Annotations for type inference


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Expr_
    = CASTC_VarLocal CDN_Name
    | CASTC_VarTopLevel CEMN_Canonical CDN_Name
    | CASTC_VarKernel CDN_Name CDN_Name
    | CASTC_VarForeign CEMN_Canonical CDN_Name CASTC_Annotation
    | CASTC_VarCtor CASTC_CtorOpts CEMN_Canonical CDN_Name CDI_ZeroBased CASTC_Annotation
    | CASTC_VarDebug CEMN_Canonical CDN_Name CASTC_Annotation
    | CASTC_VarOperator CDN_Name CEMN_Canonical CDN_Name CASTC_Annotation -- CACHE real name for optimization
    | CASTC_Chr String
    | CASTC_Str String
    | CASTC_Int Int
    | CASTC_Float Float
    | CASTC_List (List CASTC_Expr)
    | CASTC_Negate CASTC_Expr
    | CASTC_Binop CDN_Name CEMN_Canonical CDN_Name CASTC_Annotation CASTC_Expr CASTC_Expr -- CACHE real name for optimization
    | CASTC_Lambda (List CASTC_Pattern) CASTC_Expr
    | CASTC_Call CASTC_Expr (List CASTC_Expr)
    | CASTC_If (List ( CASTC_Expr, CASTC_Expr )) CASTC_Expr
    | CASTC_Let CASTC_Def CASTC_Expr
    | CASTC_LetRec (List CASTC_Def) CASTC_Expr
    | CASTC_LetDestruct CASTC_Pattern CASTC_Expr CASTC_Expr
    | CASTC_Case CASTC_Expr (List CASTC_CaseBranch)
    | CASTC_Accessor CDN_Name
    | CASTC_Access CASTC_Expr (CRA_Located CDN_Name)
    | CASTC_Update CDN_Name CASTC_Expr (Map.Dict String CDN_Name CASTC_FieldUpdate)
    | CASTC_Record (Map.Dict String CDN_Name CASTC_Expr)
    | CASTC_Unit
    | CASTC_Tuple CASTC_Expr CASTC_Expr (Maybe CASTC_Expr)
    | CASTC_Shader CASTUS_Source CASTUS_Types


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_CaseBranch
    = CASTC_CaseBranch CASTC_Pattern CASTC_Expr


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_FieldUpdate
    = CASTC_FieldUpdate CRA_Region CASTC_Expr



-- DEFS


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Def
    = CASTC_Def (CRA_Located CDN_Name) (List CASTC_Pattern) CASTC_Expr
    | CASTC_TypedDef (CRA_Located CDN_Name) CASTC_FreeVars (List ( CASTC_Pattern, CASTC_Type )) CASTC_Expr CASTC_Type


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Decls
    = CASTC_Declare CASTC_Def CASTC_Decls
    | CASTC_DeclareRec CASTC_Def (List CASTC_Def) CASTC_Decls
    | CASTC_SaveTheEnvironment



-- PATTERNS


{-| FIXME Compiler.AST.Canonical
-}
type alias CASTC_Pattern =
    CRA_Located CASTC_Pattern_


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Pattern_
    = CASTC_PAnything
    | CASTC_PVar CDN_Name
    | CASTC_PRecord (List CDN_Name)
    | CASTC_PAlias CASTC_Pattern CDN_Name
    | CASTC_PUnit
    | CASTC_PTuple CASTC_Pattern CASTC_Pattern (Maybe CASTC_Pattern)
    | CASTC_PList (List CASTC_Pattern)
    | CASTC_PCons CASTC_Pattern CASTC_Pattern
    | CASTC_PBool CASTC_Union Bool
    | CASTC_PChr String
    | CASTC_PStr String
    | CASTC_PInt Int
    | CASTC_PCtor
        -- CACHE p_home, p_type, and p_vars for type inference
        -- CACHE p_index to replace p_name in PROD code gen
        -- CACHE p_opts to allocate less in PROD code gen
        -- CACHE p_alts and p_numAlts for exhaustiveness checker
        { home : CEMN_Canonical
        , type_ : CDN_Name
        , union : CASTC_Union
        , name : CDN_Name
        , index : CDI_ZeroBased
        , args : List CASTC_PatternCtorArg
        }


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_PatternCtorArg
    = CASTC_PatternCtorArg
        -- CACHE for destructors/errors
        CDI_ZeroBased
        -- CACHE for type inference
        CASTC_Type
        CASTC_Pattern



-- MANAGER


{-| FIXME Builder.Http
-}
type BH_Manager
    = BH_Manager



-- EXCEPTIONS


{-| FIXME Builder.Http
-}
type BH_Error
    = BH_BadUrl String String
    | BH_BadHttp String UM_HttpExceptionContent
    | BH_BadMystery String UM_SomeException



-- PACKAGE CACHES


{-| FIXME Builder.Stuff
-}
type BS_PackageCache
    = BS_PackageCache String



-- REGISTRY


{-| FIXME Builder.Deps.Registry
-}
type BDR_Registry
    = BDR_Registry Int (Map.Dict ( String, String ) CEP_Name BDR_KnownVersions)


{-| FIXME Builder.Deps.Registry
-}
type BDR_KnownVersions
    = BDR_KnownVersions CEV_Version (List CEV_Version)



-- SOLVER


{-| FIXME Builder.Deps.Solver
-}
type BDS_Connection
    = BDS_Online BH_Manager
    | BDS_Offline



-- ENVIRONMENT


{-| FIXME Builder.Deps.Solver
-}
type BDS_Env
    = BDS_Env BS_PackageCache BH_Manager BDS_Connection BDR_Registry



-- REGISTRY PROBLEM


{-| FIXME Builder.Reporting.Exit
-}
type BRE_RegistryProblem
    = BRE_RP_Http BH_Error
    | BRE_RP_Data String String



-- DEPENDENCY INTERFACE


{-| FIXME Compiler.Elm.Interface
-}
type CEI_DependencyInterface
    = CEI_Public CEI_Interface
    | CEI_Private CEP_Name (Map.Dict String CDN_Name CASTC_Union) (Map.Dict String CDN_Name CASTC_Alias)



-- VERSION


{-| FIXME Compiler.Elm.Version
-}
type CEV_Version
    = CEV_Version Int Int Int



-- Network.HTTP.Client


{-| FIXME Utils.Main
-}
type UM_HttpExceptionContent
    = UM_StatusCodeException (UM_HttpResponse ()) String
    | UM_TooManyRedirects (List (UM_HttpResponse ()))
    | UM_ConnectionFailure UM_SomeException


{-| FIXME Utils.Main
-}
type UM_HttpResponse body
    = UM_HttpResponse
        { responseStatus : UM_HttpStatus
        , responseHeaders : UM_HttpResponseHeaders
        }


{-| FIXME Utils.Main
-}
type alias UM_HttpResponseHeaders =
    List ( String, String )


{-| FIXME Utils.Main
-}
type UM_HttpStatus
    = UM_HttpStatus Int String



-- Control.Exception


{-| FIXME Utils.Main
-}
type UM_SomeException
    = UM_SomeException



-- COMPILE


{-| FIXME Builder.Elm.Details
-}
type BED_DResult
    = BED_RLocal CEI_Interface CASTO_LocalGraph (Maybe CED_Module)
    | BED_RForeign CEI_Interface
    | BED_RKernelLocal (List CEK_Chunk)
    | BED_RKernelForeign



-- VERIFY DEPENDENCY


{-| FIXME Builder.Elm.Details
-}
type CED_Artifacts
    = CED_Artifacts (Map.Dict String CEMN_Raw CEI_DependencyInterface) CASTO_GlobalGraph


{-| FIXME Builder.Elm.Details
-}
type alias CED_Dep =
    Result (Maybe BRE_DetailsBadDep) CED_Artifacts



-- DETAILS


{-| FIXME Builder.Reporting.Exit
-}
type BRE_DetailsBadDep
    = BRE_BD_BadDownload CEP_Name CEV_Version BRE_PackageProblem
    | BRE_BD_BadBuild CEP_Name CEV_Version (Map.Dict ( String, String ) CEP_Name CEV_Version)



-- PACKAGE PROBLEM


{-| FIXME Builder.Reporting.Exit
-}
type BRE_PackageProblem
    = BRE_PP_BadEndpointRequest BH_Error
    | BRE_PP_BadEndpointContent String
    | BRE_PP_BadArchiveRequest BH_Error
    | BRE_PP_BadArchiveContent String
    | BRE_PP_BadArchiveHash String String String



-- FROM PATHS


{-| FIXME Builder.Build
-}
type alias BB_Dependencies =
    Map.Dict (List String) CEMN_Canonical CEI_DependencyInterface



-- TRANSITIVELY AVAILABLE TYPES


{-| FIXME Compiler.Elm.Compiler.Type.Extract
-}
type CECTE_Types
    = -- PERF profile Opt.Global representation
      -- current representation needs less allocation
      -- but maybe the lookup is much worse
      CECTE_Types (Map.Dict (List String) CEMN_Canonical CECTE_Types_)


{-| FIXME Compiler.Elm.Compiler.Type.Extract
-}
type CECTE_Types_
    = CECTE_Types_ (Map.Dict String CDN_Name CASTC_Union) (Map.Dict String CDN_Name CASTC_Alias)



-- BACKGROUND WRITER


{-| FIXME Builder.BackgroundWriter
-}
type BBW_Scope
    = BBW_Scope MVar_ListMVar



-- FROM PATHS


{-| FIXME Builder.Build
-}
type BB_Artifacts
    = BB_Artifacts CEP_Name BB_Dependencies (NE.Nonempty BB_Root) (List BB_Module)


{-| FIXME Builder.Build
-}
type BB_Module
    = BB_Fresh CEMN_Raw CEI_Interface CASTO_LocalGraph
    | BB_Cached CEMN_Raw Bool MVar_BB_CachedInterface



-- TO ARTIFACTS


{-| FIXME Builder.Build
-}
type BB_Root
    = BB_Inside CEMN_Raw
    | BB_Outside CEMN_Raw CEI_Interface CASTO_LocalGraph



-- KEY


{-| FIXME Builder.Reporting
-}
type BR_Key msg
    = BR_Key (msg -> IO ())



-- BUILD


{-| FIXME Builder.Reporting
-}
type alias BR_BKey =
    BR_Key BR_BMsg


{-| FIXME Builder.Reporting
-}
type BR_BMsg
    = BR_BDone


{-| FIXME Builder.Reporting
-}
type alias BR_BResult a =
    Result BRE_BuildProblem a



-- BUILD PROBLEM


{-| FIXME Builder.Reporting.Exit
-}
type BRE_BuildProblem
    = BRE_BuildBadModules FilePath CRE_Module (List CRE_Module)
    | BRE_BuildProjectProblem BRE_BuildProjectProblem


{-| FIXME Builder.Reporting.Exit
-}
type BRE_BuildProjectProblem
    = BRE_BP_PathUnknown FilePath
    | BRE_BP_WithBadExtension FilePath
    | BRE_BP_WithAmbiguousSrcDir FilePath FilePath FilePath
    | BRE_BP_MainPathDuplicate FilePath FilePath
    | BRE_BP_RootNameDuplicate CEMN_Raw FilePath FilePath
    | BRE_BP_RootNameInvalid FilePath FilePath (List String)
    | BRE_BP_CannotLoadDependencies
    | BRE_BP_Cycle CEMN_Raw (List CEMN_Raw)
    | BRE_BP_MissingExposed (NE.Nonempty ( CEMN_Raw, CREI_Problem ))


{-| FIXME Codec.Archive.Zip
-}
type alias CAZ_Archive =
    List CAZ_Entry


{-| FIXME Codec.Archive.Zip
-}
type alias CAZ_Entry =
    { eRelativePath : FilePath
    , eData : String
    }



-- CRAWL ROOTS


{-| FIXME Builder.Build
-}
type BB_RootStatus
    = BB_SInside CEMN_Raw
    | BB_SOutsideOk BED_Local String CASTS_Module
    | BB_SOutsideErr CRE_Module



-- CHECK ROOTS


{-| FIXME Builder.Build
-}
type BB_RootResult
    = BB_RInside CEMN_Raw
    | BB_ROutsideOk CEMN_Raw CEI_Interface CASTO_LocalGraph
    | BB_ROutsideErr CRE_Module
    | BB_ROutsideBlocked



-- CHECK DEPS


{-| FIXME Builder.Build
-}
type alias BB_Dep =
    ( CEMN_Raw, CEI_Interface )



-- ROOT INFO


{-| FIXME Builder.Build
-}
type BB_RootInfo
    = BB_RootInfo FilePath FilePath BB_RootLocation



-- FIND ROOT


{-| FIXME Builder.Build
-}
type BB_RootLocation
    = BB_LInside CEMN_Raw
    | BB_LOutside FilePath


{-| FIXME Builder.Reporting
-}
type BR_DMsg
    = BR_DStart Int
    | BR_DCached
    | BR_DRequested
    | BR_DReceived CEP_Name CEV_Version
    | BR_DFailed CEP_Name CEV_Version
    | BR_DBuilt
    | BR_DBroken
