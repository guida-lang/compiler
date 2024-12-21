module Types exposing (..)

{-| -}

import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore exposing (OneOrMore)
import Data.Map exposing (Dict)
import Data.Set exposing (EverySet)
import Time



-- GHC.IO


type alias FilePath =
    String



-- CRAWL


{-| FIXME Builder.Elm.Details
-}
type alias BED_StatusDict =
    Dict String CEMN_Raw MVar_Maybe_BED_Status


{-| FIXME Builder.Elm.Details
-}
type BED_Status
    = BED_SLocal BED_DocsStatus (Dict String CEMN_Raw ()) CASTS_Module
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
    Dict String CDN_Name ()


{-| FIXME Compiler.AST.Canonical
-}
type CASTC_Type
    = CASTC_TLambda CASTC_Type CASTC_Type
    | CASTC_TVar CDN_Name
    | CASTC_TType CEMN_Canonical CDN_Name (List CASTC_Type)
    | CASTC_TRecord (Dict String CDN_Name CASTC_FieldType) (Maybe CDN_Name)
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
    = CEI_Interface CEP_Name (Dict String CDN_Name CASTC_Annotation) (Dict String CDN_Name CEI_Union) (Dict String CDN_Name CEI_Alias) (Dict String CDN_Name CEI_Binop)


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
    = CASTUS_Types (Dict String CDN_Name CASTUS_Type) (Dict String CDN_Name CASTUS_Type) (Dict String CDN_Name CASTUS_Type)


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
    | CASTO_Update CASTO_Expr (Dict String CDN_Name CASTO_Expr)
    | CASTO_Record (Dict String CDN_Name CASTO_Expr)
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
    = CASTO_GlobalGraph (Dict (List String) CASTO_Global CASTO_Node) (Dict String CDN_Name Int)


{-| FIXME Compiler.AST.Optimized
-}
type CASTO_LocalGraph
    = CASTO_LocalGraph
        (Maybe CASTO_Main)
        -- PERF profile switching Global to Name
        (Dict (List String) CASTO_Global CASTO_Node)
        (Dict String CDN_Name Int)


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
    Dict String CEMN_Raw MVar_BB_BResult


{-| FIXME Builder.Build
-}
type BB_BResult
    = BB_RNew BED_Local CEI_Interface CASTO_LocalGraph (Maybe CED_Module)
    | BB_RSame BED_Local CEI_Interface CASTO_LocalGraph (Maybe CED_Module)
    | BB_RCached Bool BED_BuildID (MVar BB_CachedInterface)
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
    Dict String CEMN_Raw MVar_BB_Status


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
type CED_Module
    = CED_Module CDN_Name CED_Comment (Dict String CDN_Name CED_Union) (Dict String CDN_Name CED_Alias) (Dict String CDN_Name CED_Value) (Dict String CDN_Name CED_Binop)


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
    , quals : Dict String CDN_Name (EverySet String CDN_Name)
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
    | CRET_RecordUpdateKeys CDN_Name (Dict String CDN_Name CASTC_FieldUpdate)
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
    = CRRTL_Localizer (Dict String CDN_Name CRRTL_Import)


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
    | CTE_Record (Dict String CDN_Name CTE_Type) CTE_Extension
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
    | CASTC_Update CDN_Name CASTC_Expr (Dict String CDN_Name CASTC_FieldUpdate)
    | CASTC_Record (Dict String CDN_Name CASTC_Expr)
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
    = BDR_Registry Int (Dict ( String, String ) CEP_Name BDR_KnownVersions)


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
    | CEI_Private CEP_Name (Dict String CDN_Name CASTC_Union) (Dict String CDN_Name CASTC_Alias)



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
    = CED_Artifacts (Dict String CEMN_Raw CEI_DependencyInterface) CASTO_GlobalGraph


{-| FIXME Builder.Elm.Details
-}
type alias CED_Dep =
    Result (Maybe BRE_DetailsBadDep) CED_Artifacts



-- DETAILS


{-| FIXME Builder.Reporting.Exit
-}
type BRE_DetailsBadDep
    = BRE_BD_BadDownload CEP_Name CEV_Version BRE_PackageProblem
    | BRE_BD_BadBuild CEP_Name CEV_Version (Dict ( String, String ) CEP_Name CEV_Version)



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
    Dict (List String) CEMN_Canonical CEI_DependencyInterface



-- TRANSITIVELY AVAILABLE TYPES


type CECTE_Types
    = -- PERF profile Opt.Global representation
      -- current representation needs less allocation
      -- but maybe the lookup is much worse
      CECTE_Types (Dict (List String) CEMN_Canonical CECTE_Types_)


type CECTE_Types_
    = CECTE_Types_ (Dict String CDN_Name CASTC_Union) (Dict String CDN_Name CASTC_Alias)



-- BACKGROUND WRITER


type BBW_Scope
    = BBW_Scope MVar_ListMVar
