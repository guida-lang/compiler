module Types exposing (..)

{-| -}

import Data.Map exposing (Dict)



-- CRAWL


{-| FIXME Builder.Elm.Details
-}
type alias BED_StatusDict =
    Dict String CEMN_Raw (MVar (Maybe BED_Status))


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
