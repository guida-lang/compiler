module Compiler.AST.Canonical exposing
    ( Alias(..)
    , AliasType(..)
    , Annotation(..)
    , Binop(..)
    , CaseBranch(..)
    , Ctor(..)
    , CtorOpts(..)
    , Decls(..)
    , Def(..)
    , Effects(..)
    , Export(..)
    , Exports(..)
    , Expr
    , Expr_(..)
    , FieldType(..)
    , FieldUpdate(..)
    , FreeVars
    , Manager(..)
    , Module(..)
    , Pattern
    , PatternCtorArg(..)
    , Pattern_(..)
    , Port(..)
    , Type(..)
    , Union(..)
    , aliasCodec
    , annotationCodec
    , ctorOptsCodec
    , fieldUpdateCodec
    , fieldsToList
    , typeCodec
    , unionCodec
    )

{- Creating a canonical AST means finding the home module for all variables.
   So if you have L.map, you need to figure out that it is from the elm/core
   package in the List module.

   In later phases (e.g. type inference, exhaustiveness checking, optimization)
   you need to look up additional info from these modules. What is the type?
   What are the alternative type constructors? These lookups can be quite costly,
   especially in type inference. To reduce costs the canonicalization phase
   caches info needed in later phases. This means we no longer build large
   dictionaries of metadata with O(log(n)) lookups in those phases. Instead
   there is an O(1) read of an existing field! I have tried to mark all
   cached data with comments like:

   -- CACHE for exhaustiveness
   -- CACHE for inference

   So it is clear why the data is kept around.
-}

import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Serialize as S
import Data.Map as Dict exposing (Dict)
import Serialize exposing (Codec)
import System.TypeCheck.IO as IO



-- EXPRESSIONS


type alias Expr =
    A.Located Expr_



-- CACHE Annotations for type inference


type Expr_
    = VarLocal Name
    | VarTopLevel IO.Canonical Name
    | VarKernel Name Name
    | VarForeign IO.Canonical Name Annotation
    | VarCtor CtorOpts IO.Canonical Name Index.ZeroBased Annotation
    | VarDebug IO.Canonical Name Annotation
    | VarOperator Name IO.Canonical Name Annotation -- CACHE real name for optimization
    | Chr String
    | Str String
    | Int Int
    | Float Float
    | List (List Expr)
    | Negate Expr
    | Binop Name IO.Canonical Name Annotation Expr Expr -- CACHE real name for optimization
    | Lambda (List Pattern) Expr
    | Call Expr (List Expr)
    | If (List ( Expr, Expr )) Expr
    | Let Def Expr
    | LetRec (List Def) Expr
    | LetDestruct Pattern Expr Expr
    | Case Expr (List CaseBranch)
    | Accessor Name
    | Access Expr (A.Located Name)
    | Update (Maybe Name) Name Expr (Dict String (A.Located Name) FieldUpdate)
    | Record (Dict String (A.Located Name) Expr)
    | Unit
    | Tuple Expr Expr (Maybe Expr)
    | Shader Shader.Source Shader.Types


type CaseBranch
    = CaseBranch Pattern Expr


type FieldUpdate
    = FieldUpdate A.Region Expr



-- DEFS


type Def
    = Def (A.Located Name) (List Pattern) Expr
    | TypedDef (A.Located Name) FreeVars (List ( Pattern, Type )) Expr Type


type Decls
    = Declare Def Decls
    | DeclareRec Def (List Def) Decls
    | SaveTheEnvironment



-- PATTERNS


type alias Pattern =
    A.Located Pattern_


type Pattern_
    = PAnything
    | PVar Name
    | PRecord (List Name)
    | PAlias Pattern Name
    | PUnit
    | PTuple Pattern Pattern (Maybe Pattern)
    | PList (List Pattern)
    | PCons Pattern Pattern
    | PBool Union Bool
    | PChr String
    | PStr String
    | PInt Int
    | PCtor
        -- CACHE p_home, p_type, and p_vars for type inference
        -- CACHE p_index to replace p_name in PROD code gen
        -- CACHE p_opts to allocate less in PROD code gen
        -- CACHE p_alts and p_numAlts for exhaustiveness checker
        { home : IO.Canonical
        , type_ : Name
        , union : Union
        , name : Name
        , index : Index.ZeroBased
        , args : List PatternCtorArg
        }


type PatternCtorArg
    = PatternCtorArg
        -- CACHE for destructors/errors
        Index.ZeroBased
        -- CACHE for type inference
        Type
        Pattern



-- TYPES


type Annotation
    = Forall FreeVars Type


type alias FreeVars =
    Dict String Name ()


type Type
    = TLambda Type Type
    | TVar Name
    | TType IO.Canonical Name (List Type)
    | TRecord (Dict String Name FieldType) (Maybe Name)
    | TUnit
    | TTuple Type Type (Maybe Type)
    | TAlias IO.Canonical Name (List ( Name, Type )) AliasType


type AliasType
    = Holey Type
    | Filled Type


type FieldType
    = FieldType Int Type



-- NOTE: The Word16 marks the source order, but it may not be available
-- for every canonical type. For example, if the canonical type is inferred
-- the orders will all be zeros.


fieldsToList : Dict String Name FieldType -> List ( Name, Type )
fieldsToList fields =
    let
        getIndex : ( a, FieldType ) -> Int
        getIndex ( _, FieldType index _ ) =
            index

        dropIndex : ( a, FieldType ) -> ( a, Type )
        dropIndex ( name, FieldType _ tipe ) =
            ( name, tipe )
    in
    Dict.toList compare fields
        |> List.sortBy getIndex
        |> List.map dropIndex



-- MODULES


type Module
    = Module IO.Canonical Exports Src.Docs Decls (Dict String Name Union) (Dict String Name Alias) (Dict String Name Binop) Effects


type Alias
    = Alias (List Name) Type


type Binop
    = Binop_ Binop.Associativity Binop.Precedence Name


type Union
    = Union
        (List Name)
        (List Ctor)
        -- CACHE numAlts for exhaustiveness checking
        Int
        -- CACHE which optimizations are available
        CtorOpts


type CtorOpts
    = Normal
    | Enum
    | Unbox


type Ctor
    = Ctor Name Index.ZeroBased Int (List Type) -- CACHE length args



-- EXPORTS


type Exports
    = ExportEverything A.Region
    | Export (Dict String Name (A.Located Export))


type Export
    = ExportValue
    | ExportBinop
    | ExportAlias
    | ExportUnionOpen
    | ExportUnionClosed
    | ExportPort


type Effects
    = NoEffects
    | Ports (Dict String Name Port)
    | Manager A.Region A.Region A.Region Manager


type Port
    = Incoming
        { freeVars : FreeVars
        , payload : Type
        , func : Type
        }
    | Outgoing
        { freeVars : FreeVars
        , payload : Type
        , func : Type
        }


type Manager
    = Cmd Name
    | Sub Name
    | Fx Name Name



-- ENCODERS and DECODERS


annotationCodec : Codec e Annotation
annotationCodec =
    Serialize.customType
        (\forallEncoder (Forall freeVars tipe) ->
            forallEncoder freeVars tipe
        )
        |> Serialize.variant2 Forall freeVarsCodec typeCodec
        |> Serialize.finishCustomType


freeVarsCodec : Codec e FreeVars
freeVarsCodec =
    S.assocListDict identity compare Serialize.string Serialize.unit


aliasCodec : Codec e Alias
aliasCodec =
    Serialize.customType
        (\aliasCodecEncoder (Alias vars tipe) ->
            aliasCodecEncoder vars tipe
        )
        |> Serialize.variant2 Alias (Serialize.list Serialize.string) typeCodec
        |> Serialize.finishCustomType


typeCodec : Codec e Type
typeCodec =
    Serialize.customType
        (\tLambdaEncoder tVarEncoder tTypeEncoder tRecordEncoder tUnitEncoder tTupleEncoder tAliasEncoder value ->
            case value of
                TLambda a b ->
                    tLambdaEncoder a b

                TVar name ->
                    tVarEncoder name

                TType home name args ->
                    tTypeEncoder home name args

                TRecord fields ext ->
                    tRecordEncoder fields ext

                TUnit ->
                    tUnitEncoder

                TTuple a b maybeC ->
                    tTupleEncoder a b maybeC

                TAlias home name args tipe ->
                    tAliasEncoder home name args tipe
        )
        |> Serialize.variant2 TLambda (Serialize.lazy (\() -> typeCodec)) (Serialize.lazy (\() -> typeCodec))
        |> Serialize.variant1 TVar Serialize.string
        |> Serialize.variant3 TType ModuleName.canonicalCodec Serialize.string (Serialize.list (Serialize.lazy (\() -> typeCodec)))
        |> Serialize.variant2 TRecord (S.assocListDict identity compare Serialize.string fieldTypeCodec) (Serialize.maybe Serialize.string)
        |> Serialize.variant0 TUnit
        |> Serialize.variant3 TTuple (Serialize.lazy (\() -> typeCodec)) (Serialize.lazy (\() -> typeCodec)) (Serialize.maybe (Serialize.lazy (\() -> typeCodec)))
        |> Serialize.variant4 TAlias ModuleName.canonicalCodec Serialize.string (Serialize.list (Serialize.tuple Serialize.string (Serialize.lazy (\() -> typeCodec)))) aliasTypeCodec
        |> Serialize.finishCustomType


fieldTypeCodec : Codec e FieldType
fieldTypeCodec =
    Serialize.customType
        (\fieldTypeCodecEncoder (FieldType index tipe) ->
            fieldTypeCodecEncoder index tipe
        )
        |> Serialize.variant2 FieldType Serialize.int (Serialize.lazy (\() -> typeCodec))
        |> Serialize.finishCustomType


aliasTypeCodec : Codec e AliasType
aliasTypeCodec =
    Serialize.customType
        (\holeyEncoder filledEncoder value ->
            case value of
                Holey tipe ->
                    holeyEncoder tipe

                Filled tipe ->
                    filledEncoder tipe
        )
        |> Serialize.variant1 Holey (Serialize.lazy (\() -> typeCodec))
        |> Serialize.variant1 Filled (Serialize.lazy (\() -> typeCodec))
        |> Serialize.finishCustomType


unionCodec : Codec e Union
unionCodec =
    Serialize.customType
        (\unionCodecEncoder (Union vars ctors numAlts opts) ->
            unionCodecEncoder vars ctors numAlts opts
        )
        |> Serialize.variant4 Union
            (Serialize.list Serialize.string)
            (Serialize.list ctorCodec)
            Serialize.int
            ctorOptsCodec
        |> Serialize.finishCustomType


ctorCodec : Codec e Ctor
ctorCodec =
    Serialize.customType
        (\ctorCodecEncoder (Ctor ctor index numArgs args) ->
            ctorCodecEncoder ctor index numArgs args
        )
        |> Serialize.variant4 Ctor
            Serialize.string
            Index.zeroBasedCodec
            Serialize.int
            (Serialize.list typeCodec)
        |> Serialize.finishCustomType


ctorOptsCodec : Codec e CtorOpts
ctorOptsCodec =
    Serialize.customType
        (\normalEncoder enumEncoder unboxEncoder value ->
            case value of
                Normal ->
                    normalEncoder

                Enum ->
                    enumEncoder

                Unbox ->
                    unboxEncoder
        )
        |> Serialize.variant0 Normal
        |> Serialize.variant0 Enum
        |> Serialize.variant0 Unbox
        |> Serialize.finishCustomType


fieldUpdateCodec : Codec e FieldUpdate
fieldUpdateCodec =
    Serialize.customType
        (\fieldUpdateCodecEncoder (FieldUpdate fieldRegion expr) ->
            fieldUpdateCodecEncoder fieldRegion expr
        )
        |> Serialize.variant2 FieldUpdate A.regionCodec exprCodec
        |> Serialize.finishCustomType


exprCodec : Codec e Expr
exprCodec =
    A.locatedCodec (Serialize.lazy (\() -> expr_Codec))


expr_Codec : Codec e Expr_
expr_Codec =
    Serialize.customType
        (\varLocalEncoder varTopLevelEncoder varKernelEncoder varForeignEncoder varCtorEncoder varDebugEncoder varOperatorEncoder chrEncoder strEncoder intEncoder floatEncoder listEncoder negateEncoder binopEncoder lambdaEncoder callEncoder ifEncoder letEncoder letRecEncoder letDestructEncoder caseEncoder accessorEncoder accessEncoder updateEncoder recordEncoder unitEncoder tupleEncoder shaderEncoder value ->
            case value of
                VarLocal name ->
                    varLocalEncoder name

                VarTopLevel home name ->
                    varTopLevelEncoder home name

                VarKernel home name ->
                    varKernelEncoder home name

                VarForeign home name annotation ->
                    varForeignEncoder home name annotation

                VarCtor opts home name index annotation ->
                    varCtorEncoder opts home name index annotation

                VarDebug home name annotation ->
                    varDebugEncoder home name annotation

                VarOperator op home name annotation ->
                    varOperatorEncoder op home name annotation

                Chr chr ->
                    chrEncoder chr

                Str str ->
                    strEncoder str

                Int int ->
                    intEncoder int

                Float float ->
                    floatEncoder float

                List entries ->
                    listEncoder entries

                Negate expr ->
                    negateEncoder expr

                Binop op home name annotation left right ->
                    binopEncoder op home name annotation left right

                Lambda args body ->
                    lambdaEncoder args body

                Call func args ->
                    callEncoder func args

                If branches finally ->
                    ifEncoder branches finally

                Let def body ->
                    letEncoder def body

                LetRec defs body ->
                    letRecEncoder defs body

                LetDestruct pattern expr body ->
                    letDestructEncoder pattern expr body

                Case expr branches ->
                    caseEncoder expr branches

                Accessor field ->
                    accessorEncoder field

                Access record field ->
                    accessEncoder record field

                Update namespace name record updates ->
                    updateEncoder namespace name record updates

                Record fields ->
                    recordEncoder fields

                Unit ->
                    unitEncoder

                Tuple a b maybeC ->
                    tupleEncoder a b maybeC

                Shader src types ->
                    shaderEncoder src types
        )
        |> Serialize.variant1 VarLocal Serialize.string
        |> Serialize.variant2 VarTopLevel ModuleName.canonicalCodec Serialize.string
        |> Serialize.variant2 VarKernel Serialize.string Serialize.string
        |> Serialize.variant3 VarForeign ModuleName.canonicalCodec Serialize.string annotationCodec
        |> Serialize.variant5
            VarCtor
            ctorOptsCodec
            ModuleName.canonicalCodec
            Serialize.string
            Index.zeroBasedCodec
            annotationCodec
        |> Serialize.variant3 VarDebug ModuleName.canonicalCodec Serialize.string annotationCodec
        |> Serialize.variant4 VarOperator Serialize.string ModuleName.canonicalCodec Serialize.string annotationCodec
        |> Serialize.variant1 Chr Serialize.string
        |> Serialize.variant1 Str Serialize.string
        |> Serialize.variant1 Int Serialize.int
        |> Serialize.variant1 Float Serialize.float
        |> Serialize.variant1 List (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
        |> Serialize.variant1 Negate (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant6
            Binop
            Serialize.string
            ModuleName.canonicalCodec
            Serialize.string
            annotationCodec
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2
            Lambda
            (Serialize.list (A.locatedCodec pattern_Codec))
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2
            Call
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
        |> Serialize.variant2
            If
            (Serialize.list
                (Serialize.tuple
                    (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
                    (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
                )
            )
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2 Let defCodec exprCodec
        |> Serialize.variant2 LetRec (Serialize.list defCodec) (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant3
            LetDestruct
            (A.locatedCodec pattern_Codec)
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2
            Case
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (Serialize.list caseBranchCodec)
        |> Serialize.variant1 Accessor Serialize.string
        |> Serialize.variant2
            Access
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (A.locatedCodec Serialize.string)
        |> Serialize.variant4
            Update
            (Serialize.maybe Serialize.string)
            Serialize.string
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (S.assocListDict A.toValue A.compareLocated (A.locatedCodec Serialize.string) fieldUpdateCodec)
        |> Serialize.variant1 Record (S.assocListDict A.toValue A.compareLocated (A.locatedCodec Serialize.string) (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
        |> Serialize.variant0 Unit
        |> Serialize.variant3
            Tuple
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (Serialize.maybe (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
        |> Serialize.variant2 Shader Shader.sourceCodec Shader.typesCodec
        |> Serialize.finishCustomType


patternCodec : Codec e Pattern
patternCodec =
    A.locatedCodec (Serialize.lazy (\() -> pattern_Codec))


pattern_Codec : Codec e Pattern_
pattern_Codec =
    Serialize.customType
        (\pAnythingEncoder pVarEncoder pRecordEncoder pAliasEncoder pUnitEncoder pTupleEncoder pListEncoder pConsEncoder pBoolEncoder pChrEncoder pStrEncoder pIntEncoder pCtorEncoder value ->
            case value of
                PAnything ->
                    pAnythingEncoder

                PVar name ->
                    pVarEncoder name

                PRecord names ->
                    pRecordEncoder names

                PAlias pattern name ->
                    pAliasEncoder pattern name

                PUnit ->
                    pUnitEncoder

                PTuple pattern1 pattern2 maybePattern3 ->
                    pTupleEncoder pattern1 pattern2 maybePattern3

                PList patterns ->
                    pListEncoder patterns

                PCons pattern1 pattern2 ->
                    pConsEncoder pattern1 pattern2

                PBool union bool ->
                    pBoolEncoder union bool

                PChr chr ->
                    pChrEncoder chr

                PStr str ->
                    pStrEncoder str

                PInt int ->
                    pIntEncoder int

                PCtor ctor ->
                    pCtorEncoder ctor
        )
        |> Serialize.variant0 PAnything
        |> Serialize.variant1 PVar Serialize.string
        |> Serialize.variant1 PRecord (Serialize.list Serialize.string)
        |> Serialize.variant2 PAlias patternCodec Serialize.string
        |> Serialize.variant0 PUnit
        |> Serialize.variant3 PTuple patternCodec patternCodec (Serialize.maybe patternCodec)
        |> Serialize.variant1 PList (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec))))
        |> Serialize.variant2 PCons patternCodec patternCodec
        |> Serialize.variant2 PBool unionCodec Serialize.bool
        |> Serialize.variant1 PChr Serialize.string
        |> Serialize.variant1 PStr Serialize.string
        |> Serialize.variant1 PInt Serialize.int
        |> Serialize.variant1
            PCtor
            (Serialize.record
                (\home type_ union name index args ->
                    { home = home, type_ = type_, union = union, name = name, index = index, args = args }
                )
                |> Serialize.field .home ModuleName.canonicalCodec
                |> Serialize.field .type_ Serialize.string
                |> Serialize.field .union unionCodec
                |> Serialize.field .name Serialize.string
                |> Serialize.field .index Index.zeroBasedCodec
                |> Serialize.field .args (Serialize.list patternCtorArgCodec)
                |> Serialize.finishRecord
            )
        |> Serialize.finishCustomType


patternCtorArgCodec : Codec e PatternCtorArg
patternCtorArgCodec =
    Serialize.customType
        (\patternCtorArgCodecEncoder (PatternCtorArg index srcType pattern) ->
            patternCtorArgCodecEncoder index srcType pattern
        )
        |> Serialize.variant3 PatternCtorArg Index.zeroBasedCodec typeCodec patternCodec
        |> Serialize.finishCustomType


defCodec : Codec e Def
defCodec =
    Serialize.customType
        (\defCodecEncoder typedDefEncoder def ->
            case def of
                Def name args expr ->
                    defCodecEncoder name args expr

                TypedDef name freeVars typedArgs expr srcResultType ->
                    typedDefEncoder name freeVars typedArgs expr srcResultType
        )
        |> Serialize.variant3 Def (A.locatedCodec Serialize.string) (Serialize.list patternCodec) exprCodec
        |> Serialize.variant5 TypedDef (A.locatedCodec Serialize.string) freeVarsCodec (Serialize.list (Serialize.tuple patternCodec typeCodec)) exprCodec typeCodec
        |> Serialize.finishCustomType


caseBranchCodec : Codec e CaseBranch
caseBranchCodec =
    Serialize.customType
        (\caseBranchCodecEncoder (CaseBranch pattern expr) ->
            caseBranchCodecEncoder pattern expr
        )
        |> Serialize.variant2 CaseBranch patternCodec exprCodec
        |> Serialize.finishCustomType
