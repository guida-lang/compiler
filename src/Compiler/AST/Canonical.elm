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
    , aliasDecoder
    , aliasEncoder
    , annotationDecoder
    , annotationEncoder
    , ctorOptsDecoder
    , ctorOptsEncoder
    , fieldUpdateDecoder
    , fieldUpdateEncoder
    , fieldsToList
    , typeDecoder
    , typeEncoder
    , unionDecoder
    , unionEncoder
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
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



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
    | Update Expr (Dict String (A.Located Name) FieldUpdate)
    | Record (Dict String (A.Located Name) Expr)
    | Unit
    | Tuple Expr Expr (List Expr)
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
    | PTuple Pattern Pattern (List Pattern)
    | PList (List Pattern)
    | PCons Pattern Pattern
    | PBool Union Bool
    | PChr String
    | PStr String Bool
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
    | TTuple Type Type (List Type)
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


annotationEncoder : Annotation -> BE.Encoder
annotationEncoder (Forall freeVars tipe) =
    BE.sequence
        [ freeVarsEncoder freeVars
        , typeEncoder tipe
        ]


annotationDecoder : BD.Decoder Annotation
annotationDecoder =
    BD.map2 Forall
        freeVarsDecoder
        typeDecoder


freeVarsEncoder : FreeVars -> BE.Encoder
freeVarsEncoder freeVars =
    BE.list BE.string (Dict.keys compare freeVars)


freeVarsDecoder : BD.Decoder FreeVars
freeVarsDecoder =
    BD.list BD.string
        |> BD.map (List.map (\key -> ( key, () )) >> Dict.fromList identity)


aliasEncoder : Alias -> BE.Encoder
aliasEncoder (Alias vars tipe) =
    BE.sequence
        [ BE.list BE.string vars
        , typeEncoder tipe
        ]


aliasDecoder : BD.Decoder Alias
aliasDecoder =
    BD.map2 Alias
        (BD.list BD.string)
        typeDecoder


typeEncoder : Type -> BE.Encoder
typeEncoder type_ =
    case type_ of
        TLambda a b ->
            BE.sequence
                [ BE.unsignedInt8 0
                , typeEncoder a
                , typeEncoder b
                ]

        TVar name ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string name
                ]

        TType home name args ->
            BE.sequence
                [ BE.unsignedInt8 2
                , ModuleName.canonicalEncoder home
                , BE.string name
                , BE.list typeEncoder args
                ]

        TRecord fields ext ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.assocListDict compare BE.string fieldTypeEncoder fields
                , BE.maybe BE.string ext
                ]

        TUnit ->
            BE.unsignedInt8 4

        TTuple a b cs ->
            BE.sequence
                [ BE.unsignedInt8 5
                , typeEncoder a
                , typeEncoder b
                , BE.list typeEncoder cs
                ]

        TAlias home name args tipe ->
            BE.sequence
                [ BE.unsignedInt8 6
                , ModuleName.canonicalEncoder home
                , BE.string name
                , BE.list (BE.jsonPair BE.string typeEncoder) args
                , aliasTypeEncoder tipe
                ]


typeDecoder : BD.Decoder Type
typeDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 TLambda
                            typeDecoder
                            typeDecoder

                    1 ->
                        BD.map TVar BD.string

                    2 ->
                        BD.map3 TType
                            ModuleName.canonicalDecoder
                            BD.string
                            (BD.list typeDecoder)

                    3 ->
                        BD.map2 TRecord
                            (BD.assocListDict identity BD.string fieldTypeDecoder)
                            (BD.maybe BD.string)

                    4 ->
                        BD.succeed TUnit

                    5 ->
                        BD.map3 TTuple
                            typeDecoder
                            typeDecoder
                            (BD.list typeDecoder)

                    6 ->
                        BD.map4 TAlias
                            ModuleName.canonicalDecoder
                            BD.string
                            (BD.list (BD.jsonPair BD.string typeDecoder))
                            aliasTypeDecoder

                    _ ->
                        BD.fail
            )


fieldTypeEncoder : FieldType -> BE.Encoder
fieldTypeEncoder (FieldType index tipe) =
    BE.sequence
        [ BE.int index
        , typeEncoder tipe
        ]


aliasTypeEncoder : AliasType -> BE.Encoder
aliasTypeEncoder aliasType =
    case aliasType of
        Holey tipe ->
            BE.sequence
                [ BE.unsignedInt8 0
                , typeEncoder tipe
                ]

        Filled tipe ->
            BE.sequence
                [ BE.unsignedInt8 1
                , typeEncoder tipe
                ]


fieldTypeDecoder : BD.Decoder FieldType
fieldTypeDecoder =
    BD.map2 FieldType
        BD.int
        typeDecoder


aliasTypeDecoder : BD.Decoder AliasType
aliasTypeDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Holey typeDecoder

                    1 ->
                        BD.map Filled typeDecoder

                    _ ->
                        BD.fail
            )


unionEncoder : Union -> BE.Encoder
unionEncoder (Union vars ctors numAlts opts) =
    BE.sequence
        [ BE.list BE.string vars
        , BE.list ctorEncoder ctors
        , BE.int numAlts
        , ctorOptsEncoder opts
        ]


unionDecoder : BD.Decoder Union
unionDecoder =
    BD.map4 Union
        (BD.list BD.string)
        (BD.list ctorDecoder)
        BD.int
        ctorOptsDecoder


ctorEncoder : Ctor -> BE.Encoder
ctorEncoder (Ctor ctor index numArgs args) =
    BE.sequence
        [ BE.string ctor
        , Index.zeroBasedEncoder index
        , BE.int numArgs
        , BE.list typeEncoder args
        ]


ctorDecoder : BD.Decoder Ctor
ctorDecoder =
    BD.map4 Ctor
        BD.string
        Index.zeroBasedDecoder
        BD.int
        (BD.list typeDecoder)


ctorOptsEncoder : CtorOpts -> BE.Encoder
ctorOptsEncoder ctorOpts =
    BE.unsignedInt8
        (case ctorOpts of
            Normal ->
                0

            Enum ->
                1

            Unbox ->
                2
        )


ctorOptsDecoder : BD.Decoder CtorOpts
ctorOptsDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Normal

                    1 ->
                        BD.succeed Enum

                    2 ->
                        BD.succeed Unbox

                    _ ->
                        BD.fail
            )


fieldUpdateEncoder : FieldUpdate -> BE.Encoder
fieldUpdateEncoder (FieldUpdate fieldRegion expr) =
    BE.sequence
        [ A.regionEncoder fieldRegion
        , exprEncoder expr
        ]


fieldUpdateDecoder : BD.Decoder FieldUpdate
fieldUpdateDecoder =
    BD.map2 FieldUpdate
        A.regionDecoder
        exprDecoder


exprEncoder : Expr -> BE.Encoder
exprEncoder =
    A.locatedEncoder expr_Encoder


exprDecoder : BD.Decoder Expr
exprDecoder =
    A.locatedDecoder expr_Decoder


expr_Encoder : Expr_ -> BE.Encoder
expr_Encoder expr_ =
    case expr_ of
        VarLocal name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string name
                ]

        VarTopLevel home name ->
            BE.sequence
                [ BE.unsignedInt8 1
                , ModuleName.canonicalEncoder home
                , BE.string name
                ]

        VarKernel home name ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string home
                , BE.string name
                ]

        VarForeign home name annotation ->
            BE.sequence
                [ BE.unsignedInt8 3
                , ModuleName.canonicalEncoder home
                , BE.string name
                , annotationEncoder annotation
                ]

        VarCtor opts home name index annotation ->
            BE.sequence
                [ BE.unsignedInt8 4
                , ctorOptsEncoder opts
                , ModuleName.canonicalEncoder home
                , BE.string name
                , Index.zeroBasedEncoder index
                , annotationEncoder annotation
                ]

        VarDebug home name annotation ->
            BE.sequence
                [ BE.unsignedInt8 5
                , ModuleName.canonicalEncoder home
                , BE.string name
                , annotationEncoder annotation
                ]

        VarOperator op home name annotation ->
            BE.sequence
                [ BE.unsignedInt8 6
                , BE.string op
                , ModuleName.canonicalEncoder home
                , BE.string name
                , annotationEncoder annotation
                ]

        Chr chr ->
            BE.sequence
                [ BE.unsignedInt8 7
                , BE.string chr
                ]

        Str str ->
            BE.sequence
                [ BE.unsignedInt8 8
                , BE.string str
                ]

        Int int ->
            BE.sequence
                [ BE.unsignedInt8 9
                , BE.int int
                ]

        Float float ->
            BE.sequence
                [ BE.unsignedInt8 10
                , BE.float float
                ]

        List entries ->
            BE.sequence
                [ BE.unsignedInt8 11
                , BE.list exprEncoder entries
                ]

        Negate expr ->
            BE.sequence
                [ BE.unsignedInt8 12
                , exprEncoder expr
                ]

        Binop op home name annotation left right ->
            BE.sequence
                [ BE.unsignedInt8 13
                , BE.string op
                , ModuleName.canonicalEncoder home
                , BE.string name
                , annotationEncoder annotation
                , exprEncoder left
                , exprEncoder right
                ]

        Lambda args body ->
            BE.sequence
                [ BE.unsignedInt8 14
                , BE.list patternEncoder args
                , exprEncoder body
                ]

        Call func args ->
            BE.sequence
                [ BE.unsignedInt8 15
                , exprEncoder func
                , BE.list exprEncoder args
                ]

        If branches finally ->
            BE.sequence
                [ BE.unsignedInt8 16
                , BE.list (BE.jsonPair exprEncoder exprEncoder) branches
                , exprEncoder finally
                ]

        Let def body ->
            BE.sequence
                [ BE.unsignedInt8 17
                , defEncoder def
                , exprEncoder body
                ]

        LetRec defs body ->
            BE.sequence
                [ BE.unsignedInt8 18
                , BE.list defEncoder defs
                , exprEncoder body
                ]

        LetDestruct pattern expr body ->
            BE.sequence
                [ BE.unsignedInt8 19
                , patternEncoder pattern
                , exprEncoder expr
                , exprEncoder body
                ]

        Case expr branches ->
            BE.sequence
                [ BE.unsignedInt8 20
                , exprEncoder expr
                , BE.list caseBranchEncoder branches
                ]

        Accessor field ->
            BE.sequence
                [ BE.unsignedInt8 21
                , BE.string field
                ]

        Access record field ->
            BE.sequence
                [ BE.unsignedInt8 22
                , exprEncoder record
                , A.locatedEncoder BE.string field
                ]

        Update record updates ->
            BE.sequence
                [ BE.unsignedInt8 23
                , exprEncoder record
                , BE.assocListDict A.compareLocated (A.toValue >> BE.string) fieldUpdateEncoder updates
                ]

        Record fields ->
            BE.sequence
                [ BE.unsignedInt8 24
                , BE.assocListDict A.compareLocated (A.toValue >> BE.string) exprEncoder fields
                ]

        Unit ->
            BE.unsignedInt8 25

        Tuple a b cs ->
            BE.sequence
                [ BE.unsignedInt8 26
                , exprEncoder a
                , exprEncoder b
                , BE.list exprEncoder cs
                ]

        Shader src types ->
            BE.sequence
                [ BE.unsignedInt8 27
                , Shader.sourceEncoder src
                , Shader.typesEncoder types
                ]


expr_Decoder : BD.Decoder Expr_
expr_Decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map VarLocal BD.string

                    1 ->
                        BD.map2 VarTopLevel
                            ModuleName.canonicalDecoder
                            BD.string

                    2 ->
                        BD.map2 VarKernel
                            BD.string
                            BD.string

                    3 ->
                        BD.map3 VarForeign
                            ModuleName.canonicalDecoder
                            BD.string
                            annotationDecoder

                    4 ->
                        BD.map5 VarCtor
                            ctorOptsDecoder
                            ModuleName.canonicalDecoder
                            BD.string
                            Index.zeroBasedDecoder
                            annotationDecoder

                    5 ->
                        BD.map3 VarDebug
                            ModuleName.canonicalDecoder
                            BD.string
                            annotationDecoder

                    6 ->
                        BD.map4 VarOperator
                            BD.string
                            ModuleName.canonicalDecoder
                            BD.string
                            annotationDecoder

                    7 ->
                        BD.map Chr BD.string

                    8 ->
                        BD.map Str BD.string

                    9 ->
                        BD.map Int BD.int

                    10 ->
                        BD.map Float BD.float

                    11 ->
                        BD.map List (BD.list exprDecoder)

                    12 ->
                        BD.map Negate exprDecoder

                    13 ->
                        BD.map6 Binop
                            BD.string
                            ModuleName.canonicalDecoder
                            BD.string
                            annotationDecoder
                            exprDecoder
                            exprDecoder

                    14 ->
                        BD.map2 Lambda
                            (BD.list patternDecoder)
                            exprDecoder

                    15 ->
                        BD.map2 Call
                            exprDecoder
                            (BD.list exprDecoder)

                    16 ->
                        BD.map2 If
                            (BD.list (BD.jsonPair exprDecoder exprDecoder))
                            exprDecoder

                    17 ->
                        BD.map2 Let
                            defDecoder
                            exprDecoder

                    18 ->
                        BD.map2 LetRec
                            (BD.list defDecoder)
                            exprDecoder

                    19 ->
                        BD.map3 LetDestruct
                            patternDecoder
                            exprDecoder
                            exprDecoder

                    20 ->
                        BD.map2 Case
                            exprDecoder
                            (BD.list caseBranchDecoder)

                    21 ->
                        BD.map Accessor BD.string

                    22 ->
                        BD.map2 Access
                            exprDecoder
                            (A.locatedDecoder BD.string)

                    23 ->
                        BD.map2 Update
                            exprDecoder
                            (BD.assocListDict A.toValue (A.locatedDecoder BD.string) fieldUpdateDecoder)

                    24 ->
                        BD.map Record
                            (BD.assocListDict A.toValue (A.locatedDecoder BD.string) exprDecoder)

                    25 ->
                        BD.succeed Unit

                    26 ->
                        BD.map3 Tuple
                            exprDecoder
                            exprDecoder
                            (BD.list exprDecoder)

                    27 ->
                        BD.map2 Shader
                            Shader.sourceDecoder
                            Shader.typesDecoder

                    _ ->
                        BD.fail
            )


patternEncoder : Pattern -> BE.Encoder
patternEncoder =
    A.locatedEncoder pattern_Encoder


patternDecoder : BD.Decoder Pattern
patternDecoder =
    A.locatedDecoder pattern_Decoder


pattern_Encoder : Pattern_ -> BE.Encoder
pattern_Encoder pattern_ =
    case pattern_ of
        PAnything ->
            BE.unsignedInt8 0

        PVar name ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string name
                ]

        PRecord names ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.list BE.string names
                ]

        PAlias pattern name ->
            BE.sequence
                [ BE.unsignedInt8 3
                , patternEncoder pattern
                , BE.string name
                ]

        PUnit ->
            BE.unsignedInt8 4

        PTuple pattern1 pattern2 otherPatterns ->
            BE.sequence
                [ BE.unsignedInt8 5
                , patternEncoder pattern1
                , patternEncoder pattern2
                , BE.list patternEncoder otherPatterns
                ]

        PList patterns ->
            BE.sequence
                [ BE.unsignedInt8 6
                , BE.list patternEncoder patterns
                ]

        PCons pattern1 pattern2 ->
            BE.sequence
                [ BE.unsignedInt8 7
                , patternEncoder pattern1
                , patternEncoder pattern2
                ]

        PBool union bool ->
            BE.sequence
                [ BE.unsignedInt8 8
                , unionEncoder union
                , BE.bool bool
                ]

        PChr chr ->
            BE.sequence
                [ BE.unsignedInt8 9
                , BE.string chr
                ]

        PStr str multiline ->
            BE.sequence
                [ BE.unsignedInt8 10
                , BE.string str
                , BE.bool multiline
                ]

        PInt int ->
            BE.sequence
                [ BE.unsignedInt8 11
                , BE.int int
                ]

        PCtor { home, type_, union, name, index, args } ->
            BE.sequence
                [ BE.unsignedInt8 12
                , ModuleName.canonicalEncoder home
                , BE.string type_
                , unionEncoder union
                , BE.string name
                , Index.zeroBasedEncoder index
                , BE.list patternCtorArgEncoder args
                ]


pattern_Decoder : BD.Decoder Pattern_
pattern_Decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed PAnything

                    1 ->
                        BD.map PVar
                            BD.string

                    2 ->
                        BD.map PRecord
                            (BD.list BD.string)

                    3 ->
                        BD.map2 PAlias
                            patternDecoder
                            BD.string

                    4 ->
                        BD.succeed PUnit

                    5 ->
                        BD.map3 PTuple
                            patternDecoder
                            patternDecoder
                            (BD.list patternDecoder)

                    6 ->
                        BD.map PList
                            (BD.list patternDecoder)

                    7 ->
                        BD.map2 PCons
                            patternDecoder
                            patternDecoder

                    8 ->
                        BD.map2 PBool
                            unionDecoder
                            BD.bool

                    9 ->
                        BD.map PChr BD.string

                    10 ->
                        BD.map2 PStr
                            BD.string
                            BD.bool

                    11 ->
                        BD.map PInt BD.int

                    12 ->
                        BD.map6
                            (\home type_ union name index args ->
                                PCtor
                                    { home = home
                                    , type_ = type_
                                    , union = union
                                    , name = name
                                    , index = index
                                    , args = args
                                    }
                            )
                            ModuleName.canonicalDecoder
                            BD.string
                            unionDecoder
                            BD.string
                            Index.zeroBasedDecoder
                            (BD.list patternCtorArgDecoder)

                    _ ->
                        BD.fail
            )


patternCtorArgEncoder : PatternCtorArg -> BE.Encoder
patternCtorArgEncoder (PatternCtorArg index srcType pattern) =
    BE.sequence
        [ Index.zeroBasedEncoder index
        , typeEncoder srcType
        , patternEncoder pattern
        ]


patternCtorArgDecoder : BD.Decoder PatternCtorArg
patternCtorArgDecoder =
    BD.map3 PatternCtorArg
        Index.zeroBasedDecoder
        typeDecoder
        patternDecoder


defEncoder : Def -> BE.Encoder
defEncoder def =
    case def of
        Def name args expr ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.locatedEncoder BE.string name
                , BE.list patternEncoder args
                , exprEncoder expr
                ]

        TypedDef name freeVars typedArgs expr srcResultType ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.locatedEncoder BE.string name
                , freeVarsEncoder freeVars
                , BE.list (BE.jsonPair patternEncoder typeEncoder) typedArgs
                , exprEncoder expr
                , typeEncoder srcResultType
                ]


defDecoder : BD.Decoder Def
defDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map3 Def
                            (A.locatedDecoder BD.string)
                            (BD.list patternDecoder)
                            exprDecoder

                    1 ->
                        BD.map5 TypedDef
                            (A.locatedDecoder BD.string)
                            freeVarsDecoder
                            (BD.list (BD.jsonPair patternDecoder typeDecoder))
                            exprDecoder
                            typeDecoder

                    _ ->
                        BD.fail
            )


caseBranchEncoder : CaseBranch -> BE.Encoder
caseBranchEncoder (CaseBranch pattern expr) =
    BE.sequence
        [ patternEncoder pattern
        , exprEncoder expr
        ]


caseBranchDecoder : BD.Decoder CaseBranch
caseBranchDecoder =
    BD.map2 CaseBranch
        patternDecoder
        exprDecoder
