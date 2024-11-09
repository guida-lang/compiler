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
    , aliasDecoder
    , aliasEncoder
    , annotationCodec
    , annotationDecoder
    , annotationEncoder
    , ctorOptsCodec
    , ctorOptsDecoder
    , ctorOptsEncoder
    , fieldUpdateDecoder
    , fieldUpdateEncoder
    , fieldsToList
    , typeCodec
    , typeDecoder
    , typeEncoder
    , unionCodec
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
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Reporting.Annotation as A
import Compiler.Serialize as S
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Serialize exposing (Codec)



-- EXPRESSIONS


type alias Expr =
    A.Located Expr_



-- CACHE Annotations for type inference


type Expr_
    = VarLocal Name
    | VarTopLevel ModuleName.Canonical Name
    | VarKernel Name Name
    | VarForeign ModuleName.Canonical Name Annotation
    | VarCtor CtorOpts ModuleName.Canonical Name Index.ZeroBased Annotation
    | VarDebug ModuleName.Canonical Name Annotation
    | VarOperator Name ModuleName.Canonical Name Annotation -- CACHE real name for optimization
    | Chr String
    | Str String
    | Int Int
    | Float Float
    | List (List Expr)
    | Negate Expr
    | Binop Name ModuleName.Canonical Name Annotation Expr Expr -- CACHE real name for optimization
    | Lambda (List Pattern) Expr
    | Call Expr (List Expr)
    | If (List ( Expr, Expr )) Expr
    | Let Def Expr
    | LetRec (List Def) Expr
    | LetDestruct Pattern Expr Expr
    | Case Expr (List CaseBranch)
    | Accessor Name
    | Access Expr (A.Located Name)
    | Update Name Expr (Dict Name FieldUpdate)
    | Record (Dict Name Expr)
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
        { home : ModuleName.Canonical
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
    Dict Name ()


type Type
    = TLambda Type Type
    | TVar Name
    | TType ModuleName.Canonical Name (List Type)
    | TRecord (Dict Name FieldType) (Maybe Name)
    | TUnit
    | TTuple Type Type (Maybe Type)
    | TAlias ModuleName.Canonical Name (List ( Name, Type )) AliasType


type AliasType
    = Holey Type
    | Filled Type


type FieldType
    = FieldType Int Type



-- NOTE: The Word16 marks the source order, but it may not be available
-- for every canonical type. For example, if the canonical type is inferred
-- the orders will all be zeros.


fieldsToList : Dict Name FieldType -> List ( Name, Type )
fieldsToList fields =
    let
        getIndex : ( a, FieldType ) -> Int
        getIndex ( _, FieldType index _ ) =
            index

        dropIndex : ( a, FieldType ) -> ( a, Type )
        dropIndex ( name, FieldType _ tipe ) =
            ( name, tipe )
    in
    Dict.toList fields
        |> List.sortBy getIndex
        |> List.map dropIndex



-- MODULES


type Module
    = Module ModuleName.Canonical Exports Src.Docs Decls (Dict Name Union) (Dict Name Alias) (Dict Name Binop) Effects


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
    | Export (Dict Name (A.Located Export))


type Export
    = ExportValue
    | ExportBinop
    | ExportAlias
    | ExportUnionOpen
    | ExportUnionClosed
    | ExportPort


type Effects
    = NoEffects
    | Ports (Dict Name Port)
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


annotationEncoder : Annotation -> Encode.Value
annotationEncoder (Forall freeVars tipe) =
    Encode.object
        [ ( "type", Encode.string "Forall" )
        , ( "freeVars", freeVarsEncoder freeVars )
        , ( "tipe", typeEncoder tipe )
        ]


annotationDecoder : Decode.Decoder Annotation
annotationDecoder =
    Decode.map2 Forall
        (Decode.field "freeVars" freeVarsDecoder)
        (Decode.field "tipe" typeDecoder)


annotationCodec : Codec e Annotation
annotationCodec =
    Serialize.customType
        (\forallEncoder (Forall freeVars tipe) ->
            forallEncoder freeVars tipe
        )
        |> Serialize.variant2 Forall freeVarsCodec typeCodec
        |> Serialize.finishCustomType


freeVarsEncoder : FreeVars -> Encode.Value
freeVarsEncoder =
    E.assocListDict Encode.string (\_ -> Encode.object [])


freeVarsDecoder : Decode.Decoder FreeVars
freeVarsDecoder =
    D.assocListDict compare Decode.string (Decode.succeed ())


freeVarsCodec : Codec e FreeVars
freeVarsCodec =
    S.assocListDict compare Serialize.string Serialize.unit


aliasEncoder : Alias -> Encode.Value
aliasEncoder (Alias vars tipe) =
    Encode.object
        [ ( "vars", Encode.list Encode.string vars )
        , ( "tipe", typeEncoder tipe )
        ]


aliasDecoder : Decode.Decoder Alias
aliasDecoder =
    Decode.map2 Alias
        (Decode.field "vars" (Decode.list Decode.string))
        (Decode.field "tipe" typeDecoder)


aliasCodec : Codec e Alias
aliasCodec =
    Serialize.customType
        (\aliasCodecEncoder (Alias vars tipe) ->
            aliasCodecEncoder vars tipe
        )
        |> Serialize.variant2 Alias (Serialize.list Serialize.string) typeCodec
        |> Serialize.finishCustomType


typeEncoder : Type -> Encode.Value
typeEncoder type_ =
    case type_ of
        TLambda a b ->
            Encode.object
                [ ( "type", Encode.string "TLambda" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                ]

        TVar name ->
            Encode.object
                [ ( "type", Encode.string "TVar" )
                , ( "name", Encode.string name )
                ]

        TType home name args ->
            Encode.object
                [ ( "type", Encode.string "TType" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        TRecord fields ext ->
            Encode.object
                [ ( "type", Encode.string "TRecord" )
                , ( "fields", E.assocListDict Encode.string fieldTypeEncoder fields )
                , ( "ext", E.maybe Encode.string ext )
                ]

        TUnit ->
            Encode.object
                [ ( "type", Encode.string "TUnit" )
                ]

        TTuple a b maybeC ->
            Encode.object
                [ ( "type", Encode.string "TTuple" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                , ( "maybeC", E.maybe typeEncoder maybeC )
                ]

        TAlias home name args tipe ->
            Encode.object
                [ ( "type", Encode.string "TAlias" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list (E.jsonPair Encode.string typeEncoder) args )
                , ( "tipe", aliasTypeEncoder tipe )
                ]


typeDecoder : Decode.Decoder Type
typeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TLambda" ->
                        Decode.map2 TLambda
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)

                    "TVar" ->
                        Decode.map TVar
                            (Decode.field "name" Decode.string)

                    "TType" ->
                        Decode.map3 TType
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "TRecord" ->
                        Decode.map2 TRecord
                            (Decode.field "fields" (D.assocListDict compare Decode.string fieldTypeDecoder))
                            (Decode.field "ext" (Decode.maybe Decode.string))

                    "TUnit" ->
                        Decode.succeed TUnit

                    "TTuple" ->
                        Decode.map3 TTuple
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)
                            (Decode.field "maybeC" (Decode.maybe typeDecoder))

                    "TAlias" ->
                        Decode.map4 TAlias
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list (D.jsonPair Decode.string typeDecoder)))
                            (Decode.field "tipe" aliasTypeDecoder)

                    _ ->
                        Decode.fail ("Unknown Type's type: " ++ type_)
            )


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
        |> Serialize.variant2 TRecord (S.assocListDict compare Serialize.string fieldTypeCodec) (Serialize.maybe Serialize.string)
        |> Serialize.variant0 TUnit
        |> Serialize.variant3 TTuple (Serialize.lazy (\() -> typeCodec)) (Serialize.lazy (\() -> typeCodec)) (Serialize.maybe (Serialize.lazy (\() -> typeCodec)))
        |> Serialize.variant4 TAlias ModuleName.canonicalCodec Serialize.string (Serialize.list (Serialize.tuple Serialize.string (Serialize.lazy (\() -> typeCodec)))) aliasTypeCodec
        |> Serialize.finishCustomType


fieldTypeEncoder : FieldType -> Encode.Value
fieldTypeEncoder (FieldType index tipe) =
    Encode.object
        [ ( "type", Encode.string "FieldType" )
        , ( "index", Encode.int index )
        , ( "tipe", typeEncoder tipe )
        ]


fieldTypeDecoder : Decode.Decoder FieldType
fieldTypeDecoder =
    Decode.map2 FieldType
        (Decode.field "index" Decode.int)
        (Decode.field "tipe" typeDecoder)


fieldTypeCodec : Codec e FieldType
fieldTypeCodec =
    Serialize.customType
        (\fieldTypeCodecEncoder (FieldType index tipe) ->
            fieldTypeCodecEncoder index tipe
        )
        |> Serialize.variant2 FieldType Serialize.int (Serialize.lazy (\() -> typeCodec))
        |> Serialize.finishCustomType


aliasTypeEncoder : AliasType -> Encode.Value
aliasTypeEncoder aliasType =
    case aliasType of
        Holey tipe ->
            Encode.object
                [ ( "type", Encode.string "Holey" )
                , ( "tipe", typeEncoder tipe )
                ]

        Filled tipe ->
            Encode.object
                [ ( "type", Encode.string "Filled" )
                , ( "tipe", typeEncoder tipe )
                ]


aliasTypeDecoder : Decode.Decoder AliasType
aliasTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Holey" ->
                        Decode.map Holey
                            (Decode.field "tipe" typeDecoder)

                    "Filled" ->
                        Decode.map Filled
                            (Decode.field "tipe" typeDecoder)

                    _ ->
                        Decode.fail ("Unknown AliasType's type: " ++ type_)
            )


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


unionEncoder : Union -> Encode.Value
unionEncoder (Union vars ctors numAlts opts) =
    Encode.object
        [ ( "type", Encode.string "Union" )
        , ( "vars", Encode.list Encode.string vars )
        , ( "ctors", Encode.list ctorEncoder ctors )
        , ( "numAlts", Encode.int numAlts )
        , ( "opts", ctorOptsEncoder opts )
        ]


unionDecoder : Decode.Decoder Union
unionDecoder =
    Decode.map4 Union
        (Decode.field "vars" (Decode.list Decode.string))
        (Decode.field "ctors" (Decode.list ctorDecoder))
        (Decode.field "numAlts" Decode.int)
        (Decode.field "opts" ctorOptsDecoder)


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


ctorEncoder : Ctor -> Encode.Value
ctorEncoder (Ctor ctor index numArgs args) =
    Encode.object
        [ ( "type", Encode.string "Ctor" )
        , ( "ctor", Encode.string ctor )
        , ( "index", Index.zeroBasedEncoder index )
        , ( "numArgs", Encode.int numArgs )
        , ( "args", Encode.list typeEncoder args )
        ]


ctorDecoder : Decode.Decoder Ctor
ctorDecoder =
    Decode.map4 Ctor
        (Decode.field "ctor" Decode.string)
        (Decode.field "index" Index.zeroBasedDecoder)
        (Decode.field "numArgs" Decode.int)
        (Decode.field "args" (Decode.list typeDecoder))


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


ctorOptsEncoder : CtorOpts -> Encode.Value
ctorOptsEncoder ctorOpts =
    case ctorOpts of
        Normal ->
            Encode.string "Normal"

        Enum ->
            Encode.string "Enum"

        Unbox ->
            Encode.string "Unbox"


ctorOptsDecoder : Decode.Decoder CtorOpts
ctorOptsDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Normal" ->
                        Decode.succeed Normal

                    "Enum" ->
                        Decode.succeed Enum

                    "Unbox" ->
                        Decode.succeed Unbox

                    _ ->
                        Decode.fail ("Unknown CtorOpts: " ++ str)
            )


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


fieldUpdateEncoder : FieldUpdate -> Encode.Value
fieldUpdateEncoder (FieldUpdate fieldRegion expr) =
    Encode.object
        [ ( "type", Encode.string "FieldUpdate" )
        , ( "fieldRegion", A.regionEncoder fieldRegion )
        , ( "expr", exprEncoder expr )
        ]


fieldUpdateDecoder : Decode.Decoder FieldUpdate
fieldUpdateDecoder =
    Decode.map2 FieldUpdate
        (Decode.field "fieldRegion" A.regionDecoder)
        (Decode.field "expr" exprDecoder)


exprEncoder : Expr -> Encode.Value
exprEncoder =
    A.locatedEncoder expr_Encoder


exprDecoder : Decode.Decoder Expr
exprDecoder =
    A.locatedDecoder expr_Decoder


expr_Encoder : Expr_ -> Encode.Value
expr_Encoder expr_ =
    case expr_ of
        VarLocal name ->
            Encode.object
                [ ( "type", Encode.string "VarLocal" )
                , ( "name", Encode.string name )
                ]

        VarTopLevel home name ->
            Encode.object
                [ ( "type", Encode.string "VarTopLevel" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                ]

        VarKernel home name ->
            Encode.object
                [ ( "type", Encode.string "VarKernel" )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                ]

        VarForeign home name annotation ->
            Encode.object
                [ ( "type", Encode.string "VarForeign" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "annotation", annotationEncoder annotation )
                ]

        VarCtor opts home name index annotation ->
            Encode.object
                [ ( "type", Encode.string "VarCtor" )
                , ( "opts", ctorOptsEncoder opts )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "annotation", annotationEncoder annotation )
                ]

        VarDebug home name annotation ->
            Encode.object
                [ ( "type", Encode.string "VarDebug" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "annotation", annotationEncoder annotation )
                ]

        VarOperator op home name annotation ->
            Encode.object
                [ ( "type", Encode.string "VarOperator" )
                , ( "op", Encode.string op )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "annotation", annotationEncoder annotation )
                ]

        Chr chr ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "chr", Encode.string chr )
                ]

        Str str ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "str", Encode.string str )
                ]

        Int int ->
            Encode.object
                [ ( "type", Encode.string "Int" )
                , ( "int", Encode.int int )
                ]

        Float float ->
            Encode.object
                [ ( "type", Encode.string "Float" )
                , ( "float", Encode.float float )
                ]

        List entries ->
            Encode.object
                [ ( "type", Encode.string "List" )
                , ( "entries", Encode.list exprEncoder entries )
                ]

        Negate expr ->
            Encode.object
                [ ( "type", Encode.string "Negate" )
                , ( "expr", exprEncoder expr )
                ]

        Binop op home name annotation left right ->
            Encode.object
                [ ( "type", Encode.string "Binop" )
                , ( "op", Encode.string op )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "annotation", annotationEncoder annotation )
                , ( "left", exprEncoder left )
                , ( "right", exprEncoder right )
                ]

        Lambda args body ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "args", Encode.list patternEncoder args )
                , ( "body", exprEncoder body )
                ]

        Call func args ->
            Encode.object
                [ ( "type", Encode.string "Call" )
                , ( "func", exprEncoder func )
                , ( "args", Encode.list exprEncoder args )
                ]

        If branches finally ->
            Encode.object
                [ ( "type", Encode.string "If" )
                , ( "branches", Encode.list (E.jsonPair exprEncoder exprEncoder) branches )
                , ( "finally", exprEncoder finally )
                ]

        Let def body ->
            Encode.object
                [ ( "type", Encode.string "Let" )
                , ( "def", defEncoder def )
                , ( "body", exprEncoder body )
                ]

        LetRec defs body ->
            Encode.object
                [ ( "type", Encode.string "LetRec" )
                , ( "defs", Encode.list defEncoder defs )
                , ( "body", exprEncoder body )
                ]

        LetDestruct pattern expr body ->
            Encode.object
                [ ( "type", Encode.string "LetDestruct" )
                , ( "pattern", patternEncoder pattern )
                , ( "expr", exprEncoder expr )
                , ( "body", exprEncoder body )
                ]

        Case expr branches ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                , ( "expr", exprEncoder expr )
                , ( "branches", Encode.list caseBranchEncoder branches )
                ]

        Accessor field ->
            Encode.object
                [ ( "type", Encode.string "Accessor" )
                , ( "field", Encode.string field )
                ]

        Access record field ->
            Encode.object
                [ ( "type", Encode.string "Access" )
                , ( "record", exprEncoder record )
                , ( "field", A.locatedEncoder Encode.string field )
                ]

        Update name record updates ->
            Encode.object
                [ ( "type", Encode.string "Update" )
                , ( "name", Encode.string name )
                , ( "record", exprEncoder record )
                , ( "updates", E.assocListDict Encode.string fieldUpdateEncoder updates )
                ]

        Record fields ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "fields", E.assocListDict Encode.string exprEncoder fields )
                ]

        Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        Tuple a b maybeC ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", exprEncoder a )
                , ( "b", exprEncoder b )
                , ( "maybeC", E.maybe exprEncoder maybeC )
                ]

        Shader src types ->
            Encode.object
                [ ( "type", Encode.string "Shader" )
                , ( "src", Shader.sourceEncoder src )
                , ( "types", Shader.typesEncoder types )
                ]


expr_Decoder : Decode.Decoder Expr_
expr_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "VarLocal" ->
                        Decode.map VarLocal (Decode.field "name" Decode.string)

                    "VarTopLevel" ->
                        Decode.map2 VarTopLevel
                            (Decode.field "moduleName" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)

                    "VarKernel" ->
                        Decode.map2 VarKernel
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)

                    "VarForeign" ->
                        Decode.map3 VarForeign
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "annotation" annotationDecoder)

                    "VarCtor" ->
                        Decode.map5 VarCtor
                            (Decode.field "opts" ctorOptsDecoder)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "annotation" annotationDecoder)

                    "VarDebug" ->
                        Decode.map3 VarDebug
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "annotation" annotationDecoder)

                    "VarOperator" ->
                        Decode.map4 VarOperator
                            (Decode.field "op" Decode.string)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "annotation" annotationDecoder)

                    "Chr" ->
                        Decode.map Chr (Decode.field "chr" Decode.string)

                    "Str" ->
                        Decode.map Str (Decode.field "str" Decode.string)

                    "Int" ->
                        Decode.map Int (Decode.field "int" Decode.int)

                    "Float" ->
                        Decode.map Float (Decode.field "float" Decode.float)

                    "List" ->
                        Decode.map List (Decode.field "entries" (Decode.list exprDecoder))

                    "Negate" ->
                        Decode.map Negate (Decode.field "expr" exprDecoder)

                    "Binop" ->
                        Decode.map6 Binop
                            (Decode.field "op" Decode.string)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "annotation" annotationDecoder)
                            (Decode.field "left" exprDecoder)
                            (Decode.field "right" exprDecoder)

                    "Lambda" ->
                        Decode.map2 Lambda
                            (Decode.field "args" (Decode.list patternDecoder))
                            (Decode.field "body" exprDecoder)

                    "Call" ->
                        Decode.map2 Call
                            (Decode.field "func" exprDecoder)
                            (Decode.field "args" (Decode.list exprDecoder))

                    "If" ->
                        Decode.map2 If
                            (Decode.field "branches" (Decode.list (D.jsonPair exprDecoder exprDecoder)))
                            (Decode.field "finally" exprDecoder)

                    "Let" ->
                        Decode.map2 Let
                            (Decode.field "def" defDecoder)
                            (Decode.field "body" exprDecoder)

                    "LetRec" ->
                        Decode.map2 LetRec
                            (Decode.field "defs" (Decode.list defDecoder))
                            (Decode.field "body" exprDecoder)

                    "LetDestruct" ->
                        Decode.map3 LetDestruct
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "body" exprDecoder)

                    "Case" ->
                        Decode.map2 Case
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "branches" (Decode.list caseBranchDecoder))

                    "Accessor" ->
                        Decode.map Accessor (Decode.field "field" Decode.string)

                    "Access" ->
                        Decode.map2 Access
                            (Decode.field "record" exprDecoder)
                            (Decode.field "field" (A.locatedDecoder Decode.string))

                    "Update" ->
                        Decode.map3 Update
                            (Decode.field "name" Decode.string)
                            (Decode.field "record" exprDecoder)
                            (Decode.field "updates" (D.assocListDict compare Decode.string fieldUpdateDecoder))

                    "Record" ->
                        Decode.map Record
                            (Decode.field "fields" (D.assocListDict compare Decode.string exprDecoder))

                    "Unit" ->
                        Decode.succeed Unit

                    "Tuple" ->
                        Decode.map3 Tuple
                            (Decode.field "a" exprDecoder)
                            (Decode.field "b" exprDecoder)
                            (Decode.field "maybeC" (Decode.maybe exprDecoder))

                    "Shader" ->
                        Decode.map2 Shader
                            (Decode.field "src" Shader.sourceDecoder)
                            (Decode.field "types" Shader.typesDecoder)

                    _ ->
                        Decode.fail ("Unknown Expr_'s type: " ++ type_)
            )


patternEncoder : Pattern -> Encode.Value
patternEncoder =
    A.locatedEncoder pattern_Encoder


patternDecoder : Decode.Decoder Pattern
patternDecoder =
    A.locatedDecoder pattern_Decoder


pattern_Encoder : Pattern_ -> Encode.Value
pattern_Encoder pattern_ =
    case pattern_ of
        PAnything ->
            Encode.object
                [ ( "type", Encode.string "PAnything" )
                ]

        PVar name ->
            Encode.object
                [ ( "type", Encode.string "PVar" )
                , ( "name", Encode.string name )
                ]

        PRecord names ->
            Encode.object
                [ ( "type", Encode.string "PRecord" )
                , ( "names", Encode.list Encode.string names )
                ]

        PAlias pattern name ->
            Encode.object
                [ ( "type", Encode.string "PAlias" )
                , ( "pattern", patternEncoder pattern )
                , ( "name", Encode.string name )
                ]

        PUnit ->
            Encode.object
                [ ( "type", Encode.string "PUnit" )
                ]

        PTuple pattern1 pattern2 maybePattern3 ->
            Encode.object
                [ ( "type", Encode.string "PTuple" )
                , ( "pattern1", patternEncoder pattern1 )
                , ( "pattern2", patternEncoder pattern2 )
                , ( "pattern3", E.maybe patternEncoder maybePattern3 )
                ]

        PList patterns ->
            Encode.object
                [ ( "type", Encode.string "PList" )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        PCons pattern1 pattern2 ->
            Encode.object
                [ ( "type", Encode.string "PCons" )
                , ( "pattern1", patternEncoder pattern1 )
                , ( "pattern2", patternEncoder pattern2 )
                ]

        PBool union bool ->
            Encode.object
                [ ( "type", Encode.string "PBool" )
                , ( "union", unionEncoder union )
                , ( "bool", Encode.bool bool )
                ]

        PChr chr ->
            Encode.object
                [ ( "type", Encode.string "PChr" )
                , ( "chr", Encode.string chr )
                ]

        PStr str ->
            Encode.object
                [ ( "type", Encode.string "PStr" )
                , ( "str", Encode.string str )
                ]

        PInt int ->
            Encode.object
                [ ( "type", Encode.string "PInt" )
                , ( "int", Encode.int int )
                ]

        PCtor { home, type_, union, name, index, args } ->
            Encode.object
                [ ( "type", Encode.string "PCtor" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "type_", Encode.string type_ )
                , ( "union", unionEncoder union )
                , ( "name", Encode.string name )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "args", Encode.list patternCtorArgEncoder args )
                ]


pattern_Decoder : Decode.Decoder Pattern_
pattern_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\patternType ->
                case patternType of
                    "PAnything" ->
                        Decode.succeed PAnything

                    "PVar" ->
                        Decode.map PVar
                            (Decode.field "name" Decode.string)

                    "PRecord" ->
                        Decode.map PRecord
                            (Decode.field "names" (Decode.list Decode.string))

                    "PAlias" ->
                        Decode.map2 PAlias
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "name" Decode.string)

                    "PUnit" ->
                        Decode.succeed PUnit

                    "PTuple" ->
                        Decode.map3 PTuple
                            (Decode.field "pattern1" patternDecoder)
                            (Decode.field "pattern2" patternDecoder)
                            (Decode.field "pattern3" (Decode.maybe patternDecoder))

                    "PList" ->
                        Decode.map PList
                            (Decode.field "patterns" (Decode.list patternDecoder))

                    "PCons" ->
                        Decode.map2 PCons
                            (Decode.field "pattern1" patternDecoder)
                            (Decode.field "pattern2" patternDecoder)

                    "PBool" ->
                        Decode.map2 PBool
                            (Decode.field "union" unionDecoder)
                            (Decode.field "bool" Decode.bool)

                    "PChr" ->
                        Decode.map PChr (Decode.field "chr" Decode.string)

                    "PStr" ->
                        Decode.map PStr (Decode.field "str" Decode.string)

                    "PInt" ->
                        Decode.map PInt (Decode.field "int" Decode.int)

                    "PCtor" ->
                        Decode.map6
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
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "type_" Decode.string)
                            (Decode.field "union" unionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "args" (Decode.list patternCtorArgDecoder))

                    _ ->
                        Decode.fail ("Unknown Pattern_'s type: " ++ patternType)
            )


patternCtorArgEncoder : PatternCtorArg -> Encode.Value
patternCtorArgEncoder (PatternCtorArg index srcType pattern) =
    Encode.object
        [ ( "type", Encode.string "PatternCtorArg" )
        , ( "index", Index.zeroBasedEncoder index )
        , ( "srcType", typeEncoder srcType )
        , ( "pattern", patternEncoder pattern )
        ]


patternCtorArgDecoder : Decode.Decoder PatternCtorArg
patternCtorArgDecoder =
    Decode.map3 PatternCtorArg
        (Decode.field "index" Index.zeroBasedDecoder)
        (Decode.field "srcType" typeDecoder)
        (Decode.field "pattern" patternDecoder)


defEncoder : Def -> Encode.Value
defEncoder def =
    case def of
        Def name args expr ->
            Encode.object
                [ ( "type", Encode.string "Def" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "args", Encode.list patternEncoder args )
                , ( "expr", exprEncoder expr )
                ]

        TypedDef name freeVars typedArgs expr srcResultType ->
            Encode.object
                [ ( "type", Encode.string "TypedDef" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "freeVars", freeVarsEncoder freeVars )
                , ( "typedArgs", Encode.list (E.jsonPair patternEncoder typeEncoder) typedArgs )
                , ( "expr", exprEncoder expr )
                , ( "srcResultType", typeEncoder srcResultType )
                ]


defDecoder : Decode.Decoder Def
defDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Def" ->
                        Decode.map3 Def
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "args" (Decode.list patternDecoder))
                            (Decode.field "expr" exprDecoder)

                    "TypedDef" ->
                        Decode.map5 TypedDef
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "freeVars" freeVarsDecoder)
                            (Decode.field "typedArgs" (Decode.list (D.jsonPair patternDecoder typeDecoder)))
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "srcResultType" typeDecoder)

                    _ ->
                        Decode.fail ("Unknown Def's type: " ++ type_)
            )


caseBranchEncoder : CaseBranch -> Encode.Value
caseBranchEncoder (CaseBranch pattern expr) =
    Encode.object
        [ ( "type", Encode.string "CaseBranch" )
        , ( "pattern", patternEncoder pattern )
        , ( "expr", exprEncoder expr )
        ]


caseBranchDecoder : Decode.Decoder CaseBranch
caseBranchDecoder =
    Decode.map2 CaseBranch
        (Decode.field "pattern" patternDecoder)
        (Decode.field "expr" exprDecoder)
