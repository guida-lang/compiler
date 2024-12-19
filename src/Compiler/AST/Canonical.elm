module Compiler.AST.Canonical exposing
    ( Binop(..)
    , Effects(..)
    , Export(..)
    , Exports(..)
    , Manager(..)
    , Module(..)
    , Port(..)
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

import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- TYPES
-- NOTE: The Word16 marks the source order, but it may not be available
-- for every canonical type. For example, if the canonical type is inferred
-- the orders will all be zeros.


fieldsToList : Dict String T.CDN_Name T.CASTC_FieldType -> List ( T.CDN_Name, T.CASTC_Type )
fieldsToList fields =
    let
        getIndex : ( a, T.CASTC_FieldType ) -> Int
        getIndex ( _, T.CASTC_FieldType index _ ) =
            index

        dropIndex : ( a, T.CASTC_FieldType ) -> ( a, T.CASTC_Type )
        dropIndex ( name, T.CASTC_FieldType _ tipe ) =
            ( name, tipe )
    in
    Dict.toList compare fields
        |> List.sortBy getIndex
        |> List.map dropIndex



-- MODULES


type Module
    = Module T.CEMN_Canonical Exports T.CASTS_Docs T.CASTC_Decls (Dict String T.CDN_Name T.CASTC_Union) (Dict String T.CDN_Name T.CASTC_Alias) (Dict String T.CDN_Name Binop) Effects


type Binop
    = Binop_ T.CASTUB_Associativity T.CASTUB_Precedence T.CDN_Name



-- EXPORTS


type Exports
    = ExportEverything T.CRA_Region
    | Export (Dict String T.CDN_Name (T.CRA_Located Export))


type Export
    = ExportValue
    | ExportBinop
    | ExportAlias
    | ExportUnionOpen
    | ExportUnionClosed
    | ExportPort


type Effects
    = NoEffects
    | Ports (Dict String T.CDN_Name Port)
    | Manager T.CRA_Region T.CRA_Region T.CRA_Region Manager


type Port
    = Incoming
        { freeVars : T.CASTC_FreeVars
        , payload : T.CASTC_Type
        , func : T.CASTC_Type
        }
    | Outgoing
        { freeVars : T.CASTC_FreeVars
        , payload : T.CASTC_Type
        , func : T.CASTC_Type
        }


type Manager
    = Cmd T.CDN_Name
    | Sub T.CDN_Name
    | Fx T.CDN_Name T.CDN_Name



-- ENCODERS and DECODERS


annotationEncoder : T.CASTC_Annotation -> Encode.Value
annotationEncoder (T.CASTC_Forall freeVars tipe) =
    Encode.object
        [ ( "type", Encode.string "Forall" )
        , ( "freeVars", freeVarsEncoder freeVars )
        , ( "tipe", typeEncoder tipe )
        ]


annotationDecoder : Decode.Decoder T.CASTC_Annotation
annotationDecoder =
    Decode.map2 T.CASTC_Forall
        (Decode.field "freeVars" freeVarsDecoder)
        (Decode.field "tipe" typeDecoder)


freeVarsEncoder : T.CASTC_FreeVars -> Encode.Value
freeVarsEncoder =
    E.assocListDict compare Encode.string (\_ -> Encode.object [])


freeVarsDecoder : Decode.Decoder T.CASTC_FreeVars
freeVarsDecoder =
    D.assocListDict identity Decode.string (Decode.succeed ())


aliasEncoder : T.CASTC_Alias -> Encode.Value
aliasEncoder (T.CASTC_Alias vars tipe) =
    Encode.object
        [ ( "vars", Encode.list Encode.string vars )
        , ( "tipe", typeEncoder tipe )
        ]


aliasDecoder : Decode.Decoder T.CASTC_Alias
aliasDecoder =
    Decode.map2 T.CASTC_Alias
        (Decode.field "vars" (Decode.list Decode.string))
        (Decode.field "tipe" typeDecoder)


typeEncoder : T.CASTC_Type -> Encode.Value
typeEncoder type_ =
    case type_ of
        T.CASTC_TLambda a b ->
            Encode.object
                [ ( "type", Encode.string "TLambda" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                ]

        T.CASTC_TVar name ->
            Encode.object
                [ ( "type", Encode.string "TVar" )
                , ( "name", Encode.string name )
                ]

        T.CASTC_TType home name args ->
            Encode.object
                [ ( "type", Encode.string "TType" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        T.CASTC_TRecord fields ext ->
            Encode.object
                [ ( "type", Encode.string "TRecord" )
                , ( "fields", E.assocListDict compare Encode.string fieldTypeEncoder fields )
                , ( "ext", E.maybe Encode.string ext )
                ]

        T.CASTC_TUnit ->
            Encode.object
                [ ( "type", Encode.string "TUnit" )
                ]

        T.CASTC_TTuple a b maybeC ->
            Encode.object
                [ ( "type", Encode.string "TTuple" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                , ( "maybeC", E.maybe typeEncoder maybeC )
                ]

        T.CASTC_TAlias home name args tipe ->
            Encode.object
                [ ( "type", Encode.string "TAlias" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list (E.jsonPair Encode.string typeEncoder) args )
                , ( "tipe", aliasTypeEncoder tipe )
                ]


typeDecoder : Decode.Decoder T.CASTC_Type
typeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TLambda" ->
                        Decode.map2 T.CASTC_TLambda
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)

                    "TVar" ->
                        Decode.map T.CASTC_TVar
                            (Decode.field "name" Decode.string)

                    "TType" ->
                        Decode.map3 T.CASTC_TType
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "TRecord" ->
                        Decode.map2 T.CASTC_TRecord
                            (Decode.field "fields" (D.assocListDict identity Decode.string fieldTypeDecoder))
                            (Decode.field "ext" (Decode.maybe Decode.string))

                    "TUnit" ->
                        Decode.succeed T.CASTC_TUnit

                    "TTuple" ->
                        Decode.map3 T.CASTC_TTuple
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)
                            (Decode.field "maybeC" (Decode.maybe typeDecoder))

                    "TAlias" ->
                        Decode.map4 T.CASTC_TAlias
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list (D.jsonPair Decode.string typeDecoder)))
                            (Decode.field "tipe" aliasTypeDecoder)

                    _ ->
                        Decode.fail ("Unknown Type's type: " ++ type_)
            )


fieldTypeEncoder : T.CASTC_FieldType -> Encode.Value
fieldTypeEncoder (T.CASTC_FieldType index tipe) =
    Encode.object
        [ ( "type", Encode.string "FieldType" )
        , ( "index", Encode.int index )
        , ( "tipe", typeEncoder tipe )
        ]


aliasTypeEncoder : T.CASTC_AliasType -> Encode.Value
aliasTypeEncoder aliasType =
    case aliasType of
        T.CASTC_Holey tipe ->
            Encode.object
                [ ( "type", Encode.string "Holey" )
                , ( "tipe", typeEncoder tipe )
                ]

        T.CASTC_Filled tipe ->
            Encode.object
                [ ( "type", Encode.string "Filled" )
                , ( "tipe", typeEncoder tipe )
                ]


fieldTypeDecoder : Decode.Decoder T.CASTC_FieldType
fieldTypeDecoder =
    Decode.map2 T.CASTC_FieldType
        (Decode.field "index" Decode.int)
        (Decode.field "tipe" typeDecoder)


aliasTypeDecoder : Decode.Decoder T.CASTC_AliasType
aliasTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Holey" ->
                        Decode.map T.CASTC_Holey
                            (Decode.field "tipe" typeDecoder)

                    "Filled" ->
                        Decode.map T.CASTC_Filled
                            (Decode.field "tipe" typeDecoder)

                    _ ->
                        Decode.fail ("Unknown AliasType's type: " ++ type_)
            )


unionEncoder : T.CASTC_Union -> Encode.Value
unionEncoder (T.CASTC_Union vars ctors numAlts opts) =
    Encode.object
        [ ( "type", Encode.string "Union" )
        , ( "vars", Encode.list Encode.string vars )
        , ( "ctors", Encode.list ctorEncoder ctors )
        , ( "numAlts", Encode.int numAlts )
        , ( "opts", ctorOptsEncoder opts )
        ]


unionDecoder : Decode.Decoder T.CASTC_Union
unionDecoder =
    Decode.map4 T.CASTC_Union
        (Decode.field "vars" (Decode.list Decode.string))
        (Decode.field "ctors" (Decode.list ctorDecoder))
        (Decode.field "numAlts" Decode.int)
        (Decode.field "opts" ctorOptsDecoder)


ctorEncoder : T.CASTC_Ctor -> Encode.Value
ctorEncoder (T.CASTC_Ctor ctor index numArgs args) =
    Encode.object
        [ ( "type", Encode.string "Ctor" )
        , ( "ctor", Encode.string ctor )
        , ( "index", Index.zeroBasedEncoder index )
        , ( "numArgs", Encode.int numArgs )
        , ( "args", Encode.list typeEncoder args )
        ]


ctorDecoder : Decode.Decoder T.CASTC_Ctor
ctorDecoder =
    Decode.map4 T.CASTC_Ctor
        (Decode.field "ctor" Decode.string)
        (Decode.field "index" Index.zeroBasedDecoder)
        (Decode.field "numArgs" Decode.int)
        (Decode.field "args" (Decode.list typeDecoder))


ctorOptsEncoder : T.CASTC_CtorOpts -> Encode.Value
ctorOptsEncoder ctorOpts =
    case ctorOpts of
        T.CASTC_Normal ->
            Encode.string "Normal"

        T.CASTC_Enum ->
            Encode.string "Enum"

        T.CASTC_Unbox ->
            Encode.string "Unbox"


ctorOptsDecoder : Decode.Decoder T.CASTC_CtorOpts
ctorOptsDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Normal" ->
                        Decode.succeed T.CASTC_Normal

                    "Enum" ->
                        Decode.succeed T.CASTC_Enum

                    "Unbox" ->
                        Decode.succeed T.CASTC_Unbox

                    _ ->
                        Decode.fail ("Unknown CtorOpts: " ++ str)
            )


fieldUpdateEncoder : T.CASTC_FieldUpdate -> Encode.Value
fieldUpdateEncoder (T.CASTC_FieldUpdate fieldRegion expr) =
    Encode.object
        [ ( "type", Encode.string "FieldUpdate" )
        , ( "fieldRegion", A.regionEncoder fieldRegion )
        , ( "expr", exprEncoder expr )
        ]


fieldUpdateDecoder : Decode.Decoder T.CASTC_FieldUpdate
fieldUpdateDecoder =
    Decode.map2 T.CASTC_FieldUpdate
        (Decode.field "fieldRegion" A.regionDecoder)
        (Decode.field "expr" exprDecoder)


exprEncoder : T.CASTC_Expr -> Encode.Value
exprEncoder =
    A.locatedEncoder expr_Encoder


exprDecoder : Decode.Decoder T.CASTC_Expr
exprDecoder =
    A.locatedDecoder expr_Decoder


expr_Encoder : T.CASTC_Expr_ -> Encode.Value
expr_Encoder expr_ =
    case expr_ of
        T.CASTC_VarLocal name ->
            Encode.object
                [ ( "type", Encode.string "VarLocal" )
                , ( "name", Encode.string name )
                ]

        T.CASTC_VarTopLevel home name ->
            Encode.object
                [ ( "type", Encode.string "VarTopLevel" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                ]

        T.CASTC_VarKernel home name ->
            Encode.object
                [ ( "type", Encode.string "VarKernel" )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                ]

        T.CASTC_VarForeign home name annotation ->
            Encode.object
                [ ( "type", Encode.string "VarForeign" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "annotation", annotationEncoder annotation )
                ]

        T.CASTC_VarCtor opts home name index annotation ->
            Encode.object
                [ ( "type", Encode.string "VarCtor" )
                , ( "opts", ctorOptsEncoder opts )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "annotation", annotationEncoder annotation )
                ]

        T.CASTC_VarDebug home name annotation ->
            Encode.object
                [ ( "type", Encode.string "VarDebug" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "annotation", annotationEncoder annotation )
                ]

        T.CASTC_VarOperator op home name annotation ->
            Encode.object
                [ ( "type", Encode.string "VarOperator" )
                , ( "op", Encode.string op )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "annotation", annotationEncoder annotation )
                ]

        T.CASTC_Chr chr ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "chr", Encode.string chr )
                ]

        T.CASTC_Str str ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "str", Encode.string str )
                ]

        T.CASTC_Int int ->
            Encode.object
                [ ( "type", Encode.string "Int" )
                , ( "int", Encode.int int )
                ]

        T.CASTC_Float float ->
            Encode.object
                [ ( "type", Encode.string "Float" )
                , ( "float", Encode.float float )
                ]

        T.CASTC_List entries ->
            Encode.object
                [ ( "type", Encode.string "List" )
                , ( "entries", Encode.list exprEncoder entries )
                ]

        T.CASTC_Negate expr ->
            Encode.object
                [ ( "type", Encode.string "Negate" )
                , ( "expr", exprEncoder expr )
                ]

        T.CASTC_Binop op home name annotation left right ->
            Encode.object
                [ ( "type", Encode.string "Binop" )
                , ( "op", Encode.string op )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "annotation", annotationEncoder annotation )
                , ( "left", exprEncoder left )
                , ( "right", exprEncoder right )
                ]

        T.CASTC_Lambda args body ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "args", Encode.list patternEncoder args )
                , ( "body", exprEncoder body )
                ]

        T.CASTC_Call func args ->
            Encode.object
                [ ( "type", Encode.string "Call" )
                , ( "func", exprEncoder func )
                , ( "args", Encode.list exprEncoder args )
                ]

        T.CASTC_If branches finally ->
            Encode.object
                [ ( "type", Encode.string "If" )
                , ( "branches", Encode.list (E.jsonPair exprEncoder exprEncoder) branches )
                , ( "finally", exprEncoder finally )
                ]

        T.CASTC_Let def body ->
            Encode.object
                [ ( "type", Encode.string "Let" )
                , ( "def", defEncoder def )
                , ( "body", exprEncoder body )
                ]

        T.CASTC_LetRec defs body ->
            Encode.object
                [ ( "type", Encode.string "LetRec" )
                , ( "defs", Encode.list defEncoder defs )
                , ( "body", exprEncoder body )
                ]

        T.CASTC_LetDestruct pattern expr body ->
            Encode.object
                [ ( "type", Encode.string "LetDestruct" )
                , ( "pattern", patternEncoder pattern )
                , ( "expr", exprEncoder expr )
                , ( "body", exprEncoder body )
                ]

        T.CASTC_Case expr branches ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                , ( "expr", exprEncoder expr )
                , ( "branches", Encode.list caseBranchEncoder branches )
                ]

        T.CASTC_Accessor field ->
            Encode.object
                [ ( "type", Encode.string "Accessor" )
                , ( "field", Encode.string field )
                ]

        T.CASTC_Access record field ->
            Encode.object
                [ ( "type", Encode.string "Access" )
                , ( "record", exprEncoder record )
                , ( "field", A.locatedEncoder Encode.string field )
                ]

        T.CASTC_Update name record updates ->
            Encode.object
                [ ( "type", Encode.string "Update" )
                , ( "name", Encode.string name )
                , ( "record", exprEncoder record )
                , ( "updates", E.assocListDict compare Encode.string fieldUpdateEncoder updates )
                ]

        T.CASTC_Record fields ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "fields", E.assocListDict compare Encode.string exprEncoder fields )
                ]

        T.CASTC_Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        T.CASTC_Tuple a b maybeC ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", exprEncoder a )
                , ( "b", exprEncoder b )
                , ( "maybeC", E.maybe exprEncoder maybeC )
                ]

        T.CASTC_Shader src types ->
            Encode.object
                [ ( "type", Encode.string "Shader" )
                , ( "src", Shader.sourceEncoder src )
                , ( "types", Shader.typesEncoder types )
                ]


expr_Decoder : Decode.Decoder T.CASTC_Expr_
expr_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "VarLocal" ->
                        Decode.map T.CASTC_VarLocal (Decode.field "name" Decode.string)

                    "VarTopLevel" ->
                        Decode.map2 T.CASTC_VarTopLevel
                            (Decode.field "moduleName" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)

                    "VarKernel" ->
                        Decode.map2 T.CASTC_VarKernel
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)

                    "VarForeign" ->
                        Decode.map3 T.CASTC_VarForeign
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "annotation" annotationDecoder)

                    "VarCtor" ->
                        Decode.map5 T.CASTC_VarCtor
                            (Decode.field "opts" ctorOptsDecoder)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "annotation" annotationDecoder)

                    "VarDebug" ->
                        Decode.map3 T.CASTC_VarDebug
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "annotation" annotationDecoder)

                    "VarOperator" ->
                        Decode.map4 T.CASTC_VarOperator
                            (Decode.field "op" Decode.string)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "annotation" annotationDecoder)

                    "Chr" ->
                        Decode.map T.CASTC_Chr (Decode.field "chr" Decode.string)

                    "Str" ->
                        Decode.map T.CASTC_Str (Decode.field "str" Decode.string)

                    "Int" ->
                        Decode.map T.CASTC_Int (Decode.field "int" Decode.int)

                    "Float" ->
                        Decode.map T.CASTC_Float (Decode.field "float" Decode.float)

                    "List" ->
                        Decode.map T.CASTC_List (Decode.field "entries" (Decode.list exprDecoder))

                    "Negate" ->
                        Decode.map T.CASTC_Negate (Decode.field "expr" exprDecoder)

                    "Binop" ->
                        Decode.map6 T.CASTC_Binop
                            (Decode.field "op" Decode.string)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "annotation" annotationDecoder)
                            (Decode.field "left" exprDecoder)
                            (Decode.field "right" exprDecoder)

                    "Lambda" ->
                        Decode.map2 T.CASTC_Lambda
                            (Decode.field "args" (Decode.list patternDecoder))
                            (Decode.field "body" exprDecoder)

                    "Call" ->
                        Decode.map2 T.CASTC_Call
                            (Decode.field "func" exprDecoder)
                            (Decode.field "args" (Decode.list exprDecoder))

                    "If" ->
                        Decode.map2 T.CASTC_If
                            (Decode.field "branches" (Decode.list (D.jsonPair exprDecoder exprDecoder)))
                            (Decode.field "finally" exprDecoder)

                    "Let" ->
                        Decode.map2 T.CASTC_Let
                            (Decode.field "def" defDecoder)
                            (Decode.field "body" exprDecoder)

                    "LetRec" ->
                        Decode.map2 T.CASTC_LetRec
                            (Decode.field "defs" (Decode.list defDecoder))
                            (Decode.field "body" exprDecoder)

                    "LetDestruct" ->
                        Decode.map3 T.CASTC_LetDestruct
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "body" exprDecoder)

                    "Case" ->
                        Decode.map2 T.CASTC_Case
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "branches" (Decode.list caseBranchDecoder))

                    "Accessor" ->
                        Decode.map T.CASTC_Accessor (Decode.field "field" Decode.string)

                    "Access" ->
                        Decode.map2 T.CASTC_Access
                            (Decode.field "record" exprDecoder)
                            (Decode.field "field" (A.locatedDecoder Decode.string))

                    "Update" ->
                        Decode.map3 T.CASTC_Update
                            (Decode.field "name" Decode.string)
                            (Decode.field "record" exprDecoder)
                            (Decode.field "updates" (D.assocListDict identity Decode.string fieldUpdateDecoder))

                    "Record" ->
                        Decode.map T.CASTC_Record
                            (Decode.field "fields" (D.assocListDict identity Decode.string exprDecoder))

                    "Unit" ->
                        Decode.succeed T.CASTC_Unit

                    "Tuple" ->
                        Decode.map3 T.CASTC_Tuple
                            (Decode.field "a" exprDecoder)
                            (Decode.field "b" exprDecoder)
                            (Decode.field "maybeC" (Decode.maybe exprDecoder))

                    "Shader" ->
                        Decode.map2 T.CASTC_Shader
                            (Decode.field "src" Shader.sourceDecoder)
                            (Decode.field "types" Shader.typesDecoder)

                    _ ->
                        Decode.fail ("Unknown Expr_'s type: " ++ type_)
            )


patternEncoder : T.CASTC_Pattern -> Encode.Value
patternEncoder =
    A.locatedEncoder pattern_Encoder


patternDecoder : Decode.Decoder T.CASTC_Pattern
patternDecoder =
    A.locatedDecoder pattern_Decoder


pattern_Encoder : T.CASTC_Pattern_ -> Encode.Value
pattern_Encoder pattern_ =
    case pattern_ of
        T.CASTC_PAnything ->
            Encode.object
                [ ( "type", Encode.string "PAnything" )
                ]

        T.CASTC_PVar name ->
            Encode.object
                [ ( "type", Encode.string "PVar" )
                , ( "name", Encode.string name )
                ]

        T.CASTC_PRecord names ->
            Encode.object
                [ ( "type", Encode.string "PRecord" )
                , ( "names", Encode.list Encode.string names )
                ]

        T.CASTC_PAlias pattern name ->
            Encode.object
                [ ( "type", Encode.string "PAlias" )
                , ( "pattern", patternEncoder pattern )
                , ( "name", Encode.string name )
                ]

        T.CASTC_PUnit ->
            Encode.object
                [ ( "type", Encode.string "PUnit" )
                ]

        T.CASTC_PTuple pattern1 pattern2 maybePattern3 ->
            Encode.object
                [ ( "type", Encode.string "PTuple" )
                , ( "pattern1", patternEncoder pattern1 )
                , ( "pattern2", patternEncoder pattern2 )
                , ( "pattern3", E.maybe patternEncoder maybePattern3 )
                ]

        T.CASTC_PList patterns ->
            Encode.object
                [ ( "type", Encode.string "PList" )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        T.CASTC_PCons pattern1 pattern2 ->
            Encode.object
                [ ( "type", Encode.string "PCons" )
                , ( "pattern1", patternEncoder pattern1 )
                , ( "pattern2", patternEncoder pattern2 )
                ]

        T.CASTC_PBool union bool ->
            Encode.object
                [ ( "type", Encode.string "PBool" )
                , ( "union", unionEncoder union )
                , ( "bool", Encode.bool bool )
                ]

        T.CASTC_PChr chr ->
            Encode.object
                [ ( "type", Encode.string "PChr" )
                , ( "chr", Encode.string chr )
                ]

        T.CASTC_PStr str ->
            Encode.object
                [ ( "type", Encode.string "PStr" )
                , ( "str", Encode.string str )
                ]

        T.CASTC_PInt int ->
            Encode.object
                [ ( "type", Encode.string "PInt" )
                , ( "int", Encode.int int )
                ]

        T.CASTC_PCtor { home, type_, union, name, index, args } ->
            Encode.object
                [ ( "type", Encode.string "PCtor" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "type_", Encode.string type_ )
                , ( "union", unionEncoder union )
                , ( "name", Encode.string name )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "args", Encode.list patternCtorArgEncoder args )
                ]


pattern_Decoder : Decode.Decoder T.CASTC_Pattern_
pattern_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\patternType ->
                case patternType of
                    "PAnything" ->
                        Decode.succeed T.CASTC_PAnything

                    "PVar" ->
                        Decode.map T.CASTC_PVar
                            (Decode.field "name" Decode.string)

                    "PRecord" ->
                        Decode.map T.CASTC_PRecord
                            (Decode.field "names" (Decode.list Decode.string))

                    "PAlias" ->
                        Decode.map2 T.CASTC_PAlias
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "name" Decode.string)

                    "PUnit" ->
                        Decode.succeed T.CASTC_PUnit

                    "PTuple" ->
                        Decode.map3 T.CASTC_PTuple
                            (Decode.field "pattern1" patternDecoder)
                            (Decode.field "pattern2" patternDecoder)
                            (Decode.field "pattern3" (Decode.maybe patternDecoder))

                    "PList" ->
                        Decode.map T.CASTC_PList
                            (Decode.field "patterns" (Decode.list patternDecoder))

                    "PCons" ->
                        Decode.map2 T.CASTC_PCons
                            (Decode.field "pattern1" patternDecoder)
                            (Decode.field "pattern2" patternDecoder)

                    "PBool" ->
                        Decode.map2 T.CASTC_PBool
                            (Decode.field "union" unionDecoder)
                            (Decode.field "bool" Decode.bool)

                    "PChr" ->
                        Decode.map T.CASTC_PChr (Decode.field "chr" Decode.string)

                    "PStr" ->
                        Decode.map T.CASTC_PStr (Decode.field "str" Decode.string)

                    "PInt" ->
                        Decode.map T.CASTC_PInt (Decode.field "int" Decode.int)

                    "PCtor" ->
                        Decode.map6
                            (\home type_ union name index args ->
                                T.CASTC_PCtor
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


patternCtorArgEncoder : T.CASTC_PatternCtorArg -> Encode.Value
patternCtorArgEncoder (T.CASTC_PatternCtorArg index srcType pattern) =
    Encode.object
        [ ( "type", Encode.string "PatternCtorArg" )
        , ( "index", Index.zeroBasedEncoder index )
        , ( "srcType", typeEncoder srcType )
        , ( "pattern", patternEncoder pattern )
        ]


patternCtorArgDecoder : Decode.Decoder T.CASTC_PatternCtorArg
patternCtorArgDecoder =
    Decode.map3 T.CASTC_PatternCtorArg
        (Decode.field "index" Index.zeroBasedDecoder)
        (Decode.field "srcType" typeDecoder)
        (Decode.field "pattern" patternDecoder)


defEncoder : T.CASTC_Def -> Encode.Value
defEncoder def =
    case def of
        T.CASTC_Def name args expr ->
            Encode.object
                [ ( "type", Encode.string "Def" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "args", Encode.list patternEncoder args )
                , ( "expr", exprEncoder expr )
                ]

        T.CASTC_TypedDef name freeVars typedArgs expr srcResultType ->
            Encode.object
                [ ( "type", Encode.string "TypedDef" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "freeVars", freeVarsEncoder freeVars )
                , ( "typedArgs", Encode.list (E.jsonPair patternEncoder typeEncoder) typedArgs )
                , ( "expr", exprEncoder expr )
                , ( "srcResultType", typeEncoder srcResultType )
                ]


defDecoder : Decode.Decoder T.CASTC_Def
defDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Def" ->
                        Decode.map3 T.CASTC_Def
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "args" (Decode.list patternDecoder))
                            (Decode.field "expr" exprDecoder)

                    "TypedDef" ->
                        Decode.map5 T.CASTC_TypedDef
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "freeVars" freeVarsDecoder)
                            (Decode.field "typedArgs" (Decode.list (D.jsonPair patternDecoder typeDecoder)))
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "srcResultType" typeDecoder)

                    _ ->
                        Decode.fail ("Unknown Def's type: " ++ type_)
            )


caseBranchEncoder : T.CASTC_CaseBranch -> Encode.Value
caseBranchEncoder (T.CASTC_CaseBranch pattern expr) =
    Encode.object
        [ ( "type", Encode.string "CaseBranch" )
        , ( "pattern", patternEncoder pattern )
        , ( "expr", exprEncoder expr )
        ]


caseBranchDecoder : Decode.Decoder T.CASTC_CaseBranch
caseBranchDecoder =
    Decode.map2 T.CASTC_CaseBranch
        (Decode.field "pattern" patternDecoder)
        (Decode.field "expr" exprDecoder)
