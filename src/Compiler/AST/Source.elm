module Compiler.AST.Source exposing
    ( CASTS_Alias(..)
    , CASTS_Comment(..)
    , CASTS_Def(..)
    , CASTS_Docs(..)
    , CASTS_Effects(..)
    , CASTS_Exposed(..)
    , CASTS_Exposing(..)
    , CASTS_Expr
    , CASTS_Expr_(..)
    , CASTS_Import(..)
    , CASTS_Infix(..)
    , CASTS_Manager(..)
    , CASTS_Module(..)
    , CASTS_Pattern
    , CASTS_Pattern_(..)
    , CASTS_Port(..)
    , CASTS_Privacy(..)
    , CASTS_Type
    , CASTS_Type_(..)
    , CASTS_Union(..)
    , CASTS_Value(..)
    , CASTS_VarType(..)
    , getImportName
    , getName
    , moduleDecoder
    , moduleEncoder
    , typeDecoder
    , typeEncoder
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Name as Name exposing (CDN_Name)
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Annotation as A
import Json.Decode as Decode
import Json.Encode as Encode



-- EXPRESSIONS


type alias CASTS_Expr =
    A.CRA_Located CASTS_Expr_


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
    | CASTS_Binops (List ( CASTS_Expr, A.CRA_Located CDN_Name )) CASTS_Expr
    | CASTS_Lambda (List CASTS_Pattern) CASTS_Expr
    | CASTS_Call CASTS_Expr (List CASTS_Expr)
    | CASTS_If (List ( CASTS_Expr, CASTS_Expr )) CASTS_Expr
    | CASTS_Let (List (A.CRA_Located CASTS_Def)) CASTS_Expr
    | CASTS_Case CASTS_Expr (List ( CASTS_Pattern, CASTS_Expr ))
    | CASTS_Accessor CDN_Name
    | CASTS_Access CASTS_Expr (A.CRA_Located CDN_Name)
    | CASTS_Update (A.CRA_Located CDN_Name) (List ( A.CRA_Located CDN_Name, CASTS_Expr ))
    | CASTS_Record (List ( A.CRA_Located CDN_Name, CASTS_Expr ))
    | CASTS_Unit
    | CASTS_Tuple CASTS_Expr CASTS_Expr (List CASTS_Expr)
    | CASTS_Shader Shader.CASTUS_Source Shader.CASTUS_Types


type CASTS_VarType
    = CASTS_LowVar
    | CASTS_CapVar



-- DEFINITIONS


type CASTS_Def
    = CASTS_Define (A.CRA_Located CDN_Name) (List CASTS_Pattern) CASTS_Expr (Maybe CASTS_Type)
    | CASTS_Destruct CASTS_Pattern CASTS_Expr



-- PATTERN


type alias CASTS_Pattern =
    A.CRA_Located CASTS_Pattern_


type CASTS_Pattern_
    = CASTS_PAnything
    | CASTS_PVar CDN_Name
    | CASTS_PRecord (List (A.CRA_Located CDN_Name))
    | CASTS_PAlias CASTS_Pattern (A.CRA_Located CDN_Name)
    | CASTS_PUnit
    | CASTS_PTuple CASTS_Pattern CASTS_Pattern (List CASTS_Pattern)
    | CASTS_PCtor A.CRA_Region CDN_Name (List CASTS_Pattern)
    | CASTS_PCtorQual A.CRA_Region CDN_Name CDN_Name (List CASTS_Pattern)
    | CASTS_PList (List CASTS_Pattern)
    | CASTS_PCons CASTS_Pattern CASTS_Pattern
    | CASTS_PChr String
    | CASTS_PStr String
    | CASTS_PInt Int



-- TYPE


type alias CASTS_Type =
    A.CRA_Located CASTS_Type_


type CASTS_Type_
    = CASTS_TLambda CASTS_Type CASTS_Type
    | CASTS_TVar CDN_Name
    | CASTS_TType A.CRA_Region CDN_Name (List CASTS_Type)
    | CASTS_TTypeQual A.CRA_Region CDN_Name CDN_Name (List CASTS_Type)
    | CASTS_TRecord (List ( A.CRA_Located CDN_Name, CASTS_Type )) (Maybe (A.CRA_Located CDN_Name))
    | CASTS_TUnit
    | CASTS_TTuple CASTS_Type CASTS_Type (List CASTS_Type)



-- MODULE


type CASTS_Module
    = CASTS_Module (Maybe (A.CRA_Located CDN_Name)) (A.CRA_Located CASTS_Exposing) CASTS_Docs (List CASTS_Import) (List (A.CRA_Located CASTS_Value)) (List (A.CRA_Located CASTS_Union)) (List (A.CRA_Located CASTS_Alias)) (List (A.CRA_Located CASTS_Infix)) CASTS_Effects


getName : CASTS_Module -> CDN_Name
getName (CASTS_Module maybeName _ _ _ _ _ _ _ _) =
    case maybeName of
        Just (A.CRA_At _ name) ->
            name

        Nothing ->
            Name.mainModule


getImportName : CASTS_Import -> CDN_Name
getImportName (CASTS_Import (A.CRA_At _ name) _ _) =
    name


type CASTS_Import
    = CASTS_Import (A.CRA_Located CDN_Name) (Maybe Name.CDN_Name) CASTS_Exposing


type CASTS_Value
    = CASTS_Value (A.CRA_Located CDN_Name) (List CASTS_Pattern) CASTS_Expr (Maybe CASTS_Type)


type CASTS_Union
    = CASTS_Union (A.CRA_Located CDN_Name) (List (A.CRA_Located CDN_Name)) (List ( A.CRA_Located CDN_Name, List CASTS_Type ))


type CASTS_Alias
    = CASTS_Alias (A.CRA_Located CDN_Name) (List (A.CRA_Located CDN_Name)) CASTS_Type


type CASTS_Infix
    = CASTS_Infix CDN_Name Binop.CASTU_Associativity Binop.CASTU_Precedence CDN_Name


type CASTS_Port
    = CASTS_Port (A.CRA_Located CDN_Name) CASTS_Type


type CASTS_Effects
    = CASTS_NoEffects
    | CASTS_Ports (List CASTS_Port)
    | CASTS_Manager A.CRA_Region CASTS_Manager


type CASTS_Manager
    = CASTS_Cmd (A.CRA_Located CDN_Name)
    | CASTS_Sub (A.CRA_Located CDN_Name)
    | CASTS_Fx (A.CRA_Located CDN_Name) (A.CRA_Located CDN_Name)


type CASTS_Docs
    = CASTS_NoDocs A.CRA_Region
    | CASTS_YesDocs CASTS_Comment (List ( CDN_Name, CASTS_Comment ))


type CASTS_Comment
    = CASTS_Comment P.Snippet



-- EXPOSING


type CASTS_Exposing
    = CASTS_Open
    | CASTS_Explicit (List CASTS_Exposed)


type CASTS_Exposed
    = CASTS_Lower (A.CRA_Located CDN_Name)
    | CASTS_Upper (A.CRA_Located CDN_Name) CASTS_Privacy
    | CASTS_Operator A.CRA_Region CDN_Name


type CASTS_Privacy
    = CASTS_Public A.CRA_Region
    | CASTS_Private



-- ENCODERS and DECODERS


typeEncoder : CASTS_Type -> Encode.Value
typeEncoder =
    A.locatedEncoder internalTypeEncoder


typeDecoder : Decode.Decoder CASTS_Type
typeDecoder =
    A.locatedDecoder internalTypeDecoder


internalTypeEncoder : CASTS_Type_ -> Encode.Value
internalTypeEncoder type_ =
    case type_ of
        CASTS_TLambda arg result ->
            Encode.object
                [ ( "type", Encode.string "TLambda" )
                , ( "arg", typeEncoder arg )
                , ( "result", typeEncoder result )
                ]

        CASTS_TVar name ->
            Encode.object
                [ ( "type", Encode.string "TVar" )
                , ( "name", Encode.string name )
                ]

        CASTS_TType region name args ->
            Encode.object
                [ ( "type", Encode.string "TType" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        CASTS_TTypeQual region home name args ->
            Encode.object
                [ ( "type", Encode.string "TTypeQual" )
                , ( "region", A.regionEncoder region )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        CASTS_TRecord fields ext ->
            Encode.object
                [ ( "type", Encode.string "TRecord" )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) typeEncoder) fields )
                , ( "ext", E.maybe (A.locatedEncoder Encode.string) ext )
                ]

        CASTS_TUnit ->
            Encode.object
                [ ( "type", Encode.string "TUnit" )
                ]

        CASTS_TTuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "TTuple" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                , ( "cs", Encode.list typeEncoder cs )
                ]


internalTypeDecoder : Decode.Decoder CASTS_Type_
internalTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TLambda" ->
                        Decode.map2 CASTS_TLambda
                            (Decode.field "arg" typeDecoder)
                            (Decode.field "result" typeDecoder)

                    "TVar" ->
                        Decode.map CASTS_TVar (Decode.field "name" Decode.string)

                    "TType" ->
                        Decode.map3 CASTS_TType
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "TTypeQual" ->
                        Decode.map4 CASTS_TTypeQual
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "TRecord" ->
                        Decode.map2 CASTS_TRecord
                            (Decode.field "fields"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" (A.locatedDecoder Decode.string))
                                        (Decode.field "b" typeDecoder)
                                    )
                                )
                            )
                            (Decode.field "ext" (Decode.maybe (A.locatedDecoder Decode.string)))

                    "TUnit" ->
                        Decode.succeed CASTS_TUnit

                    "TTuple" ->
                        Decode.map3 CASTS_TTuple
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)
                            (Decode.field "cs" (Decode.list typeDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Type_'s type: " ++ type_)
            )


moduleEncoder : CASTS_Module -> Encode.Value
moduleEncoder (CASTS_Module maybeName exports docs imports values unions aliases binops effects) =
    Encode.object
        [ ( "type", Encode.string "Module" )
        , ( "maybeName", E.maybe (A.locatedEncoder Encode.string) maybeName )
        , ( "exports", A.locatedEncoder exposingEncoder exports )
        , ( "docs", docsEncoder docs )
        , ( "imports", Encode.list importEncoder imports )
        , ( "values", Encode.list (A.locatedEncoder valueEncoder) values )
        , ( "unions", Encode.list (A.locatedEncoder unionEncoder) unions )
        , ( "aliases", Encode.list (A.locatedEncoder aliasEncoder) aliases )
        , ( "binops", Encode.list (A.locatedEncoder infixEncoder) binops )
        , ( "effects", effectsEncoder effects )
        ]


moduleDecoder : Decode.Decoder CASTS_Module
moduleDecoder =
    Decode.map8 (\( maybeName, exports ) -> CASTS_Module maybeName exports)
        (Decode.map2 Tuple.pair
            (Decode.field "maybeName" (Decode.maybe (A.locatedDecoder Decode.string)))
            (Decode.field "exports" (A.locatedDecoder exposingDecoder))
        )
        (Decode.field "docs" docsDecoder)
        (Decode.field "imports" (Decode.list importDecoder))
        (Decode.field "values" (Decode.list (A.locatedDecoder valueDecoder)))
        (Decode.field "unions" (Decode.list (A.locatedDecoder unionDecoder)))
        (Decode.field "aliases" (Decode.list (A.locatedDecoder aliasDecoder)))
        (Decode.field "binops" (Decode.list (A.locatedDecoder infixDecoder)))
        (Decode.field "effects" effectsDecoder)


exposingEncoder : CASTS_Exposing -> Encode.Value
exposingEncoder exposing_ =
    case exposing_ of
        CASTS_Open ->
            Encode.object
                [ ( "type", Encode.string "Open" )
                ]

        CASTS_Explicit exposedList ->
            Encode.object
                [ ( "type", Encode.string "Explicit" )
                , ( "exposedList", Encode.list exposedEncoder exposedList )
                ]


exposingDecoder : Decode.Decoder CASTS_Exposing
exposingDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Open" ->
                        Decode.succeed CASTS_Open

                    "Explicit" ->
                        Decode.map CASTS_Explicit (Decode.field "exposedList" (Decode.list exposedDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Exposing's type: " ++ type_)
            )


docsEncoder : CASTS_Docs -> Encode.Value
docsEncoder docs =
    case docs of
        CASTS_NoDocs region ->
            Encode.object
                [ ( "type", Encode.string "NoDocs" )
                , ( "region", A.regionEncoder region )
                ]

        CASTS_YesDocs overview comments ->
            Encode.object
                [ ( "type", Encode.string "YesDocs" )
                , ( "overview", commentEncoder overview )
                , ( "comments", Encode.list (E.jsonPair Encode.string commentEncoder) comments )
                ]


docsDecoder : Decode.Decoder CASTS_Docs
docsDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoDocs" ->
                        Decode.map CASTS_NoDocs (Decode.field "region" A.regionDecoder)

                    "YesDocs" ->
                        Decode.map2 CASTS_YesDocs
                            (Decode.field "overview" commentDecoder)
                            (Decode.field "comments"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" Decode.string)
                                        (Decode.field "b" commentDecoder)
                                    )
                                )
                            )

                    _ ->
                        Decode.fail ("Failed to decode Docs's type: " ++ type_)
            )


importEncoder : CASTS_Import -> Encode.Value
importEncoder (CASTS_Import importName maybeAlias exposing_) =
    Encode.object
        [ ( "type", Encode.string "Import" )
        , ( "importName", A.locatedEncoder Encode.string importName )
        , ( "maybeAlias", E.maybe Encode.string maybeAlias )
        , ( "exposing", exposingEncoder exposing_ )
        ]


importDecoder : Decode.Decoder CASTS_Import
importDecoder =
    Decode.map3 CASTS_Import
        (Decode.field "importName" (A.locatedDecoder Decode.string))
        (Decode.field "maybeAlias" (Decode.maybe Decode.string))
        (Decode.field "exposing" exposingDecoder)


valueEncoder : CASTS_Value -> Encode.Value
valueEncoder (CASTS_Value name srcArgs body maybeType) =
    Encode.object
        [ ( "type", Encode.string "Value" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "srcArgs", Encode.list patternEncoder srcArgs )
        , ( "body", exprEncoder body )
        , ( "maybeType", E.maybe typeEncoder maybeType )
        ]


valueDecoder : Decode.Decoder CASTS_Value
valueDecoder =
    Decode.map4 CASTS_Value
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "srcArgs" (Decode.list patternDecoder))
        (Decode.field "body" exprDecoder)
        (Decode.field "maybeType" (Decode.maybe typeDecoder))


unionEncoder : CASTS_Union -> Encode.Value
unionEncoder (CASTS_Union name args constructors) =
    Encode.object
        [ ( "type", Encode.string "Union" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "args", Encode.list (A.locatedEncoder Encode.string) args )
        , ( "constructors", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) (Encode.list typeEncoder)) constructors )
        ]


unionDecoder : Decode.Decoder CASTS_Union
unionDecoder =
    Decode.map3 CASTS_Union
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "args" (Decode.list (A.locatedDecoder Decode.string)))
        (Decode.field "constructors"
            (Decode.list
                (Decode.map2 Tuple.pair
                    (Decode.field "a" (A.locatedDecoder Decode.string))
                    (Decode.field "b" (Decode.list typeDecoder))
                )
            )
        )


aliasEncoder : CASTS_Alias -> Encode.Value
aliasEncoder (CASTS_Alias name args tipe) =
    Encode.object
        [ ( "type", Encode.string "Alias" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "args", Encode.list (A.locatedEncoder Encode.string) args )
        , ( "tipe", typeEncoder tipe )
        ]


aliasDecoder : Decode.Decoder CASTS_Alias
aliasDecoder =
    Decode.map3 CASTS_Alias
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "args" (Decode.list (A.locatedDecoder Decode.string)))
        (Decode.field "tipe" typeDecoder)


infixEncoder : CASTS_Infix -> Encode.Value
infixEncoder (CASTS_Infix op associativity precedence name) =
    Encode.object
        [ ( "type", Encode.string "Infix" )
        , ( "op", Encode.string op )
        , ( "associativity", Binop.associativityEncoder associativity )
        , ( "precedence", Binop.precedenceEncoder precedence )
        , ( "name", Encode.string name )
        ]


infixDecoder : Decode.Decoder CASTS_Infix
infixDecoder =
    Decode.map4 CASTS_Infix
        (Decode.field "op" Decode.string)
        (Decode.field "associativity" Binop.associativityDecoder)
        (Decode.field "precedence" Binop.precedenceDecoder)
        (Decode.field "name" Decode.string)


effectsEncoder : CASTS_Effects -> Encode.Value
effectsEncoder effects =
    case effects of
        CASTS_NoEffects ->
            Encode.object
                [ ( "type", Encode.string "NoEffects" )
                ]

        CASTS_Ports ports ->
            Encode.object
                [ ( "type", Encode.string "Ports" )
                , ( "ports", Encode.list portEncoder ports )
                ]

        CASTS_Manager region manager ->
            Encode.object
                [ ( "type", Encode.string "Manager" )
                , ( "region", A.regionEncoder region )
                , ( "manager", managerEncoder manager )
                ]


effectsDecoder : Decode.Decoder CASTS_Effects
effectsDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoEffects" ->
                        Decode.succeed CASTS_NoEffects

                    "Ports" ->
                        Decode.map CASTS_Ports (Decode.field "ports" (Decode.list portDecoder))

                    "Manager" ->
                        Decode.map2 CASTS_Manager
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "manager" managerDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Effects' type: " ++ type_)
            )


commentEncoder : CASTS_Comment -> Encode.Value
commentEncoder (CASTS_Comment snippet) =
    P.snippetEncoder snippet


commentDecoder : Decode.Decoder CASTS_Comment
commentDecoder =
    Decode.map CASTS_Comment P.snippetDecoder


portEncoder : CASTS_Port -> Encode.Value
portEncoder (CASTS_Port name tipe) =
    Encode.object
        [ ( "type", Encode.string "Port" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "tipe", typeEncoder tipe )
        ]


portDecoder : Decode.Decoder CASTS_Port
portDecoder =
    Decode.map2 CASTS_Port
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "tipe" typeDecoder)


managerEncoder : CASTS_Manager -> Encode.Value
managerEncoder manager =
    case manager of
        CASTS_Cmd cmdType ->
            Encode.object
                [ ( "type", Encode.string "Cmd" )
                , ( "cmdType", A.locatedEncoder Encode.string cmdType )
                ]

        CASTS_Sub subType ->
            Encode.object
                [ ( "type", Encode.string "Sub" )
                , ( "subType", A.locatedEncoder Encode.string subType )
                ]

        CASTS_Fx cmdType subType ->
            Encode.object
                [ ( "type", Encode.string "Fx" )
                , ( "cmdType", A.locatedEncoder Encode.string cmdType )
                , ( "subType", A.locatedEncoder Encode.string subType )
                ]


managerDecoder : Decode.Decoder CASTS_Manager
managerDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Cmd" ->
                        Decode.map CASTS_Cmd (Decode.field "cmdType" (A.locatedDecoder Decode.string))

                    "Sub" ->
                        Decode.map CASTS_Sub (Decode.field "subType" (A.locatedDecoder Decode.string))

                    "Fx" ->
                        Decode.map2 CASTS_Fx
                            (Decode.field "cmdType" (A.locatedDecoder Decode.string))
                            (Decode.field "subType" (A.locatedDecoder Decode.string))

                    _ ->
                        Decode.fail ("Failed to decode Manager's type: " ++ type_)
            )


exposedEncoder : CASTS_Exposed -> Encode.Value
exposedEncoder exposed =
    case exposed of
        CASTS_Lower name ->
            Encode.object
                [ ( "type", Encode.string "Lower" )
                , ( "name", A.locatedEncoder Encode.string name )
                ]

        CASTS_Upper name dotDotRegion ->
            Encode.object
                [ ( "type", Encode.string "Upper" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "dotDotRegion", privacyEncoder dotDotRegion )
                ]

        CASTS_Operator region name ->
            Encode.object
                [ ( "type", Encode.string "Operator" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                ]


exposedDecoder : Decode.Decoder CASTS_Exposed
exposedDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Lower" ->
                        Decode.map CASTS_Lower (Decode.field "name" (A.locatedDecoder Decode.string))

                    "Upper" ->
                        Decode.map2 CASTS_Upper
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "dotDotRegion" privacyDecoder)

                    "Operator" ->
                        Decode.map2 CASTS_Operator
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail ("Failed to decode Exposed's type: " ++ type_)
            )


privacyEncoder : CASTS_Privacy -> Encode.Value
privacyEncoder privacy =
    case privacy of
        CASTS_Public region ->
            Encode.object
                [ ( "type", Encode.string "Public" )
                , ( "region", A.regionEncoder region )
                ]

        CASTS_Private ->
            Encode.object
                [ ( "type", Encode.string "Private" )
                ]


privacyDecoder : Decode.Decoder CASTS_Privacy
privacyDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Public" ->
                        Decode.map CASTS_Public (Decode.field "region" A.regionDecoder)

                    "Private" ->
                        Decode.succeed CASTS_Private

                    _ ->
                        Decode.fail ("Failed to decode Privacy's type: " ++ type_)
            )


patternEncoder : CASTS_Pattern -> Encode.Value
patternEncoder =
    A.locatedEncoder pattern_Encoder


patternDecoder : Decode.Decoder CASTS_Pattern
patternDecoder =
    A.locatedDecoder pattern_Decoder


pattern_Encoder : CASTS_Pattern_ -> Encode.Value
pattern_Encoder pattern_ =
    case pattern_ of
        CASTS_PAnything ->
            Encode.object
                [ ( "type", Encode.string "PAnything" )
                ]

        CASTS_PVar name ->
            Encode.object
                [ ( "type", Encode.string "PVar" )
                , ( "name", Encode.string name )
                ]

        CASTS_PRecord fields ->
            Encode.object
                [ ( "type", Encode.string "PRecord" )
                , ( "fields", Encode.list (A.locatedEncoder Encode.string) fields )
                ]

        CASTS_PAlias aliasPattern name ->
            Encode.object
                [ ( "type", Encode.string "PAlias" )
                , ( "aliasPattern", patternEncoder aliasPattern )
                , ( "name", A.locatedEncoder Encode.string name )
                ]

        CASTS_PUnit ->
            Encode.object
                [ ( "type", Encode.string "PUnit" )
                ]

        CASTS_PTuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "PTuple" )
                , ( "a", patternEncoder a )
                , ( "b", patternEncoder b )
                , ( "cs", Encode.list patternEncoder cs )
                ]

        CASTS_PCtor nameRegion name patterns ->
            Encode.object
                [ ( "type", Encode.string "PCtor" )
                , ( "nameRegion", A.regionEncoder nameRegion )
                , ( "name", Encode.string name )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        CASTS_PCtorQual nameRegion home name patterns ->
            Encode.object
                [ ( "type", Encode.string "PCtorQual" )
                , ( "nameRegion", A.regionEncoder nameRegion )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        CASTS_PList patterns ->
            Encode.object
                [ ( "type", Encode.string "PList" )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        CASTS_PCons hd tl ->
            Encode.object
                [ ( "type", Encode.string "PCons" )
                , ( "hd", patternEncoder hd )
                , ( "tl", patternEncoder tl )
                ]

        CASTS_PChr chr ->
            Encode.object
                [ ( "type", Encode.string "PChr" )
                , ( "chr", Encode.string chr )
                ]

        CASTS_PStr str ->
            Encode.object
                [ ( "type", Encode.string "PStr" )
                , ( "str", Encode.string str )
                ]

        CASTS_PInt int ->
            Encode.object
                [ ( "type", Encode.string "PInt" )
                , ( "int", Encode.int int )
                ]


pattern_Decoder : Decode.Decoder CASTS_Pattern_
pattern_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PAnything" ->
                        Decode.succeed CASTS_PAnything

                    "PVar" ->
                        Decode.map CASTS_PVar (Decode.field "name" Decode.string)

                    "PRecord" ->
                        Decode.map CASTS_PRecord (Decode.field "fields" (Decode.list (A.locatedDecoder Decode.string)))

                    "PAlias" ->
                        Decode.map2 CASTS_PAlias
                            (Decode.field "aliasPattern" patternDecoder)
                            (Decode.field "name" (A.locatedDecoder Decode.string))

                    "PUnit" ->
                        Decode.succeed CASTS_PUnit

                    "PTuple" ->
                        Decode.map3 CASTS_PTuple
                            (Decode.field "a" patternDecoder)
                            (Decode.field "b" patternDecoder)
                            (Decode.field "cs" (Decode.list patternDecoder))

                    "PCtor" ->
                        Decode.map3 CASTS_PCtor
                            (Decode.field "nameRegion" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "patterns" (Decode.list patternDecoder))

                    "PCtorQual" ->
                        Decode.map4 CASTS_PCtorQual
                            (Decode.field "nameRegion" A.regionDecoder)
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)
                            (Decode.field "patterns" (Decode.list patternDecoder))

                    "PList" ->
                        Decode.map CASTS_PList (Decode.field "patterns" (Decode.list patternDecoder))

                    "PCons" ->
                        Decode.map2 CASTS_PCons
                            (Decode.field "hd" patternDecoder)
                            (Decode.field "tl" patternDecoder)

                    "PChr" ->
                        Decode.map CASTS_PChr (Decode.field "chr" Decode.string)

                    "PStr" ->
                        Decode.map CASTS_PStr (Decode.field "str" Decode.string)

                    "PInt" ->
                        Decode.map CASTS_PInt (Decode.field "int" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Pattern_'s type: " ++ type_)
            )


exprEncoder : CASTS_Expr -> Encode.Value
exprEncoder =
    A.locatedEncoder expr_Encoder


exprDecoder : Decode.Decoder CASTS_Expr
exprDecoder =
    A.locatedDecoder expr_Decoder


expr_Encoder : CASTS_Expr_ -> Encode.Value
expr_Encoder expr_ =
    case expr_ of
        CASTS_Chr char ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "char", Encode.string char )
                ]

        CASTS_Str string ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "string", Encode.string string )
                ]

        CASTS_Int int ->
            Encode.object
                [ ( "type", Encode.string "Int" )
                , ( "int", Encode.int int )
                ]

        CASTS_Float float ->
            Encode.object
                [ ( "type", Encode.string "Float" )
                , ( "float", Encode.float float )
                ]

        CASTS_Var varType name ->
            Encode.object
                [ ( "type", Encode.string "Var" )
                , ( "varType", varTypeEncoder varType )
                , ( "name", Encode.string name )
                ]

        CASTS_VarQual varType prefix name ->
            Encode.object
                [ ( "type", Encode.string "VarQual" )
                , ( "varType", varTypeEncoder varType )
                , ( "prefix", Encode.string prefix )
                , ( "name", Encode.string name )
                ]

        CASTS_List list ->
            Encode.object
                [ ( "type", Encode.string "List" )
                , ( "list", Encode.list exprEncoder list )
                ]

        CASTS_Op op ->
            Encode.object
                [ ( "type", Encode.string "Op" )
                , ( "op", Encode.string op )
                ]

        CASTS_Negate expr ->
            Encode.object
                [ ( "type", Encode.string "Negate" )
                , ( "expr", exprEncoder expr )
                ]

        CASTS_Binops ops final ->
            Encode.object
                [ ( "type", Encode.string "Binops" )
                , ( "ops", Encode.list (E.jsonPair exprEncoder (A.locatedEncoder Encode.string)) ops )
                , ( "final", exprEncoder final )
                ]

        CASTS_Lambda srcArgs body ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "srcArgs", Encode.list patternEncoder srcArgs )
                , ( "body", exprEncoder body )
                ]

        CASTS_Call func args ->
            Encode.object
                [ ( "type", Encode.string "Call" )
                , ( "func", exprEncoder func )
                , ( "args", Encode.list exprEncoder args )
                ]

        CASTS_If branches finally ->
            Encode.object
                [ ( "type", Encode.string "If" )
                , ( "branches", Encode.list (E.jsonPair exprEncoder exprEncoder) branches )
                , ( "finally", exprEncoder finally )
                ]

        CASTS_Let defs expr ->
            Encode.object
                [ ( "type", Encode.string "Let" )
                , ( "defs", Encode.list (A.locatedEncoder defEncoder) defs )
                , ( "expr", exprEncoder expr )
                ]

        CASTS_Case expr branches ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                , ( "expr", exprEncoder expr )
                , ( "branches", Encode.list (E.jsonPair patternEncoder exprEncoder) branches )
                ]

        CASTS_Accessor field ->
            Encode.object
                [ ( "type", Encode.string "Accessor" )
                , ( "field", Encode.string field )
                ]

        CASTS_Access record field ->
            Encode.object
                [ ( "type", Encode.string "Access" )
                , ( "record", exprEncoder record )
                , ( "field", A.locatedEncoder Encode.string field )
                ]

        CASTS_Update name fields ->
            Encode.object
                [ ( "type", Encode.string "Update" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) exprEncoder) fields )
                ]

        CASTS_Record fields ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) exprEncoder) fields )
                ]

        CASTS_Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        CASTS_Tuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", exprEncoder a )
                , ( "b", exprEncoder b )
                , ( "cs", Encode.list exprEncoder cs )
                ]

        CASTS_Shader src tipe ->
            Encode.object
                [ ( "type", Encode.string "Shader" )
                , ( "src", Shader.sourceEncoder src )
                , ( "tipe", Shader.typesEncoder tipe )
                ]


expr_Decoder : Decode.Decoder CASTS_Expr_
expr_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Chr" ->
                        Decode.map CASTS_Chr (Decode.field "char" Decode.string)

                    "Str" ->
                        Decode.map CASTS_Str (Decode.field "string" Decode.string)

                    "Int" ->
                        Decode.map CASTS_Int (Decode.field "int" Decode.int)

                    "Float" ->
                        Decode.map CASTS_Float (Decode.field "float" Decode.float)

                    "Var" ->
                        Decode.map2 CASTS_Var
                            (Decode.field "varType" varTypeDecoder)
                            (Decode.field "name" Decode.string)

                    "VarQual" ->
                        Decode.map3 CASTS_VarQual
                            (Decode.field "varType" varTypeDecoder)
                            (Decode.field "prefix" Decode.string)
                            (Decode.field "name" Decode.string)

                    "List" ->
                        Decode.map CASTS_List (Decode.field "list" (Decode.list exprDecoder))

                    "Op" ->
                        Decode.map CASTS_Op (Decode.field "op" Decode.string)

                    "Negate" ->
                        Decode.map CASTS_Negate (Decode.field "expr" exprDecoder)

                    "Binops" ->
                        Decode.map2 CASTS_Binops
                            (Decode.field "ops"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" exprDecoder)
                                        (Decode.field "b" (A.locatedDecoder Decode.string))
                                    )
                                )
                            )
                            (Decode.field "final" exprDecoder)

                    "Lambda" ->
                        Decode.map2 CASTS_Lambda
                            (Decode.field "srcArgs" (Decode.list patternDecoder))
                            (Decode.field "body" exprDecoder)

                    "Call" ->
                        Decode.map2 CASTS_Call
                            (Decode.field "func" exprDecoder)
                            (Decode.field "args" (Decode.list exprDecoder))

                    "If" ->
                        Decode.map2 CASTS_If
                            (Decode.field "branches"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" exprDecoder)
                                        (Decode.field "b" exprDecoder)
                                    )
                                )
                            )
                            (Decode.field "finally" exprDecoder)

                    "Let" ->
                        Decode.map2 CASTS_Let
                            (Decode.field "defs" (Decode.list (A.locatedDecoder defDecoder)))
                            (Decode.field "expr" exprDecoder)

                    "Case" ->
                        Decode.map2 CASTS_Case
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "branches"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" patternDecoder)
                                        (Decode.field "b" exprDecoder)
                                    )
                                )
                            )

                    "Accessor" ->
                        Decode.map CASTS_Accessor (Decode.field "field" Decode.string)

                    "Access" ->
                        Decode.map2 CASTS_Access
                            (Decode.field "record" exprDecoder)
                            (Decode.field "field" (A.locatedDecoder Decode.string))

                    "Update" ->
                        Decode.map2 CASTS_Update
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "fields"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" (A.locatedDecoder Decode.string))
                                        (Decode.field "b" exprDecoder)
                                    )
                                )
                            )

                    "Record" ->
                        Decode.map CASTS_Record
                            (Decode.field "fields"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" (A.locatedDecoder Decode.string))
                                        (Decode.field "b" exprDecoder)
                                    )
                                )
                            )

                    "Unit" ->
                        Decode.succeed CASTS_Unit

                    "Tuple" ->
                        Decode.map3 CASTS_Tuple
                            (Decode.field "a" exprDecoder)
                            (Decode.field "b" exprDecoder)
                            (Decode.field "cs" (Decode.list exprDecoder))

                    "Shader" ->
                        Decode.map2 CASTS_Shader
                            (Decode.field "src" Shader.sourceDecoder)
                            (Decode.field "tipe" Shader.typesDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Expr_'s type: " ++ type_)
            )


varTypeEncoder : CASTS_VarType -> Encode.Value
varTypeEncoder varType =
    case varType of
        CASTS_LowVar ->
            Encode.string "LowVar"

        CASTS_CapVar ->
            Encode.string "CapVar"


varTypeDecoder : Decode.Decoder CASTS_VarType
varTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "LowVar" ->
                        Decode.succeed CASTS_LowVar

                    "CapVar" ->
                        Decode.succeed CASTS_CapVar

                    _ ->
                        Decode.fail ("Unknown VarType: " ++ str)
            )


defEncoder : CASTS_Def -> Encode.Value
defEncoder def =
    case def of
        CASTS_Define name srcArgs body maybeType ->
            Encode.object
                [ ( "type", Encode.string "Define" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "srcArgs", Encode.list patternEncoder srcArgs )
                , ( "body", exprEncoder body )
                , ( "maybeType", E.maybe typeEncoder maybeType )
                ]

        CASTS_Destruct pattern body ->
            Encode.object
                [ ( "type", Encode.string "Destruct" )
                , ( "pattern", patternEncoder pattern )
                , ( "body", exprEncoder body )
                ]


defDecoder : Decode.Decoder CASTS_Def
defDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Define" ->
                        Decode.map4 CASTS_Define
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "srcArgs" (Decode.list patternDecoder))
                            (Decode.field "body" exprDecoder)
                            (Decode.field "maybeType" (Decode.maybe typeDecoder))

                    "Destruct" ->
                        Decode.map2 CASTS_Destruct
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "body" exprDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Def's type: " ++ type_)
            )
