module Compiler.AST.Source exposing
    ( getImportName
    , getName
    , moduleDecoder
    , moduleEncoder
    , typeDecoder
    , typeEncoder
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Name as Name
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Annotation as A
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- MODULE


getName : T.CASTS_Module -> T.CDN_Name
getName (T.CASTS_Module maybeName _ _ _ _ _ _ _ _) =
    case maybeName of
        Just (T.CRA_At _ name) ->
            name

        Nothing ->
            Name.mainModule


getImportName : T.CASTS_Import -> T.CDN_Name
getImportName (T.CASTS_Import (T.CRA_At _ name) _ _) =
    name



-- ENCODERS and DECODERS


typeEncoder : T.CASTS_Type -> Encode.Value
typeEncoder =
    A.locatedEncoder internalTypeEncoder


typeDecoder : Decode.Decoder T.CASTS_Type
typeDecoder =
    A.locatedDecoder internalTypeDecoder


internalTypeEncoder : T.CASTS_Type_ -> Encode.Value
internalTypeEncoder type_ =
    case type_ of
        T.CASTS_TLambda arg result ->
            Encode.object
                [ ( "type", Encode.string "TLambda" )
                , ( "arg", typeEncoder arg )
                , ( "result", typeEncoder result )
                ]

        T.CASTS_TVar name ->
            Encode.object
                [ ( "type", Encode.string "TVar" )
                , ( "name", Encode.string name )
                ]

        T.CASTS_TType region name args ->
            Encode.object
                [ ( "type", Encode.string "TType" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        T.CASTS_TTypeQual region home name args ->
            Encode.object
                [ ( "type", Encode.string "TTypeQual" )
                , ( "region", A.regionEncoder region )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        T.CASTS_TRecord fields ext ->
            Encode.object
                [ ( "type", Encode.string "TRecord" )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) typeEncoder) fields )
                , ( "ext", E.maybe (A.locatedEncoder Encode.string) ext )
                ]

        T.CASTS_TUnit ->
            Encode.object
                [ ( "type", Encode.string "TUnit" )
                ]

        T.CASTS_TTuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "TTuple" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                , ( "cs", Encode.list typeEncoder cs )
                ]


internalTypeDecoder : Decode.Decoder T.CASTS_Type_
internalTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TLambda" ->
                        Decode.map2 T.CASTS_TLambda
                            (Decode.field "arg" typeDecoder)
                            (Decode.field "result" typeDecoder)

                    "TVar" ->
                        Decode.map T.CASTS_TVar (Decode.field "name" Decode.string)

                    "TType" ->
                        Decode.map3 T.CASTS_TType
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "TTypeQual" ->
                        Decode.map4 T.CASTS_TTypeQual
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "TRecord" ->
                        Decode.map2 T.CASTS_TRecord
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
                        Decode.succeed T.CASTS_TUnit

                    "TTuple" ->
                        Decode.map3 T.CASTS_TTuple
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)
                            (Decode.field "cs" (Decode.list typeDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Type_'s type: " ++ type_)
            )


moduleEncoder : T.CASTS_Module -> Encode.Value
moduleEncoder (T.CASTS_Module maybeName exports docs imports values unions aliases binops effects) =
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


moduleDecoder : Decode.Decoder T.CASTS_Module
moduleDecoder =
    Decode.map8 (\( maybeName, exports ) -> T.CASTS_Module maybeName exports)
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


exposingEncoder : T.CASTS_Exposing -> Encode.Value
exposingEncoder exposing_ =
    case exposing_ of
        T.CASTS_Open ->
            Encode.object
                [ ( "type", Encode.string "Open" )
                ]

        T.CASTS_Explicit exposedList ->
            Encode.object
                [ ( "type", Encode.string "Explicit" )
                , ( "exposedList", Encode.list exposedEncoder exposedList )
                ]


exposingDecoder : Decode.Decoder T.CASTS_Exposing
exposingDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Open" ->
                        Decode.succeed T.CASTS_Open

                    "Explicit" ->
                        Decode.map T.CASTS_Explicit (Decode.field "exposedList" (Decode.list exposedDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Exposing's type: " ++ type_)
            )


docsEncoder : T.CASTS_Docs -> Encode.Value
docsEncoder docs =
    case docs of
        T.CASTS_NoDocs region ->
            Encode.object
                [ ( "type", Encode.string "NoDocs" )
                , ( "region", A.regionEncoder region )
                ]

        T.CASTS_YesDocs overview comments ->
            Encode.object
                [ ( "type", Encode.string "YesDocs" )
                , ( "overview", commentEncoder overview )
                , ( "comments", Encode.list (E.jsonPair Encode.string commentEncoder) comments )
                ]


docsDecoder : Decode.Decoder T.CASTS_Docs
docsDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoDocs" ->
                        Decode.map T.CASTS_NoDocs (Decode.field "region" A.regionDecoder)

                    "YesDocs" ->
                        Decode.map2 T.CASTS_YesDocs
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


importEncoder : T.CASTS_Import -> Encode.Value
importEncoder (T.CASTS_Import importName maybeAlias exposing_) =
    Encode.object
        [ ( "type", Encode.string "Import" )
        , ( "importName", A.locatedEncoder Encode.string importName )
        , ( "maybeAlias", E.maybe Encode.string maybeAlias )
        , ( "exposing", exposingEncoder exposing_ )
        ]


importDecoder : Decode.Decoder T.CASTS_Import
importDecoder =
    Decode.map3 T.CASTS_Import
        (Decode.field "importName" (A.locatedDecoder Decode.string))
        (Decode.field "maybeAlias" (Decode.maybe Decode.string))
        (Decode.field "exposing" exposingDecoder)


valueEncoder : T.CASTS_Value -> Encode.Value
valueEncoder (T.CASTS_Value name srcArgs body maybeType) =
    Encode.object
        [ ( "type", Encode.string "Value" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "srcArgs", Encode.list patternEncoder srcArgs )
        , ( "body", exprEncoder body )
        , ( "maybeType", E.maybe typeEncoder maybeType )
        ]


valueDecoder : Decode.Decoder T.CASTS_Value
valueDecoder =
    Decode.map4 T.CASTS_Value
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "srcArgs" (Decode.list patternDecoder))
        (Decode.field "body" exprDecoder)
        (Decode.field "maybeType" (Decode.maybe typeDecoder))


unionEncoder : T.CASTS_Union -> Encode.Value
unionEncoder (T.CASTS_Union name args constructors) =
    Encode.object
        [ ( "type", Encode.string "Union" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "args", Encode.list (A.locatedEncoder Encode.string) args )
        , ( "constructors", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) (Encode.list typeEncoder)) constructors )
        ]


unionDecoder : Decode.Decoder T.CASTS_Union
unionDecoder =
    Decode.map3 T.CASTS_Union
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


aliasEncoder : T.CASTS_Alias -> Encode.Value
aliasEncoder (T.CASTS_Alias name args tipe) =
    Encode.object
        [ ( "type", Encode.string "Alias" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "args", Encode.list (A.locatedEncoder Encode.string) args )
        , ( "tipe", typeEncoder tipe )
        ]


aliasDecoder : Decode.Decoder T.CASTS_Alias
aliasDecoder =
    Decode.map3 T.CASTS_Alias
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "args" (Decode.list (A.locatedDecoder Decode.string)))
        (Decode.field "tipe" typeDecoder)


infixEncoder : T.CASTS_Infix -> Encode.Value
infixEncoder (T.CASTS_Infix op associativity precedence name) =
    Encode.object
        [ ( "type", Encode.string "Infix" )
        , ( "op", Encode.string op )
        , ( "associativity", Binop.associativityEncoder associativity )
        , ( "precedence", Binop.precedenceEncoder precedence )
        , ( "name", Encode.string name )
        ]


infixDecoder : Decode.Decoder T.CASTS_Infix
infixDecoder =
    Decode.map4 T.CASTS_Infix
        (Decode.field "op" Decode.string)
        (Decode.field "associativity" Binop.associativityDecoder)
        (Decode.field "precedence" Binop.precedenceDecoder)
        (Decode.field "name" Decode.string)


effectsEncoder : T.CASTS_Effects -> Encode.Value
effectsEncoder effects =
    case effects of
        T.CASTS_NoEffects ->
            Encode.object
                [ ( "type", Encode.string "NoEffects" )
                ]

        T.CASTS_Ports ports ->
            Encode.object
                [ ( "type", Encode.string "Ports" )
                , ( "ports", Encode.list portEncoder ports )
                ]

        T.CASTS_Manager region manager ->
            Encode.object
                [ ( "type", Encode.string "Manager" )
                , ( "region", A.regionEncoder region )
                , ( "manager", managerEncoder manager )
                ]


effectsDecoder : Decode.Decoder T.CASTS_Effects
effectsDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoEffects" ->
                        Decode.succeed T.CASTS_NoEffects

                    "Ports" ->
                        Decode.map T.CASTS_Ports (Decode.field "ports" (Decode.list portDecoder))

                    "Manager" ->
                        Decode.map2 T.CASTS_Manager
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "manager" managerDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Effects' type: " ++ type_)
            )


commentEncoder : T.CASTS_Comment -> Encode.Value
commentEncoder (T.CASTS_Comment snippet) =
    P.snippetEncoder snippet


commentDecoder : Decode.Decoder T.CASTS_Comment
commentDecoder =
    Decode.map T.CASTS_Comment P.snippetDecoder


portEncoder : T.CASTS_Port -> Encode.Value
portEncoder (T.CASTS_Port name tipe) =
    Encode.object
        [ ( "type", Encode.string "Port" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "tipe", typeEncoder tipe )
        ]


portDecoder : Decode.Decoder T.CASTS_Port
portDecoder =
    Decode.map2 T.CASTS_Port
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "tipe" typeDecoder)


managerEncoder : T.CASTS_Manager -> Encode.Value
managerEncoder manager =
    case manager of
        T.CASTS_Cmd cmdType ->
            Encode.object
                [ ( "type", Encode.string "Cmd" )
                , ( "cmdType", A.locatedEncoder Encode.string cmdType )
                ]

        T.CASTS_Sub subType ->
            Encode.object
                [ ( "type", Encode.string "Sub" )
                , ( "subType", A.locatedEncoder Encode.string subType )
                ]

        T.CASTS_Fx cmdType subType ->
            Encode.object
                [ ( "type", Encode.string "Fx" )
                , ( "cmdType", A.locatedEncoder Encode.string cmdType )
                , ( "subType", A.locatedEncoder Encode.string subType )
                ]


managerDecoder : Decode.Decoder T.CASTS_Manager
managerDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Cmd" ->
                        Decode.map T.CASTS_Cmd (Decode.field "cmdType" (A.locatedDecoder Decode.string))

                    "Sub" ->
                        Decode.map T.CASTS_Sub (Decode.field "subType" (A.locatedDecoder Decode.string))

                    "Fx" ->
                        Decode.map2 T.CASTS_Fx
                            (Decode.field "cmdType" (A.locatedDecoder Decode.string))
                            (Decode.field "subType" (A.locatedDecoder Decode.string))

                    _ ->
                        Decode.fail ("Failed to decode Manager's type: " ++ type_)
            )


exposedEncoder : T.CASTS_Exposed -> Encode.Value
exposedEncoder exposed =
    case exposed of
        T.CASTS_Lower name ->
            Encode.object
                [ ( "type", Encode.string "Lower" )
                , ( "name", A.locatedEncoder Encode.string name )
                ]

        T.CASTS_Upper name dotDotRegion ->
            Encode.object
                [ ( "type", Encode.string "Upper" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "dotDotRegion", privacyEncoder dotDotRegion )
                ]

        T.CASTS_Operator region name ->
            Encode.object
                [ ( "type", Encode.string "Operator" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                ]


exposedDecoder : Decode.Decoder T.CASTS_Exposed
exposedDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Lower" ->
                        Decode.map T.CASTS_Lower (Decode.field "name" (A.locatedDecoder Decode.string))

                    "Upper" ->
                        Decode.map2 T.CASTS_Upper
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "dotDotRegion" privacyDecoder)

                    "Operator" ->
                        Decode.map2 T.CASTS_Operator
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail ("Failed to decode Exposed's type: " ++ type_)
            )


privacyEncoder : T.CASTS_Privacy -> Encode.Value
privacyEncoder privacy =
    case privacy of
        T.CASTS_Public region ->
            Encode.object
                [ ( "type", Encode.string "Public" )
                , ( "region", A.regionEncoder region )
                ]

        T.CASTS_Private ->
            Encode.object
                [ ( "type", Encode.string "Private" )
                ]


privacyDecoder : Decode.Decoder T.CASTS_Privacy
privacyDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Public" ->
                        Decode.map T.CASTS_Public (Decode.field "region" A.regionDecoder)

                    "Private" ->
                        Decode.succeed T.CASTS_Private

                    _ ->
                        Decode.fail ("Failed to decode Privacy's type: " ++ type_)
            )


patternEncoder : T.CASTS_Pattern -> Encode.Value
patternEncoder =
    A.locatedEncoder pattern_Encoder


patternDecoder : Decode.Decoder T.CASTS_Pattern
patternDecoder =
    A.locatedDecoder pattern_Decoder


pattern_Encoder : T.CASTS_Pattern_ -> Encode.Value
pattern_Encoder pattern_ =
    case pattern_ of
        T.CASTS_PAnything ->
            Encode.object
                [ ( "type", Encode.string "PAnything" )
                ]

        T.CASTS_PVar name ->
            Encode.object
                [ ( "type", Encode.string "PVar" )
                , ( "name", Encode.string name )
                ]

        T.CASTS_PRecord fields ->
            Encode.object
                [ ( "type", Encode.string "PRecord" )
                , ( "fields", Encode.list (A.locatedEncoder Encode.string) fields )
                ]

        T.CASTS_PAlias aliasPattern name ->
            Encode.object
                [ ( "type", Encode.string "PAlias" )
                , ( "aliasPattern", patternEncoder aliasPattern )
                , ( "name", A.locatedEncoder Encode.string name )
                ]

        T.CASTS_PUnit ->
            Encode.object
                [ ( "type", Encode.string "PUnit" )
                ]

        T.CASTS_PTuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "PTuple" )
                , ( "a", patternEncoder a )
                , ( "b", patternEncoder b )
                , ( "cs", Encode.list patternEncoder cs )
                ]

        T.CASTS_PCtor nameRegion name patterns ->
            Encode.object
                [ ( "type", Encode.string "PCtor" )
                , ( "nameRegion", A.regionEncoder nameRegion )
                , ( "name", Encode.string name )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        T.CASTS_PCtorQual nameRegion home name patterns ->
            Encode.object
                [ ( "type", Encode.string "PCtorQual" )
                , ( "nameRegion", A.regionEncoder nameRegion )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        T.CASTS_PList patterns ->
            Encode.object
                [ ( "type", Encode.string "PList" )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        T.CASTS_PCons hd tl ->
            Encode.object
                [ ( "type", Encode.string "PCons" )
                , ( "hd", patternEncoder hd )
                , ( "tl", patternEncoder tl )
                ]

        T.CASTS_PChr chr ->
            Encode.object
                [ ( "type", Encode.string "PChr" )
                , ( "chr", Encode.string chr )
                ]

        T.CASTS_PStr str ->
            Encode.object
                [ ( "type", Encode.string "PStr" )
                , ( "str", Encode.string str )
                ]

        T.CASTS_PInt int ->
            Encode.object
                [ ( "type", Encode.string "PInt" )
                , ( "int", Encode.int int )
                ]


pattern_Decoder : Decode.Decoder T.CASTS_Pattern_
pattern_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PAnything" ->
                        Decode.succeed T.CASTS_PAnything

                    "PVar" ->
                        Decode.map T.CASTS_PVar (Decode.field "name" Decode.string)

                    "PRecord" ->
                        Decode.map T.CASTS_PRecord (Decode.field "fields" (Decode.list (A.locatedDecoder Decode.string)))

                    "PAlias" ->
                        Decode.map2 T.CASTS_PAlias
                            (Decode.field "aliasPattern" patternDecoder)
                            (Decode.field "name" (A.locatedDecoder Decode.string))

                    "PUnit" ->
                        Decode.succeed T.CASTS_PUnit

                    "PTuple" ->
                        Decode.map3 T.CASTS_PTuple
                            (Decode.field "a" patternDecoder)
                            (Decode.field "b" patternDecoder)
                            (Decode.field "cs" (Decode.list patternDecoder))

                    "PCtor" ->
                        Decode.map3 T.CASTS_PCtor
                            (Decode.field "nameRegion" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "patterns" (Decode.list patternDecoder))

                    "PCtorQual" ->
                        Decode.map4 T.CASTS_PCtorQual
                            (Decode.field "nameRegion" A.regionDecoder)
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)
                            (Decode.field "patterns" (Decode.list patternDecoder))

                    "PList" ->
                        Decode.map T.CASTS_PList (Decode.field "patterns" (Decode.list patternDecoder))

                    "PCons" ->
                        Decode.map2 T.CASTS_PCons
                            (Decode.field "hd" patternDecoder)
                            (Decode.field "tl" patternDecoder)

                    "PChr" ->
                        Decode.map T.CASTS_PChr (Decode.field "chr" Decode.string)

                    "PStr" ->
                        Decode.map T.CASTS_PStr (Decode.field "str" Decode.string)

                    "PInt" ->
                        Decode.map T.CASTS_PInt (Decode.field "int" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Pattern_'s type: " ++ type_)
            )


exprEncoder : T.CASTS_Expr -> Encode.Value
exprEncoder =
    A.locatedEncoder expr_Encoder


exprDecoder : Decode.Decoder T.CASTS_Expr
exprDecoder =
    A.locatedDecoder expr_Decoder


expr_Encoder : T.CASTS_Expr_ -> Encode.Value
expr_Encoder expr_ =
    case expr_ of
        T.CASTS_Chr char ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "char", Encode.string char )
                ]

        T.CASTS_Str string ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "string", Encode.string string )
                ]

        T.CASTS_Int int ->
            Encode.object
                [ ( "type", Encode.string "Int" )
                , ( "int", Encode.int int )
                ]

        T.CASTS_Float float ->
            Encode.object
                [ ( "type", Encode.string "Float" )
                , ( "float", Encode.float float )
                ]

        T.CASTS_Var varType name ->
            Encode.object
                [ ( "type", Encode.string "Var" )
                , ( "varType", varTypeEncoder varType )
                , ( "name", Encode.string name )
                ]

        T.CASTS_VarQual varType prefix name ->
            Encode.object
                [ ( "type", Encode.string "VarQual" )
                , ( "varType", varTypeEncoder varType )
                , ( "prefix", Encode.string prefix )
                , ( "name", Encode.string name )
                ]

        T.CASTS_List list ->
            Encode.object
                [ ( "type", Encode.string "List" )
                , ( "list", Encode.list exprEncoder list )
                ]

        T.CASTS_Op op ->
            Encode.object
                [ ( "type", Encode.string "Op" )
                , ( "op", Encode.string op )
                ]

        T.CASTS_Negate expr ->
            Encode.object
                [ ( "type", Encode.string "Negate" )
                , ( "expr", exprEncoder expr )
                ]

        T.CASTS_Binops ops final ->
            Encode.object
                [ ( "type", Encode.string "Binops" )
                , ( "ops", Encode.list (E.jsonPair exprEncoder (A.locatedEncoder Encode.string)) ops )
                , ( "final", exprEncoder final )
                ]

        T.CASTS_Lambda srcArgs body ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "srcArgs", Encode.list patternEncoder srcArgs )
                , ( "body", exprEncoder body )
                ]

        T.CASTS_Call func args ->
            Encode.object
                [ ( "type", Encode.string "Call" )
                , ( "func", exprEncoder func )
                , ( "args", Encode.list exprEncoder args )
                ]

        T.CASTS_If branches finally ->
            Encode.object
                [ ( "type", Encode.string "If" )
                , ( "branches", Encode.list (E.jsonPair exprEncoder exprEncoder) branches )
                , ( "finally", exprEncoder finally )
                ]

        T.CASTS_Let defs expr ->
            Encode.object
                [ ( "type", Encode.string "Let" )
                , ( "defs", Encode.list (A.locatedEncoder defEncoder) defs )
                , ( "expr", exprEncoder expr )
                ]

        T.CASTS_Case expr branches ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                , ( "expr", exprEncoder expr )
                , ( "branches", Encode.list (E.jsonPair patternEncoder exprEncoder) branches )
                ]

        T.CASTS_Accessor field ->
            Encode.object
                [ ( "type", Encode.string "Accessor" )
                , ( "field", Encode.string field )
                ]

        T.CASTS_Access record field ->
            Encode.object
                [ ( "type", Encode.string "Access" )
                , ( "record", exprEncoder record )
                , ( "field", A.locatedEncoder Encode.string field )
                ]

        T.CASTS_Update name fields ->
            Encode.object
                [ ( "type", Encode.string "Update" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) exprEncoder) fields )
                ]

        T.CASTS_Record fields ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) exprEncoder) fields )
                ]

        T.CASTS_Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        T.CASTS_Tuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", exprEncoder a )
                , ( "b", exprEncoder b )
                , ( "cs", Encode.list exprEncoder cs )
                ]

        T.CASTS_Shader src tipe ->
            Encode.object
                [ ( "type", Encode.string "Shader" )
                , ( "src", Shader.sourceEncoder src )
                , ( "tipe", Shader.typesEncoder tipe )
                ]


expr_Decoder : Decode.Decoder T.CASTS_Expr_
expr_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Chr" ->
                        Decode.map T.CASTS_Chr (Decode.field "char" Decode.string)

                    "Str" ->
                        Decode.map T.CASTS_Str (Decode.field "string" Decode.string)

                    "Int" ->
                        Decode.map T.CASTS_Int (Decode.field "int" Decode.int)

                    "Float" ->
                        Decode.map T.CASTS_Float (Decode.field "float" Decode.float)

                    "Var" ->
                        Decode.map2 T.CASTS_Var
                            (Decode.field "varType" varTypeDecoder)
                            (Decode.field "name" Decode.string)

                    "VarQual" ->
                        Decode.map3 T.CASTS_VarQual
                            (Decode.field "varType" varTypeDecoder)
                            (Decode.field "prefix" Decode.string)
                            (Decode.field "name" Decode.string)

                    "List" ->
                        Decode.map T.CASTS_List (Decode.field "list" (Decode.list exprDecoder))

                    "Op" ->
                        Decode.map T.CASTS_Op (Decode.field "op" Decode.string)

                    "Negate" ->
                        Decode.map T.CASTS_Negate (Decode.field "expr" exprDecoder)

                    "Binops" ->
                        Decode.map2 T.CASTS_Binops
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
                        Decode.map2 T.CASTS_Lambda
                            (Decode.field "srcArgs" (Decode.list patternDecoder))
                            (Decode.field "body" exprDecoder)

                    "Call" ->
                        Decode.map2 T.CASTS_Call
                            (Decode.field "func" exprDecoder)
                            (Decode.field "args" (Decode.list exprDecoder))

                    "If" ->
                        Decode.map2 T.CASTS_If
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
                        Decode.map2 T.CASTS_Let
                            (Decode.field "defs" (Decode.list (A.locatedDecoder defDecoder)))
                            (Decode.field "expr" exprDecoder)

                    "Case" ->
                        Decode.map2 T.CASTS_Case
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
                        Decode.map T.CASTS_Accessor (Decode.field "field" Decode.string)

                    "Access" ->
                        Decode.map2 T.CASTS_Access
                            (Decode.field "record" exprDecoder)
                            (Decode.field "field" (A.locatedDecoder Decode.string))

                    "Update" ->
                        Decode.map2 T.CASTS_Update
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
                        Decode.map T.CASTS_Record
                            (Decode.field "fields"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" (A.locatedDecoder Decode.string))
                                        (Decode.field "b" exprDecoder)
                                    )
                                )
                            )

                    "Unit" ->
                        Decode.succeed T.CASTS_Unit

                    "Tuple" ->
                        Decode.map3 T.CASTS_Tuple
                            (Decode.field "a" exprDecoder)
                            (Decode.field "b" exprDecoder)
                            (Decode.field "cs" (Decode.list exprDecoder))

                    "Shader" ->
                        Decode.map2 T.CASTS_Shader
                            (Decode.field "src" Shader.sourceDecoder)
                            (Decode.field "tipe" Shader.typesDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Expr_'s type: " ++ type_)
            )


varTypeEncoder : T.CASTS_VarType -> Encode.Value
varTypeEncoder varType =
    case varType of
        T.CASTS_LowVar ->
            Encode.string "LowVar"

        T.CASTS_CapVar ->
            Encode.string "CapVar"


varTypeDecoder : Decode.Decoder T.CASTS_VarType
varTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "LowVar" ->
                        Decode.succeed T.CASTS_LowVar

                    "CapVar" ->
                        Decode.succeed T.CASTS_CapVar

                    _ ->
                        Decode.fail ("Unknown VarType: " ++ str)
            )


defEncoder : T.CASTS_Def -> Encode.Value
defEncoder def =
    case def of
        T.CASTS_Define name srcArgs body maybeType ->
            Encode.object
                [ ( "type", Encode.string "Define" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "srcArgs", Encode.list patternEncoder srcArgs )
                , ( "body", exprEncoder body )
                , ( "maybeType", E.maybe typeEncoder maybeType )
                ]

        T.CASTS_Destruct pattern body ->
            Encode.object
                [ ( "type", Encode.string "Destruct" )
                , ( "pattern", patternEncoder pattern )
                , ( "body", exprEncoder body )
                ]


defDecoder : Decode.Decoder T.CASTS_Def
defDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Define" ->
                        Decode.map4 T.CASTS_Define
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "srcArgs" (Decode.list patternDecoder))
                            (Decode.field "body" exprDecoder)
                            (Decode.field "maybeType" (Decode.maybe typeDecoder))

                    "Destruct" ->
                        Decode.map2 T.CASTS_Destruct
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "body" exprDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Def's type: " ++ type_)
            )
