module Compiler.AST.Source exposing
    ( Alias(..)
    , Comment(..)
    , Def(..)
    , Docs(..)
    , Effects(..)
    , Exposed(..)
    , Exposing(..)
    , Expr
    , Expr_(..)
    , Import(..)
    , Infix(..)
    , Manager(..)
    , Module(..)
    , Pattern
    , Pattern_(..)
    , Port(..)
    , Privacy(..)
    , Type
    , Type_(..)
    , Union(..)
    , Value(..)
    , VarType(..)
    , getImportName
    , getName
    , moduleDecoder
    , moduleEncoder
    , typeDecoder
    , typeEncoder
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Annotation as A
import Json.Decode as Decode
import Json.Encode as Encode



-- EXPRESSIONS


type alias Expr =
    A.Located Expr_


type Expr_
    = Chr String
    | Str String
    | Int Int
    | Float Float
    | Var VarType Name
    | VarQual VarType Name Name
    | List (List Expr)
    | Op Name
    | Negate Expr
    | Binops (List ( Expr, A.Located Name )) Expr
    | Lambda (List Pattern) Expr
    | Call Expr (List Expr)
    | If (List ( Expr, Expr )) Expr
    | Let (List (A.Located Def)) Expr
    | Case Expr (List ( Pattern, Expr ))
    | Accessor Name
    | Access Expr (A.Located Name)
    | Update (A.Located Name) (List ( A.Located Name, Expr ))
    | Record (List ( A.Located Name, Expr ))
    | Unit
    | Tuple Expr Expr (List Expr)
    | Shader Shader.Source Shader.Types


type VarType
    = LowVar
    | CapVar



-- DEFINITIONS


type Def
    = Define (A.Located Name) (List Pattern) Expr (Maybe Type)
    | Destruct Pattern Expr



-- PATTERN


type alias Pattern =
    A.Located Pattern_


type Pattern_
    = PAnything Name
    | PVar Name
    | PRecord (List (A.Located Name))
    | PAlias Pattern (A.Located Name)
    | PUnit
    | PTuple Pattern Pattern (List Pattern)
    | PCtor A.Region Name (List Pattern)
    | PCtorQual A.Region Name Name (List Pattern)
    | PList (List Pattern)
    | PCons Pattern Pattern
    | PChr String
    | PStr String
    | PInt Int



-- TYPE


type alias Type =
    A.Located Type_


type Type_
    = TLambda Type Type
    | TVar Name
    | TType A.Region Name (List Type)
    | TTypeQual A.Region Name Name (List Type)
    | TRecord (List ( A.Located Name, Type )) (Maybe (A.Located Name))
    | TUnit
    | TTuple Type Type (List Type)



-- MODULE


type Module
    = Module (Maybe (A.Located Name)) (A.Located Exposing) Docs (List Import) (List (A.Located Value)) (List (A.Located Union)) (List (A.Located Alias)) (List (A.Located Infix)) Effects


getName : Module -> Name
getName (Module maybeName _ _ _ _ _ _ _ _) =
    case maybeName of
        Just (A.At _ name) ->
            name

        Nothing ->
            Name.mainModule


getImportName : Import -> Name
getImportName (Import (A.At _ name) _ _) =
    name


type Import
    = Import (A.Located Name) (Maybe Name.Name) Exposing


type Value
    = Value (A.Located Name) (List Pattern) Expr (Maybe Type)


type Union
    = Union (A.Located Name) (List (A.Located Name)) (List ( A.Located Name, List Type ))


type Alias
    = Alias (A.Located Name) (List (A.Located Name)) Type


type Infix
    = Infix Name Binop.Associativity Binop.Precedence Name


type Port
    = Port (A.Located Name) Type


type Effects
    = NoEffects
    | Ports (List Port)
    | Manager A.Region Manager


type Manager
    = Cmd (A.Located Name)
    | Sub (A.Located Name)
    | Fx (A.Located Name) (A.Located Name)


type Docs
    = NoDocs A.Region
    | YesDocs Comment (List ( Name, Comment ))


type Comment
    = Comment P.Snippet



-- EXPOSING


type Exposing
    = Open
    | Explicit (List Exposed)


type Exposed
    = Lower (A.Located Name)
    | Upper (A.Located Name) Privacy
    | Operator A.Region Name


type Privacy
    = Public A.Region
    | Private



-- ENCODERS and DECODERS


typeEncoder : Type -> Encode.Value
typeEncoder =
    A.locatedEncoder internalTypeEncoder


typeDecoder : Decode.Decoder Type
typeDecoder =
    A.locatedDecoder internalTypeDecoder


internalTypeEncoder : Type_ -> Encode.Value
internalTypeEncoder type_ =
    case type_ of
        TLambda arg result ->
            Encode.object
                [ ( "type", Encode.string "TLambda" )
                , ( "arg", typeEncoder arg )
                , ( "result", typeEncoder result )
                ]

        TVar name ->
            Encode.object
                [ ( "type", Encode.string "TVar" )
                , ( "name", Encode.string name )
                ]

        TType region name args ->
            Encode.object
                [ ( "type", Encode.string "TType" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        TTypeQual region home name args ->
            Encode.object
                [ ( "type", Encode.string "TTypeQual" )
                , ( "region", A.regionEncoder region )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        TRecord fields ext ->
            Encode.object
                [ ( "type", Encode.string "TRecord" )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) typeEncoder) fields )
                , ( "ext", E.maybe (A.locatedEncoder Encode.string) ext )
                ]

        TUnit ->
            Encode.object
                [ ( "type", Encode.string "TUnit" )
                ]

        TTuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "TTuple" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                , ( "cs", Encode.list typeEncoder cs )
                ]


internalTypeDecoder : Decode.Decoder Type_
internalTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "TLambda" ->
                        Decode.map2 TLambda
                            (Decode.field "arg" typeDecoder)
                            (Decode.field "result" typeDecoder)

                    "TVar" ->
                        Decode.map TVar (Decode.field "name" Decode.string)

                    "TType" ->
                        Decode.map3 TType
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "TTypeQual" ->
                        Decode.map4 TTypeQual
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "TRecord" ->
                        Decode.map2 TRecord
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
                        Decode.succeed TUnit

                    "TTuple" ->
                        Decode.map3 TTuple
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)
                            (Decode.field "cs" (Decode.list typeDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Type_'s type: " ++ type_)
            )


moduleEncoder : Module -> Encode.Value
moduleEncoder (Module maybeName exports docs imports values unions aliases binops effects) =
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


moduleDecoder : Decode.Decoder Module
moduleDecoder =
    Decode.map8 (\( maybeName, exports ) -> Module maybeName exports)
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


exposingEncoder : Exposing -> Encode.Value
exposingEncoder exposing_ =
    case exposing_ of
        Open ->
            Encode.object
                [ ( "type", Encode.string "Open" )
                ]

        Explicit exposedList ->
            Encode.object
                [ ( "type", Encode.string "Explicit" )
                , ( "exposedList", Encode.list exposedEncoder exposedList )
                ]


exposingDecoder : Decode.Decoder Exposing
exposingDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Open" ->
                        Decode.succeed Open

                    "Explicit" ->
                        Decode.map Explicit (Decode.field "exposedList" (Decode.list exposedDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Exposing's type: " ++ type_)
            )


docsEncoder : Docs -> Encode.Value
docsEncoder docs =
    case docs of
        NoDocs region ->
            Encode.object
                [ ( "type", Encode.string "NoDocs" )
                , ( "region", A.regionEncoder region )
                ]

        YesDocs overview comments ->
            Encode.object
                [ ( "type", Encode.string "YesDocs" )
                , ( "overview", commentEncoder overview )
                , ( "comments", Encode.list (E.jsonPair Encode.string commentEncoder) comments )
                ]


docsDecoder : Decode.Decoder Docs
docsDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoDocs" ->
                        Decode.map NoDocs (Decode.field "region" A.regionDecoder)

                    "YesDocs" ->
                        Decode.map2 YesDocs
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


importEncoder : Import -> Encode.Value
importEncoder (Import importName maybeAlias exposing_) =
    Encode.object
        [ ( "type", Encode.string "Import" )
        , ( "importName", A.locatedEncoder Encode.string importName )
        , ( "maybeAlias", E.maybe Encode.string maybeAlias )
        , ( "exposing", exposingEncoder exposing_ )
        ]


importDecoder : Decode.Decoder Import
importDecoder =
    Decode.map3 Import
        (Decode.field "importName" (A.locatedDecoder Decode.string))
        (Decode.field "maybeAlias" (Decode.maybe Decode.string))
        (Decode.field "exposing" exposingDecoder)


valueEncoder : Value -> Encode.Value
valueEncoder (Value name srcArgs body maybeType) =
    Encode.object
        [ ( "type", Encode.string "Value" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "srcArgs", Encode.list patternEncoder srcArgs )
        , ( "body", exprEncoder body )
        , ( "maybeType", E.maybe typeEncoder maybeType )
        ]


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.map4 Value
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "srcArgs" (Decode.list patternDecoder))
        (Decode.field "body" exprDecoder)
        (Decode.field "maybeType" (Decode.maybe typeDecoder))


unionEncoder : Union -> Encode.Value
unionEncoder (Union name args constructors) =
    Encode.object
        [ ( "type", Encode.string "Union" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "args", Encode.list (A.locatedEncoder Encode.string) args )
        , ( "constructors", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) (Encode.list typeEncoder)) constructors )
        ]


unionDecoder : Decode.Decoder Union
unionDecoder =
    Decode.map3 Union
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


aliasEncoder : Alias -> Encode.Value
aliasEncoder (Alias name args tipe) =
    Encode.object
        [ ( "type", Encode.string "Alias" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "args", Encode.list (A.locatedEncoder Encode.string) args )
        , ( "tipe", typeEncoder tipe )
        ]


aliasDecoder : Decode.Decoder Alias
aliasDecoder =
    Decode.map3 Alias
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "args" (Decode.list (A.locatedDecoder Decode.string)))
        (Decode.field "tipe" typeDecoder)


infixEncoder : Infix -> Encode.Value
infixEncoder (Infix op associativity precedence name) =
    Encode.object
        [ ( "type", Encode.string "Infix" )
        , ( "op", Encode.string op )
        , ( "associativity", Binop.associativityEncoder associativity )
        , ( "precedence", Binop.precedenceEncoder precedence )
        , ( "name", Encode.string name )
        ]


infixDecoder : Decode.Decoder Infix
infixDecoder =
    Decode.map4 Infix
        (Decode.field "op" Decode.string)
        (Decode.field "associativity" Binop.associativityDecoder)
        (Decode.field "precedence" Binop.precedenceDecoder)
        (Decode.field "name" Decode.string)


effectsEncoder : Effects -> Encode.Value
effectsEncoder effects =
    case effects of
        NoEffects ->
            Encode.object
                [ ( "type", Encode.string "NoEffects" )
                ]

        Ports ports ->
            Encode.object
                [ ( "type", Encode.string "Ports" )
                , ( "ports", Encode.list portEncoder ports )
                ]

        Manager region manager ->
            Encode.object
                [ ( "type", Encode.string "Manager" )
                , ( "region", A.regionEncoder region )
                , ( "manager", managerEncoder manager )
                ]


effectsDecoder : Decode.Decoder Effects
effectsDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoEffects" ->
                        Decode.succeed NoEffects

                    "Ports" ->
                        Decode.map Ports (Decode.field "ports" (Decode.list portDecoder))

                    "Manager" ->
                        Decode.map2 Manager
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "manager" managerDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Effects' type: " ++ type_)
            )


commentEncoder : Comment -> Encode.Value
commentEncoder (Comment snippet) =
    P.snippetEncoder snippet


commentDecoder : Decode.Decoder Comment
commentDecoder =
    Decode.map Comment P.snippetDecoder


portEncoder : Port -> Encode.Value
portEncoder (Port name tipe) =
    Encode.object
        [ ( "type", Encode.string "Port" )
        , ( "name", A.locatedEncoder Encode.string name )
        , ( "tipe", typeEncoder tipe )
        ]


portDecoder : Decode.Decoder Port
portDecoder =
    Decode.map2 Port
        (Decode.field "name" (A.locatedDecoder Decode.string))
        (Decode.field "tipe" typeDecoder)


managerEncoder : Manager -> Encode.Value
managerEncoder manager =
    case manager of
        Cmd cmdType ->
            Encode.object
                [ ( "type", Encode.string "Cmd" )
                , ( "cmdType", A.locatedEncoder Encode.string cmdType )
                ]

        Sub subType ->
            Encode.object
                [ ( "type", Encode.string "Sub" )
                , ( "subType", A.locatedEncoder Encode.string subType )
                ]

        Fx cmdType subType ->
            Encode.object
                [ ( "type", Encode.string "Fx" )
                , ( "cmdType", A.locatedEncoder Encode.string cmdType )
                , ( "subType", A.locatedEncoder Encode.string subType )
                ]


managerDecoder : Decode.Decoder Manager
managerDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Cmd" ->
                        Decode.map Cmd (Decode.field "cmdType" (A.locatedDecoder Decode.string))

                    "Sub" ->
                        Decode.map Sub (Decode.field "subType" (A.locatedDecoder Decode.string))

                    "Fx" ->
                        Decode.map2 Fx
                            (Decode.field "cmdType" (A.locatedDecoder Decode.string))
                            (Decode.field "subType" (A.locatedDecoder Decode.string))

                    _ ->
                        Decode.fail ("Failed to decode Manager's type: " ++ type_)
            )


exposedEncoder : Exposed -> Encode.Value
exposedEncoder exposed =
    case exposed of
        Lower name ->
            Encode.object
                [ ( "type", Encode.string "Lower" )
                , ( "name", A.locatedEncoder Encode.string name )
                ]

        Upper name dotDotRegion ->
            Encode.object
                [ ( "type", Encode.string "Upper" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "dotDotRegion", privacyEncoder dotDotRegion )
                ]

        Operator region name ->
            Encode.object
                [ ( "type", Encode.string "Operator" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                ]


exposedDecoder : Decode.Decoder Exposed
exposedDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Lower" ->
                        Decode.map Lower (Decode.field "name" (A.locatedDecoder Decode.string))

                    "Upper" ->
                        Decode.map2 Upper
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "dotDotRegion" privacyDecoder)

                    "Operator" ->
                        Decode.map2 Operator
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail ("Failed to decode Exposed's type: " ++ type_)
            )


privacyEncoder : Privacy -> Encode.Value
privacyEncoder privacy =
    case privacy of
        Public region ->
            Encode.object
                [ ( "type", Encode.string "Public" )
                , ( "region", A.regionEncoder region )
                ]

        Private ->
            Encode.object
                [ ( "type", Encode.string "Private" )
                ]


privacyDecoder : Decode.Decoder Privacy
privacyDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Public" ->
                        Decode.map Public (Decode.field "region" A.regionDecoder)

                    "Private" ->
                        Decode.succeed Private

                    _ ->
                        Decode.fail ("Failed to decode Privacy's type: " ++ type_)
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
        PAnything name ->
            Encode.object
                [ ( "type", Encode.string "PAnything" )
                , ( "name", Encode.string name )
                ]

        PVar name ->
            Encode.object
                [ ( "type", Encode.string "PVar" )
                , ( "name", Encode.string name )
                ]

        PRecord fields ->
            Encode.object
                [ ( "type", Encode.string "PRecord" )
                , ( "fields", Encode.list (A.locatedEncoder Encode.string) fields )
                ]

        PAlias aliasPattern name ->
            Encode.object
                [ ( "type", Encode.string "PAlias" )
                , ( "aliasPattern", patternEncoder aliasPattern )
                , ( "name", A.locatedEncoder Encode.string name )
                ]

        PUnit ->
            Encode.object
                [ ( "type", Encode.string "PUnit" )
                ]

        PTuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "PTuple" )
                , ( "a", patternEncoder a )
                , ( "b", patternEncoder b )
                , ( "cs", Encode.list patternEncoder cs )
                ]

        PCtor nameRegion name patterns ->
            Encode.object
                [ ( "type", Encode.string "PCtor" )
                , ( "nameRegion", A.regionEncoder nameRegion )
                , ( "name", Encode.string name )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        PCtorQual nameRegion home name patterns ->
            Encode.object
                [ ( "type", Encode.string "PCtorQual" )
                , ( "nameRegion", A.regionEncoder nameRegion )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        PList patterns ->
            Encode.object
                [ ( "type", Encode.string "PList" )
                , ( "patterns", Encode.list patternEncoder patterns )
                ]

        PCons hd tl ->
            Encode.object
                [ ( "type", Encode.string "PCons" )
                , ( "hd", patternEncoder hd )
                , ( "tl", patternEncoder tl )
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


pattern_Decoder : Decode.Decoder Pattern_
pattern_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PAnything" ->
                        Decode.map PAnything
                            (Decode.field "name" Decode.string)

                    "PVar" ->
                        Decode.map PVar (Decode.field "name" Decode.string)

                    "PRecord" ->
                        Decode.map PRecord (Decode.field "fields" (Decode.list (A.locatedDecoder Decode.string)))

                    "PAlias" ->
                        Decode.map2 PAlias
                            (Decode.field "aliasPattern" patternDecoder)
                            (Decode.field "name" (A.locatedDecoder Decode.string))

                    "PUnit" ->
                        Decode.succeed PUnit

                    "PTuple" ->
                        Decode.map3 PTuple
                            (Decode.field "a" patternDecoder)
                            (Decode.field "b" patternDecoder)
                            (Decode.field "cs" (Decode.list patternDecoder))

                    "PCtor" ->
                        Decode.map3 PCtor
                            (Decode.field "nameRegion" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "patterns" (Decode.list patternDecoder))

                    "PCtorQual" ->
                        Decode.map4 PCtorQual
                            (Decode.field "nameRegion" A.regionDecoder)
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)
                            (Decode.field "patterns" (Decode.list patternDecoder))

                    "PList" ->
                        Decode.map PList (Decode.field "patterns" (Decode.list patternDecoder))

                    "PCons" ->
                        Decode.map2 PCons
                            (Decode.field "hd" patternDecoder)
                            (Decode.field "tl" patternDecoder)

                    "PChr" ->
                        Decode.map PChr (Decode.field "chr" Decode.string)

                    "PStr" ->
                        Decode.map PStr (Decode.field "str" Decode.string)

                    "PInt" ->
                        Decode.map PInt (Decode.field "int" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode Pattern_'s type: " ++ type_)
            )


exprEncoder : Expr -> Encode.Value
exprEncoder =
    A.locatedEncoder expr_Encoder


exprDecoder : Decode.Decoder Expr
exprDecoder =
    A.locatedDecoder expr_Decoder


expr_Encoder : Expr_ -> Encode.Value
expr_Encoder expr_ =
    case expr_ of
        Chr char ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "char", Encode.string char )
                ]

        Str string ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "string", Encode.string string )
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

        Var varType name ->
            Encode.object
                [ ( "type", Encode.string "Var" )
                , ( "varType", varTypeEncoder varType )
                , ( "name", Encode.string name )
                ]

        VarQual varType prefix name ->
            Encode.object
                [ ( "type", Encode.string "VarQual" )
                , ( "varType", varTypeEncoder varType )
                , ( "prefix", Encode.string prefix )
                , ( "name", Encode.string name )
                ]

        List list ->
            Encode.object
                [ ( "type", Encode.string "List" )
                , ( "list", Encode.list exprEncoder list )
                ]

        Op op ->
            Encode.object
                [ ( "type", Encode.string "Op" )
                , ( "op", Encode.string op )
                ]

        Negate expr ->
            Encode.object
                [ ( "type", Encode.string "Negate" )
                , ( "expr", exprEncoder expr )
                ]

        Binops ops final ->
            Encode.object
                [ ( "type", Encode.string "Binops" )
                , ( "ops", Encode.list (E.jsonPair exprEncoder (A.locatedEncoder Encode.string)) ops )
                , ( "final", exprEncoder final )
                ]

        Lambda srcArgs body ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "srcArgs", Encode.list patternEncoder srcArgs )
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

        Let defs expr ->
            Encode.object
                [ ( "type", Encode.string "Let" )
                , ( "defs", Encode.list (A.locatedEncoder defEncoder) defs )
                , ( "expr", exprEncoder expr )
                ]

        Case expr branches ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                , ( "expr", exprEncoder expr )
                , ( "branches", Encode.list (E.jsonPair patternEncoder exprEncoder) branches )
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

        Update name fields ->
            Encode.object
                [ ( "type", Encode.string "Update" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) exprEncoder) fields )
                ]

        Record fields ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "fields", Encode.list (E.jsonPair (A.locatedEncoder Encode.string) exprEncoder) fields )
                ]

        Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        Tuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", exprEncoder a )
                , ( "b", exprEncoder b )
                , ( "cs", Encode.list exprEncoder cs )
                ]

        Shader src tipe ->
            Encode.object
                [ ( "type", Encode.string "Shader" )
                , ( "src", Shader.sourceEncoder src )
                , ( "tipe", Shader.typesEncoder tipe )
                ]


expr_Decoder : Decode.Decoder Expr_
expr_Decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Chr" ->
                        Decode.map Chr (Decode.field "char" Decode.string)

                    "Str" ->
                        Decode.map Str (Decode.field "string" Decode.string)

                    "Int" ->
                        Decode.map Int (Decode.field "int" Decode.int)

                    "Float" ->
                        Decode.map Float (Decode.field "float" Decode.float)

                    "Var" ->
                        Decode.map2 Var
                            (Decode.field "varType" varTypeDecoder)
                            (Decode.field "name" Decode.string)

                    "VarQual" ->
                        Decode.map3 VarQual
                            (Decode.field "varType" varTypeDecoder)
                            (Decode.field "prefix" Decode.string)
                            (Decode.field "name" Decode.string)

                    "List" ->
                        Decode.map List (Decode.field "list" (Decode.list exprDecoder))

                    "Op" ->
                        Decode.map Op (Decode.field "op" Decode.string)

                    "Negate" ->
                        Decode.map Negate (Decode.field "expr" exprDecoder)

                    "Binops" ->
                        Decode.map2 Binops
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
                        Decode.map2 Lambda
                            (Decode.field "srcArgs" (Decode.list patternDecoder))
                            (Decode.field "body" exprDecoder)

                    "Call" ->
                        Decode.map2 Call
                            (Decode.field "func" exprDecoder)
                            (Decode.field "args" (Decode.list exprDecoder))

                    "If" ->
                        Decode.map2 If
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
                        Decode.map2 Let
                            (Decode.field "defs" (Decode.list (A.locatedDecoder defDecoder)))
                            (Decode.field "expr" exprDecoder)

                    "Case" ->
                        Decode.map2 Case
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
                        Decode.map Accessor (Decode.field "field" Decode.string)

                    "Access" ->
                        Decode.map2 Access
                            (Decode.field "record" exprDecoder)
                            (Decode.field "field" (A.locatedDecoder Decode.string))

                    "Update" ->
                        Decode.map2 Update
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
                        Decode.map Record
                            (Decode.field "fields"
                                (Decode.list
                                    (Decode.map2 Tuple.pair
                                        (Decode.field "a" (A.locatedDecoder Decode.string))
                                        (Decode.field "b" exprDecoder)
                                    )
                                )
                            )

                    "Unit" ->
                        Decode.succeed Unit

                    "Tuple" ->
                        Decode.map3 Tuple
                            (Decode.field "a" exprDecoder)
                            (Decode.field "b" exprDecoder)
                            (Decode.field "cs" (Decode.list exprDecoder))

                    "Shader" ->
                        Decode.map2 Shader
                            (Decode.field "src" Shader.sourceDecoder)
                            (Decode.field "tipe" Shader.typesDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Expr_'s type: " ++ type_)
            )


varTypeEncoder : VarType -> Encode.Value
varTypeEncoder varType =
    case varType of
        LowVar ->
            Encode.string "LowVar"

        CapVar ->
            Encode.string "CapVar"


varTypeDecoder : Decode.Decoder VarType
varTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "LowVar" ->
                        Decode.succeed LowVar

                    "CapVar" ->
                        Decode.succeed CapVar

                    _ ->
                        Decode.fail ("Unknown VarType: " ++ str)
            )


defEncoder : Def -> Encode.Value
defEncoder def =
    case def of
        Define name srcArgs body maybeType ->
            Encode.object
                [ ( "type", Encode.string "Define" )
                , ( "name", A.locatedEncoder Encode.string name )
                , ( "srcArgs", Encode.list patternEncoder srcArgs )
                , ( "body", exprEncoder body )
                , ( "maybeType", E.maybe typeEncoder maybeType )
                ]

        Destruct pattern body ->
            Encode.object
                [ ( "type", Encode.string "Destruct" )
                , ( "pattern", patternEncoder pattern )
                , ( "body", exprEncoder body )
                ]


defDecoder : Decode.Decoder Def
defDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Define" ->
                        Decode.map4 Define
                            (Decode.field "name" (A.locatedDecoder Decode.string))
                            (Decode.field "srcArgs" (Decode.list patternDecoder))
                            (Decode.field "body" exprDecoder)
                            (Decode.field "maybeType" (Decode.maybe typeDecoder))

                    "Destruct" ->
                        Decode.map2 Destruct
                            (Decode.field "pattern" patternDecoder)
                            (Decode.field "body" exprDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Def's type: " ++ type_)
            )
