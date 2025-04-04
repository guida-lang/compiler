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
import Compiler.Parse.Primitives as P
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



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
    | Update (A.Located ( Maybe Name, Name )) (List ( A.Located Name, Expr ))
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
    = Module SyntaxVersion (Maybe (A.Located Name)) (A.Located Exposing) Docs (List Import) (List (A.Located Value)) (List (A.Located Union)) (List (A.Located Alias)) (List (A.Located Infix)) Effects


getName : Module -> Name
getName (Module _ maybeName _ _ _ _ _ _ _ _) =
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


typeEncoder : Type -> BE.Encoder
typeEncoder =
    A.locatedEncoder internalTypeEncoder


typeDecoder : BD.Decoder Type
typeDecoder =
    A.locatedDecoder internalTypeDecoder


internalTypeEncoder : Type_ -> BE.Encoder
internalTypeEncoder type_ =
    case type_ of
        TLambda arg result ->
            BE.sequence
                [ BE.unsignedInt8 0
                , typeEncoder arg
                , typeEncoder result
                ]

        TVar name ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string name
                ]

        TType region name args ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.regionEncoder region
                , BE.string name
                , BE.list typeEncoder args
                ]

        TTypeQual region home name args ->
            BE.sequence
                [ BE.unsignedInt8 3
                , A.regionEncoder region
                , BE.string home
                , BE.string name
                , BE.list typeEncoder args
                ]

        TRecord fields ext ->
            BE.sequence
                [ BE.unsignedInt8 4
                , BE.list (BE.jsonPair (A.locatedEncoder BE.string) typeEncoder) fields
                , BE.maybe (A.locatedEncoder BE.string) ext
                ]

        TUnit ->
            BE.unsignedInt8 5

        TTuple a b cs ->
            BE.sequence
                [ BE.unsignedInt8 6
                , typeEncoder a
                , typeEncoder b
                , BE.list typeEncoder cs
                ]


internalTypeDecoder : BD.Decoder Type_
internalTypeDecoder =
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
                            A.regionDecoder
                            BD.string
                            (BD.list typeDecoder)

                    3 ->
                        BD.map4 TTypeQual
                            A.regionDecoder
                            BD.string
                            BD.string
                            (BD.list typeDecoder)

                    4 ->
                        BD.map2 TRecord
                            (BD.list (BD.jsonPair (A.locatedDecoder BD.string) typeDecoder))
                            (BD.maybe (A.locatedDecoder BD.string))

                    5 ->
                        BD.succeed TUnit

                    6 ->
                        BD.map3 TTuple
                            typeDecoder
                            typeDecoder
                            (BD.list typeDecoder)

                    _ ->
                        BD.fail
            )


moduleEncoder : Module -> BE.Encoder
moduleEncoder (Module syntaxVersion maybeName exports docs imports values unions aliases binops effects) =
    BE.sequence
        [ SV.encoder syntaxVersion
        , BE.maybe (A.locatedEncoder BE.string) maybeName
        , A.locatedEncoder exposingEncoder exports
        , docsEncoder docs
        , BE.list importEncoder imports
        , BE.list (A.locatedEncoder valueEncoder) values
        , BE.list (A.locatedEncoder unionEncoder) unions
        , BE.list (A.locatedEncoder aliasEncoder) aliases
        , BE.list (A.locatedEncoder infixEncoder) binops
        , effectsEncoder effects
        ]


moduleDecoder : BD.Decoder Module
moduleDecoder =
    BD.map8 (\( syntaxVersion, maybeName ) ( exports, docs ) -> Module syntaxVersion maybeName exports docs)
        (BD.jsonPair SV.decoder (BD.maybe (A.locatedDecoder BD.string)))
        (BD.jsonPair (A.locatedDecoder exposingDecoder) docsDecoder)
        (BD.list importDecoder)
        (BD.list (A.locatedDecoder valueDecoder))
        (BD.list (A.locatedDecoder unionDecoder))
        (BD.list (A.locatedDecoder aliasDecoder))
        (BD.list (A.locatedDecoder infixDecoder))
        effectsDecoder


exposingEncoder : Exposing -> BE.Encoder
exposingEncoder exposing_ =
    case exposing_ of
        Open ->
            BE.unsignedInt8 0

        Explicit exposedList ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.list exposedEncoder exposedList
                ]


exposingDecoder : BD.Decoder Exposing
exposingDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Open

                    1 ->
                        BD.map Explicit (BD.list exposedDecoder)

                    _ ->
                        BD.fail
            )


docsEncoder : Docs -> BE.Encoder
docsEncoder docs =
    case docs of
        NoDocs region ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                ]

        YesDocs overview comments ->
            BE.sequence
                [ BE.unsignedInt8 1
                , commentEncoder overview
                , BE.list (BE.jsonPair BE.string commentEncoder) comments
                ]


docsDecoder : BD.Decoder Docs
docsDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map NoDocs A.regionDecoder

                    1 ->
                        BD.map2 YesDocs
                            commentDecoder
                            (BD.list (BD.jsonPair BD.string commentDecoder))

                    _ ->
                        BD.fail
            )


importEncoder : Import -> BE.Encoder
importEncoder (Import importName maybeAlias exposing_) =
    BE.sequence
        [ A.locatedEncoder BE.string importName
        , BE.maybe BE.string maybeAlias
        , exposingEncoder exposing_
        ]


importDecoder : BD.Decoder Import
importDecoder =
    BD.map3 Import
        (A.locatedDecoder BD.string)
        (BD.maybe BD.string)
        exposingDecoder


valueEncoder : Value -> BE.Encoder
valueEncoder (Value name srcArgs body maybeType) =
    BE.sequence
        [ A.locatedEncoder BE.string name
        , BE.list patternEncoder srcArgs
        , exprEncoder body
        , BE.maybe typeEncoder maybeType
        ]


valueDecoder : BD.Decoder Value
valueDecoder =
    BD.map4 Value
        (A.locatedDecoder BD.string)
        (BD.list patternDecoder)
        exprDecoder
        (BD.maybe typeDecoder)


unionEncoder : Union -> BE.Encoder
unionEncoder (Union name args constructors) =
    BE.sequence
        [ A.locatedEncoder BE.string name
        , BE.list (A.locatedEncoder BE.string) args
        , BE.list (BE.jsonPair (A.locatedEncoder BE.string) (BE.list typeEncoder)) constructors
        ]


unionDecoder : BD.Decoder Union
unionDecoder =
    BD.map3 Union
        (A.locatedDecoder BD.string)
        (BD.list (A.locatedDecoder BD.string))
        (BD.list (BD.jsonPair (A.locatedDecoder BD.string) (BD.list typeDecoder)))


aliasEncoder : Alias -> BE.Encoder
aliasEncoder (Alias name args tipe) =
    BE.sequence
        [ A.locatedEncoder BE.string name
        , BE.list (A.locatedEncoder BE.string) args
        , typeEncoder tipe
        ]


aliasDecoder : BD.Decoder Alias
aliasDecoder =
    BD.map3 Alias
        (A.locatedDecoder BD.string)
        (BD.list (A.locatedDecoder BD.string))
        typeDecoder


infixEncoder : Infix -> BE.Encoder
infixEncoder (Infix op associativity precedence name) =
    BE.sequence
        [ BE.string op
        , Binop.associativityEncoder associativity
        , Binop.precedenceEncoder precedence
        , BE.string name
        ]


infixDecoder : BD.Decoder Infix
infixDecoder =
    BD.map4 Infix
        BD.string
        Binop.associativityDecoder
        Binop.precedenceDecoder
        BD.string


effectsEncoder : Effects -> BE.Encoder
effectsEncoder effects =
    case effects of
        NoEffects ->
            BE.unsignedInt8 0

        Ports ports ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.list portEncoder ports
                ]

        Manager region manager ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.regionEncoder region
                , managerEncoder manager
                ]


effectsDecoder : BD.Decoder Effects
effectsDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed NoEffects

                    1 ->
                        BD.map Ports (BD.list portDecoder)

                    2 ->
                        BD.map2 Manager
                            A.regionDecoder
                            managerDecoder

                    _ ->
                        BD.fail
            )


commentEncoder : Comment -> BE.Encoder
commentEncoder (Comment snippet) =
    P.snippetEncoder snippet


commentDecoder : BD.Decoder Comment
commentDecoder =
    BD.map Comment P.snippetDecoder


portEncoder : Port -> BE.Encoder
portEncoder (Port name tipe) =
    BE.sequence
        [ A.locatedEncoder BE.string name
        , typeEncoder tipe
        ]


portDecoder : BD.Decoder Port
portDecoder =
    BD.map2 Port
        (A.locatedDecoder BD.string)
        typeDecoder


managerEncoder : Manager -> BE.Encoder
managerEncoder manager =
    case manager of
        Cmd cmdType ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.locatedEncoder BE.string cmdType
                ]

        Sub subType ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.locatedEncoder BE.string subType
                ]

        Fx cmdType subType ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.locatedEncoder BE.string cmdType
                , A.locatedEncoder BE.string subType
                ]


managerDecoder : BD.Decoder Manager
managerDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Cmd (A.locatedDecoder BD.string)

                    1 ->
                        BD.map Sub (A.locatedDecoder BD.string)

                    2 ->
                        BD.map2 Fx
                            (A.locatedDecoder BD.string)
                            (A.locatedDecoder BD.string)

                    _ ->
                        BD.fail
            )


exposedEncoder : Exposed -> BE.Encoder
exposedEncoder exposed =
    case exposed of
        Lower name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.locatedEncoder BE.string name
                ]

        Upper name dotDotRegion ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.locatedEncoder BE.string name
                , privacyEncoder dotDotRegion
                ]

        Operator region name ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.regionEncoder region
                , BE.string name
                ]


exposedDecoder : BD.Decoder Exposed
exposedDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Lower (A.locatedDecoder BD.string)

                    1 ->
                        BD.map2 Upper
                            (A.locatedDecoder BD.string)
                            privacyDecoder

                    2 ->
                        BD.map2 Operator
                            A.regionDecoder
                            BD.string

                    _ ->
                        BD.fail
            )


privacyEncoder : Privacy -> BE.Encoder
privacyEncoder privacy =
    case privacy of
        Public region ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                ]

        Private ->
            BE.unsignedInt8 1


privacyDecoder : BD.Decoder Privacy
privacyDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Public A.regionDecoder

                    1 ->
                        BD.succeed Private

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
        PAnything name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string name
                ]

        PVar name ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string name
                ]

        PRecord fields ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.list (A.locatedEncoder BE.string) fields
                ]

        PAlias aliasPattern name ->
            BE.sequence
                [ BE.unsignedInt8 3
                , patternEncoder aliasPattern
                , A.locatedEncoder BE.string name
                ]

        PUnit ->
            BE.unsignedInt8 4

        PTuple a b cs ->
            BE.sequence
                [ BE.unsignedInt8 5
                , patternEncoder a
                , patternEncoder b
                , BE.list patternEncoder cs
                ]

        PCtor nameRegion name patterns ->
            BE.sequence
                [ BE.unsignedInt8 6
                , A.regionEncoder nameRegion
                , BE.string name
                , BE.list patternEncoder patterns
                ]

        PCtorQual nameRegion home name patterns ->
            BE.sequence
                [ BE.unsignedInt8 7
                , A.regionEncoder nameRegion
                , BE.string home
                , BE.string name
                , BE.list patternEncoder patterns
                ]

        PList patterns ->
            BE.sequence
                [ BE.unsignedInt8 8
                , BE.list patternEncoder patterns
                ]

        PCons hd tl ->
            BE.sequence
                [ BE.unsignedInt8 9
                , patternEncoder hd
                , patternEncoder tl
                ]

        PChr chr ->
            BE.sequence
                [ BE.unsignedInt8 10
                , BE.string chr
                ]

        PStr str ->
            BE.sequence
                [ BE.unsignedInt8 11
                , BE.string str
                ]

        PInt int ->
            BE.sequence
                [ BE.unsignedInt8 12
                , BE.int int
                ]


pattern_Decoder : BD.Decoder Pattern_
pattern_Decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map PAnything BD.string

                    1 ->
                        BD.map PVar BD.string

                    2 ->
                        BD.map PRecord (BD.list (A.locatedDecoder BD.string))

                    3 ->
                        BD.map2 PAlias
                            patternDecoder
                            (A.locatedDecoder BD.string)

                    4 ->
                        BD.succeed PUnit

                    5 ->
                        BD.map3 PTuple
                            patternDecoder
                            patternDecoder
                            (BD.list patternDecoder)

                    6 ->
                        BD.map3 PCtor
                            A.regionDecoder
                            BD.string
                            (BD.list patternDecoder)

                    7 ->
                        BD.map4 PCtorQual
                            A.regionDecoder
                            BD.string
                            BD.string
                            (BD.list patternDecoder)

                    8 ->
                        BD.map PList (BD.list patternDecoder)

                    9 ->
                        BD.map2 PCons
                            patternDecoder
                            patternDecoder

                    10 ->
                        BD.map PChr BD.string

                    11 ->
                        BD.map PStr BD.string

                    12 ->
                        BD.map PInt BD.int

                    _ ->
                        BD.fail
            )


exprEncoder : Expr -> BE.Encoder
exprEncoder =
    A.locatedEncoder expr_Encoder


exprDecoder : BD.Decoder Expr
exprDecoder =
    A.locatedDecoder expr_Decoder


expr_Encoder : Expr_ -> BE.Encoder
expr_Encoder expr_ =
    case expr_ of
        Chr char ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string char
                ]

        Str string ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string string
                ]

        Int int ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.int int
                ]

        Float float ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.float float
                ]

        Var varType name ->
            BE.sequence
                [ BE.unsignedInt8 4
                , varTypeEncoder varType
                , BE.string name
                ]

        VarQual varType prefix name ->
            BE.sequence
                [ BE.unsignedInt8 5
                , varTypeEncoder varType
                , BE.string prefix
                , BE.string name
                ]

        List list ->
            BE.sequence
                [ BE.unsignedInt8 6
                , BE.list exprEncoder list
                ]

        Op op ->
            BE.sequence
                [ BE.unsignedInt8 7
                , BE.string op
                ]

        Negate expr ->
            BE.sequence
                [ BE.unsignedInt8 8
                , exprEncoder expr
                ]

        Binops ops final ->
            BE.sequence
                [ BE.unsignedInt8 9
                , BE.list (BE.jsonPair exprEncoder (A.locatedEncoder BE.string)) ops
                , exprEncoder final
                ]

        Lambda srcArgs body ->
            BE.sequence
                [ BE.unsignedInt8 10
                , BE.list patternEncoder srcArgs
                , exprEncoder body
                ]

        Call func args ->
            BE.sequence
                [ BE.unsignedInt8 11
                , exprEncoder func
                , BE.list exprEncoder args
                ]

        If branches finally ->
            BE.sequence
                [ BE.unsignedInt8 12
                , BE.list (BE.jsonPair exprEncoder exprEncoder) branches
                , exprEncoder finally
                ]

        Let defs expr ->
            BE.sequence
                [ BE.unsignedInt8 13
                , BE.list (A.locatedEncoder defEncoder) defs
                , exprEncoder expr
                ]

        Case expr branches ->
            BE.sequence
                [ BE.unsignedInt8 14
                , exprEncoder expr
                , BE.list (BE.jsonPair patternEncoder exprEncoder) branches
                ]

        Accessor field ->
            BE.sequence
                [ BE.unsignedInt8 15
                , BE.string field
                ]

        Access record field ->
            BE.sequence
                [ BE.unsignedInt8 16
                , exprEncoder record
                , A.locatedEncoder BE.string field
                ]

        Update name fields ->
            BE.sequence
                [ BE.unsignedInt8 17
                , A.locatedEncoder (BE.jsonPair (BE.maybe BE.string) BE.string) name
                , BE.list (BE.jsonPair (A.locatedEncoder BE.string) exprEncoder) fields
                ]

        Record fields ->
            BE.sequence
                [ BE.unsignedInt8 18
                , BE.list (BE.jsonPair (A.locatedEncoder BE.string) exprEncoder) fields
                ]

        Unit ->
            BE.unsignedInt8 19

        Tuple a b cs ->
            BE.sequence
                [ BE.unsignedInt8 20
                , exprEncoder a
                , exprEncoder b
                , BE.list exprEncoder cs
                ]

        Shader src tipe ->
            BE.sequence
                [ BE.unsignedInt8 21
                , Shader.sourceEncoder src
                , Shader.typesEncoder tipe
                ]


expr_Decoder : BD.Decoder Expr_
expr_Decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Chr BD.string

                    1 ->
                        BD.map Str BD.string

                    2 ->
                        BD.map Int BD.int

                    3 ->
                        BD.map Float BD.float

                    4 ->
                        BD.map2 Var
                            varTypeDecoder
                            BD.string

                    5 ->
                        BD.map3 VarQual
                            varTypeDecoder
                            BD.string
                            BD.string

                    6 ->
                        BD.map List (BD.list exprDecoder)

                    7 ->
                        BD.map Op BD.string

                    8 ->
                        BD.map Negate exprDecoder

                    9 ->
                        BD.map2 Binops
                            (BD.list (BD.jsonPair exprDecoder (A.locatedDecoder BD.string)))
                            exprDecoder

                    10 ->
                        BD.map2 Lambda
                            (BD.list patternDecoder)
                            exprDecoder

                    11 ->
                        BD.map2 Call
                            exprDecoder
                            (BD.list exprDecoder)

                    12 ->
                        BD.map2 If
                            (BD.list (BD.jsonPair exprDecoder exprDecoder))
                            exprDecoder

                    13 ->
                        BD.map2 Let
                            (BD.list (A.locatedDecoder defDecoder))
                            exprDecoder

                    14 ->
                        BD.map2 Case
                            exprDecoder
                            (BD.list (BD.jsonPair patternDecoder exprDecoder))

                    15 ->
                        BD.map Accessor BD.string

                    16 ->
                        BD.map2 Access
                            exprDecoder
                            (A.locatedDecoder BD.string)

                    17 ->
                        BD.map2 Update
                            (A.locatedDecoder (BD.jsonPair (BD.maybe BD.string) BD.string))
                            (BD.list (BD.jsonPair (A.locatedDecoder BD.string) exprDecoder))

                    18 ->
                        BD.map Record
                            (BD.list (BD.jsonPair (A.locatedDecoder BD.string) exprDecoder))

                    19 ->
                        BD.succeed Unit

                    20 ->
                        BD.map3 Tuple
                            exprDecoder
                            exprDecoder
                            (BD.list exprDecoder)

                    21 ->
                        BD.map2 Shader
                            Shader.sourceDecoder
                            Shader.typesDecoder

                    _ ->
                        BD.fail
            )


varTypeEncoder : VarType -> BE.Encoder
varTypeEncoder varType =
    BE.unsignedInt8
        (case varType of
            LowVar ->
                0

            CapVar ->
                1
        )


varTypeDecoder : BD.Decoder VarType
varTypeDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed LowVar

                    1 ->
                        BD.succeed CapVar

                    _ ->
                        BD.fail
            )


defEncoder : Def -> BE.Encoder
defEncoder def =
    case def of
        Define name srcArgs body maybeType ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.locatedEncoder BE.string name
                , BE.list patternEncoder srcArgs
                , exprEncoder body
                , BE.maybe typeEncoder maybeType
                ]

        Destruct pattern body ->
            BE.sequence
                [ BE.unsignedInt8 1
                , patternEncoder pattern
                , exprEncoder body
                ]


defDecoder : BD.Decoder Def
defDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map4 Define
                            (A.locatedDecoder BD.string)
                            (BD.list patternDecoder)
                            exprDecoder
                            (BD.maybe typeDecoder)

                    1 ->
                        BD.map2 Destruct
                            patternDecoder
                            exprDecoder

                    _ ->
                        BD.fail
            )
