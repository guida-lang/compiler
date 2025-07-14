module Compiler.AST.Source exposing
    ( Alias(..)
    , C0Eol
    , C1
    , C1Eol
    , C2
    , C2Eol
    , C3
    , Comment(..)
    , Def(..)
    , Docs(..)
    , Effects(..)
    , Exposed(..)
    , Exposing(..)
    , Expr
    , Expr_(..)
    , FComment(..)
    , FComments
    , ForceMultiline(..)
    , Import(..)
    , Infix(..)
    , Manager(..)
    , Module(..)
    , OpenCommentedList(..)
    , Pair(..)
    , Pattern
    , Pattern_(..)
    , Port(..)
    , Privacy(..)
    , Type
    , Type_(..)
    , Union(..)
    , Value(..)
    , VarType(..)
    , c0EolMap
    , c0EolValue
    , c1Decoder
    , c1Encoder
    , c1Value
    , c1map
    , c2EolDecoder
    , c2EolEncoder
    , c2EolMap
    , c2EolValue
    , c2Value
    , c2map
    , fCommentsDecoder
    , getImportName
    , getName
    , mapPair
    , moduleDecoder
    , moduleEncoder
    , openCommentedListMap
    , sequenceAC2
    , toCommentedList
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



-- FORMAT


type ForceMultiline
    = ForceMultiline Bool


type FComment
    = BlockComment (List String)
    | LineComment String
    | CommentTrickOpener
    | CommentTrickCloser
    | CommentTrickBlock String


type alias FComments =
    List FComment


type alias C1 a =
    ( FComments, a )


c1map : (a -> b) -> C1 a -> C1 b
c1map f ( comments, a ) =
    ( comments, f a )


c1Value : C1 a -> a
c1Value ( _, a ) =
    a


type alias C2 a =
    ( FComments, FComments, a )


c2map : (a -> b) -> C2 a -> C2 b
c2map f ( before, after, a ) =
    ( before, after, f a )


c2Value : C2 a -> a
c2Value ( _, _, a ) =
    a


sequenceAC2 : List (C2 a) -> C2 (List a)
sequenceAC2 =
    List.foldr
        (\( before, after, a ) ( beforeAcc, afterAcc, acc ) ->
            ( before ++ beforeAcc, after ++ afterAcc, a :: acc )
        )
        ( [], [], [] )


type alias C3 a =
    ( ( FComments, FComments, FComments ), a )


type alias C0Eol a =
    ( Maybe String, a )


c0EolMap : (a -> b) -> C0Eol a -> C0Eol b
c0EolMap f ( eol, a ) =
    ( eol, f a )


c0EolValue : C0Eol a -> a
c0EolValue ( eol, a ) =
    a


type alias C1Eol a =
    ( FComments, Maybe String, a )


type alias C2Eol a =
    ( ( FComments, FComments, Maybe String ), a )


c2EolMap : (a -> b) -> C2Eol a -> C2Eol b
c2EolMap f ( ( before, after, eol ), a ) =
    ( ( before, after, eol ), f a )


c2EolValue : C2Eol a -> a
c2EolValue ( ( before, after, eol ), a ) =
    a


{-| This represents a list of things that have a clear start delimiter but no
clear end delimiter.
There must be at least one item.
Comments can appear before the last item, or around any other item.
An end-of-line comment can also appear after the last item.

For example:
= a
= a, b, c

TODO: this should be replaced with (Sequence a)

-}
type OpenCommentedList a
    = OpenCommentedList (List (C2Eol a)) (C1Eol a)


openCommentedListMap : (a -> b) -> OpenCommentedList a -> OpenCommentedList b
openCommentedListMap f (OpenCommentedList rest ( preLst, eolLst, lst )) =
    OpenCommentedList
        (List.map (\( ( pre, post, eol ), a ) -> ( ( pre, post, eol ), f a )) rest)
        ( preLst, eolLst, f lst )


toCommentedList : OpenCommentedList Type -> List (C2Eol Type)
toCommentedList (OpenCommentedList rest ( cLast, eolLast, last )) =
    rest ++ [ ( ( cLast, [], eolLast ), last ) ]


{-| Represents a delimiter-separated pair.

Comments can appear after the key or before the value.

For example:

key = value
key : value

-}
type Pair key value
    = Pair (C1 key) (C1 value) ForceMultiline


mapPair : (a1 -> a2) -> (b1 -> b2) -> Pair a1 b1 -> Pair a2 b2
mapPair fa fb (Pair k v fm) =
    Pair (c1map fa k) (c1map fb v) fm



-- EXPRESSIONS


type alias Expr =
    A.Located Expr_


type Expr_
    = Chr String
    | Str String Bool
    | Int Int String
    | Float Float String
    | Var VarType Name
    | VarQual VarType Name Name
    | List (List (C2Eol Expr)) FComments
    | Op Name
    | Negate Expr
    | Binops (List ( Expr, A.Located Name )) Expr
    | Lambda (C1 (List (C1 Pattern))) (C1 Expr)
    | Call Expr (List (C1 Expr))
    | If (List (C1 ( C2 Expr, C2 Expr ))) (C1 Expr)
    | Let (List (C2 (A.Located Def))) FComments Expr
    | Case (C2 Expr) (List ( C2 Pattern, C1 Expr ))
    | Accessor Name
    | Access Expr (A.Located Name)
    | Update (C2 Expr) (List ( A.Located Name, Expr ))
    | Record (List ( A.Located Name, Expr ))
    | Unit
    | Tuple (C2 Expr) (C2 Expr) (List (C2 Expr))
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
    | PStr String Bool
    | PInt Int String



-- TYPE


type alias Type =
    A.Located Type_


type Type_
    = TLambda Type Type
    | TVar Name
    | TType A.Region Name (List (C1 Type))
    | TTypeQual A.Region Name Name (List (C1 Type))
    | TRecord (List (C2 ( C1 (A.Located Name), C1 Type ))) (Maybe (C2 (A.Located Name))) FComments
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
    = Value FComments (A.Located Name) (List Pattern) (C1 Expr) (Maybe Type)


type Union
    = Union (A.Located Name) (List (A.Located Name)) (List ( A.Located Name, List Type ))


type Alias
    = Alias FComments (C2 (A.Located Name)) (List (C1 (A.Located Name))) (C1 Type)


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
    = NoDocs A.Region (List ( Name, Comment ))
    | YesDocs Comment (List ( Name, Comment ))


type Comment
    = Comment P.Snippet



-- EXPOSING


type Exposing
    = Open
    | Explicit (A.Located (List Exposed))


type Exposed
    = Lower (A.Located Name)
    | Upper (A.Located Name) Privacy
    | Operator A.Region Name


type Privacy
    = Public A.Region
    | Private



-- ENCODERS and DECODERS


forceMultilineEncoder : ForceMultiline -> BE.Encoder
forceMultilineEncoder (ForceMultiline bool) =
    BE.bool bool


forceMultilineDecoder : BD.Decoder ForceMultiline
forceMultilineDecoder =
    BD.map ForceMultiline BD.bool


fCommentEncoder : FComment -> BE.Encoder
fCommentEncoder formatComment =
    case formatComment of
        BlockComment c ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.list BE.string c
                ]

        LineComment c ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string c
                ]

        CommentTrickOpener ->
            BE.unsignedInt8 2

        CommentTrickCloser ->
            BE.unsignedInt8 3

        CommentTrickBlock c ->
            BE.sequence
                [ BE.unsignedInt8 4
                , BE.string c
                ]


fCommentDecoder : BD.Decoder FComment
fCommentDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map BlockComment (BD.list BD.string)

                    1 ->
                        BD.map LineComment BD.string

                    2 ->
                        BD.succeed CommentTrickOpener

                    3 ->
                        BD.succeed CommentTrickCloser

                    4 ->
                        BD.map CommentTrickBlock BD.string

                    _ ->
                        BD.fail
            )


fCommentsEncoder : FComments -> BE.Encoder
fCommentsEncoder =
    BE.list fCommentEncoder


fCommentsDecoder : BD.Decoder FComments
fCommentsDecoder =
    BD.list fCommentDecoder


c1Encoder : (a -> BE.Encoder) -> C1 a -> BE.Encoder
c1Encoder encoder ( comments, a ) =
    BE.sequence
        [ fCommentsEncoder comments
        , encoder a
        ]


c1Decoder : BD.Decoder a -> BD.Decoder (C1 a)
c1Decoder decoder =
    BD.map2 Tuple.pair fCommentsDecoder decoder


c2Encoder : (a -> BE.Encoder) -> C2 a -> BE.Encoder
c2Encoder encoder ( preComments, postComments, a ) =
    BE.sequence
        [ fCommentsEncoder preComments
        , fCommentsEncoder postComments
        , encoder a
        ]


c2Decoder : BD.Decoder a -> BD.Decoder (C2 a)
c2Decoder decoder =
    BD.map3
        (\preComments postComments a ->
            ( preComments, postComments, a )
        )
        fCommentsDecoder
        fCommentsDecoder
        decoder


c2EolEncoder : (a -> BE.Encoder) -> C2Eol a -> BE.Encoder
c2EolEncoder encoder ( ( preComments, postComments, eol ), a ) =
    BE.sequence
        [ fCommentsEncoder preComments
        , fCommentsEncoder postComments
        , BE.maybe BE.string eol
        , encoder a
        ]


c2EolDecoder : BD.Decoder a -> BD.Decoder (C2Eol a)
c2EolDecoder decoder =
    BD.map4
        (\preComments postComments eol a ->
            ( ( preComments, postComments, eol ), a )
        )
        fCommentsDecoder
        fCommentsDecoder
        (BD.maybe BD.string)
        decoder


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
                , BE.list (c1Encoder typeEncoder) args
                ]

        TTypeQual region home name args ->
            BE.sequence
                [ BE.unsignedInt8 3
                , A.regionEncoder region
                , BE.string home
                , BE.string name
                , BE.list (c1Encoder typeEncoder) args
                ]

        TRecord fields ext trailing ->
            BE.sequence
                [ BE.unsignedInt8 4
                , BE.list (c2Encoder (BE.jsonPair (c1Encoder (A.locatedEncoder BE.string)) (c1Encoder typeEncoder))) fields
                , BE.maybe (c2Encoder (A.locatedEncoder BE.string)) ext
                , fCommentsEncoder trailing
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
                            (BD.list (c1Decoder typeDecoder))

                    3 ->
                        BD.map4 TTypeQual
                            A.regionDecoder
                            BD.string
                            BD.string
                            (BD.list (c1Decoder typeDecoder))

                    4 ->
                        BD.map3 TRecord
                            (BD.list (c2Decoder (BD.jsonPair (c1Decoder (A.locatedDecoder BD.string)) (c1Decoder typeDecoder))))
                            (BD.maybe (c2Decoder (A.locatedDecoder BD.string)))
                            fCommentsDecoder

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
                , A.locatedEncoder (BE.list exposedEncoder) exposedList
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
                        BD.map Explicit (A.locatedDecoder (BD.list exposedDecoder))

                    _ ->
                        BD.fail
            )


docsEncoder : Docs -> BE.Encoder
docsEncoder docs =
    case docs of
        NoDocs region comments ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                , BE.list (BE.jsonPair BE.string commentEncoder) comments
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
                        BD.map2 NoDocs
                            A.regionDecoder
                            (BD.list (BD.jsonPair BD.string commentDecoder))

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
valueEncoder (Value formatComments name srcArgs body maybeType) =
    BE.sequence
        [ fCommentsEncoder formatComments
        , A.locatedEncoder BE.string name
        , BE.list patternEncoder srcArgs
        , c1Encoder exprEncoder body
        , BE.maybe typeEncoder maybeType
        ]


valueDecoder : BD.Decoder Value
valueDecoder =
    BD.map5 Value
        fCommentsDecoder
        (A.locatedDecoder BD.string)
        (BD.list patternDecoder)
        (c1Decoder exprDecoder)
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
aliasEncoder (Alias formatComments name args tipe) =
    BE.sequence
        [ fCommentsEncoder formatComments
        , c2Encoder (A.locatedEncoder BE.string) name
        , BE.list (c1Encoder (A.locatedEncoder BE.string)) args
        , c1Encoder typeEncoder tipe
        ]


aliasDecoder : BD.Decoder Alias
aliasDecoder =
    BD.map4 Alias
        fCommentsDecoder
        (c2Decoder (A.locatedDecoder BD.string))
        (BD.list (c1Decoder (A.locatedDecoder BD.string)))
        (c1Decoder typeDecoder)


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

        PStr str multiline ->
            BE.sequence
                [ BE.unsignedInt8 11
                , BE.string str
                , BE.bool multiline
                ]

        PInt int src ->
            BE.sequence
                [ BE.unsignedInt8 12
                , BE.int int
                , BE.string src
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
                        BD.map2 PStr
                            BD.string
                            BD.bool

                    12 ->
                        BD.map2 PInt
                            BD.int
                            BD.string

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

        Str string multiline ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string string
                , BE.bool multiline
                ]

        Int int src ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.int int
                , BE.string src
                ]

        Float float src ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.float float
                , BE.string src
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

        List list trailing ->
            BE.sequence
                [ BE.unsignedInt8 6
                , BE.list (c2EolEncoder exprEncoder) list
                , fCommentsEncoder trailing
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
                , c1Encoder (BE.list (c1Encoder patternEncoder)) srcArgs
                , c1Encoder exprEncoder body
                ]

        Call func args ->
            BE.sequence
                [ BE.unsignedInt8 11
                , exprEncoder func
                , BE.list (c1Encoder exprEncoder) args
                ]

        If branches finally ->
            BE.sequence
                [ BE.unsignedInt8 12
                , BE.list (c1Encoder (BE.jsonPair (c2Encoder exprEncoder) (c2Encoder exprEncoder))) branches
                , c1Encoder exprEncoder finally
                ]

        Let defs comments expr ->
            BE.sequence
                [ BE.unsignedInt8 13
                , BE.list (c2Encoder (A.locatedEncoder defEncoder)) defs
                , fCommentsEncoder comments
                , exprEncoder expr
                ]

        Case expr branches ->
            BE.sequence
                [ BE.unsignedInt8 14
                , c2Encoder exprEncoder expr
                , BE.list (BE.jsonPair (c2Encoder patternEncoder) (c1Encoder exprEncoder)) branches
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
                , c2Encoder exprEncoder name
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
                , c2Encoder exprEncoder a
                , c2Encoder exprEncoder b
                , BE.list (c2Encoder exprEncoder) cs
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
                        BD.map2 Str
                            BD.string
                            BD.bool

                    2 ->
                        BD.map2 Int
                            BD.int
                            BD.string

                    3 ->
                        BD.map2 Float
                            BD.float
                            BD.string

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
                        BD.map2 List
                            (BD.list (c2EolDecoder exprDecoder))
                            fCommentsDecoder

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
                            (c1Decoder (BD.list (c1Decoder patternDecoder)))
                            (c1Decoder exprDecoder)

                    11 ->
                        BD.map2 Call
                            exprDecoder
                            (BD.list (c1Decoder exprDecoder))

                    12 ->
                        BD.map2 If
                            (BD.list (c1Decoder (BD.jsonPair (c2Decoder exprDecoder) (c2Decoder exprDecoder))))
                            (c1Decoder exprDecoder)

                    13 ->
                        BD.map3 Let
                            (BD.list (c2Decoder (A.locatedDecoder defDecoder)))
                            fCommentsDecoder
                            exprDecoder

                    14 ->
                        BD.map2 Case
                            (c2Decoder exprDecoder)
                            (BD.list (BD.jsonPair (c2Decoder patternDecoder) (c1Decoder exprDecoder)))

                    15 ->
                        BD.map Accessor BD.string

                    16 ->
                        BD.map2 Access
                            exprDecoder
                            (A.locatedDecoder BD.string)

                    17 ->
                        BD.map2 Update
                            (c2Decoder exprDecoder)
                            (BD.list (BD.jsonPair (A.locatedDecoder BD.string) exprDecoder))

                    18 ->
                        BD.map Record
                            (BD.list (BD.jsonPair (A.locatedDecoder BD.string) exprDecoder))

                    19 ->
                        BD.succeed Unit

                    20 ->
                        BD.map3 Tuple
                            (c2Decoder exprDecoder)
                            (c2Decoder exprDecoder)
                            (BD.list (c2Decoder exprDecoder))

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
