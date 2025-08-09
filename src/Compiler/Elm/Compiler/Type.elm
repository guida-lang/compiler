module Compiler.Elm.Compiler.Type exposing
    ( Alias(..)
    , DebugMetadata(..)
    , Type(..)
    , Union(..)
    , bytesDecoder
    , bytesEncoder
    , decoder
    , encode
    , encodeMetadata
    , jsonDecoder
    , jsonEncoder
    , toDoc
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Json.Decode as D exposing (Decoder)
import Compiler.Json.Encode as E exposing (Value)
import Compiler.Json.String as Json
import Compiler.Parse.Primitives as P
import Compiler.Parse.Type as Type
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Json.Decode as Decode
import Json.Encode as Encode
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)



-- TYPES


type Type
    = Lambda Type Type
    | Var Name.Name
    | Type Name.Name (List Type)
    | Record (List ( Name.Name, Type )) (Maybe Name.Name)
    | Unit
    | Tuple Type Type (List Type)


type DebugMetadata
    = DebugMetadata Type (List Alias) (List Union)


type Alias
    = Alias Name.Name (List Name.Name) Type


type Union
    = Union Name.Name (List Name.Name) (List ( Name.Name, List Type ))



-- TO DOC


toDoc : L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer context tipe =
    case tipe of
        Lambda _ _ ->
            case List.map (toDoc localizer RT.Func) (collectLambdas tipe) of
                a :: b :: cs ->
                    RT.lambda context a b cs

                _ ->
                    crash "toDoc Lambda"

        Var name ->
            D.fromName name

        Unit ->
            D.fromChars "()"

        Tuple a b cs ->
            RT.tuple
                (toDoc localizer RT.None a)
                (toDoc localizer RT.None b)
                (List.map (toDoc localizer RT.None) cs)

        Type name args ->
            RT.apply
                context
                (D.fromName name)
                (List.map (toDoc localizer RT.App) args)

        Record fields ext ->
            RT.record
                (List.map (entryToDoc localizer) fields)
                (Maybe.map D.fromName ext)


entryToDoc : L.Localizer -> ( Name.Name, Type ) -> ( D.Doc, D.Doc )
entryToDoc localizer ( field, fieldType ) =
    ( D.fromName field, toDoc localizer RT.None fieldType )


collectLambdas : Type -> List Type
collectLambdas tipe =
    case tipe of
        Lambda arg body ->
            arg :: collectLambdas body

        _ ->
            [ tipe ]



-- JSON for TYPE


encode : Type -> Value
encode tipe =
    E.string (D.toLine (toDoc L.empty RT.None tipe))


decoder : Decoder () Type
decoder =
    D.customString parser (\_ _ -> ())


parser : P.Parser () Type
parser =
    P.specialize (\_ _ _ -> ()) (P.fmap fromRawType (P.fmap (Tuple.first >> Tuple.second) (Type.expression [])))


fromRawType : Src.Type -> Type
fromRawType (A.At _ astType) =
    case astType of
        Src.TLambda ( _, t1 ) ( _, t2 ) ->
            Lambda (fromRawType t1) (fromRawType t2)

        Src.TVar x ->
            Var x

        Src.TUnit ->
            Unit

        Src.TTuple ( _, a ) ( _, b ) cs ->
            Tuple
                (fromRawType a)
                (fromRawType b)
                (List.map (Src.c2EolValue >> fromRawType) cs)

        Src.TType _ name args ->
            Type name (List.map fromRawType (List.map Tuple.second args))

        Src.TTypeQual _ _ name args ->
            Type name (List.map fromRawType (List.map Tuple.second args))

        Src.TRecord fields maybeExt _ ->
            let
                fromField : Src.C2 ( Src.C1 (A.Located a), Src.C1 Src.Type ) -> ( a, Type )
                fromField ( _, ( ( _, A.At _ field ), ( _, tipe ) ) ) =
                    ( field, fromRawType tipe )
            in
            Record
                (List.map fromField fields)
                (Maybe.map (\( _, A.At _ ext ) -> ext) maybeExt)

        Src.TParens ( _, astType_ ) ->
            fromRawType astType_



-- JSON for PROGRAM


encodeMetadata : DebugMetadata -> Value
encodeMetadata (DebugMetadata msg aliases unions) =
    E.object
        [ ( "message", encode msg )
        , ( "aliases", E.object (List.map toTypeAliasField aliases) )
        , ( "unions", E.object (List.map toCustomTypeField unions) )
        ]


toTypeAliasField : Alias -> ( String, Value )
toTypeAliasField (Alias name args tipe) =
    ( Json.fromName name
    , E.object
        [ ( "args", E.list E.string args )
        , ( "type", encode tipe )
        ]
    )


toCustomTypeField : Union -> ( String, Value )
toCustomTypeField (Union name args constructors) =
    ( Json.fromName name
    , E.object
        [ ( "args", E.list E.string args )
        , ( "tags", E.object (List.map toVariantObject constructors) )
        ]
    )


toVariantObject : ( Name.Name, List Type ) -> ( String, Value )
toVariantObject ( name, args ) =
    ( Json.fromName name, E.list encode args )



-- JSON ENCODERS and DECODERS


jsonEncoder : Type -> Encode.Value
jsonEncoder type_ =
    case type_ of
        Lambda arg body ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "arg", jsonEncoder arg )
                , ( "body", jsonEncoder body )
                ]

        Var name ->
            Encode.object
                [ ( "type", Encode.string "Var" )
                , ( "name", Encode.string name )
                ]

        Type name args ->
            Encode.object
                [ ( "type", Encode.string "Type" )
                , ( "name", Encode.string name )
                , ( "args", Encode.list jsonEncoder args )
                ]

        Record fields ext ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "fields", Encode.list (E.jsonPair Encode.string jsonEncoder) fields )
                , ( "ext", E.maybe Encode.string ext )
                ]

        Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        Tuple a b cs ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", jsonEncoder a )
                , ( "b", jsonEncoder b )
                , ( "cs", Encode.list jsonEncoder cs )
                ]


jsonDecoder : Decode.Decoder Type
jsonDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Lambda" ->
                        Decode.map2 Lambda
                            (Decode.field "arg" jsonDecoder)
                            (Decode.field "body" jsonDecoder)

                    "Var" ->
                        Decode.map Var
                            (Decode.field "name" Decode.string)

                    "Type" ->
                        Decode.map2 Type
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list jsonDecoder))

                    "Record" ->
                        Decode.map2 Record
                            (Decode.field "fields" (Decode.list (D.jsonPair Decode.string jsonDecoder)))
                            (Decode.field "ext" (Decode.maybe Decode.string))

                    "Unit" ->
                        Decode.succeed Unit

                    "Tuple" ->
                        Decode.map3 Tuple
                            (Decode.field "a" jsonDecoder)
                            (Decode.field "b" jsonDecoder)
                            (Decode.field "cs" (Decode.list jsonDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Type's type: " ++ type_)
            )



-- ENCODERS and DECODERS


bytesEncoder : Type -> BE.Encoder
bytesEncoder type_ =
    case type_ of
        Lambda arg body ->
            BE.sequence
                [ BE.unsignedInt8 0
                , bytesEncoder arg
                , bytesEncoder body
                ]

        Var name ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string name
                ]

        Type name args ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string name
                , BE.list bytesEncoder args
                ]

        Record fields ext ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.list (BE.jsonPair BE.string bytesEncoder) fields
                , BE.maybe BE.string ext
                ]

        Unit ->
            BE.unsignedInt8 4

        Tuple a b cs ->
            BE.sequence
                [ BE.unsignedInt8 5
                , bytesEncoder a
                , bytesEncoder b
                , BE.list bytesEncoder cs
                ]


bytesDecoder : BD.Decoder Type
bytesDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 Lambda
                            bytesDecoder
                            bytesDecoder

                    1 ->
                        BD.map Var BD.string

                    2 ->
                        BD.map2 Type
                            BD.string
                            (BD.list bytesDecoder)

                    3 ->
                        BD.map2 Record
                            (BD.list (BD.jsonPair BD.string bytesDecoder))
                            (BD.maybe BD.string)

                    4 ->
                        BD.succeed Unit

                    5 ->
                        BD.map3 Tuple
                            bytesDecoder
                            bytesDecoder
                            (BD.list bytesDecoder)

                    _ ->
                        BD.fail
            )
