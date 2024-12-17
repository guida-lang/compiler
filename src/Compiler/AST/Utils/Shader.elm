module Compiler.AST.Utils.Shader exposing
    ( fromString
    , sourceDecoder
    , sourceEncoder
    , toJsStringBuilder
    , typesDecoder
    , typesEncoder
    )

import Compiler.Json.Encode as E
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- TO BUILDER


toJsStringBuilder : T.CASTUS_Source -> String
toJsStringBuilder (T.CASTUS_Source src) =
    src



-- FROM STRING


fromString : String -> T.CASTUS_Source
fromString =
    T.CASTUS_Source << escape


escape : String -> String
escape =
    String.foldr
        (\char acc ->
            case char of
                '\u{000D}' ->
                    acc

                '\n' ->
                    acc
                        |> String.cons 'n'
                        |> String.cons '\\'

                '"' ->
                    acc
                        |> String.cons '"'
                        |> String.cons '\\'

                '\'' ->
                    acc
                        |> String.cons '\''
                        |> String.cons '\\'

                '\\' ->
                    acc
                        |> String.cons '\\'
                        |> String.cons '\\'

                _ ->
                    String.cons char acc
        )
        ""



-- ENCODERS and DECODERS


sourceEncoder : T.CASTUS_Source -> Encode.Value
sourceEncoder (T.CASTUS_Source src) =
    Encode.string src


sourceDecoder : Decode.Decoder T.CASTUS_Source
sourceDecoder =
    Decode.map T.CASTUS_Source Decode.string


typesEncoder : T.CASTUS_Types -> Encode.Value
typesEncoder (T.CASTUS_Types attribute uniform varying) =
    Encode.object
        [ ( "type", Encode.string "Types" )
        , ( "attribute", E.assocListDict compare Encode.string typeEncoder attribute )
        , ( "uniform", E.assocListDict compare Encode.string typeEncoder uniform )
        , ( "varying", E.assocListDict compare Encode.string typeEncoder varying )
        ]


typesDecoder : Decode.Decoder T.CASTUS_Types
typesDecoder =
    Decode.map3 T.CASTUS_Types
        (Decode.field "attribute" (assocListDict identity Decode.string typeDecoder))
        (Decode.field "uniform" (assocListDict identity Decode.string typeDecoder))
        (Decode.field "varying" (assocListDict identity Decode.string typeDecoder))


typeEncoder : T.CASTUS_Type -> Encode.Value
typeEncoder type_ =
    case type_ of
        T.CASTUS_Int ->
            Encode.string "Int"

        T.CASTUS_Float ->
            Encode.string "Float"

        T.CASTUS_V2 ->
            Encode.string "V2"

        T.CASTUS_V3 ->
            Encode.string "V3"

        T.CASTUS_V4 ->
            Encode.string "V4"

        T.CASTUS_M4 ->
            Encode.string "M4"

        T.CASTUS_Texture ->
            Encode.string "Texture"


typeDecoder : Decode.Decoder T.CASTUS_Type
typeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Int" ->
                        Decode.succeed T.CASTUS_Int

                    "Float" ->
                        Decode.succeed T.CASTUS_Float

                    "V2" ->
                        Decode.succeed T.CASTUS_V2

                    "V3" ->
                        Decode.succeed T.CASTUS_V3

                    "V4" ->
                        Decode.succeed T.CASTUS_V4

                    "M4" ->
                        Decode.succeed T.CASTUS_M4

                    "Texture" ->
                        Decode.succeed T.CASTUS_Texture

                    _ ->
                        Decode.fail ("Unknown Type: " ++ str)
            )



-- COPIED FROM JSON.DECODEX


assocListDict : (k -> comparable) -> Decode.Decoder k -> Decode.Decoder v -> Decode.Decoder (Dict comparable k v)
assocListDict toComparable keyDecoder valueDecoder =
    Decode.list (jsonPair keyDecoder valueDecoder)
        |> Decode.map (Dict.fromList toComparable)


jsonPair : Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder ( a, b )
jsonPair firstDecoder secondDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "a" firstDecoder)
        (Decode.field "b" secondDecoder)
