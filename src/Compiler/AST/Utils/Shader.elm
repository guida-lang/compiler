module Compiler.AST.Utils.Shader exposing
    ( CASTUS_Source(..)
    , CASTUS_Type(..)
    , CASTUS_Types(..)
    , fromString
    , sourceDecoder
    , sourceEncoder
    , toJsStringBuilder
    , typesDecoder
    , typesEncoder
    )

import Compiler.Data.Name exposing (CDN_Name)
import Compiler.Json.Encode as E
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode



-- SOURCE


type CASTUS_Source
    = CASTUS_Source String



-- TYPES


type CASTUS_Types
    = CASTUS_Types (Dict String CDN_Name CASTUS_Type) (Dict String CDN_Name CASTUS_Type) (Dict String CDN_Name CASTUS_Type)


type CASTUS_Type
    = CASTUS_Int
    | CASTUS_Float
    | CASTUS_V2
    | CASTUS_V3
    | CASTUS_V4
    | CASTUS_M4
    | CASTUS_Texture



-- TO BUILDER


toJsStringBuilder : CASTUS_Source -> String
toJsStringBuilder (CASTUS_Source src) =
    src



-- FROM STRING


fromString : String -> CASTUS_Source
fromString =
    CASTUS_Source << escape


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


sourceEncoder : CASTUS_Source -> Encode.Value
sourceEncoder (CASTUS_Source src) =
    Encode.string src


sourceDecoder : Decode.Decoder CASTUS_Source
sourceDecoder =
    Decode.map CASTUS_Source Decode.string


typesEncoder : CASTUS_Types -> Encode.Value
typesEncoder (CASTUS_Types attribute uniform varying) =
    Encode.object
        [ ( "type", Encode.string "Types" )
        , ( "attribute", E.assocListDict compare Encode.string typeEncoder attribute )
        , ( "uniform", E.assocListDict compare Encode.string typeEncoder uniform )
        , ( "varying", E.assocListDict compare Encode.string typeEncoder varying )
        ]


typesDecoder : Decode.Decoder CASTUS_Types
typesDecoder =
    Decode.map3 CASTUS_Types
        (Decode.field "attribute" (assocListDict identity Decode.string typeDecoder))
        (Decode.field "uniform" (assocListDict identity Decode.string typeDecoder))
        (Decode.field "varying" (assocListDict identity Decode.string typeDecoder))


typeEncoder : CASTUS_Type -> Encode.Value
typeEncoder type_ =
    case type_ of
        CASTUS_Int ->
            Encode.string "Int"

        CASTUS_Float ->
            Encode.string "Float"

        CASTUS_V2 ->
            Encode.string "V2"

        CASTUS_V3 ->
            Encode.string "V3"

        CASTUS_V4 ->
            Encode.string "V4"

        CASTUS_M4 ->
            Encode.string "M4"

        CASTUS_Texture ->
            Encode.string "Texture"


typeDecoder : Decode.Decoder CASTUS_Type
typeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Int" ->
                        Decode.succeed CASTUS_Int

                    "Float" ->
                        Decode.succeed CASTUS_Float

                    "V2" ->
                        Decode.succeed CASTUS_V2

                    "V3" ->
                        Decode.succeed CASTUS_V3

                    "V4" ->
                        Decode.succeed CASTUS_V4

                    "M4" ->
                        Decode.succeed CASTUS_M4

                    "Texture" ->
                        Decode.succeed CASTUS_Texture

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
