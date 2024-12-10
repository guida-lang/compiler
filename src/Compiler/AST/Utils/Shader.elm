module Compiler.AST.Utils.Shader exposing
    ( Source(..)
    , Type(..)
    , Types(..)
    , fromString
    , sourceDecoder
    , sourceEncoder
    , toJsStringBuilder
    , typesDecoder
    , typesEncoder
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Json.Encode as E
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode



-- SOURCE


type Source
    = Source String



-- TYPES


type Types
    = Types (Dict String Name Type) (Dict String Name Type) (Dict String Name Type)


type Type
    = Int
    | Float
    | V2
    | V3
    | V4
    | M4
    | Texture



-- TO BUILDER


toJsStringBuilder : Source -> String
toJsStringBuilder (Source src) =
    src



-- FROM STRING


fromString : String -> Source
fromString =
    Source << escape


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


sourceEncoder : Source -> Encode.Value
sourceEncoder (Source src) =
    Encode.string src


sourceDecoder : Decode.Decoder Source
sourceDecoder =
    Decode.map Source Decode.string


typesEncoder : Types -> Encode.Value
typesEncoder (Types attribute uniform varying) =
    Encode.object
        [ ( "type", Encode.string "Types" )
        , ( "attribute", E.assocListDict compare Encode.string typeEncoder attribute )
        , ( "uniform", E.assocListDict compare Encode.string typeEncoder uniform )
        , ( "varying", E.assocListDict compare Encode.string typeEncoder varying )
        ]


typesDecoder : Decode.Decoder Types
typesDecoder =
    Decode.map3 Types
        (Decode.field "attribute" (assocListDict identity Decode.string typeDecoder))
        (Decode.field "uniform" (assocListDict identity Decode.string typeDecoder))
        (Decode.field "varying" (assocListDict identity Decode.string typeDecoder))


typeEncoder : Type -> Encode.Value
typeEncoder type_ =
    case type_ of
        Int ->
            Encode.string "Int"

        Float ->
            Encode.string "Float"

        V2 ->
            Encode.string "V2"

        V3 ->
            Encode.string "V3"

        V4 ->
            Encode.string "V4"

        M4 ->
            Encode.string "M4"

        Texture ->
            Encode.string "Texture"


typeDecoder : Decode.Decoder Type
typeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Int" ->
                        Decode.succeed Int

                    "Float" ->
                        Decode.succeed Float

                    "V2" ->
                        Decode.succeed V2

                    "V3" ->
                        Decode.succeed V3

                    "V4" ->
                        Decode.succeed V4

                    "M4" ->
                        Decode.succeed M4

                    "Texture" ->
                        Decode.succeed Texture

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
