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
    , unescape
    )

import Compiler.Data.Name exposing (Name)
import Data.Map exposing (Dict)
import Regex
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



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


unescape : String -> String
unescape =
    Regex.replace
        (Regex.fromString "\\\\n|\\\\\"|\\\\'|\\\\\\\\"
            |> Maybe.withDefault Regex.never
        )
        (\{ match } ->
            case match of
                "\\n" ->
                    "\n"

                "\\\"" ->
                    "\""

                "\\'" ->
                    "'"

                "\\\\" ->
                    "\\"

                _ ->
                    match
        )



-- ENCODERS and DECODERS


sourceEncoder : Source -> BE.Encoder
sourceEncoder (Source src) =
    BE.string src


sourceDecoder : BD.Decoder Source
sourceDecoder =
    BD.map Source BD.string


typesEncoder : Types -> BE.Encoder
typesEncoder (Types attribute uniform varying) =
    BE.sequence
        [ BE.assocListDict compare BE.string typeEncoder attribute
        , BE.assocListDict compare BE.string typeEncoder uniform
        , BE.assocListDict compare BE.string typeEncoder varying
        ]


typesDecoder : BD.Decoder Types
typesDecoder =
    BD.map3 Types
        (BD.assocListDict identity BD.string typeDecoder)
        (BD.assocListDict identity BD.string typeDecoder)
        (BD.assocListDict identity BD.string typeDecoder)


typeEncoder : Type -> BE.Encoder
typeEncoder type_ =
    BE.unsignedInt8
        (case type_ of
            Int ->
                0

            Float ->
                1

            V2 ->
                2

            V3 ->
                3

            V4 ->
                4

            M4 ->
                5

            Texture ->
                6
        )


typeDecoder : BD.Decoder Type
typeDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Int

                    1 ->
                        BD.succeed Float

                    2 ->
                        BD.succeed V2

                    3 ->
                        BD.succeed V3

                    4 ->
                        BD.succeed V4

                    5 ->
                        BD.succeed M4

                    6 ->
                        BD.succeed Texture

                    _ ->
                        BD.fail
            )
