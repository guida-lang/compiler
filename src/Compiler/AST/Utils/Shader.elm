module Compiler.AST.Utils.Shader exposing
    ( Source(..)
    , Type(..)
    , Types(..)
    , fromString
    , sourceCodec
    , toJsStringBuilder
    , typesCodec
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Serialize as S
import Data.Map exposing (Dict)
import Serialize exposing (Codec)



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


sourceCodec : Codec e Source
sourceCodec =
    Serialize.string |> Serialize.map Source (\(Source src) -> src)


typesCodec : Codec e Types
typesCodec =
    Serialize.customType
        (\typesCodecEncoder (Types attribute uniform varying) ->
            typesCodecEncoder attribute uniform varying
        )
        |> Serialize.variant3
            Types
            (S.assocListDict identity compare Serialize.string typeCodec)
            (S.assocListDict identity compare Serialize.string typeCodec)
            (S.assocListDict identity compare Serialize.string typeCodec)
        |> Serialize.finishCustomType


typeCodec : Codec e Type
typeCodec =
    Serialize.customType
        (\intEncoder floatEncoder v2Encoder v3Encoder v4Encoder m4Encoder textureEncoder value ->
            case value of
                Int ->
                    intEncoder

                Float ->
                    floatEncoder

                V2 ->
                    v2Encoder

                V3 ->
                    v3Encoder

                V4 ->
                    v4Encoder

                M4 ->
                    m4Encoder

                Texture ->
                    textureEncoder
        )
        |> Serialize.variant0 Int
        |> Serialize.variant0 Float
        |> Serialize.variant0 V2
        |> Serialize.variant0 V3
        |> Serialize.variant0 V4
        |> Serialize.variant0 M4
        |> Serialize.variant0 Texture
        |> Serialize.finishCustomType
