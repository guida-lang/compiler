module Compiler.AST.Utils.Binop exposing
    ( Associativity(..)
    , Precedence
    , associativityCodec
    , associativityDecoder
    , associativityEncoder
    , precedenceCodec
    , precedenceDecoder
    , precedenceEncoder
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Serialize exposing (Codec)



-- BINOP STUFF


type alias Precedence =
    Int


type Associativity
    = Left
    | Non
    | Right


precedenceEncoder : Precedence -> Encode.Value
precedenceEncoder =
    Encode.int


precedenceDecoder : Decode.Decoder Precedence
precedenceDecoder =
    Decode.int


precedenceCodec : Codec e Precedence
precedenceCodec =
    Serialize.int


associativityEncoder : Associativity -> Encode.Value
associativityEncoder associativity =
    case associativity of
        Left ->
            Encode.string "Left"

        Non ->
            Encode.string "Non"

        Right ->
            Encode.string "Right"


associativityDecoder : Decode.Decoder Associativity
associativityDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Left" ->
                        Decode.succeed Left

                    "Non" ->
                        Decode.succeed Non

                    "Right" ->
                        Decode.succeed Right

                    _ ->
                        Decode.fail ("Unknown Associativity: " ++ str)
            )


associativityCodec : Codec e Associativity
associativityCodec =
    Serialize.customType
        (\leftEncoder nonEncoder rightEncoder value ->
            case value of
                Left ->
                    leftEncoder

                Non ->
                    nonEncoder

                Right ->
                    rightEncoder
        )
        |> Serialize.variant0 Left
        |> Serialize.variant0 Non
        |> Serialize.variant0 Right
        |> Serialize.finishCustomType
