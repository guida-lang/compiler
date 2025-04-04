module Compiler.AST.Utils.Binop exposing
    ( Associativity(..)
    , Precedence
    , associativityDecoder
    , associativityEncoder
    , jsonAssociativityDecoder
    , jsonAssociativityEncoder
    , jsonPrecedenceDecoder
    , jsonPrecedenceEncoder
    , precedenceDecoder
    , precedenceEncoder
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- BINOP STUFF


type alias Precedence =
    Int


type Associativity
    = Left
    | Non
    | Right



-- JSON ENCODERS and DECODERS


jsonPrecedenceEncoder : Precedence -> Encode.Value
jsonPrecedenceEncoder =
    Encode.int


jsonPrecedenceDecoder : Decode.Decoder Precedence
jsonPrecedenceDecoder =
    Decode.int


jsonAssociativityEncoder : Associativity -> Encode.Value
jsonAssociativityEncoder associativity =
    case associativity of
        Left ->
            Encode.string "Left"

        Non ->
            Encode.string "Non"

        Right ->
            Encode.string "Right"


jsonAssociativityDecoder : Decode.Decoder Associativity
jsonAssociativityDecoder =
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



-- ENCODERS and DECODERS


precedenceEncoder : Precedence -> BE.Encoder
precedenceEncoder =
    BE.int


precedenceDecoder : BD.Decoder Precedence
precedenceDecoder =
    BD.int


associativityEncoder : Associativity -> BE.Encoder
associativityEncoder associativity =
    BE.unsignedInt8
        (case associativity of
            Left ->
                0

            Non ->
                1

            Right ->
                2
        )


associativityDecoder : BD.Decoder Associativity
associativityDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Left

                    1 ->
                        BD.succeed Non

                    2 ->
                        BD.succeed Right

                    _ ->
                        BD.fail
            )
