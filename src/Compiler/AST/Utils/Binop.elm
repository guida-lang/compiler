module Compiler.AST.Utils.Binop exposing
    ( CASTU_Associativity(..)
    , CASTU_Precedence
    , associativityDecoder
    , associativityEncoder
    , precedenceDecoder
    , precedenceEncoder
    )

import Json.Decode as Decode
import Json.Encode as Encode



-- BINOP STUFF


type alias CASTU_Precedence =
    Int


type CASTU_Associativity
    = CASTU_Left
    | CASTU_Non
    | CASTU_Right


precedenceEncoder : CASTU_Precedence -> Encode.Value
precedenceEncoder =
    Encode.int


precedenceDecoder : Decode.Decoder CASTU_Precedence
precedenceDecoder =
    Decode.int


associativityEncoder : CASTU_Associativity -> Encode.Value
associativityEncoder associativity =
    case associativity of
        CASTU_Left ->
            Encode.string "Left"

        CASTU_Non ->
            Encode.string "Non"

        CASTU_Right ->
            Encode.string "Right"


associativityDecoder : Decode.Decoder CASTU_Associativity
associativityDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Left" ->
                        Decode.succeed CASTU_Left

                    "Non" ->
                        Decode.succeed CASTU_Non

                    "Right" ->
                        Decode.succeed CASTU_Right

                    _ ->
                        Decode.fail ("Unknown Associativity: " ++ str)
            )
