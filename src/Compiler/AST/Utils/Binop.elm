module Compiler.AST.Utils.Binop exposing
    ( associativityDecoder
    , associativityEncoder
    , precedenceDecoder
    , precedenceEncoder
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- BINOP STUFF


precedenceEncoder : T.CASTUB_Precedence -> Encode.Value
precedenceEncoder =
    Encode.int


precedenceDecoder : Decode.Decoder T.CASTUB_Precedence
precedenceDecoder =
    Decode.int


associativityEncoder : T.CASTUB_Associativity -> Encode.Value
associativityEncoder associativity =
    case associativity of
        T.CASTUB_Left ->
            Encode.string "Left"

        T.CASTUB_Non ->
            Encode.string "Non"

        T.CASTUB_Right ->
            Encode.string "Right"


associativityDecoder : Decode.Decoder T.CASTUB_Associativity
associativityDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Left" ->
                        Decode.succeed T.CASTUB_Left

                    "Non" ->
                        Decode.succeed T.CASTUB_Non

                    "Right" ->
                        Decode.succeed T.CASTUB_Right

                    _ ->
                        Decode.fail ("Unknown Associativity: " ++ str)
            )
