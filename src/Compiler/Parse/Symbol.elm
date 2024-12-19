module Compiler.Parse.Symbol exposing
    ( badOperatorDecoder
    , badOperatorEncoder
    , binopCharSet
    , operator
    )

import Compiler.Parse.Primitives as P exposing (Parser)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- OPERATOR


operator : (T.CPP_Row -> T.CPP_Col -> x) -> (T.CPS_BadOperator -> T.CPP_Row -> T.CPP_Col -> x) -> Parser x T.CDN_Name
operator toExpectation toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                newPos : Int
                newPos =
                    chompOps src pos end
            in
            if pos == newPos then
                Err (P.PErr P.Empty row col toExpectation)

            else
                case String.slice pos newPos src of
                    "." ->
                        Err (P.PErr P.Empty row col (toError T.CPS_BadDot))

                    "|" ->
                        Err (P.PErr P.Consumed row col (toError T.CPS_BadPipe))

                    "->" ->
                        Err (P.PErr P.Consumed row col (toError T.CPS_BadArrow))

                    "=" ->
                        Err (P.PErr P.Consumed row col (toError T.CPS_BadEquals))

                    ":" ->
                        Err (P.PErr P.Consumed row col (toError T.CPS_BadHasType))

                    op ->
                        let
                            newCol : T.CPP_Col
                            newCol =
                                col + (newPos - pos)

                            newState : P.State
                            newState =
                                P.State src newPos end indent row newCol
                        in
                        Ok (P.POk P.Consumed op newState)


chompOps : String -> Int -> Int -> Int
chompOps src pos end =
    if pos < end && isBinopCharHelp (P.unsafeIndex src pos) then
        chompOps src (pos + 1) end

    else
        pos


isBinopCharHelp : Char -> Bool
isBinopCharHelp char =
    let
        code : Int
        code =
            Char.toCode char
    in
    EverySet.member identity code binopCharSet


binopCharSet : EverySet Int Int
binopCharSet =
    EverySet.fromList identity (List.map Char.toCode (String.toList "+-/*=.<>:&|^?%!"))



-- ENCODERS and DECODERS


badOperatorEncoder : T.CPS_BadOperator -> Encode.Value
badOperatorEncoder badOperator =
    case badOperator of
        T.CPS_BadDot ->
            Encode.string "BadDot"

        T.CPS_BadPipe ->
            Encode.string "BadPipe"

        T.CPS_BadArrow ->
            Encode.string "BadArrow"

        T.CPS_BadEquals ->
            Encode.string "BadEquals"

        T.CPS_BadHasType ->
            Encode.string "BadHasType"


badOperatorDecoder : Decode.Decoder T.CPS_BadOperator
badOperatorDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "BadDot" ->
                        Decode.succeed T.CPS_BadDot

                    "BadPipe" ->
                        Decode.succeed T.CPS_BadPipe

                    "BadArrow" ->
                        Decode.succeed T.CPS_BadArrow

                    "BadEquals" ->
                        Decode.succeed T.CPS_BadEquals

                    "BadHasType" ->
                        Decode.succeed T.CPS_BadHasType

                    _ ->
                        Decode.fail ("Unknown BadOperator: " ++ str)
            )
