module Compiler.Parse.Symbol exposing
    ( CPS_BadOperator(..)
    , badOperatorDecoder
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


type CPS_BadOperator
    = CPS_BadDot
    | CPS_BadPipe
    | CPS_BadArrow
    | CPS_BadEquals
    | CPS_BadHasType


operator : (T.CPP_Row -> T.CPP_Col -> x) -> (CPS_BadOperator -> T.CPP_Row -> T.CPP_Col -> x) -> Parser x T.CDN_Name
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
                        Err (P.PErr P.Empty row col (toError CPS_BadDot))

                    "|" ->
                        Err (P.PErr P.Consumed row col (toError CPS_BadPipe))

                    "->" ->
                        Err (P.PErr P.Consumed row col (toError CPS_BadArrow))

                    "=" ->
                        Err (P.PErr P.Consumed row col (toError CPS_BadEquals))

                    ":" ->
                        Err (P.PErr P.Consumed row col (toError CPS_BadHasType))

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


badOperatorEncoder : CPS_BadOperator -> Encode.Value
badOperatorEncoder badOperator =
    case badOperator of
        CPS_BadDot ->
            Encode.string "BadDot"

        CPS_BadPipe ->
            Encode.string "BadPipe"

        CPS_BadArrow ->
            Encode.string "BadArrow"

        CPS_BadEquals ->
            Encode.string "BadEquals"

        CPS_BadHasType ->
            Encode.string "BadHasType"


badOperatorDecoder : Decode.Decoder CPS_BadOperator
badOperatorDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "BadDot" ->
                        Decode.succeed CPS_BadDot

                    "BadPipe" ->
                        Decode.succeed CPS_BadPipe

                    "BadArrow" ->
                        Decode.succeed CPS_BadArrow

                    "BadEquals" ->
                        Decode.succeed CPS_BadEquals

                    "BadHasType" ->
                        Decode.succeed CPS_BadHasType

                    _ ->
                        Decode.fail ("Unknown BadOperator: " ++ str)
            )
