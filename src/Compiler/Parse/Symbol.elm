module Compiler.Parse.Symbol exposing
    ( BadOperator(..)
    , badOperatorDecoder
    , badOperatorEncoder
    , binopCharSet
    , operator
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Parse.Primitives as P exposing (Col, Parser, Row)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode



-- OPERATOR


type BadOperator
    = BadDot
    | BadPipe
    | BadArrow
    | BadEquals
    | BadHasType


operator : (Row -> Col -> x) -> (BadOperator -> Row -> Col -> x) -> Parser x Name
operator toExpectation toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                newPos : Int
                newPos =
                    chompOps src pos end
            in
            if pos == newPos then
                P.Eerr row col toExpectation

            else
                case String.slice pos newPos src of
                    "." ->
                        P.Eerr row col (toError BadDot)

                    "|" ->
                        P.Cerr row col (toError BadPipe)

                    "->" ->
                        P.Cerr row col (toError BadArrow)

                    "=" ->
                        P.Cerr row col (toError BadEquals)

                    ":" ->
                        P.Cerr row col (toError BadHasType)

                    op ->
                        let
                            newCol : Col
                            newCol =
                                col + (newPos - pos)

                            newState : P.State
                            newState =
                                P.State src newPos end indent row newCol
                        in
                        P.Cok op newState


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


badOperatorEncoder : BadOperator -> Encode.Value
badOperatorEncoder badOperator =
    case badOperator of
        BadDot ->
            Encode.string "BadDot"

        BadPipe ->
            Encode.string "BadPipe"

        BadArrow ->
            Encode.string "BadArrow"

        BadEquals ->
            Encode.string "BadEquals"

        BadHasType ->
            Encode.string "BadHasType"


badOperatorDecoder : Decode.Decoder BadOperator
badOperatorDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "BadDot" ->
                        Decode.succeed BadDot

                    "BadPipe" ->
                        Decode.succeed BadPipe

                    "BadArrow" ->
                        Decode.succeed BadArrow

                    "BadEquals" ->
                        Decode.succeed BadEquals

                    "BadHasType" ->
                        Decode.succeed BadHasType

                    _ ->
                        Decode.fail ("Unknown BadOperator: " ++ str)
            )
