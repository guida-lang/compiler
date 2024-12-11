module Compiler.Parse.Symbol exposing
    ( BadOperator(..)
    , badOperatorCodec
    , binopCharSet
    , operator
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Parse.Primitives as P exposing (Col, Parser, Row)
import Data.Set as EverySet exposing (EverySet)
import Serialize exposing (Codec)



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
                Err (P.PErr P.Empty row col toExpectation)

            else
                case String.slice pos newPos src of
                    "." ->
                        Err (P.PErr P.Empty row col (toError BadDot))

                    "|" ->
                        Err (P.PErr P.Consumed row col (toError BadPipe))

                    "->" ->
                        Err (P.PErr P.Consumed row col (toError BadArrow))

                    "=" ->
                        Err (P.PErr P.Consumed row col (toError BadEquals))

                    ":" ->
                        Err (P.PErr P.Consumed row col (toError BadHasType))

                    op ->
                        let
                            newCol : Col
                            newCol =
                                col + (newPos - pos)

                            newState : P.State
                            newState =
                                P.State src newPos end indent row newCol
                        in
                        Ok (P.POk P.Consumed op newState)


chompOps : String -> Int -> Int -> Int
chompOps src pos end =
    if pos < end && isBinopChar src pos then
        chompOps src (pos + 1) end

    else
        pos


isBinopChar : String -> Int -> Bool
isBinopChar src pos =
    src
        |> String.dropLeft pos
        |> String.toList
        |> List.head
        |> Maybe.map isBinopCharHelp
        |> Maybe.withDefault False


isBinopCharHelp : Char -> Bool
isBinopCharHelp char =
    let
        code : Int
        code =
            Char.toCode char
    in
    EverySet.member code binopCharSet


binopCharSet : EverySet Int
binopCharSet =
    EverySet.fromList compare (List.map Char.toCode (String.toList "+-/*=.<>:&|^?%!"))



-- ENCODERS and DECODERS


badOperatorCodec : Codec e BadOperator
badOperatorCodec =
    Serialize.customType
        (\badDotEncoder badPipeEncoder badArrowEncoder badEqualsEncoder badHasTypeEncoder badOperator ->
            case badOperator of
                BadDot ->
                    badDotEncoder

                BadPipe ->
                    badPipeEncoder

                BadArrow ->
                    badArrowEncoder

                BadEquals ->
                    badEqualsEncoder

                BadHasType ->
                    badHasTypeEncoder
        )
        |> Serialize.variant0 BadDot
        |> Serialize.variant0 BadPipe
        |> Serialize.variant0 BadArrow
        |> Serialize.variant0 BadEquals
        |> Serialize.variant0 BadHasType
        |> Serialize.finishCustomType
