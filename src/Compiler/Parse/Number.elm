module Compiler.Parse.Number exposing
    ( Number(..)
    , Outcome(..)
    , chompHex
    , number
    , precedence
    )

import Compiler.Parse.Primitives as P
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Error.Syntax as E
import Types as T
import Utils.Crash exposing (crash)



-- HELPERS


isDirtyEnd : String -> Int -> Int -> Char -> Bool
isDirtyEnd src pos end word =
    Var.getInnerWidthHelp src pos end word > 0


isDecimalDigit : Char -> Bool
isDecimalDigit word =
    Char.isDigit word



-- NUMBERS


type Number
    = Int Int
    | Float Float


number : (T.CPP_Row -> T.CPP_Col -> x) -> (E.CRES_Number -> T.CPP_Row -> T.CPP_Col -> x) -> P.Parser x Number
number toExpectation toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                Err (P.PErr P.Empty row col toExpectation)

            else
                let
                    word : Char
                    word =
                        String.uncons (String.dropLeft pos src) |> Maybe.map Tuple.first |> Maybe.withDefault ' '
                in
                if not (isDecimalDigit word) then
                    Err (P.PErr P.Empty row col toExpectation)

                else
                    let
                        outcome : Outcome
                        outcome =
                            if word == '0' then
                                chompZero src (pos + 1) end

                            else
                                chompInt src (pos + 1) end (Char.toCode word - Char.toCode '0')
                    in
                    case outcome of
                        Err_ newPos problem ->
                            let
                                newCol : T.CPP_Col
                                newCol =
                                    col + (newPos - pos)
                            in
                            Err (P.PErr P.Consumed row newCol (toError problem))

                        OkInt newPos n ->
                            let
                                newCol : T.CPP_Col
                                newCol =
                                    col + (newPos - pos)

                                integer : Number
                                integer =
                                    Int n

                                newState : P.State
                                newState =
                                    P.State src newPos end indent row newCol
                            in
                            Ok (P.POk P.Consumed integer newState)

                        OkFloat newPos ->
                            let
                                newCol : T.CPP_Col
                                newCol =
                                    col + (newPos - pos)

                                copy : Float
                                copy =
                                    case String.toFloat (String.slice pos newPos src) of
                                        Just copy_ ->
                                            copy_

                                        Nothing ->
                                            crash "Failed `String.toFloat`"

                                float : Number
                                float =
                                    Float copy

                                newState : P.State
                                newState =
                                    P.State src newPos end indent row newCol
                            in
                            Ok (P.POk P.Consumed float newState)



-- CHOMP OUTCOME


type Outcome
    = Err_ Int E.CRES_Number
    | OkInt Int Int
    | OkFloat Int



-- CHOMP INT


chompInt : String -> Int -> Int -> Int -> Outcome
chompInt src pos end n =
    if pos >= end then
        OkInt pos n

    else
        let
            word : Char
            word =
                String.uncons (String.dropLeft pos src) |> Maybe.map Tuple.first |> Maybe.withDefault ' '
        in
        if isDecimalDigit word then
            chompInt src (pos + 1) end (10 * n + (Char.toCode word - Char.toCode '0'))

        else if word == '.' then
            chompFraction src pos end n

        else if word == 'e' || word == 'E' then
            chompExponent src (pos + 1) end

        else if isDirtyEnd src pos end word then
            Err_ pos E.CRES_NumberEnd

        else
            OkInt pos n



-- CHOMP FRACTION


chompFraction : String -> Int -> Int -> Int -> Outcome
chompFraction src pos end n =
    let
        pos1 : Int
        pos1 =
            pos + 1
    in
    if pos1 >= end then
        Err_ pos (E.CRES_NumberDot n)

    else if isDecimalDigit (String.uncons (String.dropLeft pos1 src) |> Maybe.map Tuple.first |> Maybe.withDefault ' ') then
        chompFractionHelp src (pos1 + 1) end

    else
        Err_ pos (E.CRES_NumberDot n)


chompFractionHelp : String -> Int -> Int -> Outcome
chompFractionHelp src pos end =
    if pos >= end then
        OkFloat pos

    else
        let
            word : Char
            word =
                String.uncons (String.dropLeft pos src) |> Maybe.map Tuple.first |> Maybe.withDefault ' '
        in
        if isDecimalDigit word then
            chompFractionHelp src (pos + 1) end

        else if word == 'e' || word == 'E' then
            chompExponent src (pos + 1) end

        else if isDirtyEnd src pos end word then
            Err_ pos E.CRES_NumberEnd

        else
            OkFloat pos



-- CHOMP EXPONENT


chompExponent : String -> Int -> Int -> Outcome
chompExponent src pos end =
    if pos >= end then
        Err_ pos E.CRES_NumberEnd

    else
        let
            word : Char
            word =
                String.uncons (String.dropLeft pos src) |> Maybe.map Tuple.first |> Maybe.withDefault ' '
        in
        if isDecimalDigit word then
            chompExponentHelp src (pos + 1) end

        else if word == '+' || word == '-' then
            let
                pos1 : Int
                pos1 =
                    pos + 1
            in
            if pos1 < end && isDecimalDigit (String.uncons (String.dropLeft pos1 src) |> Maybe.map Tuple.first |> Maybe.withDefault ' ') then
                chompExponentHelp src (pos + 2) end

            else
                Err_ pos E.CRES_NumberEnd

        else
            Err_ pos E.CRES_NumberEnd


chompExponentHelp : String -> Int -> Int -> Outcome
chompExponentHelp src pos end =
    if pos >= end then
        OkFloat pos

    else if isDecimalDigit (String.uncons (String.dropLeft pos src) |> Maybe.map Tuple.first |> Maybe.withDefault ' ') then
        chompExponentHelp src (pos + 1) end

    else
        OkFloat pos



-- CHOMP ZERO


chompZero : String -> Int -> Int -> Outcome
chompZero src pos end =
    if pos >= end then
        OkInt pos 0

    else
        let
            word : Char
            word =
                String.uncons (String.dropLeft pos src) |> Maybe.map Tuple.first |> Maybe.withDefault ' '
        in
        if word == 'x' then
            chompHexInt src (pos + 1) end

        else if word == '.' then
            chompFraction src pos end 0

        else if isDecimalDigit word then
            Err_ pos E.CRES_NumberNoLeadingZero

        else if isDirtyEnd src pos end word then
            Err_ pos E.CRES_NumberEnd

        else
            OkInt pos 0


chompHexInt : String -> Int -> Int -> Outcome
chompHexInt src pos end =
    let
        ( newPos, answer ) =
            chompHex src pos end
    in
    if answer < 0 then
        Err_ newPos E.CRES_NumberHexDigit

    else
        OkInt newPos answer



-- CHOMP HEX


chompHex : String -> Int -> Int -> ( Int, Int )
chompHex src pos end =
    chompHexHelp src pos end -1 0


chompHexHelp : String -> Int -> Int -> Int -> Int -> ( Int, Int )
chompHexHelp src pos end answer accumulator =
    if pos >= end then
        ( pos, answer )

    else
        let
            newAnswer : Int
            newAnswer =
                stepHex src pos end (String.uncons (String.dropLeft pos src) |> Maybe.map Tuple.first |> Maybe.withDefault ' ') accumulator
        in
        if newAnswer < 0 then
            ( pos
            , if newAnswer == -1 then
                answer

              else
                -2
            )

        else
            chompHexHelp src (pos + 1) end newAnswer newAnswer


stepHex : String -> Int -> Int -> Char -> Int -> Int
stepHex src pos end word acc =
    if '0' <= word && word <= '9' then
        16 * acc + (Char.toCode word - Char.toCode '0')

    else if 'a' <= word && word <= 'f' then
        16 * acc + 10 + (Char.toCode word - Char.toCode 'a')

    else if 'A' <= word && word <= 'F' then
        16 * acc + 10 + (Char.toCode word - Char.toCode 'A')

    else if isDirtyEnd src pos end word then
        -2

    else
        -1



-- PRECEDENCE


precedence : (T.CPP_Row -> T.CPP_Col -> x) -> P.Parser x T.CASTUB_Precedence
precedence toExpectation =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                Err (P.PErr P.Empty row col toExpectation)

            else
                let
                    word : Char
                    word =
                        String.uncons (String.dropLeft pos src) |> Maybe.map Tuple.first |> Maybe.withDefault ' '
                in
                if isDecimalDigit word then
                    Ok
                        (P.POk P.Consumed
                            (Char.toCode word - Char.toCode '0')
                            (P.State src (pos + 1) end indent row (col + 1))
                        )

                else
                    Err (P.PErr P.Empty row col toExpectation)
