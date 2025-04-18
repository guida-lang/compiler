module Compiler.Parse.Number exposing
    ( Number(..)
    , Outcome(..)
    , chompHex
    , number
    , precedence
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Error.Syntax as E



-- HELPERS


isDirtyEnd : String -> Int -> Int -> Char -> Bool
isDirtyEnd src pos end word =
    Var.getInnerWidthHelp src pos end word > 0


isDecimalDigit : Char -> Bool
isDecimalDigit word =
    Char.isDigit word



-- NUMBERS


type Number
    = Int Int String
    | Float Float String


number : (Row -> Col -> x) -> (E.Number -> Row -> Col -> x) -> P.Parser x Number
number toExpectation toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                P.Eerr row col toExpectation

            else
                let
                    word =
                        charAtPos pos src
                in
                if word == '_' then
                    P.Cerr row col (toError E.NumberNoLeadingOrTrailingUnderscores)

                else if not (isDecimalDigit word) then
                    P.Eerr row col toExpectation

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
                                newCol : Col
                                newCol =
                                    col + (newPos - pos)
                            in
                            P.Cerr row newCol (toError problem)

                        OkInt newPos n ->
                            let
                                newCol : Col
                                newCol =
                                    col + (newPos - pos)

                                integer : Number
                                integer =
                                    Int n (String.slice pos newPos src)

                                newState : P.State
                                newState =
                                    P.State src newPos end indent row newCol
                            in
                            P.Cok integer newState

                        OkFloat newPos ->
                            let
                                newCol : Col
                                newCol =
                                    col + (newPos - pos)

                                raw : String
                                raw =
                                    String.slice pos newPos src

                                newState : P.State
                                newState =
                                    P.State src newPos end indent row newCol
                            in
                            case String.toFloat (raw |> String.replace "_" "") of
                                Just copy_ ->
                                    P.Cok (Float copy_ raw) newState

                                Nothing ->
                                    P.Eerr row newCol toExpectation



-- CHOMP OUTCOME


type Outcome
    = Err_ Int E.Number
    | OkInt Int Int
    | OkFloat Int



-- CHOMP INT


chompInt : String -> Int -> Int -> Int -> Outcome
chompInt src pos end n =
    if pos >= end then
        OkInt pos n

    else
        let
            word =
                charAtPos pos src
        in
        if isDecimalDigit word then
            chompInt src (pos + 1) end (10 * n + (Char.toCode word - Char.toCode '0'))

        else if word == '.' then
            chompFraction src pos end n

        else if word == 'e' || word == 'E' then
            chompExponent src (pos + 1) end

        else if word == '_' then
            chompUnderscore_ src pos end n

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkInt pos n



-- CHOMP UNDERSCORE


chompUnderscore_ : String -> Int -> Int -> Int -> Outcome
chompUnderscore_ src pos end n =
    let
        nextWord =
            charAtPos (pos + 1) src
    in
    if pos >= end then
        Err_ pos E.NumberNoLeadingOrTrailingUnderscores

    else if nextWord == '_' then
        Err_ pos E.NumberNoConsecutiveUnderscores

    else if nextWord == 'e' || nextWord == 'E' then
        Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

    else if nextWord == '.' then
        Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

    else if isDecimalDigit nextWord then
        chompUnderscoreHelp src (pos + 1) end n

    else
        Err_ pos (E.NumberDot n)


chompUnderscoreHelp : String -> Int -> Int -> Int -> Outcome
chompUnderscoreHelp src pos end n =
    if pos >= end then
        OkInt pos n

    else
        let
            word =
                charAtPos pos src

            nextWord =
                charAtPos (pos + 1) src
        in
        if word == '_' then
            if nextWord == '_' then
                Err_ pos E.NumberNoConsecutiveUnderscores

            else if nextWord == 'e' || nextWord == 'E' || nextWord == '.' then
                Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

            else if pos + 1 == end then
                Err_ pos E.NumberNoLeadingOrTrailingUnderscores

            else
                chompUnderscoreHelp src (pos + 1) end n

        else if word == '.' then
            chompFraction src pos end n

        else if isDecimalDigit word then
            chompUnderscoreHelp src (pos + 1) end (10 * n + (Char.toCode word - Char.toCode '0'))

        else
            OkInt pos n



-- CHOMP FRACTION


chompFraction : String -> Int -> Int -> Int -> Outcome
chompFraction src pos end n =
    let
        pos1 : Int
        pos1 =
            pos + 1

        nextWord =
            charAtPos pos1 src
    in
    if pos1 >= end then
        Err_ pos (E.NumberDot n)

    else if nextWord == '_' then
        Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

    else if isDecimalDigit nextWord then
        chompFractionHelp src (pos1 + 1) end

    else
        Err_ pos (E.NumberDot n)


chompFractionHelp : String -> Int -> Int -> Outcome
chompFractionHelp src pos end =
    if pos >= end then
        OkFloat pos

    else
        let
            word =
                charAtPos pos src

            nextWord =
                charAtPos (pos + 1) src
        in
        if isDecimalDigit word then
            chompFractionHelp src (pos + 1) end

        else if word == '_' then
            if (pos + 1) == end then
                Err_ pos E.NumberNoLeadingOrTrailingUnderscores

            else if nextWord == '_' then
                Err_ pos E.NumberNoConsecutiveUnderscores

            else if nextWord == 'e' || nextWord == 'E' then
                Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

            else
                chompFractionHelp src (pos + 1) end

        else if word == 'e' || word == 'E' then
            chompExponent src (pos + 1) end

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkFloat pos



-- CHOMP EXPONENT


chompExponent : String -> Int -> Int -> Outcome
chompExponent src pos end =
    if pos >= end then
        Err_ pos E.NumberEnd

    else
        let
            word =
                charAtPos pos src
        in
        if isDecimalDigit word then
            chompExponentHelp src (pos + 1) end

        else if word == '_' then
            Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

        else if word == '+' || word == '-' then
            let
                pos1 : Int
                pos1 =
                    pos + 1

                nextWord =
                    charAtPos pos1 src
            in
            if nextWord == '_' then
                Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

            else if pos1 < end && isDecimalDigit nextWord then
                chompExponentHelp src (pos + 2) end

            else
                Err_ pos E.NumberEnd

        else
            Err_ pos E.NumberEnd


chompExponentHelp : String -> Int -> Int -> Outcome
chompExponentHelp src pos end =
    let
        word =
            charAtPos pos src
    in
    if pos >= end then
        OkFloat pos

    else if isDecimalDigit word || word == '_' then
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
            word =
                charAtPos pos src
        in
        if word == 'x' then
            chompHexInt src (pos + 1) end

        else if word == '.' then
            chompFraction src pos end 0

        else if isDecimalDigit word then
            Err_ pos E.NumberNoLeadingZero

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkInt pos 0


chompHexInt : String -> Int -> Int -> Outcome
chompHexInt src pos end =
    let
        ( newPos, answer ) =
            chompHex src pos end
    in
    if answer < 0 then
        Err_ newPos E.NumberHexDigit

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
                stepHex src pos end (charAtPos pos src) accumulator
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


precedence : (Row -> Col -> x) -> P.Parser x Binop.Precedence
precedence toExpectation =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                P.Eerr row col toExpectation

            else
                let
                    word =
                        charAtPos pos src
                in
                if isDecimalDigit word then
                    P.Cok
                        (Char.toCode word - Char.toCode '0')
                        (P.State src (pos + 1) end indent row (col + 1))

                else
                    P.Eerr row col toExpectation



-- helpers


charAtPos : Int -> String -> Char
charAtPos pos src =
    String.dropLeft pos src
        |> String.uncons
        |> Maybe.map Tuple.first
        |> Maybe.withDefault ' '
