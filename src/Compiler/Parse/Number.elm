module Compiler.Parse.Number exposing
    ( Number(..)
    , Outcome(..)
    , chompHex
    , number
    , precedence
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion(..))
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


number : SyntaxVersion -> (Row -> Col -> x) -> (E.Number -> Row -> Col -> x) -> P.Parser x Number
number syntaxVersion toExpectation toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                P.Eerr row col toExpectation

            else
                let
                    word =
                        charAtPos pos src

                    isGuida =
                        syntaxVersion == Guida
                in
                if word == '_' && isGuida then
                    P.Cerr row col (toError E.NumberNoLeadingOrTrailingUnderscores)

                else if not (isDecimalDigit word) then
                    P.Eerr row col toExpectation

                else
                    let
                        outcome : Outcome
                        outcome =
                            if word == '0' then
                                chompZero isGuida src (pos + 1) end

                            else
                                chompInt isGuida src (pos + 1) end (Char.toCode word - Char.toCode '0')
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

                                parsed =
                                    if isGuida then
                                        String.replace "_" "" raw |> String.toFloat

                                    else
                                        String.toFloat raw
                            in
                            case parsed of
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


chompInt : Bool -> String -> Int -> Int -> Int -> Outcome
chompInt isGuida src pos end n =
    if pos >= end then
        OkInt pos n

    else
        let
            word =
                charAtPos pos src
        in
        if isDecimalDigit word then
            chompInt isGuida src (pos + 1) end (10 * n + (Char.toCode word - Char.toCode '0'))

        else if word == '.' then
            chompFraction isGuida src pos end n

        else if word == 'e' || word == 'E' then
            chompExponent isGuida src (pos + 1) end

        else if isGuida && word == '_' then
            chompUnderscore_ isGuida src pos end n

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkInt pos n



-- CHOMP UNDERSCORE


chompUnderscore_ : Bool -> String -> Int -> Int -> Int -> Outcome
chompUnderscore_ isGuida src pos end n =
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
        chompUnderscoreHelp isGuida src (pos + 1) end n

    else
        Err_ pos (E.NumberDot n)


chompUnderscoreHelp : Bool -> String -> Int -> Int -> Int -> Outcome
chompUnderscoreHelp isGuida src pos end n =
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
                chompUnderscoreHelp isGuida src (pos + 1) end n

        else if word == '.' then
            chompFraction isGuida src pos end n

        else if isDecimalDigit word then
            chompUnderscoreHelp isGuida src (pos + 1) end (10 * n + (Char.toCode word - Char.toCode '0'))

        else
            OkInt pos n



-- CHOMP FRACTION


chompFraction : Bool -> String -> Int -> Int -> Int -> Outcome
chompFraction isGuida src pos end n =
    let
        pos1 : Int
        pos1 =
            pos + 1

        nextWord =
            charAtPos pos1 src
    in
    if pos1 >= end then
        Err_ pos (E.NumberDot n)

    else if isGuida && nextWord == '_' then
        Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

    else if isDecimalDigit nextWord then
        chompFractionHelp isGuida src (pos1 + 1) end

    else
        Err_ pos (E.NumberDot n)


chompFractionHelp : Bool -> String -> Int -> Int -> Outcome
chompFractionHelp isGuida src pos end =
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
            chompFractionHelp isGuida src (pos + 1) end

        else if isGuida && word == '_' then
            if (pos + 1) == end then
                Err_ pos E.NumberNoLeadingOrTrailingUnderscores

            else if nextWord == '_' then
                Err_ pos E.NumberNoConsecutiveUnderscores

            else if nextWord == 'e' || nextWord == 'E' then
                Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

            else
                chompFractionHelp isGuida src (pos + 1) end

        else if word == 'e' || word == 'E' then
            chompExponent isGuida src (pos + 1) end

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkFloat pos



-- CHOMP EXPONENT


chompExponent : Bool -> String -> Int -> Int -> Outcome
chompExponent isGuida src pos end =
    if pos >= end then
        Err_ pos E.NumberEnd

    else
        let
            word =
                charAtPos pos src
        in
        if isDecimalDigit word then
            chompExponentHelp isGuida src (pos + 1) end

        else if isGuida && word == '_' then
            Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

        else if word == '+' || word == '-' then
            let
                pos1 : Int
                pos1 =
                    pos + 1

                nextWord =
                    charAtPos pos1 src
            in
            if isGuida && nextWord == '_' then
                Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

            else if pos1 < end && isDecimalDigit nextWord then
                chompExponentHelp isGuida src (pos + 2) end

            else
                Err_ pos E.NumberEnd

        else
            Err_ pos E.NumberEnd


chompExponentHelp : Bool -> String -> Int -> Int -> Outcome
chompExponentHelp isGuida src pos end =
    let
        word =
            charAtPos pos src
    in
    if pos >= end then
        OkFloat pos

    else if isDecimalDigit word || (isGuida && word == '_') then
        chompExponentHelp isGuida src (pos + 1) end

    else
        OkFloat pos



-- CHOMP ZERO


chompZero : Bool -> String -> Int -> Int -> Outcome
chompZero isGuida src pos end =
    if pos >= end then
        OkInt pos 0

    else
        let
            word =
                charAtPos pos src
        in
        if word == 'x' then
            if isGuida && charAtPos (pos + 1) src == '_' then
                Err_ pos E.NumberNoUnderscoresAdjacentToHexadecimalPreFix

            else
                chompHexInt isGuida src (pos + 1) end

        else if word == '.' then
            chompFraction isGuida src pos end 0

        else if isDecimalDigit word then
            Err_ pos E.NumberNoLeadingZero

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkInt pos 0


chompHexInt : Bool -> String -> Int -> Int -> Outcome
chompHexInt isGuida src pos end =
    let
        ( newPos, answer ) =
            chompHex isGuida src pos end
    in
    if answer == -4 then
        Err_ newPos E.NumberNoConsecutiveUnderscores

    else if answer == -3 then
        Err_ newPos E.NumberNoLeadingOrTrailingUnderscores

    else if answer < 0 then
        Err_ newPos E.NumberHexDigit

    else
        OkInt newPos answer



-- CHOMP HEX


chompHex : Bool -> String -> Int -> Int -> ( Int, Int )
chompHex isGuida src pos end =
    chompHexHelp isGuida src pos end -1 0


chompHexHelp : Bool -> String -> Int -> Int -> Int -> Int -> ( Int, Int )
chompHexHelp isGuida src pos end answer accumulator =
    if pos >= end then
        ( pos, answer )

    else
        let
            newAnswer : Int
            newAnswer =
                stepHex isGuida src pos end (charAtPos pos src) accumulator
        in
        if newAnswer < 0 then
            ( pos
            , if newAnswer == -1 then
                answer

              else if newAnswer == -3 then
                -3

              else if newAnswer == -4 then
                -4

              else
                -2
            )

        else
            chompHexHelp isGuida src (pos + 1) end newAnswer newAnswer


stepHex : Bool -> String -> Int -> Int -> Char -> Int -> Int
stepHex isGuida src pos end word acc =
    if '0' <= word && word <= '9' then
        16 * acc + (Char.toCode word - Char.toCode '0')

    else if 'a' <= word && word <= 'f' then
        16 * acc + 10 + (Char.toCode word - Char.toCode 'a')

    else if 'A' <= word && word <= 'F' then
        16 * acc + 10 + (Char.toCode word - Char.toCode 'A')

    else if isGuida && '_' == word then
        let
            nextWord =
                charAtPos (pos + 1) src

            validNextWord =
                ('0' <= nextWord && nextWord <= '9')
                    || ('a' <= nextWord && nextWord <= 'f')
                    || ('A' <= nextWord && nextWord <= 'F')
        in
        if nextWord == '_' then
            -4

        else if pos + 1 == end || not validNextWord then
            -3

        else
            acc

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
