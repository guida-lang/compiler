module Compiler.Parse.Number exposing
    ( Number(..)
    , Outcome(..)
    , chompHex
    , number
    , precedence
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Parse.SyntaxVersion as SyntaxVersion exposing (SyntaxVersion)
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
                    word : Char
                    word =
                        charAtPos pos src
                in
                if word == '_' && isGuida syntaxVersion then
                    P.Cerr row col (toError E.NumberNoLeadingOrTrailingUnderscores)

                else if not (isDecimalDigit word) then
                    P.Eerr row col toExpectation

                else
                    let
                        outcome : Outcome
                        outcome =
                            if word == '0' then
                                chompZero syntaxVersion src (pos + 1) end

                            else
                                chompInt syntaxVersion src (pos + 1) end (Char.toCode word - Char.toCode '0')
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

                                parsed : Maybe Float
                                parsed =
                                    if isGuida syntaxVersion then
                                        String.replace "_" "" raw |> String.toFloat

                                    else
                                        String.toFloat raw
                            in
                            case parsed of
                                Just copy_ ->
                                    let
                                        newState : P.State
                                        newState =
                                            P.State src newPos end indent row newCol
                                    in
                                    P.Cok (Float copy_ raw) newState

                                Nothing ->
                                    P.Eerr row newCol toExpectation



-- CHOMP OUTCOME


type Outcome
    = Err_ Int E.Number
    | OkInt Int Int
    | OkFloat Int



-- CHOMP INT


chompInt : SyntaxVersion -> String -> Int -> Int -> Int -> Outcome
chompInt syntaxVersion src pos end n =
    if pos >= end then
        OkInt pos n

    else
        let
            word : Char
            word =
                charAtPos pos src
        in
        if isDecimalDigit word then
            chompInt syntaxVersion src (pos + 1) end (10 * n + (Char.toCode word - Char.toCode '0'))

        else if word == '.' then
            chompFraction syntaxVersion src pos end n

        else if word == 'e' || word == 'E' then
            chompExponent syntaxVersion src (pos + 1) end

        else if isGuida syntaxVersion && word == '_' then
            chompUnderscore_ syntaxVersion src pos end n

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkInt pos n



-- CHOMP UNDERSCORE


chompUnderscore_ : SyntaxVersion -> String -> Int -> Int -> Int -> Outcome
chompUnderscore_ syntaxVersion src pos end n =
    if pos >= end then
        Err_ pos E.NumberNoLeadingOrTrailingUnderscores

    else
        let
            nextWord : Char
            nextWord =
                charAtPos (pos + 1) src
        in
        if nextWord == '_' then
            Err_ (pos + 1) E.NumberNoConsecutiveUnderscores

        else if nextWord == 'e' || nextWord == 'E' then
            Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

        else if nextWord == '.' then
            Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

        else if isDecimalDigit nextWord then
            chompUnderscoreHelp syntaxVersion src (pos + 1) end n

        else
            Err_ pos (E.NumberDot n)


chompUnderscoreHelp : SyntaxVersion -> String -> Int -> Int -> Int -> Outcome
chompUnderscoreHelp syntaxVersion src pos end n =
    if pos >= end then
        OkInt pos n

    else
        let
            word : Char
            word =
                charAtPos pos src
        in
        if word == '_' then
            let
                nextWord : Char
                nextWord =
                    charAtPos (pos + 1) src
            in
            if nextWord == '_' then
                Err_ (pos + 1) E.NumberNoConsecutiveUnderscores

            else if nextWord == 'e' || nextWord == 'E' || nextWord == '.' then
                Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

            else if pos + 1 == end then
                Err_ pos E.NumberNoLeadingOrTrailingUnderscores

            else
                chompUnderscoreHelp syntaxVersion src (pos + 1) end n

        else if word == '.' then
            chompFraction syntaxVersion src pos end n

        else if isDecimalDigit word then
            chompUnderscoreHelp syntaxVersion src (pos + 1) end (10 * n + (Char.toCode word - Char.toCode '0'))

        else
            OkInt pos n



-- CHOMP FRACTION


chompFraction : SyntaxVersion -> String -> Int -> Int -> Int -> Outcome
chompFraction syntaxVersion src pos end n =
    let
        pos1 : Int
        pos1 =
            pos + 1
    in
    if pos1 >= end then
        Err_ pos (E.NumberDot n)

    else
        let
            nextWord : Char
            nextWord =
                charAtPos pos1 src
        in
        if isGuida syntaxVersion && nextWord == '_' then
            Err_ (pos + 1) E.NumberNoUnderscoresAdjacentToDecimalOrExponent

        else if isDecimalDigit nextWord then
            chompFractionHelp syntaxVersion src (pos1 + 1) end

        else
            Err_ pos (E.NumberDot n)


chompFractionHelp : SyntaxVersion -> String -> Int -> Int -> Outcome
chompFractionHelp syntaxVersion src pos end =
    if pos >= end then
        OkFloat pos

    else
        let
            word : Char
            word =
                charAtPos pos src
        in
        if isDecimalDigit word then
            chompFractionHelp syntaxVersion src (pos + 1) end

        else if isGuida syntaxVersion && word == '_' then
            if (pos + 1) == end then
                Err_ pos E.NumberNoLeadingOrTrailingUnderscores

            else
                let
                    nextWord : Char
                    nextWord =
                        charAtPos (pos + 1) src
                in
                if nextWord == '_' then
                    Err_ (pos + 1) E.NumberNoConsecutiveUnderscores

                else if nextWord == 'e' || nextWord == 'E' then
                    Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

                else
                    chompFractionHelp syntaxVersion src (pos + 1) end

        else if word == 'e' || word == 'E' then
            chompExponent syntaxVersion src (pos + 1) end

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkFloat pos



-- CHOMP EXPONENT


chompExponent : SyntaxVersion -> String -> Int -> Int -> Outcome
chompExponent syntaxVersion src pos end =
    if pos >= end then
        Err_ pos E.NumberEnd

    else
        let
            word : Char
            word =
                charAtPos pos src
        in
        if isDecimalDigit word then
            chompExponentHelp syntaxVersion src (pos + 1) end

        else if isGuida syntaxVersion && word == '_' then
            Err_ pos E.NumberNoUnderscoresAdjacentToDecimalOrExponent

        else if word == '+' || word == '-' then
            let
                pos1 : Int
                pos1 =
                    pos + 1

                nextWord : Char
                nextWord =
                    charAtPos pos1 src
            in
            if isGuida syntaxVersion && nextWord == '_' then
                Err_ (pos + 1) E.NumberNoUnderscoresAdjacentToDecimalOrExponent

            else if pos1 < end && isDecimalDigit nextWord then
                chompExponentHelp syntaxVersion src (pos + 2) end

            else
                Err_ pos E.NumberEnd

        else
            Err_ pos E.NumberEnd


chompExponentHelp : SyntaxVersion -> String -> Int -> Int -> Outcome
chompExponentHelp syntaxVersion src pos end =
    if pos >= end then
        OkFloat pos

    else
        let
            word : Char
            word =
                charAtPos pos src
        in
        if isDecimalDigit word || (isGuida syntaxVersion && word == '_') then
            chompExponentHelp syntaxVersion src (pos + 1) end

        else
            OkFloat pos



-- CHOMP ZERO


chompZero : SyntaxVersion -> String -> Int -> Int -> Outcome
chompZero syntaxVersion src pos end =
    if pos >= end then
        OkInt pos 0

    else
        let
            word : Char
            word =
                charAtPos pos src
        in
        if word == 'x' then
            if isGuida syntaxVersion && charAtPos (pos + 1) src == '_' then
                Err_ (pos + 1) E.NumberNoUnderscoresAdjacentToHexadecimalPreFix

            else
                chompHexInt syntaxVersion src (pos + 1) end

        else if word == '.' then
            chompFraction syntaxVersion src pos end 0

        else if isDecimalDigit word then
            Err_ pos E.NumberNoLeadingZero

        else if isDirtyEnd src pos end word then
            Err_ pos E.NumberEnd

        else
            OkInt pos 0


chompHexInt : SyntaxVersion -> String -> Int -> Int -> Outcome
chompHexInt syntaxVersion src pos end =
    let
        ( newPos, answer ) =
            chompHex syntaxVersion src pos end
    in
    if answer == -4 then
        Err_ (newPos + 1) E.NumberNoConsecutiveUnderscores

    else if answer == -3 then
        Err_ newPos E.NumberNoLeadingOrTrailingUnderscores

    else if answer < 0 then
        Err_ newPos E.NumberHexDigit

    else
        OkInt newPos answer



-- CHOMP HEX


chompHex : SyntaxVersion -> String -> Int -> Int -> ( Int, Int )
chompHex syntaxVersion src pos end =
    chompHexHelp syntaxVersion src pos end -1 0


chompHexHelp : SyntaxVersion -> String -> Int -> Int -> Int -> Int -> ( Int, Int )
chompHexHelp syntaxVersion src pos end answer accumulator =
    if pos >= end then
        ( pos, answer )

    else
        let
            newAnswer : Int
            newAnswer =
                stepHex syntaxVersion src pos end (charAtPos pos src) accumulator
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
            chompHexHelp syntaxVersion src (pos + 1) end newAnswer newAnswer


stepHex : SyntaxVersion -> String -> Int -> Int -> Char -> Int -> Int
stepHex syntaxVersion src pos end word acc =
    if '0' <= word && word <= '9' then
        16 * acc + (Char.toCode word - Char.toCode '0')

    else if 'a' <= word && word <= 'f' then
        16 * acc + 10 + (Char.toCode word - Char.toCode 'a')

    else if 'A' <= word && word <= 'F' then
        16 * acc + 10 + (Char.toCode word - Char.toCode 'A')

    else if isGuida syntaxVersion && '_' == word then
        let
            nextWord : Char
            nextWord =
                charAtPos (pos + 1) src
        in
        if nextWord == '_' then
            -4

        else
            let
                validNextWord : Bool
                validNextWord =
                    ('0' <= nextWord && nextWord <= '9')
                        || ('a' <= nextWord && nextWord <= 'f')
                        || ('A' <= nextWord && nextWord <= 'F')
            in
            if pos + 1 == end || not validNextWord then
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
                    word : Char
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


isGuida : SyntaxVersion -> Bool
isGuida syntaxVersion =
    syntaxVersion == SyntaxVersion.Guida
