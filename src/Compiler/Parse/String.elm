module Compiler.Parse.String exposing
    ( character
    , string
    )

import Compiler.Elm.String as ES
import Compiler.Parse.Number as Number
import Compiler.Parse.Primitives as P exposing (Col, Parser(..), Row)
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
import Compiler.Reporting.Error.Syntax as E



-- CHARACTER


character : SyntaxVersion -> (Row -> Col -> x) -> (E.Char -> Row -> Col -> x) -> Parser x String
character syntaxVersion toExpectation toError =
    Parser
        (\(P.State src pos end indent row col) ->
            if pos >= end || P.unsafeIndex src pos /= '\'' then
                P.Eerr row col toExpectation

            else
                case chompChar syntaxVersion src (pos + 1) end row (col + 1) 0 placeholder of
                    Good newPos newCol numChars mostRecent ->
                        if numChars /= 1 then
                            P.Cerr row col (toError (E.CharNotString (newCol - col)))

                        else
                            let
                                newState : P.State
                                newState =
                                    P.State src newPos end indent row newCol

                                char : String
                                char =
                                    ES.fromChunks src [ mostRecent ]
                            in
                            P.Cok char newState

                    CharEndless newCol ->
                        P.Cerr row newCol (toError E.CharEndless)

                    CharEscape r c escape ->
                        P.Cerr r c (toError (E.CharEscape escape))
        )


type CharResult
    = Good Int Col Int ES.Chunk
    | CharEndless Col
    | CharEscape Row Col E.Escape


chompChar : SyntaxVersion -> String -> Int -> Int -> Row -> Col -> Int -> ES.Chunk -> CharResult
chompChar syntaxVersion src pos end row col numChars mostRecent =
    if pos >= end then
        CharEndless col

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if word == '\'' then
            Good (pos + 1) (col + 1) numChars mostRecent

        else if word == '\n' then
            CharEndless col

        else if word == '"' then
            chompChar syntaxVersion src (pos + 1) end row (col + 1) (numChars + 1) doubleQuote

        else if word == '\\' then
            case eatEscape syntaxVersion src (pos + 1) end row col of
                EscapeNormal ->
                    chompChar syntaxVersion src (pos + 2) end row (col + 2) (numChars + 1) (ES.Slice pos 2)

                EscapeUnicode delta code ->
                    chompChar syntaxVersion src (pos + delta) end row (col + delta) (numChars + 1) (ES.CodePoint code)

                EscapeProblem r c badEscape ->
                    CharEscape r c badEscape

                EscapeEndOfFile ->
                    CharEndless col

        else
            let
                width : Int
                width =
                    P.getCharWidth word

                newPos : Int
                newPos =
                    pos + width
            in
            chompChar syntaxVersion src newPos end row (col + 1) (numChars + 1) (ES.Slice pos width)



-- STRINGS


string : SyntaxVersion -> (Row -> Col -> x) -> (E.String_ -> Row -> Col -> x) -> Parser x ( String, Bool )
string syntaxVersion toExpectation toError =
    Parser
        (\(P.State src pos end indent row col) ->
            if isDoubleQuote src pos end then
                let
                    pos1 : Int
                    pos1 =
                        pos + 1
                in
                case
                    if isDoubleQuote src pos1 end then
                        let
                            pos2 : Int
                            pos2 =
                                pos + 2
                        in
                        if isDoubleQuote src pos2 end then
                            let
                                pos3 : Int
                                pos3 =
                                    pos + 3

                                col3 : Col
                                col3 =
                                    col + 3
                            in
                            multiString syntaxVersion src pos3 end row col3 pos3 row col []

                        else
                            SROk pos2 row (col + 2) "" False

                    else
                        singleString syntaxVersion src pos1 end row (col + 1) pos1 []
                of
                    SROk newPos newRow newCol utf8 multiline ->
                        let
                            newState : P.State
                            newState =
                                P.State src newPos end indent newRow newCol
                        in
                        P.Cok ( utf8, multiline ) newState

                    SRErr r c x ->
                        P.Cerr r c (toError x)

            else
                P.Eerr row col toExpectation
        )


isDoubleQuote : String -> Int -> Int -> Bool
isDoubleQuote src pos end =
    pos < end && P.unsafeIndex src pos == '"'


type StringResult
    = SROk Int Row Col String Bool
    | SRErr Row Col E.String_


finalize : String -> Int -> Int -> List ES.Chunk -> String
finalize src start end revChunks =
    ES.fromChunks src <|
        List.reverse <|
            if start == end then
                revChunks

            else
                -- String.fromList (List.map (P.unsafeIndex src) (List.range start (end - 1))) ++ revChunks
                ES.Slice start (end - start) :: revChunks


addEscape : ES.Chunk -> Int -> Int -> List ES.Chunk -> List ES.Chunk
addEscape chunk start end revChunks =
    if start == end then
        chunk :: revChunks

    else
        chunk :: ES.Slice start (end - start) :: revChunks



-- SINGLE STRINGS


singleString : SyntaxVersion -> String -> Int -> Int -> Row -> Col -> Int -> List ES.Chunk -> StringResult
singleString syntaxVersion src pos end row col initialPos revChunks =
    if pos >= end then
        SRErr row col E.StringEndless_Single

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if word == '"' then
            SROk (pos + 1)
                row
                (col + 1)
                (finalize src initialPos pos revChunks)
                False

        else if word == '\n' then
            SRErr row col E.StringEndless_Single

        else if word == '\'' then
            let
                newPos : Int
                newPos =
                    pos + 1
            in
            singleString syntaxVersion src newPos end row (col + 1) newPos <|
                addEscape singleQuote initialPos pos revChunks

        else if word == '\\' then
            case eatEscape syntaxVersion src (pos + 1) end row col of
                EscapeNormal ->
                    singleString syntaxVersion src (pos + 2) end row (col + 2) initialPos revChunks

                EscapeUnicode delta code ->
                    let
                        newPos : Int
                        newPos =
                            pos + delta
                    in
                    singleString syntaxVersion src newPos end row (col + delta) newPos <|
                        addEscape (ES.CodePoint code) initialPos pos revChunks

                EscapeProblem r c x ->
                    SRErr r c (E.StringEscape x)

                EscapeEndOfFile ->
                    SRErr row (col + 1) E.StringEndless_Single

        else
            let
                newPos : Int
                newPos =
                    pos + P.getCharWidth word
            in
            singleString syntaxVersion src newPos end row (col + 1) initialPos revChunks



-- MULTI STRINGS


multiString : SyntaxVersion -> String -> Int -> Int -> Row -> Col -> Int -> Row -> Col -> List ES.Chunk -> StringResult
multiString syntaxVersion src pos end row col initialPos sr sc revChunks =
    if pos >= end then
        SRErr sr sc E.StringEndless_Multi

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if word == '"' && isDoubleQuote src (pos + 1) end && isDoubleQuote src (pos + 2) end then
            SROk (pos + 3)
                row
                (col + 3)
                (finalize src initialPos pos revChunks)
                True

        else if word == '\'' then
            let
                pos1 : Int
                pos1 =
                    pos + 1
            in
            multiString syntaxVersion src pos1 end row (col + 1) pos1 sr sc <|
                addEscape singleQuote initialPos pos revChunks

        else if word == '\n' then
            let
                pos1 : Int
                pos1 =
                    pos + 1
            in
            multiString syntaxVersion src pos1 end (row + 1) 1 pos1 sr sc <|
                addEscape newline initialPos pos revChunks

        else if word == '\u{000D}' then
            let
                pos1 : Int
                pos1 =
                    pos + 1
            in
            multiString syntaxVersion src pos1 end row col pos1 sr sc <|
                addEscape carriageReturn initialPos pos revChunks

        else if word == '\\' then
            case eatEscape syntaxVersion src (pos + 1) end row col of
                EscapeNormal ->
                    multiString syntaxVersion src (pos + 2) end row (col + 2) initialPos sr sc revChunks

                EscapeUnicode delta code ->
                    let
                        newPos : Int
                        newPos =
                            pos + delta
                    in
                    multiString syntaxVersion src newPos end row (col + delta) newPos sr sc <|
                        addEscape (ES.CodePoint code) initialPos pos revChunks

                EscapeProblem r c x ->
                    SRErr r c (E.StringEscape x)

                EscapeEndOfFile ->
                    SRErr sr sc E.StringEndless_Multi

        else
            let
                newPos : Int
                newPos =
                    pos + P.getCharWidth word
            in
            multiString syntaxVersion src newPos end row (col + 1) initialPos sr sc revChunks



-- ESCAPE CHARACTERS


type Escape
    = EscapeNormal
    | EscapeUnicode Int Int
    | EscapeEndOfFile
    | EscapeProblem Row Col E.Escape


eatEscape : SyntaxVersion -> String -> Int -> Int -> Row -> Col -> Escape
eatEscape syntaxVersion src pos end row col =
    if pos >= end then
        EscapeEndOfFile

    else
        case P.unsafeIndex src pos of
            'n' ->
                EscapeNormal

            'r' ->
                EscapeNormal

            't' ->
                EscapeNormal

            '"' ->
                EscapeNormal

            '\'' ->
                EscapeNormal

            '\\' ->
                EscapeNormal

            'u' ->
                eatUnicode syntaxVersion src (pos + 1) end row col

            _ ->
                EscapeProblem row col E.EscapeUnknown


eatUnicode : SyntaxVersion -> String -> Int -> Int -> Row -> Col -> Escape
eatUnicode syntaxVersion src pos end row col =
    if pos >= end || P.unsafeIndex src pos /= '{' then
        EscapeProblem row col (E.BadUnicodeFormat 2)

    else
        let
            digitPos : Int
            digitPos =
                pos + 1

            ( newPos, code ) =
                Number.chompHex syntaxVersion src digitPos end

            numDigits : Int
            numDigits =
                newPos - digitPos
        in
        if newPos >= end || P.unsafeIndex src newPos /= '}' then
            EscapeProblem row col (E.BadUnicodeFormat (2 + numDigits))

        else if code < 0 || code > 0x0010FFFF then
            EscapeProblem row col (E.BadUnicodeCode (3 + numDigits))

        else if numDigits < 4 || numDigits > 6 then
            EscapeProblem row col (E.BadUnicodeLength (3 + numDigits) numDigits code)

        else
            EscapeUnicode (numDigits + 4) code


singleQuote : ES.Chunk
singleQuote =
    ES.Escape '\''


doubleQuote : ES.Chunk
doubleQuote =
    ES.Escape '"'


newline : ES.Chunk
newline =
    ES.Escape 'n'


carriageReturn : ES.Chunk
carriageReturn =
    ES.Escape 'r'


placeholder : ES.Chunk
placeholder =
    ES.CodePoint 0xFFFD
