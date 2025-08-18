module Common.Format.Cheapskate.ParserCombinators exposing
    ( ParseError(..)
    , Parser(..)
    , ParserState(..)
    , Position(..)
    , advance
    , anyChar
    , apply
    , bind
    , char
    , charClass
    , column
    , comparePositions
    , count
    , empty
    , endOfInput
    , fail
    , failure
    , fmap
    , getPosition
    , guard
    , inClass
    , lazy
    , leftSequence
    , liftA2
    , lookAhead
    , many
    , many1
    , manyTill
    , mzero
    , notAfter
    , notFollowedBy
    , notInClass
    , oneOf
    , option
    , parse
    , peekChar
    , peekLastChar
    , pure
    , return
    , satisfy
    , scan
    , sequence
    , setPosition
    , showParseError
    , showPosition
    , skip
    , skipMany
    , skipMany1
    , skipP
    , skipWhile
    , string
    , stringTakeWhile
    , success
    , takeText
    , takeTill
    , takeWhile
    , takeWhile1
    , unless
    )

import Set exposing (Set)


type Position
    = Position Int Int


showPosition : Position -> String
showPosition (Position ln cn) =
    "line " ++ String.fromInt ln ++ " column " ++ String.fromInt cn


comparePositions : Position -> Position -> Basics.Order
comparePositions (Position ln1 cn1) (Position ln2 cn2) =
    if ln1 > ln2 then
        GT

    else if ln1 == ln2 then
        compare cn1 cn2

    else
        LT



-- the String indicates what the parser was expecting


type ParseError
    = ParseError Position String


showParseError : ParseError -> String
showParseError (ParseError (Position ln cn) msg) =
    "ParseError (line " ++ String.fromInt ln ++ " column " ++ String.fromInt cn ++ ") " ++ msg


type ParserState
    = ParserState
        { subject : String
        , position : Position
        , lastChar : Maybe Char
        }


advance : ParserState -> String -> ParserState
advance parserState str =
    let
        go : Char -> ParserState -> ParserState
        go c (ParserState st) =
            let
                (Position line _) =
                    st.position
            in
            ParserState
                { subject = String.dropLeft 1 st.subject
                , position =
                    case c of
                        '\n' ->
                            Position (line + 1) 1

                        _ ->
                            Position line (column st.position + 1)
                , lastChar = Just c
                }
    in
    List.foldl go parserState (String.toList str)


type Parser a
    = Parser (ParserState -> Result ParseError ( ParserState, a ))



-- instance Functor Parser where


fmap : (a -> b) -> Parser a -> Parser b
fmap f (Parser g) =
    Parser
        (\st ->
            case g st of
                Ok ( st_, x ) ->
                    Ok ( st_, f x )

                Err e ->
                    Err e
        )



-- instance Applicative Parser where


pure : a -> Parser a
pure x =
    Parser (\st -> Ok ( st, x ))


apply : Parser a -> Parser (a -> b) -> Parser b
apply (Parser g) (Parser f) =
    Parser
        (\st ->
            case f st of
                Err e ->
                    Err e

                Ok ( st_, h ) ->
                    case g st_ of
                        Ok ( st__, x ) ->
                            Ok ( st__, h x )

                        Err e ->
                            Err e
        )


unless : Bool -> Parser () -> Parser ()
unless p s =
    if p then
        pure ()

    else
        s


{-| (<\*)
-}
leftSequence : Parser a -> Parser b -> Parser a
leftSequence p1 p2 =
    p1 |> bind (\res -> p2 |> fmap (\_ -> res))



-- instance Alternative Parser where


empty : Parser a
empty =
    Parser (\(ParserState st) -> Err (ParseError st.position "(empty)"))


guard : Bool -> Parser ()
guard bool =
    if bool then
        pure ()

    else
        empty


oneOf : Parser a -> Parser a -> Parser a
oneOf (Parser f) (Parser g) =
    Parser
        (\st ->
            case f st of
                Ok res ->
                    Ok res

                Err (ParseError pos msg) ->
                    case g st of
                        Ok res ->
                            Ok res

                        Err (ParseError pos_ msg_) ->
                            Err
                                -- return error for farthest match
                                (case comparePositions pos pos_ of
                                    LT ->
                                        ParseError pos_ msg_

                                    GT ->
                                        ParseError pos msg

                                    EQ ->
                                        ParseError pos (msg ++ " or " ++ msg_)
                                )
        )



-- instance Monad Parser where


return : a -> Parser a
return x =
    Parser (\st -> Ok ( st, x ))


bind : (a -> Parser b) -> Parser a -> Parser b
bind g (Parser p) =
    Parser
        (\st ->
            case p st of
                Err e ->
                    Err e

                Ok ( st_, x ) ->
                    let
                        (Parser evalParser) =
                            g x
                    in
                    evalParser st_
        )



-- instance MonadFail Parser where


fail : String -> Parser a
fail e =
    Parser (\(ParserState st) -> Err (ParseError st.position e))



-- instance MonadPlus Parser where


mzero : Parser a
mzero =
    Parser (\(ParserState st) -> Err (ParseError st.position "(mzero)"))


parse : Parser a -> String -> Result ParseError a
parse (Parser evalParser) t =
    Result.map Tuple.second
        (evalParser
            (ParserState
                { subject = t
                , position = Position 1 1
                , lastChar = Nothing
                }
            )
        )


failure : ParserState -> String -> Result ParseError ( ParserState, a )
failure (ParserState st) msg =
    Err (ParseError st.position msg)


success : ParserState -> a -> Result ParseError ( ParserState, a )
success st x =
    Ok ( st, x )


satisfy : (Char -> Bool) -> Parser Char
satisfy f =
    let
        g : ParserState -> Result ParseError ( ParserState, Char )
        g (ParserState st) =
            case String.uncons st.subject of
                Just ( c, _ ) ->
                    if f c then
                        success (advance (ParserState st) (String.fromChar c)) c

                    else
                        failure (ParserState st) "character meeting condition"

                _ ->
                    failure (ParserState st) "character meeting condition"
    in
    Parser g


peekChar : Parser (Maybe Char)
peekChar =
    Parser
        (\(ParserState st) ->
            case String.uncons st.subject of
                Just ( c, _ ) ->
                    success (ParserState st) (Just c)

                Nothing ->
                    success (ParserState st) Nothing
        )


peekLastChar : Parser (Maybe Char)
peekLastChar =
    Parser (\(ParserState st) -> success (ParserState st) st.lastChar)


notAfter : (Char -> Bool) -> Parser ()
notAfter f =
    peekLastChar
        |> bind
            (\mbc ->
                case mbc of
                    Nothing ->
                        return ()

                    Just c ->
                        if f c then
                            mzero

                        else
                            return ()
            )



-- low-grade version of attoparsec's:


charClass : String -> Set Char
charClass =
    let
        go : List Char -> List Char
        go str =
            case str of
                a :: '-' :: b :: xs ->
                    List.map Char.fromCode (List.range (Char.toCode a) (Char.toCode b)) ++ go xs

                x :: xs ->
                    x :: go xs

                _ ->
                    []
    in
    Set.fromList << go << String.toList


inClass : String -> Char -> Bool
inClass s c =
    let
        s_ : Set Char
        s_ =
            charClass s
    in
    Set.member c s_


notInClass : String -> Char -> Bool
notInClass s =
    not << inClass s


endOfInput : Parser ()
endOfInput =
    Parser
        (\(ParserState st) ->
            if String.isEmpty st.subject then
                success (ParserState st) ()

            else
                failure (ParserState st) "end of input"
        )


char : Char -> Parser Char
char c =
    satisfy ((==) c)


anyChar : Parser Char
anyChar =
    satisfy (\_ -> True)


getPosition : Parser Position
getPosition =
    Parser (\(ParserState st) -> success (ParserState st) st.position)


column : Position -> Int
column (Position _ cn) =
    cn



-- note: this does not actually change the position in the subject;
-- it only changes what column counts as column N.  It is intended
-- to be used in cases where we're parsing a partial line but need to
-- have accurate column information.


setPosition : Position -> Parser ()
setPosition pos =
    Parser (\(ParserState st) -> success (ParserState { st | position = pos }) ())


takeWhile : (Char -> Bool) -> Parser String
takeWhile f =
    Parser
        (\(ParserState st) ->
            let
                t : String
                t =
                    stringTakeWhile f st.subject
            in
            success (advance (ParserState st) t) t
        )


takeTill : (Char -> Bool) -> Parser String
takeTill f =
    takeWhile (not << f)


takeWhile1 : (Char -> Bool) -> Parser String
takeWhile1 f =
    Parser
        (\(ParserState st) ->
            let
                t : String
                t =
                    stringTakeWhile f st.subject
            in
            if String.isEmpty t then
                failure (ParserState st) "characters satisfying condition"

            else
                success (advance (ParserState st) t) t
        )


takeText : Parser String
takeText =
    Parser
        (\(ParserState st) ->
            let
                t : String
                t =
                    st.subject
            in
            success (advance (ParserState st) t) t
        )


skip : (Char -> Bool) -> Parser ()
skip f =
    Parser
        (\(ParserState st) ->
            case String.uncons st.subject of
                Just ( c, _ ) ->
                    if f c then
                        success (advance (ParserState st) (String.fromChar c)) ()

                    else
                        failure (ParserState st) "character satisfying condition"

                _ ->
                    failure (ParserState st) "character satisfying condition"
        )


skipWhile : (Char -> Bool) -> Parser ()
skipWhile f =
    Parser
        (\(ParserState st) ->
            let
                t_ : String
                t_ =
                    stringTakeWhile f st.subject
            in
            success (advance (ParserState st) t_) ()
        )


string : String -> Parser String
string s =
    Parser
        (\(ParserState st) ->
            if String.startsWith s st.subject then
                success (advance (ParserState st) s) s

            else
                failure (ParserState st) "string"
        )


scan : s -> (s -> Char -> Maybe s) -> Parser String
scan s0 f =
    let
        go : s -> String -> ParserState -> Result ParseError ( ParserState, String )
        go s cs (ParserState st) =
            case String.uncons st.subject of
                Nothing ->
                    finish (ParserState st) cs

                Just ( c, _ ) ->
                    case f s c of
                        Just s_ ->
                            go s_
                                (String.cons c cs)
                                (advance (ParserState st) (String.fromChar c))

                        Nothing ->
                            finish (ParserState st) cs

        finish : ParserState -> String -> Result ParseError ( ParserState, String )
        finish st cs =
            success st (String.reverse cs)
    in
    Parser (go s0 "")


lookAhead : Parser a -> Parser a
lookAhead (Parser p) =
    Parser
        (\st ->
            case p st of
                Ok ( _, x ) ->
                    success st x

                Err _ ->
                    failure st "lookAhead"
        )


notFollowedBy : Parser a -> Parser ()
notFollowedBy (Parser p) =
    Parser
        (\st ->
            case p st of
                Ok _ ->
                    failure st "notFollowedBy"

                Err _ ->
                    success st ()
        )



-- combinators (definitions borrowed from attoparsec)


option : a -> Parser a -> Parser a
option x p =
    oneOf p (pure x)


many1 : Parser a -> Parser (List a)
many1 p =
    liftA2 (::) p (many p)


manyTill : Parser a -> Parser b -> Parser (List a)
manyTill p end =
    let
        go : () -> Parser (List a)
        go () =
            oneOf (end |> bind (\_ -> pure [])) (liftA2 (::) p (lazy go))
    in
    go ()


skipMany : Parser a -> Parser ()
skipMany p =
    many (skipP p) |> fmap (\_ -> ())


skipP : Parser a -> Parser ()
skipP p =
    p |> fmap (\_ -> ())


skipMany1 : Parser a -> Parser ()
skipMany1 p =
    p |> bind (\_ -> skipMany p)


count : Int -> Parser a -> Parser (List a)
count n p =
    sequence (List.repeat n p)



-- ...


lazy : (() -> Parser a) -> Parser a
lazy f =
    bind f (pure ())


many : Parser a -> Parser (List a)
many (Parser p) =
    let
        accumulate : List a -> ParserState -> Result ParseError ( ParserState, List a )
        accumulate acc state =
            case p state of
                Ok ( st_, res ) ->
                    accumulate (res :: acc) st_

                Err _ ->
                    Ok ( state, List.reverse acc )
    in
    Parser (accumulate [])


liftA2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
liftA2 f pa pb =
    pa
        |> fmap f
        |> bind (\fApplied -> fmap fApplied pb)


sequence : List (Parser a) -> Parser (List a)
sequence parsers =
    case parsers of
        [] ->
            pure []

        p :: ps ->
            liftA2 (::) p (sequence ps)


stringTakeWhile : (Char -> Bool) -> String -> String
stringTakeWhile f str =
    String.toList str
        |> List.foldl
            (\c ( found, acc ) ->
                if found && f c then
                    ( True, String.cons c acc )

                else
                    ( False, acc )
            )
            ( True, "" )
        |> Tuple.second
        |> String.reverse
