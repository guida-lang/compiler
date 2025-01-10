module Compiler.Parse.Primitives exposing
    ( Col
    , PErr(..)
    , POk(..)
    , Parser(..)
    , Row
    , Snippet(..)
    , State(..)
    , Status(..)
    , Step(..)
    , addEnd
    , addLocation
    , bind
    , fmap
    , fromByteString
    , fromSnippet
    , getCharWidth
    , getPosition
    , inContext
    , isWord
    , loop
    , oneOf
    , oneOfWithFallback
    , pure
    , snippetDecoder
    , snippetEncoder
    , specialize
    , unsafeIndex
    , withBacksetIndent
    , withIndent
    , word1
    , word2
    )

import Compiler.Reporting.Annotation as A
import Json.Decode as Decode
import Json.Encode as Encode
import Utils.Crash exposing (crash)



-- PARSER


type Parser x a
    = Parser (State -> Result (PErr x) (POk a))


type POk a
    = POk Status a State


type PErr x
    = PErr Status Row Col (Row -> Col -> x)


type Status
    = Consumed
    | Empty


type State
    = -- PERF try taking some out to avoid allocation
      State String Int Int Int Row Col


type alias Row =
    Int


type alias Col =
    Int



-- FUNCTOR


fmap : (a -> b) -> Parser x a -> Parser x b
fmap f (Parser parser) =
    Parser
        (\state ->
            Result.map
                (\(POk status a s) ->
                    POk status (f a) s
                )
                (parser state)
        )



-- ONE OF


oneOf : (Row -> Col -> x) -> List (Parser x a) -> Parser x a
oneOf toError parsers =
    Parser
        (\state ->
            oneOfHelp state toError parsers
        )


oneOfHelp : State -> (Row -> Col -> x) -> List (Parser x a) -> Result (PErr x) (POk a)
oneOfHelp state toError parsers =
    case parsers of
        (Parser parser) :: remainingParsers ->
            case parser state of
                Err (PErr Empty _ _ _) ->
                    oneOfHelp state toError remainingParsers

                result ->
                    result

        [] ->
            let
                (State _ _ _ _ row col) =
                    state
            in
            Err (PErr Empty row col toError)



-- ONE OF WITH FALLBACK


oneOfWithFallback : List (Parser x a) -> a -> Parser x a
oneOfWithFallback parsers fallback =
    Parser (\state -> oowfHelp state parsers fallback)


oowfHelp : State -> List (Parser x a) -> a -> Result (PErr x) (POk a)
oowfHelp state parsers fallback =
    case parsers of
        [] ->
            Ok (POk Empty fallback state)

        (Parser parser) :: remainingParsers ->
            case parser state of
                Err (PErr Empty _ _ _) ->
                    oowfHelp state remainingParsers fallback

                result ->
                    result



-- MONAD


pure : a -> Parser x a
pure value =
    Parser (\state -> Ok (POk Empty value state))


bind : (a -> Parser x b) -> Parser x a -> Parser x b
bind callback (Parser parserA) =
    Parser
        (\state ->
            Result.andThen
                (\(POk _ a s) ->
                    case callback a of
                        Parser parserB ->
                            parserB s
                )
                (parserA state)
        )



-- FROM BYTESTRING


fromByteString : Parser x a -> (Row -> Col -> x) -> String -> Result x a
fromByteString (Parser parser) toBadEnd src =
    let
        initialState : State
        initialState =
            State src 0 (String.length src) 0 1 1
    in
    case parser initialState of
        Ok (POk _ a state) ->
            toOk toBadEnd a state

        Err (PErr _ row col toError) ->
            toErr row col toError


toOk : (Row -> Col -> x) -> a -> State -> Result x a
toOk toBadEnd a (State _ pos end _ row col) =
    if pos == end then
        Ok a

    else
        Err (toBadEnd row col)


toErr : Row -> Col -> (Row -> Col -> x) -> Result x a
toErr row col toError =
    Err (toError row col)



-- FROM SNIPPET


type Snippet
    = Snippet
        { fptr : String
        , offset : Int
        , length : Int
        , offRow : Row
        , offCol : Col
        }


fromSnippet : Parser x a -> (Row -> Col -> x) -> Snippet -> Result x a
fromSnippet (Parser parser) toBadEnd (Snippet { fptr, offset, length, offRow, offCol }) =
    let
        initialState : State
        initialState =
            State fptr offset (offset + length) 0 offRow offCol
    in
    case parser initialState of
        Ok (POk _ a state) ->
            toOk toBadEnd a state

        Err (PErr _ row col toError) ->
            toErr row col toError



-- POSITION


getPosition : Parser x A.Position
getPosition =
    Parser
        (\((State _ _ _ _ row col) as state) ->
            Ok (POk Empty (A.Position row col) state)
        )


addLocation : Parser x a -> Parser x (A.Located a)
addLocation (Parser parser) =
    Parser
        (\((State _ _ _ _ sr sc) as state) ->
            case parser state of
                Ok (POk status a ((State _ _ _ _ er ec) as s)) ->
                    Ok (POk status (A.At (A.Region (A.Position sr sc) (A.Position er ec)) a) s)

                Err err ->
                    Err err
        )


addEnd : A.Position -> a -> Parser x (A.Located a)
addEnd start value =
    Parser
        (\((State _ _ _ _ row col) as state) ->
            Ok (POk Empty (A.at start (A.Position row col) value) state)
        )



-- INDENT


withIndent : Parser x a -> Parser x a
withIndent (Parser parser) =
    Parser
        (\(State src pos end oldIndent row col) ->
            case parser (State src pos end col row col) of
                Ok (POk status a (State s p e _ r c)) ->
                    Ok (POk status a (State s p e oldIndent r c))

                Err err ->
                    Err err
        )


withBacksetIndent : Int -> Parser x a -> Parser x a
withBacksetIndent backset (Parser parser) =
    Parser
        (\(State src pos end oldIndent row col) ->
            case parser (State src pos end (col - backset) row col) of
                Ok (POk status a (State s p e _ r c)) ->
                    Ok (POk status a (State s p e oldIndent r c))

                Err err ->
                    Err err
        )



-- CONTEXT


inContext : (x -> Row -> Col -> y) -> Parser y start -> Parser x a -> Parser y a
inContext addContext (Parser parserStart) (Parser parserA) =
    Parser
        (\state ->
            case parserStart state of
                Ok (POk _ _ okState) ->
                    case parserA okState of
                        Ok res ->
                            Ok res

                        Err (PErr status r c tx) ->
                            Err (PErr status r c (addContext (tx r c)))

                Err err ->
                    Err err
        )


specialize : (x -> Row -> Col -> y) -> Parser x a -> Parser y a
specialize addContext (Parser parser) =
    Parser
        (\state ->
            case parser state of
                Ok res ->
                    Ok res

                Err (PErr status r c tx) ->
                    Err (PErr status r c (addContext (tx r c)))
        )



-- SYMBOLS


word1 : Char -> (Row -> Col -> x) -> Parser x ()
word1 word toError =
    Parser
        (\(State src pos end indent row col) ->
            if pos < end && unsafeIndex src pos == word then
                let
                    newState : State
                    newState =
                        State src (pos + 1) end indent row (col + 1)
                in
                Ok (POk Consumed () newState)

            else
                Err (PErr Empty row col toError)
        )


word2 : Char -> Char -> (Row -> Col -> x) -> Parser x ()
word2 w1 w2 toError =
    Parser
        (\(State src pos end indent row col) ->
            let
                pos1 : Int
                pos1 =
                    pos + 1
            in
            if pos < end && unsafeIndex src pos == w1 && unsafeIndex src pos1 == w2 then
                let
                    newState : State
                    newState =
                        State src (pos + 2) end indent row (col + 2)
                in
                Ok (POk Consumed () newState)

            else
                Err (PErr Empty row col toError)
        )



-- LOW-LEVEL CHECKS


unsafeIndex : String -> Int -> Char
unsafeIndex str index =
    case String.uncons (String.dropLeft index str) of
        Just ( char, _ ) ->
            char

        Nothing ->
            crash "Error on unsafeIndex!"


isWord : String -> Int -> Int -> Char -> Bool
isWord src pos end word =
    pos < end && unsafeIndex src pos == word


getCharWidth : Char -> Int
getCharWidth word =
    if Char.toCode word > 0xFFFF then
        2

    else
        1



-- ENCODERS and DECODERS


snippetEncoder : Snippet -> Encode.Value
snippetEncoder (Snippet { fptr, offset, length, offRow, offCol }) =
    Encode.object
        [ ( "type", Encode.string "Snippet" )
        , ( "fptr", Encode.string fptr )
        , ( "offset", Encode.int offset )
        , ( "length", Encode.int length )
        , ( "offRow", Encode.int offRow )
        , ( "offCol", Encode.int offCol )
        ]


snippetDecoder : Decode.Decoder Snippet
snippetDecoder =
    Decode.map5
        (\fptr offset length offRow offCol ->
            Snippet
                { fptr = fptr
                , offset = offset
                , length = length
                , offRow = offRow
                , offCol = offCol
                }
        )
        (Decode.field "fptr" Decode.string)
        (Decode.field "offset" Decode.int)
        (Decode.field "length" Decode.int)
        (Decode.field "offRow" Decode.int)
        (Decode.field "offCol" Decode.int)



-- LOOP


type Step state a
    = Loop state
    | Done a


loop : (state -> Parser x (Step state a)) -> state -> Parser x a
loop callback loopState =
    Parser
        (\state ->
            loopHelp callback state loopState (\a s -> Ok (POk Empty a s)) (\row col toError -> Err (PErr Empty row col toError))
        )


loopHelp :
    (state -> Parser x (Step state a))
    -> State
    -> state
    -> (a -> State -> Result (PErr x) (POk a))
    -> (Row -> Col -> (Row -> Col -> x) -> Result (PErr x) (POk a))
    -> Result (PErr x) (POk a)
loopHelp callback state loopState eok eerr =
    case callback loopState of
        Parser parser ->
            case parser state of
                Ok (POk Consumed (Loop newLoopState) newState) ->
                    loopHelp callback
                        newState
                        newLoopState
                        (\a s -> Ok (POk Consumed a s))
                        (\row col toError -> Err (PErr Consumed row col toError))

                Ok (POk Consumed (Done a) newState) ->
                    Ok (POk Consumed a newState)

                Ok (POk Empty (Loop newLoopState) newState) ->
                    loopHelp callback newState newLoopState eok eerr

                Ok (POk Empty (Done a) newState) ->
                    eok a newState

                Err (PErr Consumed r c t) ->
                    Err (PErr Consumed r c t)

                Err (PErr Empty r c t) ->
                    eerr r c t
