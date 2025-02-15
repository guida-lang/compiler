module Compiler.Json.Decode exposing
    ( DecodeExpectation(..)
    , Decoder
    , Error(..)
    , KeyDecoder(..)
    , ParseError(..)
    , Problem(..)
    , StringProblem(..)
    , apply
    , assocListDict
    , bind
    , customString
    , dict
    , everySet
    , failure
    , field
    , fmap
    , fromByteString
    , int
    , jsonPair
    , list
    , mapError
    , nonEmptyList
    , nonempty
    , oneOf
    , oneOrMore
    , pair
    , pairs
    , pure
    , result
    , string
    )

import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Compiler.Json.String as Json
import Compiler.Parse.Keyword as K
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Utils.Crash exposing (crash)



-- CORE HELPERS


assocListDict : (k -> comparable) -> Decode.Decoder k -> Decode.Decoder v -> Decode.Decoder (Dict comparable k v)
assocListDict toComparable keyDecoder valueDecoder =
    Decode.list (jsonPair keyDecoder valueDecoder)
        |> Decode.map (Dict.fromList toComparable)


jsonPair : Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder ( a, b )
jsonPair firstDecoder secondDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "a" firstDecoder)
        (Decode.field "b" secondDecoder)


everySet : (a -> comparable) -> Decode.Decoder a -> Decode.Decoder (EverySet comparable a)
everySet toComparable decoder =
    Decode.list decoder
        |> Decode.map (EverySet.fromList toComparable)


nonempty : Decode.Decoder a -> Decode.Decoder (NE.Nonempty a)
nonempty decoder =
    Decode.list decoder
        |> Decode.andThen
            (\values ->
                case values of
                    x :: xs ->
                        Decode.succeed (NE.Nonempty x xs)

                    [] ->
                        Decode.fail "Empty list when it should have at least one element (non-empty list)!"
            )


oneOrMore : Decode.Decoder a -> Decode.Decoder (OneOrMore a)
oneOrMore decoder =
    Decode.oneOf
        [ Decode.map OneOrMore.one (Decode.field "one" decoder)
        , Decode.map2 OneOrMore.more
            (Decode.field "left" (Decode.lazy (\_ -> oneOrMore decoder)))
            (Decode.field "right" (Decode.lazy (\_ -> oneOrMore decoder)))
        ]


result : Decode.Decoder x -> Decode.Decoder a -> Decode.Decoder (Result x a)
result errDecoder successDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Err" ->
                        Decode.map Err (Decode.field "value" errDecoder)

                    "Ok" ->
                        Decode.map Ok (Decode.field "value" successDecoder)

                    _ ->
                        Decode.fail ("Failed to decode result's type: " ++ type_)
            )



-- RUNNERS


fromByteString : Decoder x a -> String -> Result (Error x) a
fromByteString (Decoder decode) src =
    case P.fromByteString pFile BadEnd src of
        Ok ast ->
            decode ast
                |> Result.mapError (DecodeProblem src)

        Err problem ->
            Err (ParseProblem src problem)



-- DECODERS


type Decoder x a
    = Decoder (AST -> Result (Problem x) a)



-- ERRORS


type Error x
    = DecodeProblem String (Problem x)
    | ParseProblem String ParseError



-- DECODE PROBLEMS


type Problem x
    = Field String (Problem x)
    | Index Int (Problem x)
    | OneOf (Problem x) (List (Problem x))
    | Failure A.Region x
    | Expecting A.Region DecodeExpectation


type DecodeExpectation
    = TObject
    | TArray
    | TString
    | TInt
    | TObjectWith String
    | TArrayPair Int



-- INSTANCES


fmap : (a -> b) -> Decoder x a -> Decoder x b
fmap func (Decoder decodeA) =
    Decoder (Result.map func << decodeA)


pure : a -> Decoder x a
pure a =
    Decoder (\_ -> Ok a)


apply : Decoder x a -> Decoder x (a -> b) -> Decoder x b
apply (Decoder decodeArg) (Decoder decodeFunc) =
    Decoder <|
        \ast ->
            Result.andThen
                (\a ->
                    Result.map (\b -> a |> b)
                        (decodeFunc ast)
                )
                (decodeArg ast)


bind : (a -> Decoder x b) -> Decoder x a -> Decoder x b
bind callback (Decoder decodeA) =
    Decoder <|
        \ast ->
            Result.andThen
                (\a ->
                    case callback a of
                        Decoder decodeB ->
                            decodeB ast
                )
                (decodeA ast)



-- STRINGS


string : Decoder x String
string =
    Decoder <|
        \(A.At region ast) ->
            case ast of
                String snippet ->
                    Ok (Json.fromSnippet snippet)

                _ ->
                    Err (Expecting region TString)


customString : P.Parser x a -> (Row -> Col -> x) -> Decoder x a
customString parser toBadEnd =
    Decoder <|
        \(A.At region ast) ->
            case ast of
                String snippet ->
                    P.fromSnippet parser toBadEnd snippet
                        |> Result.mapError (Failure region)

                _ ->
                    Err (Expecting region TString)



-- INT


int : Decoder x Int
int =
    Decoder <|
        \(A.At region ast) ->
            case ast of
                Int n ->
                    Ok n

                _ ->
                    Err (Expecting region TInt)



-- LISTS


list : Decoder x a -> Decoder x (List a)
list decoder =
    Decoder <|
        \(A.At region ast) ->
            case ast of
                Array asts ->
                    listHelp decoder 0 asts []

                _ ->
                    Err (Expecting region TArray)


listHelp : Decoder x a -> Int -> List AST -> List a -> Result (Problem x) (List a)
listHelp ((Decoder decodeA) as decoder) i asts revs =
    case asts of
        [] ->
            Ok (List.reverse revs)

        ast :: asts_ ->
            case decodeA ast of
                Ok value ->
                    listHelp decoder (i + 1) asts_ (value :: revs)

                Err prob ->
                    Err (Index i prob)



-- NON-EMPTY LISTS


nonEmptyList : Decoder x a -> x -> Decoder x (NE.Nonempty a)
nonEmptyList decoder x =
    Decoder <|
        \((A.At region _) as ast) ->
            let
                (Decoder values) =
                    list decoder
            in
            case values ast of
                Ok (v :: vs) ->
                    Ok (NE.Nonempty v vs)

                Ok [] ->
                    Err (Failure region x)

                Err err ->
                    Err err



-- PAIR


pair : Decoder x a -> Decoder x b -> Decoder x ( a, b )
pair (Decoder decodeA) (Decoder decodeB) =
    Decoder <|
        \(A.At region ast) ->
            case ast of
                Array vs ->
                    case vs of
                        [ astA, astB ] ->
                            Result.andThen
                                (\a ->
                                    Result.map (Tuple.pair a) (decodeB astB)
                                )
                                (decodeA astA)

                        _ ->
                            Err (Expecting region (TArrayPair (List.length vs)))

                _ ->
                    Err (Expecting region TArray)



-- OBJECTS


type KeyDecoder x a
    = KeyDecoder (P.Parser x a) (Row -> Col -> x)


dict : (k -> comparable) -> KeyDecoder x k -> Decoder x a -> Decoder x (Dict comparable k a)
dict toComparable keyDecoder valueDecoder =
    fmap (Dict.fromList toComparable) (pairs keyDecoder valueDecoder)


pairs : KeyDecoder x k -> Decoder x a -> Decoder x (List ( k, a ))
pairs keyDecoder valueDecoder =
    Decoder <|
        \(A.At region ast) ->
            case ast of
                Object kvs ->
                    pairsHelp keyDecoder valueDecoder kvs []

                _ ->
                    Err (Expecting region TObject)


pairsHelp : KeyDecoder x k -> Decoder x a -> List ( P.Snippet, AST ) -> List ( k, a ) -> Result (Problem x) (List ( k, a ))
pairsHelp ((KeyDecoder keyParser toBadEnd) as keyDecoder) ((Decoder decodeA) as valueDecoder) kvs revs =
    case kvs of
        [] ->
            Ok (List.reverse revs)

        ( snippet, ast ) :: kvs_ ->
            case P.fromSnippet keyParser toBadEnd snippet of
                Err x ->
                    Err (Failure (snippetToRegion snippet) x)

                Ok key ->
                    case decodeA ast of
                        Ok value ->
                            pairsHelp keyDecoder valueDecoder kvs_ (( key, value ) :: revs)

                        Err prob ->
                            let
                                (P.Snippet { fptr, offset, length }) =
                                    snippet
                            in
                            Err (Field (String.slice offset (offset + length) fptr) prob)


snippetToRegion : P.Snippet -> A.Region
snippetToRegion (P.Snippet { length, offRow, offCol }) =
    A.Region (A.Position offRow offCol) (A.Position offRow (offCol + length))



-- FIELDS


field : String -> Decoder x a -> Decoder x a
field key (Decoder decodeA) =
    Decoder <|
        \(A.At region ast) ->
            case ast of
                Object kvs ->
                    case findField key kvs of
                        Just value ->
                            Result.mapError (Field key)
                                (decodeA value)

                        Nothing ->
                            Err (Expecting region (TObjectWith key))

                _ ->
                    Err (Expecting region TObject)


findField : String -> List ( P.Snippet, AST ) -> Maybe AST
findField key pairs_ =
    case pairs_ of
        [] ->
            Nothing

        ( P.Snippet { fptr, offset, length }, value ) :: remainingPairs ->
            if key == String.slice offset (offset + length) fptr then
                Just value

            else
                findField key remainingPairs



-- ONE OF


oneOf : List (Decoder x a) -> Decoder x a
oneOf decoders =
    Decoder <|
        \ast ->
            case decoders of
                (Decoder decodeA) :: decoders_ ->
                    case decodeA ast of
                        Ok a ->
                            Ok a

                        Err e ->
                            oneOfHelp ast decoders_ [] e

                [] ->
                    crash "Ran into (Json.Decode.oneOf [])"


oneOfHelp : AST -> List (Decoder x a) -> List (Problem x) -> Problem x -> Result (Problem x) a
oneOfHelp ast decoders ps p =
    case decoders of
        (Decoder decodeA) :: decoders_ ->
            case decodeA ast of
                Ok a ->
                    Ok a

                Err p_ ->
                    oneOfHelp ast decoders_ (p :: ps) p_

        [] ->
            Err (oneOfError [] p ps)


oneOfError : List (Problem x) -> Problem x -> List (Problem x) -> Problem x
oneOfError problems prob ps =
    case ps of
        [] ->
            OneOf prob problems

        p :: ps_ ->
            oneOfError (prob :: problems) p ps_



-- FAILURE


failure : x -> Decoder x a
failure x =
    Decoder <|
        \(A.At region _) ->
            Err (Failure region x)



-- ERRORS


mapError : (x -> y) -> Decoder x a -> Decoder y a
mapError func (Decoder decodeA) =
    Decoder (Result.mapError (mapErrorHelp func) << decodeA)


mapErrorHelp : (x -> y) -> Problem x -> Problem y
mapErrorHelp func problem =
    case problem of
        Field k p ->
            Field k (mapErrorHelp func p)

        Index i p ->
            Index i (mapErrorHelp func p)

        OneOf p ps ->
            OneOf (mapErrorHelp func p) (List.map (mapErrorHelp func) ps)

        Failure r x ->
            Failure r (func x)

        Expecting r e ->
            Expecting r e



-- AST


type alias AST =
    A.Located AST_


type AST_
    = Array (List AST)
    | Object (List ( P.Snippet, AST ))
    | String P.Snippet
    | Int Int
    | TRUE
    | FALSE
    | NULL



-- PARSE


type alias Parser a =
    P.Parser ParseError a


type ParseError
    = Start Row Col
    | ObjectField Row Col
    | ObjectColon Row Col
    | ObjectEnd Row Col
    | ArrayEnd Row Col
    | StringProblem StringProblem Row Col
    | NoLeadingZeros Row Col
    | NoFloats Row Col
    | BadEnd Row Col


type StringProblem
    = BadStringEnd
    | BadStringControlChar
    | BadStringEscapeChar
    | BadStringEscapeHex



-- PARSE AST


pFile : Parser AST
pFile =
    spaces
        |> P.bind (\_ -> pValue)
        |> P.bind
            (\value ->
                P.fmap (\_ -> value) spaces
            )


pValue : Parser AST
pValue =
    P.addLocation <|
        P.oneOf Start
            [ P.fmap String (pString Start)
            , pObject
            , pArray
            , pInt
            , P.fmap (\_ -> TRUE) (K.k4 't' 'r' 'u' 'e' Start)
            , P.fmap (\_ -> FALSE) (K.k5 'f' 'a' 'l' 's' 'e' Start)
            , P.fmap (\_ -> NULL) (K.k4 'n' 'u' 'l' 'l' Start)
            ]



-- OBJECT


pObject : Parser AST_
pObject =
    P.word1 '{' Start
        |> P.bind (\_ -> spaces)
        |> P.bind
            (\_ ->
                P.oneOf ObjectField
                    [ pField
                        |> P.bind
                            (\entry ->
                                spaces
                                    |> P.bind (\_ -> P.loop pObjectHelp [ entry ])
                            )
                    , P.word1 '}' ObjectEnd
                        |> P.fmap (\_ -> Object [])
                    ]
            )


pObjectHelp : List ( P.Snippet, AST ) -> Parser (P.Step (List ( P.Snippet, AST )) AST_)
pObjectHelp revEntries =
    P.oneOf ObjectEnd
        [ P.word1 ',' ObjectEnd
            |> P.bind (\_ -> spaces)
            |> P.bind (\_ -> pField)
            |> P.bind
                (\entry ->
                    spaces
                        |> P.fmap (\_ -> P.Loop (entry :: revEntries))
                )
        , P.word1 '}' ObjectEnd
            |> P.fmap (\_ -> P.Done (Object (List.reverse revEntries)))
        ]


pField : Parser ( P.Snippet, AST )
pField =
    pString ObjectField
        |> P.bind
            (\key ->
                spaces
                    |> P.bind (\_ -> P.word1 ':' ObjectColon)
                    |> P.bind (\_ -> spaces)
                    |> P.bind (\_ -> pValue)
                    |> P.fmap (\value -> ( key, value ))
            )



-- ARRAY


pArray : Parser AST_
pArray =
    P.word1 '[' Start
        |> P.bind (\_ -> spaces)
        |> P.bind
            (\_ ->
                P.oneOf Start
                    [ pValue
                        |> P.bind
                            (\entry ->
                                spaces
                                    |> P.bind (\_ -> pArrayHelp [ entry ])
                            )
                    , P.word1 ']' ArrayEnd
                        |> P.fmap (\_ -> Array [])
                    ]
            )


pArrayHelp : List AST -> Parser AST_
pArrayHelp revEntries =
    P.oneOf ArrayEnd
        [ P.word1 ',' ArrayEnd
            |> P.bind (\_ -> spaces)
            |> P.bind (\_ -> pValue)
            |> P.bind
                (\entry ->
                    spaces
                        |> P.bind (\_ -> pArrayHelp (entry :: revEntries))
                )
        , P.word1 ']' ArrayEnd
            |> P.fmap (\_ -> Array (List.reverse revEntries))
        ]



-- STRING


pString : (Row -> Col -> ParseError) -> Parser P.Snippet
pString start =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos < end && P.unsafeIndex src pos == '"' then
                let
                    pos1 : Int
                    pos1 =
                        pos + 1

                    col1 : Col
                    col1 =
                        col + 1

                    ( ( status, newPos ), ( newRow, newCol ) ) =
                        pStringHelp src pos1 end row col1
                in
                case status of
                    GoodString ->
                        let
                            off : Int
                            off =
                                -- FIXME pos1 - unsafeForeignPtrToPtr src
                                pos1

                            len : Int
                            len =
                                (newPos - pos1) - 1

                            snp : P.Snippet
                            snp =
                                P.Snippet
                                    { fptr = src
                                    , offset = off
                                    , length = len
                                    , offRow = row
                                    , offCol = col1
                                    }

                            newState : P.State
                            newState =
                                P.State src newPos end indent newRow newCol
                        in
                        P.Cok snp newState

                    BadString problem ->
                        P.Cerr newRow newCol (StringProblem problem)

            else
                P.Eerr row col start


type StringStatus
    = GoodString
    | BadString StringProblem


pStringHelp : String -> Int -> Int -> Row -> Col -> ( ( StringStatus, Int ), ( Row, Col ) )
pStringHelp src pos end row col =
    if pos >= end then
        ( ( BadString BadStringEnd, pos ), ( row, col ) )

    else
        case P.unsafeIndex src pos of
            '"' ->
                ( ( GoodString, pos + 1 ), ( row, col + 1 ) )

            '\n' ->
                ( ( BadString BadStringEnd, pos ), ( row, col ) )

            '\\' ->
                let
                    pos1 : Int
                    pos1 =
                        pos + 1
                in
                if pos1 >= end then
                    ( ( BadString BadStringEnd, pos1 ), ( row + 1, col ) )

                else
                    case P.unsafeIndex src pos1 of
                        '"' ->
                            pStringHelp src (pos + 2) end row (col + 2)

                        '\\' ->
                            pStringHelp src (pos + 2) end row (col + 2)

                        '/' ->
                            pStringHelp src (pos + 2) end row (col + 2)

                        'b' ->
                            pStringHelp src (pos + 2) end row (col + 2)

                        {- f -}
                        'f' ->
                            pStringHelp src (pos + 2) end row (col + 2)

                        {- n -}
                        'n' ->
                            pStringHelp src (pos + 2) end row (col + 2)

                        {- r -}
                        'r' ->
                            pStringHelp src (pos + 2) end row (col + 2)

                        {- t -}
                        't' ->
                            pStringHelp src (pos + 2) end row (col + 2)

                        {- u -}
                        'u' ->
                            let
                                pos6 : Int
                                pos6 =
                                    pos + 6
                            in
                            if
                                (pos6 <= end)
                                    && isHex (P.unsafeIndex src (pos + 2))
                                    && isHex (P.unsafeIndex src (pos + 3))
                                    && isHex (P.unsafeIndex src (pos + 4))
                                    && isHex (P.unsafeIndex src (pos + 5))
                            then
                                pStringHelp src pos6 end row (col + 6)

                            else
                                ( ( BadString BadStringEscapeHex, pos ), ( row, col ) )

                        _ ->
                            ( ( BadString BadStringEscapeChar, pos ), ( row, col ) )

            word ->
                if Char.toCode word < 0x20 then
                    ( ( BadString BadStringControlChar, pos ), ( row, col ) )

                else
                    let
                        newPos : Int
                        newPos =
                            pos + P.getCharWidth word
                    in
                    pStringHelp src newPos end row (col + 1)


isHex : Char -> Bool
isHex word =
    let
        code : Int
        code =
            Char.toCode word
    in
    (0x30 {- 0 -} <= code)
        && (code <= 0x39 {- 9 -})
        || (0x61 {- a -} <= code)
        && (code <= 0x66 {- f -})
        || (0x41 {- A -} <= code)
        && (code <= 0x46 {- F -})



-- SPACES


spaces : Parser ()
spaces =
    P.Parser <|
        \((P.State src pos end indent row col) as state) ->
            let
                ( newPos, newRow, newCol ) =
                    eatSpaces src pos end row col
            in
            if pos == newPos then
                P.Eok () state

            else
                let
                    newState : P.State
                    newState =
                        P.State src newPos end indent newRow newCol
                in
                P.Cok () newState


eatSpaces : String -> Int -> Int -> Row -> Col -> ( Int, Row, Col )
eatSpaces src pos end row col =
    if pos >= end then
        ( pos, row, col )

    else
        case P.unsafeIndex src pos of
            ' ' ->
                eatSpaces src (pos + 1) end row (col + 1)

            '\t' ->
                eatSpaces src (pos + 1) end row (col + 1)

            '\n' ->
                eatSpaces src (pos + 1) end (row + 1) 1

            {- \r -}
            '\u{000D}' ->
                eatSpaces src (pos + 1) end row col

            _ ->
                ( pos, row, col )



-- INTS


pInt : Parser AST_
pInt =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                P.Eerr row col Start

            else
                let
                    word : Char
                    word =
                        P.unsafeIndex src pos
                in
                if not (isDecimalDigit word) then
                    P.Eerr row col Start

                else if word == '0' then
                    let
                        pos1 : Int
                        pos1 =
                            pos + 1

                        newState : P.State
                        newState =
                            P.State src pos1 end indent row (col + 1)
                    in
                    if pos1 < end then
                        let
                            word1 : Char
                            word1 =
                                P.unsafeIndex src pos1
                        in
                        if isDecimalDigit word1 then
                            P.Cerr row (col + 1) NoLeadingZeros

                        else if word1 == '.' then
                            P.Cerr row (col + 1) NoFloats

                        else
                            P.Cok (Int 0) newState

                    else
                        P.Cok (Int 0) newState

                else
                    let
                        ( status, n, newPos ) =
                            chompInt src (pos + 1) end (Char.toCode word - 0x30 {- 0 -})

                        len : Int
                        len =
                            newPos - pos
                    in
                    case status of
                        GoodInt ->
                            let
                                newState : P.State
                                newState =
                                    P.State src newPos end indent row (col + len)
                            in
                            P.Cok (Int n) newState

                        BadIntEnd ->
                            P.Cerr row (col + len) NoFloats


type IntStatus
    = GoodInt
    | BadIntEnd


chompInt : String -> Int -> Int -> Int -> ( IntStatus, Int, Int )
chompInt src pos end n =
    if pos < end then
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if isDecimalDigit word then
            let
                m : Int
                m =
                    10 * n + (Char.toCode word - 0x30 {- 0 -})
            in
            chompInt src (pos + 1) end m

        else if word == '.' || word == 'e' || word == 'E' then
            ( BadIntEnd, n, pos )

        else
            ( GoodInt, n, pos )

    else
        ( GoodInt, n, pos )


isDecimalDigit : Char -> Bool
isDecimalDigit word =
    let
        code : Int
        code =
            Char.toCode word
    in
    code <= 0x39 {- 9 -} && code >= {- 0 -} 0x30
