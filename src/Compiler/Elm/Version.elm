module Compiler.Elm.Version exposing
    ( Version(..)
    , bumpMajor
    , bumpMinor
    , bumpPatch
    , compare
    , compiler
    , decoder
    , encode
    , jsonCodec
    , major
    , max
    , maxVersion
    , min
    , one
    , parser
    , toChars
    , versionCodec
    )

import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Serialize exposing (Codec)



-- VERSION


type Version
    = Version Int Int Int


major : Version -> Int
major (Version major_ _ _) =
    major_


compare : Version -> Version -> Order
compare (Version major1 minor1 patch1) (Version major2 minor2 patch2) =
    case Basics.compare major1 major2 of
        EQ ->
            case Basics.compare minor1 minor2 of
                EQ ->
                    Basics.compare patch1 patch2

                minorRes ->
                    minorRes

        majorRes ->
            majorRes


min : Version -> Version -> Version
min v1 v2 =
    case compare v1 v2 of
        GT ->
            v2

        _ ->
            v1


max : Version -> Version -> Version
max v1 v2 =
    case compare v1 v2 of
        LT ->
            v2

        _ ->
            v1


one : Version
one =
    Version 1 0 0


maxVersion : Version
maxVersion =
    Version 2147483647 0 0


compiler : Version
compiler =
    --   case map fromIntegral (Version.versionBranch Paths_elm.version) of
    --     major : minor : patch : _ ->
    --       Version major minor patch
    --     [major, minor] ->
    --       Version major minor 0
    --     [major] ->
    --       Version major 0 0
    --     [] ->
    --       error "could not detect version of elm-compiler you are using"
    Version 0 19 1



-- BUMP


bumpPatch : Version -> Version
bumpPatch (Version major_ minor patch) =
    Version major_ minor (patch + 1)


bumpMinor : Version -> Version
bumpMinor (Version major_ minor _) =
    Version major_ (minor + 1) 0


bumpMajor : Version -> Version
bumpMajor (Version major_ _ _) =
    Version (major_ + 1) 0 0



-- TO CHARS


toChars : Version -> String
toChars (Version major_ minor patch) =
    String.fromInt major_ ++ "." ++ String.fromInt minor ++ "." ++ String.fromInt patch



-- JSON


decoder : D.Decoder ( Row, Col ) Version
decoder =
    D.customString parser Tuple.pair


encode : Version -> E.Value
encode version =
    E.string (toChars version)



-- PARSER


parser : P.Parser ( Row, Col ) Version
parser =
    numberParser
        |> P.bind
            (\major_ ->
                P.word1 '.' Tuple.pair
                    |> P.bind (\_ -> numberParser)
                    |> P.bind
                        (\minor ->
                            P.word1 '.' Tuple.pair
                                |> P.bind (\_ -> numberParser)
                                |> P.fmap
                                    (\patch ->
                                        Version major_ minor patch
                                    )
                        )
            )


numberParser : P.Parser ( Row, Col ) Int
numberParser =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                Err (P.PErr P.Empty row col Tuple.pair)

            else
                let
                    word : Char
                    word =
                        P.unsafeIndex src pos
                in
                if word == '0' then
                    let
                        newState : P.State
                        newState =
                            P.State src (pos + 1) end indent row (col + 1)
                    in
                    Ok (P.POk P.Consumed 0 newState)

                else if isDigit word then
                    let
                        ( total, newPos ) =
                            chompWord16 src (pos + 1) end (Char.toCode word - 0x30)

                        newState : P.State
                        newState =
                            P.State src newPos end indent row (col + (newPos - pos))
                    in
                    Ok (P.POk P.Consumed total newState)

                else
                    Err (P.PErr P.Empty row col Tuple.pair)


chompWord16 : String -> Int -> Int -> Int -> ( Int, Int )
chompWord16 src pos end total =
    if pos >= end then
        ( total, pos )

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if isDigit word then
            chompWord16 src (pos + 1) end (10 * total + (Char.toCode word - 0x30))

        else
            ( total, pos )


isDigit : Char -> Bool
isDigit word =
    '0' <= word && word <= '9'



-- ENCODERS and DECODERS


jsonCodec : Codec e Version
jsonCodec =
    Serialize.customType
        (\versionCodecEncoder (Version major_ minor patch) ->
            versionCodecEncoder major_ minor patch
        )
        |> Serialize.variant3 Version Serialize.int Serialize.int Serialize.int
        |> Serialize.finishCustomType


versionCodec : Codec e Version
versionCodec =
    Serialize.customType
        (\versionCodecEncoder (Version major_ minor patch) ->
            versionCodecEncoder major_ minor patch
        )
        |> Serialize.variant3 Version Serialize.int Serialize.int Serialize.int
        |> Serialize.finishCustomType
