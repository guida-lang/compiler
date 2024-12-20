module Compiler.Elm.Version exposing
    ( bumpMajor
    , bumpMinor
    , bumpPatch
    , compare
    , compiler
    , decoder
    , encode
    , jsonDecoder
    , jsonEncoder
    , major
    , max
    , maxVersion
    , min
    , one
    , parser
    , toChars
    , toComparable
    , versionDecoder
    , versionEncoder
    )

import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- VERSION


major : T.CEV_Version -> Int
major (T.CEV_Version major_ _ _) =
    major_


compare : T.CEV_Version -> T.CEV_Version -> Order
compare (T.CEV_Version major1 minor1 patch1) (T.CEV_Version major2 minor2 patch2) =
    case Basics.compare major1 major2 of
        EQ ->
            case Basics.compare minor1 minor2 of
                EQ ->
                    Basics.compare patch1 patch2

                minorRes ->
                    minorRes

        majorRes ->
            majorRes


toComparable : T.CEV_Version -> ( Int, Int, Int )
toComparable (T.CEV_Version major_ minor_ patch_) =
    ( major_, minor_, patch_ )


min : T.CEV_Version -> T.CEV_Version -> T.CEV_Version
min v1 v2 =
    case compare v1 v2 of
        GT ->
            v2

        _ ->
            v1


max : T.CEV_Version -> T.CEV_Version -> T.CEV_Version
max v1 v2 =
    case compare v1 v2 of
        LT ->
            v2

        _ ->
            v1


one : T.CEV_Version
one =
    T.CEV_Version 1 0 0


maxVersion : T.CEV_Version
maxVersion =
    T.CEV_Version 2147483647 0 0


compiler : T.CEV_Version
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
    T.CEV_Version 0 19 1



-- BUMP


bumpPatch : T.CEV_Version -> T.CEV_Version
bumpPatch (T.CEV_Version major_ minor patch) =
    T.CEV_Version major_ minor (patch + 1)


bumpMinor : T.CEV_Version -> T.CEV_Version
bumpMinor (T.CEV_Version major_ minor _) =
    T.CEV_Version major_ (minor + 1) 0


bumpMajor : T.CEV_Version -> T.CEV_Version
bumpMajor (T.CEV_Version major_ _ _) =
    T.CEV_Version (major_ + 1) 0 0



-- TO CHARS


toChars : T.CEV_Version -> String
toChars (T.CEV_Version major_ minor patch) =
    String.fromInt major_ ++ "." ++ String.fromInt minor ++ "." ++ String.fromInt patch



-- JSON


decoder : D.Decoder ( T.CPP_Row, T.CPP_Col ) T.CEV_Version
decoder =
    D.customString parser Tuple.pair


encode : T.CEV_Version -> E.Value
encode version =
    E.string (toChars version)



-- PARSER


parser : P.Parser ( T.CPP_Row, T.CPP_Col ) T.CEV_Version
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
                                        T.CEV_Version major_ minor patch
                                    )
                        )
            )


numberParser : P.Parser ( T.CPP_Row, T.CPP_Col ) Int
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


jsonDecoder : Decode.Decoder T.CEV_Version
jsonDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case P.fromByteString parser Tuple.pair str of
                    Ok version ->
                        Decode.succeed version

                    Err _ ->
                        Decode.fail "failed to parse version"
            )


versionEncoder : T.CEV_Version -> Encode.Value
versionEncoder (T.CEV_Version major_ minor_ patch_) =
    Encode.object
        [ ( "type", Encode.string "Version" )
        , ( "major", Encode.int major_ )
        , ( "minor", Encode.int minor_ )
        , ( "patch", Encode.int patch_ )
        ]


versionDecoder : Decode.Decoder T.CEV_Version
versionDecoder =
    Decode.map3 T.CEV_Version
        (Decode.field "major" Decode.int)
        (Decode.field "minor" Decode.int)
        (Decode.field "patch" Decode.int)


jsonEncoder : T.CEV_Version -> Encode.Value
jsonEncoder version =
    Encode.string (toChars version)
