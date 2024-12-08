module Compiler.Elm.Package exposing
    ( Author
    , Name
    , Project
    , browser
    , compareName
    , core
    , decoder
    , dummyName
    , encode
    , html
    , isKernel
    , json
    , kernel
    , keyDecoder
    , linearAlgebra
    , nameDecoder
    , nameEncoder
    , nearbyNames
    , parser
    , suggestions
    , toChars
    , toJsonString
    , toString
    , toUrl
    , virtualDom
    , webgl
    )

import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Reporting.Suggest as Suggest
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode



-- PACKAGE NAMES


{-| This has been simplified from `Name Author Project` as part of the work for
`System.TypeCheck.IO`.
-}
type alias Name =
    ( Author, Project )


toString : Name -> String
toString ( author, project ) =
    author ++ "/" ++ project


compareName : Name -> Name -> Order
compareName ( name1, project1 ) ( name2, project2 ) =
    case compare name1 name2 of
        LT ->
            LT

        EQ ->
            compare project1 project2

        GT ->
            GT


type alias Author =
    String


type alias Project =
    String



-- HELPERS


isKernel : Name -> Bool
isKernel ( author, _ ) =
    author == elm || author == elm_explorations


toChars : Name -> String
toChars ( author, project ) =
    author ++ "/" ++ project


toUrl : Name -> String
toUrl ( author, project ) =
    author ++ "/" ++ project


toJsonString : Name -> String
toJsonString ( author, project ) =
    String.join "/" [ author, project ]



-- COMMON PACKAGE NAMES


toName : Author -> Project -> Name
toName =
    Tuple.pair


dummyName : Name
dummyName =
    toName "author" "project"


kernel : Name
kernel =
    toName elm "kernel"


core : Name
core =
    toName elm "core"


browser : Name
browser =
    toName elm "browser"


virtualDom : Name
virtualDom =
    toName elm "virtual-dom"


html : Name
html =
    toName elm "html"


json : Name
json =
    toName elm "json"


http : Name
http =
    toName elm "http"


url : Name
url =
    toName elm "url"


webgl : Name
webgl =
    toName elm_explorations "webgl"


linearAlgebra : Name
linearAlgebra =
    toName elm_explorations "linear-algebra"


elm : Author
elm =
    "elm"


elm_explorations : Author
elm_explorations =
    "elm-explorations"



-- PACKAGE SUGGESTIONS


suggestions : Dict String Name
suggestions =
    let
        random : Name
        random =
            toName elm "random"

        time : Name
        time =
            toName elm "time"

        file : Name
        file =
            toName elm "file"
    in
    Dict.fromList compare
        [ ( "Browser", browser )
        , ( "File", file )
        , ( "File.Download", file )
        , ( "File.Select", file )
        , ( "Html", html )
        , ( "Html.Attributes", html )
        , ( "Html.Events", html )
        , ( "Http", http )
        , ( "Json.Decode", json )
        , ( "Json.Encode", json )
        , ( "Random", random )
        , ( "Time", time )
        , ( "Url.Parser", url )
        , ( "Url", url )
        ]



-- NEARBY NAMES


nearbyNames : Name -> List Name -> List Name
nearbyNames ( author1, project1 ) possibleNames =
    let
        authorDist : Author -> Int
        authorDist =
            authorDistance author1

        projectDist : Project -> Int
        projectDist =
            projectDistance project1

        nameDistance : Name -> Int
        nameDistance ( author2, project2 ) =
            authorDist author2 + projectDist project2
    in
    List.take 4 (List.sortBy nameDistance possibleNames)


authorDistance : String -> Author -> Int
authorDistance given possibility =
    if possibility == elm || possibility == elm_explorations then
        0

    else
        abs (Suggest.distance given possibility)


projectDistance : String -> Project -> Int
projectDistance given possibility =
    abs (Suggest.distance given possibility)



-- JSON


decoder : D.Decoder ( Row, Col ) Name
decoder =
    D.customString parser Tuple.pair


encode : Name -> E.Value
encode name =
    E.string (toChars name)


keyDecoder : (Row -> Col -> x) -> D.KeyDecoder x Name
keyDecoder toError =
    let
        keyParser : P.Parser x Name
        keyParser =
            P.specialize (\( r, c ) _ _ -> toError r c) parser
    in
    D.KeyDecoder keyParser toError



-- PARSER


parser : P.Parser ( Row, Col ) Name
parser =
    parseName isAlphaOrDigit isAlphaOrDigit
        |> P.bind
            (\author ->
                P.word1 '/' Tuple.pair
                    |> P.bind (\_ -> parseName isLower isLowerOrDigit)
                    |> P.fmap
                        (\project -> ( author, project ))
            )


parseName : (Char -> Bool) -> (Char -> Bool) -> P.Parser ( Row, Col ) String
parseName isGoodStart isGoodInner =
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
                if not (isGoodStart word) then
                    Err (P.PErr P.Empty row col Tuple.pair)

                else
                    let
                        ( isGood, newPos ) =
                            chompName isGoodInner src (pos + 1) end False

                        len : Int
                        len =
                            newPos - pos

                        newCol : Col
                        newCol =
                            col + len
                    in
                    if isGood && len < 256 then
                        let
                            newState : P.State
                            newState =
                                P.State src newPos end indent row newCol
                        in
                        Ok (P.POk P.Consumed (String.slice pos newPos src) newState)

                    else
                        Err (P.PErr P.Consumed row newCol Tuple.pair)


isLower : Char -> Bool
isLower =
    Char.isLower


isLowerOrDigit : Char -> Bool
isLowerOrDigit word =
    Char.isLower word || Char.isDigit word


isAlphaOrDigit : Char -> Bool
isAlphaOrDigit =
    Char.isAlphaNum


chompName : (Char -> Bool) -> String -> Int -> Int -> Bool -> ( Bool, Int )
chompName isGoodChar src pos end prevWasDash =
    if pos >= end then
        ( not prevWasDash, pos )

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if isGoodChar word then
            chompName isGoodChar src (pos + 1) end False

        else if word == '-' then
            if prevWasDash then
                ( False, pos )

            else
                chompName isGoodChar src (pos + 1) end True

        else
            ( True, pos )



-- ENCODERS and DECODERS


nameEncoder : Name -> Encode.Value
nameEncoder ( author, project ) =
    Encode.object
        [ ( "author", Encode.string author )
        , ( "project", Encode.string project )
        ]


nameDecoder : Decode.Decoder Name
nameDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "author" Decode.string)
        (Decode.field "project" Decode.string)
