module Compiler.Elm.Package exposing
    ( browser
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
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Suggest as Suggest
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- PACKAGE NAMES


toString : T.CEP_Name -> String
toString ( author, project ) =
    author ++ "/" ++ project


compareName : T.CEP_Name -> T.CEP_Name -> Order
compareName ( name1, project1 ) ( name2, project2 ) =
    case compare name1 name2 of
        LT ->
            LT

        EQ ->
            compare project1 project2

        GT ->
            GT



-- HELPERS


isKernel : T.CEP_Name -> Bool
isKernel ( author, _ ) =
    author == elm || author == elm_explorations


toChars : T.CEP_Name -> String
toChars ( author, project ) =
    author ++ "/" ++ project


toUrl : T.CEP_Name -> String
toUrl ( author, project ) =
    author ++ "/" ++ project


toJsonString : T.CEP_Name -> String
toJsonString ( author, project ) =
    String.join "/" [ author, project ]



-- COMMON PACKAGE NAMES


toName : T.CEP_Author -> T.CEP_Project -> T.CEP_Name
toName =
    Tuple.pair


dummyName : T.CEP_Name
dummyName =
    toName "author" "project"


kernel : T.CEP_Name
kernel =
    toName elm "kernel"


core : T.CEP_Name
core =
    toName elm "core"


browser : T.CEP_Name
browser =
    toName elm "browser"


virtualDom : T.CEP_Name
virtualDom =
    toName elm "virtual-dom"


html : T.CEP_Name
html =
    toName elm "html"


json : T.CEP_Name
json =
    toName elm "json"


http : T.CEP_Name
http =
    toName elm "http"


url : T.CEP_Name
url =
    toName elm "url"


webgl : T.CEP_Name
webgl =
    toName elm_explorations "webgl"


linearAlgebra : T.CEP_Name
linearAlgebra =
    toName elm_explorations "linear-algebra"


elm : T.CEP_Author
elm =
    "elm"


elm_explorations : T.CEP_Author
elm_explorations =
    "elm-explorations"



-- PACKAGE SUGGESTIONS


suggestions : Dict String String T.CEP_Name
suggestions =
    let
        random : T.CEP_Name
        random =
            toName elm "random"

        time : T.CEP_Name
        time =
            toName elm "time"

        file : T.CEP_Name
        file =
            toName elm "file"
    in
    Dict.fromList identity
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


nearbyNames : T.CEP_Name -> List T.CEP_Name -> List T.CEP_Name
nearbyNames ( author1, project1 ) possibleNames =
    let
        authorDist : T.CEP_Author -> Int
        authorDist =
            authorDistance author1

        projectDist : T.CEP_Project -> Int
        projectDist =
            projectDistance project1

        nameDistance : T.CEP_Name -> Int
        nameDistance ( author2, project2 ) =
            authorDist author2 + projectDist project2
    in
    List.take 4 (List.sortBy nameDistance possibleNames)


authorDistance : String -> T.CEP_Author -> Int
authorDistance given possibility =
    if possibility == elm || possibility == elm_explorations then
        0

    else
        abs (Suggest.distance given possibility)


projectDistance : String -> T.CEP_Project -> Int
projectDistance given possibility =
    abs (Suggest.distance given possibility)



-- JSON


decoder : D.Decoder ( T.CPP_Row, T.CPP_Col ) T.CEP_Name
decoder =
    D.customString parser Tuple.pair


encode : T.CEP_Name -> E.Value
encode name =
    E.string (toChars name)


keyDecoder : (T.CPP_Row -> T.CPP_Col -> x) -> D.KeyDecoder x T.CEP_Name
keyDecoder toError =
    let
        keyParser : P.Parser x T.CEP_Name
        keyParser =
            P.specialize (\( r, c ) _ _ -> toError r c) parser
    in
    D.KeyDecoder keyParser toError



-- PARSER


parser : P.Parser ( T.CPP_Row, T.CPP_Col ) T.CEP_Name
parser =
    parseName isAlphaOrDigit isAlphaOrDigit
        |> P.bind
            (\author ->
                P.word1 '/' Tuple.pair
                    |> P.bind (\_ -> parseName isLower isLowerOrDigit)
                    |> P.fmap
                        (\project -> ( author, project ))
            )


parseName : (Char -> Bool) -> (Char -> Bool) -> P.Parser ( T.CPP_Row, T.CPP_Col ) String
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

                        newCol : T.CPP_Col
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


nameEncoder : T.CEP_Name -> Encode.Value
nameEncoder ( author, project ) =
    Encode.object
        [ ( "author", Encode.string author )
        , ( "project", Encode.string project )
        ]


nameDecoder : Decode.Decoder T.CEP_Name
nameDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "author" Decode.string)
        (Decode.field "project" Decode.string)
