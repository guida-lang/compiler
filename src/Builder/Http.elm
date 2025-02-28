module Builder.Http exposing
    ( Error(..)
    , Header
    , Manager
    , MultiPart
    , Sha
    , accept
    , errorDecoder
    , errorEncoder
    , filePart
    , get
    , getArchive
    , getManager
    , jsonPart
    , managerDecoder
    , managerEncoder
    , post
    , shaToChars
    , stringPart
    , toUrl
    , upload
    )

import Basics.Extra exposing (uncurry)
import Codec.Archive.Zip as Zip
import Compiler.Elm.Version as V
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO(..))
import Url.Builder
import Utils.Crash exposing (crash)
import Utils.Main as Utils exposing (SomeException)



-- MANAGER


type Manager
    = Manager


managerEncoder : Manager -> Encode.Value
managerEncoder _ =
    Encode.object [ ( "type", Encode.string "Manager" ) ]


managerDecoder : Decode.Decoder Manager
managerDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Manager" ->
                        Decode.succeed Manager

                    _ ->
                        Decode.fail "Failed to decode Http.Manager"
            )


getManager : IO Manager
getManager =
    -- TODO newManager tlsManagerSettings
    IO.pure Manager



-- URL


toUrl : String -> List ( String, String ) -> String
toUrl url params =
    case params of
        [] ->
            url

        _ :: _ ->
            url ++ urlEncodeVars params


urlEncodeVars : List ( String, String ) -> String
urlEncodeVars params =
    -- includes the `?`
    Url.Builder.toQuery (List.map (uncurry Url.Builder.string) params)



-- FETCH


type alias Header =
    ( String, String )


get : Manager -> String -> List Header -> (Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
get =
    fetch "GET"


post : Manager -> String -> List Header -> (Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
post =
    fetch "POST"


fetch : String -> Manager -> String -> List Header -> (Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
fetch method _ url headers _ onSuccess =
    IO
        (\_ s ->
            ( s
            , IO.ImpureTask
                (Http.task
                    { method = method
                    , headers = List.map (\( a, b ) -> Http.header a b) (addDefaultHeaders headers)
                    , url = url
                    , body = Http.emptyBody
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ body ->
                                        Ok (onSuccess body)

                                    _ ->
                                        Ok (onSuccess "")
                            )
                    , timeout = Nothing
                    }
                )
            )
        )


addDefaultHeaders : List Header -> List Header
addDefaultHeaders headers =
    ( "User-Agent", userAgent ) :: ( "Accept-Encoding", "gzip" ) :: headers


userAgent : String
userAgent =
    "elm/" ++ V.toChars V.compiler


accept : String -> Header
accept mime =
    ( "Accept", mime )



-- EXCEPTIONS


type Error
    = BadUrl String String
    | BadHttp String Utils.HttpExceptionContent
    | BadMystery String SomeException



-- SHA


type alias Sha =
    String


shaToChars : Sha -> String
shaToChars =
    identity



-- FETCH ARCHIVE


getArchive : Manager -> String -> (Error -> e) -> e -> (( Sha, Zip.Archive ) -> IO (Result e a)) -> IO (Result e a)
getArchive _ url _ _ onSuccess =
    IO
        (\_ s ->
            ( s
            , IO.ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "getArchive"
                    , body = Http.stringBody "text/plain" url
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ body ->
                                        let
                                            shaAndArchiveResult : Result Decode.Error ( String, List Zip.Entry )
                                            shaAndArchiveResult =
                                                Decode.decodeString
                                                    (Decode.map2 Tuple.pair
                                                        (Decode.field "sha" Decode.string)
                                                        (Decode.field "archive"
                                                            (Decode.list
                                                                (Decode.map2 Zip.Entry
                                                                    (Decode.field "eRelativePath" Decode.string)
                                                                    (Decode.field "eData" Decode.string)
                                                                )
                                                            )
                                                        )
                                                    )
                                                    body
                                        in
                                        case shaAndArchiveResult of
                                            Ok shaAndArchive ->
                                                Ok (onSuccess shaAndArchive)

                                            Err _ ->
                                                crash "getArchive"

                                    _ ->
                                        crash "getArchive"
                            )
                    , timeout = Nothing
                    }
                )
            )
        )



-- UPLOAD


type MultiPart
    = FilePart String String
    | JsonPart String String Encode.Value
    | StringPart String String


upload : Manager -> String -> List MultiPart -> IO (Result Error ())
upload _ url parts =
    IO
        (\_ s ->
            ( s
            , IO.ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "httpUpload"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "urlStr", Encode.string url )
                                , ( "headers", Encode.object (List.map (Tuple.mapSecond Encode.string) (addDefaultHeaders [])) )
                                , ( "parts"
                                  , Encode.list
                                        (\part ->
                                            case part of
                                                FilePart name filePath ->
                                                    Encode.object
                                                        [ ( "type", Encode.string "FilePart" )
                                                        , ( "name", Encode.string name )
                                                        , ( "filePath", Encode.string filePath )
                                                        ]

                                                JsonPart name filePath value ->
                                                    Encode.object
                                                        [ ( "type", Encode.string "JsonPart" )
                                                        , ( "name", Encode.string name )
                                                        , ( "filePath", Encode.string filePath )
                                                        , ( "value", value )
                                                        ]

                                                StringPart name string ->
                                                    Encode.object
                                                        [ ( "type", Encode.string "StringPart" )
                                                        , ( "name", Encode.string name )
                                                        , ( "string", Encode.string string )
                                                        ]
                                        )
                                        parts
                                  )
                                ]
                            )
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ _ ->
                                        Ok (IO.pure ())

                                    _ ->
                                        crash "httpUpload"
                            )
                    , timeout = Nothing
                    }
                )
            )
        )
        |> IO.fmap Ok


filePart : String -> String -> MultiPart
filePart name filePath =
    FilePart name filePath


jsonPart : String -> String -> Encode.Value -> MultiPart
jsonPart name filePath value =
    JsonPart name filePath value


stringPart : String -> String -> MultiPart
stringPart name string =
    StringPart name string



-- ENCODERS and DECODERS


errorEncoder : Error -> Encode.Value
errorEncoder error =
    case error of
        BadUrl url reason ->
            Encode.object
                [ ( "type", Encode.string "BadUrl" )
                , ( "url", Encode.string url )
                , ( "reason", Encode.string reason )
                ]

        BadHttp url httpExceptionContent ->
            Encode.object
                [ ( "type", Encode.string "BadHttp" )
                , ( "url", Encode.string url )
                , ( "httpExceptionContent", Utils.httpExceptionContentEncoder httpExceptionContent )
                ]

        BadMystery url someException ->
            Encode.object
                [ ( "type", Encode.string "BadMystery" )
                , ( "url", Encode.string url )
                , ( "someException", Utils.someExceptionEncoder someException )
                ]


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "BadUrl" ->
                        Decode.map2 BadUrl
                            (Decode.field "url" Decode.string)
                            (Decode.field "reason" Decode.string)

                    "BadHttp" ->
                        Decode.map2 BadHttp
                            (Decode.field "url" Decode.string)
                            (Decode.field "httpExceptionContent" Utils.httpExceptionContentDecoder)

                    "BadMystery" ->
                        Decode.map2 BadMystery
                            (Decode.field "url" Decode.string)
                            (Decode.field "someException" Utils.someExceptionDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Error's type: " ++ type_)
            )
