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
import Task exposing (Task)
import Url.Builder
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Impure as Impure
import Utils.Main as Utils exposing (SomeException)
import Utils.Task.Extra as Task



-- MANAGER


type Manager
    = Manager


managerEncoder : Manager -> BE.Encoder
managerEncoder _ =
    BE.unsignedInt8 0


managerDecoder : BD.Decoder Manager
managerDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Manager

                    _ ->
                        BD.fail
            )


getManager : Task Never Manager
getManager =
    -- TODO newManager tlsManagerSettings
    Task.pure Manager



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


get : Manager -> String -> List Header -> (Error -> e) -> (String -> Task Never (Result e a)) -> Task Never (Result e a)
get =
    fetch "GET"


post : Manager -> String -> List Header -> (Error -> e) -> (String -> Task Never (Result e a)) -> Task Never (Result e a)
post =
    fetch "POST"


fetch : String -> Manager -> String -> List Header -> (Error -> e) -> (String -> Task Never (Result e a)) -> Task Never (Result e a)
fetch method _ url headers _ onSuccess =
    Impure.customTask method
        url
        (List.map (\( a, b ) -> Http.header a b) (addDefaultHeaders headers))
        Impure.EmptyBody
        (Impure.StringResolver identity)
        |> Task.andThen onSuccess


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


getArchive : Manager -> String -> (Error -> e) -> e -> (( Sha, Zip.Archive ) -> Task Never (Result e a)) -> Task Never (Result e a)
getArchive _ url _ _ onSuccess =
    Impure.task "getArchive"
        []
        (Impure.StringBody url)
        (Impure.DecoderResolver
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
        )
        |> Task.andThen onSuccess



-- UPLOAD


type MultiPart
    = FilePart String String
    | JsonPart String String Encode.Value
    | StringPart String String


upload : Manager -> String -> List MultiPart -> Task Never (Result Error ())
upload _ url parts =
    Impure.task "httpUpload"
        []
        (Impure.JsonBody
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
        )
        (Impure.Always (Ok ()))


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


errorEncoder : Error -> BE.Encoder
errorEncoder error =
    case error of
        BadUrl url reason ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string url
                , BE.string reason
                ]

        BadHttp url httpExceptionContent ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string url
                , Utils.httpExceptionContentEncoder httpExceptionContent
                ]

        BadMystery url someException ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string url
                , Utils.someExceptionEncoder someException
                ]


errorDecoder : BD.Decoder Error
errorDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 BadUrl
                            BD.string
                            BD.string

                    1 ->
                        BD.map2 BadHttp
                            BD.string
                            Utils.httpExceptionContentDecoder

                    2 ->
                        BD.map2 BadMystery
                            BD.string
                            Utils.someExceptionDecoder

                    _ ->
                        BD.fail
            )
