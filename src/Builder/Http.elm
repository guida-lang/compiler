module Builder.Http exposing
    ( Header
    , MultiPart
    , Sha
    , accept
    , filePart
    , get
    , getArchive
    , getManager
    , jsonPart
    , post
    , shaToChars
    , stringPart
    , toUrl
    , upload
    )

import Basics.Extra exposing (uncurry)
import Compiler.Elm.Version as V
import Json.Encode as Encode
import System.IO as IO
import Types as T exposing (IO(..))
import Url.Builder



-- MANAGER


getManager : IO T.BH_Manager
getManager =
    -- TODO newManager tlsManagerSettings
    IO.pure T.BH_Manager



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


get : T.BH_Manager -> String -> List Header -> (T.BH_Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
get =
    fetch MethodGet


post : T.BH_Manager -> String -> List Header -> (T.BH_Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
post =
    fetch MethodPost


type Method
    = MethodGet
    | MethodPost


fetch : Method -> T.BH_Manager -> String -> List Header -> (T.BH_Error -> e) -> (String -> IO (Result e a)) -> IO (Result e a)
fetch methodVerb _ url headers _ onSuccess =
    IO
        (\_ s ->
            ( s
            , T.HttpFetch IO.pure
                (case methodVerb of
                    MethodGet ->
                        "GET"

                    MethodPost ->
                        "POST"
                )
                url
                (addDefaultHeaders headers)
            )
        )
        |> IO.bind onSuccess


addDefaultHeaders : List Header -> List Header
addDefaultHeaders headers =
    ( "User-Agent", userAgent ) :: ( "Accept-Encoding", "gzip" ) :: headers


userAgent : String
userAgent =
    "elm/" ++ V.toChars V.compiler


accept : String -> Header
accept mime =
    ( "Accept", mime )



-- SHA


type alias Sha =
    String


shaToChars : Sha -> String
shaToChars =
    identity



-- FETCH ARCHIVE


getArchive : T.BH_Manager -> String -> (T.BH_Error -> e) -> e -> (( Sha, T.CAZ_Archive ) -> IO (Result e a)) -> IO (Result e a)
getArchive _ url _ _ onSuccess =
    IO (\_ s -> ( s, T.GetArchive IO.pure "GET" url ))
        |> IO.bind (\shaAndArchive -> onSuccess shaAndArchive)



-- UPLOAD


type MultiPart
    = FilePart String String
    | JsonPart String String Encode.Value
    | StringPart String String


upload : T.BH_Manager -> String -> List MultiPart -> IO (Result T.BH_Error ())
upload _ url parts =
    IO
        (\_ s ->
            ( s
            , T.HttpUpload IO.pure
                url
                (addDefaultHeaders [])
                (List.map
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
