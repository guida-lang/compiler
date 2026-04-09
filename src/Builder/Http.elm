module Builder.Http exposing
    ( Header
    , Manager
    , Sha
    , accept
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
import Compiler.Guida.Version as V
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import System.Misc as Misc
import Task exposing (Task)
import Url.Builder
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Impure as Impure
import Utils.Main as Utils
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
    Task.succeed Manager



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


get : Manager -> String -> List Header -> (Misc.Error -> e) -> (String -> Task Never (Result e a)) -> Task Never (Result e a)
get =
    fetch "GET"


post : Manager -> String -> List Header -> (Misc.Error -> e) -> (String -> Task Never (Result e a)) -> Task Never (Result e a)
post =
    fetch "POST"


fetch : String -> Manager -> String -> List Header -> (Misc.Error -> e) -> (String -> Task Never (Result e a)) -> Task Never (Result e a)
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
    "guida/" ++ V.toChars V.compiler


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


getArchive : Manager -> String -> (Misc.Error -> e) -> e -> (( Sha, Zip.Archive ) -> Task Never (Result e a)) -> Task Never (Result e a)
getArchive _ url _ _ onSuccess =
    Misc.getArchive url
        |> Task.map (Tuple.mapSecond (List.map (uncurry Zip.Entry)))
        |> Task.andThen onSuccess



-- UPLOAD


upload : Manager -> String -> List Misc.MultiPart -> Task Never (Result Misc.Error ())
upload _ url parts =
    Misc.httpUpload url (addDefaultHeaders []) parts


filePart : String -> String -> Misc.MultiPart
filePart name filePath =
    Misc.filePart name filePath


jsonPart : String -> String -> Encode.Value -> Misc.MultiPart
jsonPart name filePath value =
    Misc.jsonPart name filePath value


stringPart : String -> String -> Misc.MultiPart
stringPart name string =
    Misc.stringPart name string
