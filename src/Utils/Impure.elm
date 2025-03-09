module Utils.Impure exposing
    ( Body(..)
    , Resolver(..)
    , customTask
    , task
    )

import Bytes exposing (Bytes)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Serialize
import Task exposing (Task)
import Utils.Crash exposing (crash)


type Body
    = EmptyBody
    | StringBody String
    | JsonBody Encode.Value
    | BytesBody Bytes


type Resolver e a
    = Always a
    | StringResolver (String -> a)
    | DecoderResolver (Decode.Decoder a)
    | BytesResolver (Bytes -> Result (Serialize.Error e) a)
    | Crash


customTask : String -> String -> List Http.Header -> Body -> Resolver e a -> Task Never a
customTask method url headers body resolver =
    Http.task
        { method = method
        , headers = headers
        , url = url
        , body =
            case body of
                EmptyBody ->
                    Http.emptyBody

                StringBody string ->
                    Http.stringBody "text/plain" string

                JsonBody value ->
                    Http.jsonBody value

                BytesBody bytes ->
                    Http.bytesBody "application/octet-stream" bytes
        , resolver =
            case resolver of
                Always x ->
                    Http.stringResolver (\_ -> Ok x)

                StringResolver fn ->
                    Http.stringResolver
                        (\response ->
                            case response of
                                Http.BadUrl_ url_ ->
                                    crash ("Unexpected BadUrl: " ++ url_)

                                Http.Timeout_ ->
                                    crash "Unexpected Timeout"

                                Http.NetworkError_ ->
                                    crash "Unexpected NetworkError"

                                Http.BadStatus_ metadata _ ->
                                    crash ("Unexpected BadStatus. Status code: " ++ String.fromInt metadata.statusCode)

                                Http.GoodStatus_ _ body_ ->
                                    Ok (fn body_)
                        )

                DecoderResolver decoder ->
                    Http.stringResolver
                        (\response ->
                            case response of
                                Http.BadUrl_ url_ ->
                                    crash ("Unexpected BadUrl: " ++ url_)

                                Http.Timeout_ ->
                                    crash "Unexpected Timeout"

                                Http.NetworkError_ ->
                                    crash "Unexpected NetworkError"

                                Http.BadStatus_ metadata _ ->
                                    crash ("Unexpected BadStatus. Status code: " ++ String.fromInt metadata.statusCode)

                                Http.GoodStatus_ _ body_ ->
                                    case Decode.decodeString decoder body_ of
                                        Ok value ->
                                            Ok value

                                        Err err ->
                                            crash ("Decoding error: " ++ Decode.errorToString err)
                        )

                BytesResolver bytesResolver ->
                    Http.bytesResolver
                        (\response ->
                            case response of
                                Http.BadUrl_ url_ ->
                                    crash ("Unexpected BadUrl: " ++ url_)

                                Http.Timeout_ ->
                                    crash "Unexpected Timeout"

                                Http.NetworkError_ ->
                                    crash "Unexpected NetworkError"

                                Http.BadStatus_ metadata _ ->
                                    crash ("Unexpected BadStatus. Status code: " ++ String.fromInt metadata.statusCode)

                                Http.GoodStatus_ _ body_ ->
                                    case bytesResolver body_ of
                                        Ok value ->
                                            Ok value

                                        Err _ ->
                                            crash "Decoding bytes error..."
                        )

                Crash ->
                    Http.stringResolver (\_ -> crash url)
        , timeout = Nothing
        }


task : String -> List Http.Header -> Body -> Resolver e a -> Task Never a
task url headers body resolver =
    customTask "POST" url headers body resolver
