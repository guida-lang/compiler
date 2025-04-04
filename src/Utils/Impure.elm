module Utils.Impure exposing
    ( Body(..)
    , Resolver(..)
    , customTask
    , task
    )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)


type Body
    = EmptyBody
    | StringBody String
    | JsonBody Encode.Value
    | BytesBody BE.Encoder


type Resolver a
    = Always a
    | StringResolver (String -> a)
    | DecoderResolver (Decode.Decoder a)
    | BytesResolver (BD.Decoder a)
    | Crash


customTask : String -> String -> List Http.Header -> Body -> Resolver a -> Task Never a
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

                BytesBody encoder ->
                    Http.bytesBody "application/octet-stream" (BE.encode encoder)
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

                BytesResolver decoder ->
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
                                    case BD.decode decoder body_ of
                                        Just value ->
                                            Ok value

                                        Nothing ->
                                            crash "Decoding bytes error..."
                        )

                Crash ->
                    Http.stringResolver (\_ -> crash url)
        , timeout = Nothing
        }


task : String -> List Http.Header -> Body -> Resolver a -> Task Never a
task url headers body resolver =
    customTask "POST" url headers body resolver
