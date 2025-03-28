module Browser.Main exposing (main)

import Browser.Format as Format
import Browser.Make as Make
import Builder.Reporting.Exit as Exit
import Compiler.Json.Encode as E
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO)
import Utils.Impure as Impure


main : IO.Program
main =
    IO.run app


app : IO ()
app =
    getArgs
        |> IO.bind
            (\args ->
                case args of
                    MakeArgs path debug optimize withSourceMaps ->
                        Make.run path (Make.Flags debug optimize withSourceMaps)
                            |> IO.bind
                                (\result ->
                                    case result of
                                        Ok output ->
                                            exitWithResponse (Encode.object [ ( "output", Encode.string output ) ])

                                        Err error ->
                                            exitWithResponse (Encode.object [ ( "error", Encode.string (E.encodeUgly (Exit.toJson (Exit.makeToReport error))) ) ])
                                )

                    FormatArgs path ->
                        case Format.run path of
                            Ok output ->
                                exitWithResponse (Encode.object [ ( "output", Encode.string output ) ])

                            Err error ->
                                exitWithResponse (Encode.object [ ( "error", Encode.string error ) ])
            )


getArgs : IO Args
getArgs =
    Impure.task "getArgs" [] Impure.EmptyBody (Impure.DecoderResolver argsDecoder)


exitWithResponse : Encode.Value -> IO a
exitWithResponse value =
    Impure.task "exitWithResponse" [] (Impure.JsonBody value) Impure.Crash



-- ARGS


type Args
    = MakeArgs String Bool Bool Bool
    | FormatArgs String


argsDecoder : Decode.Decoder Args
argsDecoder =
    Decode.field "command" Decode.string
        |> Decode.andThen
            (\command ->
                case command of
                    "make" ->
                        Decode.map4 MakeArgs
                            (Decode.field "path" Decode.string)
                            (Decode.field "debug" Decode.bool)
                            (Decode.field "optimize" Decode.bool)
                            (Decode.field "sourcemaps" Decode.bool)

                    "format" ->
                        Decode.map FormatArgs
                            (Decode.field "content" Decode.string)

                    _ ->
                        Decode.fail ("Unknown command: " ++ command)
            )
