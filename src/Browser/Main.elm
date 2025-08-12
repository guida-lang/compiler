module Browser.Main exposing (main)

import Browser.Format as Format
import Browser.Install as Install
import Browser.Make as Make
import Browser.Uninstall as Uninstall
import Builder.Reporting.Exit as Exit
import Compiler.Elm.Package as Pkg
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO
import Task exposing (Task)
import Utils.Impure as Impure
import Utils.Task.Extra as Task


main : IO.Program
main =
    IO.run app


app : Task Never ()
app =
    getArgs
        |> Task.bind
            (\args ->
                case args of
                    MakeArgs path debug optimize withSourceMaps ->
                        Make.run path (Make.Flags debug optimize withSourceMaps)
                            |> Task.bind
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

                    InstallArgs pkgString ->
                        case P.fromByteString Pkg.parser Tuple.pair pkgString of
                            Ok pkg ->
                                Install.run pkg
                                    |> Task.bind (\_ -> exitWithResponse Encode.null)

                            Err _ ->
                                exitWithResponse (Encode.object [ ( "error", Encode.string "Invalid package..." ) ])

                    UninstallArgs pkgString ->
                        case P.fromByteString Pkg.parser Tuple.pair pkgString of
                            Ok pkg ->
                                Uninstall.run pkg
                                    |> Task.bind (\_ -> exitWithResponse Encode.null)

                            Err _ ->
                                exitWithResponse (Encode.object [ ( "error", Encode.string "Invalid package..." ) ])
            )


getArgs : Task Never Args
getArgs =
    Impure.task "getArgs" [] Impure.EmptyBody (Impure.DecoderResolver argsDecoder)


exitWithResponse : Encode.Value -> Task Never a
exitWithResponse value =
    Impure.task "exitWithResponse" [] (Impure.JsonBody value) Impure.Crash



-- ARGS


type Args
    = MakeArgs String Bool Bool Bool
    | FormatArgs String
    | InstallArgs String
    | UninstallArgs String


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

                    "install" ->
                        Decode.map InstallArgs
                            (Decode.field "pkg" Decode.string)

                    "uninstall" ->
                        Decode.map UninstallArgs
                            (Decode.field "pkg" Decode.string)

                    _ ->
                        Decode.fail ("Unknown command: " ++ command)
            )
