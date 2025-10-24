module API.Main exposing (main)

import API.Format as Format
import API.Install as Install
import API.Make as Make
import API.Uninstall as Uninstall
import Builder.Reporting.Exit as Exit
import Compiler.Guida.Package as Pkg
import Compiler.Json.Encode as E
import Compiler.Parse.Module as M
import Compiler.Parse.Primitives as P
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Error as Error
import Compiler.Reporting.Error.Syntax as E
import Compiler.Reporting.Render.Code as Code
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

                    FormatArgs content ->
                        case Format.run content of
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

                    DiagnosticsArgs (DiagnosticsSourceContent src) ->
                        case P.fromByteString (M.chompModule SV.Guida M.Application) E.ModuleBadEnd src of
                            Ok _ ->
                                exitWithResponse (Encode.object [])

                            Err err ->
                                let
                                    source : Code.Source
                                    source =
                                        Code.toSource src

                                    error : Encode.Value
                                    error =
                                        E.encodeUgly (Error.reportToJson (E.toReport SV.Guida source (E.ParseError err)))
                                            |> Decode.decodeString Decode.value
                                            |> Result.withDefault Encode.null
                                in
                                exitWithResponse (Encode.object [ ( "errors", Encode.list identity [ error ] ) ])

                    DiagnosticsArgs (DiagnosticsSourcePath path) ->
                        Make.run path (Make.Flags False False False)
                            |> Task.bind
                                (\result ->
                                    case result of
                                        Ok _ ->
                                            exitWithResponse (Encode.object [])

                                        Err error ->
                                            exitWithResponse
                                                (E.encodeUgly (Exit.toJson (Exit.makeToReport error))
                                                    |> Decode.decodeString Decode.value
                                                    |> Result.withDefault Encode.null
                                                )
                                )
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
    | DiagnosticsArgs DiagnosticsSource


type DiagnosticsSource
    = DiagnosticsSourceContent String
    | DiagnosticsSourcePath String


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

                    "diagnostics" ->
                        Decode.map DiagnosticsArgs
                            (Decode.oneOf
                                [ Decode.map DiagnosticsSourceContent (Decode.field "content" Decode.string)
                                , Decode.map DiagnosticsSourcePath (Decode.field "path" Decode.string)
                                ]
                            )

                    _ ->
                        Decode.fail ("Unknown command: " ++ command)
            )
