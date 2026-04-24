module API.Main exposing (main)

import API.Diagnostics as Diagnostics
import API.Format as Format
import API.Init as Init
import API.Install as Install
import API.LanguageServerProtocol as LanguageServerProtocol
import API.Make as Make
import API.Uninstall as Uninstall
import API.Upgrade as Upgrade
import Builder.Reporting.Exit as Exit
import Compiler.Generate.Target as Target
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
                    InitArgs package ->
                        Init.run (Init.Flags package)
                            |> Task.bind
                                (\result ->
                                    case result of
                                        Ok () ->
                                            exitWithResponse Encode.null

                                        Err error ->
                                            exitWithResponse (Encode.object [ ( "error", Encode.string (E.encodeUgly (Exit.toJson (Exit.initToReport error))) ) ])
                                )

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

                    UpgradeArgs ->
                        Upgrade.run
                            |> Task.bind (\_ -> exitWithResponse Encode.null)

                    DiagnosticsArgs (DiagnosticsSourceContent src) ->
                        -- FIXME target
                        case P.fromByteString (M.chompModule Target.GuidaTarget SV.Guida M.Application) E.ModuleBadEnd src of
                            Ok _ ->
                                exitWithResponse Encode.null

                            Err err ->
                                let
                                    source : Code.Source
                                    source =
                                        Code.toSource src

                                    error : Encode.Value
                                    error =
                                        E.encodeUgly (Error.reportToJson (E.toReport source (E.ParseError err)))
                                            |> Decode.decodeString Decode.value
                                            |> Result.withDefault Encode.null
                                in
                                exitWithResponse
                                    (Encode.object
                                        [ ( "type", Encode.string "content-error" )
                                        , ( "error", error )
                                        ]
                                    )

                    DiagnosticsArgs (DiagnosticsSourcePath path) ->
                        Diagnostics.run path
                            |> Task.bind
                                (\result ->
                                    case result of
                                        Ok _ ->
                                            exitWithResponse Encode.null

                                        Err error ->
                                            exitWithResponse
                                                (E.encodeUgly (Exit.toJson (Exit.makeToReport error))
                                                    |> Decode.decodeString Decode.value
                                                    |> Result.withDefault Encode.null
                                                )
                                )

                    GetDefinitionLocationArgs path line character ->
                        LanguageServerProtocol.getDefinitionLocation path line character
                            |> Task.bind
                                (\result ->
                                    case result of
                                        Ok location ->
                                            exitWithResponse
                                                (Encode.object
                                                    [ ( "path", Encode.string location.path )
                                                    , ( "range"
                                                      , Encode.object
                                                            [ ( "start"
                                                              , Encode.object
                                                                    [ ( "line", Encode.int location.range.start.line )
                                                                    , ( "character", Encode.int location.range.start.character )
                                                                    ]
                                                              )
                                                            , ( "end"
                                                              , Encode.object
                                                                    [ ( "line", Encode.int location.range.end.line )
                                                                    , ( "character", Encode.int location.range.end.character )
                                                                    ]
                                                              )
                                                            ]
                                                      )
                                                    ]
                                                )

                                        Err _ ->
                                            exitWithResponse Encode.null
                                )

                    FindReferencesArgs path line character ->
                        LanguageServerProtocol.findReferences path line character
                            |> Task.bind
                                (\result ->
                                    case result of
                                        Ok locations ->
                                            exitWithResponse
                                                (Encode.list
                                                    (\location ->
                                                        Encode.object
                                                            [ ( "path", Encode.string location.path )
                                                            , ( "range"
                                                              , Encode.object
                                                                    [ ( "start"
                                                                      , Encode.object
                                                                            [ ( "line", Encode.int location.range.start.line )
                                                                            , ( "character", Encode.int location.range.start.character )
                                                                            ]
                                                                      )
                                                                    , ( "end"
                                                                      , Encode.object
                                                                            [ ( "line", Encode.int location.range.end.line )
                                                                            , ( "character", Encode.int location.range.end.character )
                                                                            ]
                                                                      )
                                                                    ]
                                                              )
                                                            ]
                                                    )
                                                    locations
                                                )

                                        Err _ ->
                                            exitWithResponse Encode.null
                                )

                    GetHoverInformationArgs path line character ->
                        LanguageServerProtocol.getHoverInformation path line character
                            |> Task.bind
                                (\result ->
                                    case result of
                                        Ok maybeHover ->
                                            case maybeHover of
                                                Just hover ->
                                                    let
                                                        rangeFields : List ( String, Encode.Value )
                                                        rangeFields =
                                                            case hover.range of
                                                                Just range ->
                                                                    [ ( "range"
                                                                      , Encode.object
                                                                            [ ( "start"
                                                                              , Encode.object
                                                                                    [ ( "line", Encode.int range.start.line )
                                                                                    , ( "character", Encode.int range.start.character )
                                                                                    ]
                                                                              )
                                                                            , ( "end"
                                                                              , Encode.object
                                                                                    [ ( "line", Encode.int range.end.line )
                                                                                    , ( "character", Encode.int range.end.character )
                                                                                    ]
                                                                              )
                                                                            ]
                                                                      )
                                                                    ]

                                                                Nothing ->
                                                                    []
                                                    in
                                                    exitWithResponse
                                                        (Encode.object
                                                            (( "documentation", Encode.string hover.documentation )
                                                                :: rangeFields
                                                            )
                                                        )

                                                Nothing ->
                                                    exitWithResponse Encode.null

                                        Err _ ->
                                            exitWithResponse Encode.null
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
    = InitArgs Bool
    | MakeArgs String Bool Bool Bool
    | FormatArgs String
    | InstallArgs String
    | UninstallArgs String
    | UpgradeArgs
    | DiagnosticsArgs DiagnosticsSource
    | GetDefinitionLocationArgs String Int Int
    | FindReferencesArgs String Int Int
    | GetHoverInformationArgs String Int Int


type DiagnosticsSource
    = DiagnosticsSourceContent String
    | DiagnosticsSourcePath String


argsDecoder : Decode.Decoder Args
argsDecoder =
    Decode.field "command" Decode.string
        |> Decode.andThen
            (\command ->
                case command of
                    "init" ->
                        Decode.map InitArgs
                            (Decode.field "package" Decode.bool)

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

                    "upgrade" ->
                        Decode.succeed UpgradeArgs

                    "diagnostics" ->
                        Decode.map DiagnosticsArgs
                            (Decode.oneOf
                                [ Decode.map DiagnosticsSourceContent (Decode.field "content" Decode.string)
                                , Decode.map DiagnosticsSourcePath (Decode.field "path" Decode.string)
                                ]
                            )

                    "get-definition-location" ->
                        Decode.map3 GetDefinitionLocationArgs
                            (Decode.field "path" Decode.string)
                            (Decode.at [ "position", "line" ] Decode.int)
                            (Decode.at [ "position", "character" ] Decode.int)

                    "find-references" ->
                        Decode.map3 FindReferencesArgs
                            (Decode.field "path" Decode.string)
                            (Decode.at [ "position", "line" ] Decode.int)
                            (Decode.at [ "position", "character" ] Decode.int)

                    "get-hover-information" ->
                        Decode.map3 GetHoverInformationArgs
                            (Decode.field "path" Decode.string)
                            (Decode.at [ "position", "line" ] Decode.int)
                            (Decode.at [ "position", "character" ] Decode.int)

                    _ ->
                        Decode.fail ("Unknown command: " ++ command)
            )
