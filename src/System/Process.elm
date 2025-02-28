module System.Process exposing
    ( CmdSpec
    , CreateProcess
    , ProcessHandle
    , StdStream(..)
    , proc
    , waitForProcess
    , withCreateProcess
    )

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import System.Exit as Exit
import System.IO as IO exposing (IO(..))
import Utils.Crash exposing (crash)


type CmdSpec
    = RawCommand String (List String)


type alias CreateProcess =
    { cmdspec : CmdSpec
    , std_in : StdStream
    , std_out : StdStream
    , std_err : StdStream
    }


type StdStream
    = Inherit
    | UseHandle IO.Handle
    | CreatePipe
    | NoStream


type ProcessHandle
    = ProcessHandle Int


proc : String -> List String -> CreateProcess
proc cmd args =
    { cmdspec = RawCommand cmd args
    , std_in = Inherit
    , std_out = Inherit
    , std_err = Inherit
    }


withCreateProcess : CreateProcess -> (Maybe IO.Handle -> Maybe IO.Handle -> Maybe IO.Handle -> ProcessHandle -> IO Exit.ExitCode) -> IO Exit.ExitCode
withCreateProcess createProcess f =
    IO
        (\_ s ->
            ( s
            , IO.ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "withCreateProcess"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "cmdspec"
                                  , case createProcess.cmdspec of
                                        RawCommand cmd args ->
                                            Encode.object
                                                [ ( "type", Encode.string "RawCommand" )
                                                , ( "cmd", Encode.string cmd )
                                                , ( "args", Encode.list Encode.string args )
                                                ]
                                  )
                                , ( "stdin"
                                  , case createProcess.std_in of
                                        Inherit ->
                                            Encode.string "inherit"

                                        UseHandle (IO.Handle handle) ->
                                            Encode.int handle

                                        CreatePipe ->
                                            Encode.string "pipe"

                                        NoStream ->
                                            Encode.string "ignore"
                                  )
                                , ( "stdout"
                                  , case createProcess.std_out of
                                        Inherit ->
                                            Encode.string "inherit"

                                        UseHandle (IO.Handle handle) ->
                                            Encode.int handle

                                        CreatePipe ->
                                            Encode.string "pipe"

                                        NoStream ->
                                            Encode.string "ignore"
                                  )
                                , ( "stderr"
                                  , case createProcess.std_err of
                                        Inherit ->
                                            Encode.string "inherit"

                                        UseHandle (IO.Handle handle) ->
                                            Encode.int handle

                                        CreatePipe ->
                                            Encode.string "pipe"

                                        NoStream ->
                                            Encode.string "ignore"
                                  )
                                ]
                            )
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ body ->
                                        case Decode.decodeString (Decode.map2 Tuple.pair (Decode.field "stdinHandle" (Decode.maybe Decode.int)) (Decode.field "ph" Decode.int)) body of
                                            Ok ( stdinHandle, ph ) ->
                                                Ok (f (Maybe.map IO.Handle stdinHandle) Nothing Nothing (ProcessHandle ph))

                                            Err _ ->
                                                crash "withFile"

                                    _ ->
                                        crash "withFile"
                            )
                    , timeout = Nothing
                    }
                )
            )
        )


waitForProcess : ProcessHandle -> IO Exit.ExitCode
waitForProcess (ProcessHandle ph) =
    IO
        (\_ s ->
            ( s
            , IO.ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "waitForProcess"
                    , body =
                        Http.stringBody "text/plain" (String.fromInt ph)
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ body ->
                                        case Decode.decodeString Decode.int body of
                                            Ok 0 ->
                                                Ok (IO.pure Exit.ExitSuccess)

                                            Ok int ->
                                                Ok (IO.pure (Exit.ExitFailure int))

                                            Err _ ->
                                                crash "waitForProcess"

                                    _ ->
                                        crash "waitForProcess"
                            )
                    , timeout = Nothing
                    }
                )
            )
        )
