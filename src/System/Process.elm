module System.Process exposing
    ( CmdSpec
    , CreateProcess
    , ProcessHandle
    , StdStream(..)
    , proc
    , waitForProcess
    , withCreateProcess
    )

import Json.Decode as Decode
import Json.Encode as Encode
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Utils.Impure as Impure
import Utils.Task.Extra as Task


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


withCreateProcess : CreateProcess -> (Maybe IO.Handle -> Maybe IO.Handle -> Maybe IO.Handle -> ProcessHandle -> Task Never Exit.ExitCode) -> Task Never Exit.ExitCode
withCreateProcess createProcess f =
    Impure.task "withCreateProcess"
        []
        (Impure.JsonBody
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
        )
        (Impure.DecoderResolver
            (Decode.map2 Tuple.pair
                (Decode.field "stdinHandle" (Decode.maybe Decode.int))
                (Decode.field "ph" Decode.int)
            )
        )
        |> Task.bind
            (\( stdinHandle, ph ) ->
                f (Maybe.map IO.Handle stdinHandle) Nothing Nothing (ProcessHandle ph)
            )


waitForProcess : ProcessHandle -> Task Never Exit.ExitCode
waitForProcess (ProcessHandle ph) =
    Impure.task "waitForProcess"
        []
        (Impure.StringBody (String.fromInt ph))
        (Impure.DecoderResolver
            (Decode.map
                (\int ->
                    if int == 0 then
                        Exit.ExitSuccess

                    else
                        Exit.ExitFailure int
                )
                Decode.int
            )
        )
