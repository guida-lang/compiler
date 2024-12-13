module System.Process exposing
    ( CmdSpec
    , CreateProcess
    , ProcessHandle
    , StdStream(..)
    , proc
    , waitForProcess
    , withCreateProcess
    )

import Json.Encode as Encode
import System.Exit as Exit
import System.IO as IO exposing (IO(..))


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
            , IO.ProcWithCreateProcess IO.pure
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
        )
        |> IO.bind
            (\{ stdinHandle, ph } ->
                f (Maybe.map IO.Handle stdinHandle) Nothing Nothing (ProcessHandle ph)
            )


waitForProcess : ProcessHandle -> IO Exit.ExitCode
waitForProcess (ProcessHandle ph) =
    IO (\_ s -> ( s, IO.ProcWaitForProcess IO.pure ph ))
        |> IO.fmap
            (\exitCode ->
                case exitCode of
                    0 ->
                        Exit.ExitSuccess

                    int ->
                        Exit.ExitFailure int
            )
