module System.Exit exposing
    ( ExitCode(..)
    , exitFailure
    , exitSuccess
    , exitWith
    )

import Http
import System.IO as IO exposing (IO(..))
import Utils.Crash exposing (crash)


type ExitCode
    = ExitSuccess
    | ExitFailure Int


exitWith : ExitCode -> IO a
exitWith exitCode =
    IO
        (\_ s ->
            let
                code : Int
                code =
                    case exitCode of
                        ExitSuccess ->
                            0

                        ExitFailure int ->
                            int
            in
            ( s
            , IO.ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "exitWith"
                    , body = Http.stringBody "text/plain" (String.fromInt code)
                    , resolver = Http.stringResolver (\_ -> crash "exitWith")
                    , timeout = Nothing
                    }
                )
            )
        )


exitFailure : IO a
exitFailure =
    exitWith (ExitFailure 1)


exitSuccess : IO a
exitSuccess =
    exitWith ExitSuccess
