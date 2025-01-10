module System.Exit exposing
    ( ExitCode(..)
    , exitFailure
    , exitSuccess
    , exitWith
    )

import System.IO as IO exposing (IO(..))


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
            ( s, IO.ExitWith IO.pure code )
        )


exitFailure : IO a
exitFailure =
    exitWith (ExitFailure 1)


exitSuccess : IO a
exitSuccess =
    exitWith ExitSuccess
