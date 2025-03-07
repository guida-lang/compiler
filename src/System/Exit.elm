module System.Exit exposing
    ( ExitCode(..)
    , exitFailure
    , exitSuccess
    , exitWith
    )

import System.IO exposing (IO)
import Utils.Impure as Impure


type ExitCode
    = ExitSuccess
    | ExitFailure Int


exitWith : ExitCode -> IO a
exitWith exitCode =
    let
        code : Int
        code =
            case exitCode of
                ExitSuccess ->
                    0

                ExitFailure int ->
                    int
    in
    Impure.task "exitWith"
        []
        (Impure.StringBody (String.fromInt code))
        Impure.Crash


exitFailure : IO a
exitFailure =
    exitWith (ExitFailure 1)


exitSuccess : IO a
exitSuccess =
    exitWith ExitSuccess
