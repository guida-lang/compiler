module System.Exit exposing
    ( ExitCode(..)
    , exitFailure
    , exitSuccess
    , exitWith
    )

import Task exposing (Task)
import Utils.Impure as Impure


type ExitCode
    = ExitSuccess
    | ExitFailure Int


exitWith : ExitCode -> Task Never a
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


exitFailure : Task Never a
exitFailure =
    exitWith (ExitFailure 1)


exitSuccess : Task Never a
exitSuccess =
    exitWith ExitSuccess
