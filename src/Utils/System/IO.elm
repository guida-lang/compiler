module Utils.System.IO exposing
    ( Program, Model, Msg, run
    , FilePath
    , hFlush
    , hIsTerminalDevice
    , ReplState(..), initialReplState
    , writeString
    )

{-| Ref.: <https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html>

@docs Program, Model, Msg, run


# Files and handles

@docs FilePath


# Buffering operations

@docs hFlush


# Terminal operations (not portable: GHC only)

@docs hIsTerminalDevice


# Repl State

@docs ReplState, initialReplState


# Internal helpers

@docs writeString

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import System.Exit as Exit
import System.IO as IO
import System.Misc as Misc
import Task exposing (Task)
import Utils.Impure as Impure


type alias Program =
    Platform.Program () Model Msg


run : Task Exit.ExitCode () -> Program
run app =
    Platform.worker
        { init = update (TaskMsg app)
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    ()


type Msg
    = TaskMsg (Task Exit.ExitCode ())
    | ExitMsg (Result Exit.ExitCode ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg () =
    case msg of
        TaskMsg task ->
            ( (), Task.attempt ExitMsg task )

        ExitMsg (Ok ()) ->
            ( (), Exit.exitSuccess )

        ExitMsg (Err exitCode) ->
            ( (), Exit.exitWith exitCode )



-- Interal helpers


writeString : FilePath -> String -> Task Never ()
writeString path content =
    Misc.writeString path content



-- Files and handles


type alias FilePath =
    String



-- Buffering operations


hFlush : IO.Handle -> Task Never ()
hFlush _ =
    Task.succeed ()



-- Terminal operations (not portable: GHC only)


hIsTerminalDevice : IO.Handle -> Task Never Bool
hIsTerminalDevice _ =
    Task.succeed True



-- Repl State (Terminal.Repl)


type ReplState
    = ReplState (Dict String String) (Dict String String) (Dict String String)


initialReplState : ReplState
initialReplState =
    ReplState Dict.empty Dict.empty Dict.empty
