module System.IO exposing
    ( Program, Model, Msg, run
    , FilePath, Handle(..)
    , stdout, stderr
    , withFile, IOMode(..)
    , hClose
    , hFileSize
    , hFlush
    , hIsTerminalDevice
    , hPutStr, hPutStrLn
    , putStr, putStrLn, getLine
    , ReplState(..), initialReplState
    , writeString
    )

{-| Ref.: <https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html>

@docs Program, Model, Msg, run


# Files and handles

@docs FilePath, Handle


# Standard handles

@docs stdout, stderr


# Opening files

@docs withFile, IOMode


# Closing files

@docs hClose


# File locking

@docs hFileSize


# Buffering operations

@docs hFlush


# Terminal operations (not portable: GHC only)

@docs hIsTerminalDevice


# Text output

@docs hPutStr, hPutStrLn


# Special cases for standard input and output

@docs putStr, putStrLn, getLine


# Repl State

@docs ReplState, initialReplState


# Internal helpers

@docs writeString

-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Task exposing (Task)
import Utils.Impure as Impure


type alias Program =
    Platform.Program () Model Msg


run : Task Never () -> Program
run app =
    Platform.worker
        { init = update app
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    ()


type alias Msg =
    Task Never ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg () =
    ( (), Task.perform Task.succeed msg )



-- Interal helpers


writeString : FilePath -> String -> Task Never ()
writeString path content =
    Impure.task "writeString"
        [ Http.header "path" path ]
        (Impure.StringBody content)
        (Impure.Always ())



-- Task extra


pure : a -> Task x a
pure =
    Task.succeed



-- Files and handles


type alias FilePath =
    String


type Handle
    = Handle Int



-- Standard handles


stdout : Handle
stdout =
    Handle 1


stderr : Handle
stderr =
    Handle 2



-- Opening files


withFile : String -> IOMode -> (Handle -> Task Never a) -> Task Never a
withFile path mode callback =
    Impure.task "withFile"
        [ Http.header "mode"
            (case mode of
                ReadMode ->
                    "r"

                WriteMode ->
                    "w"

                AppendMode ->
                    "a"

                ReadWriteMode ->
                    "w+"
            )
        ]
        (Impure.StringBody path)
        (Impure.DecoderResolver (Decode.map Handle Decode.int))
        |> Task.andThen callback


type IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode



-- Closing files


hClose : Handle -> Task Never ()
hClose (Handle handle) =
    Impure.task "hClose" [] (Impure.StringBody (String.fromInt handle)) (Impure.Always ())



-- File locking


hFileSize : Handle -> Task Never Int
hFileSize (Handle handle) =
    Impure.task "hFileSize"
        []
        (Impure.StringBody (String.fromInt handle))
        (Impure.DecoderResolver Decode.int)



-- Buffering operations


hFlush : Handle -> Task Never ()
hFlush _ =
    pure ()



-- Terminal operations (not portable: GHC only)


hIsTerminalDevice : Handle -> Task Never Bool
hIsTerminalDevice _ =
    pure True



-- Text output


hPutStr : Handle -> String -> Task Never ()
hPutStr (Handle fd) content =
    Impure.task "hPutStr"
        [ Http.header "fd" (String.fromInt fd) ]
        (Impure.StringBody content)
        (Impure.Always ())


hPutStrLn : Handle -> String -> Task Never ()
hPutStrLn handle content =
    hPutStr handle (content ++ "\n")



-- Special cases for standard input and output


putStr : String -> Task Never ()
putStr =
    hPutStr stdout


putStrLn : String -> Task Never ()
putStrLn s =
    putStr (s ++ "\n")


getLine : Task Never String
getLine =
    Impure.task "getLine" [] Impure.EmptyBody (Impure.StringResolver identity)



-- Repl State (Terminal.Repl)


type ReplState
    = ReplState (Dict String String) (Dict String String) (Dict String String)


initialReplState : ReplState
initialReplState =
    ReplState Dict.empty Dict.empty Dict.empty
