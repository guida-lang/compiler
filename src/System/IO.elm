module System.IO exposing
    ( Program, Model, Msg, run
    , IO, pure, apply, fmap, bind, mapM
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


# The IO monad

@docs IO, pure, apply, fmap, bind, mapM


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


run : IO () -> Program
run app =
    Platform.worker
        { init =
            \() ->
                update
                    (bind
                        (\() ->
                            Impure.task "exitWith"
                                []
                                (Impure.StringBody "0")
                                Impure.Crash
                        )
                        app
                    )
                    ()
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    ()


type alias Msg =
    IO ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg () =
    ( (), Task.perform Task.succeed msg )



-- Interal helpers


writeString : FilePath -> String -> IO ()
writeString path content =
    Impure.task "writeString"
        [ Http.header "path" path ]
        (Impure.StringBody content)
        (Impure.Always ())



-- The IO monad


type alias IO a =
    Task Never a


pure : a -> IO a
pure =
    Task.succeed


apply : IO a -> IO (a -> b) -> IO b
apply ma mf =
    bind (\f -> bind (pure << f) ma) mf


fmap : (a -> b) -> IO a -> IO b
fmap =
    Task.map


bind : (a -> IO b) -> IO a -> IO b
bind =
    Task.andThen


mapM : (a -> IO b) -> List a -> IO (List b)
mapM f =
    List.map f >> Task.sequence



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


withFile : String -> IOMode -> (Handle -> IO a) -> IO a
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


hClose : Handle -> IO ()
hClose (Handle handle) =
    Impure.task "hClose" [] (Impure.StringBody (String.fromInt handle)) (Impure.Always ())



-- File locking


hFileSize : Handle -> IO Int
hFileSize (Handle handle) =
    Impure.task "hFileSize"
        []
        (Impure.StringBody (String.fromInt handle))
        (Impure.DecoderResolver Decode.int)



-- Buffering operations


hFlush : Handle -> IO ()
hFlush _ =
    pure ()



-- Terminal operations (not portable: GHC only)


hIsTerminalDevice : Handle -> IO Bool
hIsTerminalDevice _ =
    pure True



-- Text output


hPutStr : Handle -> String -> IO ()
hPutStr (Handle fd) content =
    Impure.task "hPutStr"
        [ Http.header "fd" (String.fromInt fd) ]
        (Impure.StringBody content)
        (Impure.Always ())


hPutStrLn : Handle -> String -> IO ()
hPutStrLn handle content =
    hPutStr handle (content ++ "\n")



-- Special cases for standard input and output


putStr : String -> IO ()
putStr =
    hPutStr stdout


putStrLn : String -> IO ()
putStrLn s =
    putStr (s ++ "\n")


getLine : IO String
getLine =
    Impure.task "getLine" [] Impure.EmptyBody (Impure.StringResolver identity)



-- Repl State (Terminal.Repl)


type ReplState
    = ReplState (Dict String String) (Dict String String) (Dict String String)


initialReplState : ReplState
initialReplState =
    ReplState Dict.empty Dict.empty Dict.empty
