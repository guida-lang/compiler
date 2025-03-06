module System.IO exposing
    ( Program, Model, Msg, run
    , IO, ION(..), RealWorld, pure, apply, fmap, bind, mapM
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

@docs IO, ION, RealWorld, pure, apply, fmap, bind, mapM


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

import Cmd.Extra as Cmd
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Task exposing (Task)
import Utils.Crash exposing (crash)
import Utils.Impure as Impure


type alias Program =
    Platform.Program () Model Msg


run : IO () -> Program
run app =
    Platform.worker
        { init =
            \() ->
                update ( 0, app )
                    { count = 1
                    , state = initialReplState
                    }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    RealWorld


type alias Msg =
    ( Int, IO () )


update : Msg -> Model -> ( Model, Cmd Msg )
update ( index, fn ) model =
    case fn model of
        ( newRealWorld, Pure () ) ->
            ( newRealWorld
            , if index == 0 then
                Http.post
                    { url = "exitWith"
                    , body = Http.stringBody "text/plain" "0"
                    , expect = Http.expectString (\_ -> crash "exitWith")
                    }

              else
                Cmd.none
            )

        ( newRealWorld, ImpureTask task ) ->
            ( newRealWorld, Task.perform (Tuple.pair index) task )

        ( newRealWorld, ForkIO next forkIO ) ->
            ( { newRealWorld | count = newRealWorld.count + 1 }
            , Cmd.batch
                [ Cmd.perform ( index, next () )
                , Cmd.perform ( newRealWorld.count, forkIO )
                ]
            )



-- Interal helpers


writeString : FilePath -> String -> IO ()
writeString path content s =
    ( s
    , ImpureTask
        (Impure.task "writeString"
            [ Http.header "path" path ]
            (Impure.StringBody content)
            (Impure.Always (pure ()))
        )
    )



-- The IO monad


type alias IO a =
    RealWorld -> ( RealWorld, ION a )


type ION a
    = Pure a
    | ImpureTask (Task Never (IO a))
    | ForkIO (() -> IO a) (IO ())


type alias RealWorld =
    { count : Int
    , state : ReplState
    }


pure : a -> IO a
pure x s =
    ( s, Pure x )


apply : IO a -> IO (a -> b) -> IO b
apply ma mf =
    bind (\f -> bind (pure << f) ma) mf


fmap : (a -> b) -> IO a -> IO b
fmap fn ma =
    bind (pure << fn) ma


bind : (a -> IO b) -> IO a -> IO b
bind f ma s0 =
    case ma s0 of
        ( s1, Pure a ) ->
            f a s1

        ( s1, ImpureTask task ) ->
            ( s1, ImpureTask (Task.map (bind f) task) )

        ( s1, ForkIO next forkIO ) ->
            ( s1, ForkIO (\() -> bind f (next ())) forkIO )


mapM : (a -> IO b) -> List a -> IO (List b)
mapM f =
    List.foldr (\a -> bind (\c -> fmap (\va -> va :: c) (f a)))
        (pure [])



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
withFile path mode callback s =
    ( s
    , ImpureTask
        (Impure.task "withFile"
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
            (Impure.DecoderResolver (Decode.map (callback << Handle) Decode.int))
        )
    )


type IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode



-- Closing files


hClose : Handle -> IO ()
hClose (Handle handle) s =
    ( s, ImpureTask (Impure.task "hClose" [] (Impure.StringBody (String.fromInt handle)) (Impure.Always (pure ()))) )



-- File locking


hFileSize : Handle -> IO Int
hFileSize (Handle handle) s =
    ( s
    , ImpureTask
        (Impure.task "hFileSize"
            []
            (Impure.StringBody (String.fromInt handle))
            (Impure.DecoderResolver (Decode.map pure Decode.int))
        )
    )



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
hPutStr (Handle fd) content s =
    ( s
    , ImpureTask
        (Impure.task "hPutStr"
            [ Http.header "fd" (String.fromInt fd) ]
            (Impure.StringBody content)
            (Impure.Always (pure ()))
        )
    )


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
getLine s =
    ( s, ImpureTask (Impure.task "getLine" [] Impure.EmptyBody (Impure.StringResolver pure)) )



-- Repl State (Terminal.Repl)


type ReplState
    = ReplState (Dict String String) (Dict String String) (Dict String String)


initialReplState : ReplState
initialReplState =
    ReplState Dict.empty Dict.empty Dict.empty
