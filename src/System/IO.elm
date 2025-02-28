module System.IO exposing
    ( Program, Flags, Model, Msg, Next, run
    , IO(..), ION(..), RealWorld, pure, apply, fmap, bind, mapM
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
    , RealWorldMVar, MVarSubscriber(..)
    , writeString
    )

{-| Ref.: <https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html>

@docs Program, Flags, Model, Msg, Next, run


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


# MVars

@docs RealWorldMVar, MVarSubscriber

-}

import Array exposing (Array)
import Cmd.Extra as Cmd
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Utils.Crash exposing (crash)


type alias Flags =
    { args : List String
    , currentDirectory : String
    , envVars : List ( String, String )
    , homedir : FilePath
    , progName : String
    }


type alias Program =
    Platform.Program Flags Model Msg


run : IO () -> Program
run app =
    Platform.worker
        { init =
            \flags ->
                update (PureMsg 0 app)
                    { count = 1
                    , args = flags.args
                    , currentDirectory = flags.currentDirectory
                    , envVars = Dict.fromList flags.envVars
                    , homedir = flags.homedir
                    , progName = flags.progName
                    , state = initialReplState
                    , mVars = Array.empty
                    , next = Dict.empty
                    }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    RealWorld


type Next
    = NewEmptyMVarNext (Int -> IO ())
    | ReadMVarNext (Encode.Value -> IO ())
    | TakeMVarNext (Encode.Value -> IO ())
    | PutMVarNext (() -> IO ())


type Msg
    = PureMsg Int (IO ())
    | NewEmptyMVarMsg Int Int
    | ReadMVarMsg Int Encode.Value
    | PutMVarMsg Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PureMsg index (IO fn) ->
            case fn index model of
                ( newRealWorld, Pure () ) ->
                    ( { newRealWorld | next = Dict.remove index newRealWorld.next }
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
                    ( newRealWorld, Task.perform (PureMsg index) task )

                ( newRealWorld, ForkIO next forkIO ) ->
                    ( { newRealWorld | count = newRealWorld.count + 1 }
                    , Cmd.batch
                        [ Cmd.perform (PureMsg index (next ()))
                        , Cmd.perform (PureMsg newRealWorld.count forkIO)
                        ]
                    )

                -- MVars
                ( newRealWorld, NewEmptyMVar next value ) ->
                    update (NewEmptyMVarMsg index value) { newRealWorld | next = Dict.insert index (NewEmptyMVarNext next) model.next }

                ( newRealWorld, ReadMVar next (Just value) ) ->
                    update (ReadMVarMsg index value) { newRealWorld | next = Dict.insert index (ReadMVarNext next) model.next }

                ( newRealWorld, ReadMVar next Nothing ) ->
                    ( { newRealWorld | next = Dict.insert index (ReadMVarNext next) model.next }, Cmd.none )

                ( newRealWorld, TakeMVar next (Just value) maybePutIndex ) ->
                    update (ReadMVarMsg index value) { newRealWorld | next = Dict.insert index (TakeMVarNext next) model.next }
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, TakeMVar next Nothing maybePutIndex ) ->
                    ( { newRealWorld | next = Dict.insert index (TakeMVarNext next) model.next }, Cmd.none )
                        |> updatePutIndex maybePutIndex

                ( newRealWorld, PutMVar next readIndexes (Just value) ) ->
                    ( { newRealWorld | next = Dict.insert index (PutMVarNext next) model.next }
                    , Cmd.batch
                        (List.foldl (\readIndex -> (::) (Cmd.perform (ReadMVarMsg readIndex value)))
                            [ Cmd.perform (PutMVarMsg index) ]
                            readIndexes
                        )
                    )

                ( newRealWorld, PutMVar next _ Nothing ) ->
                    update (PutMVarMsg index) { newRealWorld | next = Dict.insert index (PutMVarNext next) model.next }

        NewEmptyMVarMsg index value ->
            case Dict.get index model.next of
                Just (NewEmptyMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "NewEmptyMVarMsg"

        ReadMVarMsg index value ->
            case Dict.get index model.next of
                Just (ReadMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                Just (TakeMVarNext fn) ->
                    update (PureMsg index (fn value)) model

                _ ->
                    crash "ReadMVarMsg"

        PutMVarMsg index ->
            case Dict.get index model.next of
                Just (PutMVarNext fn) ->
                    update (PureMsg index (fn ())) model

                _ ->
                    crash "PutMVarMsg"


updatePutIndex : Maybe Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePutIndex maybePutIndex ( model, cmd ) =
    case maybePutIndex of
        Just putIndex ->
            update (PutMVarMsg putIndex) model
                |> Tuple.mapSecond (\putCmd -> Cmd.batch [ cmd, putCmd ])

        Nothing ->
            ( model, cmd )



-- Interal helpers


writeString : FilePath -> String -> IO ()
writeString path content =
    IO
        (\_ s ->
            ( s
            , ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "writeString"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "path", Encode.string path )
                                , ( "content", Encode.string content )
                                ]
                            )
                    , resolver = Http.stringResolver (\_ -> Ok (pure ()))
                    , timeout = Nothing
                    }
                )
            )
        )



-- The IO monad


type IO a
    = IO (Int -> RealWorld -> ( RealWorld, ION a ))


type ION a
    = Pure a
    | ImpureTask (Task Never (IO a))
    | ForkIO (() -> IO a) (IO ())
      -- MVars
    | NewEmptyMVar (Int -> IO a) Int
    | ReadMVar (Encode.Value -> IO a) (Maybe Encode.Value)
    | TakeMVar (Encode.Value -> IO a) (Maybe Encode.Value) (Maybe Int)
    | PutMVar (() -> IO a) (List Int) (Maybe Encode.Value)


type alias RealWorld =
    { count : Int
    , args : List String
    , currentDirectory : String
    , envVars : Dict String String
    , homedir : FilePath
    , progName : String
    , state : ReplState
    , mVars : Array RealWorldMVar
    , next : Dict Int Next
    }


type alias RealWorldMVar =
    { subscribers : List MVarSubscriber
    , value : Maybe Encode.Value
    }


type MVarSubscriber
    = ReadSubscriber Int
    | TakeSubscriber Int
    | PutSubscriber Int Encode.Value


pure : a -> IO a
pure x =
    IO (\_ s -> ( s, Pure x ))


apply : IO a -> IO (a -> b) -> IO b
apply ma mf =
    bind (\f -> bind (pure << f) ma) mf


fmap : (a -> b) -> IO a -> IO b
fmap fn ma =
    bind (pure << fn) ma


bind : (a -> IO b) -> IO a -> IO b
bind f (IO ma) =
    IO
        (\index s0 ->
            case ma index s0 of
                ( s1, Pure a ) ->
                    unIO (f a) index s1

                ( s1, ImpureTask task ) ->
                    ( s1, ImpureTask (Task.map (bind f) task) )

                ( s1, ForkIO next forkIO ) ->
                    ( s1, ForkIO (\() -> bind f (next ())) forkIO )

                ( s1, NewEmptyMVar next newValue ) ->
                    ( s1, NewEmptyMVar (\value -> bind f (next value)) newValue )

                ( s1, ReadMVar next mVarValue ) ->
                    ( s1, ReadMVar (\value -> bind f (next value)) mVarValue )

                ( s1, TakeMVar next mVarValue maybePutIndex ) ->
                    ( s1, TakeMVar (\value -> bind f (next value)) mVarValue maybePutIndex )

                ( s1, PutMVar next readIndexes value ) ->
                    ( s1, PutMVar (\() -> bind f (next ())) readIndexes value )
        )


unIO : IO a -> (Int -> RealWorld -> ( RealWorld, ION a ))
unIO (IO a) =
    a


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
withFile path mode callback =
    IO
        (\_ s ->
            ( s
            , ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "withFile"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "path", Encode.string path )
                                , ( "mode"
                                  , Encode.string
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
                                  )
                                ]
                            )
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ body ->
                                        case Decode.decodeString Decode.int body of
                                            Ok fd ->
                                                Ok (callback (Handle fd))

                                            Err _ ->
                                                crash "withFile"

                                    _ ->
                                        crash "withFile"
                            )
                    , timeout = Nothing
                    }
                )
            )
        )


type IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode



-- Closing files


hClose : Handle -> IO ()
hClose (Handle handle) =
    IO
        (\_ s ->
            ( s
            , ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "hFileSize"
                    , body = Http.stringBody "text/plain" (String.fromInt handle)
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ _ ->
                                        Ok (pure ())

                                    _ ->
                                        crash "hClose"
                            )
                    , timeout = Nothing
                    }
                )
            )
        )



-- File locking


hFileSize : Handle -> IO Int
hFileSize (Handle handle) =
    IO
        (\_ s ->
            ( s
            , ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "hFileSize"
                    , body = Http.stringBody "text/plain" (String.fromInt handle)
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ body ->
                                        case Decode.decodeString Decode.int body of
                                            Ok fd ->
                                                Ok (pure fd)

                                            Err _ ->
                                                crash "hFileSize"

                                    _ ->
                                        crash "hFileSize"
                            )
                    , timeout = Nothing
                    }
                )
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
hPutStr (Handle fd) content =
    IO
        (\_ s ->
            ( s
            , ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "hPutStr"
                    , body =
                        Http.jsonBody
                            (Encode.object
                                [ ( "fd", Encode.int fd )
                                , ( "content", Encode.string content )
                                ]
                            )
                    , resolver = Http.stringResolver (\_ -> Ok (pure ()))
                    , timeout = Nothing
                    }
                )
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
getLine =
    IO
        (\_ s ->
            ( s
            , ImpureTask
                (Http.task
                    { method = "POST"
                    , headers = []
                    , url = "getLine"
                    , body = Http.emptyBody
                    , resolver =
                        Http.stringResolver
                            (\response ->
                                case response of
                                    Http.GoodStatus_ _ body ->
                                        Ok (pure body)

                                    _ ->
                                        Ok (pure "")
                            )
                    , timeout = Nothing
                    }
                )
            )
        )



-- Repl State (Terminal.Repl)


type ReplState
    = ReplState (Dict String String) (Dict String String) (Dict String String)


initialReplState : ReplState
initialReplState =
    ReplState Dict.empty Dict.empty Dict.empty
