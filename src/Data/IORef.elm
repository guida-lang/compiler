module Data.IORef exposing
    ( IORef
    , ioRefDecoder
    , ioRefEncoder
    , modifyIORef
    , newIORef
    , readIORef
    , writeIORef
    )

import Array
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO(..))
import Utils.Crash exposing (crash)


type IORef a
    = IORef Int


ioRefEncoder : IORef a -> Encode.Value
ioRefEncoder (IORef value) =
    Encode.int value


ioRefDecoder : Decode.Decoder (IORef a)
ioRefDecoder =
    Decode.map IORef Decode.int


newIORef : (a -> Encode.Value) -> a -> IO (IORef a)
newIORef encoder value =
    IO (\s -> ( { s | ioRefs = Array.push (encoder value) s.ioRefs }, IO.Pure (IORef (Array.length s.ioRefs)) ))


readIORef : Decode.Decoder a -> IORef a -> IO a
readIORef decoder (IORef ref) =
    IO
        (\s ->
            case Maybe.andThen (Decode.decodeValue decoder >> Result.toMaybe) (Array.get ref s.ioRefs) of
                Just value ->
                    ( s, IO.Pure value )

                Nothing ->
                    crash "Data.IORef.readIORef: could not find entry"
        )


writeIORef : (b -> Encode.Value) -> IORef a -> b -> IO ()
writeIORef encoder (IORef ref) value =
    IO (\s -> ( { s | ioRefs = Array.set ref (encoder value) s.ioRefs }, IO.Pure () ))


modifyIORef : Decode.Decoder a -> (a -> Encode.Value) -> IORef a -> (a -> a) -> IO ()
modifyIORef decoder encoder ioRef func =
    readIORef decoder ioRef
        |> IO.bind (\value -> writeIORef encoder ioRef (func value))
