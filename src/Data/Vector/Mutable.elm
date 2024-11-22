module Data.Vector.Mutable exposing
    ( grow
    , length
    , modify
    , read
    , replicate
    , write
    )

import Array exposing (Array)
import Array.Extra as Array
import Data.IORef as IORef exposing (IORef)
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO)
import Utils.Crash exposing (crash)


length : IORef (Array (Maybe a)) -> IO Int
length =
    IORef.readIORef (Decode.array (Decode.succeed Nothing))
        >> IO.fmap Array.length


replicate : (a -> Encode.Value) -> Int -> a -> IO (IORef (Array (Maybe a)))
replicate encoder n e =
    IORef.newIORef
        (Encode.array
            (Maybe.map encoder
                >> Maybe.withDefault Encode.null
            )
        )
        (Array.repeat n (Just e))


grow : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> Int -> IO (IORef (Array (Maybe a)))
grow decoder encoder ioRef length_ =
    IORef.readIORef (Decode.array (Decode.maybe decoder)) ioRef
        |> IO.bind
            (\value ->
                IORef.writeIORef
                    (Encode.array
                        (Maybe.map encoder
                            >> Maybe.withDefault Encode.null
                        )
                    )
                    ioRef
                    (Array.append value (Array.repeat length_ Nothing))
            )
        |> IO.fmap (\_ -> ioRef)


read : Decode.Decoder a -> IORef (Array (Maybe a)) -> Int -> IO a
read decoder ioRef i =
    IORef.readIORef (Decode.array (Decode.maybe decoder)) ioRef
        |> IO.fmap
            (\array ->
                case Array.get i array of
                    Just (Just value) ->
                        value

                    Just Nothing ->
                        crash "Data.Vector.read: invalid value"

                    Nothing ->
                        crash "Data.Vector.read: could not find entry"
            )


write : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> Int -> a -> IO ()
write decoder encoder ioRef i x =
    IORef.modifyIORef (Decode.array (Decode.maybe decoder))
        (Encode.array
            (Maybe.map encoder
                >> Maybe.withDefault Encode.null
            )
        )
        ioRef
        (Array.set i (Just x))


modify : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> (a -> a) -> Int -> IO ()
modify decoder encoder ioRef func index =
    IORef.modifyIORef (Decode.array (Decode.maybe decoder))
        (Encode.array
            (Maybe.map encoder
                >> Maybe.withDefault Encode.null
            )
        )
        ioRef
        (Array.update index (Maybe.map func))
