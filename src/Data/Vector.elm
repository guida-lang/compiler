module Data.Vector exposing
    ( forM_
    , imapM_
    , unsafeFreeze
    , unsafeInit
    , unsafeLast
    )

import Array exposing (Array)
import Data.IORef as IORef exposing (IORef)
import Json.Decode as Decode
import System.IO as IO exposing (IO)
import Utils.Crash exposing (crash)


unsafeLast : Decode.Decoder a -> IORef (Array (Maybe a)) -> IO a
unsafeLast decoder ioRef =
    IORef.readIORef (Decode.array (Decode.maybe decoder)) ioRef
        |> IO.fmap
            (\array ->
                case Array.get (Array.length array - 1) array of
                    Just (Just value) ->
                        value

                    Just Nothing ->
                        crash "Data.Vector.unsafeLast: invalid value"

                    Nothing ->
                        crash "Data.Vector.unsafeLast: empty array"
            )


unsafeInit : IORef (Array (Maybe a)) -> IORef (Array (Maybe a))
unsafeInit =
    identity


imapM_ : Decode.Decoder a -> (Int -> a -> IO b) -> IORef (Array (Maybe a)) -> IO ()
imapM_ decoder action ioRef =
    IORef.readIORef (Decode.array (Decode.maybe decoder)) ioRef
        |> IO.bind
            (\value ->
                Array.foldl
                    (\( i, maybeX ) ioAcc ->
                        case maybeX of
                            Just x ->
                                IO.bind
                                    (\acc ->
                                        IO.fmap (\newX -> Array.push (Just newX) acc)
                                            (action i x)
                                    )
                                    ioAcc

                            Nothing ->
                                ioAcc
                    )
                    (IO.pure Array.empty)
                    (Array.indexedMap Tuple.pair value)
                    |> IO.fmap (\_ -> ())
            )


mapM_ : Decode.Decoder a -> (a -> IO b) -> IORef (Array (Maybe a)) -> IO ()
mapM_ decoder action ioRef =
    imapM_ decoder (\_ -> action) ioRef


forM_ : Decode.Decoder a -> IORef (Array (Maybe a)) -> (a -> IO b) -> IO ()
forM_ decoder ioRef action =
    mapM_ decoder action ioRef


unsafeFreeze : IORef (Array (Maybe a)) -> IO (IORef (Array (Maybe a)))
unsafeFreeze =
    IO.pure
