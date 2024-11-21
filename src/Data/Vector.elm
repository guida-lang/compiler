module Data.Vector exposing
    ( forM_
    , imapM_
    )

import Array exposing (Array)
import Data.IORef as IORef exposing (IORef)
import Json.Decode as Decode
import System.IO as IO exposing (IO)


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


forM_ : Decode.Decoder a -> IORef (Array (Maybe a)) -> (a -> IO b) -> IO ()
forM_ decoder ioRef action =
    -- vectorMapM_ decoder action ioRef
    Debug.todo "forM_"
