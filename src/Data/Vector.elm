module Data.Vector exposing
    ( forM_
    , imapM_
    , unsafeFreeze
    , unsafeInit
    , unsafeLast
    )

import Array exposing (Array)
import Data.IORef as IORef exposing (IORef)
import System.TypeCheck.IO as IO exposing (IO, Variable)
import Utils.Crash exposing (crash)


unsafeLast : IORef (Array (Maybe (List Variable))) -> IO (List Variable)
unsafeLast ioRef =
    IORef.readIORefMVector ioRef
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


imapM_ : (Int -> List Variable -> IO b) -> IORef (Array (Maybe (List IO.Variable))) -> IO ()
imapM_ action ioRef =
    IORef.readIORefMVector ioRef
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


mapM_ : (List IO.Variable -> IO b) -> IORef (Array (Maybe (List IO.Variable))) -> IO ()
mapM_ action ioRef =
    imapM_ (\_ -> action) ioRef


forM_ : IORef (Array (Maybe (List IO.Variable))) -> (List IO.Variable -> IO b) -> IO ()
forM_ ioRef action =
    mapM_ action ioRef


unsafeFreeze : IORef (Array (Maybe a)) -> IO (IORef (Array (Maybe a)))
unsafeFreeze =
    IO.pure
