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
import System.TypeCheck.IO as IO exposing (IO, Variable)
import Utils.Crash exposing (crash)


length : IORef (Array (Maybe (List Variable))) -> IO Int
length =
    IORef.readIORefMVector
        >> IO.fmap Array.length


replicate : Int -> List Variable -> IO (IORef (Array (Maybe (List Variable))))
replicate n e =
    IORef.newIORefMVector (Array.repeat n (Just e))


grow : IORef (Array (Maybe (List Variable))) -> Int -> IO (IORef (Array (Maybe (List Variable))))
grow ioRef length_ =
    IORef.readIORefMVector ioRef
        |> IO.bind
            (\value ->
                IORef.writeIORefMVector ioRef
                    (Array.append value (Array.repeat length_ Nothing))
            )
        |> IO.fmap (\_ -> ioRef)


read : IORef (Array (Maybe (List Variable))) -> Int -> IO (List Variable)
read ioRef i =
    IORef.readIORefMVector ioRef
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


write : IORef (Array (Maybe (List Variable))) -> Int -> List Variable -> IO ()
write ioRef i x =
    IORef.modifyIORefMVector ioRef
        (Array.set i (Just x))


modify : IORef (Array (Maybe (List Variable))) -> (List Variable -> List Variable) -> Int -> IO ()
modify ioRef func index =
    IORef.modifyIORefMVector ioRef
        (Array.update index (Maybe.map func))
