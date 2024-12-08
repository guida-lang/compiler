module Data.IORef exposing
    ( IORef(..)
    , modifyIORefDescriptor
    , modifyIORefMVector
    , newIORefDescriptor
    , newIORefMVector
    , newIORefPointInfo
    , newIORefWeight
    , readIORefDescriptor
    , readIORefMVector
    , readIORefPointInfo
    , readIORefWeight
    , writeIORefDescriptor
    , writeIORefMVector
    , writeIORefPointInfo
    , writeIORefWeight
    )

import Array exposing (Array)
import System.TypeCheck.IO as IO exposing (IO)
import Utils.Crash exposing (crash)


type IORef a
    = IORef Int


newIORefWeight : Int -> IO (IORef Int)
newIORefWeight value s =
    ( { s | ioRefsWeight = Array.push value s.ioRefsWeight }, IORef (Array.length s.ioRefsWeight) )


newIORefPointInfo : IO.PointInfo -> IO (IORef IO.PointInfo)
newIORefPointInfo value s =
    ( { s | ioRefsPointInfo = Array.push value s.ioRefsPointInfo }, IORef (Array.length s.ioRefsPointInfo) )


newIORefDescriptor : IO.Descriptor -> IO (IORef IO.Descriptor)
newIORefDescriptor value s =
    ( { s | ioRefsDescriptor = Array.push value s.ioRefsDescriptor }, IORef (Array.length s.ioRefsDescriptor) )


newIORefMVector : Array (Maybe (List IO.Variable)) -> IO (IORef (Array (Maybe (List IO.Variable))))
newIORefMVector value s =
    ( { s | ioRefsMVector = Array.push value s.ioRefsMVector }, IORef (Array.length s.ioRefsMVector) )


readIORefWeight : IORef Int -> IO Int
readIORefWeight (IORef ref) s =
    case Array.get ref s.ioRefsWeight of
        Just value ->
            ( s, value )

        Nothing ->
            crash "Data.IORef.readIORefWeight: could not find entry"


readIORefPointInfo : IORef IO.PointInfo -> IO IO.PointInfo
readIORefPointInfo (IORef ref) s =
    case Array.get ref s.ioRefsPointInfo of
        Just value ->
            ( s, value )

        Nothing ->
            crash "Data.IORef.readIORefPointInfo: could not find entry"


readIORefDescriptor : IORef IO.Descriptor -> IO IO.Descriptor
readIORefDescriptor (IORef ref) s =
    case Array.get ref s.ioRefsDescriptor of
        Just value ->
            ( s, value )

        Nothing ->
            crash "Data.IORef.readIORefDescriptor: could not find entry"


readIORefMVector : IORef (Array (Maybe (List IO.Variable))) -> IO (Array (Maybe (List IO.Variable)))
readIORefMVector (IORef ref) s =
    case Array.get ref s.ioRefsMVector of
        Just value ->
            ( s, value )

        Nothing ->
            crash "Data.IORef.readIORefDescriptor: could not find entry"


writeIORefWeight : IORef Int -> Int -> IO ()
writeIORefWeight (IORef ref) value s =
    ( { s | ioRefsWeight = Array.set ref value s.ioRefsWeight }, () )


writeIORefPointInfo : IORef IO.PointInfo -> IO.PointInfo -> IO ()
writeIORefPointInfo (IORef ref) value s =
    ( { s | ioRefsPointInfo = Array.set ref value s.ioRefsPointInfo }, () )


writeIORefDescriptor : IORef IO.Descriptor -> IO.Descriptor -> IO ()
writeIORefDescriptor (IORef ref) value s =
    ( { s | ioRefsDescriptor = Array.set ref value s.ioRefsDescriptor }, () )


writeIORefMVector : IORef (Array (Maybe (List IO.Variable))) -> Array (Maybe (List IO.Variable)) -> IO ()
writeIORefMVector (IORef ref) value s =
    ( { s | ioRefsMVector = Array.set ref value s.ioRefsMVector }, () )


modifyIORefDescriptor : IORef IO.Descriptor -> (IO.Descriptor -> IO.Descriptor) -> IO ()
modifyIORefDescriptor ioRef func =
    readIORefDescriptor ioRef
        |> IO.bind (\value -> writeIORefDescriptor ioRef (func value))


modifyIORefMVector : IORef (Array (Maybe (List IO.Variable))) -> (Array (Maybe (List IO.Variable)) -> Array (Maybe (List IO.Variable))) -> IO ()
modifyIORefMVector ioRef func =
    readIORefMVector ioRef
        |> IO.bind (\value -> writeIORefMVector ioRef (func value))
