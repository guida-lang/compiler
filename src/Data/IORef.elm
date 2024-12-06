module Data.IORef exposing
    ( IORef(..)
    , ioRefDecoder
    , ioRefEncoder
    , modifyIORef
    , modifyIORefDescriptor
    , modifyIORefMVector
    , newIORef
    , newIORefDescriptor
    , newIORefMVector
    , newIORefPointInfo
    , newIORefWeight
    , readIORef
    , readIORefDescriptor
    , readIORefMVector
    , readIORefPointInfo
    , readIORefWeight
    , writeIORef
    , writeIORefDescriptor
    , writeIORefMVector
    , writeIORefPointInfo
    , writeIORefWeight
    )

import Array exposing (Array)
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


newIORefWeight : Int -> IO (IORef Int)
newIORefWeight value =
    IO (\s -> ( { s | ioRefsWeight = Array.push value s.ioRefsWeight }, IO.Pure (IORef (Array.length s.ioRefsWeight)) ))


newIORefPointInfo : IO.PointInfo -> IO (IORef IO.PointInfo)
newIORefPointInfo value =
    IO (\s -> ( { s | ioRefsPointInfo = Array.push value s.ioRefsPointInfo }, IO.Pure (IORef (Array.length s.ioRefsPointInfo)) ))


newIORefDescriptor : IO.Descriptor -> IO (IORef IO.Descriptor)
newIORefDescriptor value =
    IO (\s -> ( { s | ioRefsDescriptor = Array.push value s.ioRefsDescriptor }, IO.Pure (IORef (Array.length s.ioRefsDescriptor)) ))


newIORefMVector : Array (Maybe (List IO.Variable)) -> IO (IORef (Array (Maybe (List IO.Variable))))
newIORefMVector value =
    IO (\s -> ( { s | ioRefsMVector = Array.push value s.ioRefsMVector }, IO.Pure (IORef (Array.length s.ioRefsMVector)) ))


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


readIORefWeight : IORef Int -> IO Int
readIORefWeight (IORef ref) =
    IO
        (\s ->
            case Array.get ref s.ioRefsWeight of
                Just value ->
                    ( s, IO.Pure value )

                Nothing ->
                    crash "Data.IORef.readIORefWeight: could not find entry"
        )


readIORefPointInfo : IORef IO.PointInfo -> IO IO.PointInfo
readIORefPointInfo (IORef ref) =
    IO
        (\s ->
            case Array.get ref s.ioRefsPointInfo of
                Just value ->
                    ( s, IO.Pure value )

                Nothing ->
                    crash "Data.IORef.readIORefPointInfo: could not find entry"
        )


readIORefDescriptor : IORef IO.Descriptor -> IO IO.Descriptor
readIORefDescriptor (IORef ref) =
    IO
        (\s ->
            case Array.get ref s.ioRefsDescriptor of
                Just value ->
                    ( s, IO.Pure value )

                Nothing ->
                    crash "Data.IORef.readIORefDescriptor: could not find entry"
        )


readIORefMVector : IORef (Array (Maybe (List IO.Variable))) -> IO (Array (Maybe (List IO.Variable)))
readIORefMVector (IORef ref) =
    IO
        (\s ->
            case Array.get ref s.ioRefsMVector of
                Just value ->
                    ( s, IO.Pure value )

                Nothing ->
                    crash "Data.IORef.readIORefDescriptor: could not find entry"
        )


writeIORef : (b -> Encode.Value) -> IORef a -> b -> IO ()
writeIORef encoder (IORef ref) value =
    IO (\s -> ( { s | ioRefs = Array.set ref (encoder value) s.ioRefs }, IO.Pure () ))


writeIORefWeight : IORef Int -> Int -> IO ()
writeIORefWeight (IORef ref) value =
    IO (\s -> ( { s | ioRefsWeight = Array.set ref value s.ioRefsWeight }, IO.Pure () ))


writeIORefPointInfo : IORef IO.PointInfo -> IO.PointInfo -> IO ()
writeIORefPointInfo (IORef ref) value =
    IO (\s -> ( { s | ioRefsPointInfo = Array.set ref value s.ioRefsPointInfo }, IO.Pure () ))


writeIORefDescriptor : IORef IO.Descriptor -> IO.Descriptor -> IO ()
writeIORefDescriptor (IORef ref) value =
    IO (\s -> ( { s | ioRefsDescriptor = Array.set ref value s.ioRefsDescriptor }, IO.Pure () ))


writeIORefMVector : IORef (Array (Maybe (List IO.Variable))) -> Array (Maybe (List IO.Variable)) -> IO ()
writeIORefMVector (IORef ref) value =
    IO (\s -> ( { s | ioRefsMVector = Array.set ref value s.ioRefsMVector }, IO.Pure () ))


modifyIORef : Decode.Decoder a -> (a -> Encode.Value) -> IORef a -> (a -> a) -> IO ()
modifyIORef decoder encoder ioRef func =
    readIORef decoder ioRef
        |> IO.bind (\value -> writeIORef encoder ioRef (func value))


modifyIORefDescriptor : IORef IO.Descriptor -> (IO.Descriptor -> IO.Descriptor) -> IO ()
modifyIORefDescriptor ioRef func =
    readIORefDescriptor ioRef
        |> IO.bind (\value -> writeIORefDescriptor ioRef (func value))


modifyIORefMVector : IORef (Array (Maybe (List IO.Variable))) -> (Array (Maybe (List IO.Variable)) -> Array (Maybe (List IO.Variable))) -> IO ()
modifyIORefMVector ioRef func =
    readIORefMVector ioRef
        |> IO.bind (\value -> writeIORefMVector ioRef (func value))
