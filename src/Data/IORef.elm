module Data.IORef exposing
    ( IORef
    , newIORef
    , readIORef
    )

import Json.Decode as Decode
import Json.Encode as Encode
import System.IO exposing (IO(..))


type IORef a
    = IORef Int


newIORef : (a -> Encode.Value) -> a -> IO (IORef a)
newIORef encoder value =
    -- make (Decode.map IORef Decode.int) (NewIORef (encoder value))
    Debug.todo "newIORef"


readIORef : Decode.Decoder a -> IORef a -> IO a
readIORef decoder (IORef ref) =
    -- make decoder (ReadIORef ref)
    Debug.todo "readIORef"
