module Data.Vector.Mutable exposing
    ( grow
    , length
    , modify
    , read
    , replicate
    , write
    )

import Array exposing (Array)
import Data.IORef exposing (IORef)
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO exposing (IO)


length : IORef (Array (Maybe a)) -> IO Int
length =
    -- IO.readIORef (Decode.array (Decode.succeed Nothing))
    --     >> IO.fmap Array.length
    Debug.todo "length"


replicate : (a -> Encode.Value) -> Int -> a -> IO (IORef (Array (Maybe a)))
replicate encoder n e =
    -- newIORef
    --     (Encode.array
    --         (Maybe.map encoder
    --             >> Maybe.withDefault Encode.null
    --         )
    --     )
    --     (Array.repeat n (Just e))
    Debug.todo "replicate"


grow : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> Int -> IO (IORef (Array (Maybe a)))
grow decoder encoder ioRef length_ =
    -- readIORef (Decode.array (Decode.maybe decoder)) ioRef
    --     |> bind
    --         (\value ->
    --             writeIORef
    --                 (Encode.array
    --                     (Maybe.map encoder
    --                         >> Maybe.withDefault Encode.null
    --                     )
    --                 )
    --                 ioRef
    --                 (Array.append value (Array.repeat length_ Nothing))
    --         )
    --     |> fmap (\_ -> ioRef)
    Debug.todo "grow"


read : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> Int -> IO a
read decoder encoder ioRef i =
    -- readIORef (Decode.array (Decode.maybe decoder)) ioRef
    --     |> bind
    --         (\vector ->
    --             make decoder
    --                 (MVectorRead i
    --                     (Encode.array
    --                         (\maybeValue ->
    --                             case maybeValue of
    --                                 Just value ->
    --                                     encoder value
    --                                 Nothing ->
    --                                     Encode.null
    --                         )
    --                         vector
    --                     )
    --                 )
    --         )
    Debug.todo "read"


write : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> Int -> a -> IO ()
write decoder encoder ioRef i x =
    -- modifyIORef (Decode.array (Decode.maybe decoder))
    --     (Encode.array
    --         (Maybe.map encoder
    --             >> Maybe.withDefault Encode.null
    --         )
    --     )
    --     ioRef
    --     (Array.set i (Just x))
    Debug.todo "write"


modify : Decode.Decoder a -> (a -> Encode.Value) -> IORef (Array (Maybe a)) -> (a -> a) -> Int -> IO ()
modify decoder encoder ioRef func index =
    -- modifyIORef (Decode.array (Decode.maybe decoder))
    --     (Encode.array
    --         (Maybe.map encoder
    --             >> Maybe.withDefault Encode.null
    --         )
    --     )
    --     ioRef
    --     (Array.update index (Maybe.map func))
    Debug.todo "modify"
