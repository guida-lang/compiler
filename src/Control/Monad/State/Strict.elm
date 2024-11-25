module Control.Monad.State.Strict exposing
    ( StateT(..)
    , apply
    , bind
    , evalStateT
    , fmap
    , get
    , gets
    , liftIO
    , modify
    , pure
    , put
    , runStateT
    )

{-| Lazy state monads, passing an updatable state through a computation.
-}

import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO(..))
import Utils.Crash exposing (crash)


{-| newtype StateT s m a

A state transformer monad parameterized by:

s - The state.
m - The inner monad. (== IO)

The return function leaves the state unchanged, while >>= uses the final state of the first computation as the initial state of the second.

Ref.: <https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-State-Lazy.html#t:StateT>

-}
type StateT s a
    = StateT (s -> IO ( a, s ))


runStateT : StateT s a -> s -> IO ( a, s )
runStateT (StateT f) =
    f


evalStateT : StateT s a -> s -> IO a
evalStateT (StateT f) =
    f >> IO.fmap Tuple.first


liftIO : IO a -> StateT s a
liftIO io =
    StateT (\s -> IO.fmap (\a -> ( a, s )) io)


apply : StateT s a -> StateT s (a -> b) -> StateT s b
apply (StateT arg) (StateT func) =
    StateT
        (\s ->
            arg s
                |> IO.bind
                    (\( a, sa ) ->
                        func sa
                            |> IO.fmap (\( fb, sb ) -> ( fb a, sb ))
                    )
        )


fmap : (a -> b) -> StateT s a -> StateT s b
fmap func argStateT =
    apply argStateT (pure func)


bind : (a -> StateT s b) -> StateT s a -> StateT s b
bind func (StateT arg) =
    StateT
        (\s ->
            arg s
                |> IO.bind
                    (\( a, sa ) ->
                        case func a of
                            StateT fb ->
                                fb sa
                    )
        )


pure : a -> StateT s a
pure value =
    StateT (\s -> IO.pure ( value, s ))


gets : (s -> a) -> StateT s a
gets f =
    StateT (\s -> IO.pure ( f s, s ))


modify : (s -> s) -> StateT s ()
modify f =
    StateT (\s -> IO.pure ( (), f s ))


get : Decode.Decoder s -> StateT s s
get decoder =
    IO
        (\s ->
            case Decode.decodeValue decoder s.state of
                Ok value ->
                    ( s, IO.Pure value )

                Err err ->
                    crash (Decode.errorToString err)
        )
        |> liftIO


put : (s -> Encode.Value) -> s -> IO ()
put encoder state =
    IO (\s -> ( { s | state = encoder state }, IO.Pure () ))
