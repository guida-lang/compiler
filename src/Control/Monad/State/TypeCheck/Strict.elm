module Control.Monad.State.TypeCheck.Strict exposing
    ( StateT(..)
    , apply
    , bind
    , evalStateT
    , fmap
    , gets
    , liftIO
    , modify
    , pure
    , runStateT
    , traverseList
    , traverseMap
    , traverseMaybe
    , traverseTuple
    )

{-| Lazy state monads, passing an updatable state through a computation.
-}

import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)


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


traverseList : (a -> StateT s b) -> List a -> StateT s (List b)
traverseList f =
    List.foldr (\a -> bind (\c -> fmap (\va -> va :: c) (f a)))
        (pure [])


traverseTuple : (b -> StateT s c) -> ( a, b ) -> StateT s ( a, c )
traverseTuple f ( a, b ) =
    fmap (Tuple.pair a) (f b)


traverseMap : (k -> k -> Order) -> (a -> StateT s b) -> Dict k a -> StateT s (Dict k b)
traverseMap keyComparison f =
    traverseMapWithKey keyComparison (\_ -> f)


traverseMapWithKey : (k -> k -> Order) -> (k -> a -> StateT s b) -> Dict k a -> StateT s (Dict k b)
traverseMapWithKey keyComparison f =
    Dict.foldl (\k a -> bind (\c -> fmap (\va -> Dict.insert keyComparison k va c) (f k a)))
        (pure Dict.empty)


traverseMaybe : (a -> StateT s b) -> Maybe a -> StateT s (Maybe b)
traverseMaybe f a =
    case Maybe.map f a of
        Just b ->
            fmap Just b

        Nothing ->
            pure Nothing
