module Control.Monad.State.Strict exposing
    ( StateT(..)
    , evalStateT
    , fmap
    , get
    , liftIO
    , put
    )

{-| Lazy state monads, passing an updatable state through a computation.
-}

import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO
import Task exposing (Task)
import Utils.Impure as Impure
import Utils.Task.Extra as Task


{-| newtype StateT s m a

A state transformer monad parameterized by:

s - The state.
m - The inner monad. (== IO)

The return function leaves the state unchanged, while >>= uses the final state of the first computation as the initial state of the second.

Ref.: <https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-State-Lazy.html#t:StateT>

-}
type StateT s a
    = StateT (s -> Task Never ( a, s ))


evalStateT : StateT s a -> s -> Task Never a
evalStateT (StateT f) =
    f >> Task.fmap Tuple.first


liftIO : Task Never a -> StateT s a
liftIO io =
    StateT (\s -> Task.fmap (\a -> ( a, s )) io)


apply : StateT s a -> StateT s (a -> b) -> StateT s b
apply (StateT arg) (StateT func) =
    StateT
        (\s ->
            arg s
                |> Task.bind
                    (\( a, sa ) ->
                        func sa
                            |> Task.fmap (\( fb, sb ) -> ( fb a, sb ))
                    )
        )


fmap : (a -> b) -> StateT s a -> StateT s b
fmap func argStateT =
    apply argStateT (pure func)


pure : a -> StateT s a
pure value =
    StateT (\s -> Task.pure ( value, s ))


get : StateT s IO.ReplState
get =
    liftIO
        (Impure.task "getStateT"
            []
            Impure.EmptyBody
            (Impure.DecoderResolver
                (Decode.map3 (\imports types decls -> IO.ReplState imports types decls)
                    (Decode.field "imports" (Decode.dict Decode.string))
                    (Decode.field "types" (Decode.dict Decode.string))
                    (Decode.field "decls" (Decode.dict Decode.string))
                )
            )
        )


put : IO.ReplState -> Task Never ()
put (IO.ReplState imports types decls) =
    Impure.task "putStateT"
        []
        (Impure.JsonBody
            (Encode.object
                [ ( "imports", Encode.dict identity Encode.string imports )
                , ( "types", Encode.dict identity Encode.string types )
                , ( "decls", Encode.dict identity Encode.string decls )
                ]
            )
        )
        (Impure.Always ())
