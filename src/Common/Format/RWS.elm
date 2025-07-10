module Common.Format.RWS exposing (..)


type RWS r w s a
    = RWS r w s a


evalRWS : RWS r w s a -> r -> s -> ( a, w )
evalRWS =
    Debug.todo "evalRWS"


mapM_ : (a -> RWS r w s b) -> List a -> RWS r w s ()
mapM_ f =
    Debug.todo "mapM_"
