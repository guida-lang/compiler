module Common.Format.RWS exposing (..)

import Dict exposing (Dict)


type RWS r w s a
    = RWS (r -> s -> ( a, w ))


evalRWS : RWS r w s a -> r -> s -> ( a, w )
evalRWS (RWS f) =
    f


mapM_ : (a -> RWS r w s b) -> List a -> RWS r w s ()
mapM_ f =
    Debug.todo "mapM_"


bind : (a -> RWS r w s b) -> RWS r w s a -> RWS r w s b
bind _ _ =
    Debug.todo "bind"


get : RWS r w s a
get =
    Debug.todo "get"


put : w -> RWS r w s ()
put _ =
    Debug.todo "put"


return : a -> RWS r w s a
return _ =
    Debug.todo "return"


tell : Dict String ( String, String ) -> RWS r w s ()
tell _ =
    Debug.todo "tell"
