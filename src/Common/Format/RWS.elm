module Common.Format.RWS exposing
    ( RWS
    , bind
    , error
    , evalRWS
    , get
    , mapM_
    , modify
    , put
    , replicateM
    , return
    , runRWS
    , tell
    )

import Data.Map as Dict exposing (Dict)
import Utils.Crash exposing (crash)


type alias RWS r s a =
    -- type alias RWS r w s a =
    -- r: (), w: ReferenceMap, s: ContainerStack, a: Container
    r -> s -> ( a, s, Dict String String ( String, String ) )


evalRWS : RWS r s a -> r -> s -> ( a, Dict String String ( String, String ) )
evalRWS rws r s =
    let
        ( a, _, w ) =
            runRWS rws r s
    in
    ( a, w )


runRWS : RWS r s a -> r -> s -> ( a, s, Dict String String ( String, String ) )
runRWS rws r s =
    rws r s


mapM_ : (a -> RWS r s b) -> List a -> RWS r s ()
mapM_ f xs =
    \r s0 ->
        List.foldr
            (\x ( _, s, w ) ->
                let
                    ( _, newS, newW ) =
                        f x r s
                in
                ( (), newS, Dict.union w newW )
            )
            ( (), s0, Dict.empty )
            xs


bind : (a -> RWS r s b) -> RWS r s a -> RWS r s b
bind f rwsa =
    \r s0 ->
        let
            ( a, s1, w1 ) =
                rwsa r s0

            ( b, s2, w2 ) =
                f a r s1
        in
        ( b, s2, Dict.union w1 w2 )


get : RWS r s s
get =
    \_ s -> ( s, s, Dict.empty )


put : s -> RWS r s ()
put newState =
    \_ _ -> ( (), newState, Dict.empty )


modify : (s -> s) -> RWS r s ()
modify f =
    \_ s -> ( (), f s, Dict.empty )


return : a -> RWS r s a
return a =
    \_ s -> ( a, s, Dict.empty )


tell : Dict String String ( String, String ) -> RWS r s ()
tell log =
    \_ s -> ( (), s, log )


replicateM : Int -> RWS r s a -> RWS r s (List a)
replicateM n rws =
    if n <= 0 then
        return []

    else
        rws
            |> bind
                (\a ->
                    replicateM (n - 1) rws
                        |> bind (\list -> return (a :: list))
                )


error : String -> RWS r s a
error =
    crash
