module Rank2TypecheckBug exposing (..)


f x =
    let
        g : (a -> ()) -> ()
        g h =
            h x
    in
    x
