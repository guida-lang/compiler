module Main exposing (add1)

{-| This is a test package for testing the Elm compiler.


# Example

@docs add1

-}


{-| Add 1 to the given number.

    add1 2 == 3

-}
add1 : Int -> Int
add1 x =
    x + 1
