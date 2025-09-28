module Compiler.Guida.Magnitude exposing
    ( Magnitude(..)
    , compare
    , toChars
    )

-- MAGNITUDE


type Magnitude
    = PATCH
    | MINOR
    | MAJOR


toChars : Magnitude -> String
toChars magnitude =
    case magnitude of
        PATCH ->
            "PATCH"

        MINOR ->
            "MINOR"

        MAJOR ->
            "MAJOR"


compare : Magnitude -> Magnitude -> Order
compare m1 m2 =
    let
        toInt : Magnitude -> number
        toInt m =
            case m of
                PATCH ->
                    0

                MINOR ->
                    1

                MAJOR ->
                    2
    in
    Basics.compare (toInt m1) (toInt m2)
