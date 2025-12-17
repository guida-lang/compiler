module MaybeMap exposing (main)

import Html exposing (Html)


andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
    Maybe.map2 (|>)


type alias X =
    { x1 : Int
    , x2 : Int
    , x3 : Int
    , x4 : Int
    , x5 : Int
    , x6 : Int
    , x7 : Int
    , x8 : Int
    , x9 : Int
    , x10 : Int
    , x11 : Int
    , x12 : Int
    , x13 : Int
    , x14 : Int
    , x15 : Int
    , x16 : Int
    , x17 : Int
    , x18 : Int
    , x19 : Int
    , x20 : Int
    , x21 : Int
    , x22 : Int
    , x23 : Int
    , x24 : Int
    }


main : Html msg
main =
    let
        foo a =
            Just X
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
                |> andMap a
    in
    foo (Just 1)
        |> Debug.toString
        |> Html.text
