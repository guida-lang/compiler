module Common.Format.Bimap exposing
    ( Bimap
    , fromList
    )

import Data.Map as Map exposing (Dict)


type Bimap a b
    = Bimap (Dict String a b) (Dict String b a)


fromList : (a -> String) -> (b -> String) -> List ( a, b ) -> Bimap a b
fromList toComparableA toComparableB list =
    Bimap (Map.fromList toComparableA list)
        (Map.fromList toComparableB (List.map (\( a, b ) -> ( b, a )) list))
