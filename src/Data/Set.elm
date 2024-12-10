module Data.Set exposing
    ( EverySet
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    )

{-| **Initial implementation from `Gizra/elm-all-set/1.0.1`**

A set of unique values. The values can be any type, as the implementation is
based on [AssocList](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest)


# Sets

@docs EverySet


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import Data.Map as Dict exposing (Dict)


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type EverySet c a
    = EverySet (Dict c a ())


{-| Create an empty set.
-}
empty : EverySet c a
empty =
    EverySet Dict.empty


{-| Create a set with one value.
-}
singleton : (a -> comparable) -> a -> EverySet comparable a
singleton toComparable k =
    EverySet <| Dict.singleton toComparable k ()


{-| Insert a value into a set.
-}
insert : (a -> comparable) -> a -> EverySet comparable a -> EverySet comparable a
insert toComparable k (EverySet d) =
    EverySet <| Dict.insert toComparable k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : (a -> comparable) -> a -> EverySet comparable a -> EverySet comparable a
remove toComparable k (EverySet d) =
    EverySet <| Dict.remove toComparable k d


{-| Determine if a set is empty.
-}
isEmpty : EverySet c a -> Bool
isEmpty (EverySet d) =
    Dict.isEmpty d


{-| Determine if a value is in a set.
-}
member : (a -> comparable) -> a -> EverySet comparable a -> Bool
member toComparable k (EverySet d) =
    Dict.member toComparable k d


{-| Determine the number of elements in a set.
-}
size : EverySet c a -> Int
size (EverySet d) =
    Dict.size d


{-| Get the union of two sets. Keep all values.
-}
union : EverySet comparable a -> EverySet comparable a -> EverySet comparable a
union (EverySet d1) (EverySet d2) =
    EverySet <| Dict.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : (a -> a -> Order) -> EverySet comparable a -> EverySet comparable a -> EverySet comparable a
intersect keyComparison (EverySet d1) (EverySet d2) =
    EverySet <| Dict.intersection keyComparison d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : EverySet comparable a -> EverySet comparable a -> EverySet comparable a
diff (EverySet d1) (EverySet d2) =
    EverySet <| Dict.diff d1 d2


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : (a -> a -> Order) -> EverySet c a -> List a
toList keyComparison (EverySet d) =
    Dict.keys keyComparison d


{-| Convert a list into a set, removing any duplicates.
-}
fromList : (a -> comparable) -> List a -> EverySet comparable a
fromList toComparable xs =
    List.foldl (insert toComparable) empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> a -> Order) -> (a -> b -> b) -> b -> EverySet c a -> b
foldl keyComparison f b (EverySet d) =
    Dict.foldl keyComparison (\k _ result -> f k result) b d


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> a -> Order) -> (a -> b -> b) -> b -> EverySet c a -> b
foldr keyComparison f b (EverySet d) =
    Dict.foldr keyComparison (\k _ result -> f k result) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> a -> Order) -> (a2 -> comparable) -> (a -> a2) -> EverySet comparable a -> EverySet comparable a2
map keyComparison toString f s =
    fromList toString (List.map f (toList keyComparison s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> EverySet comparable a -> EverySet comparable a
filter p (EverySet d) =
    EverySet <| Dict.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> EverySet comparable a -> ( EverySet comparable a, EverySet comparable a )
partition p (EverySet d) =
    let
        ( p1, p2 ) =
            Dict.partition (\k _ -> p k) d
    in
    ( EverySet p1, EverySet p2 )
