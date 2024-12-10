module Data.Map exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size, eq
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersection, diff, merge
    )

{-| **Initial implementation from `pzp1997/assoc-list/1.0.0`**

An [association list](https://en.wikipedia.org/wiki/Association_list) is a
list of tuples that map unique keys to values. The keys can be of any type (so
long as it has a reasonable definition for equality). This includes pretty
much everything except for functions and things that contain functions.

All functions in this module are "stack safe," which means that your program
won't crash from recursing over large association lists. You can read
Evan Czaplicki's
[document on tail-call elimination](https://github.com/evancz/functional-programming-in-elm/blob/master/recursion/tail-call-elimination.md)
for more information about this topic.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size, eq


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersection, diff, merge

-}

import Dict


{-| A dictionary of keys and values. So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

    import Data.Map as Dict exposing (Dict)

    users : Dict String User
    users =
        Dict.fromList
            [ ( "Alice", User "Alice" 28 1.65 )
            , ( "Bob", User "Bob" 19 1.82 )
            , ( "Chuck", User "Chuck" 33 1.75 )
            ]

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

-}
type Dict c k v
    = D (Dict.Dict c ( k, v ))


{-| Create an empty dictionary.
-}
empty : Dict c k v
empty =
    D Dict.empty


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    type Animal
        = Cat
        | Mouse

    animals : Dict String Animal
    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom" animals
    --> Just Cat

    get "Jerry" animals
    --> Just Mouse

    get "Spike" animals
    --> Nothing

-}
get : (k -> comparable) -> k -> Dict comparable k v -> Maybe v
get toComparable targetKey (D dict) =
    Dict.get (toComparable targetKey) dict
        |> Maybe.map Tuple.second


{-| Determine if a key is in a dictionary.
-}
member : (k -> comparable) -> k -> Dict comparable k v -> Bool
member toComparable targetKey (D dict) =
    Dict.member (toComparable targetKey) dict


{-| Determine the number of key-value pairs in the dictionary.

    size (fromList [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ) ])
    --> 3

    size (insert 1 "b" (singleton 1 "a"))
    --> 1

-}
size : Dict c k v -> Int
size (D dict) =
    Dict.size dict


{-| Determine if a dictionary is empty.

    isEmpty empty
    --> True

-}
isEmpty : Dict c k v -> Bool
isEmpty (D dict) =
    Dict.isEmpty dict


{-| Compare two dictionaries for equality, ignoring insertion order.
Dictionaries are defined to be equal when they have identical key-value pairs
where keys and values are compared using the built-in equality operator.

You should almost never use the built-in equality operator to compare
dictionaries from this module since association lists have no canonical form.

    eq
        (fromList [ ( "a", 1 ), ( "b", 2 ) ])
        (fromList [ ( "b", 2 ), ( "a", 1 ) ])
    --> True

-}
eq : Dict comparable k v -> Dict comparable k v -> Bool
eq leftDict rightDict =
    merge (\_ _ -> EQ)
        (\_ _ _ -> False)
        (\_ a b result -> result && a == b)
        (\_ _ _ -> False)
        leftDict
        rightDict
        True


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : (k -> comparable) -> k -> v -> Dict comparable k v -> Dict comparable k v
insert toComparable key value (D dict) =
    D (Dict.insert (toComparable key) ( key, value ) dict)


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : (k -> comparable) -> k -> Dict comparable k v -> Dict comparable k v
remove toComparable targetKey (D dict) =
    D (Dict.remove (toComparable targetKey) dict)


{-| Update the value of a dictionary for a specific key with a given function.

If you are using this module as an ordered dictionary, please note that if you
are replacing the value of an existing entry, the entry will remain where it
is in the insertion order. (If you do want to change the insertion order,
consider using `get` in conjunction with `insert` instead.)

-}
update : (k -> comparable) -> k -> (Maybe v -> Maybe v) -> Dict comparable k v -> Dict comparable k v
update toComparable targetKey alter (D dict) =
    D
        (Dict.update (toComparable targetKey)
            (Maybe.map Tuple.second
                >> alter
                >> Maybe.map (Tuple.pair targetKey)
            )
            dict
        )


{-| Create a dictionary with one key-value pair.
-}
singleton : (k -> comparable) -> k -> v -> Dict comparable k v
singleton toComparable key value =
    D (Dict.singleton (toComparable key) ( key, value ))



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.

If you are using this module as an ordered dictionary, the ordering of the
output dictionary will be all the entries of the first dictionary (from most
recently inserted to least recently inserted) followed by all the entries of
the second dictionary (from most recently inserted to least recently inserted).

-}
union : Dict comparable k v -> Dict comparable k v -> Dict comparable k v
union (D leftDict) (D rightDict) =
    D (Dict.union leftDict rightDict)


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersection : (k -> k -> Order) -> Dict comparable k a -> Dict comparable k b -> Dict comparable k a
intersection keyComparison dict1 dict2 =
    let
        keys2 : List k
        keys2 =
            keys keyComparison dict2
    in
    filter (\k _ -> List.member k keys2) dict1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable k a -> Dict comparable k b -> Dict comparable k a
diff (D leftDict) (D rightDict) =
    D (Dict.diff leftDict rightDict)


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys in the following order, building up whatever
you want:

1.  All the keys that appear only in the right dictionary from least
    recently inserted to most recently inserted.
2.  All the keys in the left dictionary from least recently inserted to most
    recently inserted (without regard to whether they appear only in the left
    dictionary or in both dictionaries).

-}
merge :
    (k -> k -> Order)
    -> (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> Dict comparable k a
    -> Dict comparable k b
    -> result
    -> result
merge keyComparison leftStep bothStep rightStep (D leftDict) (D rightDict) initialResult =
    Dict.merge
        (\_ ( k, a ) -> leftStep k a)
        (\_ ( k, a ) ( _, b ) -> bothStep k a b)
        (\_ ( k, b ) -> rightStep k b)
        leftDict
        rightDict
        initialResult



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict c k a -> Dict c k b
map alter (D dict) =
    D (Dict.map (\_ ( key, value ) -> ( key, alter key value )) dict)


{-| Fold over the key-value pairs in a dictionary from most recently inserted
to least recently inserted.

    users : Dict String Int
    users =
        empty
            |> insert "Alice" 28
            |> insert "Bob" 19
            |> insert "Chuck" 33

    foldl (\name age result -> age :: result) [] users
    --> [28,19,33]

-}
foldl : (k -> k -> Order) -> (k -> v -> b -> b) -> b -> Dict c k v -> b
foldl keyComparison func initialResult dict =
    List.foldl
        (\( key, value ) result ->
            func key value result
        )
        initialResult
        (toList keyComparison dict)


{-| Fold over the key-value pairs in a dictionary from least recently inserted
to most recently insered.

    users : Dict String Int
    users =
        empty
            |> insert "Alice" 28
            |> insert "Bob" 19
            |> insert "Chuck" 33

    foldr (\name age result -> age :: result) [] users
    --> [33,19,28]

-}
foldr : (k -> k -> Order) -> (k -> v -> b -> b) -> b -> Dict c k v -> b
foldr keyComparison func initialResult dict =
    List.foldr
        (\( key, value ) result ->
            func key value result
        )
        initialResult
        (toList keyComparison dict)


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (k -> v -> Bool) -> Dict comparable k v -> Dict comparable k v
filter isGood (D dict) =
    D (Dict.filter (\_ ( key, value ) -> isGood key value) dict)


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (k -> v -> Bool) -> Dict comparable k v -> ( Dict comparable k v, Dict comparable k v )
partition isGood (D dict) =
    let
        ( good, bad ) =
            Dict.partition (\_ ( key, value ) -> isGood key value) dict
    in
    ( D good, D bad )



-- LISTS


{-| Get all of the keys in a dictionary, in the order that they were inserted
with the most recently inserted key at the head of the list.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    --> [ 1, 0 ]

-}
keys : (k -> k -> Order) -> Dict c k v -> List k
keys keyComparison (D dict) =
    Dict.values dict
        |> List.sortWith (\( k1, _ ) ( k2, _ ) -> keyComparison k1 k2)
        |> List.map Tuple.first


{-| Get all of the values in a dictionary, in the order that they were inserted
with the most recently inserted value at the head of the list.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    --> [ "Bob", "Alice" ]

-}
values : (k -> k -> Order) -> Dict c k v -> List v
values keyComparison (D dict) =
    Dict.values dict
        |> List.sortWith (\( k1, _ ) ( k2, _ ) -> keyComparison k1 k2)
        |> List.map Tuple.second


{-| Convert a dictionary into an association list of key-value pairs, in the
order that they were inserted with the most recently inserted entry at the
head of the list.
-}
toList : (k -> k -> Order) -> Dict c k v -> List ( k, v )
toList keyComparison (D dict) =
    Dict.values dict
        |> List.sortWith (\( k1, _ ) ( k2, _ ) -> keyComparison k1 k2)


{-| Convert an association list into a dictionary. The elements are inserted
from left to right. (If you want to insert the elements from right to left, you
can simply call `List.reverse` on the input before passing it to `fromList`.)
-}
fromList : (k -> comparable) -> List ( k, v ) -> Dict comparable k v
fromList toComparable =
    List.foldl (\( key, value ) -> Dict.insert (toComparable key) ( key, value )) Dict.empty
        >> D
