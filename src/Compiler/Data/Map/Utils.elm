module Compiler.Data.Map.Utils exposing
    ( any
    , fromKeys
    , fromKeysA
    )

import Data.Map as Dict exposing (Dict)
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- FROM KEYS


fromKeys : (comparable -> v) -> List comparable -> Dict comparable comparable v
fromKeys toValue keys =
    Dict.fromList identity (List.map (\k -> ( k, toValue k )) keys)


fromKeysA : (k -> comparable) -> (k -> Task Never v) -> List k -> Task Never (Dict comparable k v)
fromKeysA toComparable toValue keys =
    Task.fmap (Dict.fromList toComparable) (Utils.listTraverse (\k -> Task.fmap (Tuple.pair k) (toValue k)) keys)



-- ANY


any : (v -> Bool) -> Dict c k v -> Bool
any isGood dict =
    Dict.foldl (\_ _ -> EQ) (\_ v acc -> isGood v || acc) False dict
