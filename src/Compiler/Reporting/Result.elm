module Compiler.Reporting.Result exposing
    ( RResult(..)
    , RStep(..)
    , Step(..)
    , apply
    , bind
    , fmap
    , indexedTraverse
    , loop
    , mapTraverseWithKey
    , ok
    , pure
    , run
    , throw
    , traverse
    , traverseDict
    , warn
    )

import Compiler.Data.Index as Index
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Reporting.Warning as Warning
import Data.Map as Dict exposing (Dict)



-- RESULT


type RResult info warnings error a
    = RResult (info -> warnings -> RStep info warnings error a)


type RStep info warnings error a
    = ROk info warnings a
    | RErr info warnings (OneOrMore.OneOrMore error)


run : RResult () (List w) e a -> ( List w, Result (OneOrMore.OneOrMore e) a )
run (RResult k) =
    case k () [] of
        ROk () w a ->
            ( List.reverse w, Ok a )

        RErr () w e ->
            ( List.reverse w, Err e )



-- LOOP


type Step state a
    = Loop state
    | Done a


loop : (state -> RResult i w e (Step state a)) -> state -> RResult i w e a
loop callback state =
    RResult <|
        \i w ->
            loopHelp callback i w state


loopHelp : (state -> RResult i w e (Step state a)) -> i -> w -> state -> RStep i w e a
loopHelp callback i w state =
    case callback state of
        RResult k ->
            case k i w of
                RErr i1 w1 e ->
                    RErr i1 w1 e

                ROk i1 w1 (Loop newState) ->
                    loopHelp callback i1 w1 newState

                ROk i1 w1 (Done a) ->
                    ROk i1 w1 a



-- HELPERS


ok : a -> RResult i w e a
ok a =
    RResult <|
        \i w ->
            ROk i w a


warn : Warning.Warning -> RResult i (List Warning.Warning) e ()
warn warning =
    RResult <|
        \i warnings ->
            ROk i (warning :: warnings) ()


throw : e -> RResult i w e a
throw e =
    RResult <|
        \i w ->
            RErr i w (OneOrMore.one e)



-- FANCY INSTANCE STUFF


fmap : (a -> b) -> RResult i w e a -> RResult i w e b
fmap func (RResult k) =
    RResult <|
        \i w ->
            case k i w of
                ROk i1 w1 value ->
                    ROk i1 w1 (func value)

                RErr i1 w1 e ->
                    RErr i1 w1 e


pure : a -> RResult i w e a
pure =
    ok


apply : RResult i w x a -> RResult i w x (a -> b) -> RResult i w x b
apply (RResult kv) (RResult kf) =
    RResult <|
        \i w ->
            case kf i w of
                ROk i1 w1 func ->
                    case kv i1 w1 of
                        ROk i2 w2 value ->
                            ROk i2 w2 (func value)

                        RErr i2 w2 e2 ->
                            RErr i2 w2 e2

                RErr i1 w1 e1 ->
                    case kv i1 w1 of
                        ROk i2 w2 _ ->
                            RErr i2 w2 e1

                        RErr i2 w2 e2 ->
                            RErr i2 w2 (OneOrMore.more e1 e2)


bind : (a -> RResult i w x b) -> RResult i w x a -> RResult i w x b
bind callback (RResult ka) =
    RResult <|
        \i w ->
            case ka i w of
                ROk i1 w1 a ->
                    case callback a of
                        RResult kb ->
                            kb i1 w1

                RErr i1 w1 e ->
                    RErr i1 w1 e


traverse : (a -> RResult i w x b) -> List a -> RResult i w x (List b)
traverse func =
    List.foldl
        (\a (RResult acc) ->
            RResult <|
                \i w ->
                    let
                        (RResult kv) =
                            func a
                    in
                    case acc i w of
                        ROk i1 w1 accList ->
                            case kv i1 w1 of
                                ROk i2 w2 value ->
                                    ROk i2 w2 (value :: accList)

                                RErr i2 w2 e2 ->
                                    RErr i2 w2 e2

                        RErr i1 w1 e1 ->
                            case kv i1 w1 of
                                ROk i2 w2 _ ->
                                    RErr i2 w2 e1

                                RErr i2 w2 e2 ->
                                    RErr i2 w2 (OneOrMore.more e1 e2)
        )
        (pure [])
        >> fmap List.reverse


mapTraverseWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> RResult i w x b) -> Dict comparable k a -> RResult i w x (Dict comparable k b)
mapTraverseWithKey toComparable keyComparison f dict =
    loop (mapTraverseWithKeyHelp toComparable f) ( Dict.toList keyComparison dict, Dict.empty )


mapTraverseWithKeyHelp : (k -> comparable) -> (k -> a -> RResult i w x b) -> ( List ( k, a ), Dict comparable k b ) -> RResult i w x (Step ( List ( k, a ), Dict comparable k b ) (Dict comparable k b))
mapTraverseWithKeyHelp toComparable f ( pairs, result ) =
    case pairs of
        [] ->
            pure (Done result)

        ( k, a ) :: rest ->
            fmap (\b -> Loop ( rest, Dict.insert toComparable k b result )) (f k a)


traverseDict : (k -> comparable) -> (k -> k -> Order) -> (a -> RResult i w x b) -> Dict comparable k a -> RResult i w x (Dict comparable k b)
traverseDict toComparable keyComparison func =
    Dict.foldr keyComparison (\k a -> bind (\acc -> fmap (\b -> Dict.insert toComparable k b acc) (func a))) (ok Dict.empty)


indexedTraverse : (Index.ZeroBased -> a -> RResult i w error b) -> List a -> RResult i w error (List b)
indexedTraverse func xs =
    List.foldr (\a -> bind (\acc -> fmap (\b -> b :: acc) a)) (ok []) (Index.indexedMap func xs)
