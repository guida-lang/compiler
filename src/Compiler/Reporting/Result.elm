module Compiler.Reporting.Result exposing
    ( RErr(..)
    , ROk(..)
    , RResult(..)
    , apply
    , bind
    , fmap
    , indexedTraverse
    , mapTraverseWithKey
    , ok
    , pure
    , run
    , then_
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
    = RResult
        (info
         -> warnings
         -> Result (RErr info warnings error) (ROk info warnings a)
        )


type ROk info warnings a
    = ROk info warnings a


type RErr info warnings error
    = RErr info warnings (OneOrMore.OneOrMore error)


run : RResult () (List w) e a -> ( List w, Result (OneOrMore.OneOrMore e) a )
run (RResult k) =
    case k () [] of
        Ok (ROk () w a) ->
            ( List.reverse w, Ok a )

        Err (RErr () w e) ->
            ( List.reverse w, Err e )



-- HELPERS


ok : a -> RResult i w e a
ok a =
    RResult <|
        \i w ->
            Ok (ROk i w a)


warn : Warning.Warning -> RResult i (List Warning.Warning) e ()
warn warning =
    RResult <|
        \i warnings ->
            Ok (ROk i (warning :: warnings) ())


throw : e -> RResult i w e a
throw e =
    RResult <|
        \i w ->
            Err (RErr i w (OneOrMore.one e))



-- FANCY INSTANCE STUFF


fmap : (a -> b) -> RResult i w e a -> RResult i w e b
fmap func (RResult k) =
    RResult <|
        \i w ->
            Result.map (\(ROk i1 w1 value) -> ROk i1 w1 (func value))
                (k i w)


pure : a -> RResult i w e a
pure =
    ok


apply : RResult i w x a -> RResult i w x (a -> b) -> RResult i w x b
apply (RResult kv) (RResult kf) =
    RResult <|
        \i w ->
            case kf i w of
                Ok (ROk i1 w1 func) ->
                    Result.map (\(ROk i2 w2 value) -> ROk i2 w2 (func value))
                        (kv i1 w1)

                Err (RErr i1 w1 e1) ->
                    case kv i1 w1 of
                        Ok (ROk i2 w2 _) ->
                            Err (RErr i2 w2 e1)

                        Err (RErr i2 w2 e2) ->
                            Err (RErr i2 w2 (OneOrMore.more e1 e2))


bind : (a -> RResult i w x b) -> RResult i w x a -> RResult i w x b
bind callback (RResult ka) =
    RResult <|
        \i w ->
            Result.andThen
                (\(ROk i1 w1 a) ->
                    case callback a of
                        RResult kb ->
                            kb i1 w1
                )
                (ka i w)


then_ : RResult i w x a -> RResult i w x b -> RResult i w x b
then_ (RResult ka) (RResult kb) =
    RResult <|
        \i w ->
            Result.andThen
                (\(ROk i1 w1 _) ->
                    kb i1 w1
                )
                (ka i w)


traverse : (a -> RResult i w x b) -> List a -> RResult i w x (List b)
traverse func =
    List.foldr (\a -> bind (\acc -> fmap (\b -> b :: acc) (func a))) (ok [])


mapTraverseWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> RResult i w x b) -> Dict comparable k a -> RResult i w x (Dict comparable k b)
mapTraverseWithKey toComparable keyComparison f =
    Dict.foldr keyComparison
        (\k a -> bind (\c -> fmap (\va -> Dict.insert toComparable k va c) (f k a)))
        (pure Dict.empty)


traverseDict : (k -> comparable) -> (k -> k -> Order) -> (a -> RResult i w x b) -> Dict comparable k a -> RResult i w x (Dict comparable k b)
traverseDict toComparable keyComparison func =
    Dict.foldr keyComparison (\k a -> bind (\acc -> fmap (\b -> Dict.insert toComparable k b acc) (func a))) (ok Dict.empty)


indexedTraverse : (Index.ZeroBased -> a -> RResult i w error b) -> List a -> RResult i w error (List b)
indexedTraverse func xs =
    List.foldr (\a -> bind (\acc -> fmap (\b -> b :: acc) a)) (ok []) (Index.indexedMap func xs)
