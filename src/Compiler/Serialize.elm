module Compiler.Serialize exposing
    ( assocListDict
    , everySet
    , nonempty
    , oneOrMore
    )

import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Serialize as S exposing (Codec)


assocListDict : (k -> comparable) -> (k -> k -> Order) -> Codec e k -> Codec e a -> Codec e (Dict comparable k a)
assocListDict toComparable keyComparison keyCodec valueCodec =
    S.list (S.tuple keyCodec valueCodec)
        |> S.map (Dict.fromList toComparable) (Dict.toList keyComparison)


everySet : (a -> comparable) -> (a -> a -> Order) -> Codec e a -> Codec e (EverySet comparable a)
everySet toComparable keyComparison codec =
    S.list codec
        |> S.map (EverySet.fromList toComparable) (List.reverse << EverySet.toList keyComparison)


nonempty : Codec e a -> Codec (S.Error e) (NE.Nonempty a)
nonempty codec =
    S.list codec
        |> S.mapError S.CustomError
        |> S.mapValid
            (\values ->
                case values of
                    x :: xs ->
                        Ok (NE.Nonempty x xs)

                    [] ->
                        Err S.DataCorrupted
            )
            (\(NE.Nonempty x xs) -> x :: xs)


oneOrMore : Codec e a -> Codec e (OneOrMore a)
oneOrMore codec =
    S.customType
        (\oneEncoder moreEncoder value ->
            case value of
                OneOrMore.One x ->
                    oneEncoder x

                OneOrMore.More a b ->
                    moreEncoder a b
        )
        |> S.variant1 OneOrMore.One codec
        |> S.variant2 OneOrMore.More (S.lazy (\() -> oneOrMore codec)) (S.lazy (\() -> oneOrMore codec))
        |> S.finishCustomType
