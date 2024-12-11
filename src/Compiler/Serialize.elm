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


assocListDict : (k -> k -> Order) -> Codec e k -> Codec e a -> Codec e (Dict k a)
assocListDict keyComparison keyCodec valueCodec =
    S.list (S.tuple keyCodec valueCodec)
        |> S.map (Dict.fromList keyComparison) Dict.toList


everySet : (a -> a -> Order) -> Codec e a -> Codec e (EverySet a)
everySet keyComparison codec =
    S.list codec
        |> S.map (EverySet.fromList keyComparison) (List.reverse << EverySet.toList)


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
