module Compiler.Data.Index exposing
    ( VerifiedList(..)
    , ZeroBased
    , first
    , indexedMap
    , indexedZipWith
    , next
    , second
    , third
    , toHuman
    , toMachine
    , zeroBasedDecoder
    , zeroBasedEncoder
    )

import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- ZERO BASED


type ZeroBased
    = ZeroBased Int


first : ZeroBased
first =
    ZeroBased 0


second : ZeroBased
second =
    ZeroBased 1


third : ZeroBased
third =
    ZeroBased 2


next : ZeroBased -> ZeroBased
next (ZeroBased i) =
    ZeroBased (i + 1)



-- DESTRUCT


toMachine : ZeroBased -> Int
toMachine (ZeroBased index) =
    index


toHuman : ZeroBased -> Int
toHuman (ZeroBased index) =
    index + 1



-- INDEXED MAP


indexedMap : (ZeroBased -> a -> b) -> List a -> List b
indexedMap func xs =
    List.map2 func (List.map ZeroBased (List.range 0 (List.length xs - 1))) xs


{-| indexedTraverse and indexedForA are defined on `Utils`
-}



-- VERIFIED/INDEXED ZIP


type VerifiedList a
    = LengthMatch (List a)
    | LengthMismatch Int Int


indexedZipWith : (ZeroBased -> a -> b -> c) -> List a -> List b -> VerifiedList c
indexedZipWith func listX listY =
    indexedZipWithHelp func 0 listX listY []


indexedZipWithHelp : (ZeroBased -> a -> b -> c) -> Int -> List a -> List b -> List c -> VerifiedList c
indexedZipWithHelp func index listX listY revListZ =
    case ( listX, listY ) of
        ( [], [] ) ->
            LengthMatch (List.reverse revListZ)

        ( x :: xs, y :: ys ) ->
            indexedZipWithHelp func (index + 1) xs ys (func (ZeroBased index) x y :: revListZ)

        _ ->
            LengthMismatch (index + List.length listX) (index + List.length listY)



-- ENCODERS and DECODERS


zeroBasedEncoder : ZeroBased -> BE.Encoder
zeroBasedEncoder (ZeroBased zeroBased) =
    BE.int zeroBased


zeroBasedDecoder : BD.Decoder ZeroBased
zeroBasedDecoder =
    BD.map ZeroBased BD.int
