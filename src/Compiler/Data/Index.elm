module Compiler.Data.Index exposing
    ( CDI_ZeroBased
    , VerifiedList(..)
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

import Json.Decode as Decode
import Json.Encode as Encode



-- ZERO BASED


type CDI_ZeroBased
    = CDI_ZeroBased Int


first : CDI_ZeroBased
first =
    CDI_ZeroBased 0


second : CDI_ZeroBased
second =
    CDI_ZeroBased 1


third : CDI_ZeroBased
third =
    CDI_ZeroBased 2


next : CDI_ZeroBased -> CDI_ZeroBased
next (CDI_ZeroBased i) =
    CDI_ZeroBased (i + 1)



-- DESTRUCT


toMachine : CDI_ZeroBased -> Int
toMachine (CDI_ZeroBased index) =
    index


toHuman : CDI_ZeroBased -> Int
toHuman (CDI_ZeroBased index) =
    index + 1



-- INDEXED MAP


indexedMap : (CDI_ZeroBased -> a -> b) -> List a -> List b
indexedMap func xs =
    List.map2 func (List.map CDI_ZeroBased (List.range 0 (List.length xs - 1))) xs


{-| indexedTraverse and indexedForA are defined on `Utils`
-}



-- VERIFIED/INDEXED ZIP


type VerifiedList a
    = LengthMatch (List a)
    | LengthMismatch Int Int


indexedZipWith : (CDI_ZeroBased -> a -> b -> c) -> List a -> List b -> VerifiedList c
indexedZipWith func listX listY =
    indexedZipWithHelp func 0 listX listY []


indexedZipWithHelp : (CDI_ZeroBased -> a -> b -> c) -> Int -> List a -> List b -> List c -> VerifiedList c
indexedZipWithHelp func index listX listY revListZ =
    case ( listX, listY ) of
        ( [], [] ) ->
            LengthMatch (List.reverse revListZ)

        ( x :: xs, y :: ys ) ->
            indexedZipWithHelp func (index + 1) xs ys (func (CDI_ZeroBased index) x y :: revListZ)

        _ ->
            LengthMismatch (index + List.length listX) (index + List.length listY)



-- ENCODERS and DECODERS


zeroBasedEncoder : CDI_ZeroBased -> Encode.Value
zeroBasedEncoder (CDI_ZeroBased zeroBased) =
    Encode.int zeroBased


zeroBasedDecoder : Decode.Decoder CDI_ZeroBased
zeroBasedDecoder =
    Decode.map CDI_ZeroBased Decode.int
