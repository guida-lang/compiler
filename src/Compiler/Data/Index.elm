module Compiler.Data.Index exposing
    ( VerifiedList(..)
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
import Types as T



-- ZERO BASED


first : T.CDI_ZeroBased
first =
    T.CDI_ZeroBased 0


second : T.CDI_ZeroBased
second =
    T.CDI_ZeroBased 1


third : T.CDI_ZeroBased
third =
    T.CDI_ZeroBased 2


next : T.CDI_ZeroBased -> T.CDI_ZeroBased
next (T.CDI_ZeroBased i) =
    T.CDI_ZeroBased (i + 1)



-- DESTRUCT


toMachine : T.CDI_ZeroBased -> Int
toMachine (T.CDI_ZeroBased index) =
    index


toHuman : T.CDI_ZeroBased -> Int
toHuman (T.CDI_ZeroBased index) =
    index + 1



-- INDEXED MAP


indexedMap : (T.CDI_ZeroBased -> a -> b) -> List a -> List b
indexedMap func xs =
    List.map2 func (List.map T.CDI_ZeroBased (List.range 0 (List.length xs - 1))) xs


{-| indexedTraverse and indexedForA are defined on `Utils`
-}



-- VERIFIED/INDEXED ZIP


type VerifiedList a
    = LengthMatch (List a)
    | LengthMismatch Int Int


indexedZipWith : (T.CDI_ZeroBased -> a -> b -> c) -> List a -> List b -> VerifiedList c
indexedZipWith func listX listY =
    indexedZipWithHelp func 0 listX listY []


indexedZipWithHelp : (T.CDI_ZeroBased -> a -> b -> c) -> Int -> List a -> List b -> List c -> VerifiedList c
indexedZipWithHelp func index listX listY revListZ =
    case ( listX, listY ) of
        ( [], [] ) ->
            LengthMatch (List.reverse revListZ)

        ( x :: xs, y :: ys ) ->
            indexedZipWithHelp func (index + 1) xs ys (func (T.CDI_ZeroBased index) x y :: revListZ)

        _ ->
            LengthMismatch (index + List.length listX) (index + List.length listY)



-- ENCODERS and DECODERS


zeroBasedEncoder : T.CDI_ZeroBased -> Encode.Value
zeroBasedEncoder (T.CDI_ZeroBased zeroBased) =
    Encode.int zeroBased


zeroBasedDecoder : Decode.Decoder T.CDI_ZeroBased
zeroBasedDecoder =
    Decode.map T.CDI_ZeroBased Decode.int
