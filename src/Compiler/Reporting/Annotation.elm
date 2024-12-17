module Compiler.Reporting.Annotation exposing
    ( CRA_Located(..)
    , CRA_Position(..)
    , CRA_Region(..)
    , at
    , locatedDecoder
    , locatedEncoder
    , merge
    , mergeRegions
    , one
    , regionDecoder
    , regionEncoder
    , toRegion
    , toValue
    , traverse
    , zero
    )

import Json.Decode as Decode
import Json.Encode as Encode
import System.TypeCheck.IO as IO exposing (IO)



-- LOCATED


type CRA_Located a
    = CRA_At CRA_Region a -- PERF see if unpacking region is helpful


traverse : (a -> IO b) -> CRA_Located a -> IO (CRA_Located b)
traverse func (CRA_At region value) =
    IO.fmap (CRA_At region) (func value)


toValue : CRA_Located a -> a
toValue (CRA_At _ value) =
    value


merge : CRA_Located a -> CRA_Located b -> c -> CRA_Located c
merge (CRA_At r1 _) (CRA_At r2 _) value =
    CRA_At (mergeRegions r1 r2) value



-- POSITION


type CRA_Position
    = CRA_Position Int Int


at : CRA_Position -> CRA_Position -> a -> CRA_Located a
at start end a =
    CRA_At (CRA_Region start end) a



-- REGION


type CRA_Region
    = CRA_Region CRA_Position CRA_Position


toRegion : CRA_Located a -> CRA_Region
toRegion (CRA_At region _) =
    region


mergeRegions : CRA_Region -> CRA_Region -> CRA_Region
mergeRegions (CRA_Region start _) (CRA_Region _ end) =
    CRA_Region start end


zero : CRA_Region
zero =
    CRA_Region (CRA_Position 0 0) (CRA_Position 0 0)


one : CRA_Region
one =
    CRA_Region (CRA_Position 1 1) (CRA_Position 1 1)



-- ENCODERS and DECODERS


regionEncoder : CRA_Region -> Encode.Value
regionEncoder (CRA_Region start end) =
    Encode.object
        [ ( "type", Encode.string "Region" )
        , ( "start", positionEncoder start )
        , ( "end", positionEncoder end )
        ]


regionDecoder : Decode.Decoder CRA_Region
regionDecoder =
    Decode.map2 CRA_Region
        (Decode.field "start" positionDecoder)
        (Decode.field "end" positionDecoder)


positionEncoder : CRA_Position -> Encode.Value
positionEncoder (CRA_Position start end) =
    Encode.object
        [ ( "type", Encode.string "Position" )
        , ( "start", Encode.int start )
        , ( "end", Encode.int end )
        ]


positionDecoder : Decode.Decoder CRA_Position
positionDecoder =
    Decode.map2 CRA_Position
        (Decode.field "start" Decode.int)
        (Decode.field "end" Decode.int)


locatedEncoder : (a -> Encode.Value) -> CRA_Located a -> Encode.Value
locatedEncoder encoder (CRA_At region value) =
    Encode.object
        [ ( "type", Encode.string "Located" )
        , ( "region", regionEncoder region )
        , ( "value", encoder value )
        ]


locatedDecoder : Decode.Decoder a -> Decode.Decoder (CRA_Located a)
locatedDecoder decoder =
    Decode.map2 CRA_At
        (Decode.field "region" regionDecoder)
        (Decode.field "value" (Decode.lazy (\_ -> decoder)))
