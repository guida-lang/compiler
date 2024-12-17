module Compiler.Reporting.Annotation exposing
    ( at
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
import Types as T



-- LOCATED


traverse : (a -> IO b) -> T.CRA_Located a -> IO (T.CRA_Located b)
traverse func (T.CRA_At region value) =
    IO.fmap (T.CRA_At region) (func value)


toValue : T.CRA_Located a -> a
toValue (T.CRA_At _ value) =
    value


merge : T.CRA_Located a -> T.CRA_Located b -> c -> T.CRA_Located c
merge (T.CRA_At r1 _) (T.CRA_At r2 _) value =
    T.CRA_At (mergeRegions r1 r2) value



-- POSITION


at : T.CRA_Position -> T.CRA_Position -> a -> T.CRA_Located a
at start end a =
    T.CRA_At (T.CRA_Region start end) a



-- REGION


toRegion : T.CRA_Located a -> T.CRA_Region
toRegion (T.CRA_At region _) =
    region


mergeRegions : T.CRA_Region -> T.CRA_Region -> T.CRA_Region
mergeRegions (T.CRA_Region start _) (T.CRA_Region _ end) =
    T.CRA_Region start end


zero : T.CRA_Region
zero =
    T.CRA_Region (T.CRA_Position 0 0) (T.CRA_Position 0 0)


one : T.CRA_Region
one =
    T.CRA_Region (T.CRA_Position 1 1) (T.CRA_Position 1 1)



-- ENCODERS and DECODERS


regionEncoder : T.CRA_Region -> Encode.Value
regionEncoder (T.CRA_Region start end) =
    Encode.object
        [ ( "type", Encode.string "Region" )
        , ( "start", positionEncoder start )
        , ( "end", positionEncoder end )
        ]


regionDecoder : Decode.Decoder T.CRA_Region
regionDecoder =
    Decode.map2 T.CRA_Region
        (Decode.field "start" positionDecoder)
        (Decode.field "end" positionDecoder)


positionEncoder : T.CRA_Position -> Encode.Value
positionEncoder (T.CRA_Position start end) =
    Encode.object
        [ ( "type", Encode.string "Position" )
        , ( "start", Encode.int start )
        , ( "end", Encode.int end )
        ]


positionDecoder : Decode.Decoder T.CRA_Position
positionDecoder =
    Decode.map2 T.CRA_Position
        (Decode.field "start" Decode.int)
        (Decode.field "end" Decode.int)


locatedEncoder : (a -> Encode.Value) -> T.CRA_Located a -> Encode.Value
locatedEncoder encoder (T.CRA_At region value) =
    Encode.object
        [ ( "type", Encode.string "Located" )
        , ( "region", regionEncoder region )
        , ( "value", encoder value )
        ]


locatedDecoder : Decode.Decoder a -> Decode.Decoder (T.CRA_Located a)
locatedDecoder decoder =
    Decode.map2 T.CRA_At
        (Decode.field "region" regionDecoder)
        (Decode.field "value" (Decode.lazy (\_ -> decoder)))
