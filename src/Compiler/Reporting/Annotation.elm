module Compiler.Reporting.Annotation exposing
    ( Located(..)
    , Position(..)
    , Region(..)
    , at
    , locatedCodec
    , merge
    , mergeRegions
    , one
    , regionCodec
    , toRegion
    , toValue
    , traverse
    , zero
    )

import Serialize exposing (Codec)
import System.TypeCheck.IO as IO exposing (IO)



-- LOCATED


type Located a
    = At Region a -- PERF see if unpacking region is helpful


traverse : (a -> IO b) -> Located a -> IO (Located b)
traverse func (At region value) =
    IO.fmap (At region) (func value)


toValue : Located a -> a
toValue (At _ value) =
    value


merge : Located a -> Located b -> c -> Located c
merge (At r1 _) (At r2 _) value =
    At (mergeRegions r1 r2) value



-- POSITION


type Position
    = Position Int Int


at : Position -> Position -> a -> Located a
at start end a =
    At (Region start end) a



-- REGION


type Region
    = Region Position Position


toRegion : Located a -> Region
toRegion (At region _) =
    region


mergeRegions : Region -> Region -> Region
mergeRegions (Region start _) (Region _ end) =
    Region start end


zero : Region
zero =
    Region (Position 0 0) (Position 0 0)


one : Region
one =
    Region (Position 1 1) (Position 1 1)



-- ENCODERS and DECODERS


regionCodec : Codec e Region
regionCodec =
    Serialize.customType
        (\regionCodecEncoder (Region start end) ->
            regionCodecEncoder start end
        )
        |> Serialize.variant2 Region positionCodec positionCodec
        |> Serialize.finishCustomType


positionCodec : Codec e Position
positionCodec =
    Serialize.customType
        (\positionCodecEncoder (Position start end) ->
            positionCodecEncoder start end
        )
        |> Serialize.variant2 Position Serialize.int Serialize.int
        |> Serialize.finishCustomType


locatedCodec : Codec e a -> Codec e (Located a)
locatedCodec a =
    Serialize.customType
        (\atEncoder (At region value) ->
            atEncoder region value
        )
        |> Serialize.variant2 At regionCodec a
        |> Serialize.finishCustomType
