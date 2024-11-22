module Compiler.Type.UnionFind exposing
    ( Point
    , equivalent
    , fresh
    , get
    , modify
    , pointDecoder
    , pointEncoder
    , redundant
    , set
    , union
    )

{- This is based on the following implementations:

     - https://hackage.haskell.org/package/union-find-0.2/docs/src/Data-UnionFind-IO.html
     - http://yann.regis-gianas.org/public/mini/code_UnionFind.html

   It seems like the OCaml one came first, but I am not sure.

   Compared to the Haskell implementation, the major changes here include:

     1. No more reallocating PointInfo when changing the weight
     2. Using the strict modifyIORef

-}

import Data.IORef as IORef exposing (IORef)
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO)
import Utils.Crash exposing (crash)



-- POINT


type Point a
    = Pt (IORef (PointInfo a))


pointEncoder : Point a -> Encode.Value
pointEncoder (Pt ioRef) =
    IORef.ioRefEncoder ioRef


pointDecoder : Decode.Decoder (Point a)
pointDecoder =
    Decode.map Pt IORef.ioRefDecoder


type PointInfo a
    = Info (IORef Int) (IORef a)
    | Link (Point a)


pointInfoEncoder : PointInfo a -> Encode.Value
pointInfoEncoder pointInfo =
    case pointInfo of
        Info weight desc ->
            Encode.object
                [ ( "type", Encode.string "Info" )
                , ( "weight", IORef.ioRefEncoder weight )
                , ( "desc", IORef.ioRefEncoder desc )
                ]

        Link point ->
            Encode.object
                [ ( "type", Encode.string "Link" )
                , ( "point", pointEncoder point )
                ]


pointInfoDecoder : Decode.Decoder (PointInfo a)
pointInfoDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Info" ->
                        Decode.map2 Info
                            (Decode.field "weight" IORef.ioRefDecoder)
                            (Decode.field "desc" IORef.ioRefDecoder)

                    "Link" ->
                        Decode.map Link
                            (Decode.field "point" pointDecoder)

                    _ ->
                        Decode.fail ("Unknown PointInfo's type: " ++ type_)
            )



-- HELPERS


fresh : (a -> Encode.Value) -> a -> IO (Point a)
fresh encoder value =
    IORef.newIORef Encode.int 1
        |> IO.bind
            (\weight ->
                IORef.newIORef encoder value
                    |> IO.bind (\desc -> IORef.newIORef pointInfoEncoder (Info weight desc))
                    |> IO.fmap (\link -> Pt link)
            )


repr : Point a -> IO (Point a)
repr ((Pt ref) as point) =
    IORef.readIORef pointInfoDecoder ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ _ ->
                        IO.pure point

                    Link ((Pt ref1) as point1) ->
                        repr point1
                            |> IO.bind
                                (\point2 ->
                                    if point2 /= point1 then
                                        IORef.readIORef pointInfoDecoder ref1
                                            |> IO.bind
                                                (\pInfo1 ->
                                                    IORef.writeIORef pointInfoEncoder ref pInfo1
                                                        |> IO.fmap (\_ -> point2)
                                                )

                                    else
                                        IO.pure point2
                                )
            )


get : Decode.Decoder a -> Point a -> IO a
get decoder ((Pt ref) as point) =
    IORef.readIORef pointInfoDecoder ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IORef.readIORef decoder descRef

                    Link (Pt ref1) ->
                        IORef.readIORef pointInfoDecoder ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IORef.readIORef decoder descRef

                                        Link _ ->
                                            IO.bind (get decoder) (repr point)
                                )
            )


set : (a -> Encode.Value) -> Point a -> a -> IO ()
set encoder ((Pt ref) as point) newDesc =
    IORef.readIORef pointInfoDecoder ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IORef.writeIORef encoder descRef newDesc

                    Link (Pt ref1) ->
                        IORef.readIORef pointInfoDecoder ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IORef.writeIORef encoder descRef newDesc

                                        Link _ ->
                                            repr point
                                                |> IO.bind
                                                    (\newPoint ->
                                                        set encoder newPoint newDesc
                                                    )
                                )
            )


modify : Decode.Decoder a -> (a -> Encode.Value) -> Point a -> (a -> a) -> IO ()
modify decoder encoder ((Pt ref) as point) func =
    IORef.readIORef pointInfoDecoder ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IORef.modifyIORef decoder encoder descRef func

                    Link (Pt ref1) ->
                        IORef.readIORef pointInfoDecoder ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IORef.modifyIORef decoder encoder descRef func

                                        Link _ ->
                                            repr point
                                                |> IO.bind (\newPoint -> modify decoder encoder newPoint func)
                                )
            )


union : (a -> Encode.Value) -> Point a -> Point a -> a -> IO ()
union encoder p1 p2 newDesc =
    repr p1
        |> IO.bind
            (\((Pt ref1) as point1) ->
                repr p2
                    |> IO.bind
                        (\((Pt ref2) as point2) ->
                            IORef.readIORef pointInfoDecoder ref1
                                |> IO.bind
                                    (\pointInfo1 ->
                                        IORef.readIORef pointInfoDecoder ref2
                                            |> IO.bind
                                                (\pointInfo2 ->
                                                    case ( pointInfo1, pointInfo2 ) of
                                                        ( Info w1 d1, Info w2 d2 ) ->
                                                            if point1 == point2 then
                                                                IORef.writeIORef encoder d1 newDesc

                                                            else
                                                                IORef.readIORef Decode.int w1
                                                                    |> IO.bind
                                                                        (\weight1 ->
                                                                            IORef.readIORef Decode.int w2
                                                                                |> IO.bind
                                                                                    (\weight2 ->
                                                                                        let
                                                                                            newWeight : Int
                                                                                            newWeight =
                                                                                                weight1 + weight2
                                                                                        in
                                                                                        if weight1 >= weight2 then
                                                                                            IORef.writeIORef pointInfoEncoder ref2 (Link point1)
                                                                                                |> IO.bind (\_ -> IORef.writeIORef Encode.int w1 newWeight)
                                                                                                |> IO.bind (\_ -> IORef.writeIORef encoder d1 newDesc)

                                                                                        else
                                                                                            IORef.writeIORef pointInfoEncoder ref1 (Link point2)
                                                                                                |> IO.bind (\_ -> IORef.writeIORef Encode.int w2 newWeight)
                                                                                                |> IO.bind (\_ -> IORef.writeIORef encoder d2 newDesc)
                                                                                    )
                                                                        )

                                                        _ ->
                                                            crash "Unexpected pattern"
                                                )
                                    )
                        )
            )


equivalent : Point a -> Point a -> IO Bool
equivalent p1 p2 =
    repr p1
        |> IO.bind
            (\v1 ->
                repr p2
                    |> IO.fmap (\v2 -> v1 == v2)
            )


redundant : Point a -> IO Bool
redundant (Pt ref) =
    IORef.readIORef pointInfoDecoder ref
        |> IO.fmap
            (\pInfo ->
                case pInfo of
                    Info _ _ ->
                        False

                    Link _ ->
                        True
            )
