module Compiler.Type.UnionFind exposing
    ( equivalent
    , fresh
    , get
    , modify
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

import Data.IORef as IORef exposing (IORef(..))
import System.TypeCheck.IO as IO exposing (Descriptor, IO)
import Utils.Crash exposing (crash)



-- HELPERS


fresh : IO.Descriptor -> IO IO.Point
fresh value =
    IORef.newIORefWeight 1
        |> IO.bind
            (\(IORef weight) ->
                IORef.newIORefDescriptor value
                    |> IO.bind (\(IORef desc) -> IORef.newIORefPointInfo (IO.Info weight desc))
                    |> IO.fmap (\(IORef link) -> IO.Pt link)
            )


repr : IO.Point -> IO IO.Point
repr ((IO.Pt ref) as point) =
    IORef.readIORefPointInfo (IORef ref)
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    IO.Info _ _ ->
                        IO.pure point

                    IO.Link ((IO.Pt ref1) as point1) ->
                        repr point1
                            |> IO.bind
                                (\point2 ->
                                    if point2 /= point1 then
                                        IORef.readIORefPointInfo (IORef ref1)
                                            |> IO.bind
                                                (\pInfo1 ->
                                                    IORef.writeIORefPointInfo (IORef ref) pInfo1
                                                        |> IO.fmap (\_ -> point2)
                                                )

                                    else
                                        IO.pure point2
                                )
            )


get : IO.Point -> IO Descriptor
get ((IO.Pt ref) as point) =
    IORef.readIORefPointInfo (IORef ref)
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    IO.Info _ descRef ->
                        IORef.readIORefDescriptor (IORef descRef)

                    IO.Link (IO.Pt ref1) ->
                        IORef.readIORefPointInfo (IORef ref1)
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        IO.Info _ descRef ->
                                            IORef.readIORefDescriptor (IORef descRef)

                                        IO.Link _ ->
                                            IO.bind get (repr point)
                                )
            )


set : IO.Point -> Descriptor -> IO ()
set ((IO.Pt ref) as point) newDesc =
    IORef.readIORefPointInfo (IORef ref)
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    IO.Info _ descRef ->
                        IORef.writeIORefDescriptor (IORef descRef) newDesc

                    IO.Link (IO.Pt ref1) ->
                        IORef.readIORefPointInfo (IORef ref1)
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        IO.Info _ descRef ->
                                            IORef.writeIORefDescriptor (IORef descRef) newDesc

                                        IO.Link _ ->
                                            repr point
                                                |> IO.bind
                                                    (\newPoint ->
                                                        set newPoint newDesc
                                                    )
                                )
            )


modify : IO.Point -> (Descriptor -> Descriptor) -> IO ()
modify ((IO.Pt ref) as point) func =
    IORef.readIORefPointInfo (IORef ref)
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    IO.Info _ descRef ->
                        IORef.modifyIORefDescriptor (IORef descRef) func

                    IO.Link (IO.Pt ref1) ->
                        IORef.readIORefPointInfo (IORef ref1)
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        IO.Info _ descRef ->
                                            IORef.modifyIORefDescriptor (IORef descRef) func

                                        IO.Link _ ->
                                            repr point
                                                |> IO.bind (\newPoint -> modify newPoint func)
                                )
            )


union : IO.Point -> IO.Point -> IO.Descriptor -> IO ()
union p1 p2 newDesc =
    repr p1
        |> IO.bind
            (\((IO.Pt ref1) as point1) ->
                repr p2
                    |> IO.bind
                        (\((IO.Pt ref2) as point2) ->
                            IORef.readIORefPointInfo (IORef ref1)
                                |> IO.bind
                                    (\pointInfo1 ->
                                        IORef.readIORefPointInfo (IORef ref2)
                                            |> IO.bind
                                                (\pointInfo2 ->
                                                    case ( pointInfo1, pointInfo2 ) of
                                                        ( IO.Info w1 d1, IO.Info w2 d2 ) ->
                                                            if point1 == point2 then
                                                                IORef.writeIORefDescriptor (IORef d1) newDesc

                                                            else
                                                                IORef.readIORefWeight (IORef w1)
                                                                    |> IO.bind
                                                                        (\weight1 ->
                                                                            IORef.readIORefWeight (IORef w2)
                                                                                |> IO.bind
                                                                                    (\weight2 ->
                                                                                        let
                                                                                            newWeight : Int
                                                                                            newWeight =
                                                                                                weight1 + weight2
                                                                                        in
                                                                                        if weight1 >= weight2 then
                                                                                            IORef.writeIORefPointInfo (IORef ref2) (IO.Link point1)
                                                                                                |> IO.bind (\_ -> IORef.writeIORefWeight (IORef w1) newWeight)
                                                                                                |> IO.bind (\_ -> IORef.writeIORefDescriptor (IORef d1) newDesc)

                                                                                        else
                                                                                            IORef.writeIORefPointInfo (IORef ref1) (IO.Link point2)
                                                                                                |> IO.bind (\_ -> IORef.writeIORefWeight (IORef w2) newWeight)
                                                                                                |> IO.bind (\_ -> IORef.writeIORefDescriptor (IORef d2) newDesc)
                                                                                    )
                                                                        )

                                                        _ ->
                                                            crash "Unexpected pattern"
                                                )
                                    )
                        )
            )


equivalent : IO.Point -> IO.Point -> IO Bool
equivalent p1 p2 =
    repr p1
        |> IO.bind
            (\v1 ->
                repr p2
                    |> IO.fmap (\v2 -> v1 == v2)
            )


redundant : IO.Point -> IO Bool
redundant (IO.Pt ref) =
    IORef.readIORefPointInfo (IORef ref)
        |> IO.fmap
            (\pInfo ->
                case pInfo of
                    IO.Info _ _ ->
                        False

                    IO.Link _ ->
                        True
            )
