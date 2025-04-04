module Builder.BackgroundWriter exposing
    ( Scope
    , withScope
    , writeBinary
    )

import Builder.File as File
import System.IO as IO exposing (IO)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Main as Utils



-- BACKGROUND WRITER


type Scope
    = Scope (Utils.MVar (List (Utils.MVar ())))


withScope : (Scope -> IO a) -> IO a
withScope callback =
    Utils.newMVar (BE.list (\_ -> BE.unit ())) []
        |> IO.bind
            (\workList ->
                callback (Scope workList)
                    |> IO.bind
                        (\result ->
                            Utils.takeMVar (BD.list Utils.mVarDecoder) workList
                                |> IO.bind
                                    (\mvars ->
                                        Utils.listTraverse_ (Utils.takeMVar (BD.succeed ())) mvars
                                            |> IO.fmap (\_ -> result)
                                    )
                        )
            )


writeBinary : (a -> BE.Encoder) -> Scope -> String -> a -> IO ()
writeBinary toEncoder (Scope workList) path value =
    Utils.newEmptyMVar
        |> IO.bind
            (\mvar ->
                Utils.forkIO
                    (File.writeBinary toEncoder path value
                        |> IO.bind (\_ -> Utils.putMVar BE.unit mvar ())
                    )
                    |> IO.bind
                        (\_ ->
                            Utils.takeMVar (BD.list Utils.mVarDecoder) workList
                                |> IO.bind
                                    (\oldWork ->
                                        let
                                            newWork : List (Utils.MVar ())
                                            newWork =
                                                mvar :: oldWork
                                        in
                                        Utils.putMVar (BE.list Utils.mVarEncoder) workList newWork
                                    )
                        )
            )
