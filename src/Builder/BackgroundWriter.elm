module Builder.BackgroundWriter exposing
    ( Scope
    , withScope
    , writeBinary
    )

import Builder.File as File
import Data.IO as IO exposing (IO)
import Json.Decode as Decode
import Json.Encode as Encode
import Serialize exposing (Codec)
import Utils.Main as Utils



-- BACKGROUND WRITER


type Scope
    = Scope (Utils.MVar (List (Utils.MVar ())))


withScope : (Scope -> IO a) -> IO a
withScope callback =
    Utils.newMVar (Serialize.list Utils.mVarCodec) []
        |> IO.bind
            (\workList ->
                callback (Scope workList)
                    |> IO.bind
                        (\result ->
                            Utils.takeMVar (Serialize.list Utils.mVarCodec) workList
                                |> IO.bind
                                    (\mvars ->
                                        Utils.listTraverse_ (Utils.takeMVar Serialize.unit) mvars
                                            |> IO.fmap (\_ -> result)
                                    )
                        )
            )


writeBinary : Codec e a -> Scope -> String -> a -> IO ()
writeBinary codec (Scope workList) path value =
    Utils.newEmptyMVar
        |> IO.bind
            (\mvar ->
                Utils.forkIO (File.writeBinary codec path value |> IO.bind (\_ -> Utils.putMVar Serialize.unit mvar ()))
                    |> IO.bind
                        (\_ ->
                            Utils.takeMVar (Serialize.list Utils.mVarCodec) workList
                                |> IO.bind
                                    (\oldWork ->
                                        let
                                            newWork : List (Utils.MVar ())
                                            newWork =
                                                mvar :: oldWork
                                        in
                                        Utils.putMVar (Serialize.list Utils.mVarCodec) workList newWork
                                    )
                        )
            )
