module Builder.BackgroundWriter exposing
    ( Scope
    , withScope
    , writeBinary
    )

import Builder.File as File
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO)
import Utils.Main as Utils



-- BACKGROUND WRITER


type Scope
    = Scope (Utils.MVar (List (Utils.MVar ())))


withScope : (Scope -> IO a) -> IO a
withScope callback =
    Utils.newMVar (Encode.list (\_ -> Encode.null)) []
        |> IO.bind
            (\workList ->
                callback (Scope workList)
                    |> IO.bind
                        (\result ->
                            Utils.takeMVar (Decode.list Utils.mVarDecoder) workList
                                |> IO.bind
                                    (\mvars ->
                                        Utils.listTraverse_ (Utils.takeMVar (Decode.succeed ())) mvars
                                            |> IO.fmap (\_ -> result)
                                    )
                        )
            )


writeBinary : (a -> Encode.Value) -> Scope -> String -> a -> IO ()
writeBinary encoder (Scope workList) path value =
    Utils.newEmptyMVar
        |> IO.bind
            (\mvar ->
                Utils.forkIO (File.writeBinary encoder path value |> IO.bind (\_ -> Utils.putMVar (\_ -> Encode.object []) mvar ()))
                    |> IO.bind
                        (\_ ->
                            Utils.takeMVar (Decode.list Utils.mVarDecoder) workList
                                |> IO.bind
                                    (\oldWork ->
                                        let
                                            newWork : List (Utils.MVar ())
                                            newWork =
                                                mvar :: oldWork
                                        in
                                        Utils.putMVar (Encode.list Utils.mVarEncoder) workList newWork
                                    )
                        )
            )
