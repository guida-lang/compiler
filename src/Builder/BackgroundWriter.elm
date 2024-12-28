module Builder.BackgroundWriter exposing
    ( withScope
    , writeBinary
    )

import Builder.File as File
import Json.Encode as Encode
import System.IO as IO
import Types as T exposing (IO)
import Utils.Main as Utils



-- BACKGROUND WRITER


withScope : (T.BBW_Scope -> IO a) -> IO a
withScope callback =
    Utils.newMVar_ListMVar []
        |> IO.bind
            (\workList ->
                callback (T.BBW_Scope workList)
                    |> IO.bind
                        (\result ->
                            Utils.takeMVar_ListMVar workList
                                |> IO.bind
                                    (\mvars ->
                                        Utils.listTraverse_ Utils.takeMVar_Unit mvars
                                            |> IO.fmap (\_ -> result)
                                    )
                        )
            )


writeBinary : (a -> Encode.Value) -> T.BBW_Scope -> String -> a -> IO ()
writeBinary encoder (T.BBW_Scope workList) path value =
    Utils.newEmptyMVar_Unit
        |> IO.bind
            (\mvar ->
                Utils.forkIO (File.writeBinary encoder path value |> IO.bind (\_ -> Utils.putMVar_Unit mvar ()))
                    |> IO.bind
                        (\_ ->
                            Utils.takeMVar_ListMVar workList
                                |> IO.bind
                                    (\oldWork ->
                                        let
                                            newWork : List T.MVar_Unit
                                            newWork =
                                                mvar :: oldWork
                                        in
                                        Utils.putMVar_ListMVar workList newWork
                                    )
                        )
            )
