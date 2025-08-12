module Builder.BackgroundWriter exposing
    ( Scope
    , withScope
    , writeBinary
    )

import Builder.File as File
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- BACKGROUND WRITER


type Scope
    = Scope (Utils.MVar (List (Utils.MVar ())))


withScope : (Scope -> Task Never a) -> Task Never a
withScope callback =
    Utils.newMVar (BE.list (\_ -> BE.unit ())) []
        |> Task.bind
            (\workList ->
                callback (Scope workList)
                    |> Task.bind
                        (\result ->
                            Utils.takeMVar (BD.list Utils.mVarDecoder) workList
                                |> Task.bind
                                    (\mvars ->
                                        Utils.listTraverse_ (Utils.takeMVar (BD.succeed ())) mvars
                                            |> Task.fmap (\_ -> result)
                                    )
                        )
            )


writeBinary : (a -> BE.Encoder) -> Scope -> String -> a -> Task Never ()
writeBinary toEncoder (Scope workList) path value =
    Utils.newEmptyMVar
        |> Task.bind
            (\mvar ->
                Utils.forkIO
                    (File.writeBinary toEncoder path value
                        |> Task.bind (\_ -> Utils.putMVar BE.unit mvar ())
                    )
                    |> Task.bind
                        (\_ ->
                            Utils.takeMVar (BD.list Utils.mVarDecoder) workList
                                |> Task.bind
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
