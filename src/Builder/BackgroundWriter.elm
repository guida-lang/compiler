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



-- BACKGROUND WRITER


type Scope
    = Scope (Utils.MVar (List (Utils.MVar ())))


withScope : (Scope -> Task Never a) -> Task Never a
withScope callback =
    Utils.newMVar (BE.list (\_ -> BE.unit ())) []
        |> Task.andThen
            (\workList ->
                callback (Scope workList)
                    |> Task.andThen
                        (\result ->
                            Utils.takeMVar (BD.list Utils.mVarDecoder) workList
                                |> Task.andThen
                                    (\mvars ->
                                        Utils.listTraverse_ (Utils.takeMVar (BD.succeed ())) mvars
                                            |> Task.map (\_ -> result)
                                    )
                        )
            )


writeBinary : (a -> BE.Encoder) -> Scope -> String -> a -> Task Never ()
writeBinary toEncoder (Scope workList) path value =
    Utils.newEmptyMVar
        |> Task.andThen
            (\mvar ->
                Utils.forkIO
                    (File.writeBinary toEncoder path value
                        |> Task.andThen (\_ -> Utils.putMVar BE.unit mvar ())
                    )
                    |> Task.andThen
                        (\_ ->
                            Utils.takeMVar (BD.list Utils.mVarDecoder) workList
                                |> Task.andThen
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
