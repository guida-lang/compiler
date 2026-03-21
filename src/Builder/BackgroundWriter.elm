module Builder.BackgroundWriter exposing
    ( Scope
    , withScope
    , writeBinary
    )

import Builder.File as File
import Control.Concurrent.MVar as MVar exposing (MVar)
import Process
import Task exposing (Task)
import Utils.Bytes.Encode as BE
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- BACKGROUND WRITER


type Scope
    = Scope (MVar (List (MVar ())))


withScope : (Scope -> Task Never a) -> Task Never a
withScope callback =
    MVar.newMVar []
        |> Task.bind
            (\workList ->
                callback (Scope workList)
                    |> Task.bind
                        (\result ->
                            MVar.takeMVar workList
                                |> Task.bind
                                    (\mvars ->
                                        Utils.listTraverse_ MVar.takeMVar mvars
                                            |> Task.fmap (\_ -> result)
                                    )
                        )
            )


writeBinary : (a -> BE.Encoder) -> Scope -> String -> a -> Task Never ()
writeBinary toEncoder (Scope workList) path value =
    MVar.newEmptyMVar
        |> Task.bind
            (\mvar ->
                Process.spawn
                    (File.writeBinary toEncoder path value
                        |> Task.bind (\_ -> MVar.putMVar mvar ())
                    )
                    |> Task.bind
                        (\_ ->
                            MVar.takeMVar workList
                                |> Task.bind
                                    (\oldWork ->
                                        let
                                            newWork : List (MVar ())
                                            newWork =
                                                mvar :: oldWork
                                        in
                                        MVar.putMVar workList newWork
                                    )
                        )
            )
