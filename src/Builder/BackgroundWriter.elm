module Builder.BackgroundWriter exposing
    ( Scope
    , withScope
    , writeBinary
    )

import Builder.File as File
import Utils.Task.Extra as TE
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
        |> TE.bind
            (\workList ->
                callback (Scope workList)
                    |> TE.bind
                        (\result ->
                            Utils.takeMVar (BD.list Utils.mVarDecoder) workList
                                |> TE.bind
                                    (\mvars ->
                                        Utils.listTraverse_ (Utils.takeMVar (BD.succeed ())) mvars
                                            |> TE.fmap (\_ -> result)
                                    )
                        )
            )


writeBinary : (a -> BE.Encoder) -> Scope -> String -> a -> Task Never ()
writeBinary toEncoder (Scope workList) path value =
    Utils.newEmptyMVar
        |> TE.bind
            (\mvar ->
                Utils.forkIO
                    (File.writeBinary toEncoder path value
                        |> TE.bind (\_ -> Utils.putMVar BE.unit mvar ())
                    )
                    |> TE.bind
                        (\_ ->
                            Utils.takeMVar (BD.list Utils.mVarDecoder) workList
                                |> TE.bind
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
