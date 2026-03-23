module Utils.Task.Extra exposing
    ( apply
    , eio
    , io
    , mapM
    , mio
    , run
    , void
    )

import Task exposing (Task)



-- TASKS


run : Task x a -> Task Never (Result x a)
run task =
    task
        |> Task.map Ok
        |> Task.onError (Err >> Task.succeed)



-- IO


io : Task Never a -> Task x a
io work =
    Task.mapError never work


mio : x -> Task Never (Maybe a) -> Task x a
mio x work =
    work
        |> Task.mapError never
        |> Task.andThen
            (\m ->
                case m of
                    Just a ->
                        Task.succeed a

                    Nothing ->
                        Task.fail x
            )


eio : (x -> y) -> Task Never (Result x a) -> Task y a
eio func work =
    work
        |> Task.mapError never
        |> Task.andThen
            (\m ->
                case m of
                    Ok a ->
                        Task.succeed a

                    Err err ->
                        func err |> Task.fail
            )



-- INSTANCES


void : Task x a -> Task x ()
void =
    Task.map (always ())


apply : Task x a -> Task x (a -> b) -> Task x b
apply ma mf =
    Task.andThen (\f -> Task.andThen (Task.succeed << f) ma) mf


mapM : (a -> Task x b) -> List a -> Task x (List b)
mapM f =
    List.map f >> Task.sequence
