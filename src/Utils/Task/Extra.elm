module Utils.Task.Extra exposing
    ( apply
    , bind
    , eio
    , fmap
    , io
    , mapM
    , mio
    , pure
    , run
    , throw
    , void
    )

import Task exposing (Task)



-- TASKS


run : Task x a -> Task Never (Result x a)
run task =
    task
        |> Task.map Ok
        |> Task.onError (Err >> Task.succeed)


throw : x -> Task x a
throw =
    Task.fail



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


pure : a -> Task x a
pure =
    Task.succeed


apply : Task x a -> Task x (a -> b) -> Task x b
apply ma mf =
    bind (\f -> bind (pure << f) ma) mf


fmap : (a -> b) -> Task x a -> Task x b
fmap =
    Task.map


bind : (a -> Task x b) -> Task x a -> Task x b
bind =
    Task.andThen


mapM : (a -> Task x b) -> List a -> Task x (List b)
mapM f =
    List.map f >> Task.sequence
