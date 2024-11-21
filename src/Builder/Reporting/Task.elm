module Builder.Reporting.Task exposing
    ( Task
    , bind
    , eio
    , fmap
    , io
    , mapError
    , mio
    , pure
    , run
    , throw
    , void
    )

import System.IO as IO exposing (IO)



-- TASKS


type Task x a
    = Task (IO (Result x a))


run : Task x a -> IO (Result x a)
run (Task task) =
    task


throw : x -> Task x a
throw x =
    Task (IO.pure (Err x))


mapError : (x -> y) -> Task x a -> Task y a
mapError func (Task task) =
    Task (IO.fmap (Result.mapError func) task)



-- IO


io : IO a -> Task x a
io work =
    Task (IO.fmap Ok work)


mio : x -> IO (Maybe a) -> Task x a
mio x work =
    Task
        (IO.fmap
            (\result ->
                case result of
                    Just a ->
                        Ok a

                    Nothing ->
                        Err x
            )
            work
        )


eio : (x -> y) -> IO (Result x a) -> Task y a
eio func work =
    Task (IO.fmap (Result.mapError func) work)



-- INSTANCES


fmap : (a -> b) -> Task x a -> Task x b
fmap func (Task taskA) =
    Task (IO.fmap (Result.map func) taskA)


void : Task x a -> Task x ()
void =
    fmap (\_ -> ())


pure : a -> Task x a
pure a =
    Task (IO.pure (Ok a))


bind : (a -> Task x b) -> Task x a -> Task x b
bind callback (Task taskA) =
    Task
        (IO.bind
            (\resultA ->
                case Result.map callback resultA of
                    Ok (Task b) ->
                        b

                    Err err ->
                        IO.pure (Err err)
            )
            taskA
        )
