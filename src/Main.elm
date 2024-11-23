module Main exposing (main)

import System.IO as IO exposing (IO)


main : IO.Program
main =
    IO.run app


app : IO ()
app =
    IO.pure 1
        |> IO.fmap ((+) 2)
        |> IO.fmap String.fromInt
        |> IO.bind (IO.hPutStr IO.stdout)
        |> IO.bind (\() -> IO.getLine)
        |> IO.bind (\name -> IO.hPutStr IO.stdout ("Hello " ++ name))
