module Utils.Crash exposing (crash)


crash : String -> a
crash str =
    Debug.todo str
