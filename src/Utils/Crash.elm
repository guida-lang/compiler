module Utils.Crash exposing (crash)


crash : String -> a
crash str =
    -- crash str
    Debug.todo str
