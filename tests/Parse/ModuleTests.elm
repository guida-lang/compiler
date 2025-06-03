module Parse.ModuleTests exposing (suite)

import Compiler.AST.Source as S
import Compiler.Parse.Module as M
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parse.Module"
        [ Test.describe "fromByteString"
            [ Test.test "Hello!" <|
                \_ ->
                    M.fromByteString SV.Elm M.Application """module Hello exposing (..)

import Html exposing (text)


main =
  text "Hello!"
                    """
                        |> Expect.equal
                            (Ok
                                (S.Module
                                    SV.Elm
                                    (Just (A.at (A.Position 1 8) (A.Position 1 13) "Hello"))
                                    (A.at (A.Position 1 23) (A.Position 1 27) S.Open)
                                    (S.NoDocs (A.Region (A.Position 1 27) (A.Position 3 1)) [])
                                    [ S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Platform.Sub") (Just "Sub") (S.Explicit [ S.Upper (A.at (A.Position 0 0) (A.Position 0 0) "Sub") S.Private ])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Platform.Cmd") (Just "Cmd") (S.Explicit [ S.Upper (A.at (A.Position 0 0) (A.Position 0 0) "Cmd") S.Private ])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Platform") Nothing (S.Explicit [ S.Upper (A.at (A.Position 0 0) (A.Position 0 0) "Program") S.Private ])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Tuple") Nothing (S.Explicit [])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Char") Nothing (S.Explicit [ S.Upper (A.at (A.Position 0 0) (A.Position 0 0) "Char") S.Private ])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "String") Nothing (S.Explicit [ S.Upper (A.at (A.Position 0 0) (A.Position 0 0) "String") S.Private ])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Result") Nothing (S.Explicit [ S.Upper (A.at (A.Position 0 0) (A.Position 0 0) "Result") (S.Public (A.Region (A.Position 0 0) (A.Position 0 0))) ])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Maybe") Nothing (S.Explicit [ S.Upper (A.at (A.Position 0 0) (A.Position 0 0) "Maybe") (S.Public (A.Region (A.Position 0 0) (A.Position 0 0))) ])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "List") Nothing (S.Explicit [ S.Operator (A.Region (A.Position 0 0) (A.Position 0 0)) "::" ])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Debug") Nothing (S.Explicit [])
                                    , S.Import (A.at (A.Position 0 0) (A.Position 0 0) "Basics") Nothing S.Open
                                    , S.Import (A.at (A.Position 3 8) (A.Position 3 12) "Html") Nothing (S.Explicit [ S.Lower (A.at (A.Position 3 23) (A.Position 3 27) "text") ])
                                    ]
                                    [ A.at (A.Position 6 1)
                                        (A.Position 7 16)
                                        (S.Value (A.at (A.Position 6 1) (A.Position 6 5) "main")
                                            []
                                            (A.at (A.Position 7 3)
                                                (A.Position 7 16)
                                                (S.Call (A.at (A.Position 7 3) (A.Position 7 7) (S.Var S.LowVar "text"))
                                                    [ A.at (A.Position 7 8) (A.Position 7 16) (S.Str "Hello!")
                                                    ]
                                                )
                                            )
                                            Nothing
                                        )
                                    ]
                                    []
                                    []
                                    []
                                    S.NoEffects
                                )
                            )
            ]
        ]
