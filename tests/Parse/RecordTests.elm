module Parse.RecordTests exposing (suite)

import Compiler.AST.Source as Src
import Compiler.Parse.Expression as E
import Compiler.Parse.Primitives as P
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parse.Record"
        [ Test.describe "Elm"
            [ Test.test "Empty record" <|
                \_ ->
                    elmRecord "{}"
                        |> Expect.equal (Ok (A.at (A.Position 1 1) (A.Position 1 3) (Src.Record ( [], [] ))))
            , Test.test "Extend record by unqualified name" <|
                \_ ->
                    elmRecord "{ a | x = 2 }"
                        |> Expect.equal
                            (Ok
                                (A.at (A.Position 1 1) (A.Position 1 14) <|
                                    Src.Update ( ( [], [] ), A.at (A.Position 1 3) (A.Position 1 4) (Src.Var Src.LowVar "a") )
                                        ( []
                                        , [ ( ( [], [], Nothing )
                                            , ( ( [], A.at (A.Position 1 7) (A.Position 1 8) "x" )
                                              , ( [], A.at (A.Position 1 11) (A.Position 1 12) (Src.Int 2 "2") )
                                              )
                                            )
                                          ]
                                        )
                                )
                            )
            , Test.test "Extend record by qualified name" <|
                \_ ->
                    elmRecord "{ A.b | x = 2 }"
                        |> Expect.equal (Err (E.Record (E.RecordOpen 1 3) 1 1))
            , Test.test "Extend record by nested qualified name" <|
                \_ ->
                    elmRecord "{ A.B.c | x = 2 }"
                        |> Expect.equal (Err (E.Record (E.RecordOpen 1 3) 1 1))
            , Test.test "Extend record with custom type" <|
                \_ ->
                    elmRecord "{ A | x = 2 }"
                        |> Expect.equal (Err (E.Record (E.RecordOpen 1 3) 1 1))
            , Test.test "Extend record with qualified custom type" <|
                \_ ->
                    elmRecord "{ A.B | x = 2 }"
                        |> Expect.equal (Err (E.Record (E.RecordOpen 1 3) 1 1))
            ]
        , Test.describe "Guida"
            [ Test.test "Empty record" <|
                \_ ->
                    guidaRecord "{}"
                        |> Expect.equal (Ok (A.at (A.Position 1 1) (A.Position 1 3) (Src.Record ( [], [] ))))
            , Test.test "Extend record by unqualified name" <|
                \_ ->
                    guidaRecord "{ a | x = 2 }"
                        |> Expect.equal
                            (Ok
                                (A.at (A.Position 1 1) (A.Position 1 14) <|
                                    Src.Update ( ( [], [] ), A.at (A.Position 1 3) (A.Position 1 4) (Src.Var Src.LowVar "a") )
                                        ( []
                                        , [ ( ( [], [], Nothing )
                                            , ( ( [], A.at (A.Position 1 7) (A.Position 1 8) "x" )
                                              , ( [], A.at (A.Position 1 11) (A.Position 1 12) (Src.Int 2 "2") )
                                              )
                                            )
                                          ]
                                        )
                                )
                            )
            , Test.test "Extend record by qualified name" <|
                \_ ->
                    guidaRecord "{ A.b | x = 2 }"
                        |> Expect.equal
                            (Ok
                                (A.at (A.Position 1 1) (A.Position 1 16) <|
                                    Src.Update ( ( [], [] ), A.at (A.Position 1 3) (A.Position 1 6) (Src.VarQual Src.LowVar "A" "b") )
                                        ( []
                                        , [ ( ( [], [], Nothing )
                                            , ( ( [], A.at (A.Position 1 9) (A.Position 1 10) "x" )
                                              , ( [], A.at (A.Position 1 13) (A.Position 1 14) (Src.Int 2 "2") )
                                              )
                                            )
                                          ]
                                        )
                                )
                            )
            , Test.test "Extend record by nested qualified name" <|
                \_ ->
                    guidaRecord "{ A.B.c | x = 2 }"
                        |> Expect.equal
                            (Ok
                                (A.at (A.Position 1 1) (A.Position 1 18) <|
                                    Src.Update ( ( [], [] ), A.at (A.Position 1 3) (A.Position 1 8) (Src.VarQual Src.LowVar "A.B" "c") )
                                        ( []
                                        , [ ( ( [], [], Nothing )
                                            , ( ( [], A.at (A.Position 1 11) (A.Position 1 12) "x" )
                                              , ( [], A.at (A.Position 1 15) (A.Position 1 16) (Src.Int 2 "2") )
                                              )
                                            )
                                          ]
                                        )
                                )
                            )
            , Test.test "Extend record by another record's field" <|
                \_ ->
                    guidaRecord "{ a.b | x = 2 }"
                        |> Expect.equal
                            (Ok
                                (A.at (A.Position 1 1) (A.Position 1 16) <|
                                    Src.Update
                                        ( ( [], [] )
                                        , A.at (A.Position 1 3) (A.Position 1 6) <|
                                            Src.Access (A.at (A.Position 1 3) (A.Position 1 4) (Src.Var Src.LowVar "a"))
                                                (A.at (A.Position 1 5) (A.Position 1 6) "b")
                                        )
                                        ( []
                                        , [ ( ( [], [], Nothing )
                                            , ( ( [], A.at (A.Position 1 9) (A.Position 1 10) "x" )
                                              , ( [], A.at (A.Position 1 13) (A.Position 1 14) (Src.Int 2 "2") )
                                              )
                                            )
                                          ]
                                        )
                                )
                            )
            , Test.test "Extend record by nested qualified name and another record's field" <|
                \_ ->
                    guidaRecord "{ A.B.c.d | x = 2 }"
                        |> Expect.equal
                            (Ok
                                (A.at (A.Position 1 1) (A.Position 1 20) <|
                                    Src.Update
                                        ( ( [], [] )
                                        , A.at (A.Position 1 3)
                                            (A.Position 1 10)
                                            (Src.Access (A.at (A.Position 1 3) (A.Position 1 8) (Src.VarQual Src.LowVar "A.B" "c"))
                                                (A.at (A.Position 1 9) (A.Position 1 10) "d")
                                            )
                                        )
                                        ( []
                                        , [ ( ( [], [], Nothing )
                                            , ( ( [], A.at (A.Position 1 13) (A.Position 1 14) "x" )
                                              , ( [], A.at (A.Position 1 17) (A.Position 1 18) (Src.Int 2 "2") )
                                              )
                                            )
                                          ]
                                        )
                                )
                            )
            , Test.test "Extend record with custom type" <|
                \_ ->
                    guidaRecord "{ A | x = 2 }"
                        |> Expect.equal (Err (E.Record (E.RecordOpen 1 3) 1 1))
            , Test.test "Extend record with qualified custom type" <|
                \_ ->
                    guidaRecord "{ A.B | x = 2 }"
                        |> Expect.equal (Err (E.Record (E.RecordOpen 1 3) 1 1))
            ]
        ]


elmRecord : String -> Result E.Expr Src.Expr
elmRecord =
    record SV.Elm


guidaRecord : String -> Result E.Expr Src.Expr
guidaRecord =
    record SV.Guida


record : SyntaxVersion -> String -> Result E.Expr Src.Expr
record syntaxVersion =
    P.fromByteString (E.record syntaxVersion (A.Position 1 1)) E.Start
