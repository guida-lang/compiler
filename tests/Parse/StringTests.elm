module Parse.StringTests exposing (suite)

import Compiler.Parse.Primitives as P
import Compiler.Parse.String as S
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parse.String"
        [ Test.describe "singleString"
            [ Test.test "🙈" <|
                \_ ->
                    singleString "\"\\u{1F648}\""
                        |> Expect.equal (Ok "\\uD83D\\uDE48")
            , Test.test "\\u{0001}" <|
                \_ ->
                    singleString "\"\\u{0001}\""
                        |> Expect.equal (Ok "\\u0001")
            , Test.test "\\u{FFFF}" <|
                \_ ->
                    singleString "\"\\u{FFFF}\""
                        |> Expect.equal (Ok "\\uD7FF\\uDFFF")
            , Test.test "\\u{10000}" <|
                \_ ->
                    singleString "\"\\u{10000}\""
                        |> Expect.equal (Ok "\\uD800\\uDC00")
            ]
        ]


singleString : String -> Result () String
singleString =
    P.fromByteString (S.string (\_ _ -> ()) (\_ _ _ -> ())) (\_ _ -> ())
