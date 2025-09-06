module Parse.NumberTests exposing (suite)

import Compiler.Parse.Number as N
import Compiler.Parse.Primitives as P
import Compiler.Parse.SyntaxVersion as SyntaxVersion exposing (SyntaxVersion)
import Compiler.Reporting.Error.Syntax as E
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parse.Number"
        [ -- Guida SyntaxVersion
          Test.describe "Parse.Number for Guida SyntaxVersion"
            [ Test.describe "Parse Int with underscores"
                [ Test.test "Simple Int with no underscores 1000" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "1000"
                            |> Expect.equal (Ok (N.Int 1000 "1000"))
                , Test.test "One underscore 1_000" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "1_000"
                            |> Expect.equal (Ok (N.Int 1000 "1_000"))
                , Test.test "One underscore 42_000" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "42_000"
                            |> Expect.equal (Ok (N.Int 42000 "42_000"))
                , Test.test "INT with multiple underscores 2_000_000" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "2_000_000"
                            |> Expect.equal (Ok (N.Int 2000000 "2_000_000"))
                , Test.test "Int with consecutive underscore should fail 42__000" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "42__000"
                            |> Expect.equal (Err E.NumberNoConsecutiveUnderscores)
                , Test.test "Leading underscore should fail _42_000" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "_42_000"
                            |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)
                , Test.test "Trailing underscore should fail 42_000_" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "42_000_"
                            |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)
                , Test.test "INT with multiple underscores, one of them immediately before exponent 'e' 6_001_222_e+36" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6_001_222_e+36"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)
                , Test.test "INT with one underscore immediately before exponent 'e' 222_e+36" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "222_e+36"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)
                ]
            , Test.describe "Parse FLOAT underscores"
                [ Test.test "Simple FLOAT with no underscores 1000.42" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "1000.42"
                            |> Expect.equal (Ok (N.Float 1000.42 "1000.42"))
                , Test.test "FLOAT with exponent and no underscores 6.022e23" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6.022e23"
                            |> Expect.equal (Ok (N.Float 6.022e23 "6.022e23"))
                , Test.test "FLOAT with exponent and +/- and no underscores 6000.022e+36" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6000.022e+36"
                            |> Expect.equal (Ok (N.Float 6.000022e39 "6000.022e+36"))
                , Test.test "FLOAT with underscore before decimal point 111_000.602" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "111_000.602"
                            |> Expect.equal (Ok (N.Float 111000.602 "111_000.602"))
                , Test.test "FLOAT with underscore after decimal point 1000.4_205" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "1000.4_205"
                            |> Expect.equal (Ok (N.Float 1000.4205 "1000.4_205"))
                , Test.test "FLOAT with underscore before and after decimal point 1_000.4_205" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "1_000.4_205"
                            |> Expect.equal (Ok (N.Float 1000.4205 "1_000.4_205"))
                , Test.test "FLOAT with underscore before decimal point and exponent 60_000.022e3" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "60_000.022e3"
                            |> Expect.equal (Ok (N.Float 60000022 "60_000.022e3"))
                , Test.test "FLOAT with underscore after decimal point and exponent 6.022e2_3" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6.022e2_3"
                            |> Expect.equal (Ok (N.Float 6.022e23 "6.022e2_3"))
                , Test.test "FLOAT with underscores before and after decimal point and exponent and '+' 6_000.0_22e+3_6" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6_000.0_22e+3_6"
                            |> Expect.equal (Ok (N.Float 6.000022e39 "6_000.0_22e+3_6"))
                , Test.test "FLOAT with Leading underscore _111000.602" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "_111000.602"
                            |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)
                , Test.test "FLOAT with trailing underscore 111_000.602_" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "111_000.602_"
                            |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)
                , Test.test "FLOAT with consecutive underscore before decimal point 111__000.602" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "111__000.602"
                            |> Expect.equal (Err E.NumberNoConsecutiveUnderscores)
                , Test.test "FLOAT with consecutive underscore after decimal point 111_000.6__002" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "111_000.6__002"
                            |> Expect.equal (Err E.NumberNoConsecutiveUnderscores)
                , Test.test "FLOAT with underscore immediately after decimal point 11._602" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "11._602"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)
                , Test.test "FLOAT with underscore immediately before decimal point 11_.602" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "11_.602"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)
                , Test.test "FLOAT with underscore adjacent to +/- 6_000.022e+_36" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6_000.022e+_36"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)
                , Test.test "FLOAT with underscore adjacent to +/- or immediately after exponent 'e' 6_000.022e_+36" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6_000.022e_+36"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)
                , Test.test "FLOAT with one underscore in fraction part immediately before exponent 'e' 6_000.022_e+36" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6_000.022_e+36"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)
                , Test.test "FLOAT with multiple underscores in fraction part, one of them immediately before exponent 'e' 6_000.1_222_e+36" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "6_000.1_222_e+36"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)
                ]
            , Test.describe "Parse HEXADECIMAL underscores"
                [ Test.test "HEXADECIMAL no underscores 0xDEADBEEF" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "0xDEADBEEF"
                            |> Expect.equal (Ok (N.Int 3735928559 "0xDEADBEEF"))
                , Test.test "HEXADECIMAL with underscores 0xDE_AD_BE_EF" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "0xDE_AD_BE_EF"
                            |> Expect.equal (Ok (N.Int 3735928559 "0xDE_AD_BE_EF"))
                , Test.test "HEXADECIMAL leading underscore should fail _0xDEADBEEF" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "_0xDEADBEEF"
                            |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)
                , Test.test "HEXADECIMAL trailing underscore should fail 0xDEADBEEF_" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "0xDEADBEEF_"
                            |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)
                , Test.test "HEXADECIMAL consecutive underscores should fail 0xDE__ADBEEF" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "0xDE__ADBEEF"
                            |> Expect.equal (Err E.NumberNoConsecutiveUnderscores)
                , Test.test "HEXADECIMAL underscores adjacent To Hexadecimal preFix '0x' should fail 0x_DE_ADBEEF" <|
                    \_ ->
                        singleNumber SyntaxVersion.Guida "0x_DE_ADBEEF"
                            |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToHexadecimalPreFix)
                ]
            ]

        -- ELM SyntaxVersion
        , Test.describe "Parse.Number for Elm SyntaxVersion"
            [ Test.test "No underscores 1000" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "1000"
                        |> Expect.equal (Ok (N.Int 1000 "1000"))
            , Test.test "Simple FLOAT with no underscores 1000.42" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "1000.42"
                        |> Expect.equal (Ok (N.Float 1000.42 "1000.42"))
            , Test.test "FLOAT with exponent and no underscores 6.022e23" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "6.022e23"
                        |> Expect.equal (Ok (N.Float 6.022e23 "6.022e23"))
            , Test.test "FLOAT with exponent and +/- and no underscores 6000.022e+36" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "6000.022e+36"
                        |> Expect.equal (Ok (N.Float 6.000022e39 "6000.022e+36"))
            , Test.test "0xDEADBEEF" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "0xDEADBEEF"
                        |> Expect.equal (Ok (N.Int 3735928559 "0xDEADBEEF"))
            , Test.test "One underscore 1_000" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "1_000"
                        |> Expect.equal (Err E.NumberEnd)
            , Test.test "FLOAT with underscore before decimal point 111_000.602" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "111_000.602"
                        |> Expect.equal (Err E.NumberEnd)
            , Test.test "FLOAT with underscore after decimal point 1000.4_205" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "1000.4_205"
                        |> Expect.equal (Err E.NumberEnd)
            , Test.test "FLOAT with underscore before and after decimal point 1_000.4_205" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "1_000.4_205"
                        |> Expect.equal (Err E.NumberEnd)
            , Test.test "FLOAT with underscore before decimal point and exponent 60_000.022e3" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "60_000.022e3"
                        |> Expect.equal (Err E.NumberEnd)
            , Test.test "FLOAT with underscore after decimal point and exponent 6.022e2_3" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "6.022e2_3"
                        |> Expect.equal (Err E.NumberEnd)
            , Test.test "FLOAT with underscores before and after decimal point and exponent and '+' 6_000.0_22e+3_6" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "6_000.0_22e+3_6"
                        |> Expect.equal (Err E.NumberEnd)
            , Test.test "0xDE_AD_BE_EF" <|
                \_ ->
                    singleNumber SyntaxVersion.Elm "0xDE_AD_BE_EF"
                        |> Expect.equal (Err E.NumberHexDigit)
            ]
        ]


singleNumber : SyntaxVersion -> String -> Result E.Number N.Number
singleNumber syntaxVersion =
    P.fromByteString (N.number syntaxVersion (\_ _ -> E.NumberEnd) (\x _ _ -> x)) (\_ _ -> E.NumberEnd)
