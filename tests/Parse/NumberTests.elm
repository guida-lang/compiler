module Parse.NumberTests exposing (suite)

import Compiler.Parse.Number as N
import Compiler.Parse.Primitives as P
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Error.Syntax as E
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parse.Number"
        [ Test.describe "Guida"
            [ Test.describe "Int"
                [ Test.test "No underscores 1000" <|
                    \_ ->
                        singleNumber SV.Guida "1000"
                            |> Expect.equal (Ok (N.Int 1000 "1000"))
                , Test.test "One underscore 1_000" <|
                    \_ ->
                        singleNumber SV.Guida "1_000"
                            |> Expect.equal (Ok (N.Int 1000 "1_000"))
                , Test.test "One underscore 42_000" <|
                    \_ ->
                        singleNumber SV.Guida "42_000"
                            |> Expect.equal (Ok (N.Int 42000 "42_000"))
                , Test.test "Multiple underscores 2_000_000" <|
                    \_ ->
                        singleNumber SV.Guida "2_000_000"
                            |> Expect.equal (Ok (N.Int 2000000 "2_000_000"))
                , Test.test "Consecutive underscores: '42__000' should fail at position 4" <|
                    \_ ->
                        expectErrAtPosition 4 SV.Guida "42__000" E.NumberNoConsecutiveUnderscores
                , Test.test "Leading underscore: '_42_000' should fail at position 1" <|
                    \_ ->
                        expectErrAtPosition 1 SV.Guida "_42_000" E.NumberNoLeadingOrTrailingUnderscores
                , Test.test "Trailing underscore: '42_000_' should fail at position 7" <|
                    \_ ->
                        expectErrAtPosition 7 SV.Guida "42_000_" E.NumberNoLeadingOrTrailingUnderscores
                , Test.test "Multiple underscores, one of them immediately before exponent 'e': '6_001_222_e+36' should fail at position 10" <|
                    \_ ->
                        expectErrAtPosition 10 SV.Guida "6_001_222_e+36" E.NumberNoUnderscoresAdjacentToDecimalOrExponent
                , Test.test "One underscore immediately before exponent 'e': '222_e+36' should failt at position 4" <|
                    \_ ->
                        expectErrAtPosition 4 SV.Guida "222_e+36" E.NumberNoUnderscoresAdjacentToDecimalOrExponent
                ]
            , Test.describe "Float"
                [ Test.test "No underscores 1000.42" <|
                    \_ ->
                        singleNumber SV.Guida "1000.42"
                            |> Expect.equal (Ok (N.Float 1000.42 "1000.42"))
                , Test.test "Exponent and no underscores 6.022e23" <|
                    \_ ->
                        singleNumber SV.Guida "6.022e23"
                            |> Expect.equal (Ok (N.Float 6.022e23 "6.022e23"))
                , Test.test "Exponent and +/- and no underscores 6000.022e+36" <|
                    \_ ->
                        singleNumber SV.Guida "6000.022e+36"
                            |> Expect.equal (Ok (N.Float 6.000022e39 "6000.022e+36"))
                , Test.test "Underscore before decimal point 111_000.602" <|
                    \_ ->
                        singleNumber SV.Guida "111_000.602"
                            |> Expect.equal (Ok (N.Float 111000.602 "111_000.602"))
                , Test.test "Underscore after decimal point 1000.4_205" <|
                    \_ ->
                        singleNumber SV.Guida "1000.4_205"
                            |> Expect.equal (Ok (N.Float 1000.4205 "1000.4_205"))
                , Test.test "Underscore before and after decimal point 1_000.4_205" <|
                    \_ ->
                        singleNumber SV.Guida "1_000.4_205"
                            |> Expect.equal (Ok (N.Float 1000.4205 "1_000.4_205"))
                , Test.test "Underscore before decimal point and exponent 60_000.022e3" <|
                    \_ ->
                        singleNumber SV.Guida "60_000.022e3"
                            |> Expect.equal (Ok (N.Float 60000022 "60_000.022e3"))
                , Test.test "Underscore after exponent 6.022e2_3" <|
                    \_ ->
                        singleNumber SV.Guida "6.022e2_3"
                            |> Expect.equal (Ok (N.Float 6.022e23 "6.022e2_3"))
                , Test.test "Underscores before and after decimal point and exponent and '+' 6_000.0_22e+3_6" <|
                    \_ ->
                        singleNumber SV.Guida "6_000.0_22e+3_6"
                            |> Expect.equal (Ok (N.Float 6.000022e39 "6_000.0_22e+3_6"))
                , Test.test "Leading underscore: '_111000.602' should fail at position 1" <|
                    \_ ->
                        expectErrAtPosition 1 SV.Guida "_111000.602" E.NumberNoLeadingOrTrailingUnderscores
                , Test.test "Trailing underscore: '111_000.602_' should fail at position 12" <|
                    \_ ->
                        expectErrAtPosition 12 SV.Guida "111_000.602_" E.NumberNoLeadingOrTrailingUnderscores
                , Test.test "Consecutive underscore before decimal point: '111__000.602' should fail at position 5" <|
                    \_ ->
                        expectErrAtPosition 5 SV.Guida "111__000.602" E.NumberNoConsecutiveUnderscores
                , Test.test "Consecutive underscore after decimal point: '111_000.6__002' should fail at position 11" <|
                    \_ ->
                        expectErrAtPosition 11 SV.Guida "111_000.6__002" E.NumberNoConsecutiveUnderscores
                , Test.test "Underscore immediately after decimal point: '11._602' should fail at position 4" <|
                    \_ ->
                        expectErrAtPosition 4 SV.Guida "11._602" E.NumberNoUnderscoresAdjacentToDecimalOrExponent
                , Test.test "Underscore immediately before decimal point: '11_.602' should fail at position 3" <|
                    \_ ->
                        expectErrAtPosition 3 SV.Guida "11_.602" E.NumberNoUnderscoresAdjacentToDecimalOrExponent
                , Test.test "Underscore adjacent to +/- '6_000.022e+_36' should fail at position 12" <|
                    \_ ->
                        expectErrAtPosition 12 SV.Guida "6_000.022e+_36" E.NumberNoUnderscoresAdjacentToDecimalOrExponent
                , Test.test "Underscore adjacent to +/- or immediately after exponent 'e': '6_000.022e_+36' should fail at position 11" <|
                    \_ ->
                        expectErrAtPosition 11 SV.Guida "6_000.022e_+36" E.NumberNoUnderscoresAdjacentToDecimalOrExponent
                , Test.test "One underscore in fraction part immediately before exponent 'e': '6_000.022_e+36' should fail at position 10" <|
                    \_ ->
                        expectErrAtPosition 10 SV.Guida "6_000.022_e+36" E.NumberNoUnderscoresAdjacentToDecimalOrExponent
                , Test.test "Multiple underscores in fraction part, one of them immediately before exponent 'e': '6_000.1_222_e+36' should fail at position 12" <|
                    \_ ->
                        expectErrAtPosition 12 SV.Guida "6_000.1_222_e+36" E.NumberNoUnderscoresAdjacentToDecimalOrExponent
                ]
            , Test.describe "Hexadecimal"
                [ Test.test "No underscores 0xDEADBEEF" <|
                    \_ ->
                        singleNumber SV.Guida "0xDEADBEEF"
                            |> Expect.equal (Ok (N.Int 3735928559 "0xDEADBEEF"))
                , Test.test "Underscores 0xDE_AD_BE_EF" <|
                    \_ ->
                        singleNumber SV.Guida "0xDE_AD_BE_EF"
                            |> Expect.equal (Ok (N.Int 3735928559 "0xDE_AD_BE_EF"))
                , Test.test "Leading underscore: '_0xDEADBEEF' should fail at position 1" <|
                    \_ ->
                        expectErrAtPosition 1 SV.Guida "_0xDEADBEEF" E.NumberNoLeadingOrTrailingUnderscores
                , Test.test "Trailing underscore: '0xDEADBEEF_' should fail at position 11" <|
                    \_ ->
                        expectErrAtPosition 11 SV.Guida "0xDEADBEEF_" E.NumberNoLeadingOrTrailingUnderscores
                , Test.test "Consecutive underscores: '0xDE__ADBEEF' should fail at position 6" <|
                    \_ ->
                        expectErrAtPosition 6 SV.Guida "0xDE__ADBEEF" E.NumberNoConsecutiveUnderscores
                , Test.test "Underscores adjacent to hexadecimal preFix '0x': '0x_DE_ADBEEF' should fail at position 3" <|
                    \_ ->
                        expectErrAtPosition 3 SV.Guida "0x_DE_ADBEEF" E.NumberNoUnderscoresAdjacentToHexadecimalPreFix
                ]
            , Test.describe "Binary"
                [ Test.test "No underscores 0b1010" <|
                    \_ ->
                        singleNumber SV.Guida "0b1010"
                            |> Expect.equal (Ok (N.Int 10 "0b1010"))
                , Test.test "Underscores 0xDE_AD_BE_EF" <|
                    \_ ->
                        singleNumber SV.Guida "0xDE_AD_BE_EF"
                            |> Expect.equal (Ok (N.Int 3735928559 "0xDE_AD_BE_EF"))
                , Test.test "Leading underscore: '_0b1010' should fail at position 1" <|
                    \_ ->
                        expectErrAtPosition 1 SV.Guida "_0b1010" E.NumberNoLeadingOrTrailingUnderscores
                , Test.test "Trailing underscore: '0b1010_' should fail at position 7" <|
                    \_ ->
                        expectErrAtPosition 7 SV.Guida "0b1010_" E.NumberNoLeadingOrTrailingUnderscores
                , Test.test "Consecutive underscores: '0b10__10' should fail at position 6" <|
                    \_ ->
                        expectErrAtPosition 6 SV.Guida "0b10__10" E.NumberNoConsecutiveUnderscores
                , Test.test "Underscores adjacent to binary preFix '0b': '0b_10_10' should fail at position 3" <|
                    \_ ->
                        expectErrAtPosition 3 SV.Guida "0b_10_10" E.NumberNoUnderscoresAdjacentToBinaryPreFix
                ]
            ]
        , Test.describe "Elm"
            [ Test.test "Int with no underscores 1000" <|
                \_ ->
                    singleNumber SV.Elm "1000"
                        |> Expect.equal (Ok (N.Int 1000 "1000"))
            , Test.test "Simple Float with no underscores 1000.42" <|
                \_ ->
                    singleNumber SV.Elm "1000.42"
                        |> Expect.equal (Ok (N.Float 1000.42 "1000.42"))
            , Test.test "Float with exponent and no underscores 6.022e23" <|
                \_ ->
                    singleNumber SV.Elm "6.022e23"
                        |> Expect.equal (Ok (N.Float 6.022e23 "6.022e23"))
            , Test.test "Float with exponent and +/- and no underscores 6000.022e+36" <|
                \_ ->
                    singleNumber SV.Elm "6000.022e+36"
                        |> Expect.equal (Ok (N.Float 6.000022e39 "6000.022e+36"))
            , Test.test "0xDEADBEEF" <|
                \_ ->
                    singleNumber SV.Elm "0xDEADBEEF"
                        |> Expect.equal (Ok (N.Int 3735928559 "0xDEADBEEF"))
            , Test.test "Int with one underscore 1_000" <|
                \_ ->
                    singleNumber SV.Elm "1_000"
                        |> Expect.equal (Err (E.NumberEnd SV.Elm))
            , Test.test "Float with underscore before decimal point 111_000.602" <|
                \_ ->
                    singleNumber SV.Elm "111_000.602"
                        |> Expect.equal (Err (E.NumberEnd SV.Elm))
            , Test.test "Float with underscore after decimal point 1000.4_205" <|
                \_ ->
                    singleNumber SV.Elm "1000.4_205"
                        |> Expect.equal (Err (E.NumberEnd SV.Elm))
            , Test.test "Float with underscore before and after decimal point 1_000.4_205" <|
                \_ ->
                    singleNumber SV.Elm "1_000.4_205"
                        |> Expect.equal (Err (E.NumberEnd SV.Elm))
            , Test.test "Float with underscore before decimal point and exponent 60_000.022e3" <|
                \_ ->
                    singleNumber SV.Elm "60_000.022e3"
                        |> Expect.equal (Err (E.NumberEnd SV.Elm))
            , Test.test "Float with underscore after decimal point and exponent 6.022e2_3" <|
                \_ ->
                    singleNumber SV.Elm "6.022e2_3"
                        |> Expect.equal (Err (E.NumberEnd SV.Elm))
            , Test.test "Float with underscores before and after decimal point and exponent and '+' 6_000.0_22e+3_6" <|
                \_ ->
                    singleNumber SV.Elm "6_000.0_22e+3_6"
                        |> Expect.equal (Err (E.NumberEnd SV.Elm))
            , Test.test "0xDE_AD_BE_EF" <|
                \_ ->
                    singleNumber SV.Elm "0xDE_AD_BE_EF"
                        |> Expect.equal (Err E.NumberHexDigit)
            , Test.test "0b1010" <|
                \_ ->
                    singleNumber SV.Elm "0b1010"
                        |> Expect.equal (Err (E.NumberEnd SV.Elm))
            ]
        ]


singleNumber : SyntaxVersion -> String -> Result E.Number N.Number
singleNumber syntaxVersion =
    P.fromByteString (N.number syntaxVersion (\_ _ -> E.NumberEnd syntaxVersion) (\x _ _ -> x)) (\_ _ -> E.NumberEnd syntaxVersion)


singleNumberAt : SyntaxVersion -> String -> Result ( E.Number, Int, Int ) N.Number
singleNumberAt syntaxVersion =
    P.fromByteString
        (N.number
            syntaxVersion
            (\row col -> ( E.NumberEnd syntaxVersion, row, col ))
            (\problem row col -> ( problem, row, col ))
        )
        (\row col -> ( E.NumberEnd syntaxVersion, row, col ))


expectErrAtPosition :
    Int
    -> SyntaxVersion
    -> String
    -> E.Number
    -> Expect.Expectation
expectErrAtPosition col v src expected =
    case singleNumberAt v src of
        Err ( problem, _, c ) ->
            Expect.all
                [ \_ -> Expect.equal expected problem
                , \_ -> Expect.equal col c
                ]
                ()

        Ok value ->
            Expect.fail ("Expected error " ++ Debug.toString expected ++ " but parsed " ++ Debug.toString value)
