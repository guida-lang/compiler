module Parse.NumberTests exposing (suite)

import Compiler.Parse.Number as N
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Error.Syntax as E
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parse.Number"
        [ -- INT
          Test.test "1000" <|
            \_ ->
                singleNumber "1000"
                    |> Expect.equal (Ok (N.Int 1000 "1000"))

        -- INT with underscore
        , Test.test "1_000" <|
            \_ ->
                singleNumber "1_000"
                    |> Expect.equal (Ok (N.Int 1000 "1_000"))

        --
        , Test.test "42_000" <|
            \_ ->
                singleNumber "42_000"
                    |> Expect.equal (Ok (N.Int 42000 "42_000"))

        -- INT with multiple underscores
        , Test.test "2_000_000" <|
            \_ ->
                singleNumber "2_000_000"
                    |> Expect.equal (Ok (N.Int 2000000 "2_000_000"))

        -- ===============
        -- INT Failures
        -- ===============
        --
        -- Int with consecutive underscore should fail
        , Test.test "42__000" <|
            \_ ->
                singleNumber "42__000"
                    |> Expect.equal (Err E.NumberNoConsecutiveUnderscores)

        -- Leading underscore should fail
        , Test.test "_42_000" <|
            \_ ->
                singleNumber "_42_000"
                    |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)

        -- Trailing underscore should fail
        , Test.test "42_000_" <|
            \_ ->
                singleNumber "42_000_"
                    |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)

        -- INT with multiple underscores, one of them immediately before exponent e
        , Test.test "6_001_222_e+36" <|
            \_ ->
                singleNumber "6_001_222_e+36"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)

        -- INT with one underscore immediately before exponent e
        , Test.test "222_e+36" <|
            \_ ->
                singleNumber "222_e+36"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)

        -- FLOAT
        , Test.test "1000.42" <|
            \_ ->
                singleNumber "1000.42"
                    |> Expect.equal (Ok (N.Float 1000.42 "1000.42"))

        -- FLOAT with exponent
        , Test.test "6.022e23" <|
            \_ ->
                singleNumber "6.022e23"
                    |> Expect.equal (Ok (N.Float 6.022e23 "6.022e23"))

        -- FLOAT with exponent and +/-
        , Test.test "6000.022e+36" <|
            \_ ->
                singleNumber "6000.022e+36"
                    |> Expect.equal (Ok (N.Float 6.000022e39 "6000.022e+36"))

        -- FLOAT with underscore before decimal point
        , Test.test "111_000.602" <|
            \_ ->
                singleNumber "111_000.602"
                    |> Expect.equal (Ok (N.Float 111000.602 "111_000.602"))

        -- FLOAT with underscore after decimal point
        , Test.test "1000.4_205" <|
            \_ ->
                singleNumber "1000.4_205"
                    |> Expect.equal (Ok (N.Float 1000.4205 "1000.4_205"))

        -- -- FLOAT with underscore before and after decimal point
        , Test.test "1_000.4_205" <|
            \_ ->
                singleNumber "1_000.4_205"
                    |> Expect.equal (Ok (N.Float 1000.4205 "1_000.4_205"))

        -- FLOAT with underscore before decimal point and exponent
        , Test.test "60_000.022e3" <|
            \_ ->
                singleNumber "60_000.022e3"
                    |> Expect.equal (Ok (N.Float 60000022 "60_000.022e3"))

        -- FLOAT with underscore after decimal point and exponent
        , Test.test "6.022e2_3" <|
            \_ ->
                singleNumber "6.022e2_3"
                    |> Expect.equal (Ok (N.Float 6.022e23 "6.022e2_3"))

        -- FLOAT with underscore before and after decimal point and exponent and +
        , Test.test "6_000.0_22e+3_6" <|
            \_ ->
                singleNumber "6_000.0_22e+3_6"
                    |> Expect.equal (Ok (N.Float 6.000022e39 "6_000.0_22e+3_6"))

        -- ===============
        -- Float Failures
        -- ===============
        --
        -- FLOAT with Leading underscore
        , Test.test "_111000.602" <|
            \_ ->
                singleNumber "_111000.602"
                    |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)

        -- FLOAT with ending underscore
        , Test.test "111_000.602_" <|
            \_ ->
                singleNumber "111_000.602_"
                    |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)

        -- FLOAT with consecutive underscore before decimal point
        , Test.test "111__000.602" <|
            \_ ->
                singleNumber "111__000.602"
                    |> Expect.equal (Err E.NumberNoConsecutiveUnderscores)

        -- FLOAT with consecutive underscore after decimal point
        , Test.test "111_000.6__002" <|
            \_ ->
                singleNumber "111_000.6__002"
                    |> Expect.equal (Err E.NumberNoConsecutiveUnderscores)

        -- FLOAT with underscore immediately after decimal point
        , Test.test "11._602" <|
            \_ ->
                singleNumber "11._602"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)

        -- FLOAT with underscore immediately before decimal point
        , Test.test "11_.602" <|
            \_ ->
                singleNumber "11_.602"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)

        -- FLOAT with underscore adjacent to +/-
        , Test.test "6_000.022e+_36" <|
            \_ ->
                singleNumber "6_000.022e+_36"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)

        -- FLOAT with underscore adjacent to +/- or immediately after exponent e
        , Test.test "6_000.022e_+36" <|
            \_ ->
                singleNumber "6_000.022e_+36"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)

        -- FLOAT with one underscore in fraction part immediately before exponent e
        , Test.test "6_000.022_e+36" <|
            \_ ->
                singleNumber "6_000.022_e+36"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)

        -- FLOAT with multiple underscores in fraction part, one of them immediately before exponent e
        , Test.test "6_000.1_222_e+36" <|
            \_ ->
                singleNumber "6_000.1_222_e+36"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToDecimalOrExponent)

        --  HEXADECIMAL
        , Test.test "0xDEADBEEF" <|
            \_ ->
                singleNumber "0xDEADBEEF"
                    |> Expect.equal (Ok (N.Int 3735928559))
        , Test.test "0xDE_AD_BE_EF" <|
            \_ ->
                singleNumber "0xDE_AD_BE_EF"
                    |> Expect.equal (Ok (N.Int 3735928559))
        , Test.test "_0xDEADBEEF" <|
            \_ ->
                singleNumber "_0xDEADBEEF"
                    |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)
        , Test.test "0xDEADBEEF_" <|
            \_ ->
                singleNumber "0xDEADBEEF_"
                    |> Expect.equal (Err E.NumberNoLeadingOrTrailingUnderscores)
        , Test.test "0xDE__ADBEEF" <|
            \_ ->
                singleNumber "0xDE__ADBEEF"
                    |> Expect.equal (Err E.NumberNoConsecutiveUnderscores)
        , Test.test "0x_DE_ADBEEF" <|
            \_ ->
                singleNumber "0x_DE_ADBEEF"
                    |> Expect.equal (Err E.NumberNoUnderscoresAdjacentToHexadecimalPreFix)
        ]


singleNumber : String -> Result E.Number N.Number
singleNumber =
    P.fromByteString (N.number (\_ _ -> E.NumberEnd) (\x _ _ -> x)) (\_ _ -> E.NumberEnd)
