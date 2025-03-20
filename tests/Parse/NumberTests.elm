module Parse.NumberTests exposing (suite)

import Compiler.Parse.Number as N
import Compiler.Parse.Primitives as P
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Parse.Number"
        [ Test.test "1" <|
            \_ ->
                singleNumber "1"
                    |> Expect.equal (Ok (N.Int 1))
        , Test.test "1000" <|
            \_ ->
                singleNumber "1000"
                    |> Expect.equal (Ok (N.Int 1000))
        , Test.test "1_000" <|
            \_ ->
                singleNumber "1_000"
                    |> Expect.equal (Ok (N.Int 1000))
        ]


singleNumber : String -> Result () N.Number
singleNumber =
    P.fromByteString (N.number (\_ _ -> ()) (\_ _ _ -> ())) (\_ _ -> ())
