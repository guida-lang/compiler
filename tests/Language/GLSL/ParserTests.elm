module Language.GLSL.ParserTests exposing (suite)

import Combine
import Expect
import Language.GLSL.Parser2 as P
import Language.GLSL.Syntax as P
import Test exposing (Test)


parse : String -> Result (Combine.ParseError ()) P.TranslationUnit
parse =
    P.parse >> Result.map (\( _, _, result ) -> result)


suite : Test
suite =
    Test.describe "Language.GLSL.Parser"
        [ Test.test "Triangle" <|
            \_ ->
                parse """
                    attribute vec3 position;
                    attribute vec3 color;
                    uniform mat4 perspective;
                    varying vec3 vcolor;

                    void main () {
                        gl_Position = perspective * vec4(position, 1.0);
                        vcolor = color;
                """
                    |> Expect.equal (Ok (P.TranslationUnit []))
        ]
