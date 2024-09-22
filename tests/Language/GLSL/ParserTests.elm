module Language.GLSL.ParserTests exposing (suite)

import Combine
import Expect
import Language.GLSL.Parser2 as P
import Language.GLSL.Syntax as P exposing (..)
import Test exposing (Test)


parse : String -> Result (Combine.ParseError ()) P.TranslationUnit
parse =
    P.parse >> Result.map (\( _, _, result ) -> result)


suite : Test
suite =
    Test.describe "Language.GLSL.Parser"
        [ Test.describe "Triangle"
            [ Test.test "vertexShader" <|
                \_ ->
                    parse """
                        attribute vec3 position;
                        attribute vec3 color;
                        uniform mat4 perspective;
                        varying vec3 vcolor;

                        void main () {
                            gl_Position = perspective * vec4(position, 1.0);
                            vcolor = color;
                        }
                        """
                        |> Expect.equal
                            (Ok
                                (P.TranslationUnit
                                    [ Declaration (InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Attribute)) (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)))) [ InitDecl "position" Nothing Nothing ])
                                    , Declaration (InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Attribute)) (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)))) [ InitDecl "color" Nothing Nothing ])
                                    , Declaration (InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Uniform)) (TypeSpec Nothing (TypeSpecNoPrecision Mat4 Nothing)))) [ InitDecl "perspective" Nothing Nothing ])
                                    , Declaration (InitDeclaration (TypeDeclarator (FullType (Just (TypeQualSto Varying)) (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing)))) [ InitDecl "vcolor" Nothing Nothing ])
                                    , FunctionDefinition
                                        (FuncProt
                                            (FullType Nothing
                                                (TypeSpec Nothing (TypeSpecNoPrecision Void Nothing))
                                            )
                                            "main"
                                            []
                                        )
                                        (Compound
                                            [ ExpressionStatement
                                                (Just
                                                    (Equal (Variable "gl_Position")
                                                        (Mul (Variable "perspective")
                                                            (FunctionCall (FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Vec4 Nothing)))
                                                                (Params [ Variable "position", FloatConstant 1.0 ])
                                                            )
                                                        )
                                                    )
                                                )
                                            , ExpressionStatement (Just (Equal (Variable "vcolor") (Variable "color")))
                                            ]
                                        )
                                    ]
                                )
                            )
            , Test.test "fragmentShader" <|
                \_ ->
                    parse """
                        precision mediump float;
                        varying vec3 vcolor;

                        void main () {
                            gl_FragColor = vec4(vcolor, 1.0);
                        }
                        """
                        |> Expect.equal
                            (Ok
                                (P.TranslationUnit
                                    [ Declaration (Precision MediumP (TypeSpecNoPrecision Float Nothing))
                                    , Declaration
                                        (InitDeclaration
                                            (TypeDeclarator
                                                (FullType (Just (TypeQualSto Varying))
                                                    (TypeSpec Nothing (TypeSpecNoPrecision Vec3 Nothing))
                                                )
                                            )
                                            [ InitDecl "vcolor" Nothing Nothing ]
                                        )
                                    , FunctionDefinition
                                        (FuncProt (FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision Void Nothing))) "main" [])
                                        (Compound
                                            [ ExpressionStatement
                                                (Just
                                                    (Equal (Variable "gl_FragColor")
                                                        (FunctionCall (FuncIdTypeSpec (TypeSpec Nothing (TypeSpecNoPrecision Vec4 Nothing)))
                                                            (Params [ Variable "vcolor", FloatConstant 1.0 ])
                                                        )
                                                    )
                                                )
                                            ]
                                        )
                                    ]
                                )
                            )
            ]
        ]
