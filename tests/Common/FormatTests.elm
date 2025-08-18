module Common.FormatTests exposing (suite)

import Common.Format
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Module as M
import Compiler.Parse.SyntaxVersion as SV
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Common.Format.format"
        [ Test.describe "fromByteString"
            [ Test.test "Header" <|
                \_ ->
                    Common.Format.format SV.Guida (M.Package Pkg.core) (generateModule defaultModule)
                        |> Expect.equal (Ok "module Main exposing (..)\n\n\nfn =\n    ()\n")
            , Test.test "Records" <|
                \_ ->
                    Common.Format.format SV.Guida
                        (M.Package Pkg.core)
                        (generateModule
                            { defaultModule
                                | declarations =
                                    [ "fn = { {- C1 -} a {- C2 -} = {- C3 -} 1 {- C4 -}, {- C5 -} b {- C6 -} = {- C7 -} { {- C8 -} M.b {- C9 -} | {- C10 -} x {- C11 -} = {- C12 -} 2 {- C13 -} }, {- C14 -} c {- C15 -} = {- C16 -} { {- C17 -} defaultC {- C18 -} | {- C19 -} y {- C20 -} = {- C21 -} 3 {- C22 -} } {- C23 -} }"
                                    ]
                            }
                        )
                        |> Expect.equal (Ok "module Main exposing (..)\n\n\nfn =\n    { {- C1 -} a {- C2 -} = {- C3 -} 1\n\n    {- C4 -}\n    , {- C5 -}\n      b {- C6 -} =\n        {- C7 -}\n        { {- C8 -} M.b {- C9 -}\n            | {- C10 -} x {- C11 -} = {- C12 -} 2\n\n            {- C13 -}\n        }\n    , {- C14 -}\n      c {- C15 -} =\n        {- C16 -}\n        { {- C17 -} defaultC {- C18 -}\n            | {- C19 -} y {- C20 -} = {- C21 -} 3\n\n            {- C22 -}\n        }\n\n    {- C23 -}\n    }\n")
            ]
        ]


type alias GenerateModuleConfig =
    { header : String
    , docs : String
    , imports : List String
    , infixes : List String
    , declarations : List String
    }


defaultModule : GenerateModuleConfig
defaultModule =
    { header = "module Main exposing (..)"
    , docs = ""
    , imports = []
    , infixes = []
    , declarations = [ "fn = ()" ]
    }


generateModule : GenerateModuleConfig -> String
generateModule { header, docs, imports, infixes, declarations } =
    String.join "\n"
        [ header
        , docs
        , String.join "\n" imports
        , String.join "\n" infixes
        , String.join "\n" declarations
        ]
