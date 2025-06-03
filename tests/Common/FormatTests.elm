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
