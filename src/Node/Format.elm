module Node.Format exposing (run)

import Common.Format
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Module as M
import Compiler.Parse.SyntaxVersion as SV



-- RUN


run : String -> Result String String
run inputText =
    Common.Format.format SV.Guida (M.Package Pkg.core) inputText
        |> Result.mapError
            (\_ ->
                -- FIXME missings errs
                "Something went wrong..."
            )
