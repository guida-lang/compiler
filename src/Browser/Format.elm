module Browser.Format exposing (run)

import Common.Format
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Module as M
import Compiler.Parse.SyntaxVersion as SV



-- RUN


run : String -> Result String String
run src =
    Common.Format.format SV.Guida (M.Package Pkg.core) src
        |> Result.mapError
            (\_ ->
                -- FIXME missings errs
                "Something went wrong..."
            )
