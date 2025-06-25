module Common.Format exposing (format)

import Common.Format.Box as Box
import Common.Format.Render.Box as Render
import Compiler.Parse.Module as M
import Compiler.Parse.Primitives as P
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
import Compiler.Reporting.Error.Syntax as E


format : SyntaxVersion -> M.ProjectType -> String -> Result E.Module String
format syntaxVersion projectType src =
    P.fromByteString (M.chompModule syntaxVersion projectType) E.ModuleBadEnd src
        |> Result.map render


render : M.Module -> String
render modu =
    Box.render (Render.formatModule True 2 modu) ++ "\n"
