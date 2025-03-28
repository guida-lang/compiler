module Browser.Format exposing (run)

import Elm.Syntax.File
import ElmSyntaxParserLenient
import ElmSyntaxPrint



-- RUN


run : String -> Result String String
run inputText =
    case ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ inputText of
        Just modu ->
            Ok (render modu)

        Nothing ->
            -- FIXME missings errs
            Err "Something went wrong..."



-- RENDER


render : Elm.Syntax.File.File -> String
render modul =
    ElmSyntaxPrint.module_ modul
        |> ElmSyntaxPrint.toString
