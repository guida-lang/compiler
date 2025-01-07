module Terminal.Format exposing
    ( Flags(..)
    , run
    )

import Builder.File as File
import Compiler.AST.Source as Src
import Compiler.Parse.Module as Parse
import Compiler.Reporting.Annotation as A
import Elm.Syntax.File
import ElmSyntaxParserLenient
import ElmSyntaxPrint
import List.Extra as List
import System.IO as IO exposing (IO)
import Utils.Main as Utils exposing (FilePath)


type Flags
    = Flags (Maybe FilePath)


run : List String -> Flags -> IO ()
run paths flags =
    runHelp paths flags


runHelp : List String -> Flags -> IO ()
runHelp paths ((Flags maybeOutput) as flags) =
    case paths of
        [] ->
            IO.pure ()

        path :: remainingPaths ->
            File.readUtf8 path
                |> IO.bind
                    (\source ->
                        case ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ source of
                            Just modul ->
                                File.writeUtf8 (Maybe.withDefault path maybeOutput) (formatModule modul)

                            Nothing ->
                                -- FIXME
                                IO.pure ()
                    )
                |> IO.bind (\_ -> runHelp remainingPaths flags)


formatModule : Elm.Syntax.File.File -> String
formatModule modul =
    ElmSyntaxPrint.module_ modul |> ElmSyntaxPrint.toString
