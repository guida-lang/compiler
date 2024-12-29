module Compiler.Reporting.Error exposing
    ( toDoc
    , toJson
    )

import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Json.Encode as E
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Canonicalize as Canonicalize
import Compiler.Reporting.Error.Docs as Docs
import Compiler.Reporting.Error.Import as Import
import Compiler.Reporting.Error.Main as Main
import Compiler.Reporting.Error.Pattern as Pattern
import Compiler.Reporting.Error.Syntax as Syntax
import Compiler.Reporting.Error.Type as Type
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Time
import Types as T
import Utils.Main as Utils



-- TO REPORT


toReports : Code.Source -> T.CRE_Error -> NE.Nonempty Report.Report
toReports source err =
    case err of
        T.CRE_BadSyntax syntaxError ->
            NE.singleton (Syntax.toReport source syntaxError)

        T.CRE_BadImports errs ->
            NE.map (Import.toReport source) errs

        T.CRE_BadNames errs ->
            NE.map (Canonicalize.toReport source) (OneOrMore.destruct NE.Nonempty errs)

        T.CRE_BadTypes localizer errs ->
            NE.map (Type.toReport source localizer) errs

        T.CRE_BadMains localizer errs ->
            NE.map (Main.toReport localizer source) (OneOrMore.destruct NE.Nonempty errs)

        T.CRE_BadPatterns errs ->
            NE.map (Pattern.toReport source) errs

        T.CRE_BadDocs docsErr ->
            Docs.toReports source docsErr



-- TO DOC


toDoc : String -> T.CRE_Module -> List T.CRE_Module -> D.Doc
toDoc root err errs =
    let
        (NE.Nonempty m ms) =
            NE.sortBy
                (\{ modificationTime } ->
                    let
                        (T.BF_Time posix) =
                            modificationTime
                    in
                    Time.posixToMillis posix
                )
                (NE.Nonempty err errs)
    in
    D.vcat (toDocHelp root m ms)


toDocHelp : String -> T.CRE_Module -> List T.CRE_Module -> List D.Doc
toDocHelp root module1 modules =
    case modules of
        [] ->
            [ moduleToDoc root module1
            , D.fromChars ""
            ]

        module2 :: otherModules ->
            moduleToDoc root module1
                :: toSeparator module1 module2
                :: toDocHelp root module2 otherModules


toSeparator : T.CRE_Module -> T.CRE_Module -> D.Doc
toSeparator beforeModule afterModule =
    let
        before : T.CEMN_Raw
        before =
            beforeModule.name ++ "  ↑    "

        after : String
        after =
            "    ↓  " ++ afterModule.name
    in
    D.dullred <|
        D.vcat
            [ D.indent (80 - String.length before) (D.fromChars before)
            , D.fromChars "====o======================================================================o===="
            , D.fromChars after
            , D.empty
            , D.empty
            ]



-- MODULE TO DOC


moduleToDoc : String -> T.CRE_Module -> D.Doc
moduleToDoc root { absolutePath, source, error } =
    let
        reports : NE.Nonempty Report.Report
        reports =
            toReports (Code.toSource source) error

        relativePath : T.FilePath
        relativePath =
            Utils.fpMakeRelative root absolutePath
    in
    D.vcat <| List.map (reportToDoc relativePath) (NE.toList reports)


reportToDoc : String -> Report.Report -> D.Doc
reportToDoc relativePath (Report.Report title _ _ message) =
    D.vcat
        [ toMessageBar title relativePath
        , D.fromChars ""
        , message
        , D.fromChars ""
        ]


toMessageBar : String -> String -> D.Doc
toMessageBar title filePath =
    let
        usedSpace : Int
        usedSpace =
            4 + String.length title + 1 + String.length filePath
    in
    D.dullcyan <|
        D.fromChars <|
            "-- "
                ++ title
                ++ " "
                ++ String.repeat (max 1 (80 - usedSpace)) "-"
                ++ " "
                ++ filePath



-- TO JSON


toJson : T.CRE_Module -> E.Value
toJson { name, absolutePath, source, error } =
    let
        reports : NE.Nonempty Report.Report
        reports =
            toReports (Code.toSource source) error
    in
    E.object
        [ ( "path", E.string absolutePath )
        , ( "name", E.string name )
        , ( "problems", E.list reportToJson (NE.toList reports) )
        ]


reportToJson : Report.Report -> E.Value
reportToJson (Report.Report title region _ message) =
    E.object
        [ ( "title", E.string title )
        , ( "region", encodeRegion region )
        , ( "message", D.encode message )
        ]


encodeRegion : T.CRA_Region -> E.Value
encodeRegion (T.CRA_Region (T.CRA_Position sr sc) (T.CRA_Position er ec)) =
    E.object
        [ ( "start"
          , E.object
                [ ( "line", E.int sr )
                , ( "column", E.int sc )
                ]
          )
        , ( "end"
          , E.object
                [ ( "line", E.int er )
                , ( "column", E.int ec )
                ]
          )
        ]
