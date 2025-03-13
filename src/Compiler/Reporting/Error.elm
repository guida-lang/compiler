module Compiler.Reporting.Error exposing
    ( Error(..)
    , Module
    , moduleDecoder
    , moduleEncoder
    , toDoc
    , toJson
    )

import Builder.File as File
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Encode as E
import Compiler.Nitpick.PatternMatches as P
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Canonicalize as Canonicalize
import Compiler.Reporting.Error.Docs as Docs
import Compiler.Reporting.Error.Import as Import
import Compiler.Reporting.Error.Main as Main
import Compiler.Reporting.Error.Pattern as Pattern
import Compiler.Reporting.Error.Syntax as Syntax
import Compiler.Reporting.Error.Type as Type
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report as Report
import Time
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Main as Utils



-- MODULE


type alias Module =
    { name : ModuleName.Raw
    , absolutePath : String
    , modificationTime : File.Time
    , source : String
    , error : Error
    }



-- ERRORS


type Error
    = BadSyntax Syntax.Error
    | BadImports (NE.Nonempty Import.Error)
    | BadNames (OneOrMore Canonicalize.Error)
    | BadTypes L.Localizer (NE.Nonempty Type.Error)
    | BadMains L.Localizer (OneOrMore Main.Error)
    | BadPatterns (NE.Nonempty P.Error)
    | BadDocs Docs.Error



-- TO REPORT


toReports : Code.Source -> Error -> NE.Nonempty Report.Report
toReports source err =
    case err of
        BadSyntax syntaxError ->
            NE.singleton (Syntax.toReport source syntaxError)

        BadImports errs ->
            NE.map (Import.toReport source) errs

        BadNames errs ->
            NE.map (Canonicalize.toReport source) (OneOrMore.destruct NE.Nonempty errs)

        BadTypes localizer errs ->
            NE.map (Type.toReport source localizer) errs

        BadMains localizer errs ->
            NE.map (Main.toReport localizer source) (OneOrMore.destruct NE.Nonempty errs)

        BadPatterns errs ->
            NE.map (Pattern.toReport source) errs

        BadDocs docsErr ->
            Docs.toReports source docsErr



-- TO DOC


toDoc : String -> Module -> List Module -> D.Doc
toDoc root err errs =
    let
        (NE.Nonempty m ms) =
            NE.sortBy
                (\{ modificationTime } ->
                    let
                        (File.Time posix) =
                            modificationTime
                    in
                    Time.posixToMillis posix
                )
                (NE.Nonempty err errs)
    in
    D.vcat (toDocHelp root m ms)


toDocHelp : String -> Module -> List Module -> List D.Doc
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


toSeparator : Module -> Module -> D.Doc
toSeparator beforeModule afterModule =
    let
        before : ModuleName.Raw
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


moduleToDoc : String -> Module -> D.Doc
moduleToDoc root { absolutePath, source, error } =
    let
        reports : NE.Nonempty Report.Report
        reports =
            toReports (Code.toSource source) error

        relativePath : Utils.FilePath
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


toJson : Module -> E.Value
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


encodeRegion : A.Region -> E.Value
encodeRegion (A.Region (A.Position sr sc) (A.Position er ec)) =
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



-- ENCODERS and DECODERS


moduleEncoder : Module -> BE.Encoder
moduleEncoder modul =
    BE.sequence
        [ ModuleName.rawEncoder modul.name
        , BE.string modul.absolutePath
        , File.timeEncoder modul.modificationTime
        , BE.string modul.source
        , errorEncoder modul.error
        ]


moduleDecoder : BD.Decoder Module
moduleDecoder =
    BD.map5 Module
        ModuleName.rawDecoder
        BD.string
        File.timeDecoder
        BD.string
        errorDecoder


errorEncoder : Error -> BE.Encoder
errorEncoder error =
    case error of
        BadSyntax syntaxError ->
            BE.sequence
                [ BE.unsignedInt8 0
                , Syntax.errorEncoder syntaxError
                ]

        BadImports errs ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.nonempty Import.errorEncoder errs
                ]

        BadNames errs ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.oneOrMore Canonicalize.errorEncoder errs
                ]

        BadTypes localizer errs ->
            BE.sequence
                [ BE.unsignedInt8 3
                , L.localizerEncoder localizer
                , BE.nonempty Type.errorEncoder errs
                ]

        BadMains localizer errs ->
            BE.sequence
                [ BE.unsignedInt8 4
                , L.localizerEncoder localizer
                , BE.oneOrMore Main.errorEncoder errs
                ]

        BadPatterns errs ->
            BE.sequence
                [ BE.unsignedInt8 5
                , BE.nonempty P.errorEncoder errs
                ]

        BadDocs docsErr ->
            BE.sequence
                [ BE.unsignedInt8 6
                , Docs.errorEncoder docsErr
                ]


errorDecoder : BD.Decoder Error
errorDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map BadSyntax Syntax.errorDecoder

                    1 ->
                        BD.map BadImports (BD.nonempty Import.errorDecoder)

                    2 ->
                        BD.map BadNames (BD.oneOrMore Canonicalize.errorDecoder)

                    3 ->
                        BD.map2 BadTypes
                            L.localizerDecoder
                            (BD.nonempty Type.errorDecoder)

                    4 ->
                        BD.map2 BadMains
                            L.localizerDecoder
                            (BD.oneOrMore Main.errorDecoder)

                    5 ->
                        BD.map BadPatterns (BD.nonempty P.errorDecoder)

                    6 ->
                        BD.map BadDocs Docs.errorDecoder

                    _ ->
                        BD.fail
            )
