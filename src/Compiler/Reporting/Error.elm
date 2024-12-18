module Compiler.Reporting.Error exposing
    ( CRE_Error(..)
    , CRE_Module
    , jsonToJson
    , moduleDecoder
    , moduleEncoder
    , toDoc
    , toJson
    )

import Builder.File as File
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as DecodeX
import Compiler.Json.Encode as E
import Compiler.Nitpick.PatternMatches as P
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
import Json.Decode as Decode
import Json.Encode as Encode
import Time
import Types as T
import Utils.Main as Utils



-- MODULE


type alias CRE_Module =
    { name : T.CEMN_Raw
    , absolutePath : String
    , modificationTime : File.BF_Time
    , source : String
    , error : CRE_Error
    }



-- ERRORS


type CRE_Error
    = CRE_BadSyntax Syntax.CRES_Error
    | CRE_BadImports (NE.Nonempty Import.CREI_Error)
    | CRE_BadNames (OneOrMore Canonicalize.CREC_Error)
    | CRE_BadTypes L.CRRTL_Localizer (NE.Nonempty Type.CRET_Error)
    | CRE_BadMains L.CRRTL_Localizer (OneOrMore Main.CREM_Error)
    | CRE_BadPatterns (NE.Nonempty P.CNPM_Error)
    | CRE_BadDocs Docs.CRED_Error



-- TO REPORT


toReports : Code.Source -> CRE_Error -> NE.Nonempty Report.Report
toReports source err =
    case err of
        CRE_BadSyntax syntaxError ->
            NE.singleton (Syntax.toReport source syntaxError)

        CRE_BadImports errs ->
            NE.map (Import.toReport source) errs

        CRE_BadNames errs ->
            NE.map (Canonicalize.toReport source) (OneOrMore.destruct NE.Nonempty errs)

        CRE_BadTypes localizer errs ->
            NE.map (Type.toReport source localizer) errs

        CRE_BadMains localizer errs ->
            NE.map (Main.toReport localizer source) (OneOrMore.destruct NE.Nonempty errs)

        CRE_BadPatterns errs ->
            NE.map (Pattern.toReport source) errs

        CRE_BadDocs docsErr ->
            Docs.toReports source docsErr



-- TO DOC


toDoc : String -> CRE_Module -> List CRE_Module -> D.Doc
toDoc root err errs =
    let
        (NE.Nonempty m ms) =
            NE.sortBy
                (\{ modificationTime } ->
                    let
                        (File.BF_Time posix) =
                            modificationTime
                    in
                    Time.posixToMillis posix
                )
                (NE.Nonempty err errs)
    in
    D.vcat (toDocHelp root m ms)


toDocHelp : String -> CRE_Module -> List CRE_Module -> List D.Doc
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


toSeparator : CRE_Module -> CRE_Module -> D.Doc
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


moduleToDoc : String -> CRE_Module -> D.Doc
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


toJson : CRE_Module -> E.Value
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



-- ENCODERS and DECODERS


jsonToJson : CRE_Module -> Encode.Value
jsonToJson =
    E.toJsonValue << toJson


moduleEncoder : CRE_Module -> Encode.Value
moduleEncoder modul =
    Encode.object
        [ ( "name", ModuleName.rawEncoder modul.name )
        , ( "absolutePath", Encode.string modul.absolutePath )
        , ( "modificationTime", File.timeEncoder modul.modificationTime )
        , ( "source", Encode.string modul.source )
        , ( "error", errorEncoder modul.error )
        ]


moduleDecoder : Decode.Decoder CRE_Module
moduleDecoder =
    Decode.map5 CRE_Module
        (Decode.field "name" ModuleName.rawDecoder)
        (Decode.field "absolutePath" Decode.string)
        (Decode.field "modificationTime" File.timeDecoder)
        (Decode.field "source" Decode.string)
        (Decode.field "error" errorDecoder)


errorEncoder : CRE_Error -> Encode.Value
errorEncoder error =
    case error of
        CRE_BadSyntax syntaxError ->
            Encode.object
                [ ( "type", Encode.string "BadSyntax" )
                , ( "syntaxError", Syntax.errorEncoder syntaxError )
                ]

        CRE_BadImports errs ->
            Encode.object
                [ ( "type", Encode.string "BadImports" )
                , ( "errs", E.nonempty Import.errorEncoder errs )
                ]

        CRE_BadNames errs ->
            Encode.object
                [ ( "type", Encode.string "BadNames" )
                , ( "errs", E.oneOrMore Canonicalize.errorEncoder errs )
                ]

        CRE_BadTypes localizer errs ->
            Encode.object
                [ ( "type", Encode.string "BadTypes" )
                , ( "localizer", L.localizerEncoder localizer )
                , ( "errs", E.nonempty Type.errorEncoder errs )
                ]

        CRE_BadMains localizer errs ->
            Encode.object
                [ ( "type", Encode.string "BadMains" )
                , ( "localizer", L.localizerEncoder localizer )
                , ( "errs", E.oneOrMore Main.errorEncoder errs )
                ]

        CRE_BadPatterns errs ->
            Encode.object
                [ ( "type", Encode.string "BadPatterns" )
                , ( "errs", E.nonempty P.errorEncoder errs )
                ]

        CRE_BadDocs docsErr ->
            Encode.object
                [ ( "type", Encode.string "BadDocs" )
                , ( "docsErr", Docs.errorEncoder docsErr )
                ]


errorDecoder : Decode.Decoder CRE_Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "BadSyntax" ->
                        Decode.map CRE_BadSyntax (Decode.field "syntaxError" Syntax.errorDecoder)

                    "BadImports" ->
                        Decode.map CRE_BadImports (Decode.field "errs" (DecodeX.nonempty Import.errorDecoder))

                    "BadNames" ->
                        Decode.map CRE_BadNames (Decode.field "errs" (DecodeX.oneOrMore Canonicalize.errorDecoder))

                    "BadTypes" ->
                        Decode.map2 CRE_BadTypes
                            (Decode.field "localizer" L.localizerDecoder)
                            (Decode.field "errs" (DecodeX.nonempty Type.errorDecoder))

                    "BadMains" ->
                        Decode.map2 CRE_BadMains
                            (Decode.field "localizer" L.localizerDecoder)
                            (Decode.field "errs" (DecodeX.oneOrMore Main.errorDecoder))

                    "BadPatterns" ->
                        Decode.map CRE_BadPatterns (Decode.field "errs" (DecodeX.nonempty P.errorDecoder))

                    "BadDocs" ->
                        Decode.map CRE_BadDocs (Decode.field "docsErr" Docs.errorDecoder)

                    _ ->
                        Decode.fail ("Unknown Path's type: " ++ type_)
            )
