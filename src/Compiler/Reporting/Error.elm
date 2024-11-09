module Compiler.Reporting.Error exposing
    ( Error(..)
    , Module
    , jsonToJson
    , moduleCodec
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
import Json.Decode as Decode
import Json.Encode as Encode
import Serialize exposing (Codec)
import Time
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


jsonToJson : Module -> Encode.Value
jsonToJson =
    E.toJsonValue << toJson


moduleEncoder : Module -> Encode.Value
moduleEncoder modul =
    Encode.object
        [ ( "name", ModuleName.rawEncoder modul.name )
        , ( "absolutePath", Encode.string modul.absolutePath )
        , ( "modificationTime", File.timeEncoder modul.modificationTime )
        , ( "source", Encode.string modul.source )
        , ( "error", errorEncoder modul.error )
        ]


moduleDecoder : Decode.Decoder Module
moduleDecoder =
    Decode.map5 Module
        (Decode.field "name" ModuleName.rawDecoder)
        (Decode.field "absolutePath" Decode.string)
        (Decode.field "modificationTime" File.timeDecoder)
        (Decode.field "source" Decode.string)
        (Decode.field "error" errorDecoder)


moduleCodec : Codec e Module
moduleCodec =
    Serialize.record Module
        |> Serialize.field .name ModuleName.rawCodec
        |> Serialize.field .absolutePath Serialize.string
        |> Serialize.field .modificationTime File.timeCodec
        |> Serialize.field .source Serialize.string
        |> Serialize.field .error errorCodec
        |> Serialize.finishRecord


errorEncoder : Error -> Encode.Value
errorEncoder error =
    case error of
        BadSyntax syntaxError ->
            Encode.object
                [ ( "type", Encode.string "BadSyntax" )
                , ( "syntaxError", Syntax.errorEncoder syntaxError )
                ]

        BadImports errs ->
            Encode.object
                [ ( "type", Encode.string "BadImports" )
                , ( "errs", E.nonempty Import.errorEncoder errs )
                ]

        BadNames errs ->
            Encode.object
                [ ( "type", Encode.string "BadNames" )
                , ( "errs", E.oneOrMore Canonicalize.errorEncoder errs )
                ]

        BadTypes localizer errs ->
            Encode.object
                [ ( "type", Encode.string "BadTypes" )
                , ( "localizer", L.localizerEncoder localizer )
                , ( "errs", E.nonempty Type.errorEncoder errs )
                ]

        BadMains localizer errs ->
            Encode.object
                [ ( "type", Encode.string "BadMains" )
                , ( "localizer", L.localizerEncoder localizer )
                , ( "errs", E.oneOrMore Main.errorEncoder errs )
                ]

        BadPatterns errs ->
            Encode.object
                [ ( "type", Encode.string "BadPatterns" )
                , ( "errs", E.nonempty P.errorEncoder errs )
                ]

        BadDocs docsErr ->
            Encode.object
                [ ( "type", Encode.string "BadDocs" )
                , ( "docsErr", Docs.errorEncoder docsErr )
                ]


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "BadSyntax" ->
                        Decode.map BadSyntax (Decode.field "syntaxError" Syntax.errorDecoder)

                    "BadImports" ->
                        Decode.map BadImports (Decode.field "errs" (DecodeX.nonempty Import.errorDecoder))

                    "BadNames" ->
                        Decode.map BadNames (Decode.field "errs" (DecodeX.oneOrMore Canonicalize.errorDecoder))

                    "BadTypes" ->
                        Decode.map2 BadTypes
                            (Decode.field "localizer" L.localizerDecoder)
                            (Decode.field "errs" (DecodeX.nonempty Type.errorDecoder))

                    "BadMains" ->
                        Decode.map2 BadMains
                            (Decode.field "localizer" L.localizerDecoder)
                            (Decode.field "errs" (DecodeX.oneOrMore Main.errorDecoder))

                    "BadPatterns" ->
                        Decode.map BadPatterns (Decode.field "errs" (DecodeX.nonempty P.errorDecoder))

                    "BadDocs" ->
                        Decode.map BadDocs (Decode.field "docsErr" Docs.errorDecoder)

                    _ ->
                        Decode.fail ("Unknown Path's type: " ++ type_)
            )


errorCodec : Codec e Error
errorCodec =
    Debug.todo "errorCodec"
