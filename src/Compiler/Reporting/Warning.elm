module Compiler.Reporting.Warning exposing
    ( Context(..)
    , Module
    , Warning(..)
    , contextDecoder
    , contextEncoder
    , moduleDecoder
    , moduleEncoder
    , toReport
    , warningDecoder
    , warningEncoder
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Name exposing (Name)
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report exposing (Report(..))
import Time
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- MODULE


type alias Module =
    { name : ModuleName.Raw
    , absolutePath : String
    , modificationTime : Time.Posix
    , source : String
    , warnings : List Warning
    }



-- ALL POSSIBLE WARNINGS


type Warning
    = UnusedImport A.Region Name
    | UnusedVariable A.Region Context Name
    | MissingTypeAnnotation A.Region Name Can.Type


type Context
    = Def
    | Pattern



-- TO REPORT


toReport : Target -> L.Localizer -> Code.Source -> Warning -> Report
toReport target localizer source warning =
    case warning of
        UnusedImport region moduleName ->
            Report "UNUSED IMPORT" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow ("Nothing from the `" ++ moduleName ++ "` module is used in this file.")
                    , D.fromChars "I recommend removing unused imports."
                    )

        UnusedVariable region context name ->
            let
                title : String
                title =
                    defOrPat context "UNUSED DEFINITION" "UNUSED VARIABLE"
            in
            Report title region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow ("You are not using `" ++ name ++ "` anywhere.")
                    , D.stack
                        [ D.reflow <|
                            "Is there a typo? Maybe you intended to use `"
                                ++ name
                                ++ "` somewhere but typed another name instead?"
                        , D.reflow <|
                            defOrPat context
                                "If you are sure there is no typo, remove the definition. This way future readers will not have to wonder why it is there!"
                                ("If you are sure there is no typo, replace `"
                                    ++ name
                                    ++ "` with _ so future readers will not have to wonder why it is there!"
                                )
                        ]
                    )

        MissingTypeAnnotation region name inferredType ->
            Report "MISSING TYPE ANNOTATION" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        case Type.deepDealias inferredType of
                            Can.TLambda _ _ ->
                                "The `" ++ name ++ "` function has no type annotation."

                            _ ->
                                "The `" ++ name ++ "` definition has no type annotation."
                    , D.stack
                        [ D.fromChars "I inferred the type annotation myself though! You can copy it into your code:"
                        , D.green <|
                            D.indent 4 <|
                                D.sep
                                    [ D.fromName name |> D.a (D.fromChars " :")
                                    , RT.canToDoc target localizer RT.None inferredType
                                    ]
                        ]
                    )


defOrPat : Context -> a -> a -> a
defOrPat context def pat =
    case context of
        Def ->
            def

        Pattern ->
            pat



-- ENCODERS and DECODERS


moduleEncoder : Module -> BE.Encoder
moduleEncoder modul =
    BE.sequence
        [ ModuleName.rawEncoder modul.name
        , BE.string modul.absolutePath
        , BE.int (Time.posixToMillis modul.modificationTime)
        , BE.string modul.source
        , BE.list warningEncoder modul.warnings
        ]


moduleDecoder : BD.Decoder Module
moduleDecoder =
    BD.map5 Module
        ModuleName.rawDecoder
        BD.string
        (BD.map Time.millisToPosix BD.int)
        BD.string
        (BD.list warningDecoder)


warningEncoder : Warning -> BE.Encoder
warningEncoder warning =
    case warning of
        UnusedImport region name ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                , BE.string name
                ]

        UnusedVariable region context name ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder region
                , contextEncoder context
                , BE.string name
                ]

        MissingTypeAnnotation region name tipe ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.regionEncoder region
                , BE.string name
                , Can.typeEncoder tipe
                ]


warningDecoder : BD.Decoder Warning
warningDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 UnusedImport
                            A.regionDecoder
                            BD.string

                    1 ->
                        BD.map3 UnusedVariable
                            A.regionDecoder
                            contextDecoder
                            BD.string

                    2 ->
                        BD.map3 MissingTypeAnnotation
                            A.regionDecoder
                            BD.string
                            Can.typeDecoder

                    _ ->
                        BD.fail
            )


contextEncoder : Context -> BE.Encoder
contextEncoder context =
    case context of
        Def ->
            BE.unsignedInt8 0

        Pattern ->
            BE.unsignedInt8 1


contextDecoder : BD.Decoder Context
contextDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Def

                    1 ->
                        BD.succeed Pattern

                    _ ->
                        BD.fail
            )
