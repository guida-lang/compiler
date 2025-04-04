module Compiler.Reporting.Error.Main exposing
    ( Error(..)
    , errorDecoder
    , errorEncoder
    , toReport
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Name exposing (Name)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Canonicalize as E
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report as Report
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- ERROR


type Error
    = BadType A.Region Can.Type
    | BadCycle A.Region Name (List Name)
    | BadFlags A.Region Can.Type E.InvalidPayload



-- TO REPORT


toReport : L.Localizer -> Code.Source -> Error -> Report.Report
toReport localizer source err =
    case err of
        BadType region tipe ->
            Report.Report "BAD MAIN TYPE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "I cannot handle this type of `main` value:"
                    , D.stack
                        [ D.fromChars "The type of `main` value I am seeing is:"
                        , D.indent 4 <| D.dullyellow <| RT.canToDoc localizer RT.None tipe
                        , D.reflow "I only know how to handle Html, Svg, and Programs though. Modify `main` to be one of those types of values!"
                        ]
                    )

        BadCycle region name names ->
            Report.Report "BAD MAIN" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "A `main` definition cannot be defined in terms of itself."
                    , D.stack
                        [ D.reflow "It should be a boring value with no recursion. But instead it is involved in this cycle of definitions:"
                        , D.cycle 4 name names
                        ]
                    )

        BadFlags region _ invalidPayload ->
            let
                formatDetails : ( String, D.Doc ) -> Report.Report
                formatDetails ( aBadKindOfThing, butThatIsNoGood ) =
                    Report.Report "BAD FLAGS" region [] <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow ("Your `main` program wants " ++ aBadKindOfThing ++ " from JavaScript.")
                            , butThatIsNoGood
                            )
            in
            formatDetails <|
                case invalidPayload of
                    E.ExtendedRecord ->
                        ( "an extended record"
                        , D.reflow "But the exact shape of the record must be known at compile time. No type variables!"
                        )

                    E.Function ->
                        ( "a function"
                        , D.reflow "But if I allowed functions from JS, it would be possible to sneak side-effects and runtime exceptions into Elm!"
                        )

                    E.TypeVariable name ->
                        ( "an unspecified type"
                        , D.reflow ("But type variables like `" ++ name ++ "` cannot be given as flags. I need to know exactly what type of data I am getting, so I can guarantee that unexpected data cannot sneak in and crash the Elm program.")
                        )

                    E.UnsupportedType name ->
                        ( "a `" ++ name ++ "` value"
                        , D.stack
                            [ D.reflow "I cannot handle that. The types that CAN be in flags include:"
                            , D.indent 4 <|
                                D.reflow "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, tuples, records, and JSON values."
                            , D.reflow "Since JSON values can flow through, you can use JSON encoders and decoders to allow other types through as well. More advanced users often just do everything with encoders and decoders for more control and better errors."
                            ]
                        )



-- ENCODERS and DECODERS


errorEncoder : Error -> BE.Encoder
errorEncoder error =
    case error of
        BadType region tipe ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                , Can.typeEncoder tipe
                ]

        BadCycle region name names ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder region
                , BE.string name
                , BE.list BE.string names
                ]

        BadFlags region subType invalidPayload ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.regionEncoder region
                , Can.typeEncoder subType
                , E.invalidPayloadEncoder invalidPayload
                ]


errorDecoder : BD.Decoder Error
errorDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 BadType
                            A.regionDecoder
                            Can.typeDecoder

                    1 ->
                        BD.map3 BadCycle
                            A.regionDecoder
                            BD.string
                            (BD.list BD.string)

                    2 ->
                        BD.map3 BadFlags
                            A.regionDecoder
                            Can.typeDecoder
                            E.invalidPayloadDecoder

                    _ ->
                        BD.fail
            )
