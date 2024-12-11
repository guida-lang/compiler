module Compiler.Reporting.Error.Main exposing
    ( Error(..)
    , errorCodec
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
import Serialize exposing (Codec)



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


errorCodec : Codec e Error
errorCodec =
    Serialize.customType
        (\badTypeEncoder badCycleEncoder badFlagsEncoder error ->
            case error of
                BadType region tipe ->
                    badTypeEncoder region tipe

                BadCycle region name names ->
                    badCycleEncoder region name names

                BadFlags region subType invalidPayload ->
                    badFlagsEncoder region subType invalidPayload
        )
        |> Serialize.variant2 BadType A.regionCodec Can.typeCodec
        |> Serialize.variant3 BadCycle A.regionCodec Serialize.string (Serialize.list Serialize.string)
        |> Serialize.variant3 BadFlags A.regionCodec Can.typeCodec E.invalidPayloadCodec
        |> Serialize.finishCustomType
