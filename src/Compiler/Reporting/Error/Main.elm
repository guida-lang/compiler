module Compiler.Reporting.Error.Main exposing
    ( CREM_Error(..)
    , errorDecoder
    , errorEncoder
    , toReport
    )

import Compiler.AST.Canonical as Can
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Canonicalize as E
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report as Report
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- ERROR


type CREM_Error
    = CREM_BadType T.CRA_Region T.CASTC_Type
    | CREM_BadCycle T.CRA_Region T.CDN_Name (List T.CDN_Name)
    | CREM_BadFlags T.CRA_Region T.CASTC_Type E.CREC_InvalidPayload



-- TO REPORT


toReport : L.CRRTL_Localizer -> Code.Source -> CREM_Error -> Report.Report
toReport localizer source err =
    case err of
        CREM_BadType region tipe ->
            Report.Report "BAD MAIN TYPE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "I cannot handle this type of `main` value:"
                    , D.stack
                        [ D.fromChars "The type of `main` value I am seeing is:"
                        , D.indent 4 <| D.dullyellow <| RT.canToDoc localizer RT.None tipe
                        , D.reflow "I only know how to handle Html, Svg, and Programs though. Modify `main` to be one of those types of values!"
                        ]
                    )

        CREM_BadCycle region name names ->
            Report.Report "BAD MAIN" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "A `main` definition cannot be defined in terms of itself."
                    , D.stack
                        [ D.reflow "It should be a boring value with no recursion. But instead it is involved in this cycle of definitions:"
                        , D.cycle 4 name names
                        ]
                    )

        CREM_BadFlags region _ invalidPayload ->
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
                    E.CREC_ExtendedRecord ->
                        ( "an extended record"
                        , D.reflow "But the exact shape of the record must be known at compile time. No type variables!"
                        )

                    E.CREC_Function ->
                        ( "a function"
                        , D.reflow "But if I allowed functions from JS, it would be possible to sneak side-effects and runtime exceptions into Elm!"
                        )

                    E.CREC_TypeVariable name ->
                        ( "an unspecified type"
                        , D.reflow ("But type variables like `" ++ name ++ "` cannot be given as flags. I need to know exactly what type of data I am getting, so I can guarantee that unexpected data cannot sneak in and crash the Elm program.")
                        )

                    E.CREC_UnsupportedType name ->
                        ( "a `" ++ name ++ "` value"
                        , D.stack
                            [ D.reflow "I cannot handle that. The types that CAN be in flags include:"
                            , D.indent 4 <|
                                D.reflow "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, tuples, records, and JSON values."
                            , D.reflow "Since JSON values can flow through, you can use JSON encoders and decoders to allow other types through as well. More advanced users often just do everything with encoders and decoders for more control and better errors."
                            ]
                        )



-- ENCODERS and DECODERS


errorEncoder : CREM_Error -> Encode.Value
errorEncoder error =
    case error of
        CREM_BadType region tipe ->
            Encode.object
                [ ( "type", Encode.string "BadType" )
                , ( "region", A.regionEncoder region )
                , ( "tipe", Can.typeEncoder tipe )
                ]

        CREM_BadCycle region name names ->
            Encode.object
                [ ( "type", Encode.string "BadCycle" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "names", Encode.list Encode.string names )
                ]

        CREM_BadFlags region subType invalidPayload ->
            Encode.object
                [ ( "type", Encode.string "BadFlags" )
                , ( "region", A.regionEncoder region )
                , ( "subType", Can.typeEncoder subType )
                , ( "invalidPayload", E.invalidPayloadEncoder invalidPayload )
                ]


errorDecoder : Decode.Decoder CREM_Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "BadType" ->
                        Decode.map2 CREM_BadType
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "tipe" Can.typeDecoder)

                    "BadCycle" ->
                        Decode.map3 CREM_BadCycle
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "names" (Decode.list Decode.string))

                    "BadFlags" ->
                        Decode.map3 CREM_BadFlags
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "subType" Can.typeDecoder)
                            (Decode.field "invalidPayload" E.invalidPayloadDecoder)

                    _ ->
                        Decode.fail ("Failed to decode Error's type: " ++ type_)
            )
