module Compiler.Reporting.Error.Main exposing (toReport)

import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Report as Report
import Types as T



-- TO REPORT


toReport : T.CRRTL_Localizer -> Code.Source -> T.CREM_Error -> Report.Report
toReport localizer source err =
    case err of
        T.CREM_BadType region tipe ->
            Report.Report "BAD MAIN TYPE" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "I cannot handle this type of `main` value:"
                    , D.stack
                        [ D.fromChars "The type of `main` value I am seeing is:"
                        , D.indent 4 <| D.dullyellow <| RT.canToDoc localizer RT.None tipe
                        , D.reflow "I only know how to handle Html, Svg, and Programs though. Modify `main` to be one of those types of values!"
                        ]
                    )

        T.CREM_BadCycle region name names ->
            Report.Report "BAD MAIN" region [] <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "A `main` definition cannot be defined in terms of itself."
                    , D.stack
                        [ D.reflow "It should be a boring value with no recursion. But instead it is involved in this cycle of definitions:"
                        , D.cycle 4 name names
                        ]
                    )

        T.CREM_BadFlags region _ invalidPayload ->
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
                    T.CREC_ExtendedRecord ->
                        ( "an extended record"
                        , D.reflow "But the exact shape of the record must be known at compile time. No type variables!"
                        )

                    T.CREC_Function ->
                        ( "a function"
                        , D.reflow "But if I allowed functions from JS, it would be possible to sneak side-effects and runtime exceptions into Elm!"
                        )

                    T.CREC_TypeVariable name ->
                        ( "an unspecified type"
                        , D.reflow ("But type variables like `" ++ name ++ "` cannot be given as flags. I need to know exactly what type of data I am getting, so I can guarantee that unexpected data cannot sneak in and crash the Elm program.")
                        )

                    T.CREC_UnsupportedType name ->
                        ( "a `" ++ name ++ "` value"
                        , D.stack
                            [ D.reflow "I cannot handle that. The types that CAN be in flags include:"
                            , D.indent 4 <|
                                D.reflow "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, tuples, records, and JSON values."
                            , D.reflow "Since JSON values can flow through, you can use JSON encoders and decoders to allow other types through as well. More advanced users often just do everything with encoders and decoders for more control and better errors."
                            ]
                        )
