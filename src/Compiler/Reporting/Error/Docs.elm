module Compiler.Reporting.Error.Docs exposing
    ( CRED_DefProblem(..)
    , CRED_Error(..)
    , CRED_NameProblem(..)
    , CRED_SyntaxProblem(..)
    , errorDecoder
    , errorEncoder
    , toReports
    )

import Compiler.Data.NonEmptyList as NE
import Compiler.Json.Decode as DecodeX
import Compiler.Json.Encode as EncodeX
import Compiler.Parse.Symbol exposing (CPS_BadOperator)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Syntax as E
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T


type CRED_Error
    = CRED_NoDocs T.CRA_Region
    | CRED_ImplicitExposing T.CRA_Region
    | CRED_SyntaxProblem CRED_SyntaxProblem
    | CRED_NameProblems (NE.Nonempty CRED_NameProblem)
    | CRED_DefProblems (NE.Nonempty CRED_DefProblem)


type CRED_SyntaxProblem
    = CRED_Op T.CPP_Row T.CPP_Col
    | CRED_OpBad CPS_BadOperator T.CPP_Row T.CPP_Col
    | CRED_Name T.CPP_Row T.CPP_Col
    | CRED_Space E.CRES_Space T.CPP_Row T.CPP_Col
    | CRED_Comma T.CPP_Row T.CPP_Col
    | CRED_BadEnd T.CPP_Row T.CPP_Col


type CRED_NameProblem
    = CRED_NameDuplicate T.CDN_Name T.CRA_Region T.CRA_Region
    | CRED_NameOnlyInDocs T.CDN_Name T.CRA_Region
    | CRED_NameOnlyInExports T.CDN_Name T.CRA_Region


type CRED_DefProblem
    = CRED_NoComment T.CDN_Name T.CRA_Region
    | CRED_NoAnnotation T.CDN_Name T.CRA_Region


toReports : Code.Source -> CRED_Error -> NE.Nonempty Report.Report
toReports source err =
    case err of
        CRED_NoDocs region ->
            NE.singleton <|
                Report.Report "NO DOCS" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow "You must have a documentation comment between the module declaration and the imports."
                        , D.reflow "Learn more at <https://package.elm-lang.org/help/documentation-format>"
                        )

        CRED_ImplicitExposing region ->
            NE.singleton <|
                Report.Report "IMPLICIT EXPOSING" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow "I need you to be explicit about what this module exposes:"
                        , D.reflow "A great API usually hides some implementation details, so it is rare that everything in the file should be exposed. And requiring package authors to be explicit about this is a way of adding another quality check before code gets published. So as you write out the public API, ask yourself if it will be easy to understand as people read the documentation!"
                        )

        CRED_SyntaxProblem problem ->
            NE.singleton <|
                toSyntaxProblemReport source problem

        CRED_NameProblems problems ->
            NE.map (toNameProblemReport source) problems

        CRED_DefProblems problems ->
            NE.map (toDefProblemReport source) problems


toSyntaxProblemReport : Code.Source -> CRED_SyntaxProblem -> Report.Report
toSyntaxProblemReport source problem =
    let
        toSyntaxReport : T.CPP_Row -> T.CPP_Col -> String -> Report.Report
        toSyntaxReport row col details =
            let
                region : T.CRA_Region
                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN DOCS" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow "I was partway through parsing your module documentation, but I got stuck here:"
                    , D.stack
                        [ D.reflow details
                        , D.toSimpleHint "Read through <https://package.elm-lang.org/help/documentation-format> for tips on how to write module documentation!"
                        ]
                    )
    in
    case problem of
        CRED_Op row col ->
            toSyntaxReport row col "I am trying to parse an operator like (+) or (*) but something is going wrong."

        CRED_OpBad _ row col ->
            toSyntaxReport row col "I am trying to parse an operator like (+) or (*) but it looks like you are using a reserved symbol in this case."

        CRED_Name row col ->
            toSyntaxReport row col "I was expecting to see the name of another exposed value from this module."

        CRED_Space space row col ->
            E.toSpaceReport source space row col

        CRED_Comma row col ->
            toSyntaxReport row col "I was expecting to see a comma next."

        CRED_BadEnd row col ->
            toSyntaxReport row col "I am not really sure what I am getting stuck on though."


toRegion : T.CPP_Row -> T.CPP_Col -> T.CRA_Region
toRegion row col =
    let
        pos : T.CRA_Position
        pos =
            T.CRA_Position row col
    in
    T.CRA_Region pos pos


toNameProblemReport : Code.Source -> CRED_NameProblem -> Report.Report
toNameProblemReport source problem =
    case problem of
        CRED_NameDuplicate name r1 r2 ->
            Report.Report "DUPLICATE DOCS" r2 [] <|
                Code.toPair source
                    r1
                    r2
                    ( D.reflow ("There can only be one `" ++ name ++ "` in your module documentation, but it is listed twice:")
                    , D.fromChars "Remove one of them!"
                    )
                    ( D.reflow ("There can only be one `" ++ name ++ "` in your module documentation, but I see two. One here:")
                    , D.fromChars "And another one over here:"
                    , D.fromChars "Remove one of them!"
                    )

        CRED_NameOnlyInDocs name region ->
            Report.Report "DOCS MISTAKE" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow ("I do not see `" ++ name ++ "` in the `exposing` list, but it is in your module documentation:")
                    , D.reflow ("Does it need to be added to the `exposing` list as well? Or maybe you removed `" ++ name ++ "` and forgot to delete it here?")
                    )

        CRED_NameOnlyInExports name region ->
            Report.Report "DOCS MISTAKE" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow ("I do not see `" ++ name ++ "` in your module documentation, but it is in your `exposing` list:")
                    , D.stack
                        [ D.reflow ("Add a line like `@docs " ++ name ++ "` to your module documentation!")
                        , D.link "Note" "See" "docs" "for more guidance on writing high quality docs."
                        ]
                    )


toDefProblemReport : Code.Source -> CRED_DefProblem -> Report.Report
toDefProblemReport source problem =
    case problem of
        CRED_NoComment name region ->
            Report.Report "NO DOCS" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow ("The `" ++ name ++ "` definition does not have a documentation comment.")
                    , D.stack
                        [ D.reflow "Add documentation with nice examples of how to use it!"
                        , D.link "Note" "Read" "docs" "for more advice on writing great docs. There are a couple important tricks!"
                        ]
                    )

        CRED_NoAnnotation name region ->
            Report.Report "NO TYPE ANNOTATION" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow ("The `" ++ name ++ "` definition does not have a type annotation.")
                    , D.stack
                        [ D.reflow "I use the type variable names from your annotations when generating docs. So if you say `Html msg` in your type annotation, I can use `msg` in the docs and make them a bit clearer. So add an annotation and try to use nice type variables!"
                        , D.link "Note" "Read" "docs" "for more advice on writing great docs. There are a couple important tricks!"
                        ]
                    )



-- ENCODERS and DECODERS


errorEncoder : CRED_Error -> Encode.Value
errorEncoder error =
    case error of
        CRED_NoDocs region ->
            Encode.object
                [ ( "type", Encode.string "NoDocs" )
                , ( "region", A.regionEncoder region )
                ]

        CRED_ImplicitExposing region ->
            Encode.object
                [ ( "type", Encode.string "ImplicitExposing" )
                , ( "region", A.regionEncoder region )
                ]

        CRED_SyntaxProblem problem ->
            Encode.object
                [ ( "type", Encode.string "SyntaxProblem" )
                , ( "problem", syntaxProblemEncoder problem )
                ]

        CRED_NameProblems problems ->
            Encode.object
                [ ( "type", Encode.string "NameProblems" )
                , ( "problems", EncodeX.nonempty nameProblemEncoder problems )
                ]

        CRED_DefProblems problems ->
            Encode.object
                [ ( "type", Encode.string "DefProblems" )
                , ( "problems", EncodeX.nonempty defProblemEncoder problems )
                ]


errorDecoder : Decode.Decoder CRED_Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoDocs" ->
                        Decode.map CRED_NoDocs (Decode.field "region" A.regionDecoder)

                    "ImplicitExposing" ->
                        Decode.map CRED_ImplicitExposing (Decode.field "region" A.regionDecoder)

                    "SyntaxProblem" ->
                        Decode.map CRED_SyntaxProblem (Decode.field "problem" syntaxProblemDecoder)

                    "NameProblems" ->
                        Decode.map CRED_NameProblems (Decode.field "problems" (DecodeX.nonempty nameProblemDecoder))

                    "DefProblems" ->
                        Decode.map CRED_DefProblems (Decode.field "problems" (DecodeX.nonempty defProblemDecoder))

                    _ ->
                        Decode.fail ("Failed to decode Error's type: " ++ type_)
            )


syntaxProblemEncoder : CRED_SyntaxProblem -> Encode.Value
syntaxProblemEncoder syntaxProblem =
    case syntaxProblem of
        CRED_Op row col ->
            Encode.object
                [ ( "type", Encode.string "Op" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        CRED_OpBad badOperator row col ->
            Encode.object
                [ ( "type", Encode.string "OpBad" )
                , ( "badOperator", Compiler.Parse.Symbol.badOperatorEncoder badOperator )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        CRED_Name row col ->
            Encode.object
                [ ( "type", Encode.string "Name" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        CRED_Space name row col ->
            Encode.object
                [ ( "type", Encode.string "Space" )
                , ( "name", E.spaceEncoder name )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        CRED_Comma row col ->
            Encode.object
                [ ( "type", Encode.string "Comma" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]

        CRED_BadEnd row col ->
            Encode.object
                [ ( "type", Encode.string "BadEnd" )
                , ( "row", Encode.int row )
                , ( "col", Encode.int col )
                ]


syntaxProblemDecoder : Decode.Decoder CRED_SyntaxProblem
syntaxProblemDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Op" ->
                        Decode.map2 CRED_Op
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "OpBad" ->
                        Decode.map3 CRED_OpBad
                            (Decode.field "badOperator" Compiler.Parse.Symbol.badOperatorDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Name" ->
                        Decode.map2 CRED_Name
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Space" ->
                        Decode.map3 CRED_Space
                            (Decode.field "name" E.spaceDecoder)
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "Comma" ->
                        Decode.map2 CRED_Comma
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    "BadEnd" ->
                        Decode.map2 CRED_BadEnd
                            (Decode.field "row" Decode.int)
                            (Decode.field "col" Decode.int)

                    _ ->
                        Decode.fail ("Failed to decode SyntaxProblem's type: " ++ type_)
            )


nameProblemEncoder : CRED_NameProblem -> Encode.Value
nameProblemEncoder nameProblem =
    case nameProblem of
        CRED_NameDuplicate name r1 r2 ->
            Encode.object
                [ ( "type", Encode.string "NameDuplicate" )
                , ( "name", Encode.string name )
                , ( "r1", A.regionEncoder r1 )
                , ( "r2", A.regionEncoder r2 )
                ]

        CRED_NameOnlyInDocs name region ->
            Encode.object
                [ ( "type", Encode.string "NameOnlyInDocs" )
                , ( "name", Encode.string name )
                , ( "region", A.regionEncoder region )
                ]

        CRED_NameOnlyInExports name region ->
            Encode.object
                [ ( "type", Encode.string "NameOnlyInExports" )
                , ( "name", Encode.string name )
                , ( "region", A.regionEncoder region )
                ]


nameProblemDecoder : Decode.Decoder CRED_NameProblem
nameProblemDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NameDuplicate" ->
                        Decode.map3 CRED_NameDuplicate
                            (Decode.field "name" Decode.string)
                            (Decode.field "r1" A.regionDecoder)
                            (Decode.field "r2" A.regionDecoder)

                    "NameOnlyInDocs" ->
                        Decode.map2 CRED_NameOnlyInDocs
                            (Decode.field "name" Decode.string)
                            (Decode.field "region" A.regionDecoder)

                    "NameOnlyInExports" ->
                        Decode.map2 CRED_NameOnlyInExports
                            (Decode.field "name" Decode.string)
                            (Decode.field "region" A.regionDecoder)

                    _ ->
                        Decode.fail ("Failed to decode NameProblem's type: " ++ type_)
            )


defProblemEncoder : CRED_DefProblem -> Encode.Value
defProblemEncoder defProblem =
    case defProblem of
        CRED_NoComment name region ->
            Encode.object
                [ ( "type", Encode.string "NoComment" )
                , ( "name", Encode.string name )
                , ( "region", A.regionEncoder region )
                ]

        CRED_NoAnnotation name region ->
            Encode.object
                [ ( "type", Encode.string "NoAnnotation" )
                , ( "name", Encode.string name )
                , ( "region", A.regionEncoder region )
                ]


defProblemDecoder : Decode.Decoder CRED_DefProblem
defProblemDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "NoComment" ->
                        Decode.map2 CRED_NoComment
                            (Decode.field "name" Decode.string)
                            (Decode.field "region" A.regionDecoder)

                    "NoAnnotation" ->
                        Decode.map2 CRED_NoAnnotation
                            (Decode.field "name" Decode.string)
                            (Decode.field "region" A.regionDecoder)

                    _ ->
                        Decode.fail ("Failed to decode DefProblem's type: " ++ type_)
            )
