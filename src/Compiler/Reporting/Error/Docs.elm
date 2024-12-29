module Compiler.Reporting.Error.Docs exposing (toReports)

import Compiler.Data.NonEmptyList as NE
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Syntax as E
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Types as T


toReports : Code.Source -> T.CRED_Error -> NE.Nonempty Report.Report
toReports source err =
    case err of
        T.CRED_NoDocs region ->
            NE.singleton <|
                Report.Report "NO DOCS" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow "You must have a documentation comment between the module declaration and the imports."
                        , D.reflow "Learn more at <https://package.elm-lang.org/help/documentation-format>"
                        )

        T.CRED_ImplicitExposing region ->
            NE.singleton <|
                Report.Report "IMPLICIT EXPOSING" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow "I need you to be explicit about what this module exposes:"
                        , D.reflow "A great API usually hides some implementation details, so it is rare that everything in the file should be exposed. And requiring package authors to be explicit about this is a way of adding another quality check before code gets published. So as you write out the public API, ask yourself if it will be easy to understand as people read the documentation!"
                        )

        T.CRED_SyntaxProblem problem ->
            NE.singleton <|
                toSyntaxProblemReport source problem

        T.CRED_NameProblems problems ->
            NE.map (toNameProblemReport source) problems

        T.CRED_DefProblems problems ->
            NE.map (toDefProblemReport source) problems


toSyntaxProblemReport : Code.Source -> T.CRED_SyntaxProblem -> Report.Report
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
        T.CRED_Op row col ->
            toSyntaxReport row col "I am trying to parse an operator like (+) or (*) but something is going wrong."

        T.CRED_OpBad _ row col ->
            toSyntaxReport row col "I am trying to parse an operator like (+) or (*) but it looks like you are using a reserved symbol in this case."

        T.CRED_Name row col ->
            toSyntaxReport row col "I was expecting to see the name of another exposed value from this module."

        T.CRED_Space space row col ->
            E.toSpaceReport source space row col

        T.CRED_Comma row col ->
            toSyntaxReport row col "I was expecting to see a comma next."

        T.CRED_BadEnd row col ->
            toSyntaxReport row col "I am not really sure what I am getting stuck on though."


toRegion : T.CPP_Row -> T.CPP_Col -> T.CRA_Region
toRegion row col =
    let
        pos : T.CRA_Position
        pos =
            T.CRA_Position row col
    in
    T.CRA_Region pos pos


toNameProblemReport : Code.Source -> T.CRED_NameProblem -> Report.Report
toNameProblemReport source problem =
    case problem of
        T.CRED_NameDuplicate name r1 r2 ->
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

        T.CRED_NameOnlyInDocs name region ->
            Report.Report "DOCS MISTAKE" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow ("I do not see `" ++ name ++ "` in the `exposing` list, but it is in your module documentation:")
                    , D.reflow ("Does it need to be added to the `exposing` list as well? Or maybe you removed `" ++ name ++ "` and forgot to delete it here?")
                    )

        T.CRED_NameOnlyInExports name region ->
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


toDefProblemReport : Code.Source -> T.CRED_DefProblem -> Report.Report
toDefProblemReport source problem =
    case problem of
        T.CRED_NoComment name region ->
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

        T.CRED_NoAnnotation name region ->
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
