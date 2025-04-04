module Compiler.Reporting.Error.Docs exposing
    ( DefProblem(..)
    , Error(..)
    , NameProblem(..)
    , SyntaxProblem(..)
    , errorDecoder
    , errorEncoder
    , toReports
    )

import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Parse.Primitives exposing (Col, Row)
import Compiler.Parse.Symbol as Symbol exposing (BadOperator)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Error.Syntax as E
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE


type Error
    = NoDocs A.Region
    | ImplicitExposing A.Region
    | SyntaxProblem SyntaxProblem
    | NameProblems (NE.Nonempty NameProblem)
    | DefProblems (NE.Nonempty DefProblem)


type SyntaxProblem
    = Op Row Col
    | OpBad BadOperator Row Col
    | Name Row Col
    | Space E.Space Row Col
    | Comma Row Col
    | BadEnd Row Col


type NameProblem
    = NameDuplicate Name.Name A.Region A.Region
    | NameOnlyInDocs Name.Name A.Region
    | NameOnlyInExports Name.Name A.Region


type DefProblem
    = NoComment Name.Name A.Region
    | NoAnnotation Name.Name A.Region


toReports : Code.Source -> Error -> NE.Nonempty Report.Report
toReports source err =
    case err of
        NoDocs region ->
            NE.singleton <|
                Report.Report "NO DOCS" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow "You must have a documentation comment between the module declaration and the imports."
                        , D.reflow "Learn more at <https://package.elm-lang.org/help/documentation-format>"
                        )

        ImplicitExposing region ->
            NE.singleton <|
                Report.Report "IMPLICIT EXPOSING" region [] <|
                    Code.toSnippet source
                        region
                        Nothing
                        ( D.reflow "I need you to be explicit about what this module exposes:"
                        , D.reflow "A great API usually hides some implementation details, so it is rare that everything in the file should be exposed. And requiring package authors to be explicit about this is a way of adding another quality check before code gets published. So as you write out the public API, ask yourself if it will be easy to understand as people read the documentation!"
                        )

        SyntaxProblem problem ->
            NE.singleton <|
                toSyntaxProblemReport source problem

        NameProblems problems ->
            NE.map (toNameProblemReport source) problems

        DefProblems problems ->
            NE.map (toDefProblemReport source) problems


toSyntaxProblemReport : Code.Source -> SyntaxProblem -> Report.Report
toSyntaxProblemReport source problem =
    let
        toSyntaxReport : Row -> Col -> String -> Report.Report
        toSyntaxReport row col details =
            let
                region : A.Region
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
        Op row col ->
            toSyntaxReport row col "I am trying to parse an operator like (+) or (*) but something is going wrong."

        OpBad _ row col ->
            toSyntaxReport row col "I am trying to parse an operator like (+) or (*) but it looks like you are using a reserved symbol in this case."

        Name row col ->
            toSyntaxReport row col "I was expecting to see the name of another exposed value from this module."

        Space space row col ->
            E.toSpaceReport source space row col

        Comma row col ->
            toSyntaxReport row col "I was expecting to see a comma next."

        BadEnd row col ->
            toSyntaxReport row col "I am not really sure what I am getting stuck on though."


toRegion : Row -> Col -> A.Region
toRegion row col =
    let
        pos : A.Position
        pos =
            A.Position row col
    in
    A.Region pos pos


toNameProblemReport : Code.Source -> NameProblem -> Report.Report
toNameProblemReport source problem =
    case problem of
        NameDuplicate name r1 r2 ->
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

        NameOnlyInDocs name region ->
            Report.Report "DOCS MISTAKE" region [] <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow ("I do not see `" ++ name ++ "` in the `exposing` list, but it is in your module documentation:")
                    , D.reflow ("Does it need to be added to the `exposing` list as well? Or maybe you removed `" ++ name ++ "` and forgot to delete it here?")
                    )

        NameOnlyInExports name region ->
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


toDefProblemReport : Code.Source -> DefProblem -> Report.Report
toDefProblemReport source problem =
    case problem of
        NoComment name region ->
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

        NoAnnotation name region ->
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


errorEncoder : Error -> BE.Encoder
errorEncoder error =
    case error of
        NoDocs region ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                ]

        ImplicitExposing region ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder region
                ]

        SyntaxProblem problem ->
            BE.sequence
                [ BE.unsignedInt8 2
                , syntaxProblemEncoder problem
                ]

        NameProblems problems ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.nonempty nameProblemEncoder problems
                ]

        DefProblems problems ->
            BE.sequence
                [ BE.unsignedInt8 4
                , BE.nonempty defProblemEncoder problems
                ]


errorDecoder : BD.Decoder Error
errorDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map NoDocs A.regionDecoder

                    1 ->
                        BD.map ImplicitExposing A.regionDecoder

                    2 ->
                        BD.map SyntaxProblem syntaxProblemDecoder

                    3 ->
                        BD.map NameProblems (BD.nonempty nameProblemDecoder)

                    4 ->
                        BD.map DefProblems (BD.nonempty defProblemDecoder)

                    _ ->
                        BD.fail
            )


syntaxProblemEncoder : SyntaxProblem -> BE.Encoder
syntaxProblemEncoder syntaxProblem =
    case syntaxProblem of
        Op row col ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.int row
                , BE.int col
                ]

        OpBad badOperator row col ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Symbol.badOperatorEncoder badOperator
                , BE.int row
                , BE.int col
                ]

        Name row col ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.int row
                , BE.int col
                ]

        Space name row col ->
            BE.sequence
                [ BE.unsignedInt8 3
                , E.spaceEncoder name
                , BE.int row
                , BE.int col
                ]

        Comma row col ->
            BE.sequence
                [ BE.unsignedInt8 4
                , BE.int row
                , BE.int col
                ]

        BadEnd row col ->
            BE.sequence
                [ BE.unsignedInt8 5
                , BE.int row
                , BE.int col
                ]


syntaxProblemDecoder : BD.Decoder SyntaxProblem
syntaxProblemDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\type_ ->
                case type_ of
                    0 ->
                        BD.map2 Op
                            BD.int
                            BD.int

                    1 ->
                        BD.map3 OpBad
                            Symbol.badOperatorDecoder
                            BD.int
                            BD.int

                    2 ->
                        BD.map2 Name
                            BD.int
                            BD.int

                    3 ->
                        BD.map3 Space
                            E.spaceDecoder
                            BD.int
                            BD.int

                    4 ->
                        BD.map2 Comma
                            BD.int
                            BD.int

                    5 ->
                        BD.map2 BadEnd
                            BD.int
                            BD.int

                    _ ->
                        BD.fail
            )


nameProblemEncoder : NameProblem -> BE.Encoder
nameProblemEncoder nameProblem =
    case nameProblem of
        NameDuplicate name r1 r2 ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string name
                , A.regionEncoder r1
                , A.regionEncoder r2
                ]

        NameOnlyInDocs name region ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string name
                , A.regionEncoder region
                ]

        NameOnlyInExports name region ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string name
                , A.regionEncoder region
                ]


nameProblemDecoder : BD.Decoder NameProblem
nameProblemDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map3 NameDuplicate
                            BD.string
                            A.regionDecoder
                            A.regionDecoder

                    1 ->
                        BD.map2 NameOnlyInDocs
                            BD.string
                            A.regionDecoder

                    2 ->
                        BD.map2 NameOnlyInExports
                            BD.string
                            A.regionDecoder

                    _ ->
                        BD.fail
            )


defProblemEncoder : DefProblem -> BE.Encoder
defProblemEncoder defProblem =
    case defProblem of
        NoComment name region ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string name
                , A.regionEncoder region
                ]

        NoAnnotation name region ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string name
                , A.regionEncoder region
                ]


defProblemDecoder : BD.Decoder DefProblem
defProblemDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 NoComment
                            BD.string
                            A.regionDecoder

                    1 ->
                        BD.map2 NoAnnotation
                            BD.string
                            A.regionDecoder

                    _ ->
                        BD.fail
            )
