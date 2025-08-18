module Compiler.Parse.Type exposing
    ( expression
    , variant
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name exposing (Name)
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- TYPE TERMS


term : P.Parser E.Type Src.Type
term =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.TStart
                    [ -- types with no arguments (Int, Float, etc.)
                      Var.foreignUpper E.TStart
                        |> P.bind
                            (\upper ->
                                P.getPosition
                                    |> P.fmap
                                        (\end ->
                                            let
                                                region : A.Region
                                                region =
                                                    A.Region start end
                                            in
                                            A.At region <|
                                                case upper of
                                                    Var.Unqualified name ->
                                                        Src.TType region name []

                                                    Var.Qualified home name ->
                                                        Src.TTypeQual region home name []
                                        )
                            )
                    , -- type variables
                      Var.lower E.TStart
                        |> P.bind
                            (\var ->
                                P.addEnd start (Src.TVar var)
                            )
                    , -- tuples
                      P.inContext E.TTuple (P.word1 '(' E.TStart) <|
                        P.oneOf E.TTupleOpen
                            [ P.word1 ')' E.TTupleOpen
                                |> P.bind (\_ -> P.addEnd start Src.TUnit)
                            , Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentType1
                                |> P.bind
                                    (\trailingComments ->
                                        P.specialize E.TTupleType (expression trailingComments)
                                            |> P.bind
                                                (\( tipe, end ) ->
                                                    Space.checkIndent end E.TTupleIndentEnd
                                                        |> P.bind (\_ -> chompTupleEnd start tipe [])
                                                )
                                    )
                            ]
                    , -- records
                      P.inContext E.TRecord (P.word1 '{' E.TStart) <|
                        (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentOpen
                            |> P.bind
                                (\initialComments ->
                                    P.oneOf E.TRecordOpen
                                        [ P.word1 '}' E.TRecordEnd
                                            |> P.bind (\_ -> P.addEnd start (Src.TRecord [] Nothing initialComments))
                                        , P.addLocation (Var.lower E.TRecordField)
                                            |> P.bind
                                                (\name ->
                                                    Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                                                        |> P.bind
                                                            (\postNameComments ->
                                                                P.oneOf E.TRecordColon
                                                                    [ P.word1 '|' E.TRecordColon
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
                                                                                    |> P.bind
                                                                                        (\preFieldComments ->
                                                                                            chompField
                                                                                                |> P.bind
                                                                                                    (\( postFieldComments, field ) ->
                                                                                                        chompRecordEnd postFieldComments [ ( ( [], preFieldComments ), field ) ]
                                                                                                            |> P.bind (\( trailingComments, fields ) -> P.addEnd start (Src.TRecord fields (Just ( ( initialComments, postNameComments ), name )) trailingComments))
                                                                                                    )
                                                                                        )
                                                                            )
                                                                    , P.word1 ':' E.TRecordColon
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                                                                                    |> P.bind
                                                                                        (\preTypeComments ->
                                                                                            P.specialize E.TRecordType (expression [])
                                                                                                |> P.bind
                                                                                                    (\( ( ( _, postExpressionComments, _ ), tipe ), end ) ->
                                                                                                        Space.checkIndent end E.TRecordIndentEnd
                                                                                                            |> P.bind
                                                                                                                (\_ ->
                                                                                                                    chompRecordEnd postExpressionComments [ ( ( [], initialComments ), ( ( postNameComments, name ), ( preTypeComments, tipe ) ) ) ]
                                                                                                                        |> P.bind (\( trailingComments, fields ) -> P.addEnd start (Src.TRecord fields Nothing trailingComments))
                                                                                                                )
                                                                                                    )
                                                                                        )
                                                                            )
                                                                    ]
                                                            )
                                                )
                                        ]
                                )
                        )
                    ]
            )



-- TYPE EXPRESSIONS


expression : Src.FComments -> Space.Parser E.Type (Src.C2Eol Src.Type)
expression trailingComments =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.TStart
                    [ app start
                    , term
                        |> P.bind
                            (\eterm ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            Space.chomp E.TSpace
                                                |> P.fmap (\postTermComments -> ( ( postTermComments, eterm ), end ))
                                        )
                            )
                    ]
                    |> P.bind
                        (\( ( postTipe1comments, tipe1 ), end1 ) ->
                            P.oneOfWithFallback
                                [ -- should never trigger
                                  Space.checkIndent end1 E.TIndentStart
                                    |> P.bind
                                        (\_ ->
                                            -- could just be another type instead
                                            P.word2 '-' '>' E.TStart
                                                |> P.bind
                                                    (\_ ->
                                                        Space.chompAndCheckIndent E.TSpace E.TIndentStart
                                                            |> P.bind
                                                                (\postArrowComments ->
                                                                    expression postArrowComments
                                                                        |> P.fmap
                                                                            (\( ( ( preTipe2Comments, postTipe2Comments, tipe2Eol ), tipe2 ), end2 ) ->
                                                                                let
                                                                                    tipe : Src.Type
                                                                                    tipe =
                                                                                        A.at start end2 (Src.TLambda ( Nothing, tipe1 ) ( ( postTipe1comments, preTipe2Comments, tipe2Eol ), tipe2 ))
                                                                                in
                                                                                ( ( ( trailingComments, postTipe2Comments, Nothing ), tipe ), end2 )
                                                                            )
                                                                )
                                                    )
                                        )
                                ]
                                ( ( ( trailingComments, postTipe1comments, Nothing ), tipe1 ), end1 )
                        )
            )



-- TYPE CONSTRUCTORS


app : A.Position -> Space.Parser E.Type (Src.C1 Src.Type)
app start =
    Var.foreignUpper E.TStart
        |> P.bind
            (\upper ->
                P.getPosition
                    |> P.bind
                        (\upperEnd ->
                            Space.chomp E.TSpace
                                |> P.bind
                                    (\postUpperComments ->
                                        chompArgs postUpperComments [] upperEnd
                                            |> P.fmap
                                                (\( ( comments, args ), end ) ->
                                                    let
                                                        region : A.Region
                                                        region =
                                                            A.Region start upperEnd

                                                        tipe : Src.Type_
                                                        tipe =
                                                            case upper of
                                                                Var.Unqualified name ->
                                                                    Src.TType region name args

                                                                Var.Qualified home name ->
                                                                    Src.TTypeQual region home name args
                                                    in
                                                    ( ( comments, A.at start end tipe ), end )
                                                )
                                    )
                        )
            )


chompArgs : Src.FComments -> List (Src.C1 Src.Type) -> A.Position -> Space.Parser E.Type (Src.C1 (List (Src.C1 Src.Type)))
chompArgs preComments args end =
    P.oneOfWithFallback
        [ Space.checkIndent end E.TIndentStart
            |> P.bind
                (\_ ->
                    term
                        |> P.bind
                            (\arg ->
                                P.getPosition
                                    |> P.bind
                                        (\newEnd ->
                                            Space.chomp E.TSpace
                                                |> P.bind
                                                    (\comments ->
                                                        chompArgs comments (( preComments, arg ) :: args) newEnd
                                                    )
                                        )
                            )
                )
        ]
        ( ( preComments, List.reverse args ), end )



-- TUPLES


chompTupleEnd : A.Position -> Src.C2Eol Src.Type -> List (Src.C2Eol Src.Type) -> P.Parser E.TTuple Src.Type
chompTupleEnd start ( firstTimeComments, firstType ) revTypes =
    P.oneOf E.TTupleEnd
        [ P.word1 ',' E.TTupleEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentTypeN
                        |> P.bind
                            (\preExpressionComments ->
                                P.specialize E.TTupleType (expression preExpressionComments)
                                    |> P.bind
                                        (\( tipe, end ) ->
                                            Space.checkIndent end E.TTupleIndentEnd
                                                |> P.bind
                                                    (\_ ->
                                                        chompTupleEnd start ( firstTimeComments, firstType ) (tipe :: revTypes)
                                                    )
                                        )
                            )
                )
        , P.word1 ')' E.TTupleEnd
            |> P.bind (\_ -> P.getPosition)
            |> P.bind
                (\end ->
                    case List.reverse revTypes of
                        [] ->
                            case firstTimeComments of
                                ( [], [], _ ) ->
                                    P.pure firstType

                                ( startParensComments, endParensComments, _ ) ->
                                    P.pure (A.at start end (Src.TParens ( ( startParensComments, endParensComments ), firstType )))

                        secondType :: otherTypes ->
                            P.addEnd start (Src.TTuple ( firstTimeComments, firstType ) secondType otherTypes)
                )
        ]



-- RECORD


type alias Field =
    ( Src.C1 (A.Located Name), Src.C1 Src.Type )


chompRecordEnd : Src.FComments -> List (Src.C2 Field) -> P.Parser E.TRecord (Src.C1 (List (Src.C2 Field)))
chompRecordEnd comments fields =
    P.oneOf E.TRecordEnd
        [ P.word1 ',' E.TRecordEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
                        |> P.bind
                            (\preNameComments ->
                                chompField
                                    |> P.bind
                                        (\( postFieldComments, field ) ->
                                            chompRecordEnd postFieldComments (( ( comments, preNameComments ), field ) :: fields)
                                        )
                            )
                )
        , P.word1 '}' E.TRecordEnd
            |> P.fmap (\_ -> ( comments, List.reverse fields ))
        ]


chompField : P.Parser E.TRecord (Src.C1 Field)
chompField =
    P.addLocation (Var.lower E.TRecordField)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                    |> P.bind
                        (\postNameComments ->
                            P.word1 ':' E.TRecordColon
                                |> P.bind
                                    (\_ ->
                                        Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                                            |> P.bind
                                                (\preTypeComments ->
                                                    P.specialize E.TRecordType (expression [])
                                                        |> P.bind
                                                            (\( ( ( _, x1, _ ), tipe ), end ) ->
                                                                Space.checkIndent end E.TRecordIndentEnd
                                                                    |> P.fmap (\_ -> ( x1, ( ( postNameComments, name ), ( preTypeComments, tipe ) ) ))
                                                            )
                                                )
                                    )
                        )
            )



-- VARIANT


variant : Src.FComments -> Space.Parser E.CustomType (Src.C2Eol ( A.Located Name, List (Src.C1 Src.Type) ))
variant trailingComments =
    P.addLocation (Var.upper E.CT_Variant)
        |> P.bind
            (\((A.At (A.Region _ nameEnd) _) as name) ->
                Space.chomp E.CT_Space
                    |> P.bind
                        (\preArgComments ->
                            P.specialize E.CT_VariantArg (chompArgs preArgComments [] nameEnd)
                                |> P.fmap
                                    (\( ( postArgsComments, args ), end ) ->
                                        ( ( ( trailingComments, postArgsComments, Nothing ), ( name, args ) ), end )
                                    )
                        )
            )
