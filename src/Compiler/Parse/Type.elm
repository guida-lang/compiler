module Compiler.Parse.Type exposing
    ( expression
    , variant
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name exposing (CDN_Name)
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- TYPE TERMS


term : P.Parser E.Type Src.CASTS_Type
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
                                                region : A.CRA_Region
                                                region =
                                                    A.CRA_Region start end
                                            in
                                            A.CRA_At region <|
                                                case upper of
                                                    Var.Unqualified name ->
                                                        Src.CASTS_TType region name []

                                                    Var.Qualified home name ->
                                                        Src.CASTS_TTypeQual region home name []
                                        )
                            )
                    , -- type variables
                      Var.lower E.TStart
                        |> P.bind
                            (\var ->
                                P.addEnd start (Src.CASTS_TVar var)
                            )
                    , -- tuples
                      P.inContext E.TTuple (P.word1 '(' E.TStart) <|
                        P.oneOf E.TTupleOpen
                            [ P.word1 ')' E.TTupleOpen
                                |> P.bind (\_ -> P.addEnd start Src.CASTS_TUnit)
                            , Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentType1
                                |> P.bind
                                    (\_ ->
                                        P.specialize E.TTupleType expression
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
                                (\_ ->
                                    P.oneOf E.TRecordOpen
                                        [ P.word1 '}' E.TRecordEnd
                                            |> P.bind (\_ -> P.addEnd start (Src.CASTS_TRecord [] Nothing))
                                        , P.addLocation (Var.lower E.TRecordField)
                                            |> P.bind
                                                (\name ->
                                                    Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                                                        |> P.bind
                                                            (\_ ->
                                                                P.oneOf E.TRecordColon
                                                                    [ P.word1 '|' E.TRecordColon
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
                                                                                    |> P.bind
                                                                                        (\_ ->
                                                                                            chompField
                                                                                                |> P.bind
                                                                                                    (\field ->
                                                                                                        chompRecordEnd [ field ]
                                                                                                            |> P.bind (\fields -> P.addEnd start (Src.CASTS_TRecord fields (Just name)))
                                                                                                    )
                                                                                        )
                                                                            )
                                                                    , P.word1 ':' E.TRecordColon
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                                                                                    |> P.bind
                                                                                        (\_ ->
                                                                                            P.specialize E.TRecordType expression
                                                                                                |> P.bind
                                                                                                    (\( tipe, end ) ->
                                                                                                        Space.checkIndent end E.TRecordIndentEnd
                                                                                                            |> P.bind
                                                                                                                (\_ ->
                                                                                                                    chompRecordEnd [ ( name, tipe ) ]
                                                                                                                        |> P.bind (\fields -> P.addEnd start (Src.CASTS_TRecord fields Nothing))
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


expression : Space.Parser E.Type Src.CASTS_Type
expression =
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
                                                |> P.fmap (\_ -> ( eterm, end ))
                                        )
                            )
                    ]
                    |> P.bind
                        (\(( tipe1, end1 ) as term1) ->
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
                                                                (\_ ->
                                                                    expression
                                                                        |> P.fmap
                                                                            (\( tipe2, end2 ) ->
                                                                                let
                                                                                    tipe : A.CRA_Located Src.CASTS_Type_
                                                                                    tipe =
                                                                                        A.at start end2 (Src.CASTS_TLambda tipe1 tipe2)
                                                                                in
                                                                                ( tipe, end2 )
                                                                            )
                                                                )
                                                    )
                                        )
                                ]
                                term1
                        )
            )



-- TYPE CONSTRUCTORS


app : A.CRA_Position -> Space.Parser E.Type Src.CASTS_Type
app start =
    Var.foreignUpper E.TStart
        |> P.bind
            (\upper ->
                P.getPosition
                    |> P.bind
                        (\upperEnd ->
                            Space.chomp E.TSpace
                                |> P.bind
                                    (\_ ->
                                        chompArgs [] upperEnd
                                            |> P.fmap
                                                (\( args, end ) ->
                                                    let
                                                        region : A.CRA_Region
                                                        region =
                                                            A.CRA_Region start upperEnd

                                                        tipe : Src.CASTS_Type_
                                                        tipe =
                                                            case upper of
                                                                Var.Unqualified name ->
                                                                    Src.CASTS_TType region name args

                                                                Var.Qualified home name ->
                                                                    Src.CASTS_TTypeQual region home name args
                                                    in
                                                    ( A.at start end tipe, end )
                                                )
                                    )
                        )
            )


chompArgs : List Src.CASTS_Type -> A.CRA_Position -> Space.Parser E.Type (List Src.CASTS_Type)
chompArgs args end =
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
                                                    (\_ ->
                                                        chompArgs (arg :: args) newEnd
                                                    )
                                        )
                            )
                )
        ]
        ( List.reverse args, end )



-- TUPLES


chompTupleEnd : A.CRA_Position -> Src.CASTS_Type -> List Src.CASTS_Type -> P.Parser E.TTuple Src.CASTS_Type
chompTupleEnd start firstType revTypes =
    P.oneOf E.TTupleEnd
        [ P.word1 ',' E.TTupleEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentTypeN
                        |> P.bind
                            (\_ ->
                                P.specialize E.TTupleType expression
                                    |> P.bind
                                        (\( tipe, end ) ->
                                            Space.checkIndent end E.TTupleIndentEnd
                                                |> P.bind
                                                    (\_ ->
                                                        chompTupleEnd start firstType (tipe :: revTypes)
                                                    )
                                        )
                            )
                )
        , P.word1 ')' E.TTupleEnd
            |> P.bind
                (\_ ->
                    case List.reverse revTypes of
                        [] ->
                            P.pure firstType

                        secondType :: otherTypes ->
                            P.addEnd start (Src.CASTS_TTuple firstType secondType otherTypes)
                )
        ]



-- RECORD


type alias Field =
    ( A.CRA_Located CDN_Name, Src.CASTS_Type )


chompRecordEnd : List Field -> P.Parser E.TRecord (List Field)
chompRecordEnd fields =
    P.oneOf E.TRecordEnd
        [ P.word1 ',' E.TRecordEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
                        |> P.bind
                            (\_ ->
                                chompField
                                    |> P.bind
                                        (\field ->
                                            chompRecordEnd (field :: fields)
                                        )
                            )
                )
        , P.word1 '}' E.TRecordEnd
            |> P.fmap (\_ -> List.reverse fields)
        ]


chompField : P.Parser E.TRecord Field
chompField =
    P.addLocation (Var.lower E.TRecordField)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                    |> P.bind
                        (\_ ->
                            P.word1 ':' E.TRecordColon
                                |> P.bind
                                    (\_ ->
                                        Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                                            |> P.bind
                                                (\_ ->
                                                    P.specialize E.TRecordType expression
                                                        |> P.bind
                                                            (\( tipe, end ) ->
                                                                Space.checkIndent end E.TRecordIndentEnd
                                                                    |> P.fmap (\_ -> ( name, tipe ))
                                                            )
                                                )
                                    )
                        )
            )



-- VARIANT


variant : Space.Parser E.CustomType ( A.CRA_Located CDN_Name, List Src.CASTS_Type )
variant =
    P.addLocation (Var.upper E.CT_Variant)
        |> P.bind
            (\((A.CRA_At (A.CRA_Region _ nameEnd) _) as name) ->
                Space.chomp E.CT_Space
                    |> P.bind
                        (\_ ->
                            P.specialize E.CT_VariantArg (chompArgs [] nameEnd)
                                |> P.fmap
                                    (\( args, end ) ->
                                        ( ( name, args ), end )
                                    )
                        )
            )
