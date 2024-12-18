module Compiler.Parse.Type exposing
    ( expression
    , variant
    )

import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Types as T



-- TYPE TERMS


term : P.Parser E.CRES_Type T.CASTS_Type
term =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.CRES_TStart
                    [ -- types with no arguments (Int, Float, etc.)
                      Var.foreignUpper E.CRES_TStart
                        |> P.bind
                            (\upper ->
                                P.getPosition
                                    |> P.fmap
                                        (\end ->
                                            let
                                                region : T.CRA_Region
                                                region =
                                                    T.CRA_Region start end
                                            in
                                            T.CRA_At region <|
                                                case upper of
                                                    Var.Unqualified name ->
                                                        T.CASTS_TType region name []

                                                    Var.Qualified home name ->
                                                        T.CASTS_TTypeQual region home name []
                                        )
                            )
                    , -- type variables
                      Var.lower E.CRES_TStart
                        |> P.bind
                            (\var ->
                                P.addEnd start (T.CASTS_TVar var)
                            )
                    , -- tuples
                      P.inContext E.CRES_TTuple (P.word1 '(' E.CRES_TStart) <|
                        P.oneOf E.CRES_TTupleOpen
                            [ P.word1 ')' E.CRES_TTupleOpen
                                |> P.bind (\_ -> P.addEnd start T.CASTS_TUnit)
                            , Space.chompAndCheckIndent E.CRES_TTupleSpace E.CRES_TTupleIndentType1
                                |> P.bind
                                    (\_ ->
                                        P.specialize E.CRES_TTupleType expression
                                            |> P.bind
                                                (\( tipe, end ) ->
                                                    Space.checkIndent end E.CRES_TTupleIndentEnd
                                                        |> P.bind (\_ -> chompTupleEnd start tipe [])
                                                )
                                    )
                            ]
                    , -- records
                      P.inContext E.CRES_TRecord (P.word1 '{' E.CRES_TStart) <|
                        (Space.chompAndCheckIndent E.CRES_TRecordSpace E.CRES_TRecordIndentOpen
                            |> P.bind
                                (\_ ->
                                    P.oneOf E.CRES_TRecordOpen
                                        [ P.word1 '}' E.CRES_TRecordEnd
                                            |> P.bind (\_ -> P.addEnd start (T.CASTS_TRecord [] Nothing))
                                        , P.addLocation (Var.lower E.CRES_TRecordField)
                                            |> P.bind
                                                (\name ->
                                                    Space.chompAndCheckIndent E.CRES_TRecordSpace E.CRES_TRecordIndentColon
                                                        |> P.bind
                                                            (\_ ->
                                                                P.oneOf E.CRES_TRecordColon
                                                                    [ P.word1 '|' E.CRES_TRecordColon
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                Space.chompAndCheckIndent E.CRES_TRecordSpace E.CRES_TRecordIndentField
                                                                                    |> P.bind
                                                                                        (\_ ->
                                                                                            chompField
                                                                                                |> P.bind
                                                                                                    (\field ->
                                                                                                        chompRecordEnd [ field ]
                                                                                                            |> P.bind (\fields -> P.addEnd start (T.CASTS_TRecord fields (Just name)))
                                                                                                    )
                                                                                        )
                                                                            )
                                                                    , P.word1 ':' E.CRES_TRecordColon
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                Space.chompAndCheckIndent E.CRES_TRecordSpace E.CRES_TRecordIndentType
                                                                                    |> P.bind
                                                                                        (\_ ->
                                                                                            P.specialize E.CRES_TRecordType expression
                                                                                                |> P.bind
                                                                                                    (\( tipe, end ) ->
                                                                                                        Space.checkIndent end E.CRES_TRecordIndentEnd
                                                                                                            |> P.bind
                                                                                                                (\_ ->
                                                                                                                    chompRecordEnd [ ( name, tipe ) ]
                                                                                                                        |> P.bind (\fields -> P.addEnd start (T.CASTS_TRecord fields Nothing))
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


expression : Space.Parser E.CRES_Type T.CASTS_Type
expression =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.CRES_TStart
                    [ app start
                    , term
                        |> P.bind
                            (\eterm ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            Space.chomp E.CRES_TSpace
                                                |> P.fmap (\_ -> ( eterm, end ))
                                        )
                            )
                    ]
                    |> P.bind
                        (\(( tipe1, end1 ) as term1) ->
                            P.oneOfWithFallback
                                [ -- should never trigger
                                  Space.checkIndent end1 E.CRES_TIndentStart
                                    |> P.bind
                                        (\_ ->
                                            -- could just be another type instead
                                            P.word2 '-' '>' E.CRES_TStart
                                                |> P.bind
                                                    (\_ ->
                                                        Space.chompAndCheckIndent E.CRES_TSpace E.CRES_TIndentStart
                                                            |> P.bind
                                                                (\_ ->
                                                                    expression
                                                                        |> P.fmap
                                                                            (\( tipe2, end2 ) ->
                                                                                let
                                                                                    tipe : T.CRA_Located T.CASTS_Type_
                                                                                    tipe =
                                                                                        A.at start end2 (T.CASTS_TLambda tipe1 tipe2)
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


app : T.CRA_Position -> Space.Parser E.CRES_Type T.CASTS_Type
app start =
    Var.foreignUpper E.CRES_TStart
        |> P.bind
            (\upper ->
                P.getPosition
                    |> P.bind
                        (\upperEnd ->
                            Space.chomp E.CRES_TSpace
                                |> P.bind
                                    (\_ ->
                                        chompArgs [] upperEnd
                                            |> P.fmap
                                                (\( args, end ) ->
                                                    let
                                                        region : T.CRA_Region
                                                        region =
                                                            T.CRA_Region start upperEnd

                                                        tipe : T.CASTS_Type_
                                                        tipe =
                                                            case upper of
                                                                Var.Unqualified name ->
                                                                    T.CASTS_TType region name args

                                                                Var.Qualified home name ->
                                                                    T.CASTS_TTypeQual region home name args
                                                    in
                                                    ( A.at start end tipe, end )
                                                )
                                    )
                        )
            )


chompArgs : List T.CASTS_Type -> T.CRA_Position -> Space.Parser E.CRES_Type (List T.CASTS_Type)
chompArgs args end =
    P.oneOfWithFallback
        [ Space.checkIndent end E.CRES_TIndentStart
            |> P.bind
                (\_ ->
                    term
                        |> P.bind
                            (\arg ->
                                P.getPosition
                                    |> P.bind
                                        (\newEnd ->
                                            Space.chomp E.CRES_TSpace
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


chompTupleEnd : T.CRA_Position -> T.CASTS_Type -> List T.CASTS_Type -> P.Parser E.CRES_TTuple T.CASTS_Type
chompTupleEnd start firstType revTypes =
    P.oneOf E.CRES_TTupleEnd
        [ P.word1 ',' E.CRES_TTupleEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.CRES_TTupleSpace E.CRES_TTupleIndentTypeN
                        |> P.bind
                            (\_ ->
                                P.specialize E.CRES_TTupleType expression
                                    |> P.bind
                                        (\( tipe, end ) ->
                                            Space.checkIndent end E.CRES_TTupleIndentEnd
                                                |> P.bind
                                                    (\_ ->
                                                        chompTupleEnd start firstType (tipe :: revTypes)
                                                    )
                                        )
                            )
                )
        , P.word1 ')' E.CRES_TTupleEnd
            |> P.bind
                (\_ ->
                    case List.reverse revTypes of
                        [] ->
                            P.pure firstType

                        secondType :: otherTypes ->
                            P.addEnd start (T.CASTS_TTuple firstType secondType otherTypes)
                )
        ]



-- RECORD


type alias Field =
    ( T.CRA_Located T.CDN_Name, T.CASTS_Type )


chompRecordEnd : List Field -> P.Parser E.CRES_TRecord (List Field)
chompRecordEnd fields =
    P.oneOf E.CRES_TRecordEnd
        [ P.word1 ',' E.CRES_TRecordEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.CRES_TRecordSpace E.CRES_TRecordIndentField
                        |> P.bind
                            (\_ ->
                                chompField
                                    |> P.bind
                                        (\field ->
                                            chompRecordEnd (field :: fields)
                                        )
                            )
                )
        , P.word1 '}' E.CRES_TRecordEnd
            |> P.fmap (\_ -> List.reverse fields)
        ]


chompField : P.Parser E.CRES_TRecord Field
chompField =
    P.addLocation (Var.lower E.CRES_TRecordField)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.CRES_TRecordSpace E.CRES_TRecordIndentColon
                    |> P.bind
                        (\_ ->
                            P.word1 ':' E.CRES_TRecordColon
                                |> P.bind
                                    (\_ ->
                                        Space.chompAndCheckIndent E.CRES_TRecordSpace E.CRES_TRecordIndentType
                                            |> P.bind
                                                (\_ ->
                                                    P.specialize E.CRES_TRecordType expression
                                                        |> P.bind
                                                            (\( tipe, end ) ->
                                                                Space.checkIndent end E.CRES_TRecordIndentEnd
                                                                    |> P.fmap (\_ -> ( name, tipe ))
                                                            )
                                                )
                                    )
                        )
            )



-- VARIANT


variant : Space.Parser E.CRES_CustomType ( T.CRA_Located T.CDN_Name, List T.CASTS_Type )
variant =
    P.addLocation (Var.upper E.CRES_CT_Variant)
        |> P.bind
            (\((T.CRA_At (T.CRA_Region _ nameEnd) _) as name) ->
                Space.chomp E.CRES_CT_Space
                    |> P.bind
                        (\_ ->
                            P.specialize E.CRES_CT_VariantArg (chompArgs [] nameEnd)
                                |> P.fmap
                                    (\( args, end ) ->
                                        ( ( name, args ), end )
                                    )
                        )
            )
