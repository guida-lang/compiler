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
                                    (\c94 ->
                                        let
                                            _ =
                                                Debug.log "c94" c94
                                        in
                                        P.specialize E.TTupleType expression
                                            |> P.bind
                                                (\( ( _, tipe ), end ) ->
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
                                    let
                                        _ =
                                            Debug.log "c95" initialComments
                                    in
                                    P.oneOf E.TRecordOpen
                                        [ P.word1 '}' E.TRecordEnd
                                            |> P.bind (\_ -> P.addEnd start (Src.TRecord [] Nothing initialComments))
                                        , P.addLocation (Var.lower E.TRecordField)
                                            |> P.bind
                                                (\name ->
                                                    Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                                                        |> P.bind
                                                            (\postNameComments ->
                                                                let
                                                                    _ =
                                                                        Debug.log "c96" postNameComments
                                                                in
                                                                P.oneOf E.TRecordColon
                                                                    [ P.word1 '|' E.TRecordColon
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
                                                                                    |> P.bind
                                                                                        (\preFieldComments ->
                                                                                            let
                                                                                                _ =
                                                                                                    Debug.log "c97" preFieldComments
                                                                                            in
                                                                                            chompField
                                                                                                |> P.bind
                                                                                                    (\( postFieldComments, field ) ->
                                                                                                        chompRecordEnd postFieldComments [ ( preFieldComments, [], field ) ]
                                                                                                            |> P.bind (\( trailingComments, fields ) -> P.addEnd start (Src.TRecord fields (Just ( initialComments, postNameComments, name )) trailingComments))
                                                                                                    )
                                                                                        )
                                                                            )
                                                                    , P.word1 ':' E.TRecordColon
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                                                                                    |> P.bind
                                                                                        (\preTypeComments ->
                                                                                            let
                                                                                                _ =
                                                                                                    Debug.log "c98" preTypeComments
                                                                                            in
                                                                                            P.specialize E.TRecordType expression
                                                                                                |> P.bind
                                                                                                    (\( ( postExpressionComments, tipe ), end ) ->
                                                                                                        Space.checkIndent end E.TRecordIndentEnd
                                                                                                            |> P.bind
                                                                                                                (\_ ->
                                                                                                                    chompRecordEnd postExpressionComments [ ( [], initialComments, ( ( postNameComments, name ), ( preTypeComments, tipe ) ) ) ]
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


expression : Space.Parser E.Type (Src.C1 Src.Type)
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
                                                |> P.fmap
                                                    (\c122 ->
                                                        let
                                                            _ =
                                                                Debug.log "c122" c122
                                                        in
                                                        ( ( [], eterm ), end )
                                                    )
                                        )
                            )
                    ]
                    |> P.bind
                        (\(( ( comments1, tipe1 ), end1 ) as term1) ->
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
                                                                (\c99 ->
                                                                    let
                                                                        _ =
                                                                            Debug.log "c99" c99
                                                                    in
                                                                    expression
                                                                        |> P.fmap
                                                                            (\( ( comments2, tipe2 ), end2 ) ->
                                                                                let
                                                                                    tipe : A.Located Src.Type_
                                                                                    tipe =
                                                                                        A.at start end2 (Src.TLambda tipe1 tipe2)
                                                                                in
                                                                                ( ( comments2, tipe ), end2 )
                                                                            )
                                                                )
                                                    )
                                        )
                                ]
                                term1
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
                                        let
                                            _ =
                                                Debug.log "c123" postUpperComments
                                        in
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


chompArgs : Src.FComments -> List Src.Type -> A.Position -> Space.Parser E.Type (Src.C1 (List Src.Type))
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
                                                    (\c124 ->
                                                        let
                                                            _ =
                                                                Debug.log "c124" c124
                                                        in
                                                        chompArgs [] (arg :: args) newEnd
                                                    )
                                        )
                            )
                )
        ]
        ( ( preComments, List.reverse args ), end )



-- TUPLES


chompTupleEnd : A.Position -> Src.Type -> List Src.Type -> P.Parser E.TTuple Src.Type
chompTupleEnd start firstType revTypes =
    P.oneOf E.TTupleEnd
        [ P.word1 ',' E.TTupleEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentTypeN
                        |> P.bind
                            (\c100 ->
                                let
                                    _ =
                                        Debug.log "c100" c100
                                in
                                P.specialize E.TTupleType expression
                                    |> P.bind
                                        (\( ( _, tipe ), end ) ->
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
                            P.addEnd start (Src.TTuple firstType secondType otherTypes)
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
                                let
                                    _ =
                                        Debug.log "c101" preNameComments
                                in
                                chompField
                                    |> P.bind
                                        (\( postFieldComments, field ) ->
                                            chompRecordEnd postFieldComments (( comments, preNameComments, field ) :: fields)
                                        )
                            )
                )
        , P.word1 '}' E.TRecordEnd
            |> P.fmap
                (\_ ->
                    ( comments, List.reverse fields )
                )
        ]


chompField : P.Parser E.TRecord (Src.C1 Field)
chompField =
    P.addLocation (Var.lower E.TRecordField)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                    |> P.bind
                        (\postNameComments ->
                            let
                                _ =
                                    Debug.log "c102" postNameComments
                            in
                            P.word1 ':' E.TRecordColon
                                |> P.bind
                                    (\_ ->
                                        Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                                            |> P.bind
                                                (\preTypeComments ->
                                                    let
                                                        _ =
                                                            Debug.log "c103" preTypeComments
                                                    in
                                                    P.specialize E.TRecordType expression
                                                        |> P.bind
                                                            (\( ( postTypeComments, tipe ), end ) ->
                                                                Space.checkIndent end E.TRecordIndentEnd
                                                                    |> P.fmap (\_ -> ( postTypeComments, ( ( postNameComments, name ), ( preTypeComments, tipe ) ) ))
                                                            )
                                                )
                                    )
                        )
            )



-- VARIANT


variant : Space.Parser E.CustomType ( A.Located Name, List Src.Type )
variant =
    P.addLocation (Var.upper E.CT_Variant)
        |> P.bind
            (\((A.At (A.Region _ nameEnd) _) as name) ->
                Space.chomp E.CT_Space
                    |> P.bind
                        (\c125 ->
                            let
                                _ =
                                    Debug.log "c125" c125
                            in
                            P.specialize E.CT_VariantArg (chompArgs [] [] nameEnd)
                                |> P.fmap
                                    (\( ( _, args ), end ) ->
                                        ( ( name, args ), end )
                                    )
                        )
            )
