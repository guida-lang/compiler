module Compiler.Parse.Pattern exposing
    ( expression
    , term
    )

import Compiler.Data.Name as Name
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.String as String
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Types as T



-- TERM


term : P.Parser T.CRES_Pattern T.CASTS_Pattern
term =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf T.CRES_PStart
                    [ record start
                    , tuple start
                    , list start
                    , termHelp start
                    ]
            )


termHelp : T.CRA_Position -> P.Parser T.CRES_Pattern T.CASTS_Pattern
termHelp start =
    P.oneOf T.CRES_PStart
        [ wildcard
            |> P.bind (\_ -> P.addEnd start T.CASTS_PAnything)
        , Var.lower T.CRES_PStart
            |> P.bind (\name -> P.addEnd start (T.CASTS_PVar name))
        , Var.foreignUpper T.CRES_PStart
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
                                A.at start end <|
                                    case upper of
                                        Var.Unqualified name ->
                                            T.CASTS_PCtor region name []

                                        Var.Qualified home name ->
                                            T.CASTS_PCtorQual region home name []
                            )
                )
        , Number.number T.CRES_PStart T.CRES_PNumber
            |> P.bind
                (\number ->
                    P.getPosition
                        |> P.bind
                            (\end ->
                                case number of
                                    Number.Int int ->
                                        P.pure (A.at start end (T.CASTS_PInt int))

                                    Number.Float float ->
                                        P.Parser <|
                                            \(P.State _ _ _ _ row col) ->
                                                let
                                                    width : Int
                                                    width =
                                                        String.fromFloat float
                                                            |> String.length
                                                in
                                                Err (P.PErr P.Consumed row (col - width) (T.CRES_PFloat width))
                            )
                )
        , String.string T.CRES_PStart T.CRES_PString
            |> P.bind (\str -> P.addEnd start (T.CASTS_PStr str))
        , String.character T.CRES_PStart T.CRES_PChar
            |> P.bind (\chr -> P.addEnd start (T.CASTS_PChr chr))
        ]



-- WILDCARD


wildcard : P.Parser T.CRES_Pattern ()
wildcard =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos == end || P.unsafeIndex src pos /= '_' then
                Err (P.PErr P.Empty row col T.CRES_PStart)

            else
                let
                    newPos : Int
                    newPos =
                        pos + 1

                    newCol : T.CPP_Col
                    newCol =
                        col + 1
                in
                if Var.getInnerWidth src newPos end > 0 then
                    let
                        ( badPos, badCol ) =
                            Var.chompInnerChars src newPos end newCol
                    in
                    Err (P.PErr P.Consumed row col (T.CRES_PWildcardNotVar (Name.fromPtr src pos badPos) (badCol - col)))

                else
                    let
                        newState : P.State
                        newState =
                            P.State src newPos end indent row newCol
                    in
                    Ok (P.POk P.Consumed () newState)



-- RECORDS


record : T.CRA_Position -> P.Parser T.CRES_Pattern T.CASTS_Pattern
record start =
    P.inContext T.CRES_PRecord (P.word1 '{' T.CRES_PStart) <|
        (Space.chompAndCheckIndent T.CRES_PRecordSpace T.CRES_PRecordIndentOpen
            |> P.bind
                (\_ ->
                    P.oneOf T.CRES_PRecordOpen
                        [ P.addLocation (Var.lower T.CRES_PRecordField)
                            |> P.bind
                                (\var ->
                                    Space.chompAndCheckIndent T.CRES_PRecordSpace T.CRES_PRecordIndentEnd
                                        |> P.bind (\_ -> recordHelp start [ var ])
                                )
                        , P.word1 '}' T.CRES_PRecordEnd
                            |> P.bind (\_ -> P.addEnd start (T.CASTS_PRecord []))
                        ]
                )
        )


recordHelp : T.CRA_Position -> List (T.CRA_Located T.CDN_Name) -> P.Parser T.CRES_PRecord T.CASTS_Pattern
recordHelp start vars =
    P.oneOf T.CRES_PRecordEnd
        [ P.word1 ',' T.CRES_PRecordEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_PRecordSpace T.CRES_PRecordIndentField)
            |> P.bind (\_ -> P.addLocation (Var.lower T.CRES_PRecordField))
            |> P.bind
                (\var ->
                    Space.chompAndCheckIndent T.CRES_PRecordSpace T.CRES_PRecordIndentEnd
                        |> P.bind (\_ -> recordHelp start (var :: vars))
                )
        , P.word1 '}' T.CRES_PRecordEnd
            |> P.bind (\_ -> P.addEnd start (T.CASTS_PRecord vars))
        ]



-- TUPLES


tuple : T.CRA_Position -> P.Parser T.CRES_Pattern T.CASTS_Pattern
tuple start =
    P.inContext T.CRES_PTuple (P.word1 '(' T.CRES_PStart) <|
        (Space.chompAndCheckIndent T.CRES_PTupleSpace T.CRES_PTupleIndentExpr1
            |> P.bind
                (\_ ->
                    P.oneOf T.CRES_PTupleOpen
                        [ P.specialize T.CRES_PTupleExpr expression
                            |> P.bind
                                (\( pattern, end ) ->
                                    Space.checkIndent end T.CRES_PTupleIndentEnd
                                        |> P.bind (\_ -> tupleHelp start pattern [])
                                )
                        , P.word1 ')' T.CRES_PTupleEnd
                            |> P.bind (\_ -> P.addEnd start T.CASTS_PUnit)
                        ]
                )
        )


tupleHelp : T.CRA_Position -> T.CASTS_Pattern -> List T.CASTS_Pattern -> P.Parser T.CRES_PTuple T.CASTS_Pattern
tupleHelp start firstPattern revPatterns =
    P.oneOf T.CRES_PTupleEnd
        [ P.word1 ',' T.CRES_PTupleEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_PTupleSpace T.CRES_PTupleIndentExprN)
            |> P.bind (\_ -> P.specialize T.CRES_PTupleExpr expression)
            |> P.bind
                (\( pattern, end ) ->
                    Space.checkIndent end T.CRES_PTupleIndentEnd
                        |> P.bind (\_ -> tupleHelp start firstPattern (pattern :: revPatterns))
                )
        , P.word1 ')' T.CRES_PTupleEnd
            |> P.bind
                (\_ ->
                    case List.reverse revPatterns of
                        [] ->
                            P.pure firstPattern

                        secondPattern :: otherPatterns ->
                            P.addEnd start (T.CASTS_PTuple firstPattern secondPattern otherPatterns)
                )
        ]



-- LIST


list : T.CRA_Position -> P.Parser T.CRES_Pattern T.CASTS_Pattern
list start =
    P.inContext T.CRES_PList (P.word1 '[' T.CRES_PStart) <|
        (Space.chompAndCheckIndent T.CRES_PListSpace T.CRES_PListIndentOpen
            |> P.bind
                (\_ ->
                    P.oneOf T.CRES_PListOpen
                        [ P.specialize T.CRES_PListExpr expression
                            |> P.bind
                                (\( pattern, end ) ->
                                    Space.checkIndent end T.CRES_PListIndentEnd
                                        |> P.bind (\_ -> listHelp start [ pattern ])
                                )
                        , P.word1 ']' T.CRES_PListEnd
                            |> P.bind (\_ -> P.addEnd start (T.CASTS_PList []))
                        ]
                )
        )


listHelp : T.CRA_Position -> List T.CASTS_Pattern -> P.Parser T.CRES_PList T.CASTS_Pattern
listHelp start patterns =
    P.oneOf T.CRES_PListEnd
        [ P.word1 ',' T.CRES_PListEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_PListSpace T.CRES_PListIndentExpr)
            |> P.bind (\_ -> P.specialize T.CRES_PListExpr expression)
            |> P.bind
                (\( pattern, end ) ->
                    Space.checkIndent end T.CRES_PListIndentEnd
                        |> P.bind (\_ -> listHelp start (pattern :: patterns))
                )
        , P.word1 ']' T.CRES_PListEnd
            |> P.bind (\_ -> P.addEnd start (T.CASTS_PList (List.reverse patterns)))
        ]



-- EXPRESSION


expression : Space.Parser T.CRES_Pattern T.CASTS_Pattern
expression =
    P.getPosition
        |> P.bind
            (\start ->
                exprPart
                    |> P.bind
                        (\ePart ->
                            exprHelp start [] ePart
                        )
            )


exprHelp : T.CRA_Position -> List T.CASTS_Pattern -> ( T.CASTS_Pattern, T.CRA_Position ) -> Space.Parser T.CRES_Pattern T.CASTS_Pattern
exprHelp start revPatterns ( pattern, end ) =
    P.oneOfWithFallback
        [ Space.checkIndent end T.CRES_PIndentStart
            |> P.bind (\_ -> P.word2 ':' ':' T.CRES_PStart)
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_PSpace T.CRES_PIndentStart)
            |> P.bind (\_ -> exprPart)
            |> P.bind (\ePart -> exprHelp start (pattern :: revPatterns) ePart)
        , Space.checkIndent end T.CRES_PIndentStart
            |> P.bind (\_ -> Keyword.as_ T.CRES_PStart)
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_PSpace T.CRES_PIndentAlias)
            |> P.bind (\_ -> P.getPosition)
            |> P.bind
                (\nameStart ->
                    Var.lower T.CRES_PAlias
                        |> P.bind
                            (\name ->
                                P.getPosition
                                    |> P.bind
                                        (\newEnd ->
                                            Space.chomp T.CRES_PSpace
                                                |> P.fmap
                                                    (\_ ->
                                                        let
                                                            alias_ : T.CRA_Located T.CDN_Name
                                                            alias_ =
                                                                A.at nameStart newEnd name
                                                        in
                                                        ( A.at start newEnd (T.CASTS_PAlias (List.foldl cons pattern revPatterns) alias_)
                                                        , newEnd
                                                        )
                                                    )
                                        )
                            )
                )
        ]
        ( List.foldl cons pattern revPatterns
        , end
        )


cons : T.CASTS_Pattern -> T.CASTS_Pattern -> T.CASTS_Pattern
cons hd tl =
    A.merge hd tl (T.CASTS_PCons hd tl)



-- EXPRESSION PART


exprPart : Space.Parser T.CRES_Pattern T.CASTS_Pattern
exprPart =
    P.oneOf T.CRES_PStart
        [ P.getPosition
            |> P.bind
                (\start ->
                    Var.foreignUpper T.CRES_PStart
                        |> P.bind
                            (\upper ->
                                P.getPosition
                                    |> P.bind (\end -> exprTermHelp (T.CRA_Region start end) upper start [])
                            )
                )
        , term
            |> P.bind
                (\((T.CRA_At (T.CRA_Region _ end) _) as eterm) ->
                    Space.chomp T.CRES_PSpace
                        |> P.fmap (\_ -> ( eterm, end ))
                )
        ]


exprTermHelp : T.CRA_Region -> Var.Upper -> T.CRA_Position -> List T.CASTS_Pattern -> Space.Parser T.CRES_Pattern T.CASTS_Pattern
exprTermHelp region upper start revArgs =
    P.getPosition
        |> P.bind
            (\end ->
                Space.chomp T.CRES_PSpace
                    |> P.bind
                        (\_ ->
                            P.oneOfWithFallback
                                [ Space.checkIndent end T.CRES_PIndentStart
                                    |> P.bind (\_ -> term)
                                    |> P.bind (\arg -> exprTermHelp region upper start (arg :: revArgs))
                                ]
                                ( A.at start end <|
                                    case upper of
                                        Var.Unqualified name ->
                                            T.CASTS_PCtor region name (List.reverse revArgs)

                                        Var.Qualified home name ->
                                            T.CASTS_PCtorQual region home name (List.reverse revArgs)
                                , end
                                )
                        )
            )
