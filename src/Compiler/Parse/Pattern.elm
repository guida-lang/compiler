module Compiler.Parse.Pattern exposing
    ( expression
    , term
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.String as String
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- TERM


term : SyntaxVersion -> P.Parser E.Pattern Src.Pattern
term syntaxVersion =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.PStart
                    [ record start
                    , tuple syntaxVersion start
                    , list syntaxVersion start
                    , termHelp syntaxVersion start
                    ]
            )


termHelp : SyntaxVersion -> A.Position -> P.Parser E.Pattern Src.Pattern
termHelp syntaxVersion start =
    P.oneOf E.PStart
        [ wildcard syntaxVersion
            |> P.bind (\name -> P.addEnd start (Src.PAnything name))
        , Var.lower E.PStart
            |> P.bind (\name -> P.addEnd start (Src.PVar name))
        , Var.foreignUpper E.PStart
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
                                A.at start end <|
                                    case upper of
                                        Var.Unqualified name ->
                                            Src.PCtor region name []

                                        Var.Qualified home name ->
                                            Src.PCtorQual region home name []
                            )
                )
        , Number.number E.PStart E.PNumber
            |> P.bind
                (\number ->
                    P.getPosition
                        |> P.bind
                            (\end ->
                                case number of
                                    Number.Int int src ->
                                        P.pure (A.at start end (Src.PInt int src))

                                    Number.Float float _ ->
                                        P.Parser <|
                                            \(P.State _ _ _ _ row col) ->
                                                let
                                                    width : Int
                                                    width =
                                                        String.fromFloat float
                                                            |> String.length
                                                in
                                                P.Cerr row (col - width) (E.PFloat width)
                            )
                )
        , String.string E.PStart E.PString
            |> P.bind (\( str, multiline ) -> P.addEnd start (Src.PStr str multiline))
        , String.character E.PStart E.PChar
            |> P.bind (\chr -> P.addEnd start (Src.PChr chr))
        ]



-- WILDCARD


wildcard : SyntaxVersion -> P.Parser E.Pattern Name.Name
wildcard syntaxVersion =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos == end || P.unsafeIndex src pos /= '_' then
                P.Eerr row col E.PStart

            else
                let
                    newPos : Int
                    newPos =
                        pos + 1

                    newCol : P.Col
                    newCol =
                        col + 1
                in
                if Var.getInnerWidth src newPos end > 0 then
                    case syntaxVersion of
                        SV.Elm ->
                            let
                                ( badPos, badCol ) =
                                    Var.chompInnerChars src newPos end newCol
                            in
                            P.Cerr row col (E.PWildcardNotVar (Name.fromPtr src pos badPos) (badCol - col))

                        SV.Guida ->
                            let
                                ( lowerPos, lowerCol ) =
                                    Var.chompLower src newPos end newCol

                                name : String
                                name =
                                    Name.fromPtr src newPos lowerPos
                            in
                            if Var.isReservedWord name then
                                P.Cerr row col (E.PWildcardReservedWord (Name.fromPtr src newPos lowerPos) (lowerCol - col))

                            else
                                let
                                    newState : P.State
                                    newState =
                                        P.State src lowerPos end indent row lowerCol
                                in
                                P.Cok name newState

                else
                    let
                        newState : P.State
                        newState =
                            P.State src newPos end indent row newCol
                    in
                    P.Cok "" newState



-- RECORDS


record : A.Position -> P.Parser E.Pattern Src.Pattern
record start =
    P.inContext E.PRecord (P.word1 '{' E.PStart) <|
        (Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentOpen
            |> P.bind
                (\c84 ->
                    let
                        _ =
                            Debug.log "c84" c84
                    in
                    P.oneOf E.PRecordOpen
                        [ P.addLocation (Var.lower E.PRecordField)
                            |> P.bind
                                (\var ->
                                    Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
                                        |> P.bind
                                            (\c85 ->
                                                let
                                                    _ =
                                                        Debug.log "c85" c85
                                                in
                                                recordHelp start [ var ]
                                            )
                                )
                        , P.word1 '}' E.PRecordEnd
                            |> P.bind (\_ -> P.addEnd start (Src.PRecord []))
                        ]
                )
        )


recordHelp : A.Position -> List (A.Located Name.Name) -> P.Parser E.PRecord Src.Pattern
recordHelp start vars =
    P.oneOf E.PRecordEnd
        [ P.word1 ',' E.PRecordEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentField)
            |> P.bind
                (\c86 ->
                    let
                        _ =
                            Debug.log "c86" c86
                    in
                    P.addLocation (Var.lower E.PRecordField)
                )
            |> P.bind
                (\var ->
                    Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
                        |> P.bind
                            (\c87 ->
                                let
                                    _ =
                                        Debug.log "c87" c87
                                in
                                recordHelp start (var :: vars)
                            )
                )
        , P.word1 '}' E.PRecordEnd
            |> P.bind (\_ -> P.addEnd start (Src.PRecord vars))
        ]



-- TUPLES


tuple : SyntaxVersion -> A.Position -> P.Parser E.Pattern Src.Pattern
tuple syntaxVersion start =
    P.inContext E.PTuple (P.word1 '(' E.PStart) <|
        (Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExpr1
            |> P.bind
                (\c88 ->
                    let
                        _ =
                            Debug.log "c88" c88
                    in
                    P.oneOf E.PTupleOpen
                        [ P.specialize E.PTupleExpr (expression syntaxVersion)
                            |> P.bind
                                (\( ( _, pattern ), end ) ->
                                    Space.checkIndent end E.PTupleIndentEnd
                                        |> P.bind (\_ -> tupleHelp syntaxVersion start pattern [])
                                )
                        , P.word1 ')' E.PTupleEnd
                            |> P.bind (\_ -> P.addEnd start Src.PUnit)
                        ]
                )
        )


tupleHelp : SyntaxVersion -> A.Position -> Src.Pattern -> List Src.Pattern -> P.Parser E.PTuple Src.Pattern
tupleHelp syntaxVersion start firstPattern revPatterns =
    P.oneOf E.PTupleEnd
        [ P.word1 ',' E.PTupleEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExprN)
            |> P.bind
                (\c89 ->
                    let
                        _ =
                            Debug.log "c89" c89
                    in
                    P.specialize E.PTupleExpr (expression syntaxVersion)
                )
            |> P.bind
                (\( ( _, pattern ), end ) ->
                    Space.checkIndent end E.PTupleIndentEnd
                        |> P.bind (\_ -> tupleHelp syntaxVersion start firstPattern (pattern :: revPatterns))
                )
        , P.word1 ')' E.PTupleEnd
            |> P.bind
                (\_ ->
                    case List.reverse revPatterns of
                        [] ->
                            P.pure firstPattern

                        secondPattern :: otherPatterns ->
                            P.addEnd start (Src.PTuple firstPattern secondPattern otherPatterns)
                )
        ]



-- LIST


list : SyntaxVersion -> A.Position -> P.Parser E.Pattern Src.Pattern
list syntaxVersion start =
    P.inContext E.PList (P.word1 '[' E.PStart) <|
        (Space.chompAndCheckIndent E.PListSpace E.PListIndentOpen
            |> P.bind
                (\c90 ->
                    let
                        _ =
                            Debug.log "c90" c90
                    in
                    P.oneOf E.PListOpen
                        [ P.specialize E.PListExpr (expression syntaxVersion)
                            |> P.bind
                                (\( ( _, pattern ), end ) ->
                                    Space.checkIndent end E.PListIndentEnd
                                        |> P.bind (\_ -> listHelp syntaxVersion start [ pattern ])
                                )
                        , P.word1 ']' E.PListEnd
                            |> P.bind (\_ -> P.addEnd start (Src.PList []))
                        ]
                )
        )


listHelp : SyntaxVersion -> A.Position -> List Src.Pattern -> P.Parser E.PList Src.Pattern
listHelp syntaxVersion start patterns =
    P.oneOf E.PListEnd
        [ P.word1 ',' E.PListEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.PListSpace E.PListIndentExpr)
            |> P.bind
                (\c91 ->
                    let
                        _ =
                            Debug.log "c91" c91
                    in
                    P.specialize E.PListExpr (expression syntaxVersion)
                )
            |> P.bind
                (\( ( _, pattern ), end ) ->
                    Space.checkIndent end E.PListIndentEnd
                        |> P.bind (\_ -> listHelp syntaxVersion start (pattern :: patterns))
                )
        , P.word1 ']' E.PListEnd
            |> P.bind (\_ -> P.addEnd start (Src.PList (List.reverse patterns)))
        ]



-- EXPRESSION


expression : SyntaxVersion -> Space.Parser E.Pattern (Src.C1 Src.Pattern)
expression syntaxVersion =
    P.getPosition
        |> P.bind
            (\start ->
                exprPart syntaxVersion
                    |> P.bind
                        (\ePart ->
                            exprHelp syntaxVersion start [] (Debug.log "ePart" ePart)
                        )
            )


exprHelp : SyntaxVersion -> A.Position -> List (Src.C1 Src.Pattern) -> ( Src.C1 Src.Pattern, A.Position ) -> Space.Parser E.Pattern (Src.C1 Src.Pattern)
exprHelp syntaxVersion start revPatterns ( ( patternComments, pattern ), end ) =
    P.oneOfWithFallback
        [ Space.checkIndent end E.PIndentStart
            |> P.bind (\_ -> P.word2 ':' ':' E.PStart)
            |> P.bind (\_ -> Space.chompAndCheckIndent E.PSpace E.PIndentStart)
            |> P.bind
                (\c92 ->
                    let
                        _ =
                            Debug.log "c92" c92
                    in
                    exprPart syntaxVersion
                )
            |> P.bind (\ePart -> exprHelp syntaxVersion start (( patternComments, pattern ) :: revPatterns) ePart)
        , Space.checkIndent end E.PIndentStart
            |> P.bind (\_ -> Keyword.as_ E.PStart)
            |> P.bind (\_ -> Space.chompAndCheckIndent E.PSpace E.PIndentAlias)
            |> P.bind
                (\c93 ->
                    let
                        _ =
                            Debug.log "c93" c93
                    in
                    P.getPosition
                )
            |> P.bind
                (\nameStart ->
                    Var.lower E.PAlias
                        |> P.bind
                            (\name ->
                                P.getPosition
                                    |> P.bind
                                        (\newEnd ->
                                            Space.chomp E.PSpace
                                                |> P.fmap
                                                    (\c119 ->
                                                        let
                                                            _ =
                                                                Debug.log "c119" c119

                                                            alias_ : A.Located Name.Name
                                                            alias_ =
                                                                A.at nameStart newEnd name
                                                        in
                                                        ( ( [], A.at start newEnd (Src.PAlias (List.foldl cons pattern (List.map Src.c1Value revPatterns)) alias_) )
                                                        , newEnd
                                                        )
                                                    )
                                        )
                            )
                )
        ]
        ( ( patternComments, List.foldl cons pattern (List.map Src.c1Value revPatterns) )
        , end
        )


cons : Src.Pattern -> Src.Pattern -> Src.Pattern
cons hd tl =
    A.merge hd tl (Src.PCons hd tl)



-- EXPRESSION PART


exprPart : SyntaxVersion -> Space.Parser E.Pattern (Src.C1 Src.Pattern)
exprPart syntaxVersion =
    P.oneOf E.PStart
        [ P.getPosition
            |> P.bind
                (\start ->
                    Var.foreignUpper E.PStart
                        |> P.bind
                            (\upper ->
                                P.getPosition
                                    |> P.bind (\end -> exprTermHelp syntaxVersion (A.Region start end) upper start [])
                            )
                )
        , term syntaxVersion
            |> P.bind
                (\((A.At (A.Region _ end) _) as eterm) ->
                    Space.chomp E.PSpace
                        |> P.fmap
                            (\comments ->
                                let
                                    _ =
                                        Debug.log "c120" comments
                                in
                                ( ( comments, eterm ), end )
                            )
                )
        ]


exprTermHelp : SyntaxVersion -> A.Region -> Var.Upper -> A.Position -> List Src.Pattern -> Space.Parser E.Pattern (Src.C1 Src.Pattern)
exprTermHelp syntaxVersion region upper start revArgs =
    P.getPosition
        |> P.bind
            (\end ->
                Space.chomp E.PSpace
                    |> P.bind
                        (\comments ->
                            let
                                _ =
                                    Debug.log "c121" comments
                            in
                            P.oneOfWithFallback
                                [ Space.checkIndent end E.PIndentStart
                                    |> P.bind (\_ -> term syntaxVersion)
                                    |> P.bind (\arg -> exprTermHelp syntaxVersion region upper start (arg :: revArgs))
                                ]
                                ( ( comments
                                  , A.at start end <|
                                        case upper of
                                            Var.Unqualified name ->
                                                Src.PCtor region name (List.reverse revArgs)

                                            Var.Qualified home name ->
                                                Src.PCtorQual region home name (List.reverse revArgs)
                                  )
                                , end
                                )
                        )
            )
