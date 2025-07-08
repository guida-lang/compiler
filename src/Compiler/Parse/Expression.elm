module Compiler.Parse.Expression exposing
    ( expression
    , record
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Parse.Shader as Shader
import Compiler.Parse.Space as Space
import Compiler.Parse.String as String
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Parse.Type as Type
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- TERMS


term : SyntaxVersion -> P.Parser E.Expr Src.Expr
term syntaxVersion =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.Start
                    [ variable start |> P.bind (accessible start)
                    , string start
                    , number start
                    , Shader.shader start
                    , list syntaxVersion start
                    , record syntaxVersion start |> P.bind (accessible start)
                    , tuple syntaxVersion start |> P.bind (accessible start)
                    , accessor start
                    , character start
                    ]
            )


string : A.Position -> P.Parser E.Expr Src.Expr
string start =
    String.string E.Start E.String_
        |> P.bind (\str -> P.addEnd start (Src.Str str))


character : A.Position -> P.Parser E.Expr Src.Expr
character start =
    String.character E.Start E.Char
        |> P.bind (\chr -> P.addEnd start (Src.Chr chr))


number : A.Position -> P.Parser E.Expr Src.Expr
number start =
    Number.number E.Start E.Number
        |> P.bind
            (\nmbr ->
                P.addEnd start <|
                    case nmbr of
                        Number.Int int ->
                            Src.Int int

                        Number.Float float ->
                            Src.Float float
            )


accessor : A.Position -> P.Parser E.Expr Src.Expr
accessor start =
    P.word1 '.' E.Dot
        |> P.bind (\_ -> Var.lower E.Access)
        |> P.bind (\field -> P.addEnd start (Src.Accessor field))


variable : A.Position -> P.Parser E.Expr Src.Expr
variable start =
    Var.foreignAlpha E.Start
        |> P.bind (\var -> P.addEnd start var)


accessible : A.Position -> Src.Expr -> P.Parser E.Expr Src.Expr
accessible start expr =
    P.oneOfWithFallback
        [ P.word1 '.' E.Dot
            |> P.bind (\_ -> P.getPosition)
            |> P.bind
                (\pos ->
                    Var.lower E.Access
                        |> P.bind
                            (\field ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            accessible start <|
                                                A.at start end (Src.Access expr (A.at pos end field))
                                        )
                            )
                )
        ]
        expr



-- LISTS


list : SyntaxVersion -> A.Position -> P.Parser E.Expr Src.Expr
list syntaxVersion start =
    P.inContext E.List (P.word1 '[' E.Start) <|
        (Space.chompAndCheckIndent E.ListSpace E.ListIndentOpen
            |> P.bind
                (\c23 ->
                    let
                        _ =
                            Debug.log "c23" c23
                    in
                    P.oneOf E.ListOpen
                        [ P.specialize E.ListExpr (expression syntaxVersion)
                            |> P.bind
                                (\( entry, end ) ->
                                    Space.checkIndent end E.ListIndentEnd
                                        |> P.bind (\_ -> P.loop (chompListEnd syntaxVersion start) [ entry ])
                                )
                        , P.word1 ']' E.ListOpen
                            |> P.bind (\_ -> P.getPosition)
                            |> P.bind (\end -> P.addEnd start (Src.List (A.At (A.Region start end) [])))
                        ]
                )
        )


chompListEnd : SyntaxVersion -> A.Position -> List Src.Expr -> P.Parser E.List_ (P.Step (List Src.Expr) Src.Expr)
chompListEnd syntaxVersion start entries =
    P.oneOf E.ListEnd
        [ P.word1 ',' E.ListEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.ListSpace E.ListIndentExpr)
            |> P.bind
                (\c24 ->
                    let
                        _ =
                            Debug.log "c24" c24
                    in
                    P.specialize E.ListExpr (expression syntaxVersion)
                )
            |> P.bind
                (\( entry, end ) ->
                    Space.checkIndent end E.ListIndentEnd
                        |> P.fmap (\_ -> P.Loop (entry :: entries))
                )
        , P.word1 ']' E.ListEnd
            |> P.bind (\_ -> P.getPosition)
            |> P.bind (\end -> P.addEnd start (Src.List (A.At (A.Region start end) (List.reverse entries))))
            |> P.fmap P.Done
        ]



-- TUPLES


tuple : SyntaxVersion -> A.Position -> P.Parser E.Expr Src.Expr
tuple syntaxVersion ((A.Position row col) as start) =
    P.inContext E.Tuple (P.word1 '(' E.Start) <|
        (P.getPosition
            |> P.bind
                (\before ->
                    Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExpr1
                        |> P.bind
                            (\c25 ->
                                let
                                    _ =
                                        Debug.log "c25" c25
                                in
                                P.getPosition
                                    |> P.bind
                                        (\after ->
                                            if before /= after then
                                                P.specialize E.TupleExpr (expression syntaxVersion)
                                                    |> P.bind
                                                        (\( entry, end ) ->
                                                            Space.checkIndent end E.TupleIndentEnd
                                                                |> P.bind (\_ -> chompTupleEnd syntaxVersion start entry [])
                                                        )

                                            else
                                                P.oneOf E.TupleIndentExpr1
                                                    [ Symbol.operator E.TupleIndentExpr1 E.TupleOperatorReserved
                                                        |> P.bind
                                                            (\op ->
                                                                if op == "-" then
                                                                    P.oneOf E.TupleOperatorClose
                                                                        [ P.word1 ')' E.TupleOperatorClose
                                                                            |> P.bind (\_ -> P.addEnd start (Src.Op op))
                                                                        , P.specialize E.TupleExpr
                                                                            (term syntaxVersion
                                                                                |> P.bind
                                                                                    (\((A.At (A.Region _ end) _) as negatedExpr) ->
                                                                                        Space.chomp E.Space
                                                                                            |> P.bind
                                                                                                (\c110 ->
                                                                                                    let
                                                                                                        _ =
                                                                                                            Debug.log "c110" c110

                                                                                                        exprStart : A.Position
                                                                                                        exprStart =
                                                                                                            A.Position row (col + 2)

                                                                                                        expr : A.Located Src.Expr_
                                                                                                        expr =
                                                                                                            A.at exprStart end (Src.Negate negatedExpr)
                                                                                                    in
                                                                                                    chompExprEnd syntaxVersion
                                                                                                        exprStart
                                                                                                        (State
                                                                                                            { ops = []
                                                                                                            , expr = expr
                                                                                                            , args = []
                                                                                                            , end = end
                                                                                                            }
                                                                                                        )
                                                                                                )
                                                                                    )
                                                                            )
                                                                            |> P.bind
                                                                                (\( entry, end ) ->
                                                                                    Space.checkIndent end E.TupleIndentEnd
                                                                                        |> P.bind (\_ -> chompTupleEnd syntaxVersion start entry [])
                                                                                )
                                                                        ]

                                                                else
                                                                    P.word1 ')' E.TupleOperatorClose
                                                                        |> P.bind (\_ -> P.addEnd start (Src.Op op))
                                                            )
                                                    , P.word1 ')' E.TupleIndentExpr1
                                                        |> P.bind (\_ -> P.addEnd start Src.Unit)
                                                    , P.specialize E.TupleExpr (expression syntaxVersion)
                                                        |> P.bind
                                                            (\( entry, end ) ->
                                                                Space.checkIndent end E.TupleIndentEnd
                                                                    |> P.bind (\_ -> chompTupleEnd syntaxVersion start entry [])
                                                            )
                                                    ]
                                        )
                            )
                )
        )


chompTupleEnd : SyntaxVersion -> A.Position -> Src.Expr -> List Src.Expr -> P.Parser E.Tuple Src.Expr
chompTupleEnd syntaxVersion start firstExpr revExprs =
    P.oneOf E.TupleEnd
        [ P.word1 ',' E.TupleEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExprN
                        |> P.bind
                            (\c26 ->
                                let
                                    _ =
                                        Debug.log "c26" c26
                                in
                                P.specialize E.TupleExpr (expression syntaxVersion)
                                    |> P.bind
                                        (\( entry, end ) ->
                                            Space.checkIndent end E.TupleIndentEnd
                                                |> P.bind (\_ -> chompTupleEnd syntaxVersion start firstExpr (entry :: revExprs))
                                        )
                            )
                )
        , P.word1 ')' E.TupleEnd
            |> P.bind
                (\_ ->
                    case List.reverse revExprs of
                        [] ->
                            P.pure firstExpr

                        secondExpr :: otherExprs ->
                            P.addEnd start (Src.Tuple firstExpr secondExpr otherExprs)
                )
        ]



-- RECORDS


record : SyntaxVersion -> A.Position -> P.Parser E.Expr Src.Expr
record syntaxVersion start =
    case syntaxVersion of
        SV.Elm ->
            P.inContext E.Record (P.word1 '{' E.Start) <|
                (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen
                    |> P.bind
                        (\c27 ->
                            let
                                _ =
                                    Debug.log "c27" c27
                            in
                            P.oneOf E.RecordOpen
                                [ P.word1 '}' E.RecordOpen
                                    |> P.bind (\_ -> P.addEnd start (Src.Record []))
                                , P.addLocation (Var.lower E.RecordField)
                                    |> P.bind
                                        (\((A.At starterPosition starterName) as starter) ->
                                            Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                                                |> P.bind
                                                    (\c28 ->
                                                        let
                                                            _ =
                                                                Debug.log "c28" c28
                                                        in
                                                        P.oneOf E.RecordEquals
                                                            [ P.word1 '|' E.RecordEquals
                                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
                                                                |> P.bind
                                                                    (\c29 ->
                                                                        let
                                                                            _ =
                                                                                Debug.log "c29" c29
                                                                        in
                                                                        chompField syntaxVersion
                                                                    )
                                                                |> P.bind (\firstField -> chompFields syntaxVersion [ firstField ])
                                                                |> P.bind (\fields -> P.addEnd start (Src.Update (A.At starterPosition (Src.Var Src.LowVar starterName)) fields))
                                                            , P.word1 '=' E.RecordEquals
                                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                                                                |> P.bind
                                                                    (\c30 ->
                                                                        let
                                                                            _ =
                                                                                Debug.log "c30" c30
                                                                        in
                                                                        P.specialize E.RecordExpr (expression syntaxVersion)
                                                                    )
                                                                |> P.bind
                                                                    (\( value, end ) ->
                                                                        Space.checkIndent end E.RecordIndentEnd
                                                                            |> P.bind (\_ -> chompFields syntaxVersion [ ( starter, value ) ])
                                                                            |> P.bind (\fields -> P.addEnd start (Src.Record fields))
                                                                    )
                                                            ]
                                                    )
                                        )
                                ]
                        )
                )

        SV.Guida ->
            P.inContext E.Record (P.word1 '{' E.Start) <|
                (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen
                    |> P.bind
                        (\c31 ->
                            let
                                _ =
                                    Debug.log "c31" c31
                            in
                            P.oneOf E.RecordOpen
                                [ P.word1 '}' E.RecordOpen
                                    |> P.bind (\_ -> P.addEnd start (Src.Record []))
                                , P.getPosition
                                    |> P.bind
                                        (\nameStart ->
                                            foreignAlpha E.RecordField
                                                |> P.bind (\var -> P.addEnd nameStart var)
                                                |> P.bind (accessibleRecord nameStart)
                                                |> P.bind
                                                    (\starter ->
                                                        Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                                                            |> P.bind
                                                                (\c32 ->
                                                                    let
                                                                        _ =
                                                                            Debug.log "c32" c32
                                                                    in
                                                                    P.word1 '|' E.RecordEquals
                                                                )
                                                            |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
                                                            |> P.bind
                                                                (\c33 ->
                                                                    let
                                                                        _ =
                                                                            Debug.log "c33" c33
                                                                    in
                                                                    chompField syntaxVersion
                                                                )
                                                            |> P.bind (\firstField -> chompFields syntaxVersion [ firstField ])
                                                            |> P.bind (\fields -> P.addEnd start (Src.Update starter fields))
                                                    )
                                        )
                                , P.addLocation (Var.lower E.RecordField)
                                    |> P.bind
                                        (\starter ->
                                            Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                                                |> P.bind
                                                    (\c34 ->
                                                        let
                                                            _ =
                                                                Debug.log "c34" c34
                                                        in
                                                        P.word1 '=' E.RecordEquals
                                                    )
                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                                                |> P.bind
                                                    (\c35 ->
                                                        let
                                                            _ =
                                                                Debug.log "c35" c35
                                                        in
                                                        P.specialize E.RecordExpr (expression syntaxVersion)
                                                    )
                                                |> P.bind
                                                    (\( value, end ) ->
                                                        Space.checkIndent end E.RecordIndentEnd
                                                            |> P.bind (\_ -> chompFields syntaxVersion [ ( starter, value ) ])
                                                            |> P.bind (\fields -> P.addEnd start (Src.Record fields))
                                                    )
                                        )
                                ]
                        )
                )


accessibleRecord : A.Position -> Src.Expr -> P.Parser E.Record Src.Expr
accessibleRecord start expr =
    P.oneOfWithFallback
        [ P.word1 '.' E.RecordOpen
            |> P.bind (\_ -> P.getPosition)
            |> P.bind
                (\pos ->
                    Var.lower E.RecordOpen
                        |> P.bind
                            (\field ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            accessibleRecord start <|
                                                A.at start end (Src.Access expr (A.at pos end field))
                                        )
                            )
                )
        ]
        expr



-- FOREIGN ALPHA


foreignAlpha : (Row -> Col -> x) -> P.Parser x Src.Expr_
foreignAlpha toError =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                ( ( alphaStart, alphaEnd ), ( newCol, varType ) ) =
                    foreignAlphaHelp src pos end col
            in
            if alphaStart == alphaEnd then
                P.Eerr row newCol toError

            else
                case varType of
                    Src.LowVar ->
                        let
                            name : Name
                            name =
                                Name.fromPtr src alphaStart alphaEnd

                            newState : P.State
                            newState =
                                P.State src alphaEnd end indent row newCol
                        in
                        if alphaStart == pos then
                            if Var.isReservedWord name then
                                P.Eerr row col toError

                            else
                                P.Cok (Src.Var varType name) newState

                        else
                            let
                                home : Name
                                home =
                                    Name.fromPtr src pos (alphaStart + -1)
                            in
                            P.Cok (Src.VarQual varType home name) newState

                    Src.CapVar ->
                        P.Eerr row col toError


foreignAlphaHelp : String -> Int -> Int -> Col -> ( ( Int, Int ), ( Col, Src.VarType ) )
foreignAlphaHelp src pos end col =
    let
        ( lowerPos, lowerCol ) =
            Var.chompLower src pos end col
    in
    if pos < lowerPos then
        ( ( pos, lowerPos ), ( lowerCol, Src.LowVar ) )

    else
        let
            ( upperPos, upperCol ) =
                Var.chompUpper src pos end col
        in
        if pos == upperPos then
            ( ( pos, pos ), ( col, Src.CapVar ) )

        else if Var.isDot src upperPos end then
            foreignAlphaHelp src (upperPos + 1) end (upperCol + 1)

        else
            ( ( pos, upperPos ), ( upperCol, Src.CapVar ) )


type alias Field =
    ( A.Located Name.Name, Src.Expr )


chompFields : SyntaxVersion -> List Field -> P.Parser E.Record (List Field)
chompFields syntaxVersion fields =
    P.oneOf E.RecordEnd
        [ P.word1 ',' E.RecordEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
            |> P.bind
                (\c36 ->
                    let
                        _ =
                            Debug.log "c36" c36
                    in
                    chompField syntaxVersion
                )
            |> P.bind (\f -> chompFields syntaxVersion (f :: fields))
        , P.word1 '}' E.RecordEnd
            |> P.fmap (\_ -> List.reverse fields)
        ]


chompField : SyntaxVersion -> P.Parser E.Record Field
chompField syntaxVersion =
    P.addLocation (Var.lower E.RecordField)
        |> P.bind
            (\key ->
                Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                    |> P.bind
                        (\c37 ->
                            let
                                _ =
                                    Debug.log "c37" c37
                            in
                            P.word1 '=' E.RecordEquals
                        )
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                    |> P.bind
                        (\c38 ->
                            let
                                _ =
                                    Debug.log "c38" c38
                            in
                            P.specialize E.RecordExpr (expression syntaxVersion)
                        )
                    |> P.bind
                        (\( value, end ) ->
                            Space.checkIndent end E.RecordIndentEnd
                                |> P.fmap (\_ -> ( key, value ))
                        )
            )



-- EXPRESSIONS


expression : SyntaxVersion -> Space.Parser E.Expr Src.Expr
expression syntaxVersion =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.Start
                    [ let_ syntaxVersion start
                    , if_ syntaxVersion start
                    , case_ syntaxVersion start
                    , function syntaxVersion start
                    , possiblyNegativeTerm syntaxVersion start
                        |> P.bind
                            (\expr ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            Space.chomp E.Space
                                                |> P.bind
                                                    (\c111 ->
                                                        let
                                                            _ =
                                                                Debug.log "c111" c111
                                                        in
                                                        chompExprEnd syntaxVersion
                                                            start
                                                            (State
                                                                { ops = []
                                                                , expr = expr
                                                                , args = []
                                                                , end = end
                                                                }
                                                            )
                                                    )
                                        )
                            )
                    ]
            )


type State
    = State
        { ops : List ( Src.Expr, A.Located Name.Name )
        , expr : Src.Expr
        , args : List Src.Expr
        , end : A.Position
        }


chompExprEnd : SyntaxVersion -> A.Position -> State -> Space.Parser E.Expr Src.Expr
chompExprEnd syntaxVersion start (State { ops, expr, args, end }) =
    P.oneOfWithFallback
        [ -- argument
          Space.checkIndent end E.Start
            |> P.bind (\_ -> term syntaxVersion)
            |> P.bind
                (\arg ->
                    P.getPosition
                        |> P.bind
                            (\newEnd ->
                                Space.chomp E.Space
                                    |> P.bind
                                        (\c112 ->
                                            let
                                                _ =
                                                    Debug.log "c112" c112
                                            in
                                            chompExprEnd syntaxVersion
                                                start
                                                (State
                                                    { ops = ops
                                                    , expr = expr
                                                    , args = arg :: args
                                                    , end = newEnd
                                                    }
                                                )
                                        )
                            )
                )
        , -- operator
          Space.checkIndent end E.Start
            |> P.bind (\_ -> P.addLocation (Symbol.operator E.Start E.OperatorReserved))
            |> P.bind
                (\((A.At (A.Region opStart opEnd) opName) as op) ->
                    Space.chompAndCheckIndent E.Space (E.IndentOperatorRight opName)
                        |> P.bind
                            (\c39 ->
                                let
                                    _ =
                                        Debug.log "c39" c39
                                in
                                P.getPosition
                            )
                        |> P.bind
                            (\newStart ->
                                if "-" == opName && end /= opStart && opEnd == newStart then
                                    -- negative terms
                                    term syntaxVersion
                                        |> P.bind
                                            (\negatedExpr ->
                                                P.getPosition
                                                    |> P.bind
                                                        (\newEnd ->
                                                            Space.chomp E.Space
                                                                |> P.bind
                                                                    (\c113 ->
                                                                        let
                                                                            _ =
                                                                                Debug.log "c113" c113

                                                                            arg : A.Located Src.Expr_
                                                                            arg =
                                                                                A.at opStart newEnd (Src.Negate negatedExpr)
                                                                        in
                                                                        chompExprEnd syntaxVersion
                                                                            start
                                                                            (State
                                                                                { ops = ops
                                                                                , expr = expr
                                                                                , args = arg :: args
                                                                                , end = newEnd
                                                                                }
                                                                            )
                                                                    )
                                                        )
                                            )

                                else
                                    let
                                        err : P.Row -> P.Col -> E.Expr
                                        err =
                                            E.OperatorRight opName
                                    in
                                    P.oneOf err
                                        [ -- term
                                          possiblyNegativeTerm syntaxVersion newStart
                                            |> P.bind
                                                (\newExpr ->
                                                    P.getPosition
                                                        |> P.bind
                                                            (\newEnd ->
                                                                Space.chomp E.Space
                                                                    |> P.bind
                                                                        (\c114 ->
                                                                            let
                                                                                _ =
                                                                                    Debug.log "c114" c114

                                                                                newOps : List ( Src.Expr, A.Located Name.Name )
                                                                                newOps =
                                                                                    ( toCall expr args, op ) :: ops
                                                                            in
                                                                            chompExprEnd syntaxVersion
                                                                                start
                                                                                (State
                                                                                    { ops = newOps
                                                                                    , expr = newExpr
                                                                                    , args = []
                                                                                    , end = newEnd
                                                                                    }
                                                                                )
                                                                        )
                                                            )
                                                )
                                        , -- final term
                                          P.oneOf err
                                            [ let_ syntaxVersion newStart
                                            , case_ syntaxVersion newStart
                                            , if_ syntaxVersion newStart
                                            , function syntaxVersion newStart
                                            ]
                                            |> P.fmap
                                                (\( newLast, newEnd ) ->
                                                    let
                                                        newOps : List ( Src.Expr, A.Located Name.Name )
                                                        newOps =
                                                            ( toCall expr args, op ) :: ops

                                                        finalExpr : Src.Expr_
                                                        finalExpr =
                                                            Src.Binops (List.reverse newOps) newLast
                                                    in
                                                    ( A.at start newEnd finalExpr, newEnd )
                                                )
                                        ]
                            )
                )
        ]
        -- done
        (case ops of
            [] ->
                ( toCall expr args
                , end
                )

            _ ->
                ( A.at start end (Src.Binops (List.reverse ops) (toCall expr args))
                , end
                )
        )


possiblyNegativeTerm : SyntaxVersion -> A.Position -> P.Parser E.Expr Src.Expr
possiblyNegativeTerm syntaxVersion start =
    P.oneOf E.Start
        [ P.word1 '-' E.Start
            |> P.bind
                (\_ ->
                    term syntaxVersion
                        |> P.bind
                            (\expr ->
                                P.addEnd start (Src.Negate expr)
                            )
                )
        , term syntaxVersion
        ]


toCall : Src.Expr -> List Src.Expr -> Src.Expr
toCall func revArgs =
    case revArgs of
        [] ->
            func

        lastArg :: _ ->
            A.merge func lastArg (Src.Call func (List.reverse revArgs))



-- IF EXPRESSION


if_ : SyntaxVersion -> A.Position -> Space.Parser E.Expr Src.Expr
if_ syntaxVersion start =
    P.inContext E.If (Keyword.if_ E.Start) <|
        chompIfEnd syntaxVersion start []


chompIfEnd : SyntaxVersion -> A.Position -> List ( Src.Expr, Src.Expr ) -> Space.Parser E.If Src.Expr
chompIfEnd syntaxVersion start branches =
    Space.chompAndCheckIndent E.IfSpace E.IfIndentCondition
        |> P.bind
            (\c40 ->
                let
                    _ =
                        Debug.log "c40" c40
                in
                P.specialize E.IfCondition (expression syntaxVersion)
            )
        |> P.bind
            (\( condition, condEnd ) ->
                Space.checkIndent condEnd E.IfIndentThen
                    |> P.bind (\_ -> Keyword.then_ E.IfThen)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch)
                    |> P.bind
                        (\c41 ->
                            let
                                _ =
                                    Debug.log "c41" c41
                            in
                            P.specialize E.IfThenBranch (expression syntaxVersion)
                        )
                    |> P.bind
                        (\( thenBranch, thenEnd ) ->
                            Space.checkIndent thenEnd E.IfIndentElse
                                |> P.bind (\_ -> Keyword.else_ E.IfElse)
                                |> P.bind (\_ -> Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch)
                                |> P.bind
                                    (\c42 ->
                                        let
                                            _ =
                                                Debug.log "c42" c42

                                            newBranches : List ( Src.Expr, Src.Expr )
                                            newBranches =
                                                ( condition, thenBranch ) :: branches
                                        in
                                        P.oneOf E.IfElseBranchStart
                                            [ Keyword.if_ E.IfElseBranchStart
                                                |> P.bind (\_ -> chompIfEnd syntaxVersion start newBranches)
                                            , P.specialize E.IfElseBranch (expression syntaxVersion)
                                                |> P.fmap
                                                    (\( elseBranch, elseEnd ) ->
                                                        let
                                                            ifExpr : Src.Expr_
                                                            ifExpr =
                                                                Src.If (List.reverse newBranches) elseBranch
                                                        in
                                                        ( A.at start elseEnd ifExpr, elseEnd )
                                                    )
                                            ]
                                    )
                        )
            )



-- LAMBDA EXPRESSION


function : SyntaxVersion -> A.Position -> Space.Parser E.Expr Src.Expr
function syntaxVersion start =
    P.inContext E.Func (P.word1 '\\' E.Start) <|
        (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArg
            |> P.bind
                (\c43 ->
                    let
                        _ =
                            Debug.log "c43" c43
                    in
                    P.specialize E.FuncArg (Pattern.term syntaxVersion)
                )
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
                        |> P.bind
                            (\c44 ->
                                let
                                    _ =
                                        Debug.log "c44" c44
                                in
                                chompArgs syntaxVersion [ arg ]
                            )
                        |> P.bind
                            (\revArgs ->
                                Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody
                                    |> P.bind
                                        (\c45 ->
                                            let
                                                _ =
                                                    Debug.log "c45" c45
                                            in
                                            P.specialize E.FuncBody (expression syntaxVersion)
                                        )
                                    |> P.fmap
                                        (\( body, end ) ->
                                            let
                                                funcExpr : Src.Expr_
                                                funcExpr =
                                                    Src.Lambda (List.reverse revArgs) body
                                            in
                                            ( A.at start end funcExpr, end )
                                        )
                            )
                )
        )


chompArgs : SyntaxVersion -> List Src.Pattern -> P.Parser E.Func (List Src.Pattern)
chompArgs syntaxVersion revArgs =
    P.oneOf E.FuncArrow
        [ P.specialize E.FuncArg (Pattern.term syntaxVersion)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
                        |> P.bind
                            (\c46 ->
                                let
                                    _ =
                                        Debug.log "c46" c46
                                in
                                chompArgs syntaxVersion (arg :: revArgs)
                            )
                )
        , P.word2 '-' '>' E.FuncArrow
            |> P.fmap (\_ -> revArgs)
        ]



-- CASE EXPRESSIONS


case_ : SyntaxVersion -> A.Position -> Space.Parser E.Expr Src.Expr
case_ syntaxVersion start =
    P.inContext E.Case (Keyword.case_ E.Start) <|
        (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr
            |> P.bind
                (\c47 ->
                    let
                        _ =
                            Debug.log "c47" c47
                    in
                    P.specialize E.CaseExpr (expression syntaxVersion)
                )
            |> P.bind
                (\( expr, exprEnd ) ->
                    Space.checkIndent exprEnd E.CaseIndentOf
                        |> P.bind (\_ -> Keyword.of_ E.CaseOf)
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern)
                        |> P.bind
                            (\c48 ->
                                let
                                    _ =
                                        Debug.log "c48" c48
                                in
                                P.withIndent <|
                                    (chompBranch syntaxVersion
                                        |> P.bind
                                            (\( firstBranch, firstEnd ) ->
                                                chompCaseEnd syntaxVersion [ firstBranch ] firstEnd
                                                    |> P.fmap
                                                        (\( branches, end ) ->
                                                            ( A.at start end (Src.Case expr branches)
                                                            , end
                                                            )
                                                        )
                                            )
                                    )
                            )
                )
        )


chompBranch : SyntaxVersion -> Space.Parser E.Case ( Src.Pattern, Src.Expr )
chompBranch syntaxVersion =
    P.specialize E.CasePattern (Pattern.expression syntaxVersion)
        |> P.bind
            (\( pattern, patternEnd ) ->
                Space.checkIndent patternEnd E.CaseIndentArrow
                    |> P.bind (\_ -> P.word2 '-' '>' E.CaseArrow)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.CaseSpace E.CaseIndentBranch)
                    |> P.bind
                        (\c49 ->
                            let
                                _ =
                                    Debug.log "c49" c49
                            in
                            P.specialize E.CaseBranch (expression syntaxVersion)
                        )
                    |> P.fmap (\( branchExpr, end ) -> ( ( pattern, branchExpr ), end ))
            )


chompCaseEnd : SyntaxVersion -> List ( Src.Pattern, Src.Expr ) -> A.Position -> Space.Parser E.Case (List ( Src.Pattern, Src.Expr ))
chompCaseEnd syntaxVersion branches end =
    P.oneOfWithFallback
        [ Space.checkAligned E.CasePatternAlignment
            |> P.bind (\_ -> chompBranch syntaxVersion)
            |> P.bind (\( branch, newEnd ) -> chompCaseEnd syntaxVersion (branch :: branches) newEnd)
        ]
        ( List.reverse branches, end )



-- LET EXPRESSION


let_ : SyntaxVersion -> A.Position -> Space.Parser E.Expr Src.Expr
let_ syntaxVersion start =
    P.inContext E.Let (Keyword.let_ E.Start) <|
        ((P.withBacksetIndent 3 <|
            (Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
                |> P.bind
                    (\c50 ->
                        let
                            _ =
                                Debug.log "c50" c50
                        in
                        P.withIndent <|
                            (chompLetDef syntaxVersion
                                |> P.bind (\( def, end ) -> chompLetDefs syntaxVersion [ def ] end)
                            )
                    )
            )
         )
            |> P.bind
                (\( defs, defsEnd ) ->
                    Space.checkIndent defsEnd E.LetIndentIn
                        |> P.bind (\_ -> Keyword.in_ E.LetIn)
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.LetSpace E.LetIndentBody)
                        |> P.bind
                            (\c51 ->
                                let
                                    _ =
                                        Debug.log "c51" c51
                                in
                                P.specialize E.LetBody (expression syntaxVersion)
                            )
                        |> P.fmap
                            (\( body, end ) ->
                                ( A.at start end (Src.Let defs body), end )
                            )
                )
        )


chompLetDefs : SyntaxVersion -> List (A.Located Src.Def) -> A.Position -> Space.Parser E.Let (List (A.Located Src.Def))
chompLetDefs syntaxVersion revDefs end =
    P.oneOfWithFallback
        [ Space.checkAligned E.LetDefAlignment
            |> P.bind (\_ -> chompLetDef syntaxVersion)
            |> P.bind (\( def, newEnd ) -> chompLetDefs syntaxVersion (def :: revDefs) newEnd)
        ]
        ( List.reverse revDefs, end )



-- LET DEFINITIONS


chompLetDef : SyntaxVersion -> Space.Parser E.Let (A.Located Src.Def)
chompLetDef syntaxVersion =
    P.oneOf E.LetDefName
        [ definition syntaxVersion
        , destructure syntaxVersion
        ]



-- DEFINITION


definition : SyntaxVersion -> Space.Parser E.Let (A.Located Src.Def)
definition syntaxVersion =
    P.addLocation (Var.lower E.LetDefName)
        |> P.bind
            (\((A.At (A.Region start _) name) as aname) ->
                P.specialize (E.LetDef name) <|
                    (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                        |> P.bind
                            (\c52 ->
                                let
                                    _ =
                                        Debug.log "c52" c52
                                in
                                P.oneOf E.DefEquals
                                    [ P.word1 ':' E.DefEquals
                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.DefSpace E.DefIndentType)
                                        |> P.bind
                                            (\c53 ->
                                                let
                                                    _ =
                                                        Debug.log "c53" c53
                                                in
                                                P.specialize E.DefType Type.expression
                                            )
                                        |> P.bind
                                            (\( ( _, tipe ), _ ) ->
                                                Space.checkAligned E.DefAlignment
                                                    |> P.bind (\_ -> chompMatchingName name)
                                                    |> P.bind
                                                        (\defName ->
                                                            Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                                                                |> P.bind
                                                                    (\c54 ->
                                                                        let
                                                                            _ =
                                                                                Debug.log "c54" c54
                                                                        in
                                                                        chompDefArgsAndBody syntaxVersion start defName (Just tipe) []
                                                                    )
                                                        )
                                            )
                                    , chompDefArgsAndBody syntaxVersion start aname Nothing []
                                    ]
                            )
                    )
            )


chompDefArgsAndBody : SyntaxVersion -> A.Position -> A.Located Name.Name -> Maybe Src.Type -> List Src.Pattern -> Space.Parser E.Def (A.Located Src.Def)
chompDefArgsAndBody syntaxVersion start name tipe revArgs =
    P.oneOf E.DefEquals
        [ P.specialize E.DefArg (Pattern.term syntaxVersion)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                        |> P.bind
                            (\c55 ->
                                let
                                    _ =
                                        Debug.log "c55" c55
                                in
                                chompDefArgsAndBody syntaxVersion start name tipe (arg :: revArgs)
                            )
                )
        , P.word1 '=' E.DefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.DefSpace E.DefIndentBody)
            |> P.bind
                (\c56 ->
                    let
                        _ =
                            Debug.log "c56" c56
                    in
                    P.specialize E.DefBody (expression syntaxVersion)
                )
            |> P.fmap
                (\( body, end ) ->
                    ( A.at start end (Src.Define name (List.reverse revArgs) body tipe)
                    , end
                    )
                )
        ]


chompMatchingName : Name.Name -> P.Parser E.Def (A.Located Name.Name)
chompMatchingName expectedName =
    let
        (P.Parser parserL) =
            Var.lower E.DefNameRepeat
    in
    P.Parser <|
        \((P.State _ _ _ _ sr sc) as state) ->
            case parserL state of
                P.Cok name ((P.State _ _ _ _ er ec) as newState) ->
                    if expectedName == name then
                        P.Cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState

                    else
                        P.Cerr sr sc (E.DefNameMatch name)

                P.Eok name ((P.State _ _ _ _ er ec) as newState) ->
                    if expectedName == name then
                        P.Eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState

                    else
                        P.Eerr sr sc (E.DefNameMatch name)

                P.Cerr r c t ->
                    P.Cerr r c t

                P.Eerr r c t ->
                    P.Eerr r c t



-- DESTRUCTURE


destructure : SyntaxVersion -> Space.Parser E.Let (A.Located Src.Def)
destructure syntaxVersion =
    P.specialize E.LetDestruct <|
        (P.getPosition
            |> P.bind
                (\start ->
                    P.specialize E.DestructPattern (Pattern.term syntaxVersion)
                        |> P.bind
                            (\pattern ->
                                Space.chompAndCheckIndent E.DestructSpace E.DestructIndentEquals
                                    |> P.bind
                                        (\c57 ->
                                            let
                                                _ =
                                                    Debug.log "c57" c57
                                            in
                                            P.word1 '=' E.DestructEquals
                                        )
                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.DestructSpace E.DestructIndentBody)
                                    |> P.bind
                                        (\c58 ->
                                            let
                                                _ =
                                                    Debug.log "c58" c58
                                            in
                                            P.specialize E.DestructBody (expression syntaxVersion)
                                        )
                                    |> P.fmap
                                        (\( expr, end ) ->
                                            ( A.at start end (Src.Destruct pattern expr)
                                            , end
                                            )
                                        )
                            )
                )
        )
