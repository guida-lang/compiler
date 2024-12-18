module Compiler.Parse.Expression exposing (expression)

import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Primitives as P
import Compiler.Parse.Shader as Shader
import Compiler.Parse.Space as Space
import Compiler.Parse.String as String
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Type as Type
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Types as T



-- TERMS


term : P.Parser E.CRES_Expr T.CASTS_Expr
term =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.CRES_Start
                    [ variable start |> P.bind (accessible start)
                    , string start
                    , number start
                    , Shader.shader start
                    , list start
                    , record start |> P.bind (accessible start)
                    , tuple start |> P.bind (accessible start)
                    , accessor start
                    , character start
                    ]
            )


string : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
string start =
    String.string E.CRES_Start E.CRES_String_
        |> P.bind (\str -> P.addEnd start (T.CASTS_Str str))


character : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
character start =
    String.character E.CRES_Start E.CRES_Char
        |> P.bind (\chr -> P.addEnd start (T.CASTS_Chr chr))


number : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
number start =
    Number.number E.CRES_Start E.CRES_Number
        |> P.bind
            (\nmbr ->
                P.addEnd start <|
                    case nmbr of
                        Number.Int int ->
                            T.CASTS_Int int

                        Number.Float float ->
                            T.CASTS_Float float
            )


accessor : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
accessor start =
    P.word1 '.' E.CRES_Dot
        |> P.bind (\_ -> Var.lower E.CRES_Access)
        |> P.bind (\field -> P.addEnd start (T.CASTS_Accessor field))


variable : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
variable start =
    Var.foreignAlpha E.CRES_Start
        |> P.bind (\var -> P.addEnd start var)


accessible : T.CRA_Position -> T.CASTS_Expr -> P.Parser E.CRES_Expr T.CASTS_Expr
accessible start expr =
    P.oneOfWithFallback
        [ P.word1 '.' E.CRES_Dot
            |> P.bind (\_ -> P.getPosition)
            |> P.bind
                (\pos ->
                    Var.lower E.CRES_Access
                        |> P.bind
                            (\field ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            accessible start <|
                                                A.at start end (T.CASTS_Access expr (A.at pos end field))
                                        )
                            )
                )
        ]
        expr



-- LISTS


list : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
list start =
    P.inContext E.CRES_List (P.word1 '[' E.CRES_Start) <|
        (Space.chompAndCheckIndent E.CRES_ListSpace E.CRES_ListIndentOpen
            |> P.bind
                (\_ ->
                    P.oneOf E.CRES_ListOpen
                        [ P.specialize E.CRES_ListExpr expression
                            |> P.bind
                                (\( entry, end ) ->
                                    Space.checkIndent end E.CRES_ListIndentEnd
                                        |> P.bind (\_ -> chompListEnd start [ entry ])
                                )
                        , P.word1 ']' E.CRES_ListOpen
                            |> P.bind (\_ -> P.addEnd start (T.CASTS_List []))
                        ]
                )
        )


chompListEnd : T.CRA_Position -> List T.CASTS_Expr -> P.Parser E.CRES_List_ T.CASTS_Expr
chompListEnd start entries =
    P.oneOf E.CRES_ListEnd
        [ P.word1 ',' E.CRES_ListEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_ListSpace E.CRES_ListIndentExpr)
            |> P.bind (\_ -> P.specialize E.CRES_ListExpr expression)
            |> P.bind
                (\( entry, end ) ->
                    Space.checkIndent end E.CRES_ListIndentEnd
                        |> P.bind (\_ -> chompListEnd start (entry :: entries))
                )
        , P.word1 ']' E.CRES_ListEnd
            |> P.bind (\_ -> P.addEnd start (T.CASTS_List (List.reverse entries)))
        ]



-- TUPLES


tuple : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
tuple ((T.CRA_Position row col) as start) =
    P.inContext E.CRES_Tuple (P.word1 '(' E.CRES_Start) <|
        (P.getPosition
            |> P.bind
                (\before ->
                    Space.chompAndCheckIndent E.CRES_TupleSpace E.CRES_TupleIndentExpr1
                        |> P.bind
                            (\_ ->
                                P.getPosition
                                    |> P.bind
                                        (\after ->
                                            if before /= after then
                                                P.specialize E.CRES_TupleExpr expression
                                                    |> P.bind
                                                        (\( entry, end ) ->
                                                            Space.checkIndent end E.CRES_TupleIndentEnd
                                                                |> P.bind (\_ -> chompTupleEnd start entry [])
                                                        )

                                            else
                                                P.oneOf E.CRES_TupleIndentExpr1
                                                    [ Symbol.operator E.CRES_TupleIndentExpr1 E.CRES_TupleOperatorReserved
                                                        |> P.bind
                                                            (\op ->
                                                                if op == "-" then
                                                                    P.oneOf E.CRES_TupleOperatorClose
                                                                        [ P.word1 ')' E.CRES_TupleOperatorClose
                                                                            |> P.bind (\_ -> P.addEnd start (T.CASTS_Op op))
                                                                        , P.specialize E.CRES_TupleExpr
                                                                            (term
                                                                                |> P.bind
                                                                                    (\((T.CRA_At (T.CRA_Region _ end) _) as negatedExpr) ->
                                                                                        Space.chomp E.CRES_Space
                                                                                            |> P.bind
                                                                                                (\_ ->
                                                                                                    let
                                                                                                        exprStart : T.CRA_Position
                                                                                                        exprStart =
                                                                                                            T.CRA_Position row (col + 2)

                                                                                                        expr : T.CRA_Located T.CASTS_Expr_
                                                                                                        expr =
                                                                                                            A.at exprStart end (T.CASTS_Negate negatedExpr)
                                                                                                    in
                                                                                                    chompExprEnd exprStart
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
                                                                                    Space.checkIndent end E.CRES_TupleIndentEnd
                                                                                        |> P.bind (\_ -> chompTupleEnd start entry [])
                                                                                )
                                                                        ]

                                                                else
                                                                    P.word1 ')' E.CRES_TupleOperatorClose
                                                                        |> P.bind (\_ -> P.addEnd start (T.CASTS_Op op))
                                                            )
                                                    , P.word1 ')' E.CRES_TupleIndentExpr1
                                                        |> P.bind (\_ -> P.addEnd start T.CASTS_Unit)
                                                    , P.specialize E.CRES_TupleExpr expression
                                                        |> P.bind
                                                            (\( entry, end ) ->
                                                                Space.checkIndent end E.CRES_TupleIndentEnd
                                                                    |> P.bind (\_ -> chompTupleEnd start entry [])
                                                            )
                                                    ]
                                        )
                            )
                )
        )


chompTupleEnd : T.CRA_Position -> T.CASTS_Expr -> List T.CASTS_Expr -> P.Parser E.CRES_Tuple T.CASTS_Expr
chompTupleEnd start firstExpr revExprs =
    P.oneOf E.CRES_TupleEnd
        [ P.word1 ',' E.CRES_TupleEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.CRES_TupleSpace E.CRES_TupleIndentExprN
                        |> P.bind
                            (\_ ->
                                P.specialize E.CRES_TupleExpr expression
                                    |> P.bind
                                        (\( entry, end ) ->
                                            Space.checkIndent end E.CRES_TupleIndentEnd
                                                |> P.bind (\_ -> chompTupleEnd start firstExpr (entry :: revExprs))
                                        )
                            )
                )
        , P.word1 ')' E.CRES_TupleEnd
            |> P.bind
                (\_ ->
                    case List.reverse revExprs of
                        [] ->
                            P.pure firstExpr

                        secondExpr :: otherExprs ->
                            P.addEnd start (T.CASTS_Tuple firstExpr secondExpr otherExprs)
                )
        ]



-- RECORDS


record : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
record start =
    P.inContext E.CRES_Record (P.word1 '{' E.CRES_Start) <|
        (Space.chompAndCheckIndent E.CRES_RecordSpace E.CRES_RecordIndentOpen
            |> P.bind
                (\_ ->
                    P.oneOf E.CRES_RecordOpen
                        [ P.word1 '}' E.CRES_RecordOpen
                            |> P.bind (\_ -> P.addEnd start (T.CASTS_Record []))
                        , P.addLocation (Var.lower E.CRES_RecordField)
                            |> P.bind
                                (\starter ->
                                    Space.chompAndCheckIndent E.CRES_RecordSpace E.CRES_RecordIndentEquals
                                        |> P.bind
                                            (\_ ->
                                                P.oneOf E.CRES_RecordEquals
                                                    [ P.word1 '|' E.CRES_RecordEquals
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_RecordSpace E.CRES_RecordIndentField)
                                                        |> P.bind (\_ -> chompField)
                                                        |> P.bind (\firstField -> chompFields [ firstField ])
                                                        |> P.bind (\fields -> P.addEnd start (T.CASTS_Update starter fields))
                                                    , P.word1 '=' E.CRES_RecordEquals
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_RecordSpace E.CRES_RecordIndentExpr)
                                                        |> P.bind (\_ -> P.specialize E.CRES_RecordExpr expression)
                                                        |> P.bind
                                                            (\( value, end ) ->
                                                                Space.checkIndent end E.CRES_RecordIndentEnd
                                                                    |> P.bind (\_ -> chompFields [ ( starter, value ) ])
                                                                    |> P.bind (\fields -> P.addEnd start (T.CASTS_Record fields))
                                                            )
                                                    ]
                                            )
                                )
                        ]
                )
        )


type alias Field =
    ( T.CRA_Located T.CDN_Name, T.CASTS_Expr )


chompFields : List Field -> P.Parser E.CRES_Record (List Field)
chompFields fields =
    P.oneOf E.CRES_RecordEnd
        [ P.word1 ',' E.CRES_RecordEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_RecordSpace E.CRES_RecordIndentField)
            |> P.bind (\_ -> chompField)
            |> P.bind (\f -> chompFields (f :: fields))
        , P.word1 '}' E.CRES_RecordEnd
            |> P.fmap (\_ -> List.reverse fields)
        ]


chompField : P.Parser E.CRES_Record Field
chompField =
    P.addLocation (Var.lower E.CRES_RecordField)
        |> P.bind
            (\key ->
                Space.chompAndCheckIndent E.CRES_RecordSpace E.CRES_RecordIndentEquals
                    |> P.bind (\_ -> P.word1 '=' E.CRES_RecordEquals)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_RecordSpace E.CRES_RecordIndentExpr)
                    |> P.bind (\_ -> P.specialize E.CRES_RecordExpr expression)
                    |> P.bind
                        (\( value, end ) ->
                            Space.checkIndent end E.CRES_RecordIndentEnd
                                |> P.fmap (\_ -> ( key, value ))
                        )
            )



-- EXPRESSIONS


expression : Space.Parser E.CRES_Expr T.CASTS_Expr
expression =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.CRES_Start
                    [ let_ start
                    , if_ start
                    , case_ start
                    , function start
                    , possiblyNegativeTerm start
                        |> P.bind
                            (\expr ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            Space.chomp E.CRES_Space
                                                |> P.bind
                                                    (\_ ->
                                                        chompExprEnd start
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
        { ops : List ( T.CASTS_Expr, T.CRA_Located T.CDN_Name )
        , expr : T.CASTS_Expr
        , args : List T.CASTS_Expr
        , end : T.CRA_Position
        }


chompExprEnd : T.CRA_Position -> State -> Space.Parser E.CRES_Expr T.CASTS_Expr
chompExprEnd start (State { ops, expr, args, end }) =
    P.oneOfWithFallback
        [ -- argument
          Space.checkIndent end E.CRES_Start
            |> P.bind (\_ -> term)
            |> P.bind
                (\arg ->
                    P.getPosition
                        |> P.bind
                            (\newEnd ->
                                Space.chomp E.CRES_Space
                                    |> P.bind
                                        (\_ ->
                                            chompExprEnd start
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
          Space.checkIndent end E.CRES_Start
            |> P.bind (\_ -> P.addLocation (Symbol.operator E.CRES_Start E.CRES_OperatorReserved))
            |> P.bind
                (\((T.CRA_At (T.CRA_Region opStart opEnd) opName) as op) ->
                    Space.chompAndCheckIndent E.CRES_Space (E.CRES_IndentOperatorRight opName)
                        |> P.bind (\_ -> P.getPosition)
                        |> P.bind
                            (\newStart ->
                                if "-" == opName && end /= opStart && opEnd == newStart then
                                    -- negative terms
                                    term
                                        |> P.bind
                                            (\negatedExpr ->
                                                P.getPosition
                                                    |> P.bind
                                                        (\newEnd ->
                                                            Space.chomp E.CRES_Space
                                                                |> P.bind
                                                                    (\_ ->
                                                                        let
                                                                            arg : T.CRA_Located T.CASTS_Expr_
                                                                            arg =
                                                                                A.at opStart newEnd (T.CASTS_Negate negatedExpr)
                                                                        in
                                                                        chompExprEnd start
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
                                        err : T.CPP_Row -> T.CPP_Col -> E.CRES_Expr
                                        err =
                                            E.CRES_OperatorRight opName
                                    in
                                    P.oneOf err
                                        [ -- term
                                          possiblyNegativeTerm newStart
                                            |> P.bind
                                                (\newExpr ->
                                                    P.getPosition
                                                        |> P.bind
                                                            (\newEnd ->
                                                                Space.chomp E.CRES_Space
                                                                    |> P.bind
                                                                        (\_ ->
                                                                            let
                                                                                newOps : List ( T.CASTS_Expr, T.CRA_Located T.CDN_Name )
                                                                                newOps =
                                                                                    ( toCall expr args, op ) :: ops
                                                                            in
                                                                            chompExprEnd start
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
                                            [ let_ newStart
                                            , case_ newStart
                                            , if_ newStart
                                            , function newStart
                                            ]
                                            |> P.fmap
                                                (\( newLast, newEnd ) ->
                                                    let
                                                        newOps : List ( T.CASTS_Expr, T.CRA_Located T.CDN_Name )
                                                        newOps =
                                                            ( toCall expr args, op ) :: ops

                                                        finalExpr : T.CASTS_Expr_
                                                        finalExpr =
                                                            T.CASTS_Binops (List.reverse newOps) newLast
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
                ( A.at start end (T.CASTS_Binops (List.reverse ops) (toCall expr args))
                , end
                )
        )


possiblyNegativeTerm : T.CRA_Position -> P.Parser E.CRES_Expr T.CASTS_Expr
possiblyNegativeTerm start =
    P.oneOf E.CRES_Start
        [ P.word1 '-' E.CRES_Start
            |> P.bind
                (\_ ->
                    term
                        |> P.bind
                            (\expr ->
                                P.addEnd start (T.CASTS_Negate expr)
                            )
                )
        , term
        ]


toCall : T.CASTS_Expr -> List T.CASTS_Expr -> T.CASTS_Expr
toCall func revArgs =
    case revArgs of
        [] ->
            func

        lastArg :: _ ->
            A.merge func lastArg (T.CASTS_Call func (List.reverse revArgs))



-- IF EXPRESSION


if_ : T.CRA_Position -> Space.Parser E.CRES_Expr T.CASTS_Expr
if_ start =
    P.inContext E.CRES_If (Keyword.if_ E.CRES_Start) <|
        chompIfEnd start []


chompIfEnd : T.CRA_Position -> List ( T.CASTS_Expr, T.CASTS_Expr ) -> Space.Parser E.CRES_If T.CASTS_Expr
chompIfEnd start branches =
    Space.chompAndCheckIndent E.CRES_IfSpace E.CRES_IfIndentCondition
        |> P.bind (\_ -> P.specialize E.CRES_IfCondition expression)
        |> P.bind
            (\( condition, condEnd ) ->
                Space.checkIndent condEnd E.CRES_IfIndentThen
                    |> P.bind (\_ -> Keyword.then_ E.CRES_IfThen)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_IfSpace E.CRES_IfIndentThenBranch)
                    |> P.bind (\_ -> P.specialize E.CRES_IfThenBranch expression)
                    |> P.bind
                        (\( thenBranch, thenEnd ) ->
                            Space.checkIndent thenEnd E.CRES_IfIndentElse
                                |> P.bind (\_ -> Keyword.else_ E.CRES_IfElse)
                                |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_IfSpace E.CRES_IfIndentElseBranch)
                                |> P.bind
                                    (\_ ->
                                        let
                                            newBranches : List ( T.CASTS_Expr, T.CASTS_Expr )
                                            newBranches =
                                                ( condition, thenBranch ) :: branches
                                        in
                                        P.oneOf E.CRES_IfElseBranchStart
                                            [ Keyword.if_ E.CRES_IfElseBranchStart
                                                |> P.bind (\_ -> chompIfEnd start newBranches)
                                            , P.specialize E.CRES_IfElseBranch expression
                                                |> P.fmap
                                                    (\( elseBranch, elseEnd ) ->
                                                        let
                                                            ifExpr : T.CASTS_Expr_
                                                            ifExpr =
                                                                T.CASTS_If (List.reverse newBranches) elseBranch
                                                        in
                                                        ( A.at start elseEnd ifExpr, elseEnd )
                                                    )
                                            ]
                                    )
                        )
            )



-- LAMBDA EXPRESSION


function : T.CRA_Position -> Space.Parser E.CRES_Expr T.CASTS_Expr
function start =
    P.inContext E.CRES_Func (P.word1 '\\' E.CRES_Start) <|
        (Space.chompAndCheckIndent E.CRES_FuncSpace E.CRES_FuncIndentArg
            |> P.bind (\_ -> P.specialize E.CRES_FuncArg Pattern.term)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.CRES_FuncSpace E.CRES_FuncIndentArrow
                        |> P.bind (\_ -> chompArgs [ arg ])
                        |> P.bind
                            (\revArgs ->
                                Space.chompAndCheckIndent E.CRES_FuncSpace E.CRES_FuncIndentBody
                                    |> P.bind (\_ -> P.specialize E.CRES_FuncBody expression)
                                    |> P.fmap
                                        (\( body, end ) ->
                                            let
                                                funcExpr : T.CASTS_Expr_
                                                funcExpr =
                                                    T.CASTS_Lambda (List.reverse revArgs) body
                                            in
                                            ( A.at start end funcExpr, end )
                                        )
                            )
                )
        )


chompArgs : List T.CASTS_Pattern -> P.Parser E.CRES_Func (List T.CASTS_Pattern)
chompArgs revArgs =
    P.oneOf E.CRES_FuncArrow
        [ P.specialize E.CRES_FuncArg Pattern.term
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.CRES_FuncSpace E.CRES_FuncIndentArrow
                        |> P.bind (\_ -> chompArgs (arg :: revArgs))
                )
        , P.word2 '-' '>' E.CRES_FuncArrow
            |> P.fmap (\_ -> revArgs)
        ]



-- CASE EXPRESSIONS


case_ : T.CRA_Position -> Space.Parser E.CRES_Expr T.CASTS_Expr
case_ start =
    P.inContext E.CRES_Case (Keyword.case_ E.CRES_Start) <|
        (Space.chompAndCheckIndent E.CRES_CaseSpace E.CRES_CaseIndentExpr
            |> P.bind (\_ -> P.specialize E.CRES_CaseExpr expression)
            |> P.bind
                (\( expr, exprEnd ) ->
                    Space.checkIndent exprEnd E.CRES_CaseIndentOf
                        |> P.bind (\_ -> Keyword.of_ E.CRES_CaseOf)
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_CaseSpace E.CRES_CaseIndentPattern)
                        |> P.bind
                            (\_ ->
                                P.withIndent <|
                                    (chompBranch
                                        |> P.bind
                                            (\( firstBranch, firstEnd ) ->
                                                chompCaseEnd [ firstBranch ] firstEnd
                                                    |> P.fmap
                                                        (\( branches, end ) ->
                                                            ( A.at start end (T.CASTS_Case expr branches)
                                                            , end
                                                            )
                                                        )
                                            )
                                    )
                            )
                )
        )


chompBranch : Space.Parser E.CRES_Case ( T.CASTS_Pattern, T.CASTS_Expr )
chompBranch =
    P.specialize E.CRES_CasePattern Pattern.expression
        |> P.bind
            (\( pattern, patternEnd ) ->
                Space.checkIndent patternEnd E.CRES_CaseIndentArrow
                    |> P.bind (\_ -> P.word2 '-' '>' E.CRES_CaseArrow)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_CaseSpace E.CRES_CaseIndentBranch)
                    |> P.bind (\_ -> P.specialize E.CRES_CaseBranch expression)
                    |> P.fmap (\( branchExpr, end ) -> ( ( pattern, branchExpr ), end ))
            )


chompCaseEnd : List ( T.CASTS_Pattern, T.CASTS_Expr ) -> T.CRA_Position -> Space.Parser E.CRES_Case (List ( T.CASTS_Pattern, T.CASTS_Expr ))
chompCaseEnd branches end =
    P.oneOfWithFallback
        [ Space.checkAligned E.CRES_CasePatternAlignment
            |> P.bind (\_ -> chompBranch)
            |> P.bind (\( branch, newEnd ) -> chompCaseEnd (branch :: branches) newEnd)
        ]
        ( List.reverse branches, end )



-- LET EXPRESSION


let_ : T.CRA_Position -> Space.Parser E.CRES_Expr T.CASTS_Expr
let_ start =
    P.inContext E.CRES_Let (Keyword.let_ E.CRES_Start) <|
        ((P.withBacksetIndent 3 <|
            (Space.chompAndCheckIndent E.CRES_LetSpace E.CRES_LetIndentDef
                |> P.bind
                    (\_ ->
                        P.withIndent <|
                            (chompLetDef
                                |> P.bind (\( def, end ) -> chompLetDefs [ def ] end)
                            )
                    )
            )
         )
            |> P.bind
                (\( defs, defsEnd ) ->
                    Space.checkIndent defsEnd E.CRES_LetIndentIn
                        |> P.bind (\_ -> Keyword.in_ E.CRES_LetIn)
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_LetSpace E.CRES_LetIndentBody)
                        |> P.bind (\_ -> P.specialize E.CRES_LetBody expression)
                        |> P.fmap
                            (\( body, end ) ->
                                ( A.at start end (T.CASTS_Let defs body), end )
                            )
                )
        )


chompLetDefs : List (T.CRA_Located T.CASTS_Def) -> T.CRA_Position -> Space.Parser E.CRES_Let (List (T.CRA_Located T.CASTS_Def))
chompLetDefs revDefs end =
    P.oneOfWithFallback
        [ Space.checkAligned E.CRES_LetDefAlignment
            |> P.bind (\_ -> chompLetDef)
            |> P.bind (\( def, newEnd ) -> chompLetDefs (def :: revDefs) newEnd)
        ]
        ( List.reverse revDefs, end )



-- LET DEFINITIONS


chompLetDef : Space.Parser E.CRES_Let (T.CRA_Located T.CASTS_Def)
chompLetDef =
    P.oneOf E.CRES_LetDefName
        [ definition
        , destructure
        ]



-- DEFINITION


definition : Space.Parser E.CRES_Let (T.CRA_Located T.CASTS_Def)
definition =
    P.addLocation (Var.lower E.CRES_LetDefName)
        |> P.bind
            (\((T.CRA_At (T.CRA_Region start _) name) as aname) ->
                P.specialize (E.CRES_LetDef name) <|
                    (Space.chompAndCheckIndent E.CRES_DefSpace E.CRES_DefIndentEquals
                        |> P.bind
                            (\_ ->
                                P.oneOf E.CRES_DefEquals
                                    [ P.word1 ':' E.CRES_DefEquals
                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_DefSpace E.CRES_DefIndentType)
                                        |> P.bind (\_ -> P.specialize E.CRES_DefType Type.expression)
                                        |> P.bind
                                            (\( tipe, _ ) ->
                                                Space.checkAligned E.CRES_DefAlignment
                                                    |> P.bind (\_ -> chompMatchingName name)
                                                    |> P.bind
                                                        (\defName ->
                                                            Space.chompAndCheckIndent E.CRES_DefSpace E.CRES_DefIndentEquals
                                                                |> P.bind (\_ -> chompDefArgsAndBody start defName (Just tipe) [])
                                                        )
                                            )
                                    , chompDefArgsAndBody start aname Nothing []
                                    ]
                            )
                    )
            )


chompDefArgsAndBody : T.CRA_Position -> T.CRA_Located T.CDN_Name -> Maybe T.CASTS_Type -> List T.CASTS_Pattern -> Space.Parser E.CRES_Def (T.CRA_Located T.CASTS_Def)
chompDefArgsAndBody start name tipe revArgs =
    P.oneOf E.CRES_DefEquals
        [ P.specialize E.CRES_DefArg Pattern.term
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.CRES_DefSpace E.CRES_DefIndentEquals
                        |> P.bind (\_ -> chompDefArgsAndBody start name tipe (arg :: revArgs))
                )
        , P.word1 '=' E.CRES_DefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_DefSpace E.CRES_DefIndentBody)
            |> P.bind (\_ -> P.specialize E.CRES_DefBody expression)
            |> P.fmap
                (\( body, end ) ->
                    ( A.at start end (T.CASTS_Define name (List.reverse revArgs) body tipe)
                    , end
                    )
                )
        ]


chompMatchingName : T.CDN_Name -> P.Parser E.CRES_Def (T.CRA_Located T.CDN_Name)
chompMatchingName expectedName =
    let
        (P.Parser parserL) =
            Var.lower E.CRES_DefNameRepeat
    in
    P.Parser <|
        \((P.State _ _ _ _ sr sc) as state) ->
            Result.andThen
                (\(P.POk status name ((P.State _ _ _ _ er ec) as newState)) ->
                    if expectedName == name then
                        Ok (P.POk status (T.CRA_At (T.CRA_Region (T.CRA_Position sr sc) (T.CRA_Position er ec)) name) newState)

                    else
                        Err (P.PErr status sr sc (E.CRES_DefNameMatch name))
                )
                (parserL state)



-- DESTRUCTURE


destructure : Space.Parser E.CRES_Let (T.CRA_Located T.CASTS_Def)
destructure =
    P.specialize E.CRES_LetDestruct <|
        (P.getPosition
            |> P.bind
                (\start ->
                    P.specialize E.CRES_DestructPattern Pattern.term
                        |> P.bind
                            (\pattern ->
                                Space.chompAndCheckIndent E.CRES_DestructSpace E.CRES_DestructIndentEquals
                                    |> P.bind (\_ -> P.word1 '=' E.CRES_DestructEquals)
                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.CRES_DestructSpace E.CRES_DestructIndentBody)
                                    |> P.bind (\_ -> P.specialize E.CRES_DestructBody expression)
                                    |> P.fmap
                                        (\( expr, end ) ->
                                            ( A.at start end (T.CASTS_Destruct pattern expr)
                                            , end
                                            )
                                        )
                            )
                )
        )
