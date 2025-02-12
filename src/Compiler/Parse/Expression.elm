module Compiler.Parse.Expression exposing (expression)

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Primitives as P
import Compiler.Parse.Shader as Shader
import Compiler.Parse.Space as Space
import Compiler.Parse.String as String
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
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
                (\_ ->
                    P.oneOf E.ListOpen
                        [ P.specialize E.ListExpr (expression syntaxVersion)
                            |> P.bind
                                (\( entry, end ) ->
                                    Space.checkIndent end E.ListIndentEnd
                                        |> P.bind (\_ -> chompListEnd syntaxVersion start [ entry ])
                                )
                        , P.word1 ']' E.ListOpen
                            |> P.bind (\_ -> P.addEnd start (Src.List []))
                        ]
                )
        )


chompListEnd : SyntaxVersion -> A.Position -> List Src.Expr -> P.Parser E.List_ Src.Expr
chompListEnd syntaxVersion start entries =
    P.oneOf E.ListEnd
        [ P.word1 ',' E.ListEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.ListSpace E.ListIndentExpr)
            |> P.bind (\_ -> P.specialize E.ListExpr (expression syntaxVersion))
            |> P.bind
                (\( entry, end ) ->
                    Space.checkIndent end E.ListIndentEnd
                        |> P.bind (\_ -> chompListEnd syntaxVersion start (entry :: entries))
                )
        , P.word1 ']' E.ListEnd
            |> P.bind (\_ -> P.addEnd start (Src.List (List.reverse entries)))
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
                            (\_ ->
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
                                                                                                (\_ ->
                                                                                                    let
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
                            (\_ ->
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
    P.inContext E.Record (P.word1 '{' E.Start) <|
        (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen
            |> P.bind
                (\_ ->
                    P.oneOf E.RecordOpen
                        [ P.word1 '}' E.RecordOpen
                            |> P.bind (\_ -> P.addEnd start (Src.Record []))
                        , P.addLocation (Var.lower E.RecordField)
                            |> P.bind
                                (\starter ->
                                    Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                                        |> P.bind
                                            (\_ ->
                                                P.oneOf E.RecordEquals
                                                    [ P.word1 '|' E.RecordEquals
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
                                                        |> P.bind (\_ -> chompField syntaxVersion)
                                                        |> P.bind (\firstField -> chompFields syntaxVersion [ firstField ])
                                                        |> P.bind (\fields -> P.addEnd start (Src.Update starter fields))
                                                    , P.word1 '=' E.RecordEquals
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                                                        |> P.bind (\_ -> P.specialize E.RecordExpr (expression syntaxVersion))
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


type alias Field =
    ( A.Located Name.Name, Src.Expr )


chompFields : SyntaxVersion -> List Field -> P.Parser E.Record (List Field)
chompFields syntaxVersion fields =
    P.oneOf E.RecordEnd
        [ P.word1 ',' E.RecordEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
            |> P.bind (\_ -> chompField syntaxVersion)
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
                    |> P.bind (\_ -> P.word1 '=' E.RecordEquals)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                    |> P.bind (\_ -> P.specialize E.RecordExpr (expression syntaxVersion))
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
                                                    (\_ ->
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
                                        (\_ ->
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
                        |> P.bind (\_ -> P.getPosition)
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
                                                                    (\_ ->
                                                                        let
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
                                                                        (\_ ->
                                                                            let
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
        |> P.bind (\_ -> P.specialize E.IfCondition (expression syntaxVersion))
        |> P.bind
            (\( condition, condEnd ) ->
                Space.checkIndent condEnd E.IfIndentThen
                    |> P.bind (\_ -> Keyword.then_ E.IfThen)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch)
                    |> P.bind (\_ -> P.specialize E.IfThenBranch (expression syntaxVersion))
                    |> P.bind
                        (\( thenBranch, thenEnd ) ->
                            Space.checkIndent thenEnd E.IfIndentElse
                                |> P.bind (\_ -> Keyword.else_ E.IfElse)
                                |> P.bind (\_ -> Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch)
                                |> P.bind
                                    (\_ ->
                                        let
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
            |> P.bind (\_ -> P.specialize E.FuncArg (Pattern.term syntaxVersion))
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
                        |> P.bind (\_ -> chompArgs syntaxVersion [ arg ])
                        |> P.bind
                            (\revArgs ->
                                Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody
                                    |> P.bind (\_ -> P.specialize E.FuncBody (expression syntaxVersion))
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
                        |> P.bind (\_ -> chompArgs syntaxVersion (arg :: revArgs))
                )
        , P.word2 '-' '>' E.FuncArrow
            |> P.fmap (\_ -> revArgs)
        ]



-- CASE EXPRESSIONS


case_ : SyntaxVersion -> A.Position -> Space.Parser E.Expr Src.Expr
case_ syntaxVersion start =
    P.inContext E.Case (Keyword.case_ E.Start) <|
        (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr
            |> P.bind (\_ -> P.specialize E.CaseExpr (expression syntaxVersion))
            |> P.bind
                (\( expr, exprEnd ) ->
                    Space.checkIndent exprEnd E.CaseIndentOf
                        |> P.bind (\_ -> Keyword.of_ E.CaseOf)
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern)
                        |> P.bind
                            (\_ ->
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
                    |> P.bind (\_ -> P.specialize E.CaseBranch (expression syntaxVersion))
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
                    (\_ ->
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
                        |> P.bind (\_ -> P.specialize E.LetBody (expression syntaxVersion))
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
                            (\_ ->
                                P.oneOf E.DefEquals
                                    [ P.word1 ':' E.DefEquals
                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.DefSpace E.DefIndentType)
                                        |> P.bind (\_ -> P.specialize E.DefType Type.expression)
                                        |> P.bind
                                            (\( tipe, _ ) ->
                                                Space.checkAligned E.DefAlignment
                                                    |> P.bind (\_ -> chompMatchingName name)
                                                    |> P.bind
                                                        (\defName ->
                                                            Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                                                                |> P.bind (\_ -> chompDefArgsAndBody syntaxVersion start defName (Just tipe) [])
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
                        |> P.bind (\_ -> chompDefArgsAndBody syntaxVersion start name tipe (arg :: revArgs))
                )
        , P.word1 '=' E.DefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.DefSpace E.DefIndentBody)
            |> P.bind (\_ -> P.specialize E.DefBody (expression syntaxVersion))
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
                                    |> P.bind (\_ -> P.word1 '=' E.DestructEquals)
                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.DestructSpace E.DestructIndentBody)
                                    |> P.bind (\_ -> P.specialize E.DestructBody (expression syntaxVersion))
                                    |> P.fmap
                                        (\( expr, end ) ->
                                            ( A.at start end (Src.Destruct pattern expr)
                                            , end
                                            )
                                        )
                            )
                )
        )
