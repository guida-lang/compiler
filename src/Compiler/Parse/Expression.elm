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
        |> P.bind (\( str, representation ) -> P.addEnd start (Src.Str str representation))


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
                        Number.Int int src ->
                            Src.Int int src

                        Number.Float float src ->
                            Src.Float float src
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
                (\comments ->
                    P.oneOf E.ListOpen
                        [ P.specialize E.ListExpr (expression syntaxVersion)
                            |> P.bind
                                (\( ( postEntryComments, entry ), end ) ->
                                    Space.checkIndent end E.ListIndentEnd
                                        |> P.bind (\_ -> P.loop (chompListEnd syntaxVersion start) ( postEntryComments, [ ( ( [], comments, Nothing ), entry ) ] ))
                                )
                        , P.word1 ']' E.ListOpen
                            |> P.bind (\_ -> P.addEnd start (Src.List [] comments))
                        ]
                )
        )


chompListEnd : SyntaxVersion -> A.Position -> Src.C1 (List (Src.C2Eol Src.Expr)) -> P.Parser E.List_ (P.Step (Src.C1 (List (Src.C2Eol Src.Expr))) Src.Expr)
chompListEnd syntaxVersion start ( trailingComments, entries ) =
    P.oneOf E.ListEnd
        [ P.word1 ',' E.ListEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.ListSpace E.ListIndentExpr)
            |> P.bind
                (\postComments ->
                    P.specialize E.ListExpr (expression syntaxVersion)
                        |> P.bind
                            (\( ( preComments, entry ), end ) ->
                                Space.checkIndent end E.ListIndentEnd
                                    |> P.fmap (\_ -> P.Loop ( preComments, ( ( trailingComments, postComments, Nothing ), entry ) :: entries ))
                            )
                )
        , P.word1 ']' E.ListEnd
            |> P.bind (\_ -> P.addEnd start (Src.List (List.reverse entries) trailingComments))
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
                            (\preEntryComments ->
                                P.getPosition
                                    |> P.bind
                                        (\after ->
                                            if before /= after then
                                                P.specialize E.TupleExpr (expression syntaxVersion)
                                                    |> P.bind
                                                        (\( ( postEntryComments, entry ), end ) ->
                                                            Space.checkIndent end E.TupleIndentEnd
                                                                |> P.bind (\_ -> chompTupleEnd syntaxVersion start ( ( preEntryComments, postEntryComments ), entry ) [])
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
                                                                                                (\postTermComments ->
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
                                                                                                        postTermComments
                                                                                                )
                                                                                    )
                                                                            )
                                                                            |> P.bind
                                                                                (\( ( postEntryComments, entry ), end ) ->
                                                                                    Space.checkIndent end E.TupleIndentEnd
                                                                                        |> P.bind (\_ -> chompTupleEnd syntaxVersion start ( ( preEntryComments, postEntryComments ), entry ) [])
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
                                                            (\( ( postEntryComments, entry ), end ) ->
                                                                Space.checkIndent end E.TupleIndentEnd
                                                                    |> P.bind (\_ -> chompTupleEnd syntaxVersion start ( ( preEntryComments, postEntryComments ), entry ) [])
                                                            )
                                                    ]
                                        )
                            )
                )
        )


chompTupleEnd : SyntaxVersion -> A.Position -> Src.C2 Src.Expr -> List (Src.C2 Src.Expr) -> P.Parser E.Tuple Src.Expr
chompTupleEnd syntaxVersion start firstExpr revExprs =
    P.oneOf E.TupleEnd
        [ P.word1 ',' E.TupleEnd
            |> P.bind
                (\_ ->
                    Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExprN
                        |> P.bind
                            (\preEntryComments ->
                                P.specialize E.TupleExpr (expression syntaxVersion)
                                    |> P.bind
                                        (\( ( postEntryComments, entry ), end ) ->
                                            Space.checkIndent end E.TupleIndentEnd
                                                |> P.bind (\_ -> chompTupleEnd syntaxVersion start firstExpr (( ( preEntryComments, postEntryComments ), entry ) :: revExprs))
                                        )
                            )
                )
        , P.word1 ')' E.TupleEnd
            |> P.bind
                (\_ ->
                    case List.reverse revExprs of
                        [] ->
                            P.addEnd start (Src.Parens firstExpr)

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
                        (\preStarterNameComments ->
                            P.oneOf E.RecordOpen
                                [ P.word1 '}' E.RecordOpen
                                    |> P.bind (\_ -> P.addEnd start (Src.Record ( preStarterNameComments, [] )))
                                , P.addLocation (Var.lower E.RecordField)
                                    |> P.bind
                                        (\((A.At starterPosition starterName) as starter) ->
                                            Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                                                |> P.bind
                                                    (\postStarterNameComments ->
                                                        P.oneOf E.RecordEquals
                                                            [ P.word1 '|' E.RecordEquals
                                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
                                                                |> P.bind
                                                                    (\postPipeComments ->
                                                                        chompField syntaxVersion [] postPipeComments
                                                                    )
                                                                |> P.bind (\( postFirstFieldComments, firstField ) -> chompFields syntaxVersion postFirstFieldComments [ firstField ])
                                                                |> P.bind (\fields -> P.addEnd start (Src.Update ( ( preStarterNameComments, postStarterNameComments ), A.At starterPosition (Src.Var Src.LowVar starterName) ) fields))
                                                            , P.word1 '=' E.RecordEquals
                                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                                                                |> P.bind
                                                                    (\preValueComments ->
                                                                        P.specialize E.RecordExpr (expression syntaxVersion)
                                                                            |> P.bind
                                                                                (\( ( postValueComments, value ), end ) ->
                                                                                    Space.checkIndent end E.RecordIndentEnd
                                                                                        |> P.bind (\_ -> chompFields syntaxVersion postValueComments [ ( ( [], preStarterNameComments, Nothing ), ( ( postStarterNameComments, starter ), ( preValueComments, value ) ) ) ])
                                                                                        |> P.bind (\fields -> P.addEnd start (Src.Record fields))
                                                                                )
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
                        (\preStarterNameComments ->
                            P.oneOf E.RecordOpen
                                [ P.word1 '}' E.RecordOpen
                                    |> P.bind (\_ -> P.addEnd start (Src.Record ( preStarterNameComments, [] )))
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
                                                                (\postStarterNameComments ->
                                                                    P.word1 '|' E.RecordEquals
                                                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
                                                                        |> P.bind (\postPipeComments -> chompField syntaxVersion [] postPipeComments)
                                                                        |> P.bind (\( postFirstFieldComments, firstField ) -> chompFields syntaxVersion postFirstFieldComments [ firstField ])
                                                                        |> P.bind (\fields -> P.addEnd start (Src.Update ( ( preStarterNameComments, postStarterNameComments ), starter ) fields))
                                                                )
                                                    )
                                        )
                                , P.addLocation (Var.lower E.RecordField)
                                    |> P.bind
                                        (\starter ->
                                            Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                                                |> P.bind
                                                    (\postStarterNameComments ->
                                                        P.word1 '=' E.RecordEquals
                                                            |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                                                            |> P.bind
                                                                (\preValueComments ->
                                                                    P.specialize E.RecordExpr (expression syntaxVersion)
                                                                        |> P.bind
                                                                            (\( ( postValueComments, value ), end ) ->
                                                                                Space.checkIndent end E.RecordIndentEnd
                                                                                    |> P.bind (\_ -> chompFields syntaxVersion postValueComments [ ( ( [], preStarterNameComments, Nothing ), ( ( postStarterNameComments, starter ), ( preValueComments, value ) ) ) ])
                                                                                    |> P.bind (\fields -> P.addEnd start (Src.Record fields))
                                                                            )
                                                                )
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
    Src.C2Eol ( Src.C1 (A.Located Name.Name), Src.C1 Src.Expr )


chompFields : SyntaxVersion -> Src.FComments -> List Field -> P.Parser E.Record (Src.C1 (List Field))
chompFields syntaxVersion trailingComments fields =
    P.oneOf E.RecordEnd
        [ P.word1 ',' E.RecordEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField)
            |> P.bind (\postCommaComments -> chompField syntaxVersion trailingComments postCommaComments)
            |> P.bind (\( postFieldComments, f ) -> chompFields syntaxVersion postFieldComments (f :: fields))
        , P.word1 '}' E.RecordEnd
            |> P.fmap (\_ -> ( trailingComments, List.reverse fields ))
        ]


chompField : SyntaxVersion -> Src.FComments -> Src.FComments -> P.Parser E.Record (Src.C1 Field)
chompField syntaxVersion preCommaComents postCommaComments =
    P.addLocation (Var.lower E.RecordField)
        |> P.bind
            (\key ->
                Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                    |> P.bind
                        (\preEqualSignComments ->
                            P.word1 '=' E.RecordEquals
                                |> P.bind (\_ -> Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr)
                                |> P.bind
                                    (\postEqualSignComments ->
                                        P.specialize E.RecordExpr (expression syntaxVersion)
                                            |> P.bind
                                                (\( ( postFieldComments, value ), end ) ->
                                                    Space.checkIndent end E.RecordIndentEnd
                                                        |> P.fmap
                                                            (\_ ->
                                                                ( postFieldComments
                                                                , ( ( preCommaComents, postCommaComments, Nothing ), ( ( preEqualSignComments, key ), ( postEqualSignComments, value ) ) )
                                                                )
                                                            )
                                                )
                                    )
                        )
            )



-- EXPRESSIONS


expression : SyntaxVersion -> Space.Parser E.Expr (Src.C1 Src.Expr)
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
                                                    (\comments ->
                                                        chompExprEnd syntaxVersion
                                                            start
                                                            (State
                                                                { ops = []
                                                                , expr = expr
                                                                , args = []
                                                                , end = end
                                                                }
                                                            )
                                                            comments
                                                    )
                                        )
                            )
                    ]
            )


type State
    = State
        { ops : List ( Src.Expr, Src.C2 (A.Located Name.Name) )
        , expr : Src.Expr
        , args : List (Src.C1 Src.Expr)
        , end : A.Position
        }


chompExprEnd : SyntaxVersion -> A.Position -> State -> Src.FComments -> Space.Parser E.Expr (Src.C1 Src.Expr)
chompExprEnd syntaxVersion start (State { ops, expr, args, end }) comments =
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
                                        (\trailingComments ->
                                            chompExprEnd syntaxVersion
                                                start
                                                (State
                                                    { ops = ops
                                                    , expr = expr
                                                    , args = ( comments, arg ) :: args
                                                    , end = newEnd
                                                    }
                                                )
                                                trailingComments
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
                            (\postOpComments ->
                                P.getPosition
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
                                                                                (\postNegatedExprComments ->
                                                                                    let
                                                                                        arg : Src.C1 (A.Located Src.Expr_)
                                                                                        arg =
                                                                                            ( postNegatedExprComments, A.at opStart newEnd (Src.Negate negatedExpr) )
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
                                                                                        []
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
                                                                                    (\trailingComments ->
                                                                                        let
                                                                                            newOps : List ( Src.Expr, Src.C2 (A.Located Name.Name) )
                                                                                            newOps =
                                                                                                ( toCall expr args, ( ( comments, postOpComments ), op ) ) :: ops
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
                                                                                            trailingComments
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
                                                            (\( ( trailingComments, newLast ), newEnd ) ->
                                                                let
                                                                    newOps : List ( Src.Expr, Src.C2 (A.Located Name.Name) )
                                                                    newOps =
                                                                        ( toCall expr args, ( ( comments, [] ), op ) ) :: ops

                                                                    finalExpr : Src.Expr_
                                                                    finalExpr =
                                                                        Src.Binops (List.reverse newOps) newLast
                                                                in
                                                                ( ( trailingComments, A.at start newEnd finalExpr ), newEnd )
                                                            )
                                                    ]
                                        )
                            )
                )
        ]
        -- done
        (case ops of
            [] ->
                ( ( comments, toCall expr args )
                , end
                )

            _ ->
                ( ( comments, A.at start end (Src.Binops (List.reverse ops) (toCall expr args)) )
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


toCall : Src.Expr -> List (Src.C1 Src.Expr) -> Src.Expr
toCall func revArgs =
    case revArgs of
        [] ->
            func

        ( _, lastArg ) :: _ ->
            A.merge func lastArg (Src.Call func (List.reverse revArgs))



-- IF EXPRESSION


if_ : SyntaxVersion -> A.Position -> Space.Parser E.Expr (Src.C1 Src.Expr)
if_ syntaxVersion start =
    P.inContext E.If (Keyword.if_ E.Start) <|
        chompIfEnd syntaxVersion start [] []


chompIfEnd : SyntaxVersion -> A.Position -> Src.FComments -> List (Src.C1 ( Src.C2 Src.Expr, Src.C2 Src.Expr )) -> Space.Parser E.If (Src.C1 Src.Expr)
chompIfEnd syntaxVersion start comments branches =
    Space.chompAndCheckIndent E.IfSpace E.IfIndentCondition
        |> P.bind
            (\preConditionComments ->
                P.specialize E.IfCondition (expression syntaxVersion)
                    |> P.bind
                        (\( ( postConditionComments, condition ), condEnd ) ->
                            Space.checkIndent condEnd E.IfIndentThen
                                |> P.bind (\_ -> Keyword.then_ E.IfThen)
                                |> P.bind (\_ -> Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch)
                                |> P.bind
                                    (\preThenBranchComments ->
                                        P.specialize E.IfThenBranch (expression syntaxVersion)
                                            |> P.bind
                                                (\( ( postThenBranchComments, thenBranch ), thenEnd ) ->
                                                    Space.checkIndent thenEnd E.IfIndentElse
                                                        |> P.bind (\_ -> Keyword.else_ E.IfElse)
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch)
                                                        |> P.bind
                                                            (\trailingComments ->
                                                                let
                                                                    newBranch : Src.C1 ( Src.C2 Src.Expr, Src.C2 Src.Expr )
                                                                    newBranch =
                                                                        ( comments, ( ( ( preConditionComments, postConditionComments ), condition ), ( ( preThenBranchComments, postThenBranchComments ), thenBranch ) ) )

                                                                    newBranches : List (Src.C1 ( Src.C2 Src.Expr, Src.C2 Src.Expr ))
                                                                    newBranches =
                                                                        newBranch :: branches
                                                                in
                                                                P.oneOf E.IfElseBranchStart
                                                                    [ Keyword.if_ E.IfElseBranchStart
                                                                        |> P.bind (\_ -> chompIfEnd syntaxVersion start trailingComments newBranches)
                                                                    , P.specialize E.IfElseBranch (expression syntaxVersion)
                                                                        |> P.fmap
                                                                            (\( ( postElseBranch, elseBranch ), elseEnd ) ->
                                                                                let
                                                                                    reversedBranches : List (Src.C1 ( Src.C2 Src.Expr, Src.C2 Src.Expr ))
                                                                                    reversedBranches =
                                                                                        List.reverse newBranches

                                                                                    ifExpr : Src.Expr_
                                                                                    ifExpr =
                                                                                        Src.If (Maybe.withDefault newBranch (List.head reversedBranches)) (Maybe.withDefault [] (List.tail reversedBranches)) ( trailingComments, elseBranch )
                                                                                in
                                                                                ( ( postElseBranch, A.at start elseEnd ifExpr ), elseEnd )
                                                                            )
                                                                    ]
                                                            )
                                                )
                                    )
                        )
            )



-- LAMBDA EXPRESSION


function : SyntaxVersion -> A.Position -> Space.Parser E.Expr (Src.C1 Src.Expr)
function syntaxVersion start =
    P.inContext E.Func (P.word1 '\\' E.Start) <|
        (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArg
            |> P.bind
                (\preArgComments ->
                    P.specialize E.FuncArg (Pattern.term syntaxVersion)
                        |> P.bind
                            (\arg ->
                                Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
                                    |> P.bind (\trailingComments -> chompArgs syntaxVersion trailingComments [ ( preArgComments, arg ) ])
                                    |> P.bind
                                        (\( trailingComments, revArgs ) ->
                                            Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody
                                                |> P.bind
                                                    (\preComments ->
                                                        P.specialize E.FuncBody (expression syntaxVersion)
                                                            |> P.fmap (Tuple.mapFirst (\( afterBodyComments, body ) -> ( afterBodyComments, ( preComments, body ) )))
                                                    )
                                                |> P.fmap
                                                    (\( ( afterBodyComments, body ), end ) ->
                                                        let
                                                            funcExpr : Src.Expr_
                                                            funcExpr =
                                                                Src.Lambda ( trailingComments, List.reverse revArgs ) body
                                                        in
                                                        ( ( afterBodyComments, A.at start end funcExpr ), end )
                                                    )
                                        )
                            )
                )
        )


chompArgs : SyntaxVersion -> Src.FComments -> List (Src.C1 Src.Pattern) -> P.Parser E.Func (Src.C1 (List (Src.C1 Src.Pattern)))
chompArgs syntaxVersion trailingComments revArgs =
    P.oneOf E.FuncArrow
        [ P.specialize E.FuncArg (Pattern.term syntaxVersion)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
                        |> P.bind (\postArgComments -> chompArgs syntaxVersion postArgComments (( trailingComments, arg ) :: revArgs))
                )
        , P.word2 '-' '>' E.FuncArrow
            |> P.fmap (\_ -> ( trailingComments, revArgs ))
        ]



-- CASE EXPRESSIONS


case_ : SyntaxVersion -> A.Position -> Space.Parser E.Expr (Src.C1 Src.Expr)
case_ syntaxVersion start =
    P.inContext E.Case (Keyword.case_ E.Start) <|
        (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr
            |> P.bind
                (\preExprComments ->
                    P.specialize E.CaseExpr (expression syntaxVersion)
                        |> P.bind
                            (\( ( postExprComments, expr ), exprEnd ) ->
                                Space.checkIndent exprEnd E.CaseIndentOf
                                    |> P.bind (\_ -> Keyword.of_ E.CaseOf)
                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern)
                                    |> P.bind
                                        (\comments ->
                                            P.withIndent
                                                (chompBranch syntaxVersion comments
                                                    |> P.bind
                                                        (\( ( trailingComments, firstBranch ), firstEnd ) ->
                                                            chompCaseEnd syntaxVersion trailingComments [ firstBranch ] firstEnd
                                                                |> P.fmap
                                                                    (\( ( branchesTrailingComments, branches ), end ) ->
                                                                        ( ( branchesTrailingComments, A.at start end (Src.Case ( ( preExprComments, postExprComments ), expr ) branches) )
                                                                        , end
                                                                        )
                                                                    )
                                                        )
                                                )
                                        )
                            )
                )
        )


chompBranch : SyntaxVersion -> Src.FComments -> Space.Parser E.Case (Src.C1 ( Src.C2 Src.Pattern, Src.C1 Src.Expr ))
chompBranch syntaxVersion prePatternComments =
    P.specialize E.CasePattern (Pattern.expression syntaxVersion)
        |> P.bind
            (\( ( postPatternComments, pattern ), patternEnd ) ->
                Space.checkIndent patternEnd E.CaseIndentArrow
                    |> P.bind (\_ -> P.word2 '-' '>' E.CaseArrow)
                    |> P.bind (\_ -> Space.chompAndCheckIndent E.CaseSpace E.CaseIndentBranch)
                    |> P.bind
                        (\preBranchExprComments ->
                            P.specialize E.CaseBranch (expression syntaxVersion)
                                |> P.fmap
                                    (\( ( trailingComments, branchExpr ), end ) ->
                                        ( ( trailingComments
                                          , ( ( ( prePatternComments, postPatternComments ), pattern )
                                            , ( preBranchExprComments, branchExpr )
                                            )
                                          )
                                        , end
                                        )
                                    )
                        )
            )


chompCaseEnd : SyntaxVersion -> Src.FComments -> List ( Src.C2 Src.Pattern, Src.C1 Src.Expr ) -> A.Position -> Space.Parser E.Case (Src.C1 (List ( Src.C2 Src.Pattern, Src.C1 Src.Expr )))
chompCaseEnd syntaxVersion prePatternComments branches end =
    P.oneOfWithFallback
        [ Space.checkAligned E.CasePatternAlignment
            |> P.bind (\_ -> chompBranch syntaxVersion prePatternComments)
            |> P.bind (\( ( comments, branch ), newEnd ) -> chompCaseEnd syntaxVersion comments (branch :: branches) newEnd)
        ]
        ( ( prePatternComments, List.reverse branches ), end )



-- LET EXPRESSION


let_ : SyntaxVersion -> A.Position -> Space.Parser E.Expr (Src.C1 Src.Expr)
let_ syntaxVersion start =
    P.inContext E.Let (Keyword.let_ E.Start) <|
        ((P.withBacksetIndent 3 <|
            (Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
                |> P.bind
                    (\preDefComments ->
                        P.withIndent <|
                            (chompLetDef syntaxVersion
                                |> P.bind (\( ( postDefComments, def ), end ) -> chompLetDefs syntaxVersion [ ( ( preDefComments, postDefComments ), def ) ] end)
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
                            (\bodyComments ->
                                P.specialize E.LetBody (expression syntaxVersion)
                                    |> P.fmap
                                        (\( ( trailingComments, body ), end ) ->
                                            ( ( trailingComments, A.at start end (Src.Let defs bodyComments body) ), end )
                                        )
                            )
                )
        )


chompLetDefs : SyntaxVersion -> List (Src.C2 (A.Located Src.Def)) -> A.Position -> Space.Parser E.Let (List (Src.C2 (A.Located Src.Def)))
chompLetDefs syntaxVersion revDefs end =
    P.oneOfWithFallback
        [ Space.checkAligned E.LetDefAlignment
            |> P.bind (\_ -> chompLetDef syntaxVersion)
            |> P.bind (\( ( postDefComments, def ), newEnd ) -> chompLetDefs syntaxVersion (( ( [], postDefComments ), def ) :: revDefs) newEnd)
        ]
        ( List.reverse revDefs, end )



-- LET DEFINITIONS


chompLetDef : SyntaxVersion -> Space.Parser E.Let (Src.C1 (A.Located Src.Def))
chompLetDef syntaxVersion =
    P.oneOf E.LetDefName
        [ definition syntaxVersion
        , destructure syntaxVersion
        ]



-- DEFINITION


definition : SyntaxVersion -> Space.Parser E.Let (Src.C1 (A.Located Src.Def))
definition syntaxVersion =
    P.addLocation (Var.lower E.LetDefName)
        |> P.bind
            (\((A.At (A.Region start _) name) as aname) ->
                P.specialize (E.LetDef name) <|
                    (Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                        |> P.bind
                            (\postNameComments ->
                                P.oneOf E.DefEquals
                                    [ P.word1 ':' E.DefEquals
                                        |> P.bind (\_ -> Space.chompAndCheckIndent E.DefSpace E.DefIndentType)
                                        |> P.bind
                                            (\preTypeComments ->
                                                P.specialize E.DefType (Type.expression preTypeComments)
                                            )
                                        |> P.bind
                                            (\( ( ( preTipeComments, postTipeComments, _ ), tipe ), _ ) ->
                                                Space.checkAligned E.DefAlignment
                                                    |> P.bind (\_ -> chompMatchingName name)
                                                    |> P.bind
                                                        (\defName ->
                                                            Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                                                                |> P.bind
                                                                    (\trailingComments ->
                                                                        chompDefArgsAndBody syntaxVersion start defName (Just ( postTipeComments, ( ( postNameComments, preTipeComments ), tipe ) )) trailingComments []
                                                                    )
                                                        )
                                            )
                                    , chompDefArgsAndBody syntaxVersion start aname Nothing postNameComments []
                                    ]
                            )
                    )
            )


chompDefArgsAndBody : SyntaxVersion -> A.Position -> A.Located Name.Name -> Maybe (Src.C1 (Src.C2 Src.Type)) -> Src.FComments -> List (Src.C1 Src.Pattern) -> Space.Parser E.Def (Src.C1 (A.Located Src.Def))
chompDefArgsAndBody syntaxVersion start name tipe trailingComments revArgs =
    P.oneOf E.DefEquals
        [ P.specialize E.DefArg (Pattern.term syntaxVersion)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                        |> P.bind (\comments -> chompDefArgsAndBody syntaxVersion start name tipe comments (( trailingComments, arg ) :: revArgs))
                )
        , P.word1 '=' E.DefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.DefSpace E.DefIndentBody)
            |> P.bind
                (\preExpressionComments ->
                    P.specialize E.DefBody (expression syntaxVersion)
                        |> P.fmap
                            (\( ( comments, body ), end ) ->
                                ( ( comments, A.at start end (Src.Define name (List.reverse revArgs) ( trailingComments ++ preExpressionComments, body ) tipe) )
                                , end
                                )
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


destructure : SyntaxVersion -> Space.Parser E.Let (Src.C1 (A.Located Src.Def))
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
                                        (\preEqualSignComments ->
                                            P.word1 '=' E.DestructEquals
                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.DestructSpace E.DestructIndentBody)
                                                |> P.bind
                                                    (\preExpressionComments ->
                                                        P.specialize E.DestructBody (expression syntaxVersion)
                                                            |> P.fmap
                                                                (\( ( comments, expr ), end ) ->
                                                                    ( ( comments, A.at start end (Src.Destruct pattern ( preEqualSignComments ++ preExpressionComments, expr )) )
                                                                    , end
                                                                    )
                                                                )
                                                    )
                                        )
                            )
                )
        )
