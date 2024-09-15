module Generate.JavaScript.Builder exposing
    ( Case(..)
    , Expr(..)
    , InfixOp(..)
    , LValue(..)
    , PrefixOp(..)
    , Stmt(..)
    , exprToBuilder
    , stmtToBuilder
    )

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Generate.JavaScript.Name as Name
import Json.EncodeX as Json
import Utils.Main as Utils



-- EXPRESSIONS
-- NOTE: I tried making this create a B.Builder directly.
--
-- The hope was that it'd allocate less and speed things up, but it seemed
-- to be neutral for perf.
--
-- The downside is that Generate.JavaScript.Expression inspects the
-- structure of Expr and Stmt on some occassions to try to strip out
-- unnecessary closures. I think these closures are already avoided
-- by other logic in code gen these days, but I am not 100% certain.
--
-- For this to be worth it, I think it would be necessary to avoid
-- returning tuples when generating expressions.
--


type Expr
    = ExprString String
    | ExprFloat String
    | ExprInt Int
    | ExprBool Bool
    | ExprNull
    | ExprJson Json.Value
    | ExprArray (List Expr)
    | ExprObject (List ( Name.Name, Expr ))
    | ExprRef Name.Name
    | ExprAccess Expr Name.Name
    | ExprIndex Expr Expr
    | ExprPrefix PrefixOp Expr
    | ExprInfix InfixOp Expr Expr
    | ExprIf Expr Expr Expr
    | ExprAssign LValue Expr
    | ExprCall Expr (List Expr)
    | ExprFunction (Maybe Name.Name) (List Name.Name) (List Stmt)


type LValue
    = LRef Name.Name
    | LDot Expr Name.Name
    | LBracket Expr Expr



-- STATEMENTS


type Stmt
    = Block (List Stmt)
    | EmptyStmt
    | ExprStmt Expr
    | IfStmt Expr Stmt Stmt
    | Switch Expr (List Case)
    | While Expr Stmt
    | Break (Maybe Name.Name)
    | Continue (Maybe Name.Name)
    | Labelled Name.Name Stmt
    | Try Stmt Name.Name Stmt
    | Throw Expr
    | Return Expr
    | Var Name.Name Expr
    | Vars (List ( Name.Name, Expr ))
    | FunctionStmt Name.Name (List Name.Name) (List Stmt)


type Case
    = Case Expr (List Stmt)
    | Default (List Stmt)



-- OPERATORS


type InfixOp
    = OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    | OpAnd
    | OpOr
    | OpBitwiseAnd
    | OpBitwiseXor
    | OpBitwiseOr
    | OpLShift
    | OpSpRShift
    | OpZfRShift


type PrefixOp
    = PrefixNot
    | PrefixNegate
    | PrefixComplement



-- ENCODE


stmtToBuilder : Stmt -> String
stmtToBuilder stmts =
    fromStmt levelZero stmts


exprToBuilder : Expr -> String
exprToBuilder expr =
    Tuple.second (fromExpr levelZero Whatever expr)



-- INDENT LEVEL


type Level
    = Level String (() -> Level)


levelZero : Level
levelZero =
    Level "" (\_ -> makeLevel 1 (String.repeat 16 "\t"))


makeLevel : Int -> String -> Level
makeLevel level oldTabs =
    let
        tabs =
            if level <= String.length oldTabs then
                oldTabs

            else
                String.repeat (String.length oldTabs * 2) "\t"
    in
    Level (String.left level tabs) (\_ -> makeLevel (level + 1) tabs)



-- HELPERS


commaSep : List String -> String
commaSep builders =
    String.join ", " builders


commaNewlineSep : Level -> List String -> String
commaNewlineSep (Level _ nextLevel) builders =
    let
        (Level deeperIndent _) =
            nextLevel ()
    in
    String.join (",\n" ++ deeperIndent) builders



-- STATEMENTS


fromStmtBlock : Level -> List Stmt -> String
fromStmtBlock level stmts =
    String.concat (List.map (fromStmt level) stmts)


fromStmt : Level -> Stmt -> String
fromStmt ((Level indent nextLevel) as level) statement =
    case statement of
        Block stmts ->
            fromStmtBlock level stmts

        EmptyStmt ->
            ""

        ExprStmt expr ->
            indent ++ Tuple.second (fromExpr level Whatever expr) ++ ";\n"

        IfStmt condition thenStmt elseStmt ->
            indent
                ++ "if ("
                ++ Tuple.second (fromExpr level Whatever condition)
                ++ ") {\n"
                ++ fromStmt (nextLevel ()) thenStmt
                ++ indent
                ++ "} else {\n"
                ++ fromStmt (nextLevel ()) elseStmt
                ++ indent
                ++ "}\n"

        Switch expr clauses ->
            indent
                ++ "switch ("
                ++ Tuple.second (fromExpr level Whatever expr)
                ++ ") {\n"
                ++ String.concat (List.map (fromClause (nextLevel ())) clauses)
                ++ indent
                ++ "}\n"

        While expr stmt ->
            indent
                ++ "while ("
                ++ Tuple.second (fromExpr level Whatever expr)
                ++ ") {\n"
                ++ fromStmt (nextLevel ()) stmt
                ++ indent
                ++ "}\n"

        Break Nothing ->
            indent ++ "break;\n"

        Break (Just label) ->
            indent ++ "break " ++ label ++ ";\n"

        Continue Nothing ->
            indent ++ "continue;\n"

        Continue (Just label) ->
            indent ++ "continue " ++ label ++ ";\n"

        Labelled label stmt ->
            String.concat
                [ indent
                , label
                , ":\n"
                , fromStmt level stmt
                ]

        Try tryStmt errorName catchStmt ->
            indent
                ++ "try {\n"
                ++ fromStmt (nextLevel ()) tryStmt
                ++ indent
                ++ "} catch ("
                ++ errorName
                ++ ") {\n"
                ++ fromStmt (nextLevel ()) catchStmt
                ++ indent
                ++ "}\n"

        Throw expr ->
            indent ++ "throw " ++ Tuple.second (fromExpr level Whatever expr) ++ ";"

        Return expr ->
            indent ++ "return " ++ Tuple.second (fromExpr level Whatever expr) ++ ";\n"

        Var name expr ->
            indent ++ "var " ++ name ++ " = " ++ Tuple.second (fromExpr level Whatever expr) ++ ";\n"

        Vars [] ->
            ""

        Vars vars ->
            indent ++ "var " ++ commaNewlineSep level (List.map (varToBuilder level) vars) ++ ";\n"

        FunctionStmt name args stmts ->
            indent
                ++ "function "
                ++ name
                ++ "("
                ++ commaSep args
                ++ ") {\n"
                ++ fromStmtBlock (nextLevel ()) stmts
                ++ indent
                ++ "}\n"



-- SWITCH CLAUSES


fromClause : Level -> Case -> String
fromClause ((Level indent nextLevel) as level) clause =
    case clause of
        Case expr stmts ->
            indent
                ++ "case "
                ++ Tuple.second (fromExpr level Whatever expr)
                ++ ":\n"
                ++ fromStmtBlock (nextLevel ()) stmts

        Default stmts ->
            indent
                ++ "default:\n"
                ++ fromStmtBlock (nextLevel ()) stmts



-- VAR DECLS


varToBuilder : Level -> ( Name.Name, Expr ) -> String
varToBuilder level ( name, expr ) =
    name ++ " = " ++ Tuple.second (fromExpr level Whatever expr)



-- EXPRESSIONS


type Lines
    = One
    | Many


merge : Lines -> Lines -> Lines
merge a b =
    if a == Many || b == Many then
        Many

    else
        One


linesMap : (a -> ( Lines, b )) -> List a -> ( Bool, List b )
linesMap func xs =
    let
        pairs =
            List.map func xs
    in
    ( List.any ((==) Many << Tuple.first) pairs
    , List.map Tuple.second pairs
    )


type Grouping
    = Atomic
    | Whatever


parensFor : Grouping -> String -> String
parensFor grouping builder =
    case grouping of
        Atomic ->
            "(" ++ builder ++ ")"

        Whatever ->
            builder


fromExpr : Level -> Grouping -> Expr -> ( Lines, String )
fromExpr ((Level indent nextLevel) as level) grouping expression =
    let
        (Level deeperIndent _) =
            nextLevel ()
    in
    case expression of
        ExprString string ->
            ( One, "'" ++ string ++ "'" )

        ExprFloat float ->
            ( One, float )

        ExprInt n ->
            ( One, String.fromInt n )

        ExprBool bool ->
            ( One
            , if bool then
                "true"

              else
                "false"
            )

        ExprNull ->
            ( One, "null" )

        ExprJson json ->
            ( One, Json.encodeUgly json )

        ExprArray exprs ->
            let
                ( anyMany, builders ) =
                    linesMap (fromExpr level Whatever) exprs
            in
            ( Many
            , if anyMany then
                "[\n"
                    ++ deeperIndent
                    ++ commaNewlineSep level builders
                    ++ "\n"
                    ++ indent
                    ++ "]"

              else
                "[" ++ commaSep builders ++ "]"
            )

        ExprObject fields ->
            let
                ( anyMany, builders ) =
                    linesMap (fromField (nextLevel ())) fields
            in
            ( Many
            , if anyMany then
                "{\n"
                    ++ deeperIndent
                    ++ commaNewlineSep level builders
                    ++ "\n"
                    ++ indent
                    ++ "}"

              else
                "{" ++ commaSep builders ++ "}"
            )

        ExprRef name ->
            ( One, name )

        ExprAccess expr field ->
            makeDot level expr field

        ExprIndex expr bracketedExpr ->
            makeBracketed level expr bracketedExpr

        ExprPrefix op expr ->
            let
                ( lines, builder ) =
                    fromExpr level Atomic expr
            in
            ( lines
            , parensFor grouping (fromPrefix op ++ builder)
            )

        ExprInfix op leftExpr rightExpr ->
            let
                ( leftLines, left ) =
                    fromExpr level Atomic leftExpr

                ( rightLines, right ) =
                    fromExpr level Atomic rightExpr
            in
            ( merge leftLines rightLines
            , parensFor grouping (left ++ fromInfix op ++ right)
            )

        ExprIf condExpr thenExpr elseExpr ->
            let
                condB =
                    Tuple.second (fromExpr level Atomic condExpr)

                thenB =
                    Tuple.second (fromExpr level Atomic thenExpr)

                elseB =
                    Tuple.second (fromExpr level Atomic elseExpr)
            in
            ( Many
            , parensFor grouping (condB ++ " ? " ++ thenB ++ " : " ++ elseB)
            )

        ExprAssign lValue expr ->
            let
                ( leftLines, left ) =
                    fromLValue level lValue

                ( rightLines, right ) =
                    fromExpr level Whatever expr
            in
            ( merge leftLines rightLines
            , parensFor grouping (left ++ " = " ++ right)
            )

        ExprCall function args ->
            let
                ( _, funcB ) =
                    fromExpr level Atomic function

                ( anyMany, argsB ) =
                    linesMap (fromExpr (nextLevel ()) Whatever) args
            in
            ( Many
            , if anyMany then
                funcB ++ "(\n" ++ deeperIndent ++ commaNewlineSep level argsB ++ ")"

              else
                funcB ++ "(" ++ commaSep argsB ++ ")"
            )

        ExprFunction maybeName args stmts ->
            ( Many
            , "function "
                ++ Utils.maybe "" identity maybeName
                ++ "("
                ++ commaSep args
                ++ ") {\n"
                ++ fromStmtBlock (nextLevel ()) stmts
                ++ indent
                ++ "}"
            )



-- FIELDS


fromField : Level -> ( Name.Name, Expr ) -> ( Lines, String )
fromField level ( field, expr ) =
    let
        ( lines, builder ) =
            fromExpr level Whatever expr
    in
    ( lines
    , field ++ ": " ++ builder
    )



-- VALUES


fromLValue : Level -> LValue -> ( Lines, String )
fromLValue level lValue =
    case lValue of
        LRef name ->
            ( One, name )

        LDot expr field ->
            makeDot level expr field

        LBracket expr bracketedExpr ->
            makeBracketed level expr bracketedExpr


makeDot : Level -> Expr -> Name.Name -> ( Lines, String )
makeDot level expr field =
    let
        ( lines, builder ) =
            fromExpr level Atomic expr
    in
    ( lines, builder ++ "." ++ field )


makeBracketed : Level -> Expr -> Expr -> ( Lines, String )
makeBracketed level expr bracketedExpr =
    let
        ( lines, builder ) =
            fromExpr level Atomic expr

        ( bracketedLines, bracketedBuilder ) =
            fromExpr level Whatever bracketedExpr
    in
    ( merge lines bracketedLines
    , builder ++ "[" ++ bracketedBuilder ++ "]"
    )



-- OPERATORS


fromPrefix : PrefixOp -> String
fromPrefix op =
    case op of
        PrefixNot ->
            "!"

        PrefixNegate ->
            "-"

        PrefixComplement ->
            "~"


fromInfix : InfixOp -> String
fromInfix op =
    case op of
        OpAdd ->
            " + "

        OpSub ->
            " - "

        OpMul ->
            " * "

        OpDiv ->
            " / "

        OpMod ->
            " % "

        OpEq ->
            " === "

        OpNe ->
            " !== "

        OpLt ->
            " < "

        OpLe ->
            " <= "

        OpGt ->
            " > "

        OpGe ->
            " >= "

        OpAnd ->
            " && "

        OpOr ->
            " || "

        OpBitwiseAnd ->
            " & "

        OpBitwiseXor ->
            " ^ "

        OpBitwiseOr ->
            " | "

        OpLShift ->
            " << "

        OpSpRShift ->
            " >> "

        OpZfRShift ->
            " >>> "
