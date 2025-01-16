module Compiler.Generate.JavaScript.Builder exposing
    ( Builder(..)
    , Case(..)
    , Expr(..)
    , InfixOp(..)
    , LValue(..)
    , Mapping(..)
    , PrefixOp(..)
    , Stmt(..)
    , addByteString
    , emptyBuilder
    , exprToBuilder
    , stmtToBuilder
    )

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Compiler.Generate.JavaScript.Name as Name
import Compiler.Json.Encode as Json
import Compiler.Reporting.Annotation as A
import Maybe.Extra as Maybe
import System.TypeCheck.IO as IO



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
    | ExprTrackedString IO.Canonical A.Position String
    | ExprFloat String
    | ExprTrackedFloat IO.Canonical A.Position String
    | ExprInt Int
    | ExprTrackedInt IO.Canonical A.Position Int
    | ExprBool Bool
    | ExprTrackedBool IO.Canonical A.Position Bool
    | ExprJson Json.Value
    | ExprArray (List Expr)
    | ExprTrackedArray IO.Canonical A.Region (List Expr)
    | ExprObject (List ( Name.Name, Expr ))
    | ExprTrackedObject IO.Canonical A.Region (List ( A.Located Name.Name, Expr ))
    | ExprRef Name.Name
    | ExprTrackedRef IO.Canonical A.Position Name.Name Name.Name
    | ExprAccess Expr Name.Name
    | ExprTrackedAccess Expr IO.Canonical A.Position Name.Name
    | ExprIndex Expr Expr
    | ExprPrefix PrefixOp Expr
    | ExprInfix InfixOp Expr Expr
    | ExprIf Expr Expr Expr
    | ExprAssign LValue Expr
    | ExprCall Expr (List Expr)
    | ExprTrackedNormalCall IO.Canonical A.Position Expr Expr (List Expr)
    | ExprFunction (Maybe Name.Name) (List Name.Name) (List Stmt)
    | ExprTrackedFunction IO.Canonical (List (A.Located Name.Name)) (List Stmt)


type LValue
    = LRef Name.Name
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
    | TrackedVar IO.Canonical A.Position Name.Name Name.Name Expr
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



-- BUILDER


type Builder
    = Builder String Int Int (List Mapping)


type Mapping
    = Mapping Int Int IO.Canonical (Maybe Name.Name) Int Int


emptyBuilder : Int -> Builder
emptyBuilder currentLine =
    Builder "" currentLine 1 []


addAscii : String -> Builder -> Builder
addAscii ascii (Builder code currentLine currentCol mappings) =
    Builder (code ++ ascii) currentLine (currentCol + String.length ascii) mappings


addByteString : String -> Builder -> Builder
addByteString str (Builder code currentLine currentCol mappings) =
    let
        bsSize =
            String.length str

        bsLines =
            String.length (String.filter ((==) '\n') str)
    in
    if bsLines == 0 then
        Builder (code ++ str) currentLine (currentCol + bsSize) mappings

    else
        Builder (code ++ str) (currentLine + bsLines) 1 mappings


addTrackedByteString : IO.Canonical -> A.Position -> String -> Builder -> Builder
addTrackedByteString moduleName (A.Position line col) str (Builder code currentLine currentCol mappings) =
    let
        bsSize =
            String.length str

        bsLines =
            String.length (String.filter ((==) '\n') str)

        newMappings =
            Mapping line col moduleName Nothing currentLine currentCol
                :: mappings
    in
    if bsLines == 0 then
        Builder (code ++ str) currentLine (currentCol + bsSize) newMappings

    else
        Builder (code ++ str) (currentLine + bsLines) 1 newMappings


addName : IO.Canonical -> A.Position -> Name.Name -> Name.Name -> Builder -> Builder
addName moduleName (A.Position line col) name genName (Builder code currentLine currentCol mappings) =
    let
        size =
            String.length genName
    in
    Builder (code ++ genName)
        currentLine
        (currentCol + size)
        (Mapping line col moduleName (Just name) currentLine currentCol
            :: mappings
        )


addTrackedDot : IO.Canonical -> A.Position -> Builder -> Builder
addTrackedDot moduleName (A.Position line col) (Builder code currentLine currentCol mappings) =
    Builder (code ++ ".")
        currentLine
        (currentCol + 1)
        (Mapping line col moduleName Nothing currentLine currentCol
            :: mappings
        )


addLine : Builder -> Builder
addLine (Builder code currentLine _ mappings) =
    Builder (code ++ "\n") (currentLine + 1) 1 mappings



-- ENCODE


stmtToBuilder : Stmt -> Builder -> Builder
stmtToBuilder stmts builder =
    fromStmt levelZero stmts builder


exprToBuilder : Expr -> Builder -> Builder
exprToBuilder expr builder =
    fromExpr levelZero Whatever expr builder



-- INDENT LEVEL


type Level
    = Level String (() -> Level)


levelZero : Level
levelZero =
    Level "" (\_ -> makeLevel 1 (String.repeat 16 "\t"))


makeLevel : Int -> String -> Level
makeLevel level oldTabs =
    let
        tabs : String
        tabs =
            if level <= String.length oldTabs then
                oldTabs

            else
                String.repeat (String.length oldTabs * 2) "\t"
    in
    Level (String.left level tabs) (\_ -> makeLevel (level + 1) tabs)



-- STATEMENTS


fromStmtBlock : Level -> List Stmt -> Builder -> Builder
fromStmtBlock level stmts builder =
    List.foldl (fromStmt level) builder stmts


fromStmt : Level -> Stmt -> Builder -> Builder
fromStmt ((Level indent nextLevel) as level) statement builder =
    case statement of
        Block stmts ->
            fromStmtBlock level stmts builder

        EmptyStmt ->
            builder

        ExprStmt expr ->
            builder
                |> addByteString indent
                |> fromExpr level Whatever expr
                |> addAscii ";"
                |> addLine

        IfStmt condition thenStmt elseStmt ->
            builder
                |> addByteString indent
                |> addAscii "if ("
                |> fromExpr level Whatever condition
                |> addAscii ") {"
                |> addLine
                |> fromStmt (nextLevel ()) thenStmt
                |> addByteString indent
                |> addAscii "} else {"
                |> addLine
                |> fromStmt (nextLevel ()) elseStmt
                |> addByteString indent
                |> addAscii "}"
                |> addLine

        Switch expr clauses ->
            builder
                |> addByteString indent
                |> addAscii "switch ("
                |> fromExpr level Whatever expr
                |> addAscii ") {"
                |> addLine
                |> fromClauses (nextLevel ()) clauses
                |> addByteString indent
                |> addAscii "}"
                |> addLine

        While expr stmt ->
            builder
                |> addByteString indent
                |> addAscii "while ("
                |> fromExpr level Whatever expr
                |> addAscii ") {"
                |> addLine
                |> fromStmt (nextLevel ()) stmt
                |> addByteString indent
                |> addAscii "}"
                |> addLine

        Break Nothing ->
            builder
                |> addByteString indent
                |> addAscii "break;"
                |> addLine

        Break (Just label) ->
            builder
                |> addByteString indent
                |> addAscii "break "
                |> addByteString label
                |> addAscii ";"
                |> addLine

        Continue Nothing ->
            builder
                |> addByteString indent
                |> addAscii "continue;"
                |> addLine

        Continue (Just label) ->
            builder
                |> addByteString indent
                |> addAscii "continue "
                |> addByteString label
                |> addAscii ";"
                |> addLine

        Labelled label stmt ->
            builder
                |> addByteString indent
                |> addByteString label
                |> addAscii ":"
                |> addLine
                |> fromStmt level stmt

        Try tryStmt errorName catchStmt ->
            builder
                |> addByteString indent
                |> addAscii "try {"
                |> addLine
                |> fromStmt (nextLevel ()) tryStmt
                |> addByteString indent
                |> addAscii "} catch ("
                |> addByteString errorName
                |> addAscii ") {"
                |> addLine
                |> fromStmt (nextLevel ()) catchStmt
                |> addByteString indent
                |> addAscii "}"
                |> addLine

        Throw expr ->
            builder
                |> addByteString indent
                |> addAscii "throw "
                |> fromExpr level Whatever expr
                |> addAscii ";"

        Return expr ->
            builder
                |> addByteString indent
                |> addAscii "return "
                |> fromExpr level Whatever expr
                |> addAscii ";"
                |> addLine

        Var name expr ->
            builder
                |> addByteString indent
                |> addAscii "var "
                |> addByteString name
                |> addAscii " = "
                |> fromExpr level Whatever expr
                |> addAscii ";"
                |> addLine

        TrackedVar moduleName pos name genName expr ->
            builder
                |> addByteString indent
                |> addAscii "var "
                |> addName moduleName pos name genName
                |> addAscii " = "
                |> fromExpr level Whatever expr
                |> addAscii ";"
                |> addLine

        Vars [] ->
            builder

        Vars vars ->
            builder
                |> addByteString indent
                |> addAscii "var "
                |> commaNewlineSepExpr level (varToBuilder level) vars
                |> addAscii ";"
                |> addLine

        FunctionStmt name args stmts ->
            builder
                |> addByteString indent
                |> addAscii "function "
                |> addByteString name
                |> addAscii "("
                |> commaSepExpr addByteString args
                |> addAscii ") {"
                |> addLine
                |> fromStmtBlock (nextLevel ()) stmts
                |> addByteString indent
                |> addAscii "}"
                |> addLine



-- SWITCH CLAUSES


fromClause : Level -> Case -> Builder -> Builder
fromClause ((Level indent nextLevel) as level) clause builder =
    case clause of
        Case expr stmts ->
            builder
                |> addByteString indent
                |> addAscii "case "
                |> fromExpr level Whatever expr
                |> addAscii ":"
                |> addLine
                |> fromStmtBlock (nextLevel ()) stmts

        Default stmts ->
            builder
                |> addByteString indent
                |> addAscii "default:"
                |> addLine
                |> fromStmtBlock (nextLevel ()) stmts


fromClauses : Level -> List Case -> Builder -> Builder
fromClauses level clauses builder =
    case clauses of
        [] ->
            builder

        first :: rest ->
            fromClauses level rest (fromClause level first builder)



-- VAR DECLS


varToBuilder : Level -> ( Name.Name, Expr ) -> Builder -> Builder
varToBuilder level ( name, expr ) builder =
    builder
        |> addByteString name
        |> addAscii " = "
        |> fromExpr level Whatever expr



-- EXPRESSIONS


commaSepExpr : (a -> Builder -> Builder) -> List a -> Builder -> Builder
commaSepExpr fn exprs builder =
    case exprs of
        [] ->
            builder

        [ first ] ->
            fn first builder

        first :: rest ->
            commaSepExpr fn rest (addAscii ", " (fn first builder))


commaNewlineSepExpr : Level -> (a -> Builder -> Builder) -> List a -> Builder -> Builder
commaNewlineSepExpr ((Level indent _) as level) fn exprs builder =
    case exprs of
        [] ->
            builder

        [ first ] ->
            fn first builder

        first :: rest ->
            commaNewlineSepExpr level fn rest (addByteString indent (addLine (addAscii "," (fn first builder))))


type Grouping
    = Atomic
    | Whatever


parensFor : Grouping -> Builder -> (Builder -> Builder) -> Builder
parensFor grouping builder fillContent =
    case grouping of
        Atomic ->
            builder
                |> addAscii "("
                |> fillContent
                |> addAscii ")"

        Whatever ->
            fillContent builder


fromExpr : Level -> Grouping -> Expr -> Builder -> Builder
fromExpr ((Level indent nextLevel) as level) grouping expression builder =
    case expression of
        ExprString string ->
            addByteString ("'" ++ string ++ "'") builder

        ExprTrackedString moduleName position string ->
            addTrackedByteString moduleName position ("'" ++ string ++ "'") builder

        ExprTrackedFloat moduleName position float ->
            addTrackedByteString moduleName position float builder

        ExprFloat float ->
            addByteString float builder

        ExprInt n ->
            addByteString (String.fromInt n) builder

        ExprTrackedInt moduleName position n ->
            addTrackedByteString moduleName position (String.fromInt n) builder

        ExprBool bool ->
            addAscii
                (if bool then
                    "true"

                 else
                    "false"
                )
                builder

        ExprTrackedBool moduleName position bool ->
            addTrackedByteString moduleName
                position
                (if bool then
                    "true"

                 else
                    "false"
                )
                builder

        ExprJson json ->
            addAscii (Json.encodeUgly json) builder

        ExprArray exprs ->
            builder
                |> addAscii "[ "
                |> commaSepExpr (fromExpr level Whatever) exprs
                |> addAscii " ]"

        ExprTrackedArray moduleName (A.Region start (A.Position endLine endCol)) exprs ->
            builder
                |> addTrackedByteString moduleName start "[ "
                |> commaSepExpr (fromExpr level Whatever) exprs
                |> addAscii " "
                |> addTrackedByteString moduleName (A.Position endLine (endCol - 1)) "]"

        ExprObject fields ->
            builder
                |> addAscii "{ "
                |> commaSepExpr (fromField level) fields
                |> addAscii " }"

        ExprTrackedObject moduleName (A.Region start (A.Position endLine endCol)) fields ->
            builder
                |> addTrackedByteString moduleName start "{ "
                |> commaSepExpr (trackedFromField level moduleName) fields
                |> addAscii " "
                |> addTrackedByteString moduleName (A.Position endLine (endCol - 1)) "}"

        ExprRef name ->
            addByteString name builder

        ExprTrackedRef position moduleName name generatedName ->
            addName position moduleName name generatedName builder

        ExprAccess expr field ->
            makeDot level expr field builder

        ExprTrackedAccess expr moduleName ((A.Position fieldLine fieldCol) as position) field ->
            builder
                |> fromExpr level Atomic expr
                |> addTrackedDot moduleName (A.Position fieldLine (fieldCol - 1))
                |> addName moduleName position field field

        ExprIndex expr bracketedExpr ->
            makeBracketed level expr bracketedExpr builder

        ExprPrefix op expr ->
            parensFor grouping builder <|
                (fromPrefix op
                    >> fromExpr level Atomic expr
                )

        ExprInfix op leftExpr rightExpr ->
            parensFor grouping builder <|
                (fromExpr level Atomic leftExpr
                    >> fromInfix op
                    >> fromExpr level Atomic rightExpr
                )

        ExprIf condExpr thenExpr elseExpr ->
            parensFor grouping builder <|
                (fromExpr level Atomic condExpr
                    >> addAscii " ? "
                    >> fromExpr level Atomic thenExpr
                    >> addAscii " : "
                    >> fromExpr level Atomic elseExpr
                )

        ExprAssign lValue expr ->
            parensFor grouping builder <|
                (fromLValue level lValue
                    >> addAscii " = "
                    >> fromExpr level Whatever expr
                )

        ExprCall function args ->
            builder
                |> fromExpr level Atomic function
                |> addAscii "("
                |> commaSepExpr (fromExpr (nextLevel ()) Whatever) args
                |> addAscii ")"

        ExprTrackedNormalCall position moduleName helper function args ->
            let
                trackedHelper =
                    case ( trackedNameFromExpr function, helper ) of
                        ( Just functionName, ExprRef helperName ) ->
                            ExprTrackedRef position moduleName functionName helperName

                        _ ->
                            helper
            in
            builder
                |> fromExpr level Atomic trackedHelper
                |> addAscii "("
                |> commaSepExpr (fromExpr (nextLevel ()) Whatever) (function :: args)
                |> addAscii ")"

        ExprFunction maybeName args stmts ->
            builder
                |> addAscii "function "
                |> addByteString (Maybe.unwrap "" identity maybeName)
                |> addAscii "("
                |> commaSepExpr addByteString args
                |> addAscii ") {"
                |> addLine
                |> fromStmtBlock (nextLevel ()) stmts
                |> addByteString indent
                |> addAscii "}"

        ExprTrackedFunction moduleName args stmts ->
            builder
                |> addAscii "function"
                |> addAscii "("
                |> commaSepExpr (\(A.At (A.Region start _) name) -> addName moduleName start name name) args
                |> addAscii ") {"
                |> addLine
                |> fromStmtBlock (nextLevel ()) stmts
                |> addByteString indent
                |> addAscii "}"


trackedNameFromExpr : Expr -> Maybe Name.Name
trackedNameFromExpr expr =
    case expr of
        ExprTrackedRef _ _ name _ ->
            Just name

        _ ->
            Nothing



-- FIELDS


fromField : Level -> ( Name.Name, Expr ) -> Builder -> Builder
fromField level ( field, expr ) builder =
    builder
        |> addByteString field
        |> addAscii ": "
        |> fromExpr level Whatever expr


trackedFromField : Level -> IO.Canonical -> ( A.Located Name.Name, Expr ) -> Builder -> Builder
trackedFromField level moduleName ( A.At (A.Region start end) field, expr ) builder =
    builder
        |> addName moduleName start field field
        |> addTrackedByteString moduleName end ": "
        |> fromExpr level Whatever expr



-- VALUES


fromLValue : Level -> LValue -> Builder -> Builder
fromLValue level lValue builder =
    case lValue of
        LRef name ->
            addByteString name builder

        LBracket expr bracketedExpr ->
            makeBracketed level expr bracketedExpr builder


makeDot : Level -> Expr -> Name.Name -> Builder -> Builder
makeDot level expr field builder =
    builder
        |> fromExpr level Atomic expr
        |> addAscii "."
        |> addByteString field


makeBracketed : Level -> Expr -> Expr -> Builder -> Builder
makeBracketed level expr bracketedExpr builder =
    builder
        |> fromExpr level Atomic expr
        |> addAscii "["
        |> fromExpr level Whatever bracketedExpr
        |> addAscii "]"



-- OPERATORS


fromPrefix : PrefixOp -> Builder -> Builder
fromPrefix op =
    addAscii
        (case op of
            PrefixNot ->
                "!"

            PrefixNegate ->
                "-"

            PrefixComplement ->
                "~"
        )


fromInfix : InfixOp -> Builder -> Builder
fromInfix op =
    addAscii
        (case op of
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
        )
