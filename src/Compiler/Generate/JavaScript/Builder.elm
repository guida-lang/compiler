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
    , addKernel
    , emptyBuilder
    , exprToBuilder
    , stmtToBuilder
    )

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Basics.Extra exposing (flip)
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
    = Builder (List String) String Int Int (List Mapping)


type Mapping
    = Mapping Int Int IO.Canonical (Maybe Name.Name) Int Int


emptyBuilder : Int -> Builder
emptyBuilder currentLine =
    Builder [] "" currentLine 1 []


addAscii : String -> Builder -> Builder
addAscii ascii (Builder revKernels revBuilders currentLine currentCol mappings) =
    Builder revKernels (revBuilders ++ ascii) currentLine (currentCol + String.length ascii) mappings


addKernel : String -> Builder -> Builder
addKernel kernel (Builder revKernels revBuilders currentLine currentCol mappings) =
    Builder (kernel :: revKernels) revBuilders currentLine currentCol mappings


addByteString : String -> Builder -> Builder
addByteString str (Builder revKernels revBuilders currentLine currentCol mappings) =
    let
        bsLines : Int
        bsLines =
            String.length (String.filter ((==) '\n') str)
    in
    if bsLines == 0 then
        let
            bsSize : Int
            bsSize =
                String.length str
        in
        Builder revKernels (revBuilders ++ str) currentLine (currentCol + bsSize) mappings

    else
        Builder revKernels (revBuilders ++ str) (currentLine + bsLines) 1 mappings


addTrackedByteString : IO.Canonical -> A.Position -> String -> Builder -> Builder
addTrackedByteString moduleName (A.Position line col) str (Builder revKernels revBuilders currentLine currentCol mappings) =
    let
        bsLines : Int
        bsLines =
            String.length (String.filter ((==) '\n') str)

        newMappings : List Mapping
        newMappings =
            Mapping line col moduleName Nothing currentLine currentCol
                :: mappings
    in
    if bsLines == 0 then
        let
            bsSize : Int
            bsSize =
                String.length str
        in
        Builder revKernels (revBuilders ++ str) currentLine (currentCol + bsSize) newMappings

    else
        Builder revKernels (revBuilders ++ str) (currentLine + bsLines) 1 newMappings


addName : IO.Canonical -> A.Position -> Name.Name -> Name.Name -> Builder -> Builder
addName moduleName (A.Position line col) name genName (Builder revKernels revBuilders currentLine currentCol mappings) =
    let
        size : Int
        size =
            String.length genName
    in
    Builder revKernels
        (revBuilders ++ genName)
        currentLine
        (currentCol + size)
        (Mapping line col moduleName (Just name) currentLine currentCol
            :: mappings
        )


addTrackedDot : IO.Canonical -> A.Position -> Builder -> Builder
addTrackedDot moduleName (A.Position line col) (Builder revKernels revBuilders currentLine currentCol mappings) =
    Builder revKernels
        (revBuilders ++ ".")
        currentLine
        (currentCol + 1)
        (Mapping line col moduleName Nothing currentLine currentCol
            :: mappings
        )


addLine : Builder -> Builder
addLine (Builder revKernels revBuilders currentLine _ mappings) =
    Builder revKernels (revBuilders ++ "\n") (currentLine + 1) 1 mappings



-- ENCODE


stmtToBuilder : Stmt -> Builder -> Builder
stmtToBuilder stmts builder =
    fromStmt levelZero stmts builder


exprToBuilder : Expr -> Builder -> Builder
exprToBuilder expr builder =
    Tuple.second (fromExpr levelZero Whatever expr builder)



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
                |> Tuple.second
                |> addAscii ";"
                |> addLine

        IfStmt condition thenStmt elseStmt ->
            builder
                |> addByteString indent
                |> addAscii "if ("
                |> fromExpr level Whatever condition
                |> Tuple.second
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
                |> Tuple.second
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
                |> Tuple.second
                |> addAscii ") {"
                |> addLine
                |> fromStmt (nextLevel ()) stmt
                |> addByteString indent
                |> addAscii "}"
                |> addLine

        Break Nothing ->
            builder
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
                |> Tuple.second
                |> addAscii ";"

        Return expr ->
            builder
                |> addByteString indent
                |> addAscii "return "
                |> fromExpr level Whatever expr
                |> Tuple.second
                |> addAscii ";"
                |> addLine

        Var name expr ->
            builder
                |> addByteString indent
                |> addAscii "var "
                |> addByteString name
                |> addAscii " = "
                |> fromExpr level Whatever expr
                |> Tuple.second
                |> addAscii ";"
                |> addLine

        TrackedVar moduleName pos name genName expr ->
            builder
                |> addByteString indent
                |> addAscii "var "
                |> addName moduleName pos name genName
                |> addAscii " = "
                |> fromExpr level Whatever expr
                |> Tuple.second
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
                |> Tuple.second
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
        |> Tuple.second



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


linesMap : (a -> Builder -> ( Lines, Builder )) -> List a -> Bool
linesMap func xs =
    List.foldl
        (\a ( lines, builder ) -> func a builder |> Tuple.mapFirst (flip (::) lines))
        ( [], emptyBuilder 0 )
        xs
        |> Tuple.first
        |> List.member Many


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
commaNewlineSepExpr ((Level _ nextLevel) as level) fn exprs builder =
    case exprs of
        [] ->
            builder

        [ first ] ->
            fn first builder

        first :: rest ->
            let
                (Level deeperIndent _) =
                    nextLevel ()
            in
            commaNewlineSepExpr level fn rest (addByteString deeperIndent (addLine (addAscii "," (fn first builder))))


type Grouping
    = Atomic
    | Whatever


parensFor : Grouping -> Builder -> (Builder -> ( Lines, Builder )) -> ( Lines, Builder )
parensFor grouping builder fillContent =
    case grouping of
        Atomic ->
            builder
                |> addAscii "("
                |> fillContent
                |> Tuple.mapSecond (addAscii ")")

        Whatever ->
            fillContent builder


fromExpr : Level -> Grouping -> Expr -> Builder -> ( Lines, Builder )
fromExpr ((Level indent nextLevel) as level) grouping expression builder =
    let
        (Level deeperIndent _) =
            nextLevel ()
    in
    case expression of
        ExprString string ->
            ( One, addByteString ("'" ++ string ++ "'") builder )

        ExprTrackedString moduleName position string ->
            ( One, addTrackedByteString moduleName position ("'" ++ string ++ "'") builder )

        ExprFloat float ->
            ( One, addByteString float builder )

        ExprTrackedFloat moduleName position float ->
            ( One, addTrackedByteString moduleName position float builder )

        ExprInt n ->
            ( One, addByteString (String.fromInt n) builder )

        ExprTrackedInt moduleName position n ->
            ( One, addTrackedByteString moduleName position (String.fromInt n) builder )

        ExprBool bool ->
            ( One
            , addAscii
                (if bool then
                    "true"

                 else
                    "false"
                )
                builder
            )

        ExprTrackedBool moduleName position bool ->
            ( One
            , addTrackedByteString moduleName
                position
                (if bool then
                    "true"

                 else
                    "false"
                )
                builder
            )

        ExprJson json ->
            ( One, addAscii (Json.encodeUgly json) builder )

        ExprArray exprs ->
            let
                anyMany : Bool
                anyMany =
                    linesMap (fromExpr level Whatever) exprs
            in
            ( Many
            , if anyMany then
                builder
                    |> addAscii "["
                    |> addLine
                    |> addByteString deeperIndent
                    |> commaNewlineSepExpr level (\expr -> Tuple.second << fromExpr level Whatever expr) exprs
                    |> addLine
                    |> addByteString indent
                    |> addAscii "]"

              else
                builder
                    |> addAscii "["
                    |> commaSepExpr (\expr -> Tuple.second << fromExpr level Whatever expr) exprs
                    |> addAscii "]"
            )

        ExprTrackedArray moduleName (A.Region start (A.Position endLine endCol)) exprs ->
            let
                anyMany : Bool
                anyMany =
                    linesMap (fromExpr level Whatever) exprs
            in
            ( Many
            , if anyMany then
                builder
                    |> addTrackedByteString moduleName start "["
                    |> addLine
                    |> addByteString deeperIndent
                    |> commaNewlineSepExpr level (\expr -> Tuple.second << fromExpr level Whatever expr) exprs
                    |> addLine
                    |> addByteString indent
                    |> addTrackedByteString moduleName (A.Position endLine (endCol - 1)) "]"

              else
                builder
                    |> addTrackedByteString moduleName start "["
                    |> commaSepExpr (\expr -> Tuple.second << fromExpr level Whatever expr) exprs
                    |> addTrackedByteString moduleName (A.Position endLine (endCol - 1)) "]"
            )

        ExprObject fields ->
            let
                anyMany : Bool
                anyMany =
                    linesMap (fromField (nextLevel ())) fields
            in
            ( Many
            , if anyMany then
                builder
                    |> addAscii "{"
                    |> addLine
                    |> addByteString deeperIndent
                    |> commaNewlineSepExpr level (\field -> Tuple.second << fromField (nextLevel ()) field) fields
                    |> addLine
                    |> addByteString indent
                    |> addAscii "}"

              else
                builder
                    |> addAscii "{"
                    |> commaSepExpr (\field -> Tuple.second << fromField (nextLevel ()) field) fields
                    |> addAscii "}"
            )

        ExprTrackedObject moduleName (A.Region start (A.Position endLine endCol)) fields ->
            let
                anyMany : Bool
                anyMany =
                    linesMap (trackedFromField (nextLevel ()) moduleName) fields
            in
            ( Many
            , if anyMany then
                builder
                    |> addTrackedByteString moduleName start "{"
                    |> addLine
                    |> addByteString deeperIndent
                    |> commaNewlineSepExpr level (\field -> Tuple.second << trackedFromField (nextLevel ()) moduleName field) fields
                    |> addLine
                    |> addByteString indent
                    |> addTrackedByteString moduleName (A.Position endLine (endCol - 1)) "}"

              else
                builder
                    |> addTrackedByteString moduleName start "{"
                    |> commaSepExpr (\field -> Tuple.second << trackedFromField (nextLevel ()) moduleName field) fields
                    |> addTrackedByteString moduleName (A.Position endLine (endCol - 1)) "}"
            )

        ExprRef name ->
            ( One, addByteString name builder )

        ExprTrackedRef position moduleName name generatedName ->
            ( One, addName position moduleName name generatedName builder )

        ExprAccess expr field ->
            makeDot level expr field builder

        ExprTrackedAccess expr moduleName ((A.Position fieldLine fieldCol) as position) field ->
            builder
                |> fromExpr level Atomic expr
                |> Tuple.mapSecond
                    (addTrackedDot moduleName (A.Position fieldLine (fieldCol - 1))
                        >> addName moduleName position field field
                    )

        ExprIndex expr bracketedExpr ->
            makeBracketed level expr bracketedExpr builder

        ExprPrefix op expr ->
            parensFor grouping builder <|
                (fromPrefix op
                    >> fromExpr level Atomic expr
                )

        ExprInfix op leftExpr rightExpr ->
            parensFor grouping builder <|
                \b ->
                    let
                        ( leftLines, left ) =
                            fromExpr level Atomic leftExpr b
                    in
                    left
                        |> fromInfix op
                        |> fromExpr level Atomic rightExpr
                        |> Tuple.mapFirst (merge leftLines)

        ExprIf condExpr thenExpr elseExpr ->
            parensFor grouping builder <|
                fromExpr level Atomic condExpr
                    >> Tuple.second
                    >> addAscii " ? "
                    >> fromExpr level Atomic thenExpr
                    >> Tuple.second
                    >> addAscii " : "
                    >> fromExpr level Atomic elseExpr
                    >> Tuple.mapFirst (\_ -> Many)

        ExprAssign lValue expr ->
            parensFor grouping builder <|
                \b ->
                    let
                        ( leftLines, left ) =
                            fromLValue level lValue b
                    in
                    left
                        |> addAscii " = "
                        |> fromExpr level Whatever expr
                        |> Tuple.mapFirst (merge leftLines)

        ExprCall function args ->
            let
                anyMany : Bool
                anyMany =
                    linesMap (fromExpr (nextLevel ()) Whatever) args

                ( _, funcB ) =
                    fromExpr level Atomic function builder
            in
            ( Many
            , if anyMany then
                funcB
                    |> addAscii "("
                    |> addLine
                    |> addByteString deeperIndent
                    |> commaNewlineSepExpr level (\arg -> Tuple.second << fromExpr (nextLevel ()) Whatever arg) args
                    |> addAscii ")"

              else
                funcB
                    |> addAscii "("
                    |> commaSepExpr (\arg -> Tuple.second << fromExpr (nextLevel ()) Whatever arg) args
                    |> addAscii ")"
            )

        ExprTrackedNormalCall moduleName position helper function args ->
            let
                anyMany : Bool
                anyMany =
                    linesMap (fromExpr (nextLevel ()) Whatever) args

                trackedHelper : Expr
                trackedHelper =
                    case ( trackedNameFromExpr function, helper ) of
                        ( Just functionName, ExprRef helperName ) ->
                            ExprTrackedRef moduleName position functionName helperName

                        _ ->
                            helper

                ( _, funcB ) =
                    fromExpr level Atomic trackedHelper builder
            in
            ( Many
            , if anyMany then
                funcB
                    |> addAscii "("
                    |> addLine
                    |> addByteString deeperIndent
                    |> commaNewlineSepExpr level (\expr -> Tuple.second << fromExpr (nextLevel ()) Whatever expr) (function :: args)
                    |> addAscii ")"

              else
                funcB
                    |> addAscii "("
                    |> commaSepExpr (\expr -> Tuple.second << fromExpr (nextLevel ()) Whatever expr) (function :: args)
                    |> addAscii ")"
            )

        ExprFunction maybeName args stmts ->
            ( Many
            , builder
                |> addAscii "function "
                |> addByteString (Maybe.unwrap "" identity maybeName)
                |> addAscii "("
                |> commaSepExpr addByteString args
                |> addAscii ") {"
                |> addLine
                |> fromStmtBlock (nextLevel ()) stmts
                |> addByteString indent
                |> addAscii "}"
            )

        ExprTrackedFunction moduleName args stmts ->
            ( Many
            , builder
                |> addAscii "function "
                |> addAscii "("
                |> commaSepExpr (\(A.At (A.Region start _) name) -> addName moduleName start name name) args
                |> addAscii ") {"
                |> addLine
                |> fromStmtBlock (nextLevel ()) stmts
                |> addByteString indent
                |> addAscii "}"
            )


trackedNameFromExpr : Expr -> Maybe Name.Name
trackedNameFromExpr expr =
    case expr of
        ExprTrackedRef _ _ name _ ->
            Just name

        _ ->
            Nothing



-- FIELDS


fromField : Level -> ( Name.Name, Expr ) -> Builder -> ( Lines, Builder )
fromField level ( field, expr ) builder =
    builder
        |> addByteString field
        |> addAscii ": "
        |> fromExpr level Whatever expr


trackedFromField : Level -> IO.Canonical -> ( A.Located Name.Name, Expr ) -> Builder -> ( Lines, Builder )
trackedFromField level moduleName ( A.At (A.Region start end) field, expr ) builder =
    builder
        |> addName moduleName start field field
        |> addTrackedByteString moduleName end ": "
        |> fromExpr level Whatever expr



-- VALUES


fromLValue : Level -> LValue -> Builder -> ( Lines, Builder )
fromLValue level lValue builder =
    case lValue of
        LRef name ->
            ( One, addByteString name builder )

        LBracket expr bracketedExpr ->
            makeBracketed level expr bracketedExpr builder


makeDot : Level -> Expr -> Name.Name -> Builder -> ( Lines, Builder )
makeDot level expr field builder =
    builder
        |> fromExpr level Atomic expr
        |> Tuple.mapSecond (addAscii "." >> addByteString field)


makeBracketed : Level -> Expr -> Expr -> Builder -> ( Lines, Builder )
makeBracketed level expr bracketedExpr builder =
    let
        ( lines, newBuilder ) =
            fromExpr level Atomic expr builder
    in
    newBuilder
        |> addAscii "["
        |> fromExpr level Whatever bracketedExpr
        |> Tuple.mapBoth (merge lines) (addAscii "]")



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
