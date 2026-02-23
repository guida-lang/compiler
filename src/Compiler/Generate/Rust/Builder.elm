module Compiler.Generate.Rust.Builder exposing
    ( Builder
    , Expr(..)
    , Stmt(..)
    , emptyBuilder
    , stmtToBuilder
    , toString
    )


type Builder
    = Builder String


type Expr
    = ExprString String
    | ExprInt Int
    | ExprFloat String
    | ExprBool Bool
    | ExprRef String
    | ExprCall Expr (List Expr)
    | ExprIf Expr Expr Expr
    | ExprClosure (List String) Expr


type Stmt
    = Let String Expr
    | Comment String
    | Return Expr
    | ExprStmt Expr
    | Block (List Stmt)


emptyBuilder : Builder
emptyBuilder =
    Builder ""


toString : Builder -> String
toString (Builder code) =
    code


stmtToBuilder : Stmt -> Builder -> Builder
stmtToBuilder stmt builder =
    fromStmt 1 stmt builder


fromStmt : Int -> Stmt -> Builder -> Builder
fromStmt level stmt builder =
    case stmt of
        Let name expr ->
            addIndent level builder
                |> addString "let "
                |> addString name
                |> addString " = "
                |> addExpr expr
                |> addString ";\n"

        Comment value ->
            addIndent level builder
                |> addString "// "
                |> addString value
                |> addString "\n"

        Return expr ->
            addIndent level builder
                |> addExpr expr
                |> addString "\n"

        ExprStmt expr ->
            addIndent level builder
                |> addExpr expr
                |> addString ";\n"

        Block stmts ->
            List.foldl (fromStmt level) builder stmts


addExpr : Expr -> Builder -> Builder
addExpr expr builder =
    case expr of
        ExprString value ->
            addString ("\"" ++ value ++ "\"") builder

        ExprInt value ->
            addString (String.fromInt value) builder

        ExprFloat value ->
            addString value builder

        ExprBool value ->
            addString
                (if value then
                    "true"

                 else
                    "false"
                )
                builder

        ExprRef name ->
            addString name builder

        ExprCall fn args ->
            addExpr fn builder
                |> addString "("
                |> addArgs args
                |> addString ")"

        ExprIf condition thenExpr elseExpr ->
            builder
                |> addString "if "
                |> addExpr condition
                |> addString " { "
                |> addExpr thenExpr
                |> addString " } else { "
                |> addExpr elseExpr
                |> addString " }"

        ExprClosure args bodyExpr ->
            builder
                |> addString "|"
                |> addNames args
                |> addString "| { "
                |> addExpr bodyExpr
                |> addString " }"


addArgs : List Expr -> Builder -> Builder
addArgs args builder =
    case args of
        [] ->
            builder

        [ arg ] ->
            addExpr arg builder

        first :: rest ->
            addArgs rest (addString ", " (addExpr first builder))


addNames : List String -> Builder -> Builder
addNames names builder =
    case names of
        [] ->
            builder

        [ name ] ->
            addString name builder

        first :: rest ->
            addNames rest (addString ", " (addString first builder))


addIndent : Int -> Builder -> Builder
addIndent level builder =
    addString (String.repeat (4 * level) " ") builder


addString : String -> Builder -> Builder
addString str (Builder code) =
    Builder (code ++ str)
