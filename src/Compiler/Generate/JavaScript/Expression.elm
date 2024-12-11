module Compiler.Generate.JavaScript.Expression exposing
    ( Code
    , codeToExpr
    , codeToStmtList
    , generate
    , generateCtor
    , generateField
    , generateMain
    , generateTailDef
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Type as Type
import Compiler.Elm.Compiler.Type.Extract as Extract
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Generate.JavaScript.Builder as JS
import Compiler.Generate.JavaScript.Name as JsName
import Compiler.Generate.Mode as Mode
import Compiler.Json.Encode as Encode
import Compiler.Optimize.DecisionTree as DT
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import Prelude
import System.TypeCheck.IO as IO
import Utils.Crash exposing (crash)
import Utils.Main as Utils


generateJsExpr : Mode.Mode -> Opt.Expr -> JS.Expr
generateJsExpr mode expression =
    codeToExpr (generate mode expression)


generate : Mode.Mode -> Opt.Expr -> Code
generate mode expression =
    case expression of
        Opt.Bool bool ->
            JsExpr <| JS.ExprBool bool

        Opt.Chr char ->
            JsExpr <|
                case mode of
                    Mode.Dev _ ->
                        JS.ExprCall toChar [ JS.ExprString char ]

                    Mode.Prod _ ->
                        JS.ExprString char

        Opt.Str string ->
            JsExpr <| JS.ExprString string

        Opt.Int int ->
            JsExpr <| JS.ExprInt int

        Opt.Float float ->
            JsExpr <|
                JS.ExprFloat
                    (if float == toFloat (floor float) then
                        String.fromFloat float ++ ".0"

                     else
                        String.fromFloat float
                    )

        Opt.VarLocal name ->
            JsExpr <| JS.ExprRef (JsName.fromLocal name)

        Opt.VarGlobal (Opt.Global home name) ->
            JsExpr <| JS.ExprRef (JsName.fromGlobal home name)

        Opt.VarEnum (Opt.Global home name) index ->
            case mode of
                Mode.Dev _ ->
                    JsExpr <| JS.ExprRef (JsName.fromGlobal home name)

                Mode.Prod _ ->
                    JsExpr <| JS.ExprInt (Index.toMachine index)

        Opt.VarBox (Opt.Global home name) ->
            JsExpr <|
                JS.ExprRef <|
                    case mode of
                        Mode.Dev _ ->
                            JsName.fromGlobal home name

                        Mode.Prod _ ->
                            JsName.fromGlobal ModuleName.basics Name.identity_

        Opt.VarCycle home name ->
            JsExpr <| JS.ExprCall (JS.ExprRef (JsName.fromCycle home name)) []

        Opt.VarDebug name home region unhandledValueName ->
            JsExpr <| generateDebug name home region unhandledValueName

        Opt.VarKernel home name ->
            JsExpr <| JS.ExprRef (JsName.fromKernel home name)

        Opt.List entries ->
            case entries of
                [] ->
                    JsExpr <| JS.ExprRef (JsName.fromKernel Name.list "Nil")

                _ ->
                    JsExpr <|
                        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.list "fromArray"))
                            [ JS.ExprArray <| List.map (generateJsExpr mode) entries
                            ]

        Opt.Function args body ->
            generateFunction (List.map JsName.fromLocal args) (generate mode body)

        Opt.Call func args ->
            JsExpr <| generateCall mode func args

        Opt.TailCall name args ->
            JsBlock <| generateTailCall mode name args

        Opt.If branches final ->
            generateIf mode branches final

        Opt.Let def body ->
            JsBlock <| generateDef mode def :: codeToStmtList (generate mode body)

        Opt.Destruct (Opt.Destructor name path) body ->
            let
                pathDef : JS.Stmt
                pathDef =
                    JS.Var (JsName.fromLocal name) (generatePath mode path)
            in
            JsBlock <| pathDef :: codeToStmtList (generate mode body)

        Opt.Case label root decider jumps ->
            JsBlock <| generateCase mode label root decider jumps

        Opt.Accessor field ->
            JsExpr <|
                JS.ExprFunction Nothing
                    [ JsName.dollar ]
                    [ JS.Return <|
                        JS.ExprAccess (JS.ExprRef JsName.dollar) (generateField mode field)
                    ]

        Opt.Access record field ->
            JsExpr <| JS.ExprAccess (generateJsExpr mode record) (generateField mode field)

        Opt.Update record fields ->
            JsExpr <|
                JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "update"))
                    [ generateJsExpr mode record
                    , generateRecord mode fields
                    ]

        Opt.Record fields ->
            JsExpr <| generateRecord mode fields

        Opt.Unit ->
            case mode of
                Mode.Dev _ ->
                    JsExpr <| JS.ExprRef (JsName.fromKernel Name.utils "Tuple0")

                Mode.Prod _ ->
                    JsExpr <| JS.ExprInt 0

        Opt.Tuple a b maybeC ->
            JsExpr <|
                case maybeC of
                    Nothing ->
                        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "Tuple2"))
                            [ generateJsExpr mode a
                            , generateJsExpr mode b
                            ]

                    Just c ->
                        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "Tuple3"))
                            [ generateJsExpr mode a
                            , generateJsExpr mode b
                            , generateJsExpr mode c
                            ]

        Opt.Shader src attributes uniforms ->
            let
                toTranlation : Name.Name -> ( JsName.Name, JS.Expr )
                toTranlation field =
                    ( JsName.fromLocal field
                    , JS.ExprString (generateField mode field)
                    )

                toTranslationObject : EverySet.EverySet String Name.Name -> JS.Expr
                toTranslationObject fields =
                    JS.ExprObject (List.map toTranlation (EverySet.toList compare fields))
            in
            JsExpr <|
                JS.ExprObject
                    [ ( JsName.fromLocal "src", JS.ExprString (Shader.toJsStringBuilder src) )
                    , ( JsName.fromLocal "attributes", toTranslationObject attributes )
                    , ( JsName.fromLocal "uniforms", toTranslationObject uniforms )
                    ]



-- CODE CHUNKS


type Code
    = JsExpr JS.Expr
    | JsBlock (List JS.Stmt)


codeToExpr : Code -> JS.Expr
codeToExpr code =
    case code of
        JsExpr expr ->
            expr

        JsBlock [ JS.Return expr ] ->
            expr

        JsBlock stmts ->
            JS.ExprCall (JS.ExprFunction Nothing [] stmts) []


codeToStmtList : Code -> List JS.Stmt
codeToStmtList code =
    case code of
        JsExpr (JS.ExprCall (JS.ExprFunction Nothing [] stmts) []) ->
            stmts

        JsExpr expr ->
            [ JS.Return expr ]

        JsBlock stmts ->
            stmts


codeToStmt : Code -> JS.Stmt
codeToStmt code =
    case code of
        JsExpr (JS.ExprCall (JS.ExprFunction Nothing [] stmts) []) ->
            JS.Block stmts

        JsExpr expr ->
            JS.Return expr

        JsBlock [ stmt ] ->
            stmt

        JsBlock stmts ->
            JS.Block stmts



-- CHARS


toChar : JS.Expr
toChar =
    JS.ExprRef (JsName.fromKernel Name.utils "chr")



-- CTOR


generateCtor : Mode.Mode -> Opt.Global -> Index.ZeroBased -> Int -> Code
generateCtor mode (Opt.Global home name) index arity =
    let
        argNames : List JsName.Name
        argNames =
            Index.indexedMap (\i _ -> JsName.fromIndex i) (List.range 1 arity)

        ctorTag : JS.Expr
        ctorTag =
            case mode of
                Mode.Dev _ ->
                    JS.ExprString name

                Mode.Prod _ ->
                    JS.ExprInt (ctorToInt home name index)
    in
    generateFunction argNames <|
        JsExpr <|
            JS.ExprObject
                (( JsName.dollar, ctorTag ) :: List.map (\n -> ( n, JS.ExprRef n )) argNames)


ctorToInt : IO.Canonical -> Name.Name -> Index.ZeroBased -> Int
ctorToInt home name index =
    if home == ModuleName.dict && (name == "RBNode_elm_builtin" || name == "RBEmpty_elm_builtin") then
        -(Index.toHuman index)

    else
        Index.toMachine index



-- RECORDS


generateRecord : Mode.Mode -> Dict String Name.Name Opt.Expr -> JS.Expr
generateRecord mode fields =
    let
        toPair : ( Name.Name, Opt.Expr ) -> ( JsName.Name, JS.Expr )
        toPair ( field, value ) =
            ( generateField mode field, generateJsExpr mode value )
    in
    JS.ExprObject (List.map toPair (Dict.toList compare fields))


generateField : Mode.Mode -> Name.Name -> JsName.Name
generateField mode name =
    case mode of
        Mode.Dev _ ->
            JsName.fromLocal name

        Mode.Prod fields ->
            Utils.find identity name fields



-- DEBUG


generateDebug : Name.Name -> IO.Canonical -> A.Region -> Maybe Name.Name -> JS.Expr
generateDebug name (IO.Canonical _ home) region unhandledValueName =
    if name /= "todo" then
        JS.ExprRef (JsName.fromGlobal ModuleName.debug name)

    else
        case unhandledValueName of
            Nothing ->
                JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.debug "todo"))
                    [ JS.ExprString home
                    , regionToJsExpr region
                    ]

            Just valueName ->
                JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.debug "todoCase"))
                    [ JS.ExprString home
                    , regionToJsExpr region
                    , JS.ExprRef (JsName.fromLocal valueName)
                    ]


regionToJsExpr : A.Region -> JS.Expr
regionToJsExpr (A.Region start end) =
    JS.ExprObject
        [ ( JsName.fromLocal "start", positionToJsExpr start )
        , ( JsName.fromLocal "end", positionToJsExpr end )
        ]


positionToJsExpr : A.Position -> JS.Expr
positionToJsExpr (A.Position line column) =
    JS.ExprObject
        [ ( JsName.fromLocal "line", JS.ExprInt line )
        , ( JsName.fromLocal "column", JS.ExprInt column )
        ]



-- FUNCTION


generateFunction : List JsName.Name -> Code -> Code
generateFunction args body =
    case Dict.get identity (List.length args) funcHelpers of
        Just helper ->
            JsExpr <|
                JS.ExprCall helper
                    [ JS.ExprFunction Nothing args <|
                        codeToStmtList body
                    ]

        Nothing ->
            let
                addArg : JsName.Name -> Code -> Code
                addArg arg code =
                    JsExpr <|
                        JS.ExprFunction Nothing [ arg ] <|
                            codeToStmtList code
            in
            List.foldr addArg body args


funcHelpers : Dict Int Int JS.Expr
funcHelpers =
    Dict.fromList identity <|
        List.map (\n -> ( n, JS.ExprRef (JsName.makeF n) )) (List.range 2 9)



-- CALLS


generateCall : Mode.Mode -> Opt.Expr -> List Opt.Expr -> JS.Expr
generateCall mode func args =
    case func of
        Opt.VarGlobal ((Opt.Global (IO.Canonical pkg _) _) as global) ->
            if pkg == Pkg.core then
                generateCoreCall mode global args

            else
                generateCallHelp mode func args

        Opt.VarBox _ ->
            case mode of
                Mode.Dev _ ->
                    generateCallHelp mode func args

                Mode.Prod _ ->
                    case args of
                        [ arg ] ->
                            generateJsExpr mode arg

                        _ ->
                            generateCallHelp mode func args

        _ ->
            generateCallHelp mode func args


generateCallHelp : Mode.Mode -> Opt.Expr -> List Opt.Expr -> JS.Expr
generateCallHelp mode func args =
    generateNormalCall
        (generateJsExpr mode func)
        (List.map (generateJsExpr mode) args)


generateGlobalCall : IO.Canonical -> Name.Name -> List JS.Expr -> JS.Expr
generateGlobalCall home name args =
    generateNormalCall (JS.ExprRef (JsName.fromGlobal home name)) args


generateNormalCall : JS.Expr -> List JS.Expr -> JS.Expr
generateNormalCall func args =
    case Dict.get identity (List.length args) callHelpers of
        Just helper ->
            JS.ExprCall helper (func :: args)

        Nothing ->
            List.foldl (\a f -> JS.ExprCall f [ a ]) func args


callHelpers : Dict Int Int JS.Expr
callHelpers =
    Dict.fromList identity <|
        List.map (\n -> ( n, JS.ExprRef (JsName.makeA n) )) (List.range 2 9)



-- CORE CALLS


generateCoreCall : Mode.Mode -> Opt.Global -> List Opt.Expr -> JS.Expr
generateCoreCall mode (Opt.Global ((IO.Canonical _ moduleName) as home) name) args =
    if moduleName == Name.basics then
        generateBasicsCall mode home name args

    else if moduleName == Name.bitwise then
        generateBitwiseCall home name (List.map (generateJsExpr mode) args)

    else if moduleName == Name.tuple then
        generateTupleCall home name (List.map (generateJsExpr mode) args)

    else if moduleName == Name.jsArray then
        generateJsArrayCall home name (List.map (generateJsExpr mode) args)

    else
        generateGlobalCall home name (List.map (generateJsExpr mode) args)


generateTupleCall : IO.Canonical -> Name.Name -> List JS.Expr -> JS.Expr
generateTupleCall home name args =
    case args of
        [ value ] ->
            case name of
                "first" ->
                    JS.ExprAccess value (JsName.fromLocal "a")

                "second" ->
                    JS.ExprAccess value (JsName.fromLocal "b")

                _ ->
                    generateGlobalCall home name args

        _ ->
            generateGlobalCall home name args


generateJsArrayCall : IO.Canonical -> Name.Name -> List JS.Expr -> JS.Expr
generateJsArrayCall home name args =
    case ( args, name ) of
        ( [ entry ], "singleton" ) ->
            JS.ExprArray [ entry ]

        ( [ index, array ], "unsafeGet" ) ->
            JS.ExprIndex array index

        _ ->
            generateGlobalCall home name args


generateBitwiseCall : IO.Canonical -> Name.Name -> List JS.Expr -> JS.Expr
generateBitwiseCall home name args =
    case args of
        [ arg ] ->
            case name of
                "complement" ->
                    JS.ExprPrefix JS.PrefixComplement arg

                _ ->
                    generateGlobalCall home name args

        [ left, right ] ->
            case name of
                "and" ->
                    JS.ExprInfix JS.OpBitwiseAnd left right

                "or" ->
                    JS.ExprInfix JS.OpBitwiseOr left right

                "xor" ->
                    JS.ExprInfix JS.OpBitwiseXor left right

                "shiftLeftBy" ->
                    JS.ExprInfix JS.OpLShift right left

                "shiftRightBy" ->
                    JS.ExprInfix JS.OpSpRShift right left

                "shiftRightZfBy" ->
                    JS.ExprInfix JS.OpZfRShift right left

                _ ->
                    generateGlobalCall home name args

        _ ->
            generateGlobalCall home name args


generateBasicsCall : Mode.Mode -> IO.Canonical -> Name.Name -> List Opt.Expr -> JS.Expr
generateBasicsCall mode home name args =
    case args of
        [ elmArg ] ->
            let
                arg : JS.Expr
                arg =
                    generateJsExpr mode elmArg
            in
            case name of
                "not" ->
                    JS.ExprPrefix JS.PrefixNot arg

                "negate" ->
                    JS.ExprPrefix JS.PrefixNegate arg

                "toFloat" ->
                    arg

                "truncate" ->
                    JS.ExprInfix JS.OpBitwiseOr arg (JS.ExprInt 0)

                _ ->
                    generateGlobalCall home name [ arg ]

        [ elmLeft, elmRight ] ->
            case name of
                -- NOTE: removed "composeL" and "composeR" because of this issue:
                -- https://github.com/elm/compiler/issues/1722
                "append" ->
                    append mode elmLeft elmRight

                "apL" ->
                    generateJsExpr mode <| apply elmLeft elmRight

                "apR" ->
                    generateJsExpr mode <| apply elmRight elmLeft

                _ ->
                    let
                        left : JS.Expr
                        left =
                            generateJsExpr mode elmLeft

                        right : JS.Expr
                        right =
                            generateJsExpr mode elmRight
                    in
                    case name of
                        "add" ->
                            JS.ExprInfix JS.OpAdd left right

                        "sub" ->
                            JS.ExprInfix JS.OpSub left right

                        "mul" ->
                            JS.ExprInfix JS.OpMul left right

                        "fdiv" ->
                            JS.ExprInfix JS.OpDiv left right

                        "idiv" ->
                            JS.ExprInfix JS.OpBitwiseOr (JS.ExprInfix JS.OpDiv left right) (JS.ExprInt 0)

                        "eq" ->
                            equal left right

                        "neq" ->
                            notEqual left right

                        "lt" ->
                            cmp JS.OpLt JS.OpLt 0 left right

                        "gt" ->
                            cmp JS.OpGt JS.OpGt 0 left right

                        "le" ->
                            cmp JS.OpLe JS.OpLt 1 left right

                        "ge" ->
                            cmp JS.OpGe JS.OpGt -1 left right

                        "or" ->
                            JS.ExprInfix JS.OpOr left right

                        "and" ->
                            JS.ExprInfix JS.OpAnd left right

                        "xor" ->
                            JS.ExprInfix JS.OpNe left right

                        "remainderBy" ->
                            JS.ExprInfix JS.OpMod right left

                        _ ->
                            generateGlobalCall home name [ left, right ]

        _ ->
            generateGlobalCall home name <| List.map (generateJsExpr mode) args


equal : JS.Expr -> JS.Expr -> JS.Expr
equal left right =
    if isLiteral left || isLiteral right then
        strictEq left right

    else
        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "eq")) [ left, right ]


notEqual : JS.Expr -> JS.Expr -> JS.Expr
notEqual left right =
    if isLiteral left || isLiteral right then
        strictNEq left right

    else
        JS.ExprPrefix JS.PrefixNot <| JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "eq")) [ left, right ]


cmp : JS.InfixOp -> JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> JS.Expr
cmp idealOp backupOp backupInt left right =
    if isLiteral left || isLiteral right then
        JS.ExprInfix idealOp left right

    else
        JS.ExprInfix backupOp
            (JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "cmp")) [ left, right ])
            (JS.ExprInt backupInt)


isLiteral : JS.Expr -> Bool
isLiteral expr =
    case expr of
        JS.ExprString _ ->
            True

        JS.ExprFloat _ ->
            True

        JS.ExprInt _ ->
            True

        JS.ExprBool _ ->
            True

        _ ->
            False


apply : Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
    case func of
        Opt.Accessor field ->
            Opt.Access value field

        Opt.Call f args ->
            Opt.Call f (args ++ [ value ])

        _ ->
            Opt.Call func [ value ]


append : Mode.Mode -> Opt.Expr -> Opt.Expr -> JS.Expr
append mode left right =
    let
        seqs : List JS.Expr
        seqs =
            generateJsExpr mode left :: toSeqs mode right
    in
    if List.any isStringLiteral seqs then
        Utils.foldr1 (JS.ExprInfix JS.OpAdd) seqs

    else
        Utils.foldr1 jsAppend seqs


jsAppend : JS.Expr -> JS.Expr -> JS.Expr
jsAppend a b =
    JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "ap")) [ a, b ]


toSeqs : Mode.Mode -> Opt.Expr -> List JS.Expr
toSeqs mode expr =
    case expr of
        Opt.Call (Opt.VarGlobal (Opt.Global home "append")) [ left, right ] ->
            if home == ModuleName.basics then
                generateJsExpr mode left :: toSeqs mode right

            else
                [ generateJsExpr mode expr ]

        _ ->
            [ generateJsExpr mode expr ]


isStringLiteral : JS.Expr -> Bool
isStringLiteral expr =
    case expr of
        JS.ExprString _ ->
            True

        _ ->
            False



-- SIMPLIFY INFIX OPERATORS


strictEq : JS.Expr -> JS.Expr -> JS.Expr
strictEq left right =
    case left of
        JS.ExprInt 0 ->
            JS.ExprPrefix JS.PrefixNot right

        JS.ExprBool bool ->
            if bool then
                right

            else
                JS.ExprPrefix JS.PrefixNot right

        _ ->
            case right of
                JS.ExprInt 0 ->
                    JS.ExprPrefix JS.PrefixNot left

                JS.ExprBool bool ->
                    if bool then
                        left

                    else
                        JS.ExprPrefix JS.PrefixNot left

                _ ->
                    JS.ExprInfix JS.OpEq left right


strictNEq : JS.Expr -> JS.Expr -> JS.Expr
strictNEq left right =
    case left of
        JS.ExprInt 0 ->
            JS.ExprPrefix JS.PrefixNot (JS.ExprPrefix JS.PrefixNot right)

        JS.ExprBool bool ->
            if bool then
                JS.ExprPrefix JS.PrefixNot right

            else
                right

        _ ->
            case right of
                JS.ExprInt 0 ->
                    JS.ExprPrefix JS.PrefixNot (JS.ExprPrefix JS.PrefixNot left)

                JS.ExprBool bool ->
                    if bool then
                        JS.ExprPrefix JS.PrefixNot left

                    else
                        left

                _ ->
                    JS.ExprInfix JS.OpNe left right



-- TAIL CALL


{-| TODO check if JS minifiers collapse unnecessary temporary variables
-}
generateTailCall : Mode.Mode -> Name.Name -> List ( Name.Name, Opt.Expr ) -> List JS.Stmt
generateTailCall mode name args =
    let
        toTempVars : ( String, Opt.Expr ) -> ( JsName.Name, JS.Expr )
        toTempVars ( argName, arg ) =
            ( JsName.makeTemp argName, generateJsExpr mode arg )

        toRealVars : ( Name.Name, b ) -> JS.Stmt
        toRealVars ( argName, _ ) =
            JS.ExprStmt <| JS.ExprAssign (JS.LRef (JsName.fromLocal argName)) (JS.ExprRef (JsName.makeTemp argName))
    in
    JS.Vars (List.map toTempVars args)
        :: List.map toRealVars args
        ++ [ JS.Continue (Just (JsName.fromLocal name)) ]



-- DEFINITIONS


generateDef : Mode.Mode -> Opt.Def -> JS.Stmt
generateDef mode def =
    case def of
        Opt.Def name body ->
            JS.Var (JsName.fromLocal name) (generateJsExpr mode body)

        Opt.TailDef name argNames body ->
            JS.Var (JsName.fromLocal name) (codeToExpr (generateTailDef mode name argNames body))


generateTailDef : Mode.Mode -> Name.Name -> List Name.Name -> Opt.Expr -> Code
generateTailDef mode name argNames body =
    generateFunction (List.map JsName.fromLocal argNames) <|
        JsBlock
            [ JS.Labelled (JsName.fromLocal name) <|
                JS.While (JS.ExprBool True) <|
                    codeToStmt <|
                        generate mode body
            ]



-- PATHS


generatePath : Mode.Mode -> Opt.Path -> JS.Expr
generatePath mode path =
    case path of
        Opt.Index index subPath ->
            JS.ExprAccess (generatePath mode subPath) (JsName.fromIndex index)

        Opt.Root name ->
            JS.ExprRef (JsName.fromLocal name)

        Opt.Field field subPath ->
            JS.ExprAccess (generatePath mode subPath) (generateField mode field)

        Opt.Unbox subPath ->
            case mode of
                Mode.Dev _ ->
                    JS.ExprAccess (generatePath mode subPath) (JsName.fromIndex Index.first)

                Mode.Prod _ ->
                    generatePath mode subPath



-- GENERATE IFS


generateIf : Mode.Mode -> List ( Opt.Expr, Opt.Expr ) -> Opt.Expr -> Code
generateIf mode givenBranches givenFinal =
    let
        ( branches, final ) =
            crushIfs givenBranches givenFinal

        convertBranch : ( Opt.Expr, Opt.Expr ) -> ( JS.Expr, Code )
        convertBranch ( condition, expr ) =
            ( generateJsExpr mode condition
            , generate mode expr
            )

        branchExprs : List ( JS.Expr, Code )
        branchExprs =
            List.map convertBranch branches

        finalCode : Code
        finalCode =
            generate mode final
    in
    if isBlock finalCode || List.any (isBlock << Tuple.second) branchExprs then
        JsBlock [ List.foldr addStmtIf (codeToStmt finalCode) branchExprs ]

    else
        JsExpr (List.foldr addExprIf (codeToExpr finalCode) branchExprs)


addExprIf : ( JS.Expr, Code ) -> JS.Expr -> JS.Expr
addExprIf ( condition, branch ) final =
    JS.ExprIf condition (codeToExpr branch) final


addStmtIf : ( JS.Expr, Code ) -> JS.Stmt -> JS.Stmt
addStmtIf ( condition, branch ) final =
    JS.IfStmt condition (codeToStmt branch) final


isBlock : Code -> Bool
isBlock code =
    case code of
        JsBlock _ ->
            True

        JsExpr _ ->
            False


crushIfs : List ( Opt.Expr, Opt.Expr ) -> Opt.Expr -> ( List ( Opt.Expr, Opt.Expr ), Opt.Expr )
crushIfs branches final =
    crushIfsHelp [] branches final


crushIfsHelp :
    List ( Opt.Expr, Opt.Expr )
    -> List ( Opt.Expr, Opt.Expr )
    -> Opt.Expr
    -> ( List ( Opt.Expr, Opt.Expr ), Opt.Expr )
crushIfsHelp visitedBranches unvisitedBranches final =
    case unvisitedBranches of
        [] ->
            case final of
                Opt.If subBranches subFinal ->
                    crushIfsHelp visitedBranches subBranches subFinal

                _ ->
                    ( List.reverse visitedBranches, final )

        visiting :: unvisited ->
            crushIfsHelp (visiting :: visitedBranches) unvisited final



-- CASE EXPRESSIONS


generateCase : Mode.Mode -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> List ( Int, Opt.Expr ) -> List JS.Stmt
generateCase mode label root decider jumps =
    List.foldr (goto mode label) (generateDecider mode label root decider) jumps


goto : Mode.Mode -> Name.Name -> ( Int, Opt.Expr ) -> List JS.Stmt -> List JS.Stmt
goto mode label ( index, branch ) stmts =
    let
        labeledDeciderStmt : JS.Stmt
        labeledDeciderStmt =
            JS.Labelled
                (JsName.makeLabel label index)
                (JS.While (JS.ExprBool True) (JS.Block stmts))
    in
    labeledDeciderStmt :: codeToStmtList (generate mode branch)


generateDecider : Mode.Mode -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> List JS.Stmt
generateDecider mode label root decisionTree =
    case decisionTree of
        Opt.Leaf (Opt.Inline branch) ->
            codeToStmtList (generate mode branch)

        Opt.Leaf (Opt.Jump index) ->
            [ JS.Break (Just (JsName.makeLabel label index)) ]

        Opt.Chain testChain success failure ->
            [ JS.IfStmt
                (Utils.foldl1_ (JS.ExprInfix JS.OpAnd) (List.map (generateIfTest mode root) testChain))
                (JS.Block (generateDecider mode label root success))
                (JS.Block (generateDecider mode label root failure))
            ]

        Opt.FanOut path edges fallback ->
            [ JS.Switch
                (generateCaseTest mode root path (Tuple.first (Prelude.head edges)))
                (List.foldr
                    (\edge cases -> generateCaseBranch mode label root edge :: cases)
                    [ JS.Default (generateDecider mode label root fallback) ]
                    edges
                )
            ]


generateIfTest : Mode.Mode -> Name.Name -> ( DT.Path, DT.Test ) -> JS.Expr
generateIfTest mode root ( path, test ) =
    let
        value : JS.Expr
        value =
            pathToJsExpr mode root path
    in
    case test of
        DT.IsCtor home name index _ opts ->
            let
                tag : JS.Expr
                tag =
                    case mode of
                        Mode.Dev _ ->
                            JS.ExprAccess value JsName.dollar

                        Mode.Prod _ ->
                            case opts of
                                Can.Normal ->
                                    JS.ExprAccess value JsName.dollar

                                Can.Enum ->
                                    value

                                Can.Unbox ->
                                    value
            in
            strictEq tag
                (case mode of
                    Mode.Dev _ ->
                        JS.ExprString name

                    Mode.Prod _ ->
                        JS.ExprInt (ctorToInt home name index)
                )

        DT.IsBool True ->
            value

        DT.IsBool False ->
            JS.ExprPrefix JS.PrefixNot value

        DT.IsInt int ->
            strictEq value (JS.ExprInt int)

        DT.IsChr char ->
            strictEq (JS.ExprString char)
                (case mode of
                    Mode.Dev _ ->
                        JS.ExprCall (JS.ExprAccess value (JsName.fromLocal "valueOf")) []

                    Mode.Prod _ ->
                        value
                )

        DT.IsStr string ->
            strictEq value (JS.ExprString string)

        DT.IsCons ->
            JS.ExprAccess value (JsName.fromLocal "b")

        DT.IsNil ->
            JS.ExprPrefix JS.PrefixNot <|
                JS.ExprAccess value (JsName.fromLocal "b")

        DT.IsTuple ->
            crash "COMPILER BUG - there should never be tests on a tuple"


generateCaseBranch : Mode.Mode -> Name.Name -> Name.Name -> ( DT.Test, Opt.Decider Opt.Choice ) -> JS.Case
generateCaseBranch mode label root ( test, subTree ) =
    JS.Case
        (generateCaseValue mode test)
        (generateDecider mode label root subTree)


generateCaseValue : Mode.Mode -> DT.Test -> JS.Expr
generateCaseValue mode test =
    case test of
        DT.IsCtor home name index _ _ ->
            case mode of
                Mode.Dev _ ->
                    JS.ExprString name

                Mode.Prod _ ->
                    JS.ExprInt (ctorToInt home name index)

        DT.IsInt int ->
            JS.ExprInt int

        DT.IsChr char ->
            JS.ExprString char

        DT.IsStr string ->
            JS.ExprString string

        DT.IsBool _ ->
            crash "COMPILER BUG - there should never be three tests on a boolean"

        DT.IsCons ->
            crash "COMPILER BUG - there should never be three tests on a list"

        DT.IsNil ->
            crash "COMPILER BUG - there should never be three tests on a list"

        DT.IsTuple ->
            crash "COMPILER BUG - there should never be three tests on a tuple"


generateCaseTest : Mode.Mode -> Name.Name -> DT.Path -> DT.Test -> JS.Expr
generateCaseTest mode root path exampleTest =
    let
        value : JS.Expr
        value =
            pathToJsExpr mode root path
    in
    case exampleTest of
        DT.IsCtor home name _ _ opts ->
            if name == Name.bool && home == ModuleName.basics then
                value

            else
                case mode of
                    Mode.Dev _ ->
                        JS.ExprAccess value JsName.dollar

                    Mode.Prod _ ->
                        case opts of
                            Can.Normal ->
                                JS.ExprAccess value JsName.dollar

                            Can.Enum ->
                                value

                            Can.Unbox ->
                                value

        DT.IsInt _ ->
            value

        DT.IsStr _ ->
            value

        DT.IsChr _ ->
            case mode of
                Mode.Dev _ ->
                    JS.ExprCall (JS.ExprAccess value (JsName.fromLocal "valueOf")) []

                Mode.Prod _ ->
                    value

        DT.IsBool _ ->
            crash "COMPILER BUG - there should never be three tests on a list"

        DT.IsCons ->
            crash "COMPILER BUG - there should never be three tests on a list"

        DT.IsNil ->
            crash "COMPILER BUG - there should never be three tests on a list"

        DT.IsTuple ->
            crash "COMPILER BUG - there should never be three tests on a list"



-- PATTERN PATHS


pathToJsExpr : Mode.Mode -> Name.Name -> DT.Path -> JS.Expr
pathToJsExpr mode root path =
    case path of
        DT.Index index subPath ->
            JS.ExprAccess (pathToJsExpr mode root subPath) (JsName.fromIndex index)

        DT.Unbox subPath ->
            case mode of
                Mode.Dev _ ->
                    JS.ExprAccess (pathToJsExpr mode root subPath) (JsName.fromIndex Index.first)

                Mode.Prod _ ->
                    pathToJsExpr mode root subPath

        DT.Empty ->
            JS.ExprRef (JsName.fromLocal root)



-- GENERATE MAIN


generateMain : Mode.Mode -> IO.Canonical -> Opt.Main -> JS.Expr
generateMain mode home main =
    case main of
        Opt.Static ->
            JS.ExprRef (JsName.fromKernel Name.virtualDom "init")
                |> call (JS.ExprRef (JsName.fromGlobal home "main"))
                |> call (JS.ExprInt 0)
                |> call (JS.ExprInt 0)

        Opt.Dynamic msgType decoder ->
            JS.ExprRef (JsName.fromGlobal home "main")
                |> call (generateJsExpr mode decoder)
                |> call (toDebugMetadata mode msgType)


call : JS.Expr -> JS.Expr -> JS.Expr
call arg func =
    JS.ExprCall func [ arg ]


toDebugMetadata : Mode.Mode -> Can.Type -> JS.Expr
toDebugMetadata mode msgType =
    case mode of
        Mode.Prod _ ->
            JS.ExprInt 0

        Mode.Dev Nothing ->
            JS.ExprInt 0

        Mode.Dev (Just interfaces) ->
            JS.ExprJson
                (Encode.object
                    [ ( "versions", Encode.object [ ( "elm", V.encode V.compiler ) ] )
                    , ( "types", Type.encodeMetadata (Extract.fromMsg interfaces msgType) )
                    ]
                )
