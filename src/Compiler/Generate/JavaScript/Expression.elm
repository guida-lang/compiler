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
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import Prelude
import Types as T
import Utils.Crash exposing (crash)
import Utils.Main as Utils


generateJsExpr : Mode.Mode -> T.CASTO_Expr -> JS.Expr
generateJsExpr mode expression =
    codeToExpr (generate mode expression)


generate : Mode.Mode -> T.CASTO_Expr -> Code
generate mode expression =
    case expression of
        T.CASTO_Bool bool ->
            JsExpr <| JS.ExprBool bool

        T.CASTO_Chr char ->
            JsExpr <|
                case mode of
                    Mode.Dev _ ->
                        JS.ExprCall toChar [ JS.ExprString char ]

                    Mode.Prod _ ->
                        JS.ExprString char

        T.CASTO_Str string ->
            JsExpr <| JS.ExprString string

        T.CASTO_Int int ->
            JsExpr <| JS.ExprInt int

        T.CASTO_Float float ->
            JsExpr <|
                JS.ExprFloat
                    (if float == toFloat (floor float) then
                        String.fromFloat float ++ ".0"

                     else
                        String.fromFloat float
                    )

        T.CASTO_VarLocal name ->
            JsExpr <| JS.ExprRef (JsName.fromLocal name)

        T.CASTO_VarGlobal (T.CASTO_Global home name) ->
            JsExpr <| JS.ExprRef (JsName.fromGlobal home name)

        T.CASTO_VarEnum (T.CASTO_Global home name) index ->
            case mode of
                Mode.Dev _ ->
                    JsExpr <| JS.ExprRef (JsName.fromGlobal home name)

                Mode.Prod _ ->
                    JsExpr <| JS.ExprInt (Index.toMachine index)

        T.CASTO_VarBox (T.CASTO_Global home name) ->
            JsExpr <|
                JS.ExprRef <|
                    case mode of
                        Mode.Dev _ ->
                            JsName.fromGlobal home name

                        Mode.Prod _ ->
                            JsName.fromGlobal ModuleName.basics Name.identity_

        T.CASTO_VarCycle home name ->
            JsExpr <| JS.ExprCall (JS.ExprRef (JsName.fromCycle home name)) []

        T.CASTO_VarDebug name home region unhandledValueName ->
            JsExpr <| generateDebug name home region unhandledValueName

        T.CASTO_VarKernel home name ->
            JsExpr <| JS.ExprRef (JsName.fromKernel home name)

        T.CASTO_List entries ->
            case entries of
                [] ->
                    JsExpr <| JS.ExprRef (JsName.fromKernel Name.list "Nil")

                _ ->
                    JsExpr <|
                        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.list "fromArray"))
                            [ JS.ExprArray <| List.map (generateJsExpr mode) entries
                            ]

        T.CASTO_Function args body ->
            generateFunction (List.map JsName.fromLocal args) (generate mode body)

        T.CASTO_Call func args ->
            JsExpr <| generateCall mode func args

        T.CASTO_TailCall name args ->
            JsBlock <| generateTailCall mode name args

        T.CASTO_If branches final ->
            generateIf mode branches final

        T.CASTO_Let def body ->
            JsBlock <| generateDef mode def :: codeToStmtList (generate mode body)

        T.CASTO_Destruct (T.CASTO_Destructor name path) body ->
            let
                pathDef : JS.Stmt
                pathDef =
                    JS.Var (JsName.fromLocal name) (generatePath mode path)
            in
            JsBlock <| pathDef :: codeToStmtList (generate mode body)

        T.CASTO_Case label root decider jumps ->
            JsBlock <| generateCase mode label root decider jumps

        T.CASTO_Accessor field ->
            JsExpr <|
                JS.ExprFunction Nothing
                    [ JsName.dollar ]
                    [ JS.Return <|
                        JS.ExprAccess (JS.ExprRef JsName.dollar) (generateField mode field)
                    ]

        T.CASTO_Access record field ->
            JsExpr <| JS.ExprAccess (generateJsExpr mode record) (generateField mode field)

        T.CASTO_Update record fields ->
            JsExpr <|
                JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "update"))
                    [ generateJsExpr mode record
                    , generateRecord mode fields
                    ]

        T.CASTO_Record fields ->
            JsExpr <| generateRecord mode fields

        T.CASTO_Unit ->
            case mode of
                Mode.Dev _ ->
                    JsExpr <| JS.ExprRef (JsName.fromKernel Name.utils "Tuple0")

                Mode.Prod _ ->
                    JsExpr <| JS.ExprInt 0

        T.CASTO_Tuple a b maybeC ->
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

        T.CASTO_Shader src attributes uniforms ->
            let
                toTranlation : T.CDN_Name -> ( JsName.Name, JS.Expr )
                toTranlation field =
                    ( JsName.fromLocal field
                    , JS.ExprString (generateField mode field)
                    )

                toTranslationObject : EverySet.EverySet String T.CDN_Name -> JS.Expr
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


generateCtor : Mode.Mode -> T.CASTO_Global -> T.CDI_ZeroBased -> Int -> Code
generateCtor mode (T.CASTO_Global home name) index arity =
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


ctorToInt : T.CEMN_Canonical -> T.CDN_Name -> T.CDI_ZeroBased -> Int
ctorToInt home name index =
    if home == ModuleName.dict && (name == "RBNode_elm_builtin" || name == "RBEmpty_elm_builtin") then
        -(Index.toHuman index)

    else
        Index.toMachine index



-- RECORDS


generateRecord : Mode.Mode -> Dict String T.CDN_Name T.CASTO_Expr -> JS.Expr
generateRecord mode fields =
    let
        toPair : ( T.CDN_Name, T.CASTO_Expr ) -> ( JsName.Name, JS.Expr )
        toPair ( field, value ) =
            ( generateField mode field, generateJsExpr mode value )
    in
    JS.ExprObject (List.map toPair (Dict.toList compare fields))


generateField : Mode.Mode -> T.CDN_Name -> JsName.Name
generateField mode name =
    case mode of
        Mode.Dev _ ->
            JsName.fromLocal name

        Mode.Prod fields ->
            Utils.find identity name fields



-- DEBUG


generateDebug : T.CDN_Name -> T.CEMN_Canonical -> T.CRA_Region -> Maybe T.CDN_Name -> JS.Expr
generateDebug name (T.CEMN_Canonical _ home) region unhandledValueName =
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


regionToJsExpr : T.CRA_Region -> JS.Expr
regionToJsExpr (T.CRA_Region start end) =
    JS.ExprObject
        [ ( JsName.fromLocal "start", positionToJsExpr start )
        , ( JsName.fromLocal "end", positionToJsExpr end )
        ]


positionToJsExpr : T.CRA_Position -> JS.Expr
positionToJsExpr (T.CRA_Position line column) =
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


generateCall : Mode.Mode -> T.CASTO_Expr -> List T.CASTO_Expr -> JS.Expr
generateCall mode func args =
    case func of
        T.CASTO_VarGlobal ((T.CASTO_Global (T.CEMN_Canonical pkg _) _) as global) ->
            if pkg == Pkg.core then
                generateCoreCall mode global args

            else
                generateCallHelp mode func args

        T.CASTO_VarBox _ ->
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


generateCallHelp : Mode.Mode -> T.CASTO_Expr -> List T.CASTO_Expr -> JS.Expr
generateCallHelp mode func args =
    generateNormalCall
        (generateJsExpr mode func)
        (List.map (generateJsExpr mode) args)


generateGlobalCall : T.CEMN_Canonical -> T.CDN_Name -> List JS.Expr -> JS.Expr
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


generateCoreCall : Mode.Mode -> T.CASTO_Global -> List T.CASTO_Expr -> JS.Expr
generateCoreCall mode (T.CASTO_Global ((T.CEMN_Canonical _ moduleName) as home) name) args =
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


generateTupleCall : T.CEMN_Canonical -> T.CDN_Name -> List JS.Expr -> JS.Expr
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


generateJsArrayCall : T.CEMN_Canonical -> T.CDN_Name -> List JS.Expr -> JS.Expr
generateJsArrayCall home name args =
    case ( args, name ) of
        ( [ entry ], "singleton" ) ->
            JS.ExprArray [ entry ]

        ( [ index, array ], "unsafeGet" ) ->
            JS.ExprIndex array index

        _ ->
            generateGlobalCall home name args


generateBitwiseCall : T.CEMN_Canonical -> T.CDN_Name -> List JS.Expr -> JS.Expr
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


generateBasicsCall : Mode.Mode -> T.CEMN_Canonical -> T.CDN_Name -> List T.CASTO_Expr -> JS.Expr
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


apply : T.CASTO_Expr -> T.CASTO_Expr -> T.CASTO_Expr
apply func value =
    case func of
        T.CASTO_Accessor field ->
            T.CASTO_Access value field

        T.CASTO_Call f args ->
            T.CASTO_Call f (args ++ [ value ])

        _ ->
            T.CASTO_Call func [ value ]


append : Mode.Mode -> T.CASTO_Expr -> T.CASTO_Expr -> JS.Expr
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


toSeqs : Mode.Mode -> T.CASTO_Expr -> List JS.Expr
toSeqs mode expr =
    case expr of
        T.CASTO_Call (T.CASTO_VarGlobal (T.CASTO_Global home "append")) [ left, right ] ->
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
generateTailCall : Mode.Mode -> T.CDN_Name -> List ( T.CDN_Name, T.CASTO_Expr ) -> List JS.Stmt
generateTailCall mode name args =
    let
        toTempVars : ( String, T.CASTO_Expr ) -> ( JsName.Name, JS.Expr )
        toTempVars ( argName, arg ) =
            ( JsName.makeTemp argName, generateJsExpr mode arg )

        toRealVars : ( T.CDN_Name, b ) -> JS.Stmt
        toRealVars ( argName, _ ) =
            JS.ExprStmt <| JS.ExprAssign (JS.LRef (JsName.fromLocal argName)) (JS.ExprRef (JsName.makeTemp argName))
    in
    JS.Vars (List.map toTempVars args)
        :: List.map toRealVars args
        ++ [ JS.Continue (Just (JsName.fromLocal name)) ]



-- DEFINITIONS


generateDef : Mode.Mode -> T.CASTO_Def -> JS.Stmt
generateDef mode def =
    case def of
        T.CASTO_Def name body ->
            JS.Var (JsName.fromLocal name) (generateJsExpr mode body)

        T.CASTO_TailDef name argNames body ->
            JS.Var (JsName.fromLocal name) (codeToExpr (generateTailDef mode name argNames body))


generateTailDef : Mode.Mode -> T.CDN_Name -> List T.CDN_Name -> T.CASTO_Expr -> Code
generateTailDef mode name argNames body =
    generateFunction (List.map JsName.fromLocal argNames) <|
        JsBlock
            [ JS.Labelled (JsName.fromLocal name) <|
                JS.While (JS.ExprBool True) <|
                    codeToStmt <|
                        generate mode body
            ]



-- PATHS


generatePath : Mode.Mode -> T.CASTO_Path -> JS.Expr
generatePath mode path =
    case path of
        T.CASTO_Index index subPath ->
            JS.ExprAccess (generatePath mode subPath) (JsName.fromIndex index)

        T.CASTO_Root name ->
            JS.ExprRef (JsName.fromLocal name)

        T.CASTO_Field field subPath ->
            JS.ExprAccess (generatePath mode subPath) (generateField mode field)

        T.CASTO_Unbox subPath ->
            case mode of
                Mode.Dev _ ->
                    JS.ExprAccess (generatePath mode subPath) (JsName.fromIndex Index.first)

                Mode.Prod _ ->
                    generatePath mode subPath



-- GENERATE IFS


generateIf : Mode.Mode -> List ( T.CASTO_Expr, T.CASTO_Expr ) -> T.CASTO_Expr -> Code
generateIf mode givenBranches givenFinal =
    let
        ( branches, final ) =
            crushIfs givenBranches givenFinal

        convertBranch : ( T.CASTO_Expr, T.CASTO_Expr ) -> ( JS.Expr, Code )
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


crushIfs : List ( T.CASTO_Expr, T.CASTO_Expr ) -> T.CASTO_Expr -> ( List ( T.CASTO_Expr, T.CASTO_Expr ), T.CASTO_Expr )
crushIfs branches final =
    crushIfsHelp [] branches final


crushIfsHelp :
    List ( T.CASTO_Expr, T.CASTO_Expr )
    -> List ( T.CASTO_Expr, T.CASTO_Expr )
    -> T.CASTO_Expr
    -> ( List ( T.CASTO_Expr, T.CASTO_Expr ), T.CASTO_Expr )
crushIfsHelp visitedBranches unvisitedBranches final =
    case unvisitedBranches of
        [] ->
            case final of
                T.CASTO_If subBranches subFinal ->
                    crushIfsHelp visitedBranches subBranches subFinal

                _ ->
                    ( List.reverse visitedBranches, final )

        visiting :: unvisited ->
            crushIfsHelp (visiting :: visitedBranches) unvisited final



-- CASE EXPRESSIONS


generateCase : Mode.Mode -> T.CDN_Name -> T.CDN_Name -> T.CASTO_Decider T.CASTO_Choice -> List ( Int, T.CASTO_Expr ) -> List JS.Stmt
generateCase mode label root decider jumps =
    List.foldr (goto mode label) (generateDecider mode label root decider) jumps


goto : Mode.Mode -> T.CDN_Name -> ( Int, T.CASTO_Expr ) -> List JS.Stmt -> List JS.Stmt
goto mode label ( index, branch ) stmts =
    let
        labeledDeciderStmt : JS.Stmt
        labeledDeciderStmt =
            JS.Labelled
                (JsName.makeLabel label index)
                (JS.While (JS.ExprBool True) (JS.Block stmts))
    in
    labeledDeciderStmt :: codeToStmtList (generate mode branch)


generateDecider : Mode.Mode -> T.CDN_Name -> T.CDN_Name -> T.CASTO_Decider T.CASTO_Choice -> List JS.Stmt
generateDecider mode label root decisionTree =
    case decisionTree of
        T.CASTO_Leaf (T.CASTO_Inline branch) ->
            codeToStmtList (generate mode branch)

        T.CASTO_Leaf (T.CASTO_Jump index) ->
            [ JS.Break (Just (JsName.makeLabel label index)) ]

        T.CASTO_Chain testChain success failure ->
            [ JS.IfStmt
                (Utils.foldl1_ (JS.ExprInfix JS.OpAnd) (List.map (generateIfTest mode root) testChain))
                (JS.Block (generateDecider mode label root success))
                (JS.Block (generateDecider mode label root failure))
            ]

        T.CASTO_FanOut path edges fallback ->
            [ JS.Switch
                (generateCaseTest mode root path (Tuple.first (Prelude.head edges)))
                (List.foldr
                    (\edge cases -> generateCaseBranch mode label root edge :: cases)
                    [ JS.Default (generateDecider mode label root fallback) ]
                    edges
                )
            ]


generateIfTest : Mode.Mode -> T.CDN_Name -> ( T.CODT_Path, T.CODT_Test ) -> JS.Expr
generateIfTest mode root ( path, test ) =
    let
        value : JS.Expr
        value =
            pathToJsExpr mode root path
    in
    case test of
        T.CODT_IsCtor home name index _ opts ->
            let
                tag : JS.Expr
                tag =
                    case mode of
                        Mode.Dev _ ->
                            JS.ExprAccess value JsName.dollar

                        Mode.Prod _ ->
                            case opts of
                                T.CASTC_Normal ->
                                    JS.ExprAccess value JsName.dollar

                                T.CASTC_Enum ->
                                    value

                                T.CASTC_Unbox ->
                                    value
            in
            strictEq tag
                (case mode of
                    Mode.Dev _ ->
                        JS.ExprString name

                    Mode.Prod _ ->
                        JS.ExprInt (ctorToInt home name index)
                )

        T.CODT_IsBool True ->
            value

        T.CODT_IsBool False ->
            JS.ExprPrefix JS.PrefixNot value

        T.CODT_IsInt int ->
            strictEq value (JS.ExprInt int)

        T.CODT_IsChr char ->
            strictEq (JS.ExprString char)
                (case mode of
                    Mode.Dev _ ->
                        JS.ExprCall (JS.ExprAccess value (JsName.fromLocal "valueOf")) []

                    Mode.Prod _ ->
                        value
                )

        T.CODT_IsStr string ->
            strictEq value (JS.ExprString string)

        T.CODT_IsCons ->
            JS.ExprAccess value (JsName.fromLocal "b")

        T.CODT_IsNil ->
            JS.ExprPrefix JS.PrefixNot <|
                JS.ExprAccess value (JsName.fromLocal "b")

        T.CODT_IsTuple ->
            crash "COMPILER BUG - there should never be tests on a tuple"


generateCaseBranch : Mode.Mode -> T.CDN_Name -> T.CDN_Name -> ( T.CODT_Test, T.CASTO_Decider T.CASTO_Choice ) -> JS.Case
generateCaseBranch mode label root ( test, subTree ) =
    JS.Case
        (generateCaseValue mode test)
        (generateDecider mode label root subTree)


generateCaseValue : Mode.Mode -> T.CODT_Test -> JS.Expr
generateCaseValue mode test =
    case test of
        T.CODT_IsCtor home name index _ _ ->
            case mode of
                Mode.Dev _ ->
                    JS.ExprString name

                Mode.Prod _ ->
                    JS.ExprInt (ctorToInt home name index)

        T.CODT_IsInt int ->
            JS.ExprInt int

        T.CODT_IsChr char ->
            JS.ExprString char

        T.CODT_IsStr string ->
            JS.ExprString string

        T.CODT_IsBool _ ->
            crash "COMPILER BUG - there should never be three tests on a boolean"

        T.CODT_IsCons ->
            crash "COMPILER BUG - there should never be three tests on a list"

        T.CODT_IsNil ->
            crash "COMPILER BUG - there should never be three tests on a list"

        T.CODT_IsTuple ->
            crash "COMPILER BUG - there should never be three tests on a tuple"


generateCaseTest : Mode.Mode -> T.CDN_Name -> T.CODT_Path -> T.CODT_Test -> JS.Expr
generateCaseTest mode root path exampleTest =
    let
        value : JS.Expr
        value =
            pathToJsExpr mode root path
    in
    case exampleTest of
        T.CODT_IsCtor home name _ _ opts ->
            if name == Name.bool && home == ModuleName.basics then
                value

            else
                case mode of
                    Mode.Dev _ ->
                        JS.ExprAccess value JsName.dollar

                    Mode.Prod _ ->
                        case opts of
                            T.CASTC_Normal ->
                                JS.ExprAccess value JsName.dollar

                            T.CASTC_Enum ->
                                value

                            T.CASTC_Unbox ->
                                value

        T.CODT_IsInt _ ->
            value

        T.CODT_IsStr _ ->
            value

        T.CODT_IsChr _ ->
            case mode of
                Mode.Dev _ ->
                    JS.ExprCall (JS.ExprAccess value (JsName.fromLocal "valueOf")) []

                Mode.Prod _ ->
                    value

        T.CODT_IsBool _ ->
            crash "COMPILER BUG - there should never be three tests on a list"

        T.CODT_IsCons ->
            crash "COMPILER BUG - there should never be three tests on a list"

        T.CODT_IsNil ->
            crash "COMPILER BUG - there should never be three tests on a list"

        T.CODT_IsTuple ->
            crash "COMPILER BUG - there should never be three tests on a list"



-- PATTERN PATHS


pathToJsExpr : Mode.Mode -> T.CDN_Name -> T.CODT_Path -> JS.Expr
pathToJsExpr mode root path =
    case path of
        T.CODT_Index index subPath ->
            JS.ExprAccess (pathToJsExpr mode root subPath) (JsName.fromIndex index)

        T.CODT_Unbox subPath ->
            case mode of
                Mode.Dev _ ->
                    JS.ExprAccess (pathToJsExpr mode root subPath) (JsName.fromIndex Index.first)

                Mode.Prod _ ->
                    pathToJsExpr mode root subPath

        T.CODT_Empty ->
            JS.ExprRef (JsName.fromLocal root)



-- GENERATE MAIN


generateMain : Mode.Mode -> T.CEMN_Canonical -> T.CASTO_Main -> JS.Expr
generateMain mode home main =
    case main of
        T.CASTO_Static ->
            JS.ExprRef (JsName.fromKernel Name.virtualDom "init")
                |> call (JS.ExprRef (JsName.fromGlobal home "main"))
                |> call (JS.ExprInt 0)
                |> call (JS.ExprInt 0)

        T.CASTO_Dynamic msgType decoder ->
            JS.ExprRef (JsName.fromGlobal home "main")
                |> call (generateJsExpr mode decoder)
                |> call (toDebugMetadata mode msgType)


call : JS.Expr -> JS.Expr -> JS.Expr
call arg func =
    JS.ExprCall func [ arg ]


toDebugMetadata : Mode.Mode -> T.CASTC_Type -> JS.Expr
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
