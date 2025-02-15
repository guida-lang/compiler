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


generateJsExpr : Mode.Mode -> IO.Canonical -> Opt.Expr -> JS.Expr
generateJsExpr mode parentModule expression =
    codeToExpr (generate mode parentModule expression)


generate : Mode.Mode -> IO.Canonical -> Opt.Expr -> Code
generate mode parentModule expression =
    case expression of
        Opt.Bool (A.Region start _) bool ->
            JsExpr <| JS.ExprTrackedBool parentModule start bool

        Opt.Chr (A.Region start _) char ->
            JsExpr <|
                case mode of
                    Mode.Dev _ ->
                        JS.ExprCall toChar [ JS.ExprTrackedString parentModule start char ]

                    Mode.Prod _ ->
                        JS.ExprTrackedString parentModule start char

        Opt.Str (A.Region start _) string ->
            JsExpr <| JS.ExprTrackedString parentModule start string

        Opt.Int (A.Region start _) int ->
            JsExpr <| JS.ExprTrackedInt parentModule start int

        Opt.Float (A.Region start _) float ->
            JsExpr <|
                JS.ExprTrackedFloat parentModule start <|
                    if float == toFloat (floor float) then
                        String.fromFloat float ++ ".0"

                    else
                        String.fromFloat float

        Opt.VarLocal name ->
            JsExpr <| JS.ExprRef (JsName.fromLocal name)

        Opt.TrackedVarLocal (A.Region startPos _) name ->
            JsExpr <| JS.ExprTrackedRef parentModule startPos (JsName.fromLocalHumanReadable name) (JsName.fromLocal name)

        Opt.VarGlobal (A.Region startPos _) (Opt.Global home name) ->
            JsExpr <| JS.ExprTrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)

        Opt.VarEnum (A.Region startPos _) (Opt.Global home name) index ->
            case mode of
                Mode.Dev _ ->
                    JsExpr <| JS.ExprTrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)

                Mode.Prod _ ->
                    JsExpr <| JS.ExprInt (Index.toMachine index)

        Opt.VarBox (A.Region startPos _) (Opt.Global home name) ->
            JsExpr <|
                case mode of
                    Mode.Dev _ ->
                        JS.ExprTrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)

                    Mode.Prod _ ->
                        JS.ExprRef (JsName.fromGlobal ModuleName.basics Name.identity_)

        Opt.VarCycle (A.Region startPos _) home name ->
            JsExpr <| JS.ExprCall (JS.ExprTrackedRef parentModule startPos (JsName.fromGlobalHumanReadable home name) (JsName.fromCycle home name)) []

        Opt.VarDebug region name home unhandledValueName ->
            JsExpr <| generateDebug name home region unhandledValueName

        Opt.VarKernel (A.Region startPos _) home name ->
            JsExpr <| JS.ExprTrackedRef parentModule startPos (JsName.fromKernel home name) (JsName.fromKernel home name)

        Opt.List region entries ->
            case entries of
                [] ->
                    JsExpr <| JS.ExprRef (JsName.fromKernel Name.list "Nil")

                _ ->
                    JsExpr <|
                        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.list "fromArray"))
                            [ JS.ExprTrackedArray parentModule region <| List.map (generateJsExpr mode parentModule) entries
                            ]

        Opt.Function args body ->
            generateFunction (List.map JsName.fromLocal args) (generate mode parentModule body)

        Opt.TrackedFunction args body ->
            let
                argNames : List (A.Located JsName.Name)
                argNames =
                    List.map (\(A.At region name) -> A.At region (JsName.fromLocal name)) args
            in
            generateTrackedFunction parentModule argNames (generate mode parentModule body)

        Opt.Call (A.Region startPos _) func args ->
            JsExpr <| generateCall mode parentModule startPos func args

        Opt.TailCall name args ->
            JsBlock <| generateTailCall mode parentModule name args

        Opt.If branches final ->
            generateIf mode parentModule branches final

        Opt.Let def body ->
            JsBlock <| generateDef mode parentModule def :: codeToStmtList (generate mode parentModule body)

        Opt.Destruct (Opt.Destructor name path) body ->
            let
                pathDef : JS.Stmt
                pathDef =
                    JS.Var (JsName.fromLocal name) (generatePath mode path)
            in
            JsBlock <| pathDef :: codeToStmtList (generate mode parentModule body)

        Opt.Case label root decider jumps ->
            JsBlock <| generateCase mode parentModule label root decider jumps

        Opt.Accessor _ field ->
            JsExpr <|
                JS.ExprFunction Nothing
                    [ JsName.dollar ]
                    [ JS.Return <|
                        JS.ExprAccess (JS.ExprRef JsName.dollar) (generateField mode field)
                    ]

        Opt.Access record (A.Region startPos _) field ->
            JsExpr <| JS.ExprTrackedAccess (generateJsExpr mode parentModule record) parentModule startPos (generateField mode field)

        Opt.Update region record fields ->
            JsExpr <|
                JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "update"))
                    [ generateJsExpr mode parentModule record
                    , generateTrackedRecord mode parentModule region fields
                    ]

        Opt.Record fields ->
            JsExpr <| generateRecord mode parentModule fields

        Opt.TrackedRecord region fields ->
            JsExpr <| generateTrackedRecord mode parentModule region fields

        Opt.Unit ->
            case mode of
                Mode.Dev _ ->
                    JsExpr <| JS.ExprRef (JsName.fromKernel Name.utils "Tuple0")

                Mode.Prod _ ->
                    JsExpr <| JS.ExprInt 0

        Opt.Tuple _ a b maybeC ->
            JsExpr <|
                case maybeC of
                    Nothing ->
                        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "Tuple2"))
                            [ generateJsExpr mode parentModule a
                            , generateJsExpr mode parentModule b
                            ]

                    Just c ->
                        JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "Tuple3"))
                            [ generateJsExpr mode parentModule a
                            , generateJsExpr mode parentModule b
                            , generateJsExpr mode parentModule c
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


generateRecord : Mode.Mode -> IO.Canonical -> Dict String Name.Name Opt.Expr -> JS.Expr
generateRecord mode parentModule fields =
    let
        toPair : ( Name.Name, Opt.Expr ) -> ( JsName.Name, JS.Expr )
        toPair ( field, value ) =
            ( generateField mode field, generateJsExpr mode parentModule value )
    in
    JS.ExprObject (List.map toPair (Dict.toList compare fields))


generateTrackedRecord : Mode.Mode -> IO.Canonical -> A.Region -> Dict String (A.Located Name.Name) Opt.Expr -> JS.Expr
generateTrackedRecord mode parentModule region fields =
    let
        toPair : ( A.Located Name.Name, Opt.Expr ) -> ( A.Located JsName.Name, JS.Expr )
        toPair ( A.At fieldRegion field, value ) =
            ( A.At fieldRegion (generateField mode field), generateJsExpr mode parentModule value )
    in
    JS.ExprTrackedObject parentModule region (List.map toPair (Dict.toList A.compareLocated fields))


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


generateTrackedFunction : IO.Canonical -> List (A.Located JsName.Name) -> Code -> Code
generateTrackedFunction parentModule args body =
    case Dict.get identity (List.length args) funcHelpers of
        Just helper ->
            JsExpr <|
                JS.ExprCall
                    helper
                    [ JS.ExprTrackedFunction parentModule args <|
                        codeToStmtList body
                    ]

        Nothing ->
            case args of
                [ _ ] ->
                    JsExpr <|
                        JS.ExprTrackedFunction parentModule args <|
                            codeToStmtList body

                _ ->
                    let
                        addArg : JsName.Name -> Code -> Code
                        addArg arg code =
                            JsExpr <|
                                JS.ExprFunction Nothing [ arg ] <|
                                    codeToStmtList code
                    in
                    List.foldr addArg body (List.map A.toValue args)


funcHelpers : Dict Int Int JS.Expr
funcHelpers =
    Dict.fromList identity <|
        List.map (\n -> ( n, JS.ExprRef (JsName.makeF n) )) (List.range 2 9)



-- CALLS


generateCall : Mode.Mode -> IO.Canonical -> A.Position -> Opt.Expr -> List Opt.Expr -> JS.Expr
generateCall mode parentModule pos func args =
    case func of
        Opt.VarGlobal _ ((Opt.Global (IO.Canonical pkg _) _) as global) ->
            if pkg == Pkg.core then
                generateCoreCall mode parentModule pos global args

            else
                generateCallHelp mode parentModule pos func args

        Opt.VarBox _ _ ->
            case mode of
                Mode.Dev _ ->
                    generateCallHelp mode parentModule pos func args

                Mode.Prod _ ->
                    case args of
                        [ arg ] ->
                            generateJsExpr mode parentModule arg

                        _ ->
                            generateCallHelp mode parentModule pos func args

        _ ->
            generateCallHelp mode parentModule pos func args


generateCallHelp : Mode.Mode -> IO.Canonical -> A.Position -> Opt.Expr -> List Opt.Expr -> JS.Expr
generateCallHelp mode parentModule pos func args =
    generateNormalCall parentModule
        pos
        (generateJsExpr mode parentModule func)
        (List.map (generateJsExpr mode parentModule) args)


generateGlobalCall : IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List JS.Expr -> JS.Expr
generateGlobalCall parentModule ((A.Position line col) as pos) home name args =
    -- generateNormalCall (JS.ExprRef (JsName.fromGlobal home name)) args
    let
        ref : JS.Expr
        ref =
            if line == 0 && col == 0 then
                JS.ExprRef (JsName.fromGlobal home name)

            else
                JS.ExprTrackedRef parentModule pos (JsName.fromGlobalHumanReadable home name) (JsName.fromGlobal home name)
    in
    generateNormalCall parentModule pos ref args


generateNormalCall : IO.Canonical -> A.Position -> JS.Expr -> List JS.Expr -> JS.Expr
generateNormalCall parentModule pos func args =
    case Dict.get identity (List.length args) callHelpers of
        Just helper ->
            JS.ExprTrackedNormalCall parentModule pos helper func args

        Nothing ->
            List.foldl (\a f -> JS.ExprCall f [ a ]) func args


callHelpers : Dict Int Int JS.Expr
callHelpers =
    Dict.fromList identity <|
        List.map (\n -> ( n, JS.ExprRef (JsName.makeA n) )) (List.range 2 9)



-- CORE CALLS


generateCoreCall : Mode.Mode -> IO.Canonical -> A.Position -> Opt.Global -> List Opt.Expr -> JS.Expr
generateCoreCall mode parentModule pos (Opt.Global ((IO.Canonical _ moduleName) as home) name) args =
    if moduleName == Name.basics then
        generateBasicsCall mode parentModule pos home name args

    else if moduleName == Name.bitwise then
        generateBitwiseCall parentModule pos home name (List.map (generateJsExpr mode parentModule) args)

    else if moduleName == Name.tuple then
        generateTupleCall parentModule pos home name (List.map (generateJsExpr mode parentModule) args)

    else if moduleName == Name.jsArray then
        generateJsArrayCall parentModule pos home name (List.map (generateJsExpr mode parentModule) args)

    else
        generateGlobalCall parentModule pos home name (List.map (generateJsExpr mode parentModule) args)


generateTupleCall : IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List JS.Expr -> JS.Expr
generateTupleCall parentModule pos home name args =
    case args of
        [ value ] ->
            case name of
                "first" ->
                    JS.ExprAccess value (JsName.fromLocal "a")

                "second" ->
                    JS.ExprAccess value (JsName.fromLocal "b")

                _ ->
                    generateGlobalCall parentModule pos home name args

        _ ->
            generateGlobalCall parentModule pos home name args


generateJsArrayCall : IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List JS.Expr -> JS.Expr
generateJsArrayCall parentModule pos home name args =
    case ( args, name ) of
        ( [ entry ], "singleton" ) ->
            JS.ExprArray [ entry ]

        ( [ index, array ], "unsafeGet" ) ->
            JS.ExprIndex array index

        _ ->
            generateGlobalCall parentModule pos home name args


generateBitwiseCall : IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List JS.Expr -> JS.Expr
generateBitwiseCall parentModule pos home name args =
    case args of
        [ arg ] ->
            case name of
                "complement" ->
                    JS.ExprPrefix JS.PrefixComplement arg

                _ ->
                    generateGlobalCall parentModule pos home name args

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
                    generateGlobalCall parentModule pos home name args

        _ ->
            generateGlobalCall parentModule pos home name args


generateBasicsCall : Mode.Mode -> IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List Opt.Expr -> JS.Expr
generateBasicsCall mode parentModule pos home name args =
    case args of
        [ elmArg ] ->
            let
                arg : JS.Expr
                arg =
                    generateJsExpr mode parentModule elmArg
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
                    generateGlobalCall parentModule pos home name [ arg ]

        [ elmLeft, elmRight ] ->
            case name of
                -- NOTE: removed "composeL" and "composeR" because of this issue:
                -- https://github.com/elm/compiler/issues/1722
                "append" ->
                    append mode parentModule elmLeft elmRight

                "apL" ->
                    generateJsExpr mode parentModule <| apply elmLeft elmRight

                "apR" ->
                    generateJsExpr mode parentModule <| apply elmRight elmLeft

                _ ->
                    let
                        left : JS.Expr
                        left =
                            generateJsExpr mode parentModule elmLeft

                        right : JS.Expr
                        right =
                            generateJsExpr mode parentModule elmRight
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
                            generateGlobalCall parentModule pos home name [ left, right ]

        _ ->
            generateGlobalCall parentModule pos home name <| List.map (generateJsExpr mode parentModule) args


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

        JS.ExprTrackedString _ _ _ ->
            True

        JS.ExprFloat _ ->
            True

        JS.ExprTrackedFloat _ _ _ ->
            True

        JS.ExprInt _ ->
            True

        JS.ExprTrackedInt _ _ _ ->
            True

        JS.ExprBool _ ->
            True

        JS.ExprTrackedBool _ _ _ ->
            True

        _ ->
            False


apply : Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
    case func of
        Opt.Accessor region field ->
            Opt.Access value region field

        Opt.Call region f args ->
            Opt.Call region f (args ++ [ value ])

        _ ->
            Opt.Call (Maybe.withDefault A.zero (exprRegion func)) func [ value ]


exprRegion : Opt.Expr -> Maybe A.Region
exprRegion expr =
    case expr of
        Opt.Bool region _ ->
            Just region

        Opt.Chr region _ ->
            Just region

        Opt.Str region _ ->
            Just region

        Opt.Int region _ ->
            Just region

        Opt.Float region _ ->
            Just region

        Opt.VarLocal _ ->
            Nothing

        Opt.TrackedVarLocal region _ ->
            Just region

        Opt.VarGlobal region _ ->
            Just region

        Opt.VarEnum region _ _ ->
            Just region

        Opt.VarBox region _ ->
            Just region

        Opt.VarCycle region _ _ ->
            Just region

        Opt.VarDebug region _ _ _ ->
            Just region

        Opt.VarKernel region _ _ ->
            Just region

        Opt.List region _ ->
            Just region

        Opt.Function _ _ ->
            Nothing

        Opt.TrackedFunction _ _ ->
            Nothing

        Opt.Call region _ _ ->
            Just region

        Opt.TailCall _ _ ->
            Nothing

        Opt.If _ _ ->
            Nothing

        Opt.Let _ _ ->
            Nothing

        Opt.Destruct _ _ ->
            Nothing

        Opt.Case _ _ _ _ ->
            Nothing

        Opt.Accessor region _ ->
            Just region

        Opt.Access _ region _ ->
            Just region

        Opt.Update region _ _ ->
            Just region

        Opt.Record _ ->
            Nothing

        Opt.TrackedRecord region _ ->
            Just region

        Opt.Unit ->
            Nothing

        Opt.Tuple region _ _ _ ->
            Just region

        Opt.Shader _ _ _ ->
            Nothing


append : Mode.Mode -> IO.Canonical -> Opt.Expr -> Opt.Expr -> JS.Expr
append mode parentModule left right =
    let
        seqs : List JS.Expr
        seqs =
            generateJsExpr mode parentModule left :: toSeqs mode parentModule right
    in
    if List.any isStringLiteral seqs then
        Utils.foldr1 (JS.ExprInfix JS.OpAdd) seqs

    else
        Utils.foldr1 jsAppend seqs


jsAppend : JS.Expr -> JS.Expr -> JS.Expr
jsAppend a b =
    JS.ExprCall (JS.ExprRef (JsName.fromKernel Name.utils "ap")) [ a, b ]


toSeqs : Mode.Mode -> IO.Canonical -> Opt.Expr -> List JS.Expr
toSeqs mode parentModule expr =
    case expr of
        Opt.Call _ (Opt.VarGlobal _ (Opt.Global home "append")) [ left, right ] ->
            if home == ModuleName.basics then
                generateJsExpr mode parentModule left :: toSeqs mode parentModule right

            else
                [ generateJsExpr mode parentModule expr ]

        _ ->
            [ generateJsExpr mode parentModule expr ]


isStringLiteral : JS.Expr -> Bool
isStringLiteral expr =
    case expr of
        JS.ExprString _ ->
            True

        JS.ExprTrackedString _ _ _ ->
            True

        _ ->
            False



-- SIMPLIFY INFIX OPERATORS


strictEq : JS.Expr -> JS.Expr -> JS.Expr
strictEq left right =
    case left of
        JS.ExprInt 0 ->
            JS.ExprPrefix JS.PrefixNot right

        JS.ExprTrackedInt _ _ 0 ->
            JS.ExprPrefix JS.PrefixNot right

        JS.ExprBool bool ->
            if bool then
                right

            else
                JS.ExprPrefix JS.PrefixNot right

        JS.ExprTrackedBool _ _ bool ->
            if bool then
                right

            else
                JS.ExprPrefix JS.PrefixNot right

        _ ->
            case right of
                JS.ExprInt 0 ->
                    JS.ExprPrefix JS.PrefixNot left

                JS.ExprTrackedInt _ _ 0 ->
                    JS.ExprPrefix JS.PrefixNot left

                JS.ExprBool bool ->
                    if bool then
                        left

                    else
                        JS.ExprPrefix JS.PrefixNot left

                JS.ExprTrackedBool _ _ bool ->
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

        JS.ExprTrackedInt _ _ 0 ->
            JS.ExprPrefix JS.PrefixNot (JS.ExprPrefix JS.PrefixNot right)

        JS.ExprBool bool ->
            if bool then
                JS.ExprPrefix JS.PrefixNot right

            else
                right

        JS.ExprTrackedBool _ _ bool ->
            if bool then
                JS.ExprPrefix JS.PrefixNot right

            else
                right

        _ ->
            case right of
                JS.ExprInt 0 ->
                    JS.ExprPrefix JS.PrefixNot (JS.ExprPrefix JS.PrefixNot left)

                JS.ExprTrackedInt _ _ 0 ->
                    JS.ExprPrefix JS.PrefixNot (JS.ExprPrefix JS.PrefixNot left)

                JS.ExprBool bool ->
                    if bool then
                        JS.ExprPrefix JS.PrefixNot left

                    else
                        left

                JS.ExprTrackedBool _ _ bool ->
                    if bool then
                        JS.ExprPrefix JS.PrefixNot left

                    else
                        left

                _ ->
                    JS.ExprInfix JS.OpNe left right



-- TAIL CALL


{-| TODO check if JS minifiers collapse unnecessary temporary variables
-}
generateTailCall : Mode.Mode -> IO.Canonical -> Name.Name -> List ( Name.Name, Opt.Expr ) -> List JS.Stmt
generateTailCall mode parentModule name args =
    let
        toTempVars : ( String, Opt.Expr ) -> ( JsName.Name, JS.Expr )
        toTempVars ( argName, arg ) =
            ( JsName.makeTemp argName, generateJsExpr mode parentModule arg )

        toRealVars : ( Name.Name, b ) -> JS.Stmt
        toRealVars ( argName, _ ) =
            JS.ExprStmt <| JS.ExprAssign (JS.LRef (JsName.fromLocal argName)) (JS.ExprRef (JsName.makeTemp argName))
    in
    JS.Vars (List.map toTempVars args)
        :: List.map toRealVars args
        ++ [ JS.Continue (Just (JsName.fromLocal name)) ]



-- DEFINITIONS


generateDef : Mode.Mode -> IO.Canonical -> Opt.Def -> JS.Stmt
generateDef mode parentModule def =
    case def of
        Opt.Def (A.Region start _) name body ->
            JS.TrackedVar parentModule start (JsName.fromLocal name) (JsName.fromLocal name) (generateJsExpr mode parentModule body)

        Opt.TailDef (A.Region start _) name argNames body ->
            JS.TrackedVar parentModule start (JsName.fromLocal name) (JsName.fromLocal name) (codeToExpr (generateTailDef mode parentModule name argNames body))


generateTailDef : Mode.Mode -> IO.Canonical -> Name.Name -> List (A.Located Name.Name) -> Opt.Expr -> Code
generateTailDef mode parentModule name argNames body =
    generateTrackedFunction parentModule (List.map (\(A.At region argName) -> A.At region (JsName.fromLocal argName)) argNames) <|
        JsBlock
            [ JS.Labelled (JsName.fromLocal name) <|
                JS.While (JS.ExprBool True) <|
                    codeToStmt <|
                        generate mode parentModule body
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


generateIf : Mode.Mode -> IO.Canonical -> List ( Opt.Expr, Opt.Expr ) -> Opt.Expr -> Code
generateIf mode parentModule givenBranches givenFinal =
    let
        ( branches, final ) =
            crushIfs givenBranches givenFinal

        convertBranch : ( Opt.Expr, Opt.Expr ) -> ( JS.Expr, Code )
        convertBranch ( condition, expr ) =
            ( generateJsExpr mode parentModule condition
            , generate mode parentModule expr
            )

        branchExprs : List ( JS.Expr, Code )
        branchExprs =
            List.map convertBranch branches

        finalCode : Code
        finalCode =
            generate mode parentModule final
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


generateCase : Mode.Mode -> IO.Canonical -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> List ( Int, Opt.Expr ) -> List JS.Stmt
generateCase mode parentModule label root decider jumps =
    List.foldr (goto mode parentModule label) (generateDecider mode parentModule label root decider) jumps


goto : Mode.Mode -> IO.Canonical -> Name.Name -> ( Int, Opt.Expr ) -> List JS.Stmt -> List JS.Stmt
goto mode parentModule label ( index, branch ) stmts =
    let
        labeledDeciderStmt : JS.Stmt
        labeledDeciderStmt =
            JS.Labelled
                (JsName.makeLabel label index)
                (JS.While (JS.ExprBool True) (JS.Block stmts))
    in
    labeledDeciderStmt :: codeToStmtList (generate mode parentModule branch)


generateDecider : Mode.Mode -> IO.Canonical -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> List JS.Stmt
generateDecider mode parentModule label root decisionTree =
    case decisionTree of
        Opt.Leaf (Opt.Inline branch) ->
            codeToStmtList (generate mode parentModule branch)

        Opt.Leaf (Opt.Jump index) ->
            [ JS.Break (Just (JsName.makeLabel label index)) ]

        Opt.Chain testChain success failure ->
            [ JS.IfStmt
                (Utils.foldl1_ (JS.ExprInfix JS.OpAnd) (List.map (generateIfTest mode root) testChain))
                (JS.Block (generateDecider mode parentModule label root success))
                (JS.Block (generateDecider mode parentModule label root failure))
            ]

        Opt.FanOut path edges fallback ->
            [ JS.Switch
                (generateCaseTest mode root path (Tuple.first (Prelude.head edges)))
                (List.foldr
                    (\edge cases -> generateCaseBranch mode parentModule label root edge :: cases)
                    [ JS.Default (generateDecider mode parentModule label root fallback) ]
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


generateCaseBranch : Mode.Mode -> IO.Canonical -> Name.Name -> Name.Name -> ( DT.Test, Opt.Decider Opt.Choice ) -> JS.Case
generateCaseBranch mode parentModule label root ( test, subTree ) =
    JS.Case
        (generateCaseValue mode test)
        (generateDecider mode parentModule label root subTree)


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
                |> call (generateJsExpr mode home decoder)
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
                    [ ( "versions", Encode.object [ ( "elm", V.encode V.elmCompiler ) ] )
                    , ( "types", Type.encodeMetadata (Extract.fromMsg interfaces msgType) )
                    ]
                )
