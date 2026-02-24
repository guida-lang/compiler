module Compiler.Generate.Rust.Expression exposing
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
import Compiler.Generate.Mode as Mode
import Compiler.Generate.Rust.Builder as Rust
import Compiler.Generate.Rust.Name as RustName
import Compiler.Generate.Target as Target exposing (Target)
import Compiler.Guida.Compiler.Type as Type
import Compiler.Guida.Compiler.Type.Extract as Extract
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Compiler.Json.Encode as Encode
import Compiler.Optimize.DecisionTree as DT
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import Prelude
import System.TypeCheck.IO as IO
import Utils.Crash exposing (crash)
import Utils.Main as Utils


generateRustExpr : Target -> Mode.Mode -> IO.Canonical -> Opt.Expr -> Rust.Expr
generateRustExpr target mode parentModule expression =
    codeToExpr (generate target mode parentModule expression)


generate : Target -> Mode.Mode -> IO.Canonical -> Opt.Expr -> Code
generate target mode parentModule expression =
    case expression of
        Opt.Bool (A.Region start _) bool ->
            JsExpr <| Rust.ExprTrackedBool parentModule start bool

        Opt.Chr (A.Region start _) char ->
            JsExpr <|
                case mode of
                    Mode.Dev _ ->
                        Rust.ExprCall toChar [ Rust.ExprTrackedString parentModule start char ]

                    Mode.Prod _ ->
                        Rust.ExprTrackedString parentModule start char

        Opt.Str (A.Region start _) string ->
            JsExpr <| Rust.ExprTrackedString parentModule start string

        Opt.Int (A.Region start _) int ->
            JsExpr <| Rust.ExprTrackedInt parentModule start int

        Opt.Float (A.Region start _) float ->
            JsExpr <|
                Rust.ExprTrackedFloat parentModule start <|
                    if float == toFloat (floor float) then
                        String.fromFloat float ++ ".0"

                    else
                        String.fromFloat float

        Opt.VarLocal name ->
            JsExpr <| Rust.ExprRef (RustName.fromLocal name)

        Opt.TrackedVarLocal (A.Region startPos _) name ->
            JsExpr <| Rust.ExprTrackedRef parentModule startPos (RustName.fromLocalHumanReadable name) (RustName.fromLocal name)

        Opt.VarGlobal (A.Region startPos _) (Opt.Global home name) ->
            JsExpr <| Rust.ExprTrackedRef parentModule startPos (RustName.fromGlobalHumanReadable home name) (RustName.fromGlobal home name)

        Opt.VarEnum (A.Region startPos _) (Opt.Global home name) index ->
            case mode of
                Mode.Dev _ ->
                    JsExpr <| Rust.ExprTrackedRef parentModule startPos (RustName.fromGlobalHumanReadable home name) (RustName.fromGlobal home name)

                Mode.Prod _ ->
                    JsExpr <| Rust.ExprInt (Index.toMachine index)

        Opt.VarBox (A.Region startPos _) (Opt.Global home name) ->
            JsExpr <|
                case mode of
                    Mode.Dev _ ->
                        Rust.ExprTrackedRef parentModule startPos (RustName.fromGlobalHumanReadable home name) (RustName.fromGlobal home name)

                    Mode.Prod _ ->
                        Rust.ExprRef (RustName.fromGlobal (ModuleName.basics target) Name.identity_)

        Opt.VarCycle (A.Region startPos _) home name ->
            JsExpr <| Rust.ExprCall (Rust.ExprTrackedRef parentModule startPos (RustName.fromGlobalHumanReadable home name) (RustName.fromCycle home name)) []

        Opt.VarDebug region name home unhandledValueName ->
            JsExpr <| generateDebug target name home region unhandledValueName

        Opt.VarKernel (A.Region startPos _) home name ->
            JsExpr <| Rust.ExprTrackedRef parentModule startPos (RustName.fromKernel home name) (RustName.fromKernel home name)

        Opt.List region entries ->
            case entries of
                [] ->
                    JsExpr <| Rust.ExprRef (RustName.fromKernel Name.list "Nil")

                _ ->
                    JsExpr <|
                        Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.list "fromArray"))
                            [ Rust.ExprTrackedArray parentModule region <| List.map (generateRustExpr target mode parentModule) entries
                            ]

        Opt.Function args body ->
            generateFunction (List.map RustName.fromLocal args) (generate target mode parentModule body)

        Opt.TrackedFunction args body ->
            let
                argNames : List (A.Located RustName.Name)
                argNames =
                    List.map (\(A.At region name) -> A.At region (RustName.fromLocal name)) args
            in
            generateTrackedFunction parentModule argNames (generate target mode parentModule body)

        Opt.Call (A.Region startPos _) func args ->
            JsExpr <| generateCall target mode parentModule startPos func args

        Opt.TailCall name args ->
            JsBlock <| generateTailCall target mode parentModule name args

        Opt.If branches final ->
            generateIf target mode parentModule branches final

        Opt.Let def body ->
            JsBlock <| generateDef target mode parentModule def :: codeToStmtList (generate target mode parentModule body)

        Opt.Destruct (Opt.Destructor name path) body ->
            let
                pathDef : Rust.Stmt
                pathDef =
                    Rust.Var (RustName.fromLocal name) (generatePath mode path)
            in
            JsBlock <| pathDef :: codeToStmtList (generate target mode parentModule body)

        Opt.Case label root decider jumps ->
            JsBlock <| generateCase target mode parentModule label root decider jumps

        Opt.Accessor _ field ->
            JsExpr <|
                Rust.ExprFunction Nothing
                    [ RustName.dollar ]
                    [ Rust.Return <|
                        Rust.ExprAccess (Rust.ExprRef RustName.dollar) (generateField mode field)
                    ]

        Opt.Access record (A.Region startPos _) field ->
            JsExpr <| Rust.ExprTrackedAccess (generateRustExpr target mode parentModule record) parentModule startPos (generateField mode field)

        Opt.Update region record fields ->
            JsExpr <|
                Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.utils "update"))
                    [ generateRustExpr target mode parentModule record
                    , generateTrackedRecord target mode parentModule region fields
                    ]

        Opt.Record fields ->
            JsExpr <| generateRecord target mode parentModule fields

        Opt.TrackedRecord region fields ->
            JsExpr <| generateTrackedRecord target mode parentModule region fields

        Opt.Unit ->
            case mode of
                Mode.Dev _ ->
                    JsExpr <| Rust.ExprRef (RustName.fromKernel Name.utils "Tuple0")

                Mode.Prod _ ->
                    JsExpr <| Rust.ExprInt 0

        Opt.Tuple _ a b cs ->
            JsExpr <|
                case cs of
                    [] ->
                        Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.utils "Tuple2"))
                            [ generateRustExpr target mode parentModule a
                            , generateRustExpr target mode parentModule b
                            ]

                    [ c ] ->
                        Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.utils "Tuple3"))
                            [ generateRustExpr target mode parentModule a
                            , generateRustExpr target mode parentModule b
                            , generateRustExpr target mode parentModule c
                            ]

                    _ ->
                        Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.utils "TupleN"))
                            (List.map (generateRustExpr target mode parentModule) (a :: b :: cs))

        Opt.Shader src attributes uniforms ->
            let
                toTranlation : Name.Name -> ( RustName.Name, Rust.Expr )
                toTranlation field =
                    ( RustName.fromLocal field
                    , Rust.ExprString (generateField mode field)
                    )

                toTranslationObject : EverySet.EverySet String Name.Name -> Rust.Expr
                toTranslationObject fields =
                    Rust.ExprObject (List.map toTranlation (EverySet.toList compare fields))
            in
            JsExpr <|
                Rust.ExprObject
                    [ ( RustName.fromLocal "src", Rust.ExprString (Shader.toJsStringBuilder src) )
                    , ( RustName.fromLocal "attributes", toTranslationObject attributes )
                    , ( RustName.fromLocal "uniforms", toTranslationObject uniforms )
                    ]



-- CODE CHUNKS


type Code
    = JsExpr Rust.Expr
    | JsBlock (List Rust.Stmt)


codeToExpr : Code -> Rust.Expr
codeToExpr code =
    case code of
        JsExpr expr ->
            expr

        JsBlock [ Rust.Return expr ] ->
            expr

        JsBlock stmts ->
            Rust.ExprCall (Rust.ExprFunction Nothing [] stmts) []


codeToStmtList : Code -> List Rust.Stmt
codeToStmtList code =
    case code of
        JsExpr (Rust.ExprCall (Rust.ExprFunction Nothing [] stmts) []) ->
            stmts

        JsExpr expr ->
            [ Rust.Return expr ]

        JsBlock stmts ->
            stmts


codeToStmt : Code -> Rust.Stmt
codeToStmt code =
    case code of
        JsExpr (Rust.ExprCall (Rust.ExprFunction Nothing [] stmts) []) ->
            Rust.Block stmts

        JsExpr expr ->
            Rust.Return expr

        JsBlock [ stmt ] ->
            stmt

        JsBlock stmts ->
            Rust.Block stmts



-- CHARS


toChar : Rust.Expr
toChar =
    Rust.ExprRef (RustName.fromKernel Name.utils "chr")



-- CTOR


generateCtor : Target -> Mode.Mode -> Opt.Global -> Index.ZeroBased -> Int -> Code
generateCtor target mode (Opt.Global home name) index arity =
    let
        argNames : List RustName.Name
        argNames =
            Index.indexedMap (\i _ -> RustName.fromIndex i) (List.range 1 arity)

        ctorTag : Rust.Expr
        ctorTag =
            case mode of
                Mode.Dev _ ->
                    Rust.ExprString name

                Mode.Prod _ ->
                    Rust.ExprInt (ctorToInt target home name index)
    in
    generateFunction argNames <|
        JsExpr <|
            Rust.ExprObject
                (( RustName.dollar, ctorTag ) :: List.map (\n -> ( n, Rust.ExprRef n )) argNames)


ctorToInt : Target -> IO.Canonical -> Name.Name -> Index.ZeroBased -> Int
ctorToInt target home name index =
    if home == ModuleName.dict target && (name == "RBNode_elm_builtin" || name == "RBEmpty_elm_builtin") then
        -(Index.toHuman index)

    else
        Index.toMachine index



-- RECORDS


generateRecord : Target -> Mode.Mode -> IO.Canonical -> Dict String Name.Name Opt.Expr -> Rust.Expr
generateRecord target mode parentModule fields =
    let
        toPair : ( Name.Name, Opt.Expr ) -> ( RustName.Name, Rust.Expr )
        toPair ( field, value ) =
            ( generateField mode field, generateRustExpr target mode parentModule value )
    in
    Rust.ExprObject (List.map toPair (Dict.toList compare fields))


generateTrackedRecord : Target -> Mode.Mode -> IO.Canonical -> A.Region -> Dict String (A.Located Name.Name) Opt.Expr -> Rust.Expr
generateTrackedRecord target mode parentModule region fields =
    let
        toPair : ( A.Located Name.Name, Opt.Expr ) -> ( A.Located RustName.Name, Rust.Expr )
        toPair ( A.At fieldRegion field, value ) =
            ( A.At fieldRegion (generateField mode field), generateRustExpr target mode parentModule value )
    in
    Rust.ExprTrackedObject parentModule region (List.map toPair (Dict.toList A.compareLocated fields))


generateField : Mode.Mode -> Name.Name -> RustName.Name
generateField mode name =
    case mode of
        Mode.Dev _ ->
            RustName.fromLocal name

        Mode.Prod fields ->
            Utils.find identity name fields



-- DEBUG


generateDebug : Target -> Name.Name -> IO.Canonical -> A.Region -> Maybe Name.Name -> Rust.Expr
generateDebug target name (IO.Canonical _ home) region unhandledValueName =
    if name /= "todo" then
        Rust.ExprRef (RustName.fromGlobal (ModuleName.debug target) name)

    else
        case unhandledValueName of
            Nothing ->
                Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.debug "todo"))
                    [ Rust.ExprString home
                    , regionToJsExpr region
                    ]

            Just valueName ->
                Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.debug "todoCase"))
                    [ Rust.ExprString home
                    , regionToJsExpr region
                    , Rust.ExprRef (RustName.fromLocal valueName)
                    ]


regionToJsExpr : A.Region -> Rust.Expr
regionToJsExpr (A.Region start end) =
    Rust.ExprObject
        [ ( RustName.fromLocal "start", positionToJsExpr start )
        , ( RustName.fromLocal "end", positionToJsExpr end )
        ]


positionToJsExpr : A.Position -> Rust.Expr
positionToJsExpr (A.Position line column) =
    Rust.ExprObject
        [ ( RustName.fromLocal "line", Rust.ExprInt line )
        , ( RustName.fromLocal "column", Rust.ExprInt column )
        ]



-- FUNCTION


generateFunction : List RustName.Name -> Code -> Code
generateFunction args body =
    case Dict.get identity (List.length args) funcHelpers of
        Just helper ->
            JsExpr <|
                Rust.ExprCall helper
                    [ Rust.ExprFunction Nothing args <|
                        codeToStmtList body
                    ]

        Nothing ->
            let
                addArg : RustName.Name -> Code -> Code
                addArg arg code =
                    JsExpr <|
                        Rust.ExprFunction Nothing [ arg ] <|
                            codeToStmtList code
            in
            List.foldr addArg body args


generateTrackedFunction : IO.Canonical -> List (A.Located RustName.Name) -> Code -> Code
generateTrackedFunction parentModule args body =
    case Dict.get identity (List.length args) funcHelpers of
        Just helper ->
            JsExpr <|
                Rust.ExprCall
                    helper
                    [ Rust.ExprTrackedFunction parentModule args <|
                        codeToStmtList body
                    ]

        Nothing ->
            case args of
                [ _ ] ->
                    JsExpr <|
                        Rust.ExprTrackedFunction parentModule args <|
                            codeToStmtList body

                _ ->
                    let
                        addArg : RustName.Name -> Code -> Code
                        addArg arg code =
                            JsExpr <|
                                Rust.ExprFunction Nothing [ arg ] <|
                                    codeToStmtList code
                    in
                    List.foldr addArg body (List.map A.toValue args)


funcHelpers : Dict Int Int Rust.Expr
funcHelpers =
    Dict.fromList identity <|
        List.map (\n -> ( n, Rust.ExprRef (RustName.makeF n) )) (List.range 2 9)



-- CALLS


generateCall : Target -> Mode.Mode -> IO.Canonical -> A.Position -> Opt.Expr -> List Opt.Expr -> Rust.Expr
generateCall target mode parentModule pos func args =
    case func of
        Opt.VarGlobal _ ((Opt.Global (IO.Canonical pkg _) _) as global) ->
            if pkg == Pkg.core || pkg == Pkg.stdlib then
                generateCoreCall target mode parentModule pos global args

            else
                generateCallHelp target mode parentModule pos func args

        Opt.VarBox _ _ ->
            case mode of
                Mode.Dev _ ->
                    generateCallHelp target mode parentModule pos func args

                Mode.Prod _ ->
                    case args of
                        [ arg ] ->
                            generateRustExpr target mode parentModule arg

                        _ ->
                            generateCallHelp target mode parentModule pos func args

        _ ->
            generateCallHelp target mode parentModule pos func args


generateCallHelp : Target -> Mode.Mode -> IO.Canonical -> A.Position -> Opt.Expr -> List Opt.Expr -> Rust.Expr
generateCallHelp target mode parentModule pos func args =
    generateNormalCall parentModule
        pos
        (generateRustExpr target mode parentModule func)
        (List.map (generateRustExpr target mode parentModule) args)


generateGlobalCall : IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List Rust.Expr -> Rust.Expr
generateGlobalCall parentModule ((A.Position line col) as pos) home name args =
    -- generateNormalCall (Rust.ExprRef (RustName.fromGlobal home name)) args
    let
        ref : Rust.Expr
        ref =
            if line == 0 && col == 0 then
                Rust.ExprRef (RustName.fromGlobal home name)

            else
                Rust.ExprTrackedRef parentModule pos (RustName.fromGlobalHumanReadable home name) (RustName.fromGlobal home name)
    in
    generateNormalCall parentModule pos ref args


generateNormalCall : IO.Canonical -> A.Position -> Rust.Expr -> List Rust.Expr -> Rust.Expr
generateNormalCall parentModule pos func args =
    case Dict.get identity (List.length args) callHelpers of
        Just helper ->
            Rust.ExprTrackedNormalCall parentModule pos helper func args

        Nothing ->
            List.foldl (\a f -> Rust.ExprCall f [ a ]) func args


callHelpers : Dict Int Int Rust.Expr
callHelpers =
    Dict.fromList identity <|
        List.map (\n -> ( n, Rust.ExprRef (RustName.makeA n) )) (List.range 2 9)



-- CORE CALLS


generateCoreCall : Target -> Mode.Mode -> IO.Canonical -> A.Position -> Opt.Global -> List Opt.Expr -> Rust.Expr
generateCoreCall target mode parentModule pos (Opt.Global ((IO.Canonical _ moduleName) as home) name) args =
    if moduleName == Name.basics then
        generateBasicsCall target mode parentModule pos home name args

    else if moduleName == Name.bitwise then
        generateBitwiseCall parentModule pos home name (List.map (generateRustExpr target mode parentModule) args)

    else if moduleName == Name.tuple then
        generateTupleCall parentModule pos home name (List.map (generateRustExpr target mode parentModule) args)

    else if moduleName == Name.jsArray then
        generateJsArrayCall parentModule pos home name (List.map (generateRustExpr target mode parentModule) args)

    else
        generateGlobalCall parentModule pos home name (List.map (generateRustExpr target mode parentModule) args)


generateTupleCall : IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List Rust.Expr -> Rust.Expr
generateTupleCall parentModule pos home name args =
    case args of
        [ value ] ->
            case name of
                "first" ->
                    Rust.ExprAccess value (RustName.fromLocal "a")

                "second" ->
                    Rust.ExprAccess value (RustName.fromLocal "b")

                _ ->
                    generateGlobalCall parentModule pos home name args

        _ ->
            generateGlobalCall parentModule pos home name args


generateJsArrayCall : IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List Rust.Expr -> Rust.Expr
generateJsArrayCall parentModule pos home name args =
    case ( args, name ) of
        ( [ entry ], "singleton" ) ->
            Rust.ExprArray [ entry ]

        ( [ index, array ], "unsafeGet" ) ->
            Rust.ExprIndex array index

        _ ->
            generateGlobalCall parentModule pos home name args


generateBitwiseCall : IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List Rust.Expr -> Rust.Expr
generateBitwiseCall parentModule pos home name args =
    case args of
        [ arg ] ->
            case name of
                "complement" ->
                    Rust.ExprPrefix Rust.PrefixComplement arg

                _ ->
                    generateGlobalCall parentModule pos home name args

        [ left, right ] ->
            case name of
                "and" ->
                    Rust.ExprInfix Rust.OpBitwiseAnd left right

                "or" ->
                    Rust.ExprInfix Rust.OpBitwiseOr left right

                "xor" ->
                    Rust.ExprInfix Rust.OpBitwiseXor left right

                "shiftLeftBy" ->
                    Rust.ExprInfix Rust.OpLShift right left

                "shiftRightBy" ->
                    Rust.ExprInfix Rust.OpSpRShift right left

                "shiftRightZfBy" ->
                    Rust.ExprInfix Rust.OpZfRShift right left

                _ ->
                    generateGlobalCall parentModule pos home name args

        _ ->
            generateGlobalCall parentModule pos home name args


generateBasicsCall : Target -> Mode.Mode -> IO.Canonical -> A.Position -> IO.Canonical -> Name.Name -> List Opt.Expr -> Rust.Expr
generateBasicsCall target mode parentModule pos home name args =
    case args of
        [ guidaArg ] ->
            let
                arg : Rust.Expr
                arg =
                    generateRustExpr target mode parentModule guidaArg
            in
            case name of
                "not" ->
                    Rust.ExprPrefix Rust.PrefixNot arg

                "negate" ->
                    Rust.ExprPrefix Rust.PrefixNegate arg

                "toFloat" ->
                    arg

                "truncate" ->
                    Rust.ExprInfix Rust.OpBitwiseOr arg (Rust.ExprInt 0)

                _ ->
                    generateGlobalCall parentModule pos home name [ arg ]

        [ guidaLeft, guidaRight ] ->
            case name of
                -- NOTE: removed "composeL" and "composeR" because of this issue:
                -- https://github.com/elm/compiler/issues/1722
                "append" ->
                    append target mode parentModule guidaLeft guidaRight

                "apL" ->
                    generateRustExpr target mode parentModule <| apply guidaLeft guidaRight

                "apR" ->
                    generateRustExpr target mode parentModule <| apply guidaRight guidaLeft

                _ ->
                    let
                        left : Rust.Expr
                        left =
                            generateRustExpr target mode parentModule guidaLeft

                        right : Rust.Expr
                        right =
                            generateRustExpr target mode parentModule guidaRight
                    in
                    case name of
                        "add" ->
                            Rust.ExprInfix Rust.OpAdd left right

                        "sub" ->
                            Rust.ExprInfix Rust.OpSub left right

                        "mul" ->
                            Rust.ExprInfix Rust.OpMul left right

                        "fdiv" ->
                            Rust.ExprInfix Rust.OpDiv left right

                        "idiv" ->
                            Rust.ExprInfix Rust.OpBitwiseOr (Rust.ExprInfix Rust.OpDiv left right) (Rust.ExprInt 0)

                        "eq" ->
                            equal left right

                        "neq" ->
                            notEqual left right

                        "lt" ->
                            cmp Rust.OpLt Rust.OpLt 0 left right

                        "gt" ->
                            cmp Rust.OpGt Rust.OpGt 0 left right

                        "le" ->
                            cmp Rust.OpLe Rust.OpLt 1 left right

                        "ge" ->
                            cmp Rust.OpGe Rust.OpGt -1 left right

                        "or" ->
                            Rust.ExprInfix Rust.OpOr left right

                        "and" ->
                            Rust.ExprInfix Rust.OpAnd left right

                        "xor" ->
                            Rust.ExprInfix Rust.OpNe left right

                        "remainderBy" ->
                            Rust.ExprInfix Rust.OpMod right left

                        _ ->
                            generateGlobalCall parentModule pos home name [ left, right ]

        _ ->
            generateGlobalCall parentModule pos home name <| List.map (generateRustExpr target mode parentModule) args


equal : Rust.Expr -> Rust.Expr -> Rust.Expr
equal left right =
    if isLiteral left || isLiteral right then
        strictEq left right

    else
        Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.utils "eq")) [ left, right ]


notEqual : Rust.Expr -> Rust.Expr -> Rust.Expr
notEqual left right =
    if isLiteral left || isLiteral right then
        strictNEq left right

    else
        Rust.ExprPrefix Rust.PrefixNot <| Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.utils "eq")) [ left, right ]


cmp : Rust.InfixOp -> Rust.InfixOp -> Int -> Rust.Expr -> Rust.Expr -> Rust.Expr
cmp idealOp backupOp backupInt left right =
    if isLiteral left || isLiteral right then
        Rust.ExprInfix idealOp left right

    else
        Rust.ExprInfix backupOp
            (Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.utils "cmp")) [ left, right ])
            (Rust.ExprInt backupInt)


isLiteral : Rust.Expr -> Bool
isLiteral expr =
    case expr of
        Rust.ExprString _ ->
            True

        Rust.ExprTrackedString _ _ _ ->
            True

        Rust.ExprFloat _ ->
            True

        Rust.ExprTrackedFloat _ _ _ ->
            True

        Rust.ExprInt _ ->
            True

        Rust.ExprTrackedInt _ _ _ ->
            True

        Rust.ExprBool _ ->
            True

        Rust.ExprTrackedBool _ _ _ ->
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


append : Target -> Mode.Mode -> IO.Canonical -> Opt.Expr -> Opt.Expr -> Rust.Expr
append target mode parentModule left right =
    let
        seqs : List Rust.Expr
        seqs =
            generateRustExpr target mode parentModule left :: toSeqs target mode parentModule right
    in
    if List.any isStringLiteral seqs then
        Utils.foldr1 (Rust.ExprInfix Rust.OpAdd) seqs

    else
        Utils.foldr1 rustAppend seqs


rustAppend : Rust.Expr -> Rust.Expr -> Rust.Expr
rustAppend a b =
    Rust.ExprCall (Rust.ExprRef (RustName.fromKernel Name.utils "ap")) [ a, b ]


toSeqs : Target -> Mode.Mode -> IO.Canonical -> Opt.Expr -> List Rust.Expr
toSeqs target mode parentModule expr =
    case expr of
        Opt.Call _ (Opt.VarGlobal _ (Opt.Global home "append")) [ left, right ] ->
            if home == ModuleName.basics target then
                generateRustExpr target mode parentModule left :: toSeqs target mode parentModule right

            else
                [ generateRustExpr target mode parentModule expr ]

        _ ->
            [ generateRustExpr target mode parentModule expr ]


isStringLiteral : Rust.Expr -> Bool
isStringLiteral expr =
    case expr of
        Rust.ExprString _ ->
            True

        Rust.ExprTrackedString _ _ _ ->
            True

        _ ->
            False



-- SIMPLIFY INFIX OPERATORS


strictEq : Rust.Expr -> Rust.Expr -> Rust.Expr
strictEq left right =
    case left of
        Rust.ExprInt 0 ->
            Rust.ExprPrefix Rust.PrefixNot right

        Rust.ExprTrackedInt _ _ 0 ->
            Rust.ExprPrefix Rust.PrefixNot right

        Rust.ExprBool bool ->
            if bool then
                right

            else
                Rust.ExprPrefix Rust.PrefixNot right

        Rust.ExprTrackedBool _ _ bool ->
            if bool then
                right

            else
                Rust.ExprPrefix Rust.PrefixNot right

        _ ->
            case right of
                Rust.ExprInt 0 ->
                    Rust.ExprPrefix Rust.PrefixNot left

                Rust.ExprTrackedInt _ _ 0 ->
                    Rust.ExprPrefix Rust.PrefixNot left

                Rust.ExprBool bool ->
                    if bool then
                        left

                    else
                        Rust.ExprPrefix Rust.PrefixNot left

                Rust.ExprTrackedBool _ _ bool ->
                    if bool then
                        left

                    else
                        Rust.ExprPrefix Rust.PrefixNot left

                _ ->
                    Rust.ExprInfix Rust.OpEq left right


strictNEq : Rust.Expr -> Rust.Expr -> Rust.Expr
strictNEq left right =
    case left of
        Rust.ExprInt 0 ->
            Rust.ExprPrefix Rust.PrefixNot (Rust.ExprPrefix Rust.PrefixNot right)

        Rust.ExprTrackedInt _ _ 0 ->
            Rust.ExprPrefix Rust.PrefixNot (Rust.ExprPrefix Rust.PrefixNot right)

        Rust.ExprBool bool ->
            if bool then
                Rust.ExprPrefix Rust.PrefixNot right

            else
                right

        Rust.ExprTrackedBool _ _ bool ->
            if bool then
                Rust.ExprPrefix Rust.PrefixNot right

            else
                right

        _ ->
            case right of
                Rust.ExprInt 0 ->
                    Rust.ExprPrefix Rust.PrefixNot (Rust.ExprPrefix Rust.PrefixNot left)

                Rust.ExprTrackedInt _ _ 0 ->
                    Rust.ExprPrefix Rust.PrefixNot (Rust.ExprPrefix Rust.PrefixNot left)

                Rust.ExprBool bool ->
                    if bool then
                        Rust.ExprPrefix Rust.PrefixNot left

                    else
                        left

                Rust.ExprTrackedBool _ _ bool ->
                    if bool then
                        Rust.ExprPrefix Rust.PrefixNot left

                    else
                        left

                _ ->
                    Rust.ExprInfix Rust.OpNe left right



-- TAIL CALL


{-| TODO check if JS minifiers collapse unnecessary temporary variables
-}
generateTailCall : Target -> Mode.Mode -> IO.Canonical -> Name.Name -> List ( Name.Name, Opt.Expr ) -> List Rust.Stmt
generateTailCall target mode parentModule name args =
    let
        toTempVars : ( String, Opt.Expr ) -> ( RustName.Name, Rust.Expr )
        toTempVars ( argName, arg ) =
            ( RustName.makeTemp argName, generateRustExpr target mode parentModule arg )

        toRealVars : ( Name.Name, b ) -> Rust.Stmt
        toRealVars ( argName, _ ) =
            Rust.ExprStmt <| Rust.ExprAssign (Rust.LRef (RustName.fromLocal argName)) (Rust.ExprRef (RustName.makeTemp argName))
    in
    Rust.Vars (List.map toTempVars args)
        :: List.map toRealVars args
        ++ [ Rust.Continue (Just (RustName.fromLocal name)) ]



-- DEFINITIONS


generateDef : Target -> Mode.Mode -> IO.Canonical -> Opt.Def -> Rust.Stmt
generateDef target mode parentModule def =
    case def of
        Opt.Def (A.Region start _) name body ->
            Rust.TrackedVar parentModule start (RustName.fromLocal name) (RustName.fromLocal name) (generateRustExpr target mode parentModule body)

        Opt.TailDef (A.Region start _) name argNames body ->
            Rust.TrackedVar parentModule start (RustName.fromLocal name) (RustName.fromLocal name) (codeToExpr (generateTailDef target mode parentModule name argNames body))


generateTailDef : Target -> Mode.Mode -> IO.Canonical -> Name.Name -> List (A.Located Name.Name) -> Opt.Expr -> Code
generateTailDef target mode parentModule name argNames body =
    generateTrackedFunction parentModule (List.map (\(A.At region argName) -> A.At region (RustName.fromLocal argName)) argNames) <|
        JsBlock
            [ Rust.Labelled (RustName.fromLocal name) <|
                Rust.While (Rust.ExprBool True) <|
                    codeToStmt <|
                        generate target mode parentModule body
            ]



-- PATHS


generatePath : Mode.Mode -> Opt.Path -> Rust.Expr
generatePath mode path =
    case path of
        Opt.Index index subPath ->
            Rust.ExprAccess (generatePath mode subPath) (RustName.fromIndex index)

        Opt.ArrayIndex index subPath ->
            Rust.ExprIndex (generatePath mode subPath) (Rust.ExprInt index)

        Opt.Root name ->
            Rust.ExprRef (RustName.fromLocal name)

        Opt.Field field subPath ->
            Rust.ExprAccess (generatePath mode subPath) (generateField mode field)

        Opt.Unbox subPath ->
            case mode of
                Mode.Dev _ ->
                    Rust.ExprAccess (generatePath mode subPath) (RustName.fromIndex Index.first)

                Mode.Prod _ ->
                    generatePath mode subPath



-- GENERATE IFS


generateIf : Target -> Mode.Mode -> IO.Canonical -> List ( Opt.Expr, Opt.Expr ) -> Opt.Expr -> Code
generateIf target mode parentModule givenBranches givenFinal =
    let
        ( branches, final ) =
            crushIfs givenBranches givenFinal

        convertBranch : ( Opt.Expr, Opt.Expr ) -> ( Rust.Expr, Code )
        convertBranch ( condition, expr ) =
            ( generateRustExpr target mode parentModule condition
            , generate target mode parentModule expr
            )

        branchExprs : List ( Rust.Expr, Code )
        branchExprs =
            List.map convertBranch branches

        finalCode : Code
        finalCode =
            generate target mode parentModule final
    in
    if isBlock finalCode || List.any (isBlock << Tuple.second) branchExprs then
        JsBlock [ List.foldr addStmtIf (codeToStmt finalCode) branchExprs ]

    else
        JsExpr (List.foldr addExprIf (codeToExpr finalCode) branchExprs)


addExprIf : ( Rust.Expr, Code ) -> Rust.Expr -> Rust.Expr
addExprIf ( condition, branch ) final =
    Rust.ExprIf condition (codeToExpr branch) final


addStmtIf : ( Rust.Expr, Code ) -> Rust.Stmt -> Rust.Stmt
addStmtIf ( condition, branch ) final =
    Rust.IfStmt condition (codeToStmt branch) final


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


generateCase : Target -> Mode.Mode -> IO.Canonical -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> List ( Int, Opt.Expr ) -> List Rust.Stmt
generateCase target mode parentModule label root decider jumps =
    List.foldr (goto target mode parentModule label) (generateDecider target mode parentModule label root decider) jumps


goto : Target -> Mode.Mode -> IO.Canonical -> Name.Name -> ( Int, Opt.Expr ) -> List Rust.Stmt -> List Rust.Stmt
goto target mode parentModule label ( index, branch ) stmts =
    let
        labeledDeciderStmt : Rust.Stmt
        labeledDeciderStmt =
            Rust.Labelled
                (RustName.makeLabel label index)
                (Rust.While (Rust.ExprBool True) (Rust.Block stmts))
    in
    labeledDeciderStmt :: codeToStmtList (generate target mode parentModule branch)


generateDecider : Target -> Mode.Mode -> IO.Canonical -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> List Rust.Stmt
generateDecider target mode parentModule label root decisionTree =
    case decisionTree of
        Opt.Leaf (Opt.Inline branch) ->
            codeToStmtList (generate target mode parentModule branch)

        Opt.Leaf (Opt.Jump index) ->
            [ Rust.Break (Just (RustName.makeLabel label index)) ]

        Opt.Chain testChain success failure ->
            [ Rust.IfStmt
                (Utils.foldl1_ (Rust.ExprInfix Rust.OpAnd) (List.map (generateIfTest target mode root) testChain))
                (Rust.Block (generateDecider target mode parentModule label root success))
                (Rust.Block (generateDecider target mode parentModule label root failure))
            ]

        Opt.FanOut path edges fallback ->
            [ Rust.Switch
                (generateCaseTest target mode root path (Tuple.first (Prelude.head edges)))
                (List.foldr
                    (\edge cases -> generateCaseBranch target mode parentModule label root edge :: cases)
                    [ Rust.Default (generateDecider target mode parentModule label root fallback) ]
                    edges
                )
            ]


generateIfTest : Target -> Mode.Mode -> Name.Name -> ( DT.Path, DT.Test ) -> Rust.Expr
generateIfTest target mode root ( path, test ) =
    let
        value : Rust.Expr
        value =
            pathToJsExpr mode root path
    in
    case test of
        DT.IsCtor home name index _ opts ->
            let
                tag : Rust.Expr
                tag =
                    case mode of
                        Mode.Dev _ ->
                            Rust.ExprAccess value RustName.dollar

                        Mode.Prod _ ->
                            case opts of
                                Can.Normal ->
                                    Rust.ExprAccess value RustName.dollar

                                Can.Enum ->
                                    value

                                Can.Unbox ->
                                    value
            in
            strictEq tag
                (case mode of
                    Mode.Dev _ ->
                        Rust.ExprString name

                    Mode.Prod _ ->
                        Rust.ExprInt (ctorToInt target home name index)
                )

        DT.IsBool True ->
            value

        DT.IsBool False ->
            Rust.ExprPrefix Rust.PrefixNot value

        DT.IsInt int ->
            strictEq value (Rust.ExprInt int)

        DT.IsChr char ->
            strictEq (Rust.ExprString char)
                (case mode of
                    Mode.Dev _ ->
                        Rust.ExprCall (Rust.ExprAccess value (RustName.fromLocal "valueOf")) []

                    Mode.Prod _ ->
                        value
                )

        DT.IsStr string ->
            strictEq value (Rust.ExprString string)

        DT.IsCons ->
            Rust.ExprAccess value (RustName.fromLocal "b")

        DT.IsNil ->
            Rust.ExprPrefix Rust.PrefixNot <|
                Rust.ExprAccess value (RustName.fromLocal "b")

        DT.IsTuple ->
            crash "COMPILER BUG - there should never be tests on a tuple"


generateCaseBranch : Target -> Mode.Mode -> IO.Canonical -> Name.Name -> Name.Name -> ( DT.Test, Opt.Decider Opt.Choice ) -> Rust.Case
generateCaseBranch target mode parentModule label root ( test, subTree ) =
    Rust.Case
        (generateCaseValue target mode test)
        (generateDecider target mode parentModule label root subTree)


generateCaseValue : Target -> Mode.Mode -> DT.Test -> Rust.Expr
generateCaseValue target mode test =
    case test of
        DT.IsCtor home name index _ _ ->
            case mode of
                Mode.Dev _ ->
                    Rust.ExprString name

                Mode.Prod _ ->
                    Rust.ExprInt (ctorToInt target home name index)

        DT.IsInt int ->
            Rust.ExprInt int

        DT.IsChr char ->
            Rust.ExprString char

        DT.IsStr string ->
            Rust.ExprString string

        DT.IsBool _ ->
            crash "COMPILER BUG - there should never be three tests on a boolean"

        DT.IsCons ->
            crash "COMPILER BUG - there should never be three tests on a list"

        DT.IsNil ->
            crash "COMPILER BUG - there should never be three tests on a list"

        DT.IsTuple ->
            crash "COMPILER BUG - there should never be three tests on a tuple"


generateCaseTest : Target -> Mode.Mode -> Name.Name -> DT.Path -> DT.Test -> Rust.Expr
generateCaseTest target mode root path exampleTest =
    let
        value : Rust.Expr
        value =
            pathToJsExpr mode root path
    in
    case exampleTest of
        DT.IsCtor home name _ _ opts ->
            if name == Name.bool && (home == ModuleName.basics target) then
                value

            else
                case mode of
                    Mode.Dev _ ->
                        Rust.ExprAccess value RustName.dollar

                    Mode.Prod _ ->
                        case opts of
                            Can.Normal ->
                                Rust.ExprAccess value RustName.dollar

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
                    Rust.ExprCall (Rust.ExprAccess value (RustName.fromLocal "valueOf")) []

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


pathToJsExpr : Mode.Mode -> Name.Name -> DT.Path -> Rust.Expr
pathToJsExpr mode root path =
    case path of
        DT.Index index subPath ->
            Rust.ExprAccess (pathToJsExpr mode root subPath) (RustName.fromIndex index)

        DT.Unbox subPath ->
            case mode of
                Mode.Dev _ ->
                    Rust.ExprAccess (pathToJsExpr mode root subPath) (RustName.fromIndex Index.first)

                Mode.Prod _ ->
                    pathToJsExpr mode root subPath

        DT.Empty ->
            Rust.ExprRef (RustName.fromLocal root)



-- GENERATE MAIN


generateMain : Target -> Mode.Mode -> IO.Canonical -> Opt.Main -> Rust.Expr
generateMain target mode home main =
    case main of
        Opt.Static ->
            Rust.ExprRef (RustName.fromKernel Name.virtualDom "init")
                |> call (Rust.ExprRef (RustName.fromGlobal home "main"))
                |> call (Rust.ExprInt 0)
                |> call (Rust.ExprInt 0)

        Opt.Dynamic msgType decoder ->
            Rust.ExprRef (RustName.fromGlobal home "main")
                |> call (generateRustExpr target mode home decoder)
                |> call (toDebugMetadata target mode msgType)


call : Rust.Expr -> Rust.Expr -> Rust.Expr
call arg func =
    Rust.ExprCall func [ arg ]


toDebugMetadata : Target -> Mode.Mode -> Can.Type -> Rust.Expr
toDebugMetadata target mode msgType =
    case mode of
        Mode.Prod _ ->
            Rust.ExprInt 0

        Mode.Dev Nothing ->
            Rust.ExprInt 0

        Mode.Dev (Just interfaces) ->
            Rust.ExprJson
                (Encode.object
                    [ ( "versions"
                      , Encode.object
                            [ case target of
                                Target.GuidaTarget ->
                                    ( "guida", V.encode V.compiler )

                                Target.ElmTarget ->
                                    ( "elm", V.encode V.elmCompiler )
                            ]
                      )
                    , ( "types", Type.encodeMetadata (Extract.fromMsg target interfaces msgType) )
                    ]
                )
