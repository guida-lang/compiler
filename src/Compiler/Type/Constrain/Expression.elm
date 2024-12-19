module Compiler.Type.Constrain.Expression exposing
    ( RTV
    , constrainDef
    , constrainRecursiveDefs
    )

import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Type.Constrain.Pattern as Pattern
import Compiler.Type.Instantiate as Instantiate
import Compiler.Type.Type as Type exposing (Constraint(..), Type(..))
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)
import Types as T
import Utils.Main as Utils



-- CONSTRAIN


{-| As we step past type annotations, the free type variables are added to
the "rigid type variables" dict. Allowing sharing of rigid variables
between nested type annotations.

So if you have a top-level type annotation like (func : a -> b) the RTV
dictionary will hold variables for `a` and `b`

-}
type alias RTV =
    Dict String T.CDN_Name Type


constrain : RTV -> T.CASTC_Expr -> T.CRET_Expected Type -> IO Constraint
constrain rtv (T.CRA_At region expression) expected =
    case expression of
        T.CASTC_VarLocal name ->
            IO.pure (CLocal region name expected)

        T.CASTC_VarTopLevel _ name ->
            IO.pure (CLocal region name expected)

        T.CASTC_VarKernel _ _ ->
            IO.pure CTrue

        T.CASTC_VarForeign _ name annotation ->
            IO.pure (CForeign region name annotation expected)

        T.CASTC_VarCtor _ _ name _ annotation ->
            IO.pure (CForeign region name annotation expected)

        T.CASTC_VarDebug _ name annotation ->
            IO.pure (CForeign region name annotation expected)

        T.CASTC_VarOperator op _ _ annotation ->
            IO.pure (CForeign region op annotation expected)

        T.CASTC_Str _ ->
            IO.pure (CEqual region T.CRET_String Type.string expected)

        T.CASTC_Chr _ ->
            IO.pure (CEqual region T.CRET_Char Type.char expected)

        T.CASTC_Int _ ->
            Type.mkFlexNumber
                |> IO.fmap
                    (\var ->
                        Type.exists [ var ] (CEqual region T.CRET_Number (VarN var) expected)
                    )

        T.CASTC_Float _ ->
            IO.pure (CEqual region T.CRET_Float Type.float expected)

        T.CASTC_List elements ->
            constrainList rtv region elements expected

        T.CASTC_Negate expr ->
            Type.mkFlexNumber
                |> IO.bind
                    (\numberVar ->
                        let
                            numberType : Type
                            numberType =
                                VarN numberVar
                        in
                        constrain rtv expr (T.CRET_FromContext region T.CRET_Negate numberType)
                            |> IO.fmap
                                (\numberCon ->
                                    let
                                        negateCon : Constraint
                                        negateCon =
                                            CEqual region T.CRET_Number numberType expected
                                    in
                                    Type.exists [ numberVar ] (CAnd [ numberCon, negateCon ])
                                )
                    )

        T.CASTC_Binop op _ _ annotation leftExpr rightExpr ->
            constrainBinop rtv region op annotation leftExpr rightExpr expected

        T.CASTC_Lambda args body ->
            constrainLambda rtv region args body expected

        T.CASTC_Call func args ->
            constrainCall rtv region func args expected

        T.CASTC_If branches finally ->
            constrainIf rtv region branches finally expected

        T.CASTC_Case expr branches ->
            constrainCase rtv region expr branches expected

        T.CASTC_Let def body ->
            IO.bind (constrainDef rtv def)
                (constrain rtv body expected)

        T.CASTC_LetRec defs body ->
            IO.bind (constrainRecursiveDefs rtv defs)
                (constrain rtv body expected)

        T.CASTC_LetDestruct pattern expr body ->
            IO.bind (constrainDestruct rtv region pattern expr)
                (constrain rtv body expected)

        T.CASTC_Accessor field ->
            Type.mkFlexVar
                |> IO.bind
                    (\extVar ->
                        Type.mkFlexVar
                            |> IO.fmap
                                (\fieldVar ->
                                    let
                                        extType : Type
                                        extType =
                                            VarN extVar

                                        fieldType : Type
                                        fieldType =
                                            VarN fieldVar

                                        recordType : Type
                                        recordType =
                                            RecordN (Dict.singleton identity field fieldType) extType
                                    in
                                    Type.exists [ fieldVar, extVar ] (CEqual region (T.CRET_Accessor field) (FunN recordType fieldType) expected)
                                )
                    )

        T.CASTC_Access expr (T.CRA_At accessRegion field) ->
            Type.mkFlexVar
                |> IO.bind
                    (\extVar ->
                        Type.mkFlexVar
                            |> IO.bind
                                (\fieldVar ->
                                    let
                                        extType : Type
                                        extType =
                                            VarN extVar

                                        fieldType : Type
                                        fieldType =
                                            VarN fieldVar

                                        recordType : Type
                                        recordType =
                                            RecordN (Dict.singleton identity field fieldType) extType

                                        context : T.CRET_Context
                                        context =
                                            T.CRET_RecordAccess (A.toRegion expr) (getAccessName expr) accessRegion field
                                    in
                                    constrain rtv expr (T.CRET_FromContext region context recordType)
                                        |> IO.fmap
                                            (\recordCon ->
                                                Type.exists [ fieldVar, extVar ] (CAnd [ recordCon, CEqual region (T.CRET_Access field) fieldType expected ])
                                            )
                                )
                    )

        T.CASTC_Update name expr fields ->
            constrainUpdate rtv region name expr fields expected

        T.CASTC_Record fields ->
            constrainRecord rtv region fields expected

        T.CASTC_Unit ->
            IO.pure (CEqual region T.CRET_Unit UnitN expected)

        T.CASTC_Tuple a b maybeC ->
            constrainTuple rtv region a b maybeC expected

        T.CASTC_Shader _ types ->
            constrainShader region types expected



-- CONSTRAIN LAMBDA


constrainLambda : RTV -> T.CRA_Region -> List T.CASTC_Pattern -> T.CASTC_Expr -> T.CRET_Expected Type -> IO Constraint
constrainLambda rtv region args body expected =
    constrainArgs args
        |> IO.bind
            (\(Args vars tipe resultType (Pattern.State headers pvars revCons)) ->
                constrain rtv body (T.CRET_NoExpectation resultType)
                    |> IO.fmap
                        (\bodyCon ->
                            Type.exists vars <|
                                CAnd
                                    [ CLet []
                                        pvars
                                        headers
                                        (CAnd (List.reverse revCons))
                                        bodyCon
                                    , CEqual region T.CRET_Lambda tipe expected
                                    ]
                        )
            )



-- CONSTRAIN CALL


constrainCall : RTV -> T.CRA_Region -> T.CASTC_Expr -> List T.CASTC_Expr -> T.CRET_Expected Type -> IO Constraint
constrainCall rtv region ((T.CRA_At funcRegion _) as func) args expected =
    let
        maybeName : T.CRET_MaybeName
        maybeName =
            getName func
    in
    Type.mkFlexVar
        |> IO.bind
            (\funcVar ->
                Type.mkFlexVar
                    |> IO.bind
                        (\resultVar ->
                            let
                                funcType : Type
                                funcType =
                                    VarN funcVar

                                resultType : Type
                                resultType =
                                    VarN resultVar
                            in
                            constrain rtv func (T.CRET_NoExpectation funcType)
                                |> IO.bind
                                    (\funcCon ->
                                        IO.fmap Utils.unzip3 (IO.traverseIndexed (constrainArg rtv region maybeName) args)
                                            |> IO.fmap
                                                (\( argVars, argTypes, argCons ) ->
                                                    let
                                                        arityType : Type
                                                        arityType =
                                                            List.foldr FunN resultType argTypes

                                                        category : T.CRET_Category
                                                        category =
                                                            T.CRET_CallResult maybeName
                                                    in
                                                    Type.exists (funcVar :: resultVar :: argVars)
                                                        (CAnd
                                                            [ funcCon
                                                            , CEqual funcRegion category funcType (T.CRET_FromContext region (T.CRET_CallArity maybeName (List.length args)) arityType)
                                                            , CAnd argCons
                                                            , CEqual region category resultType expected
                                                            ]
                                                        )
                                                )
                                    )
                        )
            )


constrainArg : RTV -> T.CRA_Region -> T.CRET_MaybeName -> T.CDI_ZeroBased -> T.CASTC_Expr -> IO ( IO.Variable, Type, Constraint )
constrainArg rtv region maybeName index arg =
    Type.mkFlexVar
        |> IO.bind
            (\argVar ->
                let
                    argType : Type
                    argType =
                        VarN argVar
                in
                constrain rtv arg (T.CRET_FromContext region (T.CRET_CallArg maybeName index) argType)
                    |> IO.fmap
                        (\argCon ->
                            ( argVar, argType, argCon )
                        )
            )


getName : T.CASTC_Expr -> T.CRET_MaybeName
getName (T.CRA_At _ expr) =
    case expr of
        T.CASTC_VarLocal name ->
            T.CRET_FuncName name

        T.CASTC_VarTopLevel _ name ->
            T.CRET_FuncName name

        T.CASTC_VarForeign _ name _ ->
            T.CRET_FuncName name

        T.CASTC_VarCtor _ _ name _ _ ->
            T.CRET_CtorName name

        T.CASTC_VarOperator op _ _ _ ->
            T.CRET_OpName op

        T.CASTC_VarKernel _ name ->
            T.CRET_FuncName name

        _ ->
            T.CRET_NoName


getAccessName : T.CASTC_Expr -> Maybe T.CDN_Name
getAccessName (T.CRA_At _ expr) =
    case expr of
        T.CASTC_VarLocal name ->
            Just name

        T.CASTC_VarTopLevel _ name ->
            Just name

        T.CASTC_VarForeign _ name _ ->
            Just name

        _ ->
            Nothing



-- CONSTRAIN BINOP


constrainBinop : RTV -> T.CRA_Region -> T.CDN_Name -> T.CASTC_Annotation -> T.CASTC_Expr -> T.CASTC_Expr -> T.CRET_Expected Type -> IO Constraint
constrainBinop rtv region op annotation leftExpr rightExpr expected =
    Type.mkFlexVar
        |> IO.bind
            (\leftVar ->
                Type.mkFlexVar
                    |> IO.bind
                        (\rightVar ->
                            Type.mkFlexVar
                                |> IO.bind
                                    (\answerVar ->
                                        let
                                            leftType : Type
                                            leftType =
                                                VarN leftVar

                                            rightType : Type
                                            rightType =
                                                VarN rightVar

                                            answerType : Type
                                            answerType =
                                                VarN answerVar

                                            binopType : Type
                                            binopType =
                                                Type.funType leftType (Type.funType rightType answerType)

                                            opCon : Constraint
                                            opCon =
                                                CForeign region op annotation (T.CRET_NoExpectation binopType)
                                        in
                                        constrain rtv leftExpr (T.CRET_FromContext region (T.CRET_OpLeft op) leftType)
                                            |> IO.bind
                                                (\leftCon ->
                                                    constrain rtv rightExpr (T.CRET_FromContext region (T.CRET_OpRight op) rightType)
                                                        |> IO.fmap
                                                            (\rightCon ->
                                                                Type.exists [ leftVar, rightVar, answerVar ]
                                                                    (CAnd
                                                                        [ opCon
                                                                        , leftCon
                                                                        , rightCon
                                                                        , CEqual region (T.CRET_CallResult (T.CRET_OpName op)) answerType expected
                                                                        ]
                                                                    )
                                                            )
                                                )
                                    )
                        )
            )



-- CONSTRAIN LISTS


constrainList : RTV -> T.CRA_Region -> List T.CASTC_Expr -> T.CRET_Expected Type -> IO Constraint
constrainList rtv region entries expected =
    Type.mkFlexVar
        |> IO.bind
            (\entryVar ->
                let
                    entryType : Type
                    entryType =
                        VarN entryVar

                    listType : Type
                    listType =
                        AppN ModuleName.list Name.list [ entryType ]
                in
                IO.traverseIndexed (constrainListEntry rtv region entryType) entries
                    |> IO.fmap
                        (\entryCons ->
                            Type.exists [ entryVar ]
                                (CAnd
                                    [ CAnd entryCons
                                    , CEqual region T.CRET_List listType expected
                                    ]
                                )
                        )
            )


constrainListEntry : RTV -> T.CRA_Region -> Type -> T.CDI_ZeroBased -> T.CASTC_Expr -> IO Constraint
constrainListEntry rtv region tipe index expr =
    constrain rtv expr (T.CRET_FromContext region (T.CRET_ListEntry index) tipe)



-- CONSTRAIN IF EXPRESSIONS


constrainIf : RTV -> T.CRA_Region -> List ( T.CASTC_Expr, T.CASTC_Expr ) -> T.CASTC_Expr -> T.CRET_Expected Type -> IO Constraint
constrainIf rtv region branches final expected =
    let
        boolExpect : T.CRET_Expected Type
        boolExpect =
            T.CRET_FromContext region T.CRET_IfCondition Type.bool

        ( conditions, exprs ) =
            List.foldr (\( c, e ) ( cs, es ) -> ( c :: cs, e :: es )) ( [], [ final ] ) branches
    in
    IO.traverseList (\c -> constrain rtv c boolExpect) conditions
        |> IO.bind
            (\condCons ->
                case expected of
                    T.CRET_FromAnnotation name arity _ tipe ->
                        IO.indexedForA exprs (\index expr -> constrain rtv expr (T.CRET_FromAnnotation name arity (T.CRET_TypedIfBranch index) tipe))
                            |> IO.fmap
                                (\branchCons ->
                                    CAnd (CAnd condCons :: branchCons)
                                )

                    _ ->
                        Type.mkFlexVar
                            |> IO.bind
                                (\branchVar ->
                                    let
                                        branchType : Type
                                        branchType =
                                            VarN branchVar
                                    in
                                    IO.indexedForA exprs
                                        (\index expr ->
                                            constrain rtv expr (T.CRET_FromContext region (T.CRET_IfBranch index) branchType)
                                        )
                                        |> IO.fmap
                                            (\branchCons ->
                                                Type.exists [ branchVar ]
                                                    (CAnd
                                                        [ CAnd condCons
                                                        , CAnd branchCons
                                                        , CEqual region T.CRET_If branchType expected
                                                        ]
                                                    )
                                            )
                                )
            )



-- CONSTRAIN CASE EXPRESSIONS


constrainCase : RTV -> T.CRA_Region -> T.CASTC_Expr -> List T.CASTC_CaseBranch -> T.CRET_Expected Type -> IO Constraint
constrainCase rtv region expr branches expected =
    Type.mkFlexVar
        |> IO.bind
            (\ptrnVar ->
                let
                    ptrnType : Type
                    ptrnType =
                        VarN ptrnVar
                in
                constrain rtv expr (T.CRET_NoExpectation ptrnType)
                    |> IO.bind
                        (\exprCon ->
                            case expected of
                                T.CRET_FromAnnotation name arity _ tipe ->
                                    IO.indexedForA branches
                                        (\index branch ->
                                            constrainCaseBranch rtv
                                                branch
                                                (T.CRET_PFromContext region (T.CRET_PCaseMatch index) ptrnType)
                                                (T.CRET_FromAnnotation name arity (T.CRET_TypedCaseBranch index) tipe)
                                        )
                                        |> IO.fmap
                                            (\branchCons ->
                                                Type.exists [ ptrnVar ] (CAnd (exprCon :: branchCons))
                                            )

                                _ ->
                                    Type.mkFlexVar
                                        |> IO.bind
                                            (\branchVar ->
                                                let
                                                    branchType : Type
                                                    branchType =
                                                        VarN branchVar
                                                in
                                                IO.indexedForA branches
                                                    (\index branch ->
                                                        constrainCaseBranch rtv
                                                            branch
                                                            (T.CRET_PFromContext region (T.CRET_PCaseMatch index) ptrnType)
                                                            (T.CRET_FromContext region (T.CRET_CaseBranch index) branchType)
                                                    )
                                                    |> IO.fmap
                                                        (\branchCons ->
                                                            Type.exists [ ptrnVar, branchVar ]
                                                                (CAnd
                                                                    [ exprCon
                                                                    , CAnd branchCons
                                                                    , CEqual region T.CRET_Case branchType expected
                                                                    ]
                                                                )
                                                        )
                                            )
                        )
            )


constrainCaseBranch : RTV -> T.CASTC_CaseBranch -> T.CRET_PExpected Type -> T.CRET_Expected Type -> IO Constraint
constrainCaseBranch rtv (T.CASTC_CaseBranch pattern expr) pExpect bExpect =
    Pattern.add pattern pExpect Pattern.emptyState
        |> IO.bind
            (\(Pattern.State headers pvars revCons) ->
                IO.fmap (CLet [] pvars headers (CAnd (List.reverse revCons)))
                    (constrain rtv expr bExpect)
            )



-- CONSTRAIN RECORD


constrainRecord : RTV -> T.CRA_Region -> Dict String T.CDN_Name T.CASTC_Expr -> T.CRET_Expected Type -> IO Constraint
constrainRecord rtv region fields expected =
    IO.traverseMap identity compare (constrainField rtv) fields
        |> IO.fmap
            (\dict ->
                let
                    getType : a -> ( b, c, d ) -> c
                    getType _ ( _, t, _ ) =
                        t

                    recordType : Type
                    recordType =
                        RecordN (Dict.map getType dict) EmptyRecordN

                    recordCon : Constraint
                    recordCon =
                        CEqual region T.CRET_Record recordType expected

                    vars : List IO.Variable
                    vars =
                        Dict.foldr compare (\_ ( v, _, _ ) vs -> v :: vs) [] dict

                    cons : List Constraint
                    cons =
                        Dict.foldr compare (\_ ( _, _, c ) cs -> c :: cs) [ recordCon ] dict
                in
                Type.exists vars (CAnd cons)
            )


constrainField : RTV -> T.CASTC_Expr -> IO ( IO.Variable, Type, Constraint )
constrainField rtv expr =
    Type.mkFlexVar
        |> IO.bind
            (\var ->
                let
                    tipe : Type
                    tipe =
                        VarN var
                in
                constrain rtv expr (T.CRET_NoExpectation tipe)
                    |> IO.fmap
                        (\con ->
                            ( var, tipe, con )
                        )
            )



-- CONSTRAIN RECORD UPDATE


constrainUpdate : RTV -> T.CRA_Region -> T.CDN_Name -> T.CASTC_Expr -> Dict String T.CDN_Name T.CASTC_FieldUpdate -> T.CRET_Expected Type -> IO Constraint
constrainUpdate rtv region name expr fields expected =
    Type.mkFlexVar
        |> IO.bind
            (\extVar ->
                IO.traverseMapWithKey identity compare (constrainUpdateField rtv region) fields
                    |> IO.bind
                        (\fieldDict ->
                            Type.mkFlexVar
                                |> IO.bind
                                    (\recordVar ->
                                        let
                                            recordType : Type
                                            recordType =
                                                VarN recordVar

                                            fieldsType : Type
                                            fieldsType =
                                                RecordN (Dict.map (\_ ( _, t, _ ) -> t) fieldDict) (VarN extVar)

                                            -- NOTE: fieldsType is separate so that Error propagates better
                                            fieldsCon : Constraint
                                            fieldsCon =
                                                CEqual region T.CRET_Record recordType (T.CRET_NoExpectation fieldsType)

                                            recordCon : Constraint
                                            recordCon =
                                                CEqual region T.CRET_Record recordType expected

                                            vars : List IO.Variable
                                            vars =
                                                Dict.foldr compare (\_ ( v, _, _ ) vs -> v :: vs) [ recordVar, extVar ] fieldDict

                                            cons : List Constraint
                                            cons =
                                                Dict.foldr compare (\_ ( _, _, c ) cs -> c :: cs) [ recordCon ] fieldDict
                                        in
                                        constrain rtv expr (T.CRET_FromContext region (T.CRET_RecordUpdateKeys name fields) recordType)
                                            |> IO.fmap (\con -> Type.exists vars (CAnd (fieldsCon :: con :: cons)))
                                    )
                        )
            )


constrainUpdateField : RTV -> T.CRA_Region -> T.CDN_Name -> T.CASTC_FieldUpdate -> IO ( IO.Variable, Type, Constraint )
constrainUpdateField rtv region field (T.CASTC_FieldUpdate _ expr) =
    Type.mkFlexVar
        |> IO.bind
            (\var ->
                let
                    tipe : Type
                    tipe =
                        VarN var
                in
                constrain rtv expr (T.CRET_FromContext region (T.CRET_RecordUpdateValue field) tipe)
                    |> IO.fmap (\con -> ( var, tipe, con ))
            )



-- CONSTRAIN TUPLE


constrainTuple : RTV -> T.CRA_Region -> T.CASTC_Expr -> T.CASTC_Expr -> Maybe T.CASTC_Expr -> T.CRET_Expected Type -> IO Constraint
constrainTuple rtv region a b maybeC expected =
    Type.mkFlexVar
        |> IO.bind
            (\aVar ->
                Type.mkFlexVar
                    |> IO.bind
                        (\bVar ->
                            let
                                aType : Type
                                aType =
                                    VarN aVar

                                bType : Type
                                bType =
                                    VarN bVar
                            in
                            constrain rtv a (T.CRET_NoExpectation aType)
                                |> IO.bind
                                    (\aCon ->
                                        constrain rtv b (T.CRET_NoExpectation bType)
                                            |> IO.bind
                                                (\bCon ->
                                                    case maybeC of
                                                        Nothing ->
                                                            let
                                                                tupleType : Type
                                                                tupleType =
                                                                    TupleN aType bType Nothing

                                                                tupleCon : Constraint
                                                                tupleCon =
                                                                    CEqual region T.CRET_Tuple tupleType expected
                                                            in
                                                            IO.pure (Type.exists [ aVar, bVar ] (CAnd [ aCon, bCon, tupleCon ]))

                                                        Just c ->
                                                            Type.mkFlexVar
                                                                |> IO.bind
                                                                    (\cVar ->
                                                                        let
                                                                            cType : Type
                                                                            cType =
                                                                                VarN cVar
                                                                        in
                                                                        constrain rtv c (T.CRET_NoExpectation cType)
                                                                            |> IO.fmap
                                                                                (\cCon ->
                                                                                    let
                                                                                        tupleType : Type
                                                                                        tupleType =
                                                                                            TupleN aType bType (Just cType)

                                                                                        tupleCon : Constraint
                                                                                        tupleCon =
                                                                                            CEqual region T.CRET_Tuple tupleType expected
                                                                                    in
                                                                                    Type.exists [ aVar, bVar, cVar ] (CAnd [ aCon, bCon, cCon, tupleCon ])
                                                                                )
                                                                    )
                                                )
                                    )
                        )
            )



-- CONSTRAIN SHADER


constrainShader : T.CRA_Region -> T.CASTUS_Types -> T.CRET_Expected Type -> IO Constraint
constrainShader region (T.CASTUS_Types attributes uniforms varyings) expected =
    Type.mkFlexVar
        |> IO.bind
            (\attrVar ->
                Type.mkFlexVar
                    |> IO.fmap
                        (\unifVar ->
                            let
                                attrType : Type
                                attrType =
                                    VarN attrVar

                                unifType : Type
                                unifType =
                                    VarN unifVar

                                shaderType : Type
                                shaderType =
                                    AppN ModuleName.webgl
                                        Name.shader
                                        [ toShaderRecord attributes attrType
                                        , toShaderRecord uniforms unifType
                                        , toShaderRecord varyings EmptyRecordN
                                        ]
                            in
                            Type.exists [ attrVar, unifVar ] (CEqual region T.CRET_Shader shaderType expected)
                        )
            )


toShaderRecord : Dict String T.CDN_Name T.CASTUS_Type -> Type -> Type
toShaderRecord types baseRecType =
    if Dict.isEmpty types then
        baseRecType

    else
        RecordN (Dict.map (\_ -> glToType) types) baseRecType


glToType : T.CASTUS_Type -> Type
glToType glType =
    case glType of
        T.CASTUS_V2 ->
            Type.vec2

        T.CASTUS_V3 ->
            Type.vec3

        T.CASTUS_V4 ->
            Type.vec4

        T.CASTUS_M4 ->
            Type.mat4

        T.CASTUS_Int ->
            Type.int

        T.CASTUS_Float ->
            Type.float

        T.CASTUS_Texture ->
            Type.texture



-- CONSTRAIN DESTRUCTURES


constrainDestruct : RTV -> T.CRA_Region -> T.CASTC_Pattern -> T.CASTC_Expr -> Constraint -> IO Constraint
constrainDestruct rtv region pattern expr bodyCon =
    Type.mkFlexVar
        |> IO.bind
            (\patternVar ->
                let
                    patternType : Type
                    patternType =
                        VarN patternVar
                in
                Pattern.add pattern (T.CRET_PNoExpectation patternType) Pattern.emptyState
                    |> IO.bind
                        (\(Pattern.State headers pvars revCons) ->
                            constrain rtv expr (T.CRET_FromContext region T.CRET_Destructure patternType)
                                |> IO.fmap
                                    (\exprCon ->
                                        CLet [] (patternVar :: pvars) headers (CAnd (List.reverse (exprCon :: revCons))) bodyCon
                                    )
                        )
            )



-- CONSTRAIN DEF


constrainDef : RTV -> T.CASTC_Def -> Constraint -> IO Constraint
constrainDef rtv def bodyCon =
    case def of
        T.CASTC_Def (T.CRA_At region name) args expr ->
            constrainArgs args
                |> IO.bind
                    (\(Args vars tipe resultType (Pattern.State headers pvars revCons)) ->
                        constrain rtv expr (T.CRET_NoExpectation resultType)
                            |> IO.fmap
                                (\exprCon ->
                                    CLet []
                                        vars
                                        (Dict.singleton identity name (T.CRA_At region tipe))
                                        (CLet []
                                            pvars
                                            headers
                                            (CAnd (List.reverse revCons))
                                            exprCon
                                        )
                                        bodyCon
                                )
                    )

        T.CASTC_TypedDef (T.CRA_At region name) freeVars typedArgs expr srcResultType ->
            let
                newNames : Dict String T.CDN_Name ()
                newNames =
                    Dict.diff freeVars rtv
            in
            IO.traverseMapWithKey identity compare (\n _ -> Type.nameToRigid n) newNames
                |> IO.bind
                    (\newRigids ->
                        let
                            newRtv : Dict String T.CDN_Name Type
                            newRtv =
                                Dict.union rtv (Dict.map (\_ -> VarN) newRigids)
                        in
                        constrainTypedArgs newRtv name typedArgs srcResultType
                            |> IO.bind
                                (\(TypedArgs tipe resultType (Pattern.State headers pvars revCons)) ->
                                    let
                                        expected : T.CRET_Expected Type
                                        expected =
                                            T.CRET_FromAnnotation name (List.length typedArgs) T.CRET_TypedBody resultType
                                    in
                                    constrain newRtv expr expected
                                        |> IO.fmap
                                            (\exprCon ->
                                                CLet (Dict.values compare newRigids)
                                                    []
                                                    (Dict.singleton identity name (T.CRA_At region tipe))
                                                    (CLet []
                                                        pvars
                                                        headers
                                                        (CAnd (List.reverse revCons))
                                                        exprCon
                                                    )
                                                    bodyCon
                                            )
                                )
                    )



-- CONSTRAIN RECURSIVE DEFS


type Info
    = Info (List IO.Variable) (List Constraint) (Dict String T.CDN_Name (T.CRA_Located Type))


emptyInfo : Info
emptyInfo =
    Info [] [] Dict.empty


constrainRecursiveDefs : RTV -> List T.CASTC_Def -> Constraint -> IO Constraint
constrainRecursiveDefs rtv defs bodyCon =
    recDefsHelp rtv defs bodyCon emptyInfo emptyInfo


recDefsHelp : RTV -> List T.CASTC_Def -> Constraint -> Info -> Info -> IO Constraint
recDefsHelp rtv defs bodyCon rigidInfo flexInfo =
    case defs of
        [] ->
            let
                (Info rigidVars rigidCons rigidHeaders) =
                    rigidInfo

                (Info flexVars flexCons flexHeaders) =
                    flexInfo
            in
            IO.pure <|
                CLet rigidVars [] rigidHeaders CTrue <|
                    CLet [] flexVars flexHeaders (CLet [] [] flexHeaders CTrue (CAnd flexCons)) <|
                        CAnd [ CAnd rigidCons, bodyCon ]

        def :: otherDefs ->
            case def of
                T.CASTC_Def (T.CRA_At region name) args expr ->
                    let
                        (Info flexVars flexCons flexHeaders) =
                            flexInfo
                    in
                    argsHelp args (Pattern.State Dict.empty flexVars [])
                        |> IO.bind
                            (\(Args newFlexVars tipe resultType (Pattern.State headers pvars revCons)) ->
                                constrain rtv expr (T.CRET_NoExpectation resultType)
                                    |> IO.bind
                                        (\exprCon ->
                                            let
                                                defCon : Constraint
                                                defCon =
                                                    CLet []
                                                        pvars
                                                        headers
                                                        (CAnd (List.reverse revCons))
                                                        exprCon
                                            in
                                            recDefsHelp rtv otherDefs bodyCon rigidInfo <|
                                                Info newFlexVars
                                                    (defCon :: flexCons)
                                                    (Dict.insert identity name (T.CRA_At region tipe) flexHeaders)
                                        )
                            )

                T.CASTC_TypedDef (T.CRA_At region name) freeVars typedArgs expr srcResultType ->
                    let
                        newNames : Dict String T.CDN_Name ()
                        newNames =
                            Dict.diff freeVars rtv
                    in
                    IO.traverseMapWithKey identity compare (\n _ -> Type.nameToRigid n) newNames
                        |> IO.bind
                            (\newRigids ->
                                let
                                    newRtv : Dict String T.CDN_Name Type
                                    newRtv =
                                        Dict.union rtv (Dict.map (\_ -> VarN) newRigids)
                                in
                                constrainTypedArgs newRtv name typedArgs srcResultType
                                    |> IO.bind
                                        (\(TypedArgs tipe resultType (Pattern.State headers pvars revCons)) ->
                                            constrain newRtv expr (T.CRET_FromAnnotation name (List.length typedArgs) T.CRET_TypedBody resultType)
                                                |> IO.bind
                                                    (\exprCon ->
                                                        let
                                                            defCon : Constraint
                                                            defCon =
                                                                CLet []
                                                                    pvars
                                                                    headers
                                                                    (CAnd (List.reverse revCons))
                                                                    exprCon

                                                            (Info rigidVars rigidCons rigidHeaders) =
                                                                rigidInfo
                                                        in
                                                        recDefsHelp rtv
                                                            otherDefs
                                                            bodyCon
                                                            (Info
                                                                (Dict.foldr compare (\_ -> (::)) rigidVars newRigids)
                                                                (CLet (Dict.values compare newRigids) [] Dict.empty defCon CTrue :: rigidCons)
                                                                (Dict.insert identity name (T.CRA_At region tipe) rigidHeaders)
                                                            )
                                                            flexInfo
                                                    )
                                        )
                            )



-- CONSTRAIN ARGS


type Args
    = Args (List IO.Variable) Type Type Pattern.State


constrainArgs : List T.CASTC_Pattern -> IO Args
constrainArgs args =
    argsHelp args Pattern.emptyState


argsHelp : List T.CASTC_Pattern -> Pattern.State -> IO Args
argsHelp args state =
    case args of
        [] ->
            Type.mkFlexVar
                |> IO.fmap
                    (\resultVar ->
                        let
                            resultType : Type
                            resultType =
                                VarN resultVar
                        in
                        Args [ resultVar ] resultType resultType state
                    )

        pattern :: otherArgs ->
            Type.mkFlexVar
                |> IO.bind
                    (\argVar ->
                        let
                            argType : Type
                            argType =
                                VarN argVar
                        in
                        Pattern.add pattern (T.CRET_PNoExpectation argType) state
                            |> IO.bind (argsHelp otherArgs)
                            |> IO.fmap
                                (\(Args vars tipe result newState) ->
                                    Args (argVar :: vars) (FunN argType tipe) result newState
                                )
                    )



-- CONSTRAIN TYPED ARGS


type TypedArgs
    = TypedArgs Type Type Pattern.State


constrainTypedArgs : Dict String T.CDN_Name Type -> T.CDN_Name -> List ( T.CASTC_Pattern, T.CASTC_Type ) -> T.CASTC_Type -> IO TypedArgs
constrainTypedArgs rtv name args srcResultType =
    typedArgsHelp rtv name Index.first args srcResultType Pattern.emptyState


typedArgsHelp : Dict String T.CDN_Name Type -> T.CDN_Name -> T.CDI_ZeroBased -> List ( T.CASTC_Pattern, T.CASTC_Type ) -> T.CASTC_Type -> Pattern.State -> IO TypedArgs
typedArgsHelp rtv name index args srcResultType state =
    case args of
        [] ->
            Instantiate.fromSrcType rtv srcResultType
                |> IO.fmap
                    (\resultType ->
                        TypedArgs resultType resultType state
                    )

        ( (T.CRA_At region _) as pattern, srcType ) :: otherArgs ->
            Instantiate.fromSrcType rtv srcType
                |> IO.bind
                    (\argType ->
                        let
                            expected : T.CRET_PExpected Type
                            expected =
                                T.CRET_PFromContext region (T.CRET_PTypedArg name index) argType
                        in
                        Pattern.add pattern expected state
                            |> IO.bind (typedArgsHelp rtv name (Index.next index) otherArgs srcResultType)
                            |> IO.fmap
                                (\(TypedArgs tipe resultType newState) ->
                                    TypedArgs (FunN argType tipe) resultType newState
                                )
                    )
