module Compiler.Type.Constrain.Expression exposing
    ( RTV
    , constrainDef
    , constrainRecursiveDefs
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E exposing (Category(..), Context(..), Expected(..), MaybeName(..), PContext(..), PExpected(..), SubContext(..))
import Compiler.Type.Constrain.Pattern as Pattern
import Compiler.Type.Instantiate as Instantiate
import Compiler.Type.Type as Type exposing (Constraint(..), Type(..))
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)
import Utils.Main as Utils



-- CONSTRAIN


{-| As we step past type annotations, the free type variables are added to
the "rigid type variables" dict. Allowing sharing of rigid variables
between nested type annotations.

So if you have a top-level type annotation like (func : a -> b) the RTV
dictionary will hold variables for `a` and `b`

-}
type alias RTV =
    Dict String Name.Name Type


constrain : Target -> RTV -> Can.Expr -> E.Expected Type -> IO Constraint
constrain target rtv (A.At region expression) expected =
    case expression of
        Can.VarLocal name ->
            IO.pure (CLocal region name expected)

        Can.VarTopLevel _ name ->
            IO.pure (CLocal region name expected)

        Can.VarKernel _ _ ->
            IO.pure CTrue

        Can.VarForeign _ name annotation ->
            IO.pure (CForeign region name annotation expected)

        Can.VarCtor _ _ name _ annotation ->
            IO.pure (CForeign region name annotation expected)

        Can.VarDebug _ name annotation ->
            IO.pure (CForeign region name annotation expected)

        Can.VarOperator op _ _ annotation ->
            IO.pure (CForeign region op annotation expected)

        Can.Str _ ->
            IO.pure (CEqual region String (Type.string target) expected)

        Can.Chr _ ->
            IO.pure (CEqual region Char (Type.char target) expected)

        Can.Int _ ->
            Type.mkFlexNumber
                |> IO.fmap
                    (\var ->
                        Type.exists [ var ] (CEqual region E.Number (VarN var) expected)
                    )

        Can.Float _ ->
            IO.pure (CEqual region Float (Type.float target) expected)

        Can.List elements ->
            constrainList target rtv region elements expected

        Can.Negate expr ->
            Type.mkFlexNumber
                |> IO.bind
                    (\numberVar ->
                        let
                            numberType : Type
                            numberType =
                                VarN numberVar
                        in
                        constrain target rtv expr (FromContext target region Negate numberType)
                            |> IO.fmap
                                (\numberCon ->
                                    let
                                        negateCon : Constraint
                                        negateCon =
                                            CEqual region E.Number numberType expected
                                    in
                                    Type.exists [ numberVar ] (CAnd [ numberCon, negateCon ])
                                )
                    )

        Can.Binop op _ _ annotation leftExpr rightExpr ->
            constrainBinop target rtv region op annotation leftExpr rightExpr expected

        Can.Lambda args body ->
            constrainLambda target rtv region args body expected

        Can.Call func args ->
            constrainCall target rtv region func args expected

        Can.If branches finally ->
            constrainIf target rtv region branches finally expected

        Can.Case expr branches ->
            constrainCase target rtv region expr branches expected

        Can.Let def body ->
            IO.bind (constrainDef target rtv def)
                (constrain target rtv body expected)

        Can.LetRec defs body ->
            IO.bind (constrainRecursiveDefs target rtv defs)
                (constrain target rtv body expected)

        Can.LetDestruct pattern expr body ->
            IO.bind (constrainDestruct target rtv region pattern expr)
                (constrain target rtv body expected)

        Can.Accessor field ->
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
                                    Type.exists [ fieldVar, extVar ] (CEqual region (Accessor field) (FunN recordType fieldType) expected)
                                )
                    )

        Can.Access expr (A.At accessRegion field) ->
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

                                        context : Context
                                        context =
                                            RecordAccess (A.toRegion expr) (getAccessName expr) accessRegion field
                                    in
                                    constrain target rtv expr (FromContext target region context recordType)
                                        |> IO.fmap
                                            (\recordCon ->
                                                Type.exists [ fieldVar, extVar ] (CAnd [ recordCon, CEqual region (Access field) fieldType expected ])
                                            )
                                )
                    )

        Can.Update expr fields ->
            constrainUpdate target rtv region expr fields expected

        Can.Record fields ->
            constrainRecord target rtv region fields expected

        Can.Unit ->
            IO.pure (CEqual region Unit UnitN expected)

        Can.Tuple a b cs ->
            constrainTuple target rtv region a b cs expected

        Can.Shader _ types ->
            constrainShader target region types expected



-- CONSTRAIN LAMBDA


constrainLambda : Target -> RTV -> A.Region -> List Can.Pattern -> Can.Expr -> E.Expected Type -> IO Constraint
constrainLambda target rtv region args body expected =
    constrainArgs target args
        |> IO.bind
            (\(Args vars tipe resultType (Pattern.State headers pvars revCons)) ->
                constrain target rtv body (NoExpectation target resultType)
                    |> IO.fmap
                        (\bodyCon ->
                            Type.exists vars <|
                                CAnd
                                    [ CLet []
                                        pvars
                                        headers
                                        (CAnd (List.reverse revCons))
                                        bodyCon
                                    , CEqual region Lambda tipe expected
                                    ]
                        )
            )



-- CONSTRAIN CALL


constrainCall : Target -> RTV -> A.Region -> Can.Expr -> List Can.Expr -> E.Expected Type -> IO Constraint
constrainCall target rtv region ((A.At funcRegion _) as func) args expected =
    let
        maybeName : MaybeName
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
                            constrain target rtv func (E.NoExpectation target funcType)
                                |> IO.bind
                                    (\funcCon ->
                                        IO.fmap Utils.unzip3 (IO.traverseIndexed (constrainArg target rtv region maybeName) args)
                                            |> IO.fmap
                                                (\( argVars, argTypes, argCons ) ->
                                                    let
                                                        arityType : Type
                                                        arityType =
                                                            List.foldr FunN resultType argTypes

                                                        category : Category
                                                        category =
                                                            CallResult maybeName
                                                    in
                                                    Type.exists (funcVar :: resultVar :: argVars)
                                                        (CAnd
                                                            [ funcCon
                                                            , CEqual funcRegion category funcType (FromContext target region (CallArity maybeName (List.length args)) arityType)
                                                            , CAnd argCons
                                                            , CEqual region category resultType expected
                                                            ]
                                                        )
                                                )
                                    )
                        )
            )


constrainArg : Target -> RTV -> A.Region -> E.MaybeName -> Index.ZeroBased -> Can.Expr -> IO ( IO.Variable, Type, Constraint )
constrainArg target rtv region maybeName index arg =
    Type.mkFlexVar
        |> IO.bind
            (\argVar ->
                let
                    argType : Type
                    argType =
                        VarN argVar
                in
                constrain target rtv arg (FromContext target region (CallArg maybeName index) argType)
                    |> IO.fmap
                        (\argCon ->
                            ( argVar, argType, argCon )
                        )
            )


getName : Can.Expr -> MaybeName
getName (A.At _ expr) =
    case expr of
        Can.VarLocal name ->
            FuncName name

        Can.VarTopLevel _ name ->
            FuncName name

        Can.VarForeign _ name _ ->
            FuncName name

        Can.VarCtor _ _ name _ _ ->
            CtorName name

        Can.VarOperator op _ _ _ ->
            OpName op

        Can.VarKernel _ name ->
            FuncName name

        _ ->
            NoName


getAccessName : Can.Expr -> Maybe Name.Name
getAccessName (A.At _ expr) =
    case expr of
        Can.VarLocal name ->
            Just name

        Can.VarTopLevel _ name ->
            Just name

        Can.VarForeign _ name _ ->
            Just name

        _ ->
            Nothing



-- CONSTRAIN BINOP


constrainBinop : Target -> RTV -> A.Region -> Name.Name -> Can.Annotation -> Can.Expr -> Can.Expr -> E.Expected Type -> IO Constraint
constrainBinop target rtv region op annotation leftExpr rightExpr expected =
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
                                                CForeign region op annotation (NoExpectation target binopType)
                                        in
                                        constrain target rtv leftExpr (FromContext target region (OpLeft op) leftType)
                                            |> IO.bind
                                                (\leftCon ->
                                                    constrain target rtv rightExpr (FromContext target region (OpRight op) rightType)
                                                        |> IO.fmap
                                                            (\rightCon ->
                                                                Type.exists [ leftVar, rightVar, answerVar ]
                                                                    (CAnd
                                                                        [ opCon
                                                                        , leftCon
                                                                        , rightCon
                                                                        , CEqual region (CallResult (OpName op)) answerType expected
                                                                        ]
                                                                    )
                                                            )
                                                )
                                    )
                        )
            )



-- CONSTRAIN LISTS


constrainList : Target -> RTV -> A.Region -> List Can.Expr -> E.Expected Type -> IO Constraint
constrainList target rtv region entries expected =
    Type.mkFlexVar
        |> IO.bind
            (\entryVar ->
                let
                    entryType : Type
                    entryType =
                        VarN entryVar

                    listType : Type
                    listType =
                        AppN (ModuleName.list target) Name.list [ entryType ]
                in
                IO.traverseIndexed (constrainListEntry target rtv region entryType) entries
                    |> IO.fmap
                        (\entryCons ->
                            Type.exists [ entryVar ]
                                (CAnd
                                    [ CAnd entryCons
                                    , CEqual region List listType expected
                                    ]
                                )
                        )
            )


constrainListEntry : Target -> RTV -> A.Region -> Type -> Index.ZeroBased -> Can.Expr -> IO Constraint
constrainListEntry target rtv region tipe index expr =
    constrain target rtv expr (FromContext target region (ListEntry index) tipe)



-- CONSTRAIN IF EXPRESSIONS


constrainIf : Target -> RTV -> A.Region -> List ( Can.Expr, Can.Expr ) -> Can.Expr -> E.Expected Type -> IO Constraint
constrainIf target rtv region branches final expected =
    let
        boolExpect : Expected Type
        boolExpect =
            FromContext target region IfCondition (Type.bool target)

        ( conditions, exprs ) =
            List.foldr (\( c, e ) ( cs, es ) -> ( c :: cs, e :: es )) ( [], [ final ] ) branches
    in
    IO.traverseList (\c -> constrain target rtv c boolExpect) conditions
        |> IO.bind
            (\condCons ->
                case expected of
                    FromAnnotation _ name arity _ tipe ->
                        IO.indexedForA exprs (\index expr -> constrain target rtv expr (FromAnnotation target name arity (TypedIfBranch index) tipe))
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
                                            constrain target rtv expr (FromContext target region (IfBranch index) branchType)
                                        )
                                        |> IO.fmap
                                            (\branchCons ->
                                                Type.exists [ branchVar ]
                                                    (CAnd
                                                        [ CAnd condCons
                                                        , CAnd branchCons
                                                        , CEqual region If branchType expected
                                                        ]
                                                    )
                                            )
                                )
            )



-- CONSTRAIN CASE EXPRESSIONS


constrainCase : Target -> RTV -> A.Region -> Can.Expr -> List Can.CaseBranch -> Expected Type -> IO Constraint
constrainCase target rtv region expr branches expected =
    Type.mkFlexVar
        |> IO.bind
            (\ptrnVar ->
                let
                    ptrnType : Type
                    ptrnType =
                        VarN ptrnVar
                in
                constrain target rtv expr (NoExpectation target ptrnType)
                    |> IO.bind
                        (\exprCon ->
                            case expected of
                                FromAnnotation _ name arity _ tipe ->
                                    IO.indexedForA branches
                                        (\index branch ->
                                            constrainCaseBranch target
                                                rtv
                                                branch
                                                (PFromContext region (PCaseMatch index) ptrnType)
                                                (FromAnnotation target name arity (TypedCaseBranch index) tipe)
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
                                                        constrainCaseBranch target
                                                            rtv
                                                            branch
                                                            (PFromContext region (PCaseMatch index) ptrnType)
                                                            (FromContext target region (CaseBranch index) branchType)
                                                    )
                                                    |> IO.fmap
                                                        (\branchCons ->
                                                            Type.exists [ ptrnVar, branchVar ]
                                                                (CAnd
                                                                    [ exprCon
                                                                    , CAnd branchCons
                                                                    , CEqual region Case branchType expected
                                                                    ]
                                                                )
                                                        )
                                            )
                        )
            )


constrainCaseBranch : Target -> RTV -> Can.CaseBranch -> PExpected Type -> Expected Type -> IO Constraint
constrainCaseBranch target rtv (Can.CaseBranch pattern expr) pExpect bExpect =
    Pattern.add target pattern pExpect Pattern.emptyState
        |> IO.bind
            (\(Pattern.State headers pvars revCons) ->
                IO.fmap (CLet [] pvars headers (CAnd (List.reverse revCons)))
                    (constrain target rtv expr bExpect)
            )



-- CONSTRAIN RECORD


constrainRecord : Target -> RTV -> A.Region -> Dict String (A.Located Name.Name) Can.Expr -> Expected Type -> IO Constraint
constrainRecord target rtv region fields expected =
    IO.traverseMap A.toValue A.compareLocated (constrainField target rtv) fields
        |> IO.fmap
            (\dict ->
                let
                    getType : a -> ( b, c, d ) -> c
                    getType _ ( _, t, _ ) =
                        t

                    recordType : Type
                    recordType =
                        RecordN (Utils.mapMapKeys identity A.compareLocated A.toValue (Dict.map getType dict)) EmptyRecordN

                    recordCon : Constraint
                    recordCon =
                        CEqual region Record recordType expected

                    vars : List IO.Variable
                    vars =
                        Dict.foldr A.compareLocated (\_ ( v, _, _ ) vs -> v :: vs) [] dict

                    cons : List Constraint
                    cons =
                        Dict.foldr A.compareLocated (\_ ( _, _, c ) cs -> c :: cs) [ recordCon ] dict
                in
                Type.exists vars (CAnd cons)
            )


constrainField : Target -> RTV -> Can.Expr -> IO ( IO.Variable, Type, Constraint )
constrainField target rtv expr =
    Type.mkFlexVar
        |> IO.bind
            (\var ->
                let
                    tipe : Type
                    tipe =
                        VarN var
                in
                constrain target rtv expr (NoExpectation target tipe)
                    |> IO.fmap
                        (\con ->
                            ( var, tipe, con )
                        )
            )



-- CONSTRAIN RECORD UPDATE


constrainUpdate : Target -> RTV -> A.Region -> Can.Expr -> Dict String (A.Located Name.Name) Can.FieldUpdate -> Expected Type -> IO Constraint
constrainUpdate target rtv region expr locatedFields expected =
    Type.mkFlexVar
        |> IO.bind
            (\extVar ->
                let
                    fields : Dict String Name.Name Can.FieldUpdate
                    fields =
                        Utils.mapMapKeys identity A.compareLocated A.toValue locatedFields
                in
                IO.traverseMapWithKey identity compare (constrainUpdateField target rtv region) fields
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
                                                CEqual region Record recordType (NoExpectation target fieldsType)

                                            recordCon : Constraint
                                            recordCon =
                                                CEqual region Record recordType expected

                                            vars : List IO.Variable
                                            vars =
                                                Dict.foldr compare (\_ ( v, _, _ ) vs -> v :: vs) [ recordVar, extVar ] fieldDict

                                            cons : List Constraint
                                            cons =
                                                Dict.foldr compare (\_ ( _, _, c ) cs -> c :: cs) [ recordCon ] fieldDict
                                        in
                                        constrain target rtv expr (FromContext target region (RecordUpdateKeys fields) recordType)
                                            |> IO.fmap (\con -> Type.exists vars (CAnd (fieldsCon :: con :: cons)))
                                    )
                        )
            )


constrainUpdateField : Target -> RTV -> A.Region -> Name.Name -> Can.FieldUpdate -> IO ( IO.Variable, Type, Constraint )
constrainUpdateField target rtv region field (Can.FieldUpdate _ expr) =
    Type.mkFlexVar
        |> IO.bind
            (\var ->
                let
                    tipe : Type
                    tipe =
                        VarN var
                in
                constrain target rtv expr (FromContext target region (RecordUpdateValue field) tipe)
                    |> IO.fmap (\con -> ( var, tipe, con ))
            )



-- CONSTRAIN TUPLE


constrainTuple : Target -> RTV -> A.Region -> Can.Expr -> Can.Expr -> List Can.Expr -> Expected Type -> IO Constraint
constrainTuple target rtv region a b cs expected =
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
                            constrain target rtv a (NoExpectation target aType)
                                |> IO.bind
                                    (\aCon ->
                                        constrain target rtv b (NoExpectation target bType)
                                            |> IO.bind
                                                (\bCon ->
                                                    List.foldr
                                                        (\c ->
                                                            IO.bind
                                                                (\( cons, vars ) ->
                                                                    Type.mkFlexVar
                                                                        |> IO.bind
                                                                            (\cVar ->
                                                                                constrain target rtv c (NoExpectation target (VarN cVar))
                                                                                    |> IO.fmap (\cCon -> ( cCon :: cons, cVar :: vars ))
                                                                            )
                                                                )
                                                        )
                                                        (IO.pure ( [], [] ))
                                                        cs
                                                        |> IO.fmap
                                                            (\( cons, vars ) ->
                                                                let
                                                                    tupleType : Type
                                                                    tupleType =
                                                                        TupleN aType bType (List.map VarN vars)

                                                                    tupleCon : Constraint
                                                                    tupleCon =
                                                                        CEqual region Tuple tupleType expected
                                                                in
                                                                Type.exists (aVar :: bVar :: vars) (CAnd (aCon :: bCon :: cons ++ [ tupleCon ]))
                                                            )
                                                )
                                    )
                        )
            )



-- CONSTRAIN SHADER


constrainShader : Target -> A.Region -> Shader.Types -> Expected Type -> IO Constraint
constrainShader target region (Shader.Types attributes uniforms varyings) expected =
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
                                    AppN (ModuleName.webgl target)
                                        Name.shader
                                        [ toShaderRecord target attributes attrType
                                        , toShaderRecord target uniforms unifType
                                        , toShaderRecord target varyings EmptyRecordN
                                        ]
                            in
                            Type.exists [ attrVar, unifVar ] (CEqual region Shader shaderType expected)
                        )
            )


toShaderRecord : Target -> Dict String Name.Name Shader.Type -> Type -> Type
toShaderRecord target types baseRecType =
    if Dict.isEmpty types then
        baseRecType

    else
        RecordN (Dict.map (\_ -> glToType target) types) baseRecType


glToType : Target -> Shader.Type -> Type
glToType target glType =
    case glType of
        Shader.V2 ->
            Type.vec2 target

        Shader.V3 ->
            Type.vec3 target

        Shader.V4 ->
            Type.vec4 target

        Shader.M4 ->
            Type.mat4 target

        Shader.Int ->
            Type.int target

        Shader.Float ->
            Type.float target

        Shader.Texture ->
            Type.texture target

        Shader.Bool ->
            Type.bool target



-- CONSTRAIN DESTRUCTURES


constrainDestruct : Target -> RTV -> A.Region -> Can.Pattern -> Can.Expr -> Constraint -> IO Constraint
constrainDestruct target rtv region pattern expr bodyCon =
    Type.mkFlexVar
        |> IO.bind
            (\patternVar ->
                let
                    patternType : Type
                    patternType =
                        VarN patternVar
                in
                Pattern.add target pattern (PNoExpectation patternType) Pattern.emptyState
                    |> IO.bind
                        (\(Pattern.State headers pvars revCons) ->
                            constrain target rtv expr (FromContext target region Destructure patternType)
                                |> IO.fmap
                                    (\exprCon ->
                                        CLet [] (patternVar :: pvars) headers (CAnd (List.reverse (exprCon :: revCons))) bodyCon
                                    )
                        )
            )



-- CONSTRAIN DEF


constrainDef : Target -> RTV -> Can.Def -> Constraint -> IO Constraint
constrainDef target rtv def bodyCon =
    case def of
        Can.Def (A.At region name) args expr ->
            constrainArgs target args
                |> IO.bind
                    (\(Args vars tipe resultType (Pattern.State headers pvars revCons)) ->
                        constrain target rtv expr (NoExpectation target resultType)
                            |> IO.fmap
                                (\exprCon ->
                                    CLet []
                                        vars
                                        (Dict.singleton identity name (A.At region tipe))
                                        (CLet []
                                            pvars
                                            headers
                                            (CAnd (List.reverse revCons))
                                            exprCon
                                        )
                                        bodyCon
                                )
                    )

        Can.TypedDef (A.At region name) freeVars typedArgs expr srcResultType ->
            let
                newNames : Dict String Name ()
                newNames =
                    Dict.diff freeVars rtv
            in
            IO.traverseMapWithKey identity compare (\n _ -> Type.nameToRigid n) newNames
                |> IO.bind
                    (\newRigids ->
                        let
                            newRtv : Dict String Name Type
                            newRtv =
                                Dict.union rtv (Dict.map (\_ -> VarN) newRigids)
                        in
                        constrainTypedArgs target newRtv name typedArgs srcResultType
                            |> IO.bind
                                (\(TypedArgs tipe resultType (Pattern.State headers pvars revCons)) ->
                                    let
                                        expected : Expected Type
                                        expected =
                                            FromAnnotation target name (List.length typedArgs) TypedBody resultType
                                    in
                                    constrain target newRtv expr expected
                                        |> IO.fmap
                                            (\exprCon ->
                                                CLet (Dict.values compare newRigids)
                                                    []
                                                    (Dict.singleton identity name (A.At region tipe))
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
    = Info (List IO.Variable) (List Constraint) (Dict String Name (A.Located Type))


emptyInfo : Info
emptyInfo =
    Info [] [] Dict.empty


constrainRecursiveDefs : Target -> RTV -> List Can.Def -> Constraint -> IO Constraint
constrainRecursiveDefs target rtv defs bodyCon =
    recDefsHelp target rtv defs bodyCon emptyInfo emptyInfo


recDefsHelp : Target -> RTV -> List Can.Def -> Constraint -> Info -> Info -> IO Constraint
recDefsHelp target rtv defs bodyCon rigidInfo flexInfo =
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
                Can.Def (A.At region name) args expr ->
                    let
                        (Info flexVars flexCons flexHeaders) =
                            flexInfo
                    in
                    argsHelp target args (Pattern.State Dict.empty flexVars [])
                        |> IO.bind
                            (\(Args newFlexVars tipe resultType (Pattern.State headers pvars revCons)) ->
                                constrain target rtv expr (NoExpectation target resultType)
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
                                            recDefsHelp target rtv otherDefs bodyCon rigidInfo <|
                                                Info newFlexVars
                                                    (defCon :: flexCons)
                                                    (Dict.insert identity name (A.At region tipe) flexHeaders)
                                        )
                            )

                Can.TypedDef (A.At region name) freeVars typedArgs expr srcResultType ->
                    let
                        newNames : Dict String Name ()
                        newNames =
                            Dict.diff freeVars rtv
                    in
                    IO.traverseMapWithKey identity compare (\n _ -> Type.nameToRigid n) newNames
                        |> IO.bind
                            (\newRigids ->
                                let
                                    newRtv : Dict String Name Type
                                    newRtv =
                                        Dict.union rtv (Dict.map (\_ -> VarN) newRigids)
                                in
                                constrainTypedArgs target newRtv name typedArgs srcResultType
                                    |> IO.bind
                                        (\(TypedArgs tipe resultType (Pattern.State headers pvars revCons)) ->
                                            constrain target newRtv expr (FromAnnotation target name (List.length typedArgs) TypedBody resultType)
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
                                                        recDefsHelp target
                                                            rtv
                                                            otherDefs
                                                            bodyCon
                                                            (Info
                                                                (Dict.foldr compare (\_ -> (::)) rigidVars newRigids)
                                                                (CLet (Dict.values compare newRigids) [] Dict.empty defCon CTrue :: rigidCons)
                                                                (Dict.insert identity name (A.At region tipe) rigidHeaders)
                                                            )
                                                            flexInfo
                                                    )
                                        )
                            )



-- CONSTRAIN ARGS


type Args
    = Args (List IO.Variable) Type Type Pattern.State


constrainArgs : Target -> List Can.Pattern -> IO Args
constrainArgs target args =
    argsHelp target args Pattern.emptyState


argsHelp : Target -> List Can.Pattern -> Pattern.State -> IO Args
argsHelp target args state =
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
                        Pattern.add target pattern (PNoExpectation argType) state
                            |> IO.bind (argsHelp target otherArgs)
                            |> IO.fmap
                                (\(Args vars tipe result newState) ->
                                    Args (argVar :: vars) (FunN argType tipe) result newState
                                )
                    )



-- CONSTRAIN TYPED ARGS


type TypedArgs
    = TypedArgs Type Type Pattern.State


constrainTypedArgs : Target -> Dict String Name.Name Type -> Name.Name -> List ( Can.Pattern, Can.Type ) -> Can.Type -> IO TypedArgs
constrainTypedArgs target rtv name args srcResultType =
    typedArgsHelp target rtv name Index.first args srcResultType Pattern.emptyState


typedArgsHelp : Target -> Dict String Name.Name Type -> Name.Name -> Index.ZeroBased -> List ( Can.Pattern, Can.Type ) -> Can.Type -> Pattern.State -> IO TypedArgs
typedArgsHelp target rtv name index args srcResultType state =
    case args of
        [] ->
            Instantiate.fromSrcType rtv srcResultType
                |> IO.fmap
                    (\resultType ->
                        TypedArgs resultType resultType state
                    )

        ( (A.At region _) as pattern, srcType ) :: otherArgs ->
            Instantiate.fromSrcType rtv srcType
                |> IO.bind
                    (\argType ->
                        let
                            expected : PExpected Type
                            expected =
                                PFromContext region (PTypedArg name index) argType
                        in
                        Pattern.add target pattern expected state
                            |> IO.bind (typedArgsHelp target rtv name (Index.next index) otherArgs srcResultType)
                            |> IO.fmap
                                (\(TypedArgs tipe resultType newState) ->
                                    TypedArgs (FunN argType tipe) resultType newState
                                )
                    )
