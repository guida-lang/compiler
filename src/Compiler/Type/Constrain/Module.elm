module Compiler.Type.Constrain.Module exposing (constrain)

import Compiler.AST.Canonical as Can
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Constrain.Expression as Expr
import Compiler.Type.Instantiate as Instantiate
import Compiler.Type.Type as Type exposing (Constraint(..), Type(..), mkFlexVar, nameToRigid)
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)



-- CONSTRAIN


constrain : Target -> Can.Module -> IO Constraint
constrain target (Can.Module home _ _ decls _ _ _ effects) =
    case effects of
        Can.NoEffects ->
            constrainDecls target decls CSaveTheEnvironment

        Can.Ports ports ->
            Dict.foldr compare letPort (constrainDecls target decls CSaveTheEnvironment) ports

        Can.Manager r0 r1 r2 manager ->
            case manager of
                Can.Cmd cmdName ->
                    constrainEffects target home r0 r1 r2 manager
                        |> IO.bind (constrainDecls target decls)
                        |> IO.bind (letCmd target home cmdName)

                Can.Sub subName ->
                    constrainEffects target home r0 r1 r2 manager
                        |> IO.bind (constrainDecls target decls)
                        |> IO.bind (letSub target home subName)

                Can.Fx cmdName subName ->
                    constrainEffects target home r0 r1 r2 manager
                        |> IO.bind (constrainDecls target decls)
                        |> IO.bind (letSub target home subName)
                        |> IO.bind (letCmd target home cmdName)



-- CONSTRAIN DECLARATIONS


constrainDecls : Target -> Can.Decls -> Constraint -> IO Constraint
constrainDecls target decls finalConstraint =
    constrainDeclsHelp target decls finalConstraint identity


constrainDeclsHelp : Target -> Can.Decls -> Constraint -> (IO Constraint -> IO Constraint) -> IO Constraint
constrainDeclsHelp target decls finalConstraint cont =
    case decls of
        Can.Declare def otherDecls ->
            constrainDeclsHelp target otherDecls finalConstraint (IO.bind (Expr.constrainDef target Dict.empty def) >> cont)

        Can.DeclareRec def defs otherDecls ->
            constrainDeclsHelp target otherDecls finalConstraint (IO.bind (Expr.constrainRecursiveDefs target Dict.empty (def :: defs)) >> cont)

        Can.SaveTheEnvironment ->
            cont (IO.pure finalConstraint)



-- PORT HELPERS


letPort : Name -> Can.Port -> IO Constraint -> IO Constraint
letPort name port_ makeConstraint =
    case port_ of
        Can.Incoming { freeVars, func } ->
            IO.traverseMapWithKey identity compare (\k _ -> nameToRigid k) freeVars
                |> IO.bind
                    (\vars ->
                        Instantiate.fromSrcType (Dict.map (\_ v -> VarN v) vars) func
                            |> IO.bind
                                (\tipe ->
                                    let
                                        header : Dict String Name (A.Located Type)
                                        header =
                                            Dict.singleton identity name (A.At A.zero tipe)
                                    in
                                    IO.fmap (CLet (Dict.values compare vars) [] header CTrue) makeConstraint
                                )
                    )

        Can.Outgoing { freeVars, func } ->
            IO.traverseMapWithKey identity compare (\k _ -> nameToRigid k) freeVars
                |> IO.bind
                    (\vars ->
                        Instantiate.fromSrcType (Dict.map (\_ v -> VarN v) vars) func
                            |> IO.bind
                                (\tipe ->
                                    let
                                        header : Dict String Name (A.Located Type)
                                        header =
                                            Dict.singleton identity name (A.At A.zero tipe)
                                    in
                                    IO.fmap (CLet (Dict.values compare vars) [] header CTrue) makeConstraint
                                )
                    )



-- EFFECT MANAGER HELPERS


letCmd : Target -> IO.Canonical -> Name -> Constraint -> IO Constraint
letCmd target home tipe constraint =
    mkFlexVar
        |> IO.fmap
            (\msgVar ->
                let
                    msg : Type
                    msg =
                        VarN msgVar

                    cmdType : Type
                    cmdType =
                        FunN (AppN home tipe [ msg ]) (AppN (ModuleName.cmd target) Name.cmd [ msg ])

                    header : Dict String Name (A.Located Type)
                    header =
                        Dict.singleton identity "command" (A.At A.zero cmdType)
                in
                CLet [ msgVar ] [] header CTrue constraint
            )


letSub : Target -> IO.Canonical -> Name -> Constraint -> IO Constraint
letSub target home tipe constraint =
    mkFlexVar
        |> IO.fmap
            (\msgVar ->
                let
                    msg : Type
                    msg =
                        VarN msgVar

                    subType : Type
                    subType =
                        FunN (AppN home tipe [ msg ]) (AppN (ModuleName.sub target) Name.sub [ msg ])

                    header : Dict String Name (A.Located Type)
                    header =
                        Dict.singleton identity "subscription" (A.At A.zero subType)
                in
                CLet [ msgVar ] [] header CTrue constraint
            )


constrainEffects : Target -> IO.Canonical -> A.Region -> A.Region -> A.Region -> Can.Manager -> IO Constraint
constrainEffects target home r0 r1 r2 manager =
    mkFlexVar
        |> IO.bind
            (\s0 ->
                mkFlexVar
                    |> IO.bind
                        (\s1 ->
                            mkFlexVar
                                |> IO.bind
                                    (\s2 ->
                                        mkFlexVar
                                            |> IO.bind
                                                (\m1 ->
                                                    mkFlexVar
                                                        |> IO.bind
                                                            (\m2 ->
                                                                mkFlexVar
                                                                    |> IO.bind
                                                                        (\sm1 ->
                                                                            mkFlexVar
                                                                                |> IO.bind
                                                                                    (\sm2 ->
                                                                                        let
                                                                                            state0 : Type
                                                                                            state0 =
                                                                                                VarN s0

                                                                                            state1 : Type
                                                                                            state1 =
                                                                                                VarN s1

                                                                                            state2 : Type
                                                                                            state2 =
                                                                                                VarN s2

                                                                                            msg1 : Type
                                                                                            msg1 =
                                                                                                VarN m1

                                                                                            msg2 : Type
                                                                                            msg2 =
                                                                                                VarN m2

                                                                                            self1 : Type
                                                                                            self1 =
                                                                                                VarN sm1

                                                                                            self2 : Type
                                                                                            self2 =
                                                                                                VarN sm2

                                                                                            onSelfMsg : Type
                                                                                            onSelfMsg =
                                                                                                Type.funType (router target msg2 self2) (Type.funType self2 (Type.funType state2 (task target state2)))

                                                                                            onEffects : Type
                                                                                            onEffects =
                                                                                                case manager of
                                                                                                    Can.Cmd cmd ->
                                                                                                        Type.funType (router target msg1 self1) (Type.funType (effectList target home cmd msg1) (Type.funType state1 (task target state1)))

                                                                                                    Can.Sub sub ->
                                                                                                        Type.funType (router target msg1 self1) (Type.funType (effectList target home sub msg1) (Type.funType state1 (task target state1)))

                                                                                                    Can.Fx cmd sub ->
                                                                                                        Type.funType (router target msg1 self1) (Type.funType (effectList target home cmd msg1) (Type.funType (effectList target home sub msg1) (Type.funType state1 (task target state1))))

                                                                                            effectCons : Constraint
                                                                                            effectCons =
                                                                                                CAnd
                                                                                                    [ CLocal r0 "init" (E.NoExpectation target (task target state0))
                                                                                                    , CLocal r1 "onEffects" (E.NoExpectation target onEffects)
                                                                                                    , CLocal r2 "onSelfMsg" (E.NoExpectation target onSelfMsg)
                                                                                                    , CEqual r1 E.Effects state0 (E.NoExpectation target state1)
                                                                                                    , CEqual r2 E.Effects state0 (E.NoExpectation target state2)
                                                                                                    , CEqual r2 E.Effects self1 (E.NoExpectation target self2)
                                                                                                    ]
                                                                                        in
                                                                                        IO.fmap (CLet [] [ s0, s1, s2, m1, m2, sm1, sm2 ] Dict.empty effectCons)
                                                                                            (case manager of
                                                                                                Can.Cmd cmd ->
                                                                                                    checkMap target "cmdMap" home cmd CSaveTheEnvironment

                                                                                                Can.Sub sub ->
                                                                                                    checkMap target "subMap" home sub CSaveTheEnvironment

                                                                                                Can.Fx cmd sub ->
                                                                                                    IO.bind (checkMap target "cmdMap" home cmd)
                                                                                                        (checkMap target "subMap" home sub CSaveTheEnvironment)
                                                                                            )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


effectList : Target -> IO.Canonical -> Name -> Type -> Type
effectList target home name msg =
    AppN (ModuleName.list target) Name.list [ AppN home name [ msg ] ]


task : Target -> Type -> Type
task target answer =
    AppN (ModuleName.platform target) Name.task [ Type.never target, answer ]


router : Target -> Type -> Type -> Type
router target msg self =
    AppN (ModuleName.platform target) Name.router [ msg, self ]


checkMap : Target -> Name -> IO.Canonical -> Name -> Constraint -> IO Constraint
checkMap target name home tipe constraint =
    mkFlexVar
        |> IO.bind
            (\a ->
                mkFlexVar
                    |> IO.fmap
                        (\b ->
                            let
                                mapType : Type
                                mapType =
                                    toMapType home tipe (VarN a) (VarN b)

                                mapCon : Constraint
                                mapCon =
                                    CLocal A.zero name (E.NoExpectation target mapType)
                            in
                            CLet [ a, b ] [] Dict.empty mapCon constraint
                        )
            )


toMapType : IO.Canonical -> Name -> Type -> Type -> Type
toMapType home tipe a b =
    Type.funType (Type.funType a b) (Type.funType (AppN home tipe [ a ]) (AppN home tipe [ b ]))
