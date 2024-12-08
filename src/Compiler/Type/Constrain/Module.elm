module Compiler.Type.Constrain.Module exposing (constrain)

import Compiler.AST.Canonical as Can
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Constrain.Expression as Expr
import Compiler.Type.Instantiate as Instantiate
import Compiler.Type.Type as Type exposing (Constraint(..), Type(..), mkFlexVar, nameToRigid)
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)



-- CONSTRAIN


constrain : Can.Module -> IO Constraint
constrain (Can.Module home _ _ decls _ _ _ effects) =
    case effects of
        Can.NoEffects ->
            constrainDecls decls CSaveTheEnvironment

        Can.Ports ports ->
            Dict.foldr letPort (constrainDecls decls CSaveTheEnvironment) ports

        Can.Manager r0 r1 r2 manager ->
            case manager of
                Can.Cmd cmdName ->
                    constrainEffects home r0 r1 r2 manager
                        |> IO.bind (constrainDecls decls)
                        |> IO.bind (letCmd home cmdName)

                Can.Sub subName ->
                    constrainEffects home r0 r1 r2 manager
                        |> IO.bind (constrainDecls decls)
                        |> IO.bind (letSub home subName)

                Can.Fx cmdName subName ->
                    constrainEffects home r0 r1 r2 manager
                        |> IO.bind (constrainDecls decls)
                        |> IO.bind (letSub home subName)
                        |> IO.bind (letCmd home cmdName)



-- CONSTRAIN DECLARATIONS


constrainDecls : Can.Decls -> Constraint -> IO Constraint
constrainDecls decls finalConstraint =
    case decls of
        Can.Declare def otherDecls ->
            IO.bind (Expr.constrainDef Dict.empty def) (constrainDecls otherDecls finalConstraint)

        Can.DeclareRec def defs otherDecls ->
            IO.bind (Expr.constrainRecursiveDefs Dict.empty (def :: defs)) (constrainDecls otherDecls finalConstraint)

        Can.SaveTheEnvironment ->
            IO.pure finalConstraint



-- PORT HELPERS


letPort : Name -> Can.Port -> IO Constraint -> IO Constraint
letPort name port_ makeConstraint =
    case port_ of
        Can.Incoming { freeVars, func } ->
            IO.traverseMapWithKey compare (\k _ -> nameToRigid k) freeVars
                |> IO.bind
                    (\vars ->
                        Instantiate.fromSrcType (Dict.map (\_ v -> VarN v) vars) func
                            |> IO.bind
                                (\tipe ->
                                    let
                                        header : Dict Name (A.Located Type)
                                        header =
                                            Dict.singleton name (A.At A.zero tipe)
                                    in
                                    IO.fmap (CLet (Dict.values vars) [] header CTrue) makeConstraint
                                )
                    )

        Can.Outgoing { freeVars, func } ->
            IO.traverseMapWithKey compare (\k _ -> nameToRigid k) freeVars
                |> IO.bind
                    (\vars ->
                        Instantiate.fromSrcType (Dict.map (\_ v -> VarN v) vars) func
                            |> IO.bind
                                (\tipe ->
                                    let
                                        header : Dict Name (A.Located Type)
                                        header =
                                            Dict.singleton name (A.At A.zero tipe)
                                    in
                                    IO.fmap (CLet (Dict.values vars) [] header CTrue) makeConstraint
                                )
                    )



-- EFFECT MANAGER HELPERS


letCmd : IO.Canonical -> Name -> Constraint -> IO Constraint
letCmd home tipe constraint =
    mkFlexVar
        |> IO.fmap
            (\msgVar ->
                let
                    msg : Type
                    msg =
                        VarN msgVar

                    cmdType : Type
                    cmdType =
                        FunN (AppN home tipe [ msg ]) (AppN ModuleName.cmd Name.cmd [ msg ])

                    header : Dict Name (A.Located Type)
                    header =
                        Dict.singleton "command" (A.At A.zero cmdType)
                in
                CLet [ msgVar ] [] header CTrue constraint
            )


letSub : IO.Canonical -> Name -> Constraint -> IO Constraint
letSub home tipe constraint =
    mkFlexVar
        |> IO.fmap
            (\msgVar ->
                let
                    msg : Type
                    msg =
                        VarN msgVar

                    subType : Type
                    subType =
                        FunN (AppN home tipe [ msg ]) (AppN ModuleName.sub Name.sub [ msg ])

                    header : Dict Name (A.Located Type)
                    header =
                        Dict.singleton "subscription" (A.At A.zero subType)
                in
                CLet [ msgVar ] [] header CTrue constraint
            )


constrainEffects : IO.Canonical -> A.Region -> A.Region -> A.Region -> Can.Manager -> IO Constraint
constrainEffects home r0 r1 r2 manager =
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
                                                                                                Type.funType (router msg2 self2) (Type.funType self2 (Type.funType state2 (task state2)))

                                                                                            onEffects : Type
                                                                                            onEffects =
                                                                                                case manager of
                                                                                                    Can.Cmd cmd ->
                                                                                                        Type.funType (router msg1 self1) (Type.funType (effectList home cmd msg1) (Type.funType state1 (task state1)))

                                                                                                    Can.Sub sub ->
                                                                                                        Type.funType (router msg1 self1) (Type.funType (effectList home sub msg1) (Type.funType state1 (task state1)))

                                                                                                    Can.Fx cmd sub ->
                                                                                                        Type.funType (router msg1 self1) (Type.funType (effectList home cmd msg1) (Type.funType (effectList home sub msg1) (Type.funType state1 (task state1))))

                                                                                            effectCons : Constraint
                                                                                            effectCons =
                                                                                                CAnd
                                                                                                    [ CLocal r0 "init" (E.NoExpectation (task state0))
                                                                                                    , CLocal r1 "onEffects" (E.NoExpectation onEffects)
                                                                                                    , CLocal r2 "onSelfMsg" (E.NoExpectation onSelfMsg)
                                                                                                    , CEqual r1 E.Effects state0 (E.NoExpectation state1)
                                                                                                    , CEqual r2 E.Effects state0 (E.NoExpectation state2)
                                                                                                    , CEqual r2 E.Effects self1 (E.NoExpectation self2)
                                                                                                    ]
                                                                                        in
                                                                                        IO.fmap (CLet [] [ s0, s1, s2, m1, m2, sm1, sm2 ] Dict.empty effectCons)
                                                                                            (case manager of
                                                                                                Can.Cmd cmd ->
                                                                                                    checkMap "cmdMap" home cmd CSaveTheEnvironment

                                                                                                Can.Sub sub ->
                                                                                                    checkMap "subMap" home sub CSaveTheEnvironment

                                                                                                Can.Fx cmd sub ->
                                                                                                    IO.bind (checkMap "cmdMap" home cmd)
                                                                                                        (checkMap "subMap" home sub CSaveTheEnvironment)
                                                                                            )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


effectList : IO.Canonical -> Name -> Type -> Type
effectList home name msg =
    AppN ModuleName.list Name.list [ AppN home name [ msg ] ]


task : Type -> Type
task answer =
    AppN ModuleName.platform Name.task [ Type.never, answer ]


router : Type -> Type -> Type
router msg self =
    AppN ModuleName.platform Name.router [ msg, self ]


checkMap : Name -> IO.Canonical -> Name -> Constraint -> IO Constraint
checkMap name home tipe constraint =
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
                                    CLocal A.zero name (E.NoExpectation mapType)
                            in
                            CLet [ a, b ] [] Dict.empty mapCon constraint
                        )
            )


toMapType : IO.Canonical -> Name -> Type -> Type -> Type
toMapType home tipe a b =
    Type.funType (Type.funType a b) (Type.funType (AppN home tipe [ a ]) (AppN home tipe [ b ]))
