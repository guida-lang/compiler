module Compiler.Nitpick.Debug exposing (hasDebugUses)

import Compiler.AST.Optimized as Opt
import Compiler.Data.Map.Utils as Map
import Data.Map as Dict



-- HAS DEBUG USES


hasDebugUses : Opt.LocalGraph -> Bool
hasDebugUses (Opt.LocalGraph _ graph _) =
    Map.any nodeHasDebug graph


nodeHasDebug : Opt.Node -> Bool
nodeHasDebug node =
    case node of
        Opt.Define expr _ ->
            hasDebug expr

        Opt.DefineTailFunc _ expr _ ->
            hasDebug expr

        Opt.Ctor _ _ ->
            False

        Opt.Enum _ ->
            False

        Opt.Box ->
            False

        Opt.Link _ ->
            False

        Opt.Cycle _ vs fs _ ->
            List.any (hasDebug << Tuple.second) vs || List.any defHasDebug fs

        Opt.Manager _ ->
            False

        Opt.Kernel _ _ ->
            False

        Opt.PortIncoming expr _ ->
            hasDebug expr

        Opt.PortOutgoing expr _ ->
            hasDebug expr


hasDebug : Opt.Expr -> Bool
hasDebug expression =
    case expression of
        Opt.Bool _ ->
            False

        Opt.Chr _ ->
            False

        Opt.Str _ ->
            False

        Opt.Int _ ->
            False

        Opt.Float _ ->
            False

        Opt.VarLocal _ ->
            False

        Opt.VarGlobal _ ->
            False

        Opt.VarEnum _ _ ->
            False

        Opt.VarBox _ ->
            False

        Opt.VarCycle _ _ ->
            False

        Opt.VarDebug _ _ _ _ ->
            True

        Opt.VarKernel _ _ ->
            False

        Opt.List exprs ->
            List.any hasDebug exprs

        Opt.Function _ expr ->
            hasDebug expr

        Opt.Call e es ->
            hasDebug e || List.any hasDebug es

        Opt.TailCall _ args ->
            List.any (hasDebug << Tuple.second) args

        Opt.If conds finally ->
            List.any (\( c, e ) -> hasDebug c || hasDebug e) conds || hasDebug finally

        Opt.Let def body ->
            defHasDebug def || hasDebug body

        Opt.Destruct _ expr ->
            hasDebug expr

        Opt.Case _ _ d jumps ->
            deciderHasDebug d || List.any (hasDebug << Tuple.second) jumps

        Opt.Accessor _ ->
            False

        Opt.Access r _ ->
            hasDebug r

        Opt.Update r fs ->
            hasDebug r || List.any hasDebug (Dict.values compare fs)

        Opt.Record fs ->
            List.any hasDebug (Dict.values compare fs)

        Opt.Unit ->
            False

        Opt.Tuple a b c ->
            hasDebug a || hasDebug b || Maybe.withDefault False (Maybe.map hasDebug c)

        Opt.Shader _ _ _ ->
            False


defHasDebug : Opt.Def -> Bool
defHasDebug def =
    case def of
        Opt.Def _ expr ->
            hasDebug expr

        Opt.TailDef _ _ expr ->
            hasDebug expr


deciderHasDebug : Opt.Decider Opt.Choice -> Bool
deciderHasDebug decider =
    case decider of
        Opt.Leaf (Opt.Inline expr) ->
            hasDebug expr

        Opt.Leaf (Opt.Jump _) ->
            False

        Opt.Chain _ success failure ->
            deciderHasDebug success || deciderHasDebug failure

        Opt.FanOut _ tests fallback ->
            List.any (deciderHasDebug << Tuple.second) tests || deciderHasDebug fallback
