module Compiler.Nitpick.Debug exposing (hasDebugUses)

import Compiler.Data.Map.Utils as Map
import Data.Map as Dict
import Types as T



-- HAS DEBUG USES


hasDebugUses : T.CASTO_LocalGraph -> Bool
hasDebugUses (T.CASTO_LocalGraph _ graph _) =
    Map.any nodeHasDebug graph


nodeHasDebug : T.CASTO_Node -> Bool
nodeHasDebug node =
    case node of
        T.CASTO_Define expr _ ->
            hasDebug expr

        T.CASTO_DefineTailFunc _ expr _ ->
            hasDebug expr

        T.CASTO_Ctor _ _ ->
            False

        T.CASTO_Enum _ ->
            False

        T.CASTO_Box ->
            False

        T.CASTO_Link _ ->
            False

        T.CASTO_Cycle _ vs fs _ ->
            List.any (hasDebug << Tuple.second) vs || List.any defHasDebug fs

        T.CASTO_Manager _ ->
            False

        T.CASTO_Kernel _ _ ->
            False

        T.CASTO_PortIncoming expr _ ->
            hasDebug expr

        T.CASTO_PortOutgoing expr _ ->
            hasDebug expr


hasDebug : T.CASTO_Expr -> Bool
hasDebug expression =
    case expression of
        T.CASTO_Bool _ ->
            False

        T.CASTO_Chr _ ->
            False

        T.CASTO_Str _ ->
            False

        T.CASTO_Int _ ->
            False

        T.CASTO_Float _ ->
            False

        T.CASTO_VarLocal _ ->
            False

        T.CASTO_VarGlobal _ ->
            False

        T.CASTO_VarEnum _ _ ->
            False

        T.CASTO_VarBox _ ->
            False

        T.CASTO_VarCycle _ _ ->
            False

        T.CASTO_VarDebug _ _ _ _ ->
            True

        T.CASTO_VarKernel _ _ ->
            False

        T.CASTO_List exprs ->
            List.any hasDebug exprs

        T.CASTO_Function _ expr ->
            hasDebug expr

        T.CASTO_Call e es ->
            hasDebug e || List.any hasDebug es

        T.CASTO_TailCall _ args ->
            List.any (hasDebug << Tuple.second) args

        T.CASTO_If conds finally ->
            List.any (\( c, e ) -> hasDebug c || hasDebug e) conds || hasDebug finally

        T.CASTO_Let def body ->
            defHasDebug def || hasDebug body

        T.CASTO_Destruct _ expr ->
            hasDebug expr

        T.CASTO_Case _ _ d jumps ->
            deciderHasDebug d || List.any (hasDebug << Tuple.second) jumps

        T.CASTO_Accessor _ ->
            False

        T.CASTO_Access r _ ->
            hasDebug r

        T.CASTO_Update r fs ->
            hasDebug r || List.any hasDebug (Dict.values compare fs)

        T.CASTO_Record fs ->
            List.any hasDebug (Dict.values compare fs)

        T.CASTO_Unit ->
            False

        T.CASTO_Tuple a b c ->
            hasDebug a || hasDebug b || Maybe.withDefault False (Maybe.map hasDebug c)

        T.CASTO_Shader _ _ _ ->
            False


defHasDebug : T.CASTO_Def -> Bool
defHasDebug def =
    case def of
        T.CASTO_Def _ expr ->
            hasDebug expr

        T.CASTO_TailDef _ _ expr ->
            hasDebug expr


deciderHasDebug : T.CASTO_Decider T.CASTO_Choice -> Bool
deciderHasDebug decider =
    case decider of
        T.CASTO_Leaf (T.CASTO_Inline expr) ->
            hasDebug expr

        T.CASTO_Leaf (T.CASTO_Jump _) ->
            False

        T.CASTO_Chain _ success failure ->
            deciderHasDebug success || deciderHasDebug failure

        T.CASTO_FanOut _ tests fallback ->
            List.any (deciderHasDebug << Tuple.second) tests || deciderHasDebug fallback
