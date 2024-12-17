module Compiler.AST.Utils.Type exposing
    ( dealias
    , deepDealias
    , delambda
    , iteratedDealias
    )

import Data.Map as Dict exposing (Dict)
import Types as T



-- DELAMBDA


delambda : T.CASTC_Type -> List T.CASTC_Type
delambda tipe =
    case tipe of
        T.CASTC_TLambda arg result ->
            arg :: delambda result

        _ ->
            [ tipe ]



-- DEALIAS


dealias : List ( T.CDN_Name, T.CASTC_Type ) -> T.CASTC_AliasType -> T.CASTC_Type
dealias args aliasType =
    case aliasType of
        T.CASTC_Holey tipe ->
            dealiasHelp (Dict.fromList identity args) tipe

        T.CASTC_Filled tipe ->
            tipe


dealiasHelp : Dict String T.CDN_Name T.CASTC_Type -> T.CASTC_Type -> T.CASTC_Type
dealiasHelp typeTable tipe =
    case tipe of
        T.CASTC_TLambda a b ->
            T.CASTC_TLambda
                (dealiasHelp typeTable a)
                (dealiasHelp typeTable b)

        T.CASTC_TVar x ->
            Dict.get identity x typeTable
                |> Maybe.withDefault tipe

        T.CASTC_TRecord fields ext ->
            T.CASTC_TRecord (Dict.map (\_ -> dealiasField typeTable) fields) ext

        T.CASTC_TAlias home name args t_ ->
            T.CASTC_TAlias home name (List.map (Tuple.mapSecond (dealiasHelp typeTable)) args) t_

        T.CASTC_TType home name args ->
            T.CASTC_TType home name (List.map (dealiasHelp typeTable) args)

        T.CASTC_TUnit ->
            T.CASTC_TUnit

        T.CASTC_TTuple a b maybeC ->
            T.CASTC_TTuple
                (dealiasHelp typeTable a)
                (dealiasHelp typeTable b)
                (Maybe.map (dealiasHelp typeTable) maybeC)


dealiasField : Dict String T.CDN_Name T.CASTC_Type -> T.CASTC_FieldType -> T.CASTC_FieldType
dealiasField typeTable (T.CASTC_FieldType index tipe) =
    T.CASTC_FieldType index (dealiasHelp typeTable tipe)



-- DEEP DEALIAS


deepDealias : T.CASTC_Type -> T.CASTC_Type
deepDealias tipe =
    case tipe of
        T.CASTC_TLambda a b ->
            T.CASTC_TLambda (deepDealias a) (deepDealias b)

        T.CASTC_TVar _ ->
            tipe

        T.CASTC_TRecord fields ext ->
            T.CASTC_TRecord (Dict.map (\_ -> deepDealiasField) fields) ext

        T.CASTC_TAlias _ _ args tipe_ ->
            deepDealias (dealias args tipe_)

        T.CASTC_TType home name args ->
            T.CASTC_TType home name (List.map deepDealias args)

        T.CASTC_TUnit ->
            T.CASTC_TUnit

        T.CASTC_TTuple a b c ->
            T.CASTC_TTuple (deepDealias a) (deepDealias b) (Maybe.map deepDealias c)


deepDealiasField : T.CASTC_FieldType -> T.CASTC_FieldType
deepDealiasField (T.CASTC_FieldType index tipe) =
    T.CASTC_FieldType index (deepDealias tipe)



-- ITERATED DEALIAS


iteratedDealias : T.CASTC_Type -> T.CASTC_Type
iteratedDealias tipe =
    case tipe of
        T.CASTC_TAlias _ _ args realType ->
            iteratedDealias (dealias args realType)

        _ ->
            tipe
