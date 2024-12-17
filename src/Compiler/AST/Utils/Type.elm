module Compiler.AST.Utils.Type exposing
    ( dealias
    , deepDealias
    , delambda
    , iteratedDealias
    )

import Compiler.AST.Canonical exposing (CASTC_AliasType(..), CASTC_FieldType(..), CASTC_Type(..))
import Compiler.Data.Name exposing (CDN_Name)
import Data.Map as Dict exposing (Dict)



-- DELAMBDA


delambda : CASTC_Type -> List CASTC_Type
delambda tipe =
    case tipe of
        CASTC_TLambda arg result ->
            arg :: delambda result

        _ ->
            [ tipe ]



-- DEALIAS


dealias : List ( CDN_Name, CASTC_Type ) -> CASTC_AliasType -> CASTC_Type
dealias args aliasType =
    case aliasType of
        CASTC_Holey tipe ->
            dealiasHelp (Dict.fromList identity args) tipe

        CASTC_Filled tipe ->
            tipe


dealiasHelp : Dict String CDN_Name CASTC_Type -> CASTC_Type -> CASTC_Type
dealiasHelp typeTable tipe =
    case tipe of
        CASTC_TLambda a b ->
            CASTC_TLambda
                (dealiasHelp typeTable a)
                (dealiasHelp typeTable b)

        CASTC_TVar x ->
            Dict.get identity x typeTable
                |> Maybe.withDefault tipe

        CASTC_TRecord fields ext ->
            CASTC_TRecord (Dict.map (\_ -> dealiasField typeTable) fields) ext

        CASTC_TAlias home name args t_ ->
            CASTC_TAlias home name (List.map (Tuple.mapSecond (dealiasHelp typeTable)) args) t_

        CASTC_TType home name args ->
            CASTC_TType home name (List.map (dealiasHelp typeTable) args)

        CASTC_TUnit ->
            CASTC_TUnit

        CASTC_TTuple a b maybeC ->
            CASTC_TTuple
                (dealiasHelp typeTable a)
                (dealiasHelp typeTable b)
                (Maybe.map (dealiasHelp typeTable) maybeC)


dealiasField : Dict String CDN_Name CASTC_Type -> CASTC_FieldType -> CASTC_FieldType
dealiasField typeTable (CASTC_FieldType index tipe) =
    CASTC_FieldType index (dealiasHelp typeTable tipe)



-- DEEP DEALIAS


deepDealias : CASTC_Type -> CASTC_Type
deepDealias tipe =
    case tipe of
        CASTC_TLambda a b ->
            CASTC_TLambda (deepDealias a) (deepDealias b)

        CASTC_TVar _ ->
            tipe

        CASTC_TRecord fields ext ->
            CASTC_TRecord (Dict.map (\_ -> deepDealiasField) fields) ext

        CASTC_TAlias _ _ args tipe_ ->
            deepDealias (dealias args tipe_)

        CASTC_TType home name args ->
            CASTC_TType home name (List.map deepDealias args)

        CASTC_TUnit ->
            CASTC_TUnit

        CASTC_TTuple a b c ->
            CASTC_TTuple (deepDealias a) (deepDealias b) (Maybe.map deepDealias c)


deepDealiasField : CASTC_FieldType -> CASTC_FieldType
deepDealiasField (CASTC_FieldType index tipe) =
    CASTC_FieldType index (deepDealias tipe)



-- ITERATED DEALIAS


iteratedDealias : CASTC_Type -> CASTC_Type
iteratedDealias tipe =
    case tipe of
        CASTC_TAlias _ _ args realType ->
            iteratedDealias (dealias args realType)

        _ ->
            tipe
