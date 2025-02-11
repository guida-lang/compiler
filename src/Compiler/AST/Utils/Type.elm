module Compiler.AST.Utils.Type exposing
    ( dealias
    , deepDealias
    , delambda
    , iteratedDealias
    )

import Compiler.AST.Canonical exposing (AliasType(..), FieldType(..), Type(..))
import Compiler.Data.Name exposing (Name)
import Data.Map as Dict exposing (Dict)



-- DELAMBDA


delambda : Type -> List Type
delambda tipe =
    case tipe of
        TLambda arg result ->
            arg :: delambda result

        _ ->
            [ tipe ]



-- DEALIAS


dealias : List ( Name, Type ) -> AliasType -> Type
dealias args aliasType =
    case aliasType of
        Holey tipe ->
            dealiasHelp (Dict.fromList identity args) tipe

        Filled tipe ->
            tipe


dealiasHelp : Dict String Name Type -> Type -> Type
dealiasHelp typeTable tipe =
    case tipe of
        TLambda a b ->
            TLambda
                (dealiasHelp typeTable a)
                (dealiasHelp typeTable b)

        TVar x ->
            Dict.get identity x typeTable
                |> Maybe.withDefault tipe

        TRecord fields ext ->
            TRecord (Dict.map (\_ -> dealiasField typeTable) fields) ext

        TAlias home name args t_ ->
            TAlias home name (List.map (Tuple.mapSecond (dealiasHelp typeTable)) args) t_

        TType home name args ->
            TType home name (List.map (dealiasHelp typeTable) args)

        TUnit ->
            TUnit

        TTuple a b cs ->
            TTuple
                (dealiasHelp typeTable a)
                (dealiasHelp typeTable b)
                (List.map (dealiasHelp typeTable) cs)


dealiasField : Dict String Name Type -> FieldType -> FieldType
dealiasField typeTable (FieldType index tipe) =
    FieldType index (dealiasHelp typeTable tipe)



-- DEEP DEALIAS


deepDealias : Type -> Type
deepDealias tipe =
    case tipe of
        TLambda a b ->
            TLambda (deepDealias a) (deepDealias b)

        TVar _ ->
            tipe

        TRecord fields ext ->
            TRecord (Dict.map (\_ -> deepDealiasField) fields) ext

        TAlias _ _ args tipe_ ->
            deepDealias (dealias args tipe_)

        TType home name args ->
            TType home name (List.map deepDealias args)

        TUnit ->
            TUnit

        TTuple a b cs ->
            TTuple (deepDealias a) (deepDealias b) (List.map deepDealias cs)


deepDealiasField : FieldType -> FieldType
deepDealiasField (FieldType index tipe) =
    FieldType index (deepDealias tipe)



-- ITERATED DEALIAS


iteratedDealias : Type -> Type
iteratedDealias tipe =
    case tipe of
        TAlias _ _ args realType ->
            iteratedDealias (dealias args realType)

        _ ->
            tipe
