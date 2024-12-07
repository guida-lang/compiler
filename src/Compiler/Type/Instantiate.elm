module Compiler.Type.Instantiate exposing
    ( FreeVars
    , fromSrcType
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Name exposing (Name)
import Compiler.Type.Type exposing (Type(..))
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)
import Utils.Main as Utils



-- FREE VARS


type alias FreeVars =
    Dict Name Type



-- FROM SOURCE TYPE


fromSrcType : FreeVars -> Can.Type -> IO Type
fromSrcType freeVars sourceType =
    case sourceType of
        Can.TLambda arg result ->
            IO.pure FunN
                |> IO.apply (fromSrcType freeVars arg)
                |> IO.apply (fromSrcType freeVars result)

        Can.TVar name ->
            IO.pure (Utils.find name freeVars)

        Can.TType home name args ->
            IO.fmap (AppN home name)
                (IO.traverseList (fromSrcType freeVars) args)

        Can.TAlias home name args aliasedType ->
            IO.traverseList (IO.traverseTuple (fromSrcType freeVars)) args
                |> IO.bind
                    (\targs ->
                        IO.fmap (AliasN home name targs)
                            (case aliasedType of
                                Can.Filled realType ->
                                    fromSrcType freeVars realType

                                Can.Holey realType ->
                                    fromSrcType (Dict.fromList compare targs) realType
                            )
                    )

        Can.TTuple a b maybeC ->
            IO.pure TupleN
                |> IO.apply (fromSrcType freeVars a)
                |> IO.apply (fromSrcType freeVars b)
                |> IO.apply (IO.traverseMaybe (fromSrcType freeVars) maybeC)

        Can.TUnit ->
            IO.pure UnitN

        Can.TRecord fields maybeExt ->
            IO.pure RecordN
                |> IO.apply (IO.traverseMap compare (fromSrcFieldType freeVars) fields)
                |> IO.apply
                    (case maybeExt of
                        Nothing ->
                            IO.pure EmptyRecordN

                        Just ext ->
                            IO.pure (Utils.find ext freeVars)
                    )


fromSrcFieldType : Dict Name Type -> Can.FieldType -> IO Type
fromSrcFieldType freeVars (Can.FieldType _ tipe) =
    fromSrcType freeVars tipe
