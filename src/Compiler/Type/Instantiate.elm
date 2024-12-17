module Compiler.Type.Instantiate exposing
    ( FreeVars
    , fromSrcType
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Name exposing (CDN_Name)
import Compiler.Type.Type exposing (Type(..))
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)
import Utils.Main as Utils



-- FREE VARS


type alias FreeVars =
    Dict String CDN_Name Type



-- FROM SOURCE TYPE


fromSrcType : FreeVars -> Can.CASTC_Type -> IO Type
fromSrcType freeVars sourceType =
    case sourceType of
        Can.CASTC_TLambda arg result ->
            IO.pure FunN
                |> IO.apply (fromSrcType freeVars arg)
                |> IO.apply (fromSrcType freeVars result)

        Can.CASTC_TVar name ->
            IO.pure (Utils.find identity name freeVars)

        Can.CASTC_TType home name args ->
            IO.fmap (AppN home name)
                (IO.traverseList (fromSrcType freeVars) args)

        Can.CASTC_TAlias home name args aliasedType ->
            IO.traverseList (IO.traverseTuple (fromSrcType freeVars)) args
                |> IO.bind
                    (\targs ->
                        IO.fmap (AliasN home name targs)
                            (case aliasedType of
                                Can.CASTC_Filled realType ->
                                    fromSrcType freeVars realType

                                Can.CASTC_Holey realType ->
                                    fromSrcType (Dict.fromList identity targs) realType
                            )
                    )

        Can.CASTC_TTuple a b maybeC ->
            IO.pure TupleN
                |> IO.apply (fromSrcType freeVars a)
                |> IO.apply (fromSrcType freeVars b)
                |> IO.apply (IO.traverseMaybe (fromSrcType freeVars) maybeC)

        Can.CASTC_TUnit ->
            IO.pure UnitN

        Can.CASTC_TRecord fields maybeExt ->
            IO.pure RecordN
                |> IO.apply (IO.traverseMap identity compare (fromSrcFieldType freeVars) fields)
                |> IO.apply
                    (case maybeExt of
                        Nothing ->
                            IO.pure EmptyRecordN

                        Just ext ->
                            IO.pure (Utils.find identity ext freeVars)
                    )


fromSrcFieldType : Dict String CDN_Name Type -> Can.CASTC_FieldType -> IO Type
fromSrcFieldType freeVars (Can.CASTC_FieldType _ tipe) =
    fromSrcType freeVars tipe
