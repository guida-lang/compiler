module Compiler.Type.Instantiate exposing
    ( FreeVars
    , fromSrcType
    )

import Compiler.Type.Type exposing (Type(..))
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO exposing (IO)
import Types as T
import Utils.Main as Utils



-- FREE VARS


type alias FreeVars =
    Dict String T.CDN_Name Type



-- FROM SOURCE TYPE


fromSrcType : FreeVars -> T.CASTC_Type -> IO Type
fromSrcType freeVars sourceType =
    case sourceType of
        T.CASTC_TLambda arg result ->
            IO.pure FunN
                |> IO.apply (fromSrcType freeVars arg)
                |> IO.apply (fromSrcType freeVars result)

        T.CASTC_TVar name ->
            IO.pure (Utils.find identity name freeVars)

        T.CASTC_TType home name args ->
            IO.fmap (AppN home name)
                (IO.traverseList (fromSrcType freeVars) args)

        T.CASTC_TAlias home name args aliasedType ->
            IO.traverseList (IO.traverseTuple (fromSrcType freeVars)) args
                |> IO.bind
                    (\targs ->
                        IO.fmap (AliasN home name targs)
                            (case aliasedType of
                                T.CASTC_Filled realType ->
                                    fromSrcType freeVars realType

                                T.CASTC_Holey realType ->
                                    fromSrcType (Dict.fromList identity targs) realType
                            )
                    )

        T.CASTC_TTuple a b maybeC ->
            IO.pure TupleN
                |> IO.apply (fromSrcType freeVars a)
                |> IO.apply (fromSrcType freeVars b)
                |> IO.apply (IO.traverseMaybe (fromSrcType freeVars) maybeC)

        T.CASTC_TUnit ->
            IO.pure UnitN

        T.CASTC_TRecord fields maybeExt ->
            IO.pure RecordN
                |> IO.apply (IO.traverseMap identity compare (fromSrcFieldType freeVars) fields)
                |> IO.apply
                    (case maybeExt of
                        Nothing ->
                            IO.pure EmptyRecordN

                        Just ext ->
                            IO.pure (Utils.find identity ext freeVars)
                    )


fromSrcFieldType : Dict String T.CDN_Name Type -> T.CASTC_FieldType -> IO Type
fromSrcFieldType freeVars (T.CASTC_FieldType _ tipe) =
    fromSrcType freeVars tipe
