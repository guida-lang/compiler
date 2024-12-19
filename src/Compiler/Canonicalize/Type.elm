module Compiler.Canonicalize.Type exposing
    ( CResult
    , canonicalize
    , toAnnotation
    )

import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Types as T
import Utils.Main as Utils



-- RESULT


type alias CResult i w a =
    R.RResult i w T.CREC_Error a



-- TO ANNOTATION


toAnnotation : Env.Env -> T.CASTS_Type -> CResult i w T.CASTC_Annotation
toAnnotation env srcType =
    canonicalize env srcType
        |> R.bind (\tipe -> R.ok (T.CASTC_Forall (addFreeVars Dict.empty tipe) tipe))



-- CANONICALIZE TYPES


canonicalize : Env.Env -> T.CASTS_Type -> CResult i w T.CASTC_Type
canonicalize env (T.CRA_At typeRegion tipe) =
    case tipe of
        T.CASTS_TVar x ->
            R.ok (T.CASTC_TVar x)

        T.CASTS_TType region name args ->
            Env.findType region env name
                |> R.bind (canonicalizeType env typeRegion name args)

        T.CASTS_TTypeQual region home name args ->
            Env.findTypeQual region env home name
                |> R.bind (canonicalizeType env typeRegion name args)

        T.CASTS_TLambda a b ->
            canonicalize env a
                |> R.fmap T.CASTC_TLambda
                |> R.bind
                    (\tLambda ->
                        R.fmap tLambda (canonicalize env b)
                    )

        T.CASTS_TRecord fields ext ->
            Dups.checkFields (canonicalizeFields env fields)
                |> R.bind (Utils.sequenceADict identity compare)
                |> R.fmap (\cfields -> T.CASTC_TRecord cfields (Maybe.map A.toValue ext))

        T.CASTS_TUnit ->
            R.ok T.CASTC_TUnit

        T.CASTS_TTuple a b cs ->
            canonicalize env a
                |> R.fmap T.CASTC_TTuple
                |> R.bind (\tTuple -> R.fmap tTuple (canonicalize env b))
                |> R.bind
                    (\tTuple ->
                        case cs of
                            [] ->
                                R.ok (tTuple Nothing)

                            [ c ] ->
                                canonicalize env c
                                    |> R.fmap (tTuple << Just)

                            _ ->
                                R.throw <| T.CREC_TupleLargerThanThree typeRegion
                    )


canonicalizeFields : Env.Env -> List ( T.CRA_Located T.CDN_Name, T.CASTS_Type ) -> List ( T.CRA_Located T.CDN_Name, CResult i w T.CASTC_FieldType )
canonicalizeFields env fields =
    let
        canonicalizeField : Int -> ( a, T.CASTS_Type ) -> ( a, R.RResult i w T.CREC_Error T.CASTC_FieldType )
        canonicalizeField index ( name, srcType ) =
            ( name, R.fmap (T.CASTC_FieldType index) (canonicalize env srcType) )
    in
    List.indexedMap canonicalizeField fields



-- CANONICALIZE TYPE


canonicalizeType : Env.Env -> T.CRA_Region -> T.CDN_Name -> List T.CASTS_Type -> Env.Type -> CResult i w T.CASTC_Type
canonicalizeType env region name args info =
    R.traverse (canonicalize env) args
        |> R.bind
            (\cargs ->
                case info of
                    Env.Alias arity home argNames aliasedType ->
                        checkArity arity region name args <|
                            T.CASTC_TAlias home name (List.map2 Tuple.pair argNames cargs) (T.CASTC_Holey aliasedType)

                    Env.Union arity home ->
                        checkArity arity region name args <|
                            T.CASTC_TType home name cargs
            )


checkArity : Int -> T.CRA_Region -> T.CDN_Name -> List (T.CRA_Located arg) -> answer -> CResult i w answer
checkArity expected region name args answer =
    let
        actual : Int
        actual =
            List.length args
    in
    if expected == actual then
        R.ok answer

    else
        R.throw (T.CREC_BadArity region T.CREC_TypeArity name expected actual)



-- ADD FREE VARS


addFreeVars : Dict String T.CDN_Name () -> T.CASTC_Type -> Dict String T.CDN_Name ()
addFreeVars freeVars tipe =
    case tipe of
        T.CASTC_TLambda arg result ->
            addFreeVars (addFreeVars freeVars result) arg

        T.CASTC_TVar var ->
            Dict.insert identity var () freeVars

        T.CASTC_TType _ _ args ->
            List.foldl (\b c -> addFreeVars c b) freeVars args

        T.CASTC_TRecord fields Nothing ->
            Dict.foldl compare (\_ b c -> addFieldFreeVars c b) freeVars fields

        T.CASTC_TRecord fields (Just ext) ->
            Dict.foldl compare (\_ b c -> addFieldFreeVars c b) (Dict.insert identity ext () freeVars) fields

        T.CASTC_TUnit ->
            freeVars

        T.CASTC_TTuple a b maybeC ->
            case maybeC of
                Nothing ->
                    addFreeVars (addFreeVars freeVars a) b

                Just c ->
                    addFreeVars (addFreeVars (addFreeVars freeVars a) b) c

        T.CASTC_TAlias _ _ args _ ->
            List.foldl (\( _, arg ) fvs -> addFreeVars fvs arg) freeVars args


addFieldFreeVars : Dict String T.CDN_Name () -> T.CASTC_FieldType -> Dict String T.CDN_Name ()
addFieldFreeVars freeVars (T.CASTC_FieldType _ tipe) =
    addFreeVars freeVars tipe
