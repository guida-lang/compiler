module Compiler.Canonicalize.Type exposing
    ( CResult
    , canonicalize
    , toAnnotation
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Data.Name as Name
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Utils.Main as Utils



-- RESULT


type alias CResult i w a =
    R.RResult i w Error.Error a



-- TO ANNOTATION


toAnnotation : Env.Env -> Src.CASTS_Type -> CResult i w Can.CASTC_Annotation
toAnnotation env srcType =
    canonicalize env srcType
        |> R.bind (\tipe -> R.ok (Can.CASTC_Forall (addFreeVars Dict.empty tipe) tipe))



-- CANONICALIZE TYPES


canonicalize : Env.Env -> Src.CASTS_Type -> CResult i w Can.CASTC_Type
canonicalize env (A.CRA_At typeRegion tipe) =
    case tipe of
        Src.CASTS_TVar x ->
            R.ok (Can.CASTC_TVar x)

        Src.CASTS_TType region name args ->
            Env.findType region env name
                |> R.bind (canonicalizeType env typeRegion name args)

        Src.CASTS_TTypeQual region home name args ->
            Env.findTypeQual region env home name
                |> R.bind (canonicalizeType env typeRegion name args)

        Src.CASTS_TLambda a b ->
            canonicalize env a
                |> R.fmap Can.CASTC_TLambda
                |> R.bind
                    (\tLambda ->
                        R.fmap tLambda (canonicalize env b)
                    )

        Src.CASTS_TRecord fields ext ->
            Dups.checkFields (canonicalizeFields env fields)
                |> R.bind (Utils.sequenceADict identity compare)
                |> R.fmap (\cfields -> Can.CASTC_TRecord cfields (Maybe.map A.toValue ext))

        Src.CASTS_TUnit ->
            R.ok Can.CASTC_TUnit

        Src.CASTS_TTuple a b cs ->
            canonicalize env a
                |> R.fmap Can.CASTC_TTuple
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
                                R.throw <| Error.TupleLargerThanThree typeRegion
                    )


canonicalizeFields : Env.Env -> List ( A.CRA_Located Name.CDN_Name, Src.CASTS_Type ) -> List ( A.CRA_Located Name.CDN_Name, CResult i w Can.CASTC_FieldType )
canonicalizeFields env fields =
    let
        canonicalizeField : Int -> ( a, Src.CASTS_Type ) -> ( a, R.RResult i w Error.Error Can.CASTC_FieldType )
        canonicalizeField index ( name, srcType ) =
            ( name, R.fmap (Can.CASTC_FieldType index) (canonicalize env srcType) )
    in
    List.indexedMap canonicalizeField fields



-- CANONICALIZE TYPE


canonicalizeType : Env.Env -> A.CRA_Region -> Name.CDN_Name -> List Src.CASTS_Type -> Env.Type -> CResult i w Can.CASTC_Type
canonicalizeType env region name args info =
    R.traverse (canonicalize env) args
        |> R.bind
            (\cargs ->
                case info of
                    Env.Alias arity home argNames aliasedType ->
                        checkArity arity region name args <|
                            Can.CASTC_TAlias home name (List.map2 Tuple.pair argNames cargs) (Can.CASTC_Holey aliasedType)

                    Env.Union arity home ->
                        checkArity arity region name args <|
                            Can.CASTC_TType home name cargs
            )


checkArity : Int -> A.CRA_Region -> Name.CDN_Name -> List (A.CRA_Located arg) -> answer -> CResult i w answer
checkArity expected region name args answer =
    let
        actual : Int
        actual =
            List.length args
    in
    if expected == actual then
        R.ok answer

    else
        R.throw (Error.BadArity region Error.TypeArity name expected actual)



-- ADD FREE VARS


addFreeVars : Dict String Name.CDN_Name () -> Can.CASTC_Type -> Dict String Name.CDN_Name ()
addFreeVars freeVars tipe =
    case tipe of
        Can.CASTC_TLambda arg result ->
            addFreeVars (addFreeVars freeVars result) arg

        Can.CASTC_TVar var ->
            Dict.insert identity var () freeVars

        Can.CASTC_TType _ _ args ->
            List.foldl (\b c -> addFreeVars c b) freeVars args

        Can.CASTC_TRecord fields Nothing ->
            Dict.foldl compare (\_ b c -> addFieldFreeVars c b) freeVars fields

        Can.CASTC_TRecord fields (Just ext) ->
            Dict.foldl compare (\_ b c -> addFieldFreeVars c b) (Dict.insert identity ext () freeVars) fields

        Can.CASTC_TUnit ->
            freeVars

        Can.CASTC_TTuple a b maybeC ->
            case maybeC of
                Nothing ->
                    addFreeVars (addFreeVars freeVars a) b

                Just c ->
                    addFreeVars (addFreeVars (addFreeVars freeVars a) b) c

        Can.CASTC_TAlias _ _ args _ ->
            List.foldl (\( _, arg ) fvs -> addFreeVars fvs arg) freeVars args


addFieldFreeVars : Dict String Name.CDN_Name () -> Can.CASTC_FieldType -> Dict String Name.CDN_Name ()
addFieldFreeVars freeVars (Can.CASTC_FieldType _ tipe) =
    addFreeVars freeVars tipe
