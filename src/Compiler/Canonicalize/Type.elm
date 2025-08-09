module Compiler.Canonicalize.Type exposing
    ( CResult
    , canonicalize
    , toAnnotation
    )

import Basics.Extra exposing (flip)
import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Data.Name as Name
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Utils.Main as Utils



-- RESULT


type alias CResult i w a =
    R.RResult i w Error.Error a



-- TO ANNOTATION


toAnnotation : SyntaxVersion -> Env.Env -> Src.Type -> CResult i w Can.Annotation
toAnnotation syntaxVersion env srcType =
    canonicalize syntaxVersion env srcType
        |> R.bind (\tipe -> R.ok (Can.Forall (addFreeVars Dict.empty tipe) tipe))



-- CANONICALIZE TYPES


canonicalize : SyntaxVersion -> Env.Env -> Src.Type -> CResult i w Can.Type
canonicalize syntaxVersion env (A.At typeRegion tipe) =
    case tipe of
        Src.TVar x ->
            R.ok (Can.TVar x)

        Src.TType region name args ->
            Env.findType region env name
                |> R.bind (canonicalizeType syntaxVersion env typeRegion name (List.map Tuple.second args))

        Src.TTypeQual region home name args ->
            Env.findTypeQual region env home name
                |> R.bind (canonicalizeType syntaxVersion env typeRegion name (List.map Tuple.second args))

        Src.TLambda ( _, a ) ( _, b ) ->
            R.fmap Can.TLambda (canonicalize syntaxVersion env a)
                |> R.apply (canonicalize syntaxVersion env b)

        Src.TRecord fields maybeExt _ ->
            Dups.checkFields (canonicalizeFields syntaxVersion env fields)
                |> R.bind (Utils.sequenceADict identity compare)
                |> R.fmap (\cfields -> Can.TRecord cfields (Maybe.map (\( _, A.At _ ext ) -> ext) maybeExt))

        Src.TUnit ->
            R.ok Can.TUnit

        Src.TTuple ( _, a ) ( _, b ) cs ->
            R.fmap Can.TTuple (canonicalize syntaxVersion env a)
                |> R.apply (canonicalize syntaxVersion env b)
                |> R.apply
                    (case cs of
                        [] ->
                            R.ok []

                        [ ( _, c ) ] ->
                            canonicalize syntaxVersion env c
                                |> R.fmap List.singleton

                        _ ->
                            case syntaxVersion of
                                SV.Elm ->
                                    R.throw (Error.TupleLargerThanThree typeRegion)

                                SV.Guida ->
                                    R.traverse (canonicalize syntaxVersion env) (List.map Src.c2EolValue cs)
                    )

        Src.TParens ( _, tipe_ ) ->
            canonicalize syntaxVersion env tipe_


canonicalizeFields : SyntaxVersion -> Env.Env -> List (Src.C2 ( Src.C1 (A.Located Name.Name), Src.C1 Src.Type )) -> List ( A.Located Name.Name, CResult i w Can.FieldType )
canonicalizeFields syntaxVersion env fields =
    let
        canonicalizeField : Int -> Src.C2 ( Src.C1 a, Src.C1 Src.Type ) -> ( a, R.RResult i w Error.Error Can.FieldType )
        canonicalizeField index ( _, ( ( _, name ), ( _, srcType ) ) ) =
            ( name, R.fmap (Can.FieldType index) (canonicalize syntaxVersion env srcType) )
    in
    List.indexedMap canonicalizeField fields



-- CANONICALIZE TYPE


canonicalizeType : SyntaxVersion -> Env.Env -> A.Region -> Name.Name -> List Src.Type -> Env.Type -> CResult i w Can.Type
canonicalizeType syntaxVersion env region name args info =
    R.traverse (canonicalize syntaxVersion env) args
        |> R.bind
            (\cargs ->
                case info of
                    Env.Alias arity home argNames aliasedType ->
                        checkArity arity region name args <|
                            Can.TAlias home name (List.map2 Tuple.pair argNames cargs) (Can.Holey aliasedType)

                    Env.Union arity home ->
                        checkArity arity region name args <|
                            Can.TType home name cargs
            )


checkArity : Int -> A.Region -> Name.Name -> List (A.Located arg) -> answer -> CResult i w answer
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


addFreeVars : Dict String Name.Name () -> Can.Type -> Dict String Name.Name ()
addFreeVars freeVars tipe =
    case tipe of
        Can.TLambda arg result ->
            addFreeVars (addFreeVars freeVars result) arg

        Can.TVar var ->
            Dict.insert identity var () freeVars

        Can.TType _ _ args ->
            List.foldl (\b c -> addFreeVars c b) freeVars args

        Can.TRecord fields Nothing ->
            Dict.foldl compare (\_ b c -> addFieldFreeVars c b) freeVars fields

        Can.TRecord fields (Just ext) ->
            Dict.foldl compare (\_ b c -> addFieldFreeVars c b) (Dict.insert identity ext () freeVars) fields

        Can.TUnit ->
            freeVars

        Can.TTuple a b cs ->
            List.foldl (flip addFreeVars) (addFreeVars (addFreeVars freeVars a) b) cs

        Can.TAlias _ _ args _ ->
            List.foldl (\( _, arg ) fvs -> addFreeVars fvs arg) freeVars args


addFieldFreeVars : Dict String Name.Name () -> Can.FieldType -> Dict String Name.Name ()
addFieldFreeVars freeVars (Can.FieldType _ tipe) =
    addFreeVars freeVars tipe
