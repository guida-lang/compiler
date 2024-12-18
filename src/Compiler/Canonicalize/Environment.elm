module Compiler.Canonicalize.Environment exposing
    ( Binop(..)
    , Ctor(..)
    , EResult
    , Env
    , Exposed
    , Info(..)
    , Qualified
    , Type(..)
    , Var(..)
    , addLocals
    , findBinop
    , findCtor
    , findCtorQual
    , findType
    , findTypeQual
    , mergeInfo
    )

import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import Maybe exposing (Maybe(..))
import Types as T



-- RESULT


type alias EResult i w a =
    R.RResult i w Error.CREC_Error a



-- ENVIRONMENT


type alias Env =
    { home : T.CEMN_Canonical
    , vars : Dict String T.CDN_Name Var
    , types : Exposed Type
    , ctors : Exposed Ctor
    , binops : Exposed Binop
    , q_vars : Qualified T.CASTC_Annotation
    , q_types : Qualified Type
    , q_ctors : Qualified Ctor
    }


type alias Exposed a =
    Dict String T.CDN_Name (Info a)


type alias Qualified a =
    Dict String T.CDN_Name (Dict String T.CDN_Name (Info a))



-- INFO


type Info a
    = Specific T.CEMN_Canonical a
    | Ambiguous T.CEMN_Canonical (OneOrMore.OneOrMore T.CEMN_Canonical)


mergeInfo : Info a -> Info a -> Info a
mergeInfo info1 info2 =
    case info1 of
        Specific h1 _ ->
            case info2 of
                Specific h2 _ ->
                    if h1 == h2 then
                        info1

                    else
                        Ambiguous h1 (OneOrMore.one h2)

                Ambiguous h2 hs2 ->
                    Ambiguous h1 (OneOrMore.more (OneOrMore.one h2) hs2)

        Ambiguous h1 hs1 ->
            case info2 of
                Specific h2 _ ->
                    Ambiguous h1 (OneOrMore.more hs1 (OneOrMore.one h2))

                Ambiguous h2 hs2 ->
                    Ambiguous h1 (OneOrMore.more hs1 (OneOrMore.more (OneOrMore.one h2) hs2))



-- VARIABLES


type Var
    = Local T.CRA_Region
    | TopLevel T.CRA_Region
    | Foreign T.CEMN_Canonical T.CASTC_Annotation
    | Foreigns T.CEMN_Canonical (OneOrMore.OneOrMore T.CEMN_Canonical)



-- TYPES


type Type
    = Alias Int T.CEMN_Canonical (List T.CDN_Name) T.CASTC_Type
    | Union Int T.CEMN_Canonical



-- CTORS


type Ctor
    = RecordCtor T.CEMN_Canonical (List T.CDN_Name) T.CASTC_Type
    | Ctor T.CEMN_Canonical T.CDN_Name T.CASTC_Union T.CDI_ZeroBased (List T.CASTC_Type)



-- BINOPS


type Binop
    = Binop T.CDN_Name T.CEMN_Canonical T.CDN_Name T.CASTC_Annotation T.CASTUB_Associativity T.CASTUB_Precedence



-- VARIABLE -- ADD LOCALS


addLocals : Dict String T.CDN_Name T.CRA_Region -> Env -> EResult i w Env
addLocals names env =
    R.fmap (\newVars -> { env | vars = newVars })
        (Dict.merge compare
            (\name region -> R.fmap (Dict.insert identity name (addLocalLeft name region)))
            (\name region var acc ->
                addLocalBoth name region var
                    |> R.bind (\var_ -> R.fmap (Dict.insert identity name var_) acc)
            )
            (\name var -> R.fmap (Dict.insert identity name var))
            names
            env.vars
            (R.ok Dict.empty)
        )


addLocalLeft : T.CDN_Name -> T.CRA_Region -> Var
addLocalLeft _ region =
    Local region


addLocalBoth : T.CDN_Name -> T.CRA_Region -> Var -> EResult i w Var
addLocalBoth name region var =
    case var of
        Foreign _ _ ->
            R.ok (Local region)

        Foreigns _ _ ->
            R.ok (Local region)

        Local parentRegion ->
            R.throw (Error.CREC_Shadowing name parentRegion region)

        TopLevel parentRegion ->
            R.throw (Error.CREC_Shadowing name parentRegion region)



-- FIND TYPE


findType : T.CRA_Region -> Env -> T.CDN_Name -> EResult i w Type
findType region { types, q_types } name =
    case Dict.get identity name types of
        Just (Specific _ tipe) ->
            R.ok tipe

        Just (Ambiguous h hs) ->
            R.throw (Error.CREC_AmbiguousType region Nothing name h hs)

        Nothing ->
            R.throw (Error.CREC_NotFoundType region Nothing name (toPossibleNames types q_types))


findTypeQual : T.CRA_Region -> Env -> T.CDN_Name -> T.CDN_Name -> EResult i w Type
findTypeQual region { types, q_types } prefix name =
    case Dict.get identity prefix q_types of
        Just qualified ->
            case Dict.get identity name qualified of
                Just (Specific _ tipe) ->
                    R.ok tipe

                Just (Ambiguous h hs) ->
                    R.throw (Error.CREC_AmbiguousType region (Just prefix) name h hs)

                Nothing ->
                    R.throw (Error.CREC_NotFoundType region (Just prefix) name (toPossibleNames types q_types))

        Nothing ->
            R.throw (Error.CREC_NotFoundType region (Just prefix) name (toPossibleNames types q_types))



-- FIND CTOR


findCtor : T.CRA_Region -> Env -> T.CDN_Name -> EResult i w Ctor
findCtor region { ctors, q_ctors } name =
    case Dict.get identity name ctors of
        Just (Specific _ ctor) ->
            R.ok ctor

        Just (Ambiguous h hs) ->
            R.throw (Error.CREC_AmbiguousVariant region Nothing name h hs)

        Nothing ->
            R.throw (Error.CREC_NotFoundVariant region Nothing name (toPossibleNames ctors q_ctors))


findCtorQual : T.CRA_Region -> Env -> T.CDN_Name -> T.CDN_Name -> EResult i w Ctor
findCtorQual region { ctors, q_ctors } prefix name =
    case Dict.get identity prefix q_ctors of
        Just qualified ->
            case Dict.get identity name qualified of
                Just (Specific _ pattern) ->
                    R.ok pattern

                Just (Ambiguous h hs) ->
                    R.throw (Error.CREC_AmbiguousVariant region (Just prefix) name h hs)

                Nothing ->
                    R.throw (Error.CREC_NotFoundVariant region (Just prefix) name (toPossibleNames ctors q_ctors))

        Nothing ->
            R.throw (Error.CREC_NotFoundVariant region (Just prefix) name (toPossibleNames ctors q_ctors))



-- FIND BINOP


findBinop : T.CRA_Region -> Env -> T.CDN_Name -> EResult i w Binop
findBinop region { binops } name =
    case Dict.get identity name binops of
        Just (Specific _ binop) ->
            R.ok binop

        Just (Ambiguous h hs) ->
            R.throw (Error.CREC_AmbiguousBinop region name h hs)

        Nothing ->
            R.throw (Error.CREC_NotFoundBinop region name (EverySet.fromList identity (Dict.keys compare binops)))



-- TO POSSIBLE NAMES


toPossibleNames : Exposed a -> Qualified a -> Error.CREC_PossibleNames
toPossibleNames exposed qualified =
    Error.CREC_PossibleNames (EverySet.fromList identity (Dict.keys compare exposed)) (Dict.map (\_ -> Dict.keys compare >> EverySet.fromList identity) qualified)
