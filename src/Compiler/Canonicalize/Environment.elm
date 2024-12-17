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

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import Maybe exposing (Maybe(..))
import System.TypeCheck.IO exposing (CEMN_Canonical)



-- RESULT


type alias EResult i w a =
    R.RResult i w Error.Error a



-- ENVIRONMENT


type alias Env =
    { home : CEMN_Canonical
    , vars : Dict String Name.CDN_Name Var
    , types : Exposed Type
    , ctors : Exposed Ctor
    , binops : Exposed Binop
    , q_vars : Qualified Can.CASTC_Annotation
    , q_types : Qualified Type
    , q_ctors : Qualified Ctor
    }


type alias Exposed a =
    Dict String Name.CDN_Name (Info a)


type alias Qualified a =
    Dict String Name.CDN_Name (Dict String Name.CDN_Name (Info a))



-- INFO


type Info a
    = Specific CEMN_Canonical a
    | Ambiguous CEMN_Canonical (OneOrMore.OneOrMore CEMN_Canonical)


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
    = Local A.CRA_Region
    | TopLevel A.CRA_Region
    | Foreign CEMN_Canonical Can.CASTC_Annotation
    | Foreigns CEMN_Canonical (OneOrMore.OneOrMore CEMN_Canonical)



-- TYPES


type Type
    = Alias Int CEMN_Canonical (List Name.CDN_Name) Can.CASTC_Type
    | Union Int CEMN_Canonical



-- CTORS


type Ctor
    = RecordCtor CEMN_Canonical (List Name.CDN_Name) Can.CASTC_Type
    | Ctor CEMN_Canonical Name.CDN_Name Can.CASTC_Union Index.CDI_ZeroBased (List Can.CASTC_Type)



-- BINOPS


type Binop
    = Binop Name.CDN_Name CEMN_Canonical Name.CDN_Name Can.CASTC_Annotation Binop.CASTU_Associativity Binop.CASTU_Precedence



-- VARIABLE -- ADD LOCALS


addLocals : Dict String Name.CDN_Name A.CRA_Region -> Env -> EResult i w Env
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


addLocalLeft : Name.CDN_Name -> A.CRA_Region -> Var
addLocalLeft _ region =
    Local region


addLocalBoth : Name.CDN_Name -> A.CRA_Region -> Var -> EResult i w Var
addLocalBoth name region var =
    case var of
        Foreign _ _ ->
            R.ok (Local region)

        Foreigns _ _ ->
            R.ok (Local region)

        Local parentRegion ->
            R.throw (Error.Shadowing name parentRegion region)

        TopLevel parentRegion ->
            R.throw (Error.Shadowing name parentRegion region)



-- FIND TYPE


findType : A.CRA_Region -> Env -> Name.CDN_Name -> EResult i w Type
findType region { types, q_types } name =
    case Dict.get identity name types of
        Just (Specific _ tipe) ->
            R.ok tipe

        Just (Ambiguous h hs) ->
            R.throw (Error.AmbiguousType region Nothing name h hs)

        Nothing ->
            R.throw (Error.NotFoundType region Nothing name (toPossibleNames types q_types))


findTypeQual : A.CRA_Region -> Env -> Name.CDN_Name -> Name.CDN_Name -> EResult i w Type
findTypeQual region { types, q_types } prefix name =
    case Dict.get identity prefix q_types of
        Just qualified ->
            case Dict.get identity name qualified of
                Just (Specific _ tipe) ->
                    R.ok tipe

                Just (Ambiguous h hs) ->
                    R.throw (Error.AmbiguousType region (Just prefix) name h hs)

                Nothing ->
                    R.throw (Error.NotFoundType region (Just prefix) name (toPossibleNames types q_types))

        Nothing ->
            R.throw (Error.NotFoundType region (Just prefix) name (toPossibleNames types q_types))



-- FIND CTOR


findCtor : A.CRA_Region -> Env -> Name.CDN_Name -> EResult i w Ctor
findCtor region { ctors, q_ctors } name =
    case Dict.get identity name ctors of
        Just (Specific _ ctor) ->
            R.ok ctor

        Just (Ambiguous h hs) ->
            R.throw (Error.AmbiguousVariant region Nothing name h hs)

        Nothing ->
            R.throw (Error.NotFoundVariant region Nothing name (toPossibleNames ctors q_ctors))


findCtorQual : A.CRA_Region -> Env -> Name.CDN_Name -> Name.CDN_Name -> EResult i w Ctor
findCtorQual region { ctors, q_ctors } prefix name =
    case Dict.get identity prefix q_ctors of
        Just qualified ->
            case Dict.get identity name qualified of
                Just (Specific _ pattern) ->
                    R.ok pattern

                Just (Ambiguous h hs) ->
                    R.throw (Error.AmbiguousVariant region (Just prefix) name h hs)

                Nothing ->
                    R.throw (Error.NotFoundVariant region (Just prefix) name (toPossibleNames ctors q_ctors))

        Nothing ->
            R.throw (Error.NotFoundVariant region (Just prefix) name (toPossibleNames ctors q_ctors))



-- FIND BINOP


findBinop : A.CRA_Region -> Env -> Name.CDN_Name -> EResult i w Binop
findBinop region { binops } name =
    case Dict.get identity name binops of
        Just (Specific _ binop) ->
            R.ok binop

        Just (Ambiguous h hs) ->
            R.throw (Error.AmbiguousBinop region name h hs)

        Nothing ->
            R.throw (Error.NotFoundBinop region name (EverySet.fromList identity (Dict.keys compare binops)))



-- TO POSSIBLE NAMES


toPossibleNames : Exposed a -> Qualified a -> Error.PossibleNames
toPossibleNames exposed qualified =
    Error.PossibleNames (EverySet.fromList identity (Dict.keys compare exposed)) (Dict.map (\_ -> Dict.keys compare >> EverySet.fromList identity) qualified)
