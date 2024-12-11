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
import System.TypeCheck.IO exposing (Canonical)



-- RESULT


type alias EResult i w a =
    R.RResult i w Error.Error a



-- ENVIRONMENT


type alias Env =
    { home : Canonical
    , vars : Dict String Name.Name Var
    , types : Exposed Type
    , ctors : Exposed Ctor
    , binops : Exposed Binop
    , q_vars : Qualified Can.Annotation
    , q_types : Qualified Type
    , q_ctors : Qualified Ctor
    }


type alias Exposed a =
    Dict String Name.Name (Info a)


type alias Qualified a =
    Dict String Name.Name (Dict String Name.Name (Info a))



-- INFO


type Info a
    = Specific Canonical a
    | Ambiguous Canonical (OneOrMore.OneOrMore Canonical)


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
    = Local A.Region
    | TopLevel A.Region
    | Foreign Canonical Can.Annotation
    | Foreigns Canonical (OneOrMore.OneOrMore Canonical)



-- TYPES


type Type
    = Alias Int Canonical (List Name.Name) Can.Type
    | Union Int Canonical



-- CTORS


type Ctor
    = RecordCtor Canonical (List Name.Name) Can.Type
    | Ctor Canonical Name.Name Can.Union Index.ZeroBased (List Can.Type)



-- BINOPS


type Binop
    = Binop Name.Name Canonical Name.Name Can.Annotation Binop.Associativity Binop.Precedence



-- VARIABLE -- ADD LOCALS


addLocals : Dict String Name.Name A.Region -> Env -> EResult i w Env
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


addLocalLeft : Name.Name -> A.Region -> Var
addLocalLeft _ region =
    Local region


addLocalBoth : Name.Name -> A.Region -> Var -> EResult i w Var
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


findType : A.Region -> Env -> Name.Name -> EResult i w Type
findType region { types, q_types } name =
    case Dict.get identity name types of
        Just (Specific _ tipe) ->
            R.ok tipe

        Just (Ambiguous h hs) ->
            R.throw (Error.AmbiguousType region Nothing name h hs)

        Nothing ->
            R.throw (Error.NotFoundType region Nothing name (toPossibleNames types q_types))


findTypeQual : A.Region -> Env -> Name.Name -> Name.Name -> EResult i w Type
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


findCtor : A.Region -> Env -> Name.Name -> EResult i w Ctor
findCtor region { ctors, q_ctors } name =
    case Dict.get identity name ctors of
        Just (Specific _ ctor) ->
            R.ok ctor

        Just (Ambiguous h hs) ->
            R.throw (Error.AmbiguousVariant region Nothing name h hs)

        Nothing ->
            R.throw (Error.NotFoundVariant region Nothing name (toPossibleNames ctors q_ctors))


findCtorQual : A.Region -> Env -> Name.Name -> Name.Name -> EResult i w Ctor
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


findBinop : A.Region -> Env -> Name.Name -> EResult i w Binop
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
