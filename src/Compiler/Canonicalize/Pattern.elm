module Compiler.Canonicalize.Pattern exposing
    ( Bindings
    , DupsDict
    , PResult
    , canonicalize
    , verify
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map exposing (Dict)
import Utils.Main as Utils



-- RESULTS


type alias PResult i w a =
    R.RResult i w Error.Error a


type alias Bindings =
    Dict String Name.Name A.Region



-- VERIFY


verify : Error.DuplicatePatternContext -> PResult DupsDict w a -> PResult i w ( a, Bindings )
verify context (R.RResult k) =
    R.RResult <|
        \info warnings ->
            case k Dups.none warnings of
                Err (R.RErr _ warnings1 errors) ->
                    Err (R.RErr info warnings1 errors)

                Ok (R.ROk bindings warnings1 value) ->
                    case Dups.detect (Error.DuplicatePattern context) bindings of
                        R.RResult k1 ->
                            case k1 () () of
                                Err (R.RErr () () errs) ->
                                    Err (R.RErr info warnings1 errs)

                                Ok (R.ROk () () dict) ->
                                    Ok (R.ROk info warnings1 ( value, dict ))



-- CANONICALIZE


type alias DupsDict =
    Dups.Tracker A.Region


canonicalize : Env.Env -> Src.Pattern -> PResult DupsDict w Can.Pattern
canonicalize env (A.At region pattern) =
    R.fmap (A.At region) <|
        case pattern of
            Src.PAnything _ ->
                R.ok Can.PAnything

            Src.PVar name ->
                logVar name region (Can.PVar name)

            Src.PRecord fields ->
                logFields fields (Can.PRecord (List.map A.toValue fields))

            Src.PUnit ->
                R.ok Can.PUnit

            Src.PTuple a b cs ->
                R.ok Can.PTuple
                    |> R.apply (canonicalize env a)
                    |> R.apply (canonicalize env b)
                    |> R.apply (canonicalizeTuple region env cs)

            Src.PCtor nameRegion name patterns ->
                Env.findCtor nameRegion env name
                    |> R.bind (canonicalizeCtor env region name patterns)

            Src.PCtorQual nameRegion home name patterns ->
                Env.findCtorQual nameRegion env home name
                    |> R.bind (canonicalizeCtor env region name patterns)

            Src.PList patterns ->
                R.fmap Can.PList (canonicalizeList env patterns)

            Src.PCons first rest ->
                R.ok Can.PCons
                    |> R.apply (canonicalize env first)
                    |> R.apply (canonicalize env rest)

            Src.PAlias ptrn (A.At reg name) ->
                canonicalize env ptrn
                    |> R.bind (\cpattern -> logVar name reg (Can.PAlias cpattern name))

            Src.PChr chr ->
                R.ok (Can.PChr chr)

            Src.PStr str ->
                R.ok (Can.PStr str)

            Src.PInt int ->
                R.ok (Can.PInt int)


canonicalizeCtor : Env.Env -> A.Region -> Name.Name -> List Src.Pattern -> Env.Ctor -> PResult DupsDict w Can.Pattern_
canonicalizeCtor env region name patterns ctor =
    case ctor of
        Env.Ctor home tipe union index args ->
            let
                toCanonicalArg : Index.ZeroBased -> Src.Pattern -> Can.Type -> R.RResult DupsDict w Error.Error Can.PatternCtorArg
                toCanonicalArg argIndex argPattern argTipe =
                    R.fmap (Can.PatternCtorArg argIndex argTipe)
                        (canonicalize env argPattern)
            in
            Utils.indexedZipWithA toCanonicalArg patterns args
                |> R.bind
                    (\verifiedList ->
                        case verifiedList of
                            Index.LengthMatch cargs ->
                                if tipe == Name.bool && home == ModuleName.basics then
                                    R.ok (Can.PBool union (name == Name.true))

                                else
                                    R.ok (Can.PCtor { home = home, type_ = tipe, union = union, name = name, index = index, args = cargs })

                            Index.LengthMismatch actualLength expectedLength ->
                                R.throw (Error.BadArity region Error.PatternArity name expectedLength actualLength)
                    )

        Env.RecordCtor _ _ _ ->
            R.throw (Error.PatternHasRecordCtor region name)


canonicalizeTuple : A.Region -> Env.Env -> List Src.Pattern -> PResult DupsDict w (Maybe Can.Pattern)
canonicalizeTuple tupleRegion env extras =
    case extras of
        [] ->
            R.ok Nothing

        [ three ] ->
            R.fmap Just (canonicalize env three)

        _ ->
            R.throw (Error.TupleLargerThanThree tupleRegion)


canonicalizeList : Env.Env -> List Src.Pattern -> PResult DupsDict w (List Can.Pattern)
canonicalizeList env list =
    case list of
        [] ->
            R.ok []

        pattern :: otherPatterns ->
            R.ok (::)
                |> R.apply (canonicalize env pattern)
                |> R.apply (canonicalizeList env otherPatterns)



-- LOG BINDINGS


logVar : Name.Name -> A.Region -> a -> PResult DupsDict w a
logVar name region value =
    R.RResult <|
        \bindings warnings ->
            Ok (R.ROk (Dups.insert name region region bindings) warnings value)


logFields : List (A.Located Name.Name) -> a -> PResult DupsDict w a
logFields fields value =
    let
        addField : A.Located Name.Name -> Dups.Tracker A.Region -> Dups.Tracker A.Region
        addField (A.At region name) dict =
            Dups.insert name region region dict
    in
    R.RResult <|
        \bindings warnings ->
            Ok (R.ROk (List.foldl addField bindings fields) warnings value)
