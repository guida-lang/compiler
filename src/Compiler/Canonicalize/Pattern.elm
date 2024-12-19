module Compiler.Canonicalize.Pattern exposing
    ( Bindings
    , DupsDict
    , PResult
    , canonicalize
    , verify
    )

import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Result as R
import Data.Map exposing (Dict)
import Types as T
import Utils.Main as Utils



-- RESULTS


type alias PResult i w a =
    R.RResult i w T.CREC_Error a


type alias Bindings =
    Dict String T.CDN_Name T.CRA_Region



-- VERIFY


verify : T.CREC_DuplicatePatternContext -> PResult DupsDict w a -> PResult i w ( a, Bindings )
verify context (R.RResult k) =
    R.RResult <|
        \info warnings ->
            case k Dups.none warnings of
                Err (R.RErr _ warnings1 errors) ->
                    Err (R.RErr info warnings1 errors)

                Ok (R.ROk bindings warnings1 value) ->
                    case Dups.detect (T.CREC_DuplicatePattern context) bindings of
                        R.RResult k1 ->
                            case k1 () () of
                                Err (R.RErr () () errs) ->
                                    Err (R.RErr info warnings1 errs)

                                Ok (R.ROk () () dict) ->
                                    Ok (R.ROk info warnings1 ( value, dict ))



-- CANONICALIZE


type alias DupsDict =
    Dups.Tracker T.CRA_Region


canonicalize : Env.Env -> T.CASTS_Pattern -> PResult DupsDict w T.CASTC_Pattern
canonicalize env (T.CRA_At region pattern) =
    R.fmap (T.CRA_At region) <|
        case pattern of
            T.CASTS_PAnything ->
                R.ok T.CASTC_PAnything

            T.CASTS_PVar name ->
                logVar name region (T.CASTC_PVar name)

            T.CASTS_PRecord fields ->
                logFields fields (T.CASTC_PRecord (List.map A.toValue fields))

            T.CASTS_PUnit ->
                R.ok T.CASTC_PUnit

            T.CASTS_PTuple a b cs ->
                R.ok T.CASTC_PTuple
                    |> R.apply (canonicalize env a)
                    |> R.apply (canonicalize env b)
                    |> R.apply (canonicalizeTuple region env cs)

            T.CASTS_PCtor nameRegion name patterns ->
                Env.findCtor nameRegion env name
                    |> R.bind (canonicalizeCtor env region name patterns)

            T.CASTS_PCtorQual nameRegion home name patterns ->
                Env.findCtorQual nameRegion env home name
                    |> R.bind (canonicalizeCtor env region name patterns)

            T.CASTS_PList patterns ->
                R.fmap T.CASTC_PList (canonicalizeList env patterns)

            T.CASTS_PCons first rest ->
                R.ok T.CASTC_PCons
                    |> R.apply (canonicalize env first)
                    |> R.apply (canonicalize env rest)

            T.CASTS_PAlias ptrn (T.CRA_At reg name) ->
                canonicalize env ptrn
                    |> R.bind (\cpattern -> logVar name reg (T.CASTC_PAlias cpattern name))

            T.CASTS_PChr chr ->
                R.ok (T.CASTC_PChr chr)

            T.CASTS_PStr str ->
                R.ok (T.CASTC_PStr str)

            T.CASTS_PInt int ->
                R.ok (T.CASTC_PInt int)


canonicalizeCtor : Env.Env -> T.CRA_Region -> T.CDN_Name -> List T.CASTS_Pattern -> Env.Ctor -> PResult DupsDict w T.CASTC_Pattern_
canonicalizeCtor env region name patterns ctor =
    case ctor of
        Env.Ctor home tipe union index args ->
            let
                toCanonicalArg : T.CDI_ZeroBased -> T.CASTS_Pattern -> T.CASTC_Type -> R.RResult DupsDict w T.CREC_Error T.CASTC_PatternCtorArg
                toCanonicalArg argIndex argPattern argTipe =
                    R.fmap (T.CASTC_PatternCtorArg argIndex argTipe)
                        (canonicalize env argPattern)
            in
            Utils.indexedZipWithA toCanonicalArg patterns args
                |> R.bind
                    (\verifiedList ->
                        case verifiedList of
                            Index.LengthMatch cargs ->
                                if tipe == Name.bool && home == ModuleName.basics then
                                    R.ok (T.CASTC_PBool union (name == Name.true))

                                else
                                    R.ok (T.CASTC_PCtor { home = home, type_ = tipe, union = union, name = name, index = index, args = cargs })

                            Index.LengthMismatch actualLength expectedLength ->
                                R.throw (T.CREC_BadArity region T.CREC_PatternArity name expectedLength actualLength)
                    )

        Env.RecordCtor _ _ _ ->
            R.throw (T.CREC_PatternHasRecordCtor region name)


canonicalizeTuple : T.CRA_Region -> Env.Env -> List T.CASTS_Pattern -> PResult DupsDict w (Maybe T.CASTC_Pattern)
canonicalizeTuple tupleRegion env extras =
    case extras of
        [] ->
            R.ok Nothing

        [ three ] ->
            R.fmap Just (canonicalize env three)

        _ ->
            R.throw (T.CREC_TupleLargerThanThree tupleRegion)


canonicalizeList : Env.Env -> List T.CASTS_Pattern -> PResult DupsDict w (List T.CASTC_Pattern)
canonicalizeList env list =
    case list of
        [] ->
            R.ok []

        pattern :: otherPatterns ->
            R.ok (::)
                |> R.apply (canonicalize env pattern)
                |> R.apply (canonicalizeList env otherPatterns)



-- LOG BINDINGS


logVar : T.CDN_Name -> T.CRA_Region -> a -> PResult DupsDict w a
logVar name region value =
    R.RResult <|
        \bindings warnings ->
            Ok (R.ROk (Dups.insert name region region bindings) warnings value)


logFields : List (T.CRA_Located T.CDN_Name) -> a -> PResult DupsDict w a
logFields fields value =
    let
        addField : T.CRA_Located T.CDN_Name -> Dups.Tracker T.CRA_Region -> Dups.Tracker T.CRA_Region
        addField (T.CRA_At region name) dict =
            Dups.insert name region region dict
    in
    R.RResult <|
        \bindings warnings ->
            Ok (R.ROk (List.foldl addField bindings fields) warnings value)
