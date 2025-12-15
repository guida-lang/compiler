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
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
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
                R.RErr _ warnings1 errors ->
                    R.RErr info warnings1 errors

                R.ROk bindings warnings1 value ->
                    case Dups.detect (Error.DuplicatePattern context) bindings of
                        R.RResult k1 ->
                            case k1 () () of
                                R.RErr () () errs ->
                                    R.RErr info warnings1 errs

                                R.ROk () () dict ->
                                    R.ROk info warnings1 ( value, dict )



-- CANONICALIZE


type alias DupsDict =
    Dups.Tracker A.Region


canonicalize : Target -> SyntaxVersion -> Env.Env -> Src.Pattern -> PResult DupsDict w Can.Pattern
canonicalize target syntaxVersion env (A.At region pattern) =
    case pattern of
        Src.PAnything _ ->
            R.ok Can.PAnything
                |> R.fmap (A.At region)

        Src.PVar name ->
            logVar name region (Can.PVar name)
                |> R.fmap (A.At region)

        Src.PRecord ( _, c2Fields ) ->
            let
                fields : List (A.Located Name.Name)
                fields =
                    List.map Src.c2Value c2Fields
            in
            logFields fields (Can.PRecord (List.map A.toValue fields))
                |> R.fmap (A.At region)

        Src.PUnit _ ->
            R.ok Can.PUnit
                |> R.fmap (A.At region)

        Src.PTuple ( _, a ) ( _, b ) cs ->
            R.fmap Can.PTuple (canonicalize target syntaxVersion env a)
                |> R.apply (canonicalize target syntaxVersion env b)
                |> R.apply (canonicalizeTuple target syntaxVersion region env (List.map Src.c2Value cs))
                |> R.fmap (A.At region)

        Src.PCtor nameRegion name patterns ->
            Env.findCtor target nameRegion env name
                |> R.bind (canonicalizeCtor target syntaxVersion env region name (List.map Src.c1Value patterns))
                |> R.fmap (A.At region)

        Src.PCtorQual nameRegion home name patterns ->
            Env.findCtorQual target nameRegion env home name
                |> R.bind (canonicalizeCtor target syntaxVersion env region name (List.map Src.c1Value patterns))
                |> R.fmap (A.At region)

        Src.PList ( _, patterns ) ->
            R.fmap Can.PList (canonicalizeList target syntaxVersion env (List.map Src.c2Value patterns))
                |> R.fmap (A.At region)

        Src.PCons ( _, first ) ( _, rest ) ->
            R.fmap Can.PCons (canonicalize target syntaxVersion env first)
                |> R.apply (canonicalize target syntaxVersion env rest)
                |> R.fmap (A.At region)

        Src.PAlias ( _, ptrn ) ( _, A.At reg name ) ->
            canonicalize target syntaxVersion env ptrn
                |> R.bind (\cpattern -> logVar name reg (Can.PAlias cpattern name))
                |> R.fmap (A.At region)

        Src.PChr chr ->
            R.ok (Can.PChr chr)
                |> R.fmap (A.At region)

        Src.PStr str multiline ->
            R.ok (Can.PStr str multiline)
                |> R.fmap (A.At region)

        Src.PInt int _ ->
            R.ok (Can.PInt int)
                |> R.fmap (A.At region)

        Src.PParens ( _, pattern_ ) ->
            canonicalize target syntaxVersion env pattern_


canonicalizeCtor : Target -> SyntaxVersion -> Env.Env -> A.Region -> Name.Name -> List Src.Pattern -> Env.Ctor -> PResult DupsDict w Can.Pattern_
canonicalizeCtor target syntaxVersion env region name patterns ctor =
    case ctor of
        Env.Ctor home tipe union index args ->
            let
                toCanonicalArg : Index.ZeroBased -> Src.Pattern -> Can.Type -> R.RResult DupsDict w Error.Error Can.PatternCtorArg
                toCanonicalArg argIndex argPattern argTipe =
                    R.fmap (Can.PatternCtorArg argIndex argTipe)
                        (canonicalize target syntaxVersion env argPattern)
            in
            Utils.indexedZipWithA toCanonicalArg patterns args
                |> R.bind
                    (\verifiedList ->
                        case verifiedList of
                            Index.LengthMatch cargs ->
                                if tipe == Name.bool && (home == ModuleName.basics target) then
                                    R.ok (Can.PBool union (name == Name.true))

                                else
                                    R.ok (Can.PCtor { home = home, type_ = tipe, union = union, name = name, index = index, args = cargs })

                            Index.LengthMismatch actualLength expectedLength ->
                                R.throw (Error.BadArity region Error.PatternArity name expectedLength actualLength)
                    )

        Env.RecordCtor _ _ _ ->
            R.throw (Error.PatternHasRecordCtor region name)


canonicalizeTuple : Target -> SyntaxVersion -> A.Region -> Env.Env -> List Src.Pattern -> PResult DupsDict w (List Can.Pattern)
canonicalizeTuple target syntaxVersion tupleRegion env extras =
    case extras of
        [] ->
            R.ok []

        [ three ] ->
            R.fmap List.singleton (canonicalize target syntaxVersion env three)

        _ ->
            case syntaxVersion of
                SV.Elm ->
                    R.throw (Error.TupleLargerThanThree tupleRegion)

                SV.Guida ->
                    R.traverse (canonicalize target syntaxVersion env) extras


canonicalizeList : Target -> SyntaxVersion -> Env.Env -> List Src.Pattern -> PResult DupsDict w (List Can.Pattern)
canonicalizeList target syntaxVersion env list =
    case list of
        [] ->
            R.ok []

        pattern :: otherPatterns ->
            R.fmap (::) (canonicalize target syntaxVersion env pattern)
                |> R.apply (canonicalizeList target syntaxVersion env otherPatterns)



-- LOG BINDINGS


logVar : Name.Name -> A.Region -> a -> PResult DupsDict w a
logVar name region value =
    R.RResult <|
        \bindings warnings ->
            R.ROk (Dups.insert name region region bindings) warnings value


logFields : List (A.Located Name.Name) -> a -> PResult DupsDict w a
logFields fields value =
    let
        addField : A.Located Name.Name -> Dups.Tracker A.Region -> Dups.Tracker A.Region
        addField (A.At region name) dict =
            Dups.insert name region region dict
    in
    R.RResult <|
        \bindings warnings ->
            R.ROk (List.foldl addField bindings fields) warnings value
