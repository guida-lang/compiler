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


canonicalize : SyntaxVersion -> Env.Env -> Src.Pattern -> PResult DupsDict w Can.Pattern
canonicalize syntaxVersion env (A.At region pattern) =
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
            R.fmap Can.PTuple (canonicalize syntaxVersion env a)
                |> R.apply (canonicalize syntaxVersion env b)
                |> R.apply (canonicalizeTuple syntaxVersion region env (List.map Src.c2Value cs))
                |> R.fmap (A.At region)

        Src.PCtor nameRegion name patterns ->
            Env.findCtor nameRegion env name
                |> R.bind (canonicalizeCtor syntaxVersion env region name (List.map Src.c1Value patterns))
                |> R.fmap (A.At region)

        Src.PCtorQual nameRegion home name patterns ->
            Env.findCtorQual nameRegion env home name
                |> R.bind (canonicalizeCtor syntaxVersion env region name (List.map Src.c1Value patterns))
                |> R.fmap (A.At region)

        Src.PList ( _, patterns ) ->
            R.fmap Can.PList (canonicalizeList syntaxVersion env (List.map Src.c2Value patterns))
                |> R.fmap (A.At region)

        Src.PCons ( _, first ) ( _, rest ) ->
            R.fmap Can.PCons (canonicalize syntaxVersion env first)
                |> R.apply (canonicalize syntaxVersion env rest)
                |> R.fmap (A.At region)

        Src.PAlias ( _, ptrn ) ( _, A.At reg name ) ->
            canonicalize syntaxVersion env ptrn
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
            canonicalize syntaxVersion env pattern_


canonicalizeCtor : SyntaxVersion -> Env.Env -> A.Region -> Name.Name -> List Src.Pattern -> Env.Ctor -> PResult DupsDict w Can.Pattern_
canonicalizeCtor syntaxVersion env region name patterns ctor =
    case ctor of
        Env.Ctor home tipe union index args ->
            let
                toCanonicalArg : Index.ZeroBased -> Src.Pattern -> Can.Type -> R.RResult DupsDict w Error.Error Can.PatternCtorArg
                toCanonicalArg argIndex argPattern argTipe =
                    R.fmap (Can.PatternCtorArg argIndex argTipe)
                        (canonicalize syntaxVersion env argPattern)
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


canonicalizeTuple : SyntaxVersion -> A.Region -> Env.Env -> List Src.Pattern -> PResult DupsDict w (List Can.Pattern)
canonicalizeTuple syntaxVersion tupleRegion env extras =
    case extras of
        [] ->
            R.ok []

        [ three ] ->
            R.fmap List.singleton (canonicalize syntaxVersion env three)

        _ ->
            case syntaxVersion of
                SV.Elm ->
                    R.throw (Error.TupleLargerThanThree tupleRegion)

                SV.Guida ->
                    R.traverse (canonicalize syntaxVersion env) extras


canonicalizeList : SyntaxVersion -> Env.Env -> List Src.Pattern -> PResult DupsDict w (List Can.Pattern)
canonicalizeList syntaxVersion env list =
    case list of
        [] ->
            R.ok []

        pattern :: otherPatterns ->
            R.fmap (::) (canonicalize syntaxVersion env pattern)
                |> R.apply (canonicalizeList syntaxVersion env otherPatterns)



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
