module Compiler.Nitpick.PatternMatches exposing
    ( Context(..)
    , Error(..)
    , Literal(..)
    , Pattern(..)
    , check
    , errorDecoder
    , errorEncoder
    )

{-| Pattern match exhaustiveness and redundancy checking for canonical modules.

This module simplifies canonical patterns into a reduced representation, then
uses Luc Maranget's pattern match warning algorithm to detect missing cases and
redundant branches.

The algorithm used here comes from "Warnings for Pattern Matching"
by Luc Maranget. Check it out for more information!

<http://moscova.inria.fr/~maranget/papers/warn/warn.pdf>

-}

import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Generate.Target as Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import List.Extra as List
import Prelude
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- PATTERN


{-| A simplified version of canonical patterns used by the exhaustive and
redundant pattern checker.
-}
type Pattern
    = Anything
    | Literal Literal
    | Ctor Can.Union Name.Name (List Pattern)


{-| A literal value used inside simplified patterns.
-}
type Literal
    = Chr String
    | Str String
    | Int Int



-- CREATE SIMPLIFIED PATTERNS


{-| Convert a canonical pattern into the simplified pattern representation
used by this checker.
-}
simplify : Target -> Can.Pattern -> Pattern
simplify target (A.At _ pattern) =
    case pattern of
        Can.PAnything ->
            Anything

        Can.PVar _ ->
            Anything

        Can.PRecord _ ->
            Anything

        Can.PUnit ->
            Ctor unit unitName []

        Can.PTuple a b [] ->
            Ctor pair pairName [ simplify target a, simplify target b ]

        Can.PTuple a b [ c ] ->
            Ctor triple tripleName [ simplify target a, simplify target b, simplify target c ]

        Can.PTuple a b cs ->
            Ctor nTuple nTupleName (List.map (simplify target) (a :: b :: cs))

        Can.PCtor { union, name, args } ->
            Ctor union name <|
                List.map (\(Can.PatternCtorArg _ _ arg) -> simplify target arg) args

        Can.PList entries ->
            List.foldr (cons target) (nil target) entries

        Can.PCons hd tl ->
            cons target hd (simplify target tl)

        Can.PAlias subPattern _ ->
            simplify target subPattern

        Can.PInt int ->
            Literal (Int int)

        Can.PStr str _ ->
            Literal (Str str)

        Can.PChr chr ->
            Literal (Chr chr)

        Can.PBool union bool ->
            Ctor union
                (if bool then
                    Name.true

                 else
                    Name.false
                )
                []


{-| Build a simplified list cons pattern for the given head and tail.
-}
cons : Target -> Can.Pattern -> Pattern -> Pattern
cons target hd tl =
    Ctor (list target) consName [ simplify target hd, tl ]


{-| Build a simplified empty list pattern.
-}
nil : Target -> Pattern
nil target =
    Ctor (list target) nilName []



-- BUILT-IN UNIONS


{-| The built-in unit constructor pattern.
-}
unit : Can.Union
unit =
    let
        ctor : Can.Ctor
        ctor =
            Can.Ctor unitName Index.first 0 []
    in
    Can.Union [] [ ctor ] 1 Can.Normal


{-| The built-in pair constructor pattern.
-}
pair : Can.Union
pair =
    let
        ctor : Can.Ctor
        ctor =
            Can.Ctor pairName Index.first 2 [ Can.TVar "a", Can.TVar "b" ]
    in
    Can.Union [ "a", "b" ] [ ctor ] 1 Can.Normal


{-| The built-in triple constructor pattern.
-}
triple : Can.Union
triple =
    let
        ctor : Can.Ctor
        ctor =
            Can.Ctor tripleName Index.first 3 [ Can.TVar "a", Can.TVar "b", Can.TVar "c" ]
    in
    Can.Union [ "a", "b", "c" ] [ ctor ] 1 Can.Normal


{-| The built-in n-tuple constructor pattern.
-}
nTuple : Can.Union
nTuple =
    let
        ctor : Can.Ctor
        ctor =
            Can.Ctor nTupleName Index.first 3 [ Can.TVar "a", Can.TVar "b", Can.TVar "cs" ]
    in
    Can.Union [ "a", "b", "cs" ] [ ctor ] 1 Can.Normal


{-| The built-in list constructor pattern for the current target.
-}
list : Target -> Can.Union
list target =
    let
        nilCtor : Can.Ctor
        nilCtor =
            Can.Ctor nilName Index.first 0 []

        consCtor : Can.Ctor
        consCtor =
            Can.Ctor consName
                Index.second
                2
                [ Can.TVar "a"
                , Can.TType (ModuleName.list target) Name.list [ Can.TVar "a" ]
                ]
    in
    Can.Union [ "a" ] [ nilCtor, consCtor ] 2 Can.Normal


{-| The built-in unit constructor name.
-}
unitName : Name.Name
unitName =
    "#0"


{-| The built-in pair constructor name.
-}
pairName : Name.Name
pairName =
    "#2"


{-| The built-in triple constructor name.
-}
tripleName : Name.Name
tripleName =
    "#3"


{-| The built-in n-tuple constructor name.
-}
nTupleName : Name.Name
nTupleName =
    "#N"


{-| The built-in list cons constructor name.
-}
consName : Name.Name
consName =
    "::"


{-| The built-in list nil constructor name.
-}
nilName : Name.Name
nilName =
    "[]"



-- ERROR


{-| A nitpick error produced when pattern matching is incomplete or redundant.
-}
type Error
    = Incomplete A.Region Context (List Pattern)
    | Redundant A.Region A.Region Int


{-| Describes the location and shape of a pattern match error.
-}
type Context
    = BadArg
    | BadDestruct
    | BadCase Target



-- CHECK


{-| Check a canonical module's patterns for exhaustiveness and redundancy.
-}
check : Target -> Can.Module -> Result (NE.Nonempty Error) ()
check target (Can.Module _ _ _ decls _ _ _ _) =
    case checkDecls target decls [] identity of
        [] ->
            Ok ()

        e :: es ->
            Err (NE.Nonempty e es)



-- CHECK DECLS


{-| Recursively check declarations for pattern match errors.
-}
checkDecls : Target -> Can.Decls -> List Error -> (List Error -> List Error) -> List Error
checkDecls target decls errors cont =
    case decls of
        Can.Declare def subDecls ->
            checkDecls target subDecls errors (checkDef target def >> cont)

        Can.DeclareRec def defs subDecls ->
            List.foldr (checkDef target) (checkDecls target subDecls errors (checkDef target def >> cont)) defs

        Can.SaveTheEnvironment ->
            cont errors



-- CHECK DEFS


{-| Check a single function or value definition for pattern match errors.
-}
checkDef : Target -> Can.Def -> List Error -> List Error
checkDef target def errors =
    case def of
        Can.Def _ args body ->
            List.foldr (checkArg target) (checkExpr target body errors) args

        Can.TypedDef _ _ args body _ ->
            List.foldr (checkTypedArg target) (checkExpr target body errors) args


{-| Check a function argument pattern for pattern match errors.
-}
checkArg : Target -> Can.Pattern -> List Error -> List Error
checkArg target ((A.At region _) as pattern) errors =
    checkPatterns target region BadArg [ pattern ] errors


{-| Check a typed function argument pattern for pattern match errors.
-}
checkTypedArg : Target -> ( Can.Pattern, tipe ) -> List Error -> List Error
checkTypedArg target ( (A.At region _) as pattern, _ ) errors =
    checkPatterns target region BadArg [ pattern ] errors



-- CHECK EXPRESSIONS


{-| Check an expression for pattern match errors, including nested branches and
pattern bindings.
-}
checkExpr : Target -> Can.Expr -> List Error -> List Error
checkExpr target (A.At region expression) errors =
    case expression of
        Can.VarLocal _ ->
            errors

        Can.VarTopLevel _ _ ->
            errors

        Can.VarKernel _ _ ->
            errors

        Can.VarForeign _ _ _ ->
            errors

        Can.VarCtor _ _ _ _ _ ->
            errors

        Can.VarDebug _ _ _ ->
            errors

        Can.VarOperator _ _ _ _ ->
            errors

        Can.Chr _ ->
            errors

        Can.Str _ ->
            errors

        Can.Int _ ->
            errors

        Can.Float _ ->
            errors

        Can.List entries ->
            List.foldr (checkExpr target) errors entries

        Can.Negate expr ->
            checkExpr target expr errors

        Can.Binop _ _ _ _ left right ->
            checkExpr target
                left
                (checkExpr target right errors)

        Can.Lambda args body ->
            List.foldr (checkArg target) (checkExpr target body errors) args

        Can.Call func args ->
            checkExpr target func (List.foldr (checkExpr target) errors args)

        Can.If branches finally ->
            List.foldr (checkIfBranch target) (checkExpr target finally errors) branches

        Can.Let def body ->
            checkDef target def (checkExpr target body errors)

        Can.LetRec defs body ->
            List.foldr (checkDef target) (checkExpr target body errors) defs

        Can.LetDestruct ((A.At reg _) as pattern) expr body ->
            checkPatterns target reg BadDestruct [ pattern ] <|
                checkExpr target expr (checkExpr target body errors)

        Can.Case expr branches ->
            checkExpr target expr (checkCases target region branches errors)

        Can.Accessor _ ->
            errors

        Can.Access record _ ->
            checkExpr target record errors

        Can.Update record fields ->
            checkExpr target record <| Dict.foldr A.compareLocated (\_ -> checkField target) errors fields

        Can.Record fields ->
            Dict.foldr A.compareLocated (\_ -> checkExpr target) errors fields

        Can.Unit ->
            errors

        Can.Tuple a b cs ->
            checkExpr target a <|
                checkExpr target b <|
                    List.foldr (checkExpr target) errors cs

        Can.Shader _ _ ->
            errors



-- CHECK FIELD


{-| Check a field update expression for pattern match errors.
-}
checkField : Target -> Can.FieldUpdate -> List Error -> List Error
checkField target (Can.FieldUpdate _ expr) errors =
    checkExpr target expr errors



-- CHECK IF BRANCH


{-| Check a single if branch for pattern match errors.
-}
checkIfBranch : Target -> ( Can.Expr, Can.Expr ) -> List Error -> List Error
checkIfBranch target ( condition, branch ) errs =
    checkExpr target condition (checkExpr target branch errs)



-- CHECK CASE EXPRESSION


{-| Check all case branches and collect their patterns to detect missing or
redundant alternatives.
-}
checkCases : Target -> A.Region -> List Can.CaseBranch -> List Error -> List Error
checkCases target region branches errors =
    let
        ( patterns, newErrors ) =
            List.foldr (checkCaseBranch target) ( [], errors ) branches
    in
    checkPatterns target region (BadCase target) patterns newErrors


{-| Inspect a case branch and add its pattern to the list of rows for
exhaustiveness and redundancy analysis.
-}
checkCaseBranch : Target -> Can.CaseBranch -> ( List Can.Pattern, List Error ) -> ( List Can.Pattern, List Error )
checkCaseBranch target (Can.CaseBranch pattern expr) ( patterns, errors ) =
    ( pattern :: patterns
    , checkExpr target expr errors
    )



-- CHECK PATTERNS


{-| Check a list of patterns for redundancy and exhaustiveness.
-}
checkPatterns : Target -> A.Region -> Context -> List Can.Pattern -> List Error -> List Error
checkPatterns target region context patterns errors =
    case toNonRedundantRows target region patterns of
        Err err ->
            err :: errors

        Ok matrix ->
            case isExhaustive matrix 1 of
                [] ->
                    errors

                badPatterns ->
                    Incomplete region context (List.map Prelude.head badPatterns) :: errors



-- EXHAUSTIVE PATTERNS


{-| Determine whether the current pattern matrix covers all possible cases.

INVARIANTS:

  - The initial rows "matrix" are all of length 1
  - The initial count of items per row "n" is also 1
  - The resulting rows are examples of missing patterns

-}
isExhaustive : List (List Pattern) -> Int -> List (List Pattern)
isExhaustive matrix n =
    case matrix of
        [] ->
            [ List.repeat n Anything ]

        _ ->
            if n == 0 then
                []

            else
                let
                    ctors : Dict String Name.Name Can.Union
                    ctors =
                        collectCtors matrix

                    numSeen : Int
                    numSeen =
                        Dict.size ctors
                in
                if numSeen == 0 then
                    List.map ((::) Anything)
                        (isExhaustive (List.filterMap specializeRowByAnything matrix) (n - 1))

                else
                    let
                        ((Can.Union _ altList numAlts _) as alts) =
                            Tuple.second (Utils.mapFindMin ctors)
                    in
                    if numSeen < numAlts then
                        List.filterMap (isMissing alts ctors) altList
                            |> List.map (::)
                            |> List.andMap (isExhaustive (List.filterMap specializeRowByAnything matrix) (n - 1))

                    else
                        let
                            isAltExhaustive : Can.Ctor -> List (List Pattern)
                            isAltExhaustive (Can.Ctor name _ arity _) =
                                List.map (recoverCtor alts name arity)
                                    (isExhaustive
                                        (List.filterMap (specializeRowByCtor name arity) matrix)
                                        (arity + n - 1)
                                    )
                        in
                        List.concatMap isAltExhaustive altList


{-| Create a missing constructor pattern when not all constructors are covered.
-}
isMissing : Can.Union -> Dict String Name.Name a -> Can.Ctor -> Maybe Pattern
isMissing union ctors (Can.Ctor name _ arity _) =
    if Dict.member identity name ctors then
        Nothing

    else
        Just (Ctor union name (List.repeat arity Anything))


{-| Recover a constructor row from its specialized arguments.
-}
recoverCtor : Can.Union -> Name.Name -> Int -> List Pattern -> List Pattern
recoverCtor union name arity patterns =
    let
        ( args, rest ) =
            List.splitAt arity patterns
    in
    Ctor union name args :: rest



-- REDUNDANT PATTERNS


{-| Convert source patterns into a simplified row matrix while removing
redundant rows.

INVARIANT: Produces a list of rows where (forall row. length row == 1)

-}
toNonRedundantRows : Target -> A.Region -> List Can.Pattern -> Result Error (List (List Pattern))
toNonRedundantRows target region patterns =
    toSimplifiedUsefulRows target region [] patterns


{-| Build the simplified useful rows matrix and detect redundant cases.

INVARIANT: Produces a list of rows where (forall row. length row == 1)

-}
toSimplifiedUsefulRows : Target -> A.Region -> List (List Pattern) -> List Can.Pattern -> Result Error (List (List Pattern))
toSimplifiedUsefulRows target overallRegion checkedRows uncheckedPatterns =
    case uncheckedPatterns of
        [] ->
            Ok checkedRows

        ((A.At region _) as pattern) :: rest ->
            let
                nextRow : List Pattern
                nextRow =
                    [ simplify target pattern ]
            in
            if isUseful checkedRows nextRow then
                toSimplifiedUsefulRows target overallRegion (nextRow :: checkedRows) rest

            else
                Err (Redundant overallRegion region (List.length checkedRows + 1))


{-| Check if a new row "vector" is useful given previous rows "matrix"
-}
isUseful : List (List Pattern) -> List Pattern -> Bool
isUseful matrix vector =
    case matrix of
        [] ->
            -- No rows are the same as the new vector! The vector is useful!
            True

        _ ->
            case vector of
                [] ->
                    -- There is nothing left in the new vector, but we still have
                    -- rows that match the same things. This is not a useful vector!
                    False

                firstPattern :: patterns ->
                    case firstPattern of
                        Ctor _ name args ->
                            -- keep checking rows that start with this Ctor or Anything
                            isUseful
                                (List.filterMap (specializeRowByCtor name (List.length args)) matrix)
                                (args ++ patterns)

                        Anything ->
                            -- check if all alts appear in matrix
                            case isComplete matrix of
                                No ->
                                    -- This Anything is useful because some Ctors are missing.
                                    -- But what if a previous row has an Anything?
                                    -- If so, this one is not useful.
                                    isUseful (List.filterMap specializeRowByAnything matrix) patterns

                                Yes alts ->
                                    -- All Ctors are covered, so this Anything is not needed for any
                                    -- of those. But what if some of those Ctors have subpatterns
                                    -- that make them less general? If so, this actually is useful!
                                    let
                                        isUsefulAlt : Can.Ctor -> Bool
                                        isUsefulAlt (Can.Ctor name _ arity _) =
                                            isUseful
                                                (List.filterMap (specializeRowByCtor name arity) matrix)
                                                (List.repeat arity Anything ++ patterns)
                                    in
                                    List.any isUsefulAlt alts

                        Literal literal ->
                            -- keep checking rows that start with this Literal or Anything
                            isUseful
                                (List.filterMap (specializeRowByLiteral literal) matrix)
                                patterns


{-| Specialize a row by a constructor, expanding its arguments for further
exhaustiveness checking.

INVARIANT: (length row == N) ==> (length result == arity + N - 1)

-}
specializeRowByCtor : Name.Name -> Int -> List Pattern -> Maybe (List Pattern)
specializeRowByCtor ctorName arity row =
    case row of
        (Ctor _ name args) :: patterns ->
            if name == ctorName then
                Just (args ++ patterns)

            else
                Nothing

        Anything :: patterns ->
            Just (List.repeat arity Anything ++ patterns)

        (Literal _) :: _ ->
            crash <|
                "Compiler bug! After type checking, constructors and literals should never align in pattern match exhaustiveness checks."

        [] ->
            crash "Compiler error! Empty matrices should not get specialized."


{-| Specialize a row by a literal, narrowing matching rows for exhaustiveness.

INVARIANT: (length row == N) ==> (length result == N-1)

-}
specializeRowByLiteral : Literal -> List Pattern -> Maybe (List Pattern)
specializeRowByLiteral literal row =
    case row of
        (Literal lit) :: patterns ->
            if lit == literal then
                Just patterns

            else
                Nothing

        Anything :: patterns ->
            Just patterns

        (Ctor _ _ _) :: _ ->
            crash <|
                "Compiler bug! After type checking, constructors and literals should never align in pattern match exhaustiveness checks."

        [] ->
            crash "Compiler error! Empty matrices should not get specialized."


{-| Specialize a row when the first pattern is a wildcard, preserving the
remaining patterns.

INVARIANT: (length row == N) ==> (length result == N-1)

-}
specializeRowByAnything : List Pattern -> Maybe (List Pattern)
specializeRowByAnything row =
    case row of
        [] ->
            Nothing

        (Ctor _ _ _) :: _ ->
            Nothing

        Anything :: patterns ->
            Just patterns

        (Literal _) :: _ ->
            Nothing



-- ALL CONSTRUCTORS ARE PRESENT?


{-| Indicates whether a pattern matrix covers all constructors for a type.
-}
type Complete
    = Yes (List Can.Ctor)
    | No


{-| Determine whether all constructors are present for the current pattern
matrix.
-}
isComplete : List (List Pattern) -> Complete
isComplete matrix =
    let
        ctors : Dict String Name.Name Can.Union
        ctors =
            collectCtors matrix

        numSeen : Int
        numSeen =
            Dict.size ctors
    in
    if numSeen == 0 then
        No

    else
        let
            (Can.Union _ alts numAlts _) =
                Tuple.second (Utils.mapFindMin ctors)
        in
        if numSeen == numAlts then
            Yes alts

        else
            No



-- COLLECT CTORS


{-| Collect the first constructor from each row of the pattern matrix.
-}
collectCtors : List (List Pattern) -> Dict String Name.Name Can.Union
collectCtors matrix =
    List.foldl (\row acc -> collectCtorsHelp acc row) Dict.empty matrix


{-| Helper for collecting constructors from a single row.
-}
collectCtorsHelp : Dict String Name.Name Can.Union -> List Pattern -> Dict String Name.Name Can.Union
collectCtorsHelp ctors row =
    case row of
        (Ctor union name _) :: _ ->
            Dict.insert identity name union ctors

        _ ->
            ctors



-- ENCODERS and DECODERS


errorEncoder : Error -> BE.Encoder
errorEncoder error =
    case error of
        Incomplete region context unhandled ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                , contextEncoder context
                , BE.list patternEncoder unhandled
                ]

        Redundant caseRegion patternRegion index ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder caseRegion
                , A.regionEncoder patternRegion
                , BE.int index
                ]


errorDecoder : BD.Decoder Error
errorDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map3 Incomplete
                            A.regionDecoder
                            contextDecoder
                            (BD.list patternDecoder)

                    1 ->
                        BD.map3 Redundant
                            A.regionDecoder
                            A.regionDecoder
                            BD.int

                    _ ->
                        BD.fail
            )


contextEncoder : Context -> BE.Encoder
contextEncoder context =
    case context of
        BadArg ->
            BE.unsignedInt8 0

        BadDestruct ->
            BE.unsignedInt8 1

        BadCase target ->
            BE.sequence
                [ BE.unsignedInt8 2
                , Target.encoder target
                ]


contextDecoder : BD.Decoder Context
contextDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\str ->
                case str of
                    0 ->
                        BD.succeed BadArg

                    1 ->
                        BD.succeed BadDestruct

                    2 ->
                        BD.map BadCase Target.decoder

                    _ ->
                        BD.fail
            )


patternEncoder : Pattern -> BE.Encoder
patternEncoder pattern =
    case pattern of
        Anything ->
            BE.unsignedInt8 0

        Literal index ->
            BE.sequence
                [ BE.unsignedInt8 1
                , literalEncoder index
                ]

        Ctor union name args ->
            BE.sequence
                [ BE.unsignedInt8 2
                , Can.unionEncoder union
                , BE.string name
                , BE.list patternEncoder args
                ]


patternDecoder : BD.Decoder Pattern
patternDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Anything

                    1 ->
                        BD.map Literal literalDecoder

                    2 ->
                        BD.map3 Ctor
                            Can.unionDecoder
                            BD.string
                            (BD.list patternDecoder)

                    _ ->
                        BD.fail
            )


literalEncoder : Literal -> BE.Encoder
literalEncoder literal =
    case literal of
        Chr value ->
            BE.sequence
                [ BE.unsignedInt8 0
                , BE.string value
                ]

        Str value ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string value
                ]

        Int value ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.int value
                ]


literalDecoder : BD.Decoder Literal
literalDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Chr BD.string

                    1 ->
                        BD.map Str BD.string

                    2 ->
                        BD.map Int BD.int

                    _ ->
                        BD.fail
            )
