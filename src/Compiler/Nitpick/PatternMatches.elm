module Compiler.Nitpick.PatternMatches exposing
    ( Context(..)
    , Error(..)
    , Literal(..)
    , Pattern(..)
    , check
    , errorDecoder
    , errorEncoder
    )

{- The algorithm used here comes from "Warnings for Pattern Matching"
   by Luc Maranget. Check it out for more information!

   http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

-}

import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import List.Extra as List
import Prelude
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- PATTERN


type Pattern
    = Anything
    | Literal Literal
    | Ctor Can.Union Name.Name (List Pattern)


type Literal
    = Chr String
    | Str String
    | Int Int



-- CREATE SIMPLIFIED PATTERNS


simplify : Can.Pattern -> Pattern
simplify (A.At _ pattern) =
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
            Ctor pair pairName [ simplify a, simplify b ]

        Can.PTuple a b [ c ] ->
            Ctor triple tripleName [ simplify a, simplify b, simplify c ]

        Can.PTuple a b cs ->
            Ctor nTuple nTupleName (List.map simplify (a :: b :: cs))

        Can.PCtor { union, name, args } ->
            Ctor union name <|
                List.map (\(Can.PatternCtorArg _ _ arg) -> simplify arg) args

        Can.PList entries ->
            List.foldr cons nil entries

        Can.PCons hd tl ->
            cons hd (simplify tl)

        Can.PAlias subPattern _ ->
            simplify subPattern

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


cons : Can.Pattern -> Pattern -> Pattern
cons hd tl =
    Ctor list consName [ simplify hd, tl ]


nil : Pattern
nil =
    Ctor list nilName []



-- BUILT-IN UNIONS


unit : Can.Union
unit =
    let
        ctor : Can.Ctor
        ctor =
            Can.Ctor unitName Index.first 0 []
    in
    Can.Union [] [ ctor ] 1 Can.Normal


pair : Can.Union
pair =
    let
        ctor : Can.Ctor
        ctor =
            Can.Ctor pairName Index.first 2 [ Can.TVar "a", Can.TVar "b" ]
    in
    Can.Union [ "a", "b" ] [ ctor ] 1 Can.Normal


triple : Can.Union
triple =
    let
        ctor : Can.Ctor
        ctor =
            Can.Ctor tripleName Index.first 3 [ Can.TVar "a", Can.TVar "b", Can.TVar "c" ]
    in
    Can.Union [ "a", "b", "c" ] [ ctor ] 1 Can.Normal


nTuple : Can.Union
nTuple =
    let
        ctor : Can.Ctor
        ctor =
            Can.Ctor nTupleName Index.first 3 [ Can.TVar "a", Can.TVar "b", Can.TVar "cs" ]
    in
    Can.Union [ "a", "b", "cs" ] [ ctor ] 1 Can.Normal


list : Can.Union
list =
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
                , Can.TType ModuleName.list Name.list [ Can.TVar "a" ]
                ]
    in
    Can.Union [ "a" ] [ nilCtor, consCtor ] 2 Can.Normal


unitName : Name.Name
unitName =
    "#0"


pairName : Name.Name
pairName =
    "#2"


tripleName : Name.Name
tripleName =
    "#3"


nTupleName : Name.Name
nTupleName =
    "#N"


consName : Name.Name
consName =
    "::"


nilName : Name.Name
nilName =
    "[]"



-- ERROR


type Error
    = Incomplete A.Region Context (List Pattern)
    | Redundant A.Region A.Region Int


type Context
    = BadArg
    | BadDestruct
    | BadCase



-- CHECK


check : Can.Module -> Result (NE.Nonempty Error) ()
check (Can.Module _ _ _ decls _ _ _ _) =
    case checkDecls decls [] identity of
        [] ->
            Ok ()

        e :: es ->
            Err (NE.Nonempty e es)



-- CHECK DECLS


checkDecls : Can.Decls -> List Error -> (List Error -> List Error) -> List Error
checkDecls decls errors cont =
    case decls of
        Can.Declare def subDecls ->
            checkDecls subDecls errors (checkDef def >> cont)

        Can.DeclareRec def defs subDecls ->
            List.foldr checkDef (checkDecls subDecls errors (checkDef def >> cont)) defs

        Can.SaveTheEnvironment ->
            cont errors



-- CHECK DEFS


checkDef : Can.Def -> List Error -> List Error
checkDef def errors =
    case def of
        Can.Def _ args body ->
            List.foldr checkArg (checkExpr body errors) args

        Can.TypedDef _ _ args body _ ->
            List.foldr checkTypedArg (checkExpr body errors) args


checkArg : Can.Pattern -> List Error -> List Error
checkArg ((A.At region _) as pattern) errors =
    checkPatterns region BadArg [ pattern ] errors


checkTypedArg : ( Can.Pattern, tipe ) -> List Error -> List Error
checkTypedArg ( (A.At region _) as pattern, _ ) errors =
    checkPatterns region BadArg [ pattern ] errors



-- CHECK EXPRESSIONS


checkExpr : Can.Expr -> List Error -> List Error
checkExpr (A.At region expression) errors =
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
            List.foldr checkExpr errors entries

        Can.Negate expr ->
            checkExpr expr errors

        Can.Binop _ _ _ _ left right ->
            checkExpr left
                (checkExpr right errors)

        Can.Lambda args body ->
            List.foldr checkArg (checkExpr body errors) args

        Can.Call func args ->
            checkExpr func (List.foldr checkExpr errors args)

        Can.If branches finally ->
            List.foldr checkIfBranch (checkExpr finally errors) branches

        Can.Let def body ->
            checkDef def (checkExpr body errors)

        Can.LetRec defs body ->
            List.foldr checkDef (checkExpr body errors) defs

        Can.LetDestruct ((A.At reg _) as pattern) expr body ->
            checkPatterns reg BadDestruct [ pattern ] <|
                checkExpr expr (checkExpr body errors)

        Can.Case expr branches ->
            checkExpr expr (checkCases region branches errors)

        Can.Accessor _ ->
            errors

        Can.Access record _ ->
            checkExpr record errors

        Can.Update record fields ->
            checkExpr record <| Dict.foldr A.compareLocated (\_ -> checkField) errors fields

        Can.Record fields ->
            Dict.foldr A.compareLocated (\_ -> checkExpr) errors fields

        Can.Unit ->
            errors

        Can.Tuple a b cs ->
            checkExpr a
                (checkExpr b
                    (List.foldr checkExpr errors cs)
                )

        Can.Shader _ _ ->
            errors



-- CHECK FIELD


checkField : Can.FieldUpdate -> List Error -> List Error
checkField (Can.FieldUpdate _ expr) errors =
    checkExpr expr errors



-- CHECK IF BRANCH


checkIfBranch : ( Can.Expr, Can.Expr ) -> List Error -> List Error
checkIfBranch ( condition, branch ) errs =
    checkExpr condition (checkExpr branch errs)



-- CHECK CASE EXPRESSION


checkCases : A.Region -> List Can.CaseBranch -> List Error -> List Error
checkCases region branches errors =
    let
        ( patterns, newErrors ) =
            List.foldr checkCaseBranch ( [], errors ) branches
    in
    checkPatterns region BadCase patterns newErrors


checkCaseBranch : Can.CaseBranch -> ( List Can.Pattern, List Error ) -> ( List Can.Pattern, List Error )
checkCaseBranch (Can.CaseBranch pattern expr) ( patterns, errors ) =
    ( pattern :: patterns
    , checkExpr expr errors
    )



-- CHECK PATTERNS


checkPatterns : A.Region -> Context -> List Can.Pattern -> List Error -> List Error
checkPatterns region context patterns errors =
    case toNonRedundantRows region patterns of
        Err err ->
            err :: errors

        Ok matrix ->
            case isExhaustive matrix 1 of
                [] ->
                    errors

                badPatterns ->
                    Incomplete region context (List.map Prelude.head badPatterns) :: errors



-- EXHAUSTIVE PATTERNS
-- INVARIANTS:
--
--   The initial rows "matrix" are all of length 1
--   The initial count of items per row "n" is also 1
--   The resulting rows are examples of missing patterns
--


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


isMissing : Can.Union -> Dict String Name.Name a -> Can.Ctor -> Maybe Pattern
isMissing union ctors (Can.Ctor name _ arity _) =
    if Dict.member identity name ctors then
        Nothing

    else
        Just (Ctor union name (List.repeat arity Anything))


recoverCtor : Can.Union -> Name.Name -> Int -> List Pattern -> List Pattern
recoverCtor union name arity patterns =
    let
        ( args, rest ) =
            List.splitAt arity patterns
    in
    Ctor union name args :: rest



-- REDUNDANT PATTERNS


{-| INVARIANT: Produces a list of rows where (forall row. length row == 1)
-}
toNonRedundantRows : A.Region -> List Can.Pattern -> Result Error (List (List Pattern))
toNonRedundantRows region patterns =
    toSimplifiedUsefulRows region [] patterns


{-| INVARIANT: Produces a list of rows where (forall row. length row == 1)
-}
toSimplifiedUsefulRows : A.Region -> List (List Pattern) -> List Can.Pattern -> Result Error (List (List Pattern))
toSimplifiedUsefulRows overallRegion checkedRows uncheckedPatterns =
    case uncheckedPatterns of
        [] ->
            Ok checkedRows

        ((A.At region _) as pattern) :: rest ->
            let
                nextRow : List Pattern
                nextRow =
                    [ simplify pattern ]
            in
            if isUseful checkedRows nextRow then
                toSimplifiedUsefulRows overallRegion (nextRow :: checkedRows) rest

            else
                Err (Redundant overallRegion region (List.length checkedRows + 1))



-- Check if a new row "vector" is useful given previous rows "matrix"


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



-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)


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



-- INVARIANT: (length row == N) ==> (length result == N-1)


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



-- INVARIANT: (length row == N) ==> (length result == N-1)


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


type Complete
    = Yes (List Can.Ctor)
    | No


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


collectCtors : List (List Pattern) -> Dict String Name.Name Can.Union
collectCtors matrix =
    List.foldl (\row acc -> collectCtorsHelp acc row) Dict.empty matrix


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
    BE.unsignedInt8
        (case context of
            BadArg ->
                0

            BadDestruct ->
                1

            BadCase ->
                2
        )


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
                        BD.succeed BadCase

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
