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
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Prelude
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

        Can.PTuple a b Nothing ->
            Ctor pair pairName [ simplify a, simplify b ]

        Can.PTuple a b (Just c) ->
            Ctor triple tripleName [ simplify a, simplify b, simplify c ]

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

        Can.PStr str ->
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
    case checkDecls decls [] of
        [] ->
            Ok ()

        e :: es ->
            Err (NE.Nonempty e es)



-- CHECK DECLS


checkDecls : Can.Decls -> List Error -> List Error
checkDecls decls errors =
    case decls of
        Can.Declare def subDecls ->
            checkDef def (checkDecls subDecls errors)

        Can.DeclareRec def defs subDecls ->
            checkDef def (List.foldr checkDef (checkDecls subDecls errors) defs)

        Can.SaveTheEnvironment ->
            errors



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

        Can.Update _ record fields ->
            checkExpr record <| Dict.foldr compare (\_ -> checkField) errors fields

        Can.Record fields ->
            Dict.foldr compare (\_ -> checkExpr) errors fields

        Can.Unit ->
            errors

        Can.Tuple a b maybeC ->
            checkExpr a
                (checkExpr b
                    (case maybeC of
                        Nothing ->
                            errors

                        Just c ->
                            checkExpr c errors
                    )
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


errorEncoder : Error -> Encode.Value
errorEncoder error =
    case error of
        Incomplete region context unhandled ->
            Encode.object
                [ ( "type", Encode.string "Incomplete" )
                , ( "region", A.regionEncoder region )
                , ( "context", contextEncoder context )
                , ( "unhandled", Encode.list patternEncoder unhandled )
                ]

        Redundant caseRegion patternRegion index ->
            Encode.object
                [ ( "type", Encode.string "Redundant" )
                , ( "caseRegion", A.regionEncoder caseRegion )
                , ( "patternRegion", A.regionEncoder patternRegion )
                , ( "index", Encode.int index )
                ]


errorDecoder : Decode.Decoder Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Incomplete" ->
                        Decode.map3 Incomplete
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "context" contextDecoder)
                            (Decode.field "unhandled" (Decode.list patternDecoder))

                    "Redundant" ->
                        Decode.map3 Redundant
                            (Decode.field "caseRegion" A.regionDecoder)
                            (Decode.field "patternRegion" A.regionDecoder)
                            (Decode.field "index" Decode.int)

                    _ ->
                        Decode.fail ("Unknown Error's type: " ++ type_)
            )


contextEncoder : Context -> Encode.Value
contextEncoder context =
    case context of
        BadArg ->
            Encode.string "BadArg"

        BadDestruct ->
            Encode.string "BadDestruct"

        BadCase ->
            Encode.string "BadCase"


contextDecoder : Decode.Decoder Context
contextDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "BadArg" ->
                        Decode.succeed BadArg

                    "BadDestruct" ->
                        Decode.succeed BadDestruct

                    "BadCase" ->
                        Decode.succeed BadCase

                    _ ->
                        Decode.fail ("Unknown Context: " ++ str)
            )


patternEncoder : Pattern -> Encode.Value
patternEncoder pattern =
    case pattern of
        Anything ->
            Encode.object
                [ ( "type", Encode.string "Anything" )
                ]

        Literal index ->
            Encode.object
                [ ( "type", Encode.string "Literal" )
                , ( "index", literalEncoder index )
                ]

        Ctor union name args ->
            Encode.object
                [ ( "type", Encode.string "Ctor" )
                , ( "union", Can.unionEncoder union )
                , ( "name", Encode.string name )
                , ( "args", Encode.list patternEncoder args )
                ]


patternDecoder : Decode.Decoder Pattern
patternDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Anything" ->
                        Decode.succeed Anything

                    "Literal" ->
                        Decode.map Literal (Decode.field "index" literalDecoder)

                    "Ctor" ->
                        Decode.map3 Ctor
                            (Decode.field "union" Can.unionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list patternDecoder))

                    _ ->
                        Decode.fail ("Unknown Pattern's type: " ++ type_)
            )


literalEncoder : Literal -> Encode.Value
literalEncoder literal =
    case literal of
        Chr value ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "value", Encode.string value )
                ]

        Str value ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "value", Encode.string value )
                ]

        Int value ->
            Encode.object
                [ ( "type", Encode.string "Int" )
                , ( "value", Encode.int value )
                ]


literalDecoder : Decode.Decoder Literal
literalDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Chr" ->
                        Decode.map Chr (Decode.field "value" Decode.string)

                    "Str" ->
                        Decode.map Str (Decode.field "value" Decode.string)

                    "Int" ->
                        Decode.map Int (Decode.field "value" Decode.int)

                    _ ->
                        Decode.fail ("Unknown Literal's type: " ++ type_)
            )
