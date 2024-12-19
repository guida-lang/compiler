module Compiler.Nitpick.PatternMatches exposing
    ( check
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
import Types as T
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- CREATE SIMPLIFIED PATTERNS


simplify : T.CASTC_Pattern -> T.CNPM_Pattern
simplify (T.CRA_At _ pattern) =
    case pattern of
        T.CASTC_PAnything ->
            T.CNPM_Anything

        T.CASTC_PVar _ ->
            T.CNPM_Anything

        T.CASTC_PRecord _ ->
            T.CNPM_Anything

        T.CASTC_PUnit ->
            T.CNPM_Ctor unit unitName []

        T.CASTC_PTuple a b Nothing ->
            T.CNPM_Ctor pair pairName [ simplify a, simplify b ]

        T.CASTC_PTuple a b (Just c) ->
            T.CNPM_Ctor triple tripleName [ simplify a, simplify b, simplify c ]

        T.CASTC_PCtor { union, name, args } ->
            T.CNPM_Ctor union name <|
                List.map (\(T.CASTC_PatternCtorArg _ _ arg) -> simplify arg) args

        T.CASTC_PList entries ->
            List.foldr cons nil entries

        T.CASTC_PCons hd tl ->
            cons hd (simplify tl)

        T.CASTC_PAlias subPattern _ ->
            simplify subPattern

        T.CASTC_PInt int ->
            T.CNPM_Literal (T.CNPM_Int int)

        T.CASTC_PStr str ->
            T.CNPM_Literal (T.CNPM_Str str)

        T.CASTC_PChr chr ->
            T.CNPM_Literal (T.CNPM_Chr chr)

        T.CASTC_PBool union bool ->
            T.CNPM_Ctor union
                (if bool then
                    Name.true

                 else
                    Name.false
                )
                []


cons : T.CASTC_Pattern -> T.CNPM_Pattern -> T.CNPM_Pattern
cons hd tl =
    T.CNPM_Ctor list consName [ simplify hd, tl ]


nil : T.CNPM_Pattern
nil =
    T.CNPM_Ctor list nilName []



-- BUILT-IN UNIONS


unit : T.CASTC_Union
unit =
    let
        ctor : T.CASTC_Ctor
        ctor =
            T.CASTC_Ctor unitName Index.first 0 []
    in
    T.CASTC_Union [] [ ctor ] 1 T.CASTC_Normal


pair : T.CASTC_Union
pair =
    let
        ctor : T.CASTC_Ctor
        ctor =
            T.CASTC_Ctor pairName Index.first 2 [ T.CASTC_TVar "a", T.CASTC_TVar "b" ]
    in
    T.CASTC_Union [ "a", "b" ] [ ctor ] 1 T.CASTC_Normal


triple : T.CASTC_Union
triple =
    let
        ctor : T.CASTC_Ctor
        ctor =
            T.CASTC_Ctor tripleName Index.first 3 [ T.CASTC_TVar "a", T.CASTC_TVar "b", T.CASTC_TVar "c" ]
    in
    T.CASTC_Union [ "a", "b", "c" ] [ ctor ] 1 T.CASTC_Normal


list : T.CASTC_Union
list =
    let
        nilCtor : T.CASTC_Ctor
        nilCtor =
            T.CASTC_Ctor nilName Index.first 0 []

        consCtor : T.CASTC_Ctor
        consCtor =
            T.CASTC_Ctor consName
                Index.second
                2
                [ T.CASTC_TVar "a"
                , T.CASTC_TType ModuleName.list Name.list [ T.CASTC_TVar "a" ]
                ]
    in
    T.CASTC_Union [ "a" ] [ nilCtor, consCtor ] 2 T.CASTC_Normal


unitName : T.CDN_Name
unitName =
    "#0"


pairName : T.CDN_Name
pairName =
    "#2"


tripleName : T.CDN_Name
tripleName =
    "#3"


consName : T.CDN_Name
consName =
    "::"


nilName : T.CDN_Name
nilName =
    "[]"



-- CHECK


check : Can.Module -> Result (NE.Nonempty T.CNPM_Error) ()
check (Can.Module _ _ _ decls _ _ _ _) =
    case checkDecls decls [] of
        [] ->
            Ok ()

        e :: es ->
            Err (NE.Nonempty e es)



-- CHECK DECLS


checkDecls : T.CASTC_Decls -> List T.CNPM_Error -> List T.CNPM_Error
checkDecls decls errors =
    case decls of
        T.CASTC_Declare def subDecls ->
            checkDef def (checkDecls subDecls errors)

        T.CASTC_DeclareRec def defs subDecls ->
            checkDef def (List.foldr checkDef (checkDecls subDecls errors) defs)

        T.CASTC_SaveTheEnvironment ->
            errors



-- CHECK DEFS


checkDef : T.CASTC_Def -> List T.CNPM_Error -> List T.CNPM_Error
checkDef def errors =
    case def of
        T.CASTC_Def _ args body ->
            List.foldr checkArg (checkExpr body errors) args

        T.CASTC_TypedDef _ _ args body _ ->
            List.foldr checkTypedArg (checkExpr body errors) args


checkArg : T.CASTC_Pattern -> List T.CNPM_Error -> List T.CNPM_Error
checkArg ((T.CRA_At region _) as pattern) errors =
    checkPatterns region T.CNPM_BadArg [ pattern ] errors


checkTypedArg : ( T.CASTC_Pattern, tipe ) -> List T.CNPM_Error -> List T.CNPM_Error
checkTypedArg ( (T.CRA_At region _) as pattern, _ ) errors =
    checkPatterns region T.CNPM_BadArg [ pattern ] errors



-- CHECK EXPRESSIONS


checkExpr : T.CASTC_Expr -> List T.CNPM_Error -> List T.CNPM_Error
checkExpr (T.CRA_At region expression) errors =
    case expression of
        T.CASTC_VarLocal _ ->
            errors

        T.CASTC_VarTopLevel _ _ ->
            errors

        T.CASTC_VarKernel _ _ ->
            errors

        T.CASTC_VarForeign _ _ _ ->
            errors

        T.CASTC_VarCtor _ _ _ _ _ ->
            errors

        T.CASTC_VarDebug _ _ _ ->
            errors

        T.CASTC_VarOperator _ _ _ _ ->
            errors

        T.CASTC_Chr _ ->
            errors

        T.CASTC_Str _ ->
            errors

        T.CASTC_Int _ ->
            errors

        T.CASTC_Float _ ->
            errors

        T.CASTC_List entries ->
            List.foldr checkExpr errors entries

        T.CASTC_Negate expr ->
            checkExpr expr errors

        T.CASTC_Binop _ _ _ _ left right ->
            checkExpr left
                (checkExpr right errors)

        T.CASTC_Lambda args body ->
            List.foldr checkArg (checkExpr body errors) args

        T.CASTC_Call func args ->
            checkExpr func (List.foldr checkExpr errors args)

        T.CASTC_If branches finally ->
            List.foldr checkIfBranch (checkExpr finally errors) branches

        T.CASTC_Let def body ->
            checkDef def (checkExpr body errors)

        T.CASTC_LetRec defs body ->
            List.foldr checkDef (checkExpr body errors) defs

        T.CASTC_LetDestruct ((T.CRA_At reg _) as pattern) expr body ->
            checkPatterns reg T.CNPM_BadDestruct [ pattern ] <|
                checkExpr expr (checkExpr body errors)

        T.CASTC_Case expr branches ->
            checkExpr expr (checkCases region branches errors)

        T.CASTC_Accessor _ ->
            errors

        T.CASTC_Access record _ ->
            checkExpr record errors

        T.CASTC_Update _ record fields ->
            checkExpr record <| Dict.foldr compare (\_ -> checkField) errors fields

        T.CASTC_Record fields ->
            Dict.foldr compare (\_ -> checkExpr) errors fields

        T.CASTC_Unit ->
            errors

        T.CASTC_Tuple a b maybeC ->
            checkExpr a
                (checkExpr b
                    (case maybeC of
                        Nothing ->
                            errors

                        Just c ->
                            checkExpr c errors
                    )
                )

        T.CASTC_Shader _ _ ->
            errors



-- CHECK FIELD


checkField : T.CASTC_FieldUpdate -> List T.CNPM_Error -> List T.CNPM_Error
checkField (T.CASTC_FieldUpdate _ expr) errors =
    checkExpr expr errors



-- CHECK IF BRANCH


checkIfBranch : ( T.CASTC_Expr, T.CASTC_Expr ) -> List T.CNPM_Error -> List T.CNPM_Error
checkIfBranch ( condition, branch ) errs =
    checkExpr condition (checkExpr branch errs)



-- CHECK CASE EXPRESSION


checkCases : T.CRA_Region -> List T.CASTC_CaseBranch -> List T.CNPM_Error -> List T.CNPM_Error
checkCases region branches errors =
    let
        ( patterns, newErrors ) =
            List.foldr checkCaseBranch ( [], errors ) branches
    in
    checkPatterns region T.CNPM_BadCase patterns newErrors


checkCaseBranch : T.CASTC_CaseBranch -> ( List T.CASTC_Pattern, List T.CNPM_Error ) -> ( List T.CASTC_Pattern, List T.CNPM_Error )
checkCaseBranch (T.CASTC_CaseBranch pattern expr) ( patterns, errors ) =
    ( pattern :: patterns
    , checkExpr expr errors
    )



-- CHECK PATTERNS


checkPatterns : T.CRA_Region -> T.CNPM_Context -> List T.CASTC_Pattern -> List T.CNPM_Error -> List T.CNPM_Error
checkPatterns region context patterns errors =
    case toNonRedundantRows region patterns of
        Err err ->
            err :: errors

        Ok matrix ->
            case isExhaustive matrix 1 of
                [] ->
                    errors

                badPatterns ->
                    T.CNPM_Incomplete region context (List.map Prelude.head badPatterns) :: errors



-- EXHAUSTIVE PATTERNS
-- INVARIANTS:
--
--   The initial rows "matrix" are all of length 1
--   The initial count of items per row "n" is also 1
--   The resulting rows are examples of missing patterns
--


isExhaustive : List (List T.CNPM_Pattern) -> Int -> List (List T.CNPM_Pattern)
isExhaustive matrix n =
    case matrix of
        [] ->
            [ List.repeat n T.CNPM_Anything ]

        _ ->
            if n == 0 then
                []

            else
                let
                    ctors : Dict String T.CDN_Name T.CASTC_Union
                    ctors =
                        collectCtors matrix

                    numSeen : Int
                    numSeen =
                        Dict.size ctors
                in
                if numSeen == 0 then
                    List.map ((::) T.CNPM_Anything)
                        (isExhaustive (List.filterMap specializeRowByAnything matrix) (n - 1))

                else
                    let
                        ((T.CASTC_Union _ altList numAlts _) as alts) =
                            Tuple.second (Utils.mapFindMin ctors)
                    in
                    if numSeen < numAlts then
                        List.filterMap (isMissing alts ctors) altList
                            |> List.map (::)
                            |> List.andMap (isExhaustive (List.filterMap specializeRowByAnything matrix) (n - 1))

                    else
                        let
                            isAltExhaustive : T.CASTC_Ctor -> List (List T.CNPM_Pattern)
                            isAltExhaustive (T.CASTC_Ctor name _ arity _) =
                                List.map (recoverCtor alts name arity)
                                    (isExhaustive
                                        (List.filterMap (specializeRowByCtor name arity) matrix)
                                        (arity + n - 1)
                                    )
                        in
                        List.concatMap isAltExhaustive altList


isMissing : T.CASTC_Union -> Dict String T.CDN_Name a -> T.CASTC_Ctor -> Maybe T.CNPM_Pattern
isMissing union ctors (T.CASTC_Ctor name _ arity _) =
    if Dict.member identity name ctors then
        Nothing

    else
        Just (T.CNPM_Ctor union name (List.repeat arity T.CNPM_Anything))


recoverCtor : T.CASTC_Union -> T.CDN_Name -> Int -> List T.CNPM_Pattern -> List T.CNPM_Pattern
recoverCtor union name arity patterns =
    let
        ( args, rest ) =
            List.splitAt arity patterns
    in
    T.CNPM_Ctor union name args :: rest



-- REDUNDANT PATTERNS


{-| INVARIANT: Produces a list of rows where (forall row. length row == 1)
-}
toNonRedundantRows : T.CRA_Region -> List T.CASTC_Pattern -> Result T.CNPM_Error (List (List T.CNPM_Pattern))
toNonRedundantRows region patterns =
    toSimplifiedUsefulRows region [] patterns


{-| INVARIANT: Produces a list of rows where (forall row. length row == 1)
-}
toSimplifiedUsefulRows : T.CRA_Region -> List (List T.CNPM_Pattern) -> List T.CASTC_Pattern -> Result T.CNPM_Error (List (List T.CNPM_Pattern))
toSimplifiedUsefulRows overallRegion checkedRows uncheckedPatterns =
    case uncheckedPatterns of
        [] ->
            Ok checkedRows

        ((T.CRA_At region _) as pattern) :: rest ->
            let
                nextRow : List T.CNPM_Pattern
                nextRow =
                    [ simplify pattern ]
            in
            if isUseful checkedRows nextRow then
                toSimplifiedUsefulRows overallRegion (nextRow :: checkedRows) rest

            else
                Err (T.CNPM_Redundant overallRegion region (List.length checkedRows + 1))



-- Check if a new row "vector" is useful given previous rows "matrix"


isUseful : List (List T.CNPM_Pattern) -> List T.CNPM_Pattern -> Bool
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
                        T.CNPM_Ctor _ name args ->
                            -- keep checking rows that start with this Ctor or Anything
                            isUseful
                                (List.filterMap (specializeRowByCtor name (List.length args)) matrix)
                                (args ++ patterns)

                        T.CNPM_Anything ->
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
                                        isUsefulAlt : T.CASTC_Ctor -> Bool
                                        isUsefulAlt (T.CASTC_Ctor name _ arity _) =
                                            isUseful
                                                (List.filterMap (specializeRowByCtor name arity) matrix)
                                                (List.repeat arity T.CNPM_Anything ++ patterns)
                                    in
                                    List.any isUsefulAlt alts

                        T.CNPM_Literal literal ->
                            -- keep checking rows that start with this Literal or Anything
                            isUseful
                                (List.filterMap (specializeRowByLiteral literal) matrix)
                                patterns



-- INVARIANT: (length row == N) ==> (length result == arity + N - 1)


specializeRowByCtor : T.CDN_Name -> Int -> List T.CNPM_Pattern -> Maybe (List T.CNPM_Pattern)
specializeRowByCtor ctorName arity row =
    case row of
        (T.CNPM_Ctor _ name args) :: patterns ->
            if name == ctorName then
                Just (args ++ patterns)

            else
                Nothing

        T.CNPM_Anything :: patterns ->
            Just (List.repeat arity T.CNPM_Anything ++ patterns)

        (T.CNPM_Literal _) :: _ ->
            crash <|
                "Compiler bug! After type checking, constructors and literals should never align in pattern match exhaustiveness checks."

        [] ->
            crash "Compiler error! Empty matrices should not get specialized."



-- INVARIANT: (length row == N) ==> (length result == N-1)


specializeRowByLiteral : T.CNPM_Literal -> List T.CNPM_Pattern -> Maybe (List T.CNPM_Pattern)
specializeRowByLiteral literal row =
    case row of
        (T.CNPM_Literal lit) :: patterns ->
            if lit == literal then
                Just patterns

            else
                Nothing

        T.CNPM_Anything :: patterns ->
            Just patterns

        (T.CNPM_Ctor _ _ _) :: _ ->
            crash <|
                "Compiler bug! After type checking, constructors and literals should never align in pattern match exhaustiveness checks."

        [] ->
            crash "Compiler error! Empty matrices should not get specialized."



-- INVARIANT: (length row == N) ==> (length result == N-1)


specializeRowByAnything : List T.CNPM_Pattern -> Maybe (List T.CNPM_Pattern)
specializeRowByAnything row =
    case row of
        [] ->
            Nothing

        (T.CNPM_Ctor _ _ _) :: _ ->
            Nothing

        T.CNPM_Anything :: patterns ->
            Just patterns

        (T.CNPM_Literal _) :: _ ->
            Nothing



-- ALL CONSTRUCTORS ARE PRESENT?


type Complete
    = Yes (List T.CASTC_Ctor)
    | No


isComplete : List (List T.CNPM_Pattern) -> Complete
isComplete matrix =
    let
        ctors : Dict String T.CDN_Name T.CASTC_Union
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
            (T.CASTC_Union _ alts numAlts _) =
                Tuple.second (Utils.mapFindMin ctors)
        in
        if numSeen == numAlts then
            Yes alts

        else
            No



-- COLLECT CTORS


collectCtors : List (List T.CNPM_Pattern) -> Dict String T.CDN_Name T.CASTC_Union
collectCtors matrix =
    List.foldl (\row acc -> collectCtorsHelp acc row) Dict.empty matrix


collectCtorsHelp : Dict String T.CDN_Name T.CASTC_Union -> List T.CNPM_Pattern -> Dict String T.CDN_Name T.CASTC_Union
collectCtorsHelp ctors row =
    case row of
        (T.CNPM_Ctor union name _) :: _ ->
            Dict.insert identity name union ctors

        _ ->
            ctors



-- ENCODERS and DECODERS


errorEncoder : T.CNPM_Error -> Encode.Value
errorEncoder error =
    case error of
        T.CNPM_Incomplete region context unhandled ->
            Encode.object
                [ ( "type", Encode.string "Incomplete" )
                , ( "region", A.regionEncoder region )
                , ( "context", contextEncoder context )
                , ( "unhandled", Encode.list patternEncoder unhandled )
                ]

        T.CNPM_Redundant caseRegion patternRegion index ->
            Encode.object
                [ ( "type", Encode.string "Redundant" )
                , ( "caseRegion", A.regionEncoder caseRegion )
                , ( "patternRegion", A.regionEncoder patternRegion )
                , ( "index", Encode.int index )
                ]


errorDecoder : Decode.Decoder T.CNPM_Error
errorDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Incomplete" ->
                        Decode.map3 T.CNPM_Incomplete
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "context" contextDecoder)
                            (Decode.field "unhandled" (Decode.list patternDecoder))

                    "Redundant" ->
                        Decode.map3 T.CNPM_Redundant
                            (Decode.field "caseRegion" A.regionDecoder)
                            (Decode.field "patternRegion" A.regionDecoder)
                            (Decode.field "index" Decode.int)

                    _ ->
                        Decode.fail ("Unknown Error's type: " ++ type_)
            )


contextEncoder : T.CNPM_Context -> Encode.Value
contextEncoder context =
    case context of
        T.CNPM_BadArg ->
            Encode.string "BadArg"

        T.CNPM_BadDestruct ->
            Encode.string "BadDestruct"

        T.CNPM_BadCase ->
            Encode.string "BadCase"


contextDecoder : Decode.Decoder T.CNPM_Context
contextDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "BadArg" ->
                        Decode.succeed T.CNPM_BadArg

                    "BadDestruct" ->
                        Decode.succeed T.CNPM_BadDestruct

                    "BadCase" ->
                        Decode.succeed T.CNPM_BadCase

                    _ ->
                        Decode.fail ("Unknown Context: " ++ str)
            )


patternEncoder : T.CNPM_Pattern -> Encode.Value
patternEncoder pattern =
    case pattern of
        T.CNPM_Anything ->
            Encode.object
                [ ( "type", Encode.string "Anything" )
                ]

        T.CNPM_Literal index ->
            Encode.object
                [ ( "type", Encode.string "Literal" )
                , ( "index", literalEncoder index )
                ]

        T.CNPM_Ctor union name args ->
            Encode.object
                [ ( "type", Encode.string "Ctor" )
                , ( "union", Can.unionEncoder union )
                , ( "name", Encode.string name )
                , ( "args", Encode.list patternEncoder args )
                ]


patternDecoder : Decode.Decoder T.CNPM_Pattern
patternDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Anything" ->
                        Decode.succeed T.CNPM_Anything

                    "Literal" ->
                        Decode.map T.CNPM_Literal (Decode.field "index" literalDecoder)

                    "Ctor" ->
                        Decode.map3 T.CNPM_Ctor
                            (Decode.field "union" Can.unionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list patternDecoder))

                    _ ->
                        Decode.fail ("Unknown Pattern's type: " ++ type_)
            )


literalEncoder : T.CNPM_Literal -> Encode.Value
literalEncoder literal =
    case literal of
        T.CNPM_Chr value ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "value", Encode.string value )
                ]

        T.CNPM_Str value ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "value", Encode.string value )
                ]

        T.CNPM_Int value ->
            Encode.object
                [ ( "type", Encode.string "Int" )
                , ( "value", Encode.int value )
                ]


literalDecoder : Decode.Decoder T.CNPM_Literal
literalDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Chr" ->
                        Decode.map T.CNPM_Chr (Decode.field "value" Decode.string)

                    "Str" ->
                        Decode.map T.CNPM_Str (Decode.field "value" Decode.string)

                    "Int" ->
                        Decode.map T.CNPM_Int (Decode.field "value" Decode.int)

                    _ ->
                        Decode.fail ("Unknown Literal's type: " ++ type_)
            )
