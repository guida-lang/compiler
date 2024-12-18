module Compiler.Optimize.DecisionTree exposing
    ( DecisionTree(..)
    , compile
    , pathDecoder
    , pathEncoder
    , testDecoder
    , testEncoder
    )

{- To learn more about how this works, definitely read through:

       "When Do Match-Compilation Heuristics Matter?"

   by Kevin Scott and Norman Ramsey. The rough idea is that we start with a simple
   list of patterns and expressions, and then turn that into a "decision tree"
   that requires as few tests as possible to make it to a leaf. Read the paper, it
   explains this extraordinarily well! We are currently using the same heuristics
   as SML/NJ to get nice trees.
-}

import Compiler.AST.Canonical as Can
import Compiler.Data.Index as Index
import Compiler.Elm.ModuleName as ModuleName
import Data.Set as EverySet
import Json.Decode as Decode
import Json.Encode as Encode
import Prelude
import Types as T
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- COMPILE CASES


{-| Users of this module will mainly interact with this function. It takes
some normal branches and gives out a decision tree that has "labels" at all
the leafs and a dictionary that maps these "labels" to the code that should
run.

If 2 or more leaves point to the same label, we need to do some tricks in JS to
make that work nicely. When is JS getting goto?! ;) That is outside the scope
of this module though.

-}
compile : List ( Can.Pattern, Int ) -> DecisionTree
compile rawBranches =
    let
        format : ( Can.Pattern, Int ) -> Branch
        format ( pattern, index ) =
            Branch index [ ( T.CODT_Empty, pattern ) ]
    in
    toDecisionTree (List.map format rawBranches)



-- DECISION TREES


type DecisionTree
    = Match Int
    | Decision T.CODT_Path (List ( T.CODT_Test, DecisionTree )) (Maybe DecisionTree)



-- ACTUALLY BUILD DECISION TREES


type Branch
    = Branch Int (List ( T.CODT_Path, Can.Pattern ))


toDecisionTree : List Branch -> DecisionTree
toDecisionTree rawBranches =
    let
        branches : List Branch
        branches =
            List.map flattenPatterns rawBranches
    in
    case checkForMatch branches of
        Just goal ->
            Match goal

        Nothing ->
            let
                path : T.CODT_Path
                path =
                    pickPath branches

                ( edges, fallback ) =
                    gatherEdges branches path

                decisionEdges : List ( T.CODT_Test, DecisionTree )
                decisionEdges =
                    List.map (Tuple.mapSecond toDecisionTree) edges
            in
            case ( decisionEdges, fallback ) of
                ( [ ( _, decisionTree ) ], [] ) ->
                    decisionTree

                ( _, [] ) ->
                    Decision path decisionEdges Nothing

                ( [], _ :: _ ) ->
                    toDecisionTree fallback

                _ ->
                    Decision path decisionEdges (Just (toDecisionTree fallback))


isComplete : List T.CODT_Test -> Bool
isComplete tests =
    case Prelude.head tests of
        T.CODT_IsCtor _ _ _ numAlts _ ->
            numAlts == List.length tests

        T.CODT_IsCons ->
            List.length tests == 2

        T.CODT_IsNil ->
            List.length tests == 2

        T.CODT_IsTuple ->
            True

        T.CODT_IsInt _ ->
            False

        T.CODT_IsChr _ ->
            False

        T.CODT_IsStr _ ->
            False

        T.CODT_IsBool _ ->
            List.length tests == 2



-- FLATTEN PATTERNS


{-| Flatten type aliases and use the VariantDict to figure out when a tag is
the only variant so we can skip doing any tests on it.
-}
flattenPatterns : Branch -> Branch
flattenPatterns (Branch goal pathPatterns) =
    Branch goal (List.foldr flatten [] pathPatterns)


flatten : ( T.CODT_Path, Can.Pattern ) -> List ( T.CODT_Path, Can.Pattern ) -> List ( T.CODT_Path, Can.Pattern )
flatten (( path, T.CRA_At region pattern ) as pathPattern) otherPathPatterns =
    case pattern of
        Can.PVar _ ->
            pathPattern :: otherPathPatterns

        Can.PAnything ->
            pathPattern :: otherPathPatterns

        Can.PCtor { union, args } ->
            let
                (T.CASTC_Union _ _ numAlts _) =
                    union
            in
            if numAlts == 1 then
                case List.map dearg args of
                    [ arg ] ->
                        flatten ( T.CODT_Unbox path, arg ) otherPathPatterns

                    args_ ->
                        List.foldr flatten otherPathPatterns (subPositions path args_)

            else
                pathPattern :: otherPathPatterns

        Can.PTuple a b maybeC ->
            flatten ( T.CODT_Index Index.first path, a ) <|
                flatten ( T.CODT_Index Index.second path, b ) <|
                    case maybeC of
                        Nothing ->
                            otherPathPatterns

                        Just c ->
                            flatten ( T.CODT_Index Index.third path, c ) otherPathPatterns

        Can.PUnit ->
            otherPathPatterns

        Can.PAlias realPattern alias ->
            flatten ( path, realPattern ) <|
                ( path, T.CRA_At region (Can.PVar alias) )
                    :: otherPathPatterns

        Can.PRecord _ ->
            pathPattern :: otherPathPatterns

        Can.PList _ ->
            pathPattern :: otherPathPatterns

        Can.PCons _ _ ->
            pathPattern :: otherPathPatterns

        Can.PChr _ ->
            pathPattern :: otherPathPatterns

        Can.PStr _ ->
            pathPattern :: otherPathPatterns

        Can.PInt _ ->
            pathPattern :: otherPathPatterns

        Can.PBool _ _ ->
            pathPattern :: otherPathPatterns


subPositions : T.CODT_Path -> List Can.Pattern -> List ( T.CODT_Path, Can.Pattern )
subPositions path patterns =
    Index.indexedMap (\index pattern -> ( T.CODT_Index index path, pattern )) patterns


dearg : Can.PatternCtorArg -> Can.Pattern
dearg (Can.PatternCtorArg _ _ pattern) =
    pattern



-- SUCCESSFULLY MATCH


{-| If the first branch has no more "decision points" we can finally take that
path. If that is the case we give the resulting label and a mapping from free
variables to "how to get their value". So a pattern like (Just (x,\_)) will give
us something like ("x" => value.0.0)
-}
checkForMatch : List Branch -> Maybe Int
checkForMatch branches =
    case branches of
        (Branch goal patterns) :: _ ->
            if List.all (not << needsTests << Tuple.second) patterns then
                Just goal

            else
                Nothing

        _ ->
            Nothing



-- GATHER OUTGOING EDGES


gatherEdges : List Branch -> T.CODT_Path -> ( List ( T.CODT_Test, List Branch ), List Branch )
gatherEdges branches path =
    let
        relevantTests : List T.CODT_Test
        relevantTests =
            testsAtPath path branches

        allEdges : List ( T.CODT_Test, List Branch )
        allEdges =
            List.map (edgesFor path branches) relevantTests

        fallbacks : List Branch
        fallbacks =
            if isComplete relevantTests then
                []

            else
                List.filter (isIrrelevantTo path) branches
    in
    ( allEdges, fallbacks )



-- FIND RELEVANT TESTS


testsAtPath : T.CODT_Path -> List Branch -> List T.CODT_Test
testsAtPath selectedPath branches =
    let
        allTests : List T.CODT_Test
        allTests =
            List.filterMap (testAtPath selectedPath) branches

        skipVisited : T.CODT_Test -> ( List T.CODT_Test, EverySet.EverySet String T.CODT_Test ) -> ( List T.CODT_Test, EverySet.EverySet String T.CODT_Test )
        skipVisited test (( uniqueTests, visitedTests ) as curr) =
            if EverySet.member (Encode.encode 0 << testEncoder) test visitedTests then
                curr

            else
                ( test :: uniqueTests
                , EverySet.insert (Encode.encode 0 << testEncoder) test visitedTests
                )
    in
    Tuple.first (List.foldr skipVisited ( [], EverySet.empty ) allTests)


testAtPath : T.CODT_Path -> Branch -> Maybe T.CODT_Test
testAtPath selectedPath (Branch _ pathPatterns) =
    Utils.listLookup selectedPath pathPatterns
        |> Maybe.andThen
            (\(T.CRA_At _ pattern) ->
                case pattern of
                    Can.PCtor { home, union, name, index } ->
                        let
                            (T.CASTC_Union _ _ numAlts opts) =
                                union
                        in
                        Just (T.CODT_IsCtor home name index numAlts opts)

                    Can.PList ps ->
                        Just
                            (case ps of
                                [] ->
                                    T.CODT_IsNil

                                _ ->
                                    T.CODT_IsCons
                            )

                    Can.PCons _ _ ->
                        Just T.CODT_IsCons

                    Can.PTuple _ _ _ ->
                        Just T.CODT_IsTuple

                    Can.PUnit ->
                        Just T.CODT_IsTuple

                    Can.PVar _ ->
                        Nothing

                    Can.PAnything ->
                        Nothing

                    Can.PInt int ->
                        Just (T.CODT_IsInt int)

                    Can.PStr str ->
                        Just (T.CODT_IsStr str)

                    Can.PChr chr ->
                        Just (T.CODT_IsChr chr)

                    Can.PBool _ bool ->
                        Just (T.CODT_IsBool bool)

                    Can.PRecord _ ->
                        Nothing

                    Can.PAlias _ _ ->
                        crash "aliases should never reach 'testAtPath' function"
            )



-- BUILD EDGES


edgesFor : T.CODT_Path -> List Branch -> T.CODT_Test -> ( T.CODT_Test, List Branch )
edgesFor path branches test =
    ( test
    , List.filterMap (toRelevantBranch test path) branches
    )


toRelevantBranch : T.CODT_Test -> T.CODT_Path -> Branch -> Maybe Branch
toRelevantBranch test path ((Branch goal pathPatterns) as branch) =
    case extract path pathPatterns of
        Found start (T.CRA_At region pattern) end ->
            case pattern of
                Can.PCtor { union, name, args } ->
                    case test of
                        T.CODT_IsCtor _ testName _ _ _ ->
                            if name == testName then
                                Just
                                    (Branch goal <|
                                        case List.map dearg args of
                                            (arg :: []) as args_ ->
                                                let
                                                    (T.CASTC_Union _ _ numAlts _) =
                                                        union
                                                in
                                                if numAlts == 1 then
                                                    start ++ (( T.CODT_Unbox path, arg ) :: end)

                                                else
                                                    start ++ subPositions path args_ ++ end

                                            args_ ->
                                                start ++ subPositions path args_ ++ end
                                    )

                            else
                                Nothing

                        _ ->
                            Nothing

                Can.PList [] ->
                    case test of
                        T.CODT_IsNil ->
                            Just (Branch goal (start ++ end))

                        _ ->
                            Nothing

                Can.PList (hd :: tl) ->
                    case test of
                        T.CODT_IsCons ->
                            let
                                tl_ : T.CRA_Located Can.Pattern_
                                tl_ =
                                    T.CRA_At region (Can.PList tl)
                            in
                            Just (Branch goal (start ++ subPositions path [ hd, tl_ ] ++ end))

                        _ ->
                            Nothing

                Can.PCons hd tl ->
                    case test of
                        T.CODT_IsCons ->
                            Just (Branch goal (start ++ subPositions path [ hd, tl ] ++ end))

                        _ ->
                            Nothing

                Can.PChr chr ->
                    case test of
                        T.CODT_IsChr testChr ->
                            if chr == testChr then
                                Just (Branch goal (start ++ end))

                            else
                                Nothing

                        _ ->
                            Nothing

                Can.PStr str ->
                    case test of
                        T.CODT_IsStr testStr ->
                            if str == testStr then
                                Just (Branch goal (start ++ end))

                            else
                                Nothing

                        _ ->
                            Nothing

                Can.PInt int ->
                    case test of
                        T.CODT_IsInt testInt ->
                            if int == testInt then
                                Just (Branch goal (start ++ end))

                            else
                                Nothing

                        _ ->
                            Nothing

                Can.PBool _ bool ->
                    case test of
                        T.CODT_IsBool testBool ->
                            if bool == testBool then
                                Just (Branch goal (start ++ end))

                            else
                                Nothing

                        _ ->
                            Nothing

                Can.PUnit ->
                    Just (Branch goal (start ++ end))

                Can.PTuple a b maybeC ->
                    Just
                        (Branch goal
                            (start
                                ++ subPositions path
                                    (a
                                        :: b
                                        :: (Maybe.map List.singleton maybeC
                                                |> Maybe.withDefault []
                                           )
                                    )
                                ++ end
                            )
                        )

                Can.PVar _ ->
                    Just branch

                Can.PAnything ->
                    Just branch

                Can.PRecord _ ->
                    Just branch

                Can.PAlias _ _ ->
                    Just branch

        NotFound ->
            Just branch


type Extract
    = NotFound
    | Found (List ( T.CODT_Path, Can.Pattern )) Can.Pattern (List ( T.CODT_Path, Can.Pattern ))


extract : T.CODT_Path -> List ( T.CODT_Path, Can.Pattern ) -> Extract
extract selectedPath pathPatterns =
    case pathPatterns of
        [] ->
            NotFound

        (( path, pattern ) as first) :: rest ->
            if path == selectedPath then
                Found [] pattern rest

            else
                case extract selectedPath rest of
                    NotFound ->
                        NotFound

                    Found start foundPattern end ->
                        Found (first :: start) foundPattern end



-- FIND IRRELEVANT BRANCHES


isIrrelevantTo : T.CODT_Path -> Branch -> Bool
isIrrelevantTo selectedPath (Branch _ pathPatterns) =
    case Utils.listLookup selectedPath pathPatterns of
        Nothing ->
            True

        Just pattern ->
            not (needsTests pattern)


needsTests : Can.Pattern -> Bool
needsTests (T.CRA_At _ pattern) =
    case pattern of
        Can.PVar _ ->
            False

        Can.PAnything ->
            False

        Can.PRecord _ ->
            False

        Can.PCtor _ ->
            True

        Can.PList _ ->
            True

        Can.PCons _ _ ->
            True

        Can.PUnit ->
            True

        Can.PTuple _ _ _ ->
            True

        Can.PChr _ ->
            True

        Can.PStr _ ->
            True

        Can.PInt _ ->
            True

        Can.PBool _ _ ->
            True

        Can.PAlias _ _ ->
            crash "aliases should never reach 'isIrrelevantTo' function"



-- PICK A PATH


pickPath : List Branch -> T.CODT_Path
pickPath branches =
    let
        allPaths : List T.CODT_Path
        allPaths =
            List.filterMap isChoicePath (List.concatMap (\(Branch _ patterns) -> patterns) branches)
    in
    case bests (addWeights (smallDefaults branches) allPaths) of
        [ path ] ->
            path

        tiedPaths ->
            Prelude.head (bests (addWeights (smallBranchingFactor branches) tiedPaths))


isChoicePath : ( T.CODT_Path, Can.Pattern ) -> Maybe T.CODT_Path
isChoicePath ( path, pattern ) =
    if needsTests pattern then
        Just path

    else
        Nothing


addWeights : (T.CODT_Path -> Int) -> List T.CODT_Path -> List ( T.CODT_Path, Int )
addWeights toWeight paths =
    List.map (\path -> ( path, toWeight path )) paths


bests : List ( T.CODT_Path, Int ) -> List T.CODT_Path
bests allPaths =
    case allPaths of
        [] ->
            crash "Cannot choose the best of zero paths. This should never happen."

        ( headPath, headWeight ) :: weightedPaths ->
            let
                gatherMinimum : ( a, comparable ) -> ( comparable, List a ) -> ( comparable, List a )
                gatherMinimum ( path, weight ) (( minWeight, paths ) as acc) =
                    if weight == minWeight then
                        ( minWeight, path :: paths )

                    else if weight < minWeight then
                        ( weight, [ path ] )

                    else
                        acc
            in
            Tuple.second (List.foldl gatherMinimum ( headWeight, [ headPath ] ) weightedPaths)



-- PATH PICKING HEURISTICS


smallDefaults : List Branch -> T.CODT_Path -> Int
smallDefaults branches path =
    List.length (List.filter (isIrrelevantTo path) branches)


smallBranchingFactor : List Branch -> T.CODT_Path -> Int
smallBranchingFactor branches path =
    let
        ( edges, fallback ) =
            gatherEdges branches path
    in
    List.length edges
        + (if List.isEmpty fallback then
            0

           else
            1
          )



-- ENCODERS and DECODERS


pathEncoder : T.CODT_Path -> Encode.Value
pathEncoder path_ =
    case path_ of
        T.CODT_Index index path ->
            Encode.object
                [ ( "type", Encode.string "Index" )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "path", pathEncoder path )
                ]

        T.CODT_Unbox path ->
            Encode.object
                [ ( "type", Encode.string "Unbox" )
                , ( "path", pathEncoder path )
                ]

        T.CODT_Empty ->
            Encode.object
                [ ( "type", Encode.string "Empty" )
                ]


pathDecoder : Decode.Decoder T.CODT_Path
pathDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Index" ->
                        Decode.map2 T.CODT_Index
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "path" pathDecoder)

                    "Unbox" ->
                        Decode.map T.CODT_Unbox (Decode.field "path" pathDecoder)

                    "Empty" ->
                        Decode.succeed T.CODT_Empty

                    _ ->
                        Decode.fail ("Unknown Path's type: " ++ type_)
            )


testEncoder : T.CODT_Test -> Encode.Value
testEncoder test =
    case test of
        T.CODT_IsCtor home name index numAlts opts ->
            Encode.object
                [ ( "type", Encode.string "IsCtor" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "numAlts", Encode.int numAlts )
                , ( "opts", Can.ctorOptsEncoder opts )
                ]

        T.CODT_IsCons ->
            Encode.object
                [ ( "type", Encode.string "IsCons" )
                ]

        T.CODT_IsNil ->
            Encode.object
                [ ( "type", Encode.string "IsNil" )
                ]

        T.CODT_IsTuple ->
            Encode.object
                [ ( "type", Encode.string "IsTuple" )
                ]

        T.CODT_IsInt value ->
            Encode.object
                [ ( "type", Encode.string "IsInt" )
                , ( "value", Encode.int value )
                ]

        T.CODT_IsChr value ->
            Encode.object
                [ ( "type", Encode.string "IsChr" )
                , ( "value", Encode.string value )
                ]

        T.CODT_IsStr value ->
            Encode.object
                [ ( "type", Encode.string "IsStr" )
                , ( "value", Encode.string value )
                ]

        T.CODT_IsBool value ->
            Encode.object
                [ ( "type", Encode.string "IsBool" )
                , ( "value", Encode.bool value )
                ]


testDecoder : Decode.Decoder T.CODT_Test
testDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "IsCtor" ->
                        Decode.map5 T.CODT_IsCtor
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "numAlts" Decode.int)
                            (Decode.field "opts" Can.ctorOptsDecoder)

                    "IsCons" ->
                        Decode.succeed T.CODT_IsCons

                    "IsNil" ->
                        Decode.succeed T.CODT_IsNil

                    "IsTuple" ->
                        Decode.succeed T.CODT_IsTuple

                    "IsInt" ->
                        Decode.map T.CODT_IsInt (Decode.field "value" Decode.int)

                    "IsChr" ->
                        Decode.map T.CODT_IsChr (Decode.field "value" Decode.string)

                    "IsStr" ->
                        Decode.map T.CODT_IsStr (Decode.field "value" Decode.string)

                    "IsBool" ->
                        Decode.map T.CODT_IsBool (Decode.field "value" Decode.bool)

                    _ ->
                        Decode.fail ("Unknown Test's type: " ++ type_)
            )
