module Compiler.Optimize.Case exposing (optimize)

import Compiler.Optimize.DecisionTree as DT
import Data.Map as Dict exposing (Dict)
import Prelude
import Types as T
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- OPTIMIZE A CASE EXPRESSION


optimize : T.CDN_Name -> T.CDN_Name -> List ( T.CASTC_Pattern, T.CASTO_Expr ) -> T.CASTO_Expr
optimize temp root optBranches =
    let
        ( patterns, indexedBranches ) =
            List.unzip (List.indexedMap indexify optBranches)

        decider : T.CASTO_Decider Int
        decider =
            treeToDecider (DT.compile patterns)

        targetCounts : Dict Int Int Int
        targetCounts =
            countTargets decider

        ( choices, maybeJumps ) =
            List.unzip (List.map (createChoices targetCounts) indexedBranches)
    in
    T.CASTO_Case temp
        root
        (insertChoices (Dict.fromList identity choices) decider)
        (List.filterMap identity maybeJumps)


indexify : Int -> ( a, b ) -> ( ( a, Int ), ( Int, b ) )
indexify index ( pattern, branch ) =
    ( ( pattern, index )
    , ( index, branch )
    )



-- TREE TO DECIDER
--
-- Decision trees may have some redundancies, so we convert them to a Decider
-- which has special constructs to avoid code duplication when possible.


treeToDecider : DT.DecisionTree -> T.CASTO_Decider Int
treeToDecider tree =
    case tree of
        DT.Match target ->
            T.CASTO_Leaf target

        -- zero options
        DT.Decision _ [] Nothing ->
            crash "compiler bug, somehow created an empty decision tree"

        -- one option
        DT.Decision _ [ ( _, subTree ) ] Nothing ->
            treeToDecider subTree

        DT.Decision _ [] (Just subTree) ->
            treeToDecider subTree

        -- two options
        DT.Decision path [ ( test, successTree ) ] (Just failureTree) ->
            toChain path test successTree failureTree

        DT.Decision path [ ( test, successTree ), ( _, failureTree ) ] Nothing ->
            toChain path test successTree failureTree

        -- many options
        DT.Decision path edges Nothing ->
            let
                ( necessaryTests, fallback ) =
                    ( Prelude.init edges, Tuple.second (Prelude.last edges) )
            in
            T.CASTO_FanOut
                path
                (List.map (Tuple.mapSecond treeToDecider) necessaryTests)
                (treeToDecider fallback)

        DT.Decision path edges (Just fallback) ->
            T.CASTO_FanOut path (List.map (Tuple.mapSecond treeToDecider) edges) (treeToDecider fallback)


toChain : T.CODT_Path -> T.CODT_Test -> DT.DecisionTree -> DT.DecisionTree -> T.CASTO_Decider Int
toChain path test successTree failureTree =
    let
        failure : T.CASTO_Decider Int
        failure =
            treeToDecider failureTree
    in
    case treeToDecider successTree of
        (T.CASTO_Chain testChain success subFailure) as success_ ->
            if failure == subFailure then
                T.CASTO_Chain (( path, test ) :: testChain) success failure

            else
                T.CASTO_Chain [ ( path, test ) ] success_ failure

        success ->
            T.CASTO_Chain [ ( path, test ) ] success failure



-- INSERT CHOICES
--
-- If a target appears exactly once in a Decider, the corresponding expression
-- can be inlined. Whether things are inlined or jumps is called a "choice".


countTargets : T.CASTO_Decider Int -> Dict Int Int Int
countTargets decisionTree =
    case decisionTree of
        T.CASTO_Leaf target ->
            Dict.singleton identity target 1

        T.CASTO_Chain _ success failure ->
            Utils.mapUnionWith identity compare (+) (countTargets success) (countTargets failure)

        T.CASTO_FanOut _ tests fallback ->
            Utils.mapUnionsWith identity compare (+) (List.map countTargets (fallback :: List.map Tuple.second tests))


createChoices : Dict Int Int Int -> ( Int, T.CASTO_Expr ) -> ( ( Int, T.CASTO_Choice ), Maybe ( Int, T.CASTO_Expr ) )
createChoices targetCounts ( target, branch ) =
    if Dict.get identity target targetCounts == Just 1 then
        ( ( target, T.CASTO_Inline branch )
        , Nothing
        )

    else
        ( ( target, T.CASTO_Jump target )
        , Just ( target, branch )
        )


insertChoices : Dict Int Int T.CASTO_Choice -> T.CASTO_Decider Int -> T.CASTO_Decider T.CASTO_Choice
insertChoices choiceDict decider =
    let
        go : T.CASTO_Decider Int -> T.CASTO_Decider T.CASTO_Choice
        go =
            insertChoices choiceDict
    in
    case decider of
        T.CASTO_Leaf target ->
            T.CASTO_Leaf (Utils.find identity target choiceDict)

        T.CASTO_Chain testChain success failure ->
            T.CASTO_Chain testChain (go success) (go failure)

        T.CASTO_FanOut path tests fallback ->
            T.CASTO_FanOut path (List.map (Tuple.mapSecond go) tests) (go fallback)
