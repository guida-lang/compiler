module Graph exposing
    ( SCC(..)
    , stronglyConnComp
    )

-- Parts ported from https://hackage.haskell.org/package/containers-0.7/docs/Data-Graph.html

import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Utils.Main as Utils



-- STRONGLY CONNECTED COMPONENTS


type SCC vertex
    = AcyclicSCC vertex
    | CyclicSCC (List vertex)


stronglyConnComp : List ( node, comparable, List comparable ) -> List (SCC node)
stronglyConnComp edges0 =
    let
        get_node : SCC ( a, b, c ) -> SCC a
        get_node scc =
            case scc of
                AcyclicSCC ( n, _, _ ) ->
                    AcyclicSCC n

                CyclicSCC triples ->
                    CyclicSCC (List.map (\( n, _, _ ) -> n) triples)
    in
    List.map get_node (stronglyConnCompR edges0)


stronglyConnCompR :
    List ( node, comparable, List comparable )
    -- ^ The graph: a list of nodes uniquely identified by keys,
    -- with a list of keys of nodes this node has edges to.
    -- The out-list may contain keys that don't correspond to
    -- nodes of the graph; such edges are ignored.
    -> List (SCC ( node, comparable, List comparable )) -- ^ Reverse topologically sorted
stronglyConnCompR edges0 =
    let
        lt : ( node, comparable, List comparable ) -> ( node, comparable, List comparable ) -> Order
        lt ( _, v, _ ) ( _, w, _ ) =
            compare v w

        sorted : List ( Int, ( node, comparable, List comparable ) )
        sorted =
            List.sortWith lt edges0 |> List.indexedMap Tuple.pair

        keyMap : Dict comparable comparable Vertex
        keyMap =
            List.foldl
                (\( i, ( _, k, _ ) ) acc -> Dict.insert identity k i acc)
                Dict.empty
                sorted

        nodeMap : Dict Int Vertex ( node, comparable, List comparable )
        nodeMap =
            Dict.fromList identity sorted

        getNode : Vertex -> ( node, comparable, List comparable )
        getNode vertex =
            Utils.find identity vertex nodeMap

        edges : List ( Vertex, List Vertex )
        edges =
            List.filterMap
                (\( _, from, neighbors ) ->
                    Dict.get identity from keyMap
                        |> Maybe.map
                            (\vertex ->
                                ( vertex, List.filterMap (\to -> Dict.get identity to keyMap) neighbors )
                            )
                )
                edges0

        selfRefs : EverySet Int Vertex
        selfRefs =
            List.foldl
                (\( from, neighbors ) acc ->
                    if List.member from neighbors then
                        EverySet.insert identity from acc

                    else
                        acc
                )
                EverySet.empty
                edges

        graph : Graph
        graph =
            Dict.fromList identity edges

        components : List (List Vertex)
        components =
            sccs graph

        toSCC : List Vertex -> SCC ( node, comparable, List comparable )
        toSCC component =
            case component of
                [ vertex ] ->
                    if EverySet.member identity vertex selfRefs then
                        CyclicSCC [ getNode vertex ]

                    else
                        AcyclicSCC (getNode vertex)

                _ ->
                    CyclicSCC (List.map getNode component)
    in
    List.map toSCC components



-- KOSARAJU'S ALGORITHM


type alias Vertex =
    Int


type alias Graph =
    Dict Int Vertex (List Vertex)


type VisitTask
    = CheckVisited Vertex
    | Output Vertex


sccs : Graph -> List (List Vertex)
sccs graph =
    assign (reverse graph) (visit graph)


visit : Graph -> List Vertex
visit graph =
    let
        check : List Vertex -> List VisitTask
        check vertices =
            List.map CheckVisited vertices

        go : List VisitTask -> EverySet Int Vertex -> List Vertex -> ( EverySet Int Vertex, List Vertex )
        go tasks visited output =
            case tasks of
                [] ->
                    ( visited, output )

                (CheckVisited vertex) :: rest ->
                    if EverySet.member identity vertex visited then
                        go rest visited output

                    else
                        let
                            neighbors : List Vertex
                            neighbors =
                                Dict.get identity vertex graph |> Maybe.withDefault []

                            newTasks : List VisitTask
                            newTasks =
                                check neighbors ++ (Output vertex :: rest)

                            newVisited : EverySet Vertex Vertex
                            newVisited =
                                EverySet.insert identity vertex visited
                        in
                        go newTasks newVisited output

                (Output vertex) :: rest ->
                    go rest visited (vertex :: output)
    in
    go (check (Dict.keys compare graph)) EverySet.empty [] |> Tuple.second


reverse : Graph -> Graph
reverse originalGraph =
    let
        addReversedEdge : Vertex -> Vertex -> Graph -> Graph
        addReversedEdge from to graph =
            Dict.update identity to (\neighbors -> Just (from :: Maybe.withDefault [] neighbors)) graph

        addReversedEdges : Graph -> Vertex -> List Vertex -> Graph
        addReversedEdges graph from tos =
            List.foldl (\to acc -> addReversedEdge from to acc) graph tos

        addAllReversedEdges : Graph -> Graph
        addAllReversedEdges graph =
            Dict.foldl compare (\k b a -> addReversedEdges a k b) Dict.empty graph
    in
    addAllReversedEdges originalGraph


type AssignTask
    = CheckAssigned Vertex
    | FinishComponent


assign : Graph -> List Vertex -> List (List Vertex)
assign graph orderedVertices =
    let
        check : List Vertex -> List AssignTask
        check vertices =
            List.map CheckAssigned vertices

        go : List AssignTask -> EverySet Int Vertex -> List Vertex -> List (List Vertex) -> ( EverySet Int Vertex, List Vertex, List (List Vertex) )
        go tasks assigned scc output =
            case tasks of
                [] ->
                    ( assigned, scc, output )

                (CheckAssigned vertex) :: rest ->
                    if EverySet.member identity vertex assigned then
                        go rest assigned scc output

                    else
                        let
                            neighborTasks : List AssignTask
                            neighborTasks =
                                Dict.get identity vertex graph |> Maybe.withDefault [] |> check

                            finishTasks : List AssignTask
                            finishTasks =
                                case scc of
                                    [] ->
                                        [ FinishComponent ]

                                    _ ->
                                        []

                            newTasks : List AssignTask
                            newTasks =
                                neighborTasks ++ finishTasks ++ rest

                            newAssigned : EverySet Vertex Vertex
                            newAssigned =
                                EverySet.insert identity vertex assigned

                            newScc : List Vertex
                            newScc =
                                vertex :: scc
                        in
                        go newTasks newAssigned newScc output

                FinishComponent :: rest ->
                    go rest assigned [] (scc :: output)
    in
    go (check orderedVertices) EverySet.empty [] [] |> (\( _, _, output ) -> output)
