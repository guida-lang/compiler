module Compiler.Generate.JavaScript.SourceMap exposing (generate)

import Base64
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Generate.JavaScript.Builder as JS
import Compiler.Generate.JavaScript.Name as JSName
import Data.Map as Dict exposing (Dict)
import Json.Encode as Encode
import System.TypeCheck.IO as IO
import Utils.Main as Utils
import VLQ


generate : Int -> Int -> Dict (List String) IO.Canonical String -> List JS.Mapping -> String
generate leadingLines kernelLeadingLines moduleSources mappings =
    "\n"
        ++ "//# sourceMappingURL=data:application/json;base64,"
        ++ generateHelp leadingLines kernelLeadingLines moduleSources mappings


generateHelp : Int -> Int -> Dict (List String) IO.Canonical String -> List JS.Mapping -> String
generateHelp leadingLines kernelLeadingLines moduleSources mappings =
    mappings
        |> List.map
            (\(JS.Mapping srcLine srcCol srcModule srcName genLine genCol) ->
                JS.Mapping srcLine srcCol srcModule srcName (genLine + leadingLines + kernelLeadingLines) genCol
            )
        |> parseMappings
        |> mappingsToJson moduleSources
        |> Encode.encode 4
        |> Base64.encode


type Mappings
    = Mappings (OrderedListBuilder (List String) IO.Canonical) (OrderedListBuilder String JSName.Name) SegmentAccounting String


type SegmentAccounting
    = SegmentAccounting (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)


parseMappings : List JS.Mapping -> Mappings
parseMappings mappings =
    let
        mappingMap : Dict Int Int (List JS.Mapping)
        mappingMap =
            List.foldr
                (\((JS.Mapping _ _ _ _ genLine _) as mapping) acc ->
                    Dict.update identity genLine (mappingMapUpdater mapping) acc
                )
                Dict.empty
                mappings
    in
    parseMappingsHelp 1 (Tuple.first (Utils.findMax compare mappingMap)) mappingMap <|
        Mappings emptyOrderedListBuilder emptyOrderedListBuilder (SegmentAccounting Nothing Nothing Nothing Nothing Nothing) ""


mappingMapUpdater : JS.Mapping -> Maybe (List JS.Mapping) -> Maybe (List JS.Mapping)
mappingMapUpdater toInsert maybeVal =
    case maybeVal of
        Nothing ->
            Just [ toInsert ]

        Just existing ->
            Just (toInsert :: existing)


parseMappingsHelp : Int -> Int -> Dict Int Int (List JS.Mapping) -> Mappings -> Mappings
parseMappingsHelp currentLine lastLine mappingMap acc =
    if currentLine > lastLine then
        acc

    else
        case Dict.get identity currentLine mappingMap of
            Nothing ->
                parseMappingsHelp (currentLine + 1)
                    lastLine
                    mappingMap
                    (prepareForNewLine acc)

            Just segments ->
                let
                    sortedSegments : List JS.Mapping
                    sortedSegments =
                        List.sortBy (\(JS.Mapping _ _ _ _ _ genCol) -> -genCol) segments
                in
                parseMappingsHelp (currentLine + 1)
                    lastLine
                    mappingMap
                    (prepareForNewLine (List.foldr encodeSegment acc sortedSegments))


prepareForNewLine : Mappings -> Mappings
prepareForNewLine (Mappings srcs nms (SegmentAccounting _ saPrevSourceIdx saPrevSourceLine saPrevSourceCol saPrevNameIdx) vlqs) =
    Mappings
        srcs
        nms
        (SegmentAccounting Nothing saPrevSourceIdx saPrevSourceLine saPrevSourceCol saPrevNameIdx)
        (vlqs ++ ";")


encodeSegment : JS.Mapping -> Mappings -> Mappings
encodeSegment (JS.Mapping segmentSrcLine segmentSrcCol segmentSrcModule segmentSrcName _ segmentGenCol) (Mappings srcs nms (SegmentAccounting saPrevCol saPrevSourceIdx saPrevSourceLine saPrevSourceCol saPrevNameIdx) vlqs) =
    let
        newSources : OrderedListBuilder (List String) IO.Canonical
        newSources =
            insertIntoOrderedListBuilder ModuleName.toComparableCanonical segmentSrcModule srcs

        genCol : Int
        genCol =
            segmentGenCol - 1

        moduleIdx : Int
        moduleIdx =
            Maybe.withDefault 0 (lookupIndexOrderedListBuilder ModuleName.toComparableCanonical segmentSrcModule newSources)

        sourceLine : Int
        sourceLine =
            segmentSrcLine - 1

        sourceCol : Int
        sourceCol =
            segmentSrcCol - 1

        genColDelta : Int
        genColDelta =
            genCol - Maybe.withDefault 0 saPrevCol

        moduleIdxDelta : Int
        moduleIdxDelta =
            moduleIdx - Maybe.withDefault 0 saPrevSourceIdx

        sourceLineDelta : Int
        sourceLineDelta =
            sourceLine - Maybe.withDefault 0 saPrevSourceLine

        sourceColDelta : Int
        sourceColDelta =
            sourceCol - Maybe.withDefault 0 saPrevSourceCol

        ((SegmentAccounting updatedSaPrevCol updatedSaPrevSourceIdx updatedSaPrevSourceLine updatedSaPrevSourceCol _) as updatedSa) =
            SegmentAccounting (Just genCol) (Just moduleIdx) (Just sourceLine) (Just sourceCol) saPrevNameIdx

        vlqPrefix : String
        vlqPrefix =
            case saPrevCol of
                Nothing ->
                    ""

                Just _ ->
                    ","
    in
    case segmentSrcName of
        Just segmentName ->
            let
                newNames : OrderedListBuilder JSName.Name JSName.Name
                newNames =
                    insertIntoOrderedListBuilder identity segmentName nms

                nameIdx : Int
                nameIdx =
                    Maybe.withDefault 0 (lookupIndexOrderedListBuilder identity segmentName newNames)

                nameIdxDelta : Int
                nameIdxDelta =
                    nameIdx - Maybe.withDefault 0 saPrevNameIdx
            in
            Mappings newSources newNames (SegmentAccounting updatedSaPrevCol updatedSaPrevSourceIdx updatedSaPrevSourceLine updatedSaPrevSourceCol (Just nameIdx)) <|
                vlqs
                    ++ vlqPrefix
                    ++ VLQ.encode
                        [ genColDelta
                        , moduleIdxDelta
                        , sourceLineDelta
                        , sourceColDelta
                        , nameIdxDelta
                        ]

        Nothing ->
            Mappings newSources nms updatedSa <|
                vlqs
                    ++ vlqPrefix
                    ++ VLQ.encode
                        [ genColDelta
                        , moduleIdxDelta
                        , sourceLineDelta
                        , sourceColDelta
                        ]



-- Array builder


type OrderedListBuilder c k
    = OrderedListBuilder Int (Dict c k Int)


emptyOrderedListBuilder : OrderedListBuilder c k
emptyOrderedListBuilder =
    OrderedListBuilder 0 Dict.empty


insertIntoOrderedListBuilder : (k -> comparable) -> k -> OrderedListBuilder comparable k -> OrderedListBuilder comparable k
insertIntoOrderedListBuilder toComparable value ((OrderedListBuilder nextIndex values) as builder) =
    case Dict.get toComparable value values of
        Just _ ->
            builder

        Nothing ->
            OrderedListBuilder (nextIndex + 1) (Dict.insert toComparable value nextIndex values)


lookupIndexOrderedListBuilder : (k -> comparable) -> k -> OrderedListBuilder comparable k -> Maybe Int
lookupIndexOrderedListBuilder toComparable value (OrderedListBuilder _ values) =
    Dict.get toComparable value values


orderedListBuilderToList : (k -> k -> Order) -> OrderedListBuilder c k -> List k
orderedListBuilderToList keyComparison (OrderedListBuilder _ values) =
    values
        |> Dict.toList keyComparison
        |> List.map (\( val, idx ) -> ( idx, val ))
        |> Dict.fromList identity
        |> Dict.values compare


mappingsToJson : Dict (List String) IO.Canonical String -> Mappings -> Encode.Value
mappingsToJson moduleSources (Mappings sources names _ vlqs) =
    let
        moduleNames : List IO.Canonical
        moduleNames =
            orderedListBuilderToList ModuleName.compareCanonical sources
    in
    Encode.object
        [ ( "version", Encode.int 3 )
        , ( "sources", Encode.list (\(IO.Canonical _ name) -> Encode.string name) moduleNames )
        , ( "sourcesContent"
          , Encode.list
                (\moduleName ->
                    Dict.get ModuleName.toComparableCanonical moduleName moduleSources
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                )
                moduleNames
          )
        , ( "names", Encode.list (\jsName -> Encode.string jsName) (orderedListBuilderToList compare names) )
        , ( "mappings", Encode.string vlqs )
        ]
