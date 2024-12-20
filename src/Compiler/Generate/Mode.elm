module Compiler.Generate.Mode exposing
    ( Mode(..)
    , ShortFieldNames
    , isDebug
    , shortenFieldNames
    )

import Compiler.Generate.JavaScript.Name as JsName
import Data.Map as Dict exposing (Dict)
import Types as T
import Utils.Main as Utils



-- MODE


type Mode
    = Dev (Maybe T.CECTE_Types)
    | Prod ShortFieldNames


isDebug : Mode -> Bool
isDebug mode =
    case mode of
        Dev (Just _) ->
            True

        Dev Nothing ->
            False

        Prod _ ->
            False



-- SHORTEN FIELD NAMES


type alias ShortFieldNames =
    Dict String T.CDN_Name JsName.Name


shortenFieldNames : T.CASTO_GlobalGraph -> ShortFieldNames
shortenFieldNames (T.CASTO_GlobalGraph _ frequencies) =
    Dict.foldr compare (\_ -> addToShortNames) Dict.empty <|
        Dict.foldr compare addToBuckets Dict.empty frequencies


addToBuckets : T.CDN_Name -> Int -> Dict Int Int (List T.CDN_Name) -> Dict Int Int (List T.CDN_Name)
addToBuckets field frequency buckets =
    Utils.mapInsertWith identity (++) frequency [ field ] buckets


addToShortNames : List T.CDN_Name -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
    List.foldl addField shortNames fields


addField : T.CDN_Name -> ShortFieldNames -> ShortFieldNames
addField field shortNames =
    let
        rename : JsName.Name
        rename =
            JsName.fromInt (Dict.size shortNames)
    in
    Dict.insert identity field rename shortNames
