module Compiler.Canonicalize.Environment.Dups exposing
    ( Info
    , ToError
    , Tracker
    , checkFields
    , checkFields_
    , detect
    , insert
    , none
    , one
    , union
    , unions
    )

import Compiler.Data.Name exposing (CDN_Name)
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error exposing (Error)
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Utils.Main as Utils



-- DUPLICATE TRACKER


type alias Tracker value =
    Dict String CDN_Name (OneOrMore (Info value))


type alias Info value =
    { region : A.CRA_Region
    , value : value
    }



-- DETECT


type alias ToError =
    CDN_Name -> A.CRA_Region -> A.CRA_Region -> Error


detect : ToError -> Tracker a -> R.RResult i w Error (Dict String CDN_Name a)
detect toError dict =
    Dict.foldl compare
        (\name values ->
            R.bind
                (\acc ->
                    R.fmap (\b -> Dict.insert identity name b acc)
                        (detectHelp toError name values)
                )
        )
        (R.ok Dict.empty)
        dict


detectHelp : ToError -> CDN_Name -> OneOrMore (Info a) -> R.RResult i w Error a
detectHelp toError name values =
    case values of
        OneOrMore.One { value } ->
            R.ok value

        OneOrMore.More left right ->
            let
                ( info1, info2 ) =
                    OneOrMore.getFirstTwo left right
            in
            R.throw (toError name info1.region info2.region)



-- CHECK FIELDS


checkFields : List ( A.CRA_Located CDN_Name, a ) -> R.RResult i w Error (Dict String CDN_Name a)
checkFields fields =
    detect Error.DuplicateField (List.foldr addField none fields)


addField : ( A.CRA_Located CDN_Name, a ) -> Tracker a -> Tracker a
addField ( A.CRA_At region name, value ) dups =
    Utils.mapInsertWith identity OneOrMore.more name (OneOrMore.one (Info region value)) dups


checkFields_ : (A.CRA_Region -> a -> b) -> List ( A.CRA_Located CDN_Name, a ) -> R.RResult i w Error (Dict String CDN_Name b)
checkFields_ toValue fields =
    detect Error.DuplicateField (List.foldr (addField_ toValue) none fields)


addField_ : (A.CRA_Region -> a -> b) -> ( A.CRA_Located CDN_Name, a ) -> Tracker b -> Tracker b
addField_ toValue ( A.CRA_At region name, value ) dups =
    Utils.mapInsertWith identity OneOrMore.more name (OneOrMore.one (Info region (toValue region value))) dups



-- BUILDING DICTIONARIES


none : Tracker a
none =
    Dict.empty


one : CDN_Name -> A.CRA_Region -> value -> Tracker value
one name region value =
    Dict.singleton identity name (OneOrMore.one (Info region value))


insert : CDN_Name -> A.CRA_Region -> a -> Tracker a -> Tracker a
insert name region value dict =
    Utils.mapInsertWith identity (\new old -> OneOrMore.more old new) name (OneOrMore.one (Info region value)) dict


union : Tracker a -> Tracker a -> Tracker a
union a b =
    Utils.mapUnionWith identity compare OneOrMore.more a b


unions : List (Tracker a) -> Tracker a
unions dicts =
    List.foldl union Dict.empty dicts
