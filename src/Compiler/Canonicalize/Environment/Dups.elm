module Compiler.Canonicalize.Environment.Dups exposing
    ( Info
    , ToError
    , Tracker
    , checkFields
    , checkFields_
    , checkLocatedFields
    , checkLocatedFields_
    , detect
    , detectLocated
    , insert
    , none
    , one
    , union
    , unions
    )

import Compiler.Data.Name exposing (Name)
import Compiler.Data.OneOrMore as OneOrMore exposing (OneOrMore)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error exposing (Error)
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Utils.Main as Utils



-- DUPLICATE TRACKER


type alias Tracker value =
    Dict String Name (OneOrMore (Info value))


type Info value
    = Info A.Region value



-- DETECT


type alias ToError =
    Name -> A.Region -> A.Region -> Error


detect : ToError -> Tracker a -> R.RResult i w Error (Dict String Name a)
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


detectLocated : ToError -> Tracker a -> R.RResult i w Error (Dict String (A.Located Name) a)
detectLocated toError dict =
    let
        nameLocations : Dict String Name A.Region
        nameLocations =
            Utils.mapMapMaybe identity compare extractLocation dict
    in
    dict
        |> Utils.mapMapKeys A.toValue compare (\k -> A.At (Maybe.withDefault A.zero <| Dict.get identity k nameLocations) k)
        |> R.mapTraverseWithKey A.toValue (\a b -> compare (A.toValue a) (A.toValue b)) (\(A.At _ name) values -> detectHelp toError name values)


extractLocation : OneOrMore.OneOrMore (Info a) -> Maybe A.Region
extractLocation oneOrMore =
    case oneOrMore of
        OneOrMore.One (Info region _) ->
            Just region

        OneOrMore.More _ _ ->
            Nothing


detectHelp : ToError -> Name -> OneOrMore (Info a) -> R.RResult i w Error a
detectHelp toError name values =
    case values of
        OneOrMore.One (Info _ value) ->
            R.ok value

        OneOrMore.More left right ->
            let
                ( Info r1 _, Info r2 _ ) =
                    OneOrMore.getFirstTwo left right
            in
            R.throw (toError name r1 r2)



-- CHECK FIELDS


checkLocatedFields : List ( A.Located Name, a ) -> R.RResult i w Error (Dict String (A.Located Name) a)
checkLocatedFields fields =
    detectLocated Error.DuplicateField (List.foldr addField none fields)


checkFields : List ( A.Located Name, a ) -> R.RResult i w Error (Dict String Name a)
checkFields fields =
    detect Error.DuplicateField (List.foldr addField none fields)


addField : ( A.Located Name, a ) -> Tracker a -> Tracker a
addField ( A.At region name, value ) dups =
    Utils.mapInsertWith identity OneOrMore.more name (OneOrMore.one (Info region value)) dups


checkLocatedFields_ : (A.Region -> a -> b) -> List ( A.Located Name, a ) -> R.RResult i w Error (Dict String (A.Located Name) b)
checkLocatedFields_ toValue fields =
    detectLocated Error.DuplicateField (List.foldr (addField_ toValue) none fields)


checkFields_ : (A.Region -> a -> b) -> List ( A.Located Name, a ) -> R.RResult i w Error (Dict String Name b)
checkFields_ toValue fields =
    detect Error.DuplicateField (List.foldr (addField_ toValue) none fields)


addField_ : (A.Region -> a -> b) -> ( A.Located Name, a ) -> Tracker b -> Tracker b
addField_ toValue ( A.At region name, value ) dups =
    Utils.mapInsertWith identity OneOrMore.more name (OneOrMore.one (Info region (toValue region value))) dups



-- BUILDING DICTIONARIES


none : Tracker a
none =
    Dict.empty


one : Name -> A.Region -> value -> Tracker value
one name region value =
    Dict.singleton identity name (OneOrMore.one (Info region value))


insert : Name -> A.Region -> a -> Tracker a -> Tracker a
insert name region value dict =
    Utils.mapInsertWith identity (\new old -> OneOrMore.more old new) name (OneOrMore.one (Info region value)) dict


union : Tracker a -> Tracker a -> Tracker a
union a b =
    Utils.mapUnionWith identity compare OneOrMore.more a b


unions : List (Tracker a) -> Tracker a
unions dicts =
    List.foldl union Dict.empty dicts
