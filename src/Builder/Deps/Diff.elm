module Builder.Deps.Diff exposing
    ( Changes(..)
    , ModuleChanges(..)
    , PackageChanges(..)
    , bump
    , diff
    , getDocs
    , moduleChangeMagnitude
    , toMagnitude
    )

import Builder.Deps.Website as Website
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit as Exit exposing (DocsProblem(..))
import Builder.Stuff as Stuff
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Type as Type
import Compiler.Elm.Docs as Docs
import Compiler.Elm.Magnitude as M
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V exposing (Version)
import Compiler.Json.Decode as D
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import List
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task


type PackageChanges
    = PackageChanges (List ModuleName.Raw) (Dict String ModuleName.Raw ModuleChanges) (List ModuleName.Raw)


type ModuleChanges
    = ModuleChanges (Changes String Name.Name Docs.Union) (Changes String Name.Name Docs.Alias) (Changes String Name.Name Docs.Value) (Changes String Name.Name Docs.Binop)


type Changes c k v
    = Changes (Dict c k v) (Dict c k ( v, v )) (Dict c k v)


getChanges : (k -> comparable) -> (k -> k -> Order) -> (v -> v -> Bool) -> Dict comparable k v -> Dict comparable k v -> Changes comparable k v
getChanges toComparable keyComparison isEquivalent old new =
    let
        overlap : Dict comparable k ( v, v )
        overlap =
            Utils.mapIntersectionWith toComparable keyComparison Tuple.pair old new

        changed : Dict comparable k ( v, v )
        changed =
            Dict.filter (\_ ( v1, v2 ) -> not (isEquivalent v1 v2)) overlap
    in
    Changes
        (Dict.diff new old)
        changed
        (Dict.diff old new)



-- DIFF


diff : Docs.Documentation -> Docs.Documentation -> PackageChanges
diff oldDocs newDocs =
    let
        filterOutPatches : Dict comparable a ModuleChanges -> Dict comparable a ModuleChanges
        filterOutPatches chngs =
            Dict.filter (\_ chng -> moduleChangeMagnitude chng /= M.PATCH) chngs

        (Changes added changed removed) =
            getChanges identity compare (\_ _ -> False) oldDocs newDocs
    in
    PackageChanges
        (Dict.keys compare added)
        (filterOutPatches (Dict.map (\_ -> diffModule) changed))
        (Dict.keys compare removed)


diffModule : ( Docs.Module, Docs.Module ) -> ModuleChanges
diffModule ( Docs.Module _ _ u1 a1 v1 b1, Docs.Module _ _ u2 a2 v2 b2 ) =
    ModuleChanges
        (getChanges identity compare isEquivalentUnion u1 u2)
        (getChanges identity compare isEquivalentAlias a1 a2)
        (getChanges identity compare isEquivalentValue v1 v2)
        (getChanges identity compare isEquivalentBinop b1 b2)



-- EQUIVALENCE


isEquivalentUnion : Docs.Union -> Docs.Union -> Bool
isEquivalentUnion (Docs.Union oldComment oldVars oldCtors) (Docs.Union newComment newVars newCtors) =
    let
        equiv : List Type.Type -> List Type.Type -> Bool
        equiv oldTypes newTypes =
            let
                allEquivalent : List Bool
                allEquivalent =
                    List.map2
                        isEquivalentAlias
                        (List.map (Docs.Alias oldComment oldVars) oldTypes)
                        (List.map (Docs.Alias newComment newVars) newTypes)
            in
            (List.length oldTypes == List.length newTypes)
                && List.all identity allEquivalent
    in
    (List.length oldCtors == List.length newCtors)
        && List.all identity (List.map2 (==) (List.map Tuple.first oldCtors) (List.map Tuple.first newCtors))
        && List.all identity (Dict.values compare (Utils.mapIntersectionWith identity compare equiv (Dict.fromList identity oldCtors) (Dict.fromList identity newCtors)))


isEquivalentAlias : Docs.Alias -> Docs.Alias -> Bool
isEquivalentAlias (Docs.Alias _ oldVars oldType) (Docs.Alias _ newVars newType) =
    case diffType oldType newType of
        Nothing ->
            False

        Just renamings ->
            (List.length oldVars == List.length newVars)
                && isEquivalentRenaming (List.map2 Tuple.pair oldVars newVars ++ renamings)


isEquivalentValue : Docs.Value -> Docs.Value -> Bool
isEquivalentValue (Docs.Value c1 t1) (Docs.Value c2 t2) =
    isEquivalentAlias (Docs.Alias c1 [] t1) (Docs.Alias c2 [] t2)


isEquivalentBinop : Docs.Binop -> Docs.Binop -> Bool
isEquivalentBinop (Docs.Binop c1 t1 a1 p1) (Docs.Binop c2 t2 a2 p2) =
    isEquivalentAlias (Docs.Alias c1 [] t1) (Docs.Alias c2 [] t2)
        && (a1 == a2)
        && (p1 == p2)



-- DIFF TYPES


diffType : Type.Type -> Type.Type -> Maybe (List ( Name.Name, Name.Name ))
diffType oldType newType =
    case ( oldType, newType ) of
        ( Type.Var oldName, Type.Var newName ) ->
            Just [ ( oldName, newName ) ]

        ( Type.Lambda a b, Type.Lambda a_ b_ ) ->
            Maybe.map2 (++) (diffType a a_) (diffType b b_)

        ( Type.Type oldName oldArgs, Type.Type newName newArgs ) ->
            if not (isSameName oldName newName) || List.length oldArgs /= List.length newArgs then
                Nothing

            else
                Maybe.map List.concat (Utils.zipWithM diffType oldArgs newArgs)

        ( Type.Record fields maybeExt, Type.Record fields_ maybeExt_ ) ->
            case ( maybeExt, maybeExt_ ) of
                ( Nothing, Just _ ) ->
                    Nothing

                ( Just _, Nothing ) ->
                    Nothing

                ( Nothing, Nothing ) ->
                    diffFields fields fields_

                ( Just oldExt, Just newExt ) ->
                    Maybe.map ((::) ( oldExt, newExt )) (diffFields fields fields_)

        ( Type.Unit, Type.Unit ) ->
            Just []

        ( Type.Tuple a b cs, Type.Tuple x y zs ) ->
            if List.length cs /= List.length zs then
                Nothing

            else
                Maybe.map3 (\aVars bVars cVars -> aVars ++ bVars ++ cVars)
                    (diffType a x)
                    (diffType b y)
                    (Maybe.map List.concat (Utils.zipWithM diffType cs zs))

        _ ->
            Nothing



-- handle very old docs that do not use qualified names


isSameName : Name.Name -> Name.Name -> Bool
isSameName oldFullName newFullName =
    let
        dedot : String -> List String
        dedot name =
            List.reverse (String.split "." name)
    in
    case ( dedot oldFullName, dedot newFullName ) of
        ( oldName :: [], newName :: _ ) ->
            oldName == newName

        ( oldName :: _, newName :: [] ) ->
            oldName == newName

        _ ->
            oldFullName == newFullName


diffFields : List ( Name.Name, Type.Type ) -> List ( Name.Name, Type.Type ) -> Maybe (List ( Name.Name, Name.Name ))
diffFields oldRawFields newRawFields =
    if List.length oldRawFields /= List.length newRawFields then
        Nothing

    else
        let
            sort : List ( comparable, b ) -> List ( comparable, b )
            sort fields =
                List.sortBy Tuple.first fields

            oldFields : List ( Name.Name, Type.Type )
            oldFields =
                sort oldRawFields

            newFields : List ( Name.Name, Type.Type )
            newFields =
                sort newRawFields
        in
        if List.any identity (List.map2 (/=) (List.map Tuple.first oldFields) (List.map Tuple.first newFields)) then
            Nothing

        else
            Maybe.map List.concat (Utils.zipWithM diffType (List.map Tuple.second oldFields) (List.map Tuple.second newFields))



-- TYPE VARIABLES


isEquivalentRenaming : List ( Name.Name, Name.Name ) -> Bool
isEquivalentRenaming varPairs =
    let
        renamings : List ( Name.Name, List Name.Name )
        renamings =
            Dict.toList compare (List.foldr insert Dict.empty varPairs)

        insert : ( Name.Name, Name.Name ) -> Dict String Name.Name (List Name.Name) -> Dict String Name.Name (List Name.Name)
        insert ( old, new ) dict =
            Utils.mapInsertWith identity (++) old [ new ] dict

        verify : ( a, List b ) -> Maybe ( a, b )
        verify ( old, news ) =
            case news of
                [] ->
                    Nothing

                new :: rest ->
                    if List.all ((==) new) rest then
                        Just ( old, new )

                    else
                        Nothing

        allUnique : List comparable -> Bool
        allUnique list =
            List.length list == EverySet.size (EverySet.fromList identity list)
    in
    case Utils.maybeMapM verify renamings of
        Nothing ->
            False

        Just verifiedRenamings ->
            List.all compatibleVars verifiedRenamings
                && allUnique (List.map Tuple.second verifiedRenamings)


compatibleVars : ( Name.Name, Name.Name ) -> Bool
compatibleVars ( old, new ) =
    case ( categorizeVar old, categorizeVar new ) of
        ( CompAppend, CompAppend ) ->
            True

        ( Comparable, Comparable ) ->
            True

        ( Appendable, Appendable ) ->
            True

        ( Number, Number ) ->
            True

        ( Number, Comparable ) ->
            True

        ( _, Var ) ->
            True

        _ ->
            False


type TypeVarCategory
    = CompAppend
    | Comparable
    | Appendable
    | Number
    | Var


categorizeVar : Name.Name -> TypeVarCategory
categorizeVar name =
    if Name.isCompappendType name then
        CompAppend

    else if Name.isComparableType name then
        Comparable

    else if Name.isAppendableType name then
        Appendable

    else if Name.isNumberType name then
        Number

    else
        Var



-- MAGNITUDE


bump : PackageChanges -> Version -> Version
bump changes version =
    case toMagnitude changes of
        M.PATCH ->
            V.bumpPatch version

        M.MINOR ->
            V.bumpMinor version

        M.MAJOR ->
            V.bumpMajor version


toMagnitude : PackageChanges -> M.Magnitude
toMagnitude (PackageChanges added changed removed) =
    let
        addMag : M.Magnitude
        addMag =
            if List.isEmpty added then
                M.PATCH

            else
                M.MINOR

        removeMag : M.Magnitude
        removeMag =
            if List.isEmpty removed then
                M.PATCH

            else
                M.MAJOR

        changeMags : List M.Magnitude
        changeMags =
            List.map moduleChangeMagnitude (Dict.values compare changed)
    in
    Utils.listMaximum M.compare (addMag :: removeMag :: changeMags)


moduleChangeMagnitude : ModuleChanges -> M.Magnitude
moduleChangeMagnitude (ModuleChanges unions aliases values binops) =
    Utils.listMaximum M.compare
        [ changeMagnitude unions
        , changeMagnitude aliases
        , changeMagnitude values
        , changeMagnitude binops
        ]


changeMagnitude : Changes comparable k v -> M.Magnitude
changeMagnitude (Changes added changed removed) =
    if Dict.size removed > 0 || Dict.size changed > 0 then
        M.MAJOR

    else if Dict.size added > 0 then
        M.MINOR

    else
        M.PATCH



-- GET DOCS


getDocs : Stuff.PackageCache -> Http.Manager -> Pkg.Name -> V.Version -> Task Never (Result Exit.DocsProblem Docs.Documentation)
getDocs cache manager name version =
    let
        home : String
        home =
            Stuff.package cache name version

        path : String
        path =
            home ++ "/docs.json"
    in
    File.exists path
        |> Task.bind
            (\exists ->
                if exists then
                    File.readUtf8 path
                        |> Task.bind
                            (\bytes ->
                                case D.fromByteString Docs.decoder bytes of
                                    Ok docs ->
                                        Task.pure (Ok docs)

                                    Err _ ->
                                        File.remove path
                                            |> Task.fmap (\_ -> Err DP_Cache)
                            )

                else
                    Website.metadata name version "docs.json"
                        |> Task.bind
                            (\url ->
                                Http.get manager url [] Exit.DP_Http <|
                                    \body ->
                                        case D.fromByteString Docs.decoder body of
                                            Ok docs ->
                                                Utils.dirCreateDirectoryIfMissing True home
                                                    |> Task.bind (\_ -> File.writeUtf8 path body)
                                                    |> Task.fmap (\_ -> Ok docs)

                                            Err _ ->
                                                Task.pure (Err (DP_Data url body))
                            )
            )
