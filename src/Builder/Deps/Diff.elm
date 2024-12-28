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
import Compiler.Elm.Docs as Docs
import Compiler.Elm.Magnitude as M
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet
import List
import System.IO as IO
import Types as T exposing (IO)
import Utils.Main as Utils


type PackageChanges
    = PackageChanges (List T.CEMN_Raw) (Dict String T.CEMN_Raw ModuleChanges) (List T.CEMN_Raw)


type ModuleChanges
    = ModuleChanges (Changes String T.CDN_Name T.CED_Union) (Changes String T.CDN_Name T.CED_Alias) (Changes String T.CDN_Name T.CED_Value) (Changes String T.CDN_Name T.CED_Binop)


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


diff : T.CED_Documentation -> T.CED_Documentation -> PackageChanges
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


diffModule : ( T.CED_Module, T.CED_Module ) -> ModuleChanges
diffModule ( T.CED_Module _ _ u1 a1 v1 b1, T.CED_Module _ _ u2 a2 v2 b2 ) =
    ModuleChanges
        (getChanges identity compare isEquivalentUnion u1 u2)
        (getChanges identity compare isEquivalentAlias a1 a2)
        (getChanges identity compare isEquivalentValue v1 v2)
        (getChanges identity compare isEquivalentBinop b1 b2)



-- EQUIVALENCE


isEquivalentUnion : T.CED_Union -> T.CED_Union -> Bool
isEquivalentUnion (T.CED_Union oldComment oldVars oldCtors) (T.CED_Union newComment newVars newCtors) =
    let
        equiv : List T.CECT_Type -> List T.CECT_Type -> Bool
        equiv oldTypes newTypes =
            let
                allEquivalent : List Bool
                allEquivalent =
                    List.map2
                        isEquivalentAlias
                        (List.map (T.CED_Alias oldComment oldVars) oldTypes)
                        (List.map (T.CED_Alias newComment newVars) newTypes)
            in
            (List.length oldTypes == List.length newTypes)
                && List.all identity allEquivalent
    in
    (List.length oldCtors == List.length newCtors)
        && List.all identity (List.map2 (==) (List.map Tuple.first oldCtors) (List.map Tuple.first newCtors))
        && List.all identity (Dict.values compare (Utils.mapIntersectionWith identity compare equiv (Dict.fromList identity oldCtors) (Dict.fromList identity newCtors)))


isEquivalentAlias : T.CED_Alias -> T.CED_Alias -> Bool
isEquivalentAlias (T.CED_Alias _ oldVars oldType) (T.CED_Alias _ newVars newType) =
    case diffType oldType newType of
        Nothing ->
            False

        Just renamings ->
            (List.length oldVars == List.length newVars)
                && isEquivalentRenaming (List.map2 Tuple.pair oldVars newVars ++ renamings)


isEquivalentValue : T.CED_Value -> T.CED_Value -> Bool
isEquivalentValue (T.CED_Value c1 t1) (T.CED_Value c2 t2) =
    isEquivalentAlias (T.CED_Alias c1 [] t1) (T.CED_Alias c2 [] t2)


isEquivalentBinop : T.CED_Binop -> T.CED_Binop -> Bool
isEquivalentBinop (T.CED_Binop c1 t1 a1 p1) (T.CED_Binop c2 t2 a2 p2) =
    isEquivalentAlias (T.CED_Alias c1 [] t1) (T.CED_Alias c2 [] t2)
        && (a1 == a2)
        && (p1 == p2)



-- DIFF TYPES


diffType : T.CECT_Type -> T.CECT_Type -> Maybe (List ( T.CDN_Name, T.CDN_Name ))
diffType oldType newType =
    case ( oldType, newType ) of
        ( T.CECT_Var oldName, T.CECT_Var newName ) ->
            Just [ ( oldName, newName ) ]

        ( T.CECT_Lambda a b, T.CECT_Lambda a_ b_ ) ->
            Maybe.map2 (++) (diffType a a_) (diffType b b_)

        ( T.CECT_Type oldName oldArgs, T.CECT_Type newName newArgs ) ->
            if not (isSameName oldName newName) || List.length oldArgs /= List.length newArgs then
                Nothing

            else
                Maybe.map List.concat (Utils.zipWithM diffType oldArgs newArgs)

        ( T.CECT_Record fields maybeExt, T.CECT_Record fields_ maybeExt_ ) ->
            case ( maybeExt, maybeExt_ ) of
                ( Nothing, Just _ ) ->
                    Nothing

                ( Just _, Nothing ) ->
                    Nothing

                ( Nothing, Nothing ) ->
                    diffFields fields fields_

                ( Just oldExt, Just newExt ) ->
                    Maybe.map ((::) ( oldExt, newExt )) (diffFields fields fields_)

        ( T.CECT_Unit, T.CECT_Unit ) ->
            Just []

        ( T.CECT_Tuple a b cs, T.CECT_Tuple x y zs ) ->
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


isSameName : T.CDN_Name -> T.CDN_Name -> Bool
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


diffFields : List ( T.CDN_Name, T.CECT_Type ) -> List ( T.CDN_Name, T.CECT_Type ) -> Maybe (List ( T.CDN_Name, T.CDN_Name ))
diffFields oldRawFields newRawFields =
    if List.length oldRawFields /= List.length newRawFields then
        Nothing

    else
        let
            sort : List ( comparable, b ) -> List ( comparable, b )
            sort fields =
                List.sortBy Tuple.first fields

            oldFields : List ( T.CDN_Name, T.CECT_Type )
            oldFields =
                sort oldRawFields

            newFields : List ( T.CDN_Name, T.CECT_Type )
            newFields =
                sort newRawFields
        in
        if List.any identity (List.map2 (/=) (List.map Tuple.first oldFields) (List.map Tuple.first newFields)) then
            Nothing

        else
            Maybe.map List.concat (Utils.zipWithM diffType (List.map Tuple.second oldFields) (List.map Tuple.second newFields))



-- TYPE VARIABLES


isEquivalentRenaming : List ( T.CDN_Name, T.CDN_Name ) -> Bool
isEquivalentRenaming varPairs =
    let
        renamings : List ( T.CDN_Name, List T.CDN_Name )
        renamings =
            Dict.toList compare (List.foldr insert Dict.empty varPairs)

        insert : ( T.CDN_Name, T.CDN_Name ) -> Dict String T.CDN_Name (List T.CDN_Name) -> Dict String T.CDN_Name (List T.CDN_Name)
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


compatibleVars : ( T.CDN_Name, T.CDN_Name ) -> Bool
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


categorizeVar : T.CDN_Name -> TypeVarCategory
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


bump : PackageChanges -> T.CEV_Version -> T.CEV_Version
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


getDocs : T.BS_PackageCache -> T.BH_Manager -> T.CEP_Name -> T.CEV_Version -> IO (Result Exit.DocsProblem T.CED_Documentation)
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
        |> IO.bind
            (\exists ->
                if exists then
                    File.readUtf8 path
                        |> IO.bind
                            (\bytes ->
                                case D.fromByteString Docs.decoder bytes of
                                    Ok docs ->
                                        IO.pure (Ok docs)

                                    Err _ ->
                                        File.remove path
                                            |> IO.fmap (\_ -> Err DP_Cache)
                            )

                else
                    let
                        url : String
                        url =
                            Website.metadata name version "docs.json"
                    in
                    Http.get manager url [] Exit.DP_Http <|
                        \body ->
                            case D.fromByteString Docs.decoder body of
                                Ok docs ->
                                    Utils.dirCreateDirectoryIfMissing True home
                                        |> IO.bind (\_ -> File.writeUtf8 path body)
                                        |> IO.fmap (\_ -> Ok docs)

                                Err _ ->
                                    IO.pure (Err (DP_Data url body))
            )
