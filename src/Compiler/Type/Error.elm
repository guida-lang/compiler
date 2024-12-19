module Compiler.Type.Error exposing
    ( Direction(..)
    , Problem(..)
    , isChar
    , isFloat
    , isInt
    , isList
    , isString
    , iteratedDealias
    , toComparison
    , toDoc
    , typeDecoder
    , typeEncoder
    )

import Compiler.Data.Bag as Bag
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as DecodeX
import Compiler.Json.Encode as EncodeX
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Prelude
import Types as T


iteratedDealias : T.CTE_Type -> T.CTE_Type
iteratedDealias tipe =
    case tipe of
        T.CTE_Alias _ _ _ real ->
            iteratedDealias real

        _ ->
            tipe



-- TO DOC


toDoc : T.CRRTL_Localizer -> RT.Context -> T.CTE_Type -> D.Doc
toDoc localizer ctx tipe =
    case tipe of
        T.CTE_Lambda a b cs ->
            RT.lambda ctx
                (toDoc localizer RT.Func a)
                (toDoc localizer RT.Func b)
                (List.map (toDoc localizer RT.Func) cs)

        T.CTE_Infinite ->
            D.fromChars "∞"

        T.CTE_Error ->
            D.fromChars "?"

        T.CTE_FlexVar name ->
            D.fromName name

        T.CTE_FlexSuper _ name ->
            D.fromName name

        T.CTE_RigidVar name ->
            D.fromName name

        T.CTE_RigidSuper _ name ->
            D.fromName name

        T.CTE_Type home name args ->
            RT.apply ctx
                (L.toDoc localizer home name)
                (List.map (toDoc localizer RT.App) args)

        T.CTE_Record fields ext ->
            RT.record (fieldsToDocs localizer fields) (extToDoc ext)

        T.CTE_Unit ->
            D.fromChars "()"

        T.CTE_Tuple a b maybeC ->
            RT.tuple
                (toDoc localizer RT.None a)
                (toDoc localizer RT.None b)
                (List.map (toDoc localizer RT.None) (Maybe.toList maybeC))

        T.CTE_Alias home name args _ ->
            aliasToDoc localizer ctx home name args


aliasToDoc : T.CRRTL_Localizer -> RT.Context -> T.CEMN_Canonical -> T.CDN_Name -> List ( T.CDN_Name, T.CTE_Type ) -> D.Doc
aliasToDoc localizer ctx home name args =
    RT.apply ctx
        (L.toDoc localizer home name)
        (List.map (toDoc localizer RT.App << Tuple.second) args)


fieldsToDocs : T.CRRTL_Localizer -> Dict String T.CDN_Name T.CTE_Type -> List ( D.Doc, D.Doc )
fieldsToDocs localizer fields =
    Dict.foldr compare (addField localizer) [] fields


addField : T.CRRTL_Localizer -> T.CDN_Name -> T.CTE_Type -> List ( D.Doc, D.Doc ) -> List ( D.Doc, D.Doc )
addField localizer fieldName fieldType docs =
    let
        f : D.Doc
        f =
            D.fromName fieldName

        t : D.Doc
        t =
            toDoc localizer RT.None fieldType
    in
    ( f, t ) :: docs


extToDoc : T.CTE_Extension -> Maybe D.Doc
extToDoc ext =
    case ext of
        T.CTE_Closed ->
            Nothing

        T.CTE_FlexOpen x ->
            Just (D.fromName x)

        T.CTE_RigidOpen x ->
            Just (D.fromName x)



-- DIFF


type Diff a
    = Diff a a Status


type Status
    = Similar
    | Different (Bag.Bag Problem)


type Problem
    = IntFloat
    | StringFromInt
    | StringFromFloat
    | StringToInt
    | StringToFloat
    | AnythingToBool
    | AnythingFromMaybe
    | ArityMismatch Int Int
    | BadFlexSuper Direction T.CTE_Super T.CTE_Type
    | BadRigidVar T.CDN_Name T.CTE_Type
    | BadRigidSuper T.CTE_Super T.CDN_Name T.CTE_Type
    | FieldTypo T.CDN_Name (List T.CDN_Name)
    | FieldsMissing (List T.CDN_Name)


type Direction
    = Have
    | Need


fmapDiff : (a -> b) -> Diff a -> Diff b
fmapDiff func (Diff a b status) =
    Diff (func a) (func b) status


pureDiff : a -> Diff a
pureDiff a =
    Diff a a Similar


applyDiff : Diff a -> Diff (a -> b) -> Diff b
applyDiff (Diff aArg bArg status2) (Diff aFunc bFunc status1) =
    Diff (aFunc aArg) (bFunc bArg) (merge status1 status2)


liftA2 : (a -> b -> c) -> Diff a -> Diff b -> Diff c
liftA2 f x y =
    applyDiff y (fmapDiff f x)


merge : Status -> Status -> Status
merge status1 status2 =
    case status1 of
        Similar ->
            status2

        Different problems1 ->
            case status2 of
                Similar ->
                    status1

                Different problems2 ->
                    Different (Bag.append problems1 problems2)



-- COMPARISON


toComparison : T.CRRTL_Localizer -> T.CTE_Type -> T.CTE_Type -> ( D.Doc, D.Doc, List Problem )
toComparison localizer tipe1 tipe2 =
    case toDiff localizer RT.None tipe1 tipe2 of
        Diff doc1 doc2 Similar ->
            ( doc1, doc2, [] )

        Diff doc1 doc2 (Different problems) ->
            ( doc1, doc2, Bag.toList problems )


toDiff : T.CRRTL_Localizer -> RT.Context -> T.CTE_Type -> T.CTE_Type -> Diff D.Doc
toDiff localizer ctx tipe1 tipe2 =
    case ( tipe1, tipe2 ) of
        ( T.CTE_Unit, T.CTE_Unit ) ->
            same localizer ctx tipe1

        ( T.CTE_Error, T.CTE_Error ) ->
            same localizer ctx tipe1

        ( T.CTE_Infinite, T.CTE_Infinite ) ->
            same localizer ctx tipe1

        ( T.CTE_FlexVar x, T.CTE_FlexVar y ) ->
            if x == y then
                same localizer ctx tipe1

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( T.CTE_FlexSuper _ x, T.CTE_FlexSuper _ y ) ->
            if x == y then
                same localizer ctx tipe1

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( T.CTE_RigidVar x, T.CTE_RigidVar y ) ->
            if x == y then
                same localizer ctx tipe1

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( T.CTE_RigidSuper _ x, T.CTE_RigidSuper _ y ) ->
            if x == y then
                same localizer ctx tipe1

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( T.CTE_FlexVar _, _ ) ->
            similar localizer ctx tipe1 tipe2

        ( _, T.CTE_FlexVar _ ) ->
            similar localizer ctx tipe1 tipe2

        ( T.CTE_FlexSuper s _, t ) ->
            if isSuper s t then
                similar localizer ctx tipe1 tipe2

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( t, T.CTE_FlexSuper s _ ) ->
            if isSuper s t then
                similar localizer ctx tipe1 tipe2

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( T.CTE_Lambda a b cs, T.CTE_Lambda x y zs ) ->
            if List.length cs == List.length zs then
                toDiff localizer RT.Func a x
                    |> fmapDiff (RT.lambda ctx)
                    |> applyDiff (toDiff localizer RT.Func b y)
                    |> applyDiff
                        (List.map2 (toDiff localizer RT.Func) cs zs
                            |> List.foldr (liftA2 (::)) (pureDiff [])
                        )

            else
                let
                    f : T.CTE_Type -> D.Doc
                    f =
                        toDoc localizer RT.Func
                in
                different
                    (D.dullyellow (RT.lambda ctx (f a) (f b) (List.map f cs)))
                    (D.dullyellow (RT.lambda ctx (f x) (f y) (List.map f zs)))
                    (Bag.one (ArityMismatch (2 + List.length cs) (2 + List.length zs)))

        ( T.CTE_Tuple a b Nothing, T.CTE_Tuple x y Nothing ) ->
            toDiff localizer RT.None a x
                |> fmapDiff RT.tuple
                |> applyDiff (toDiff localizer RT.None b y)
                |> applyDiff (Diff [] [] Similar)

        ( T.CTE_Tuple a b (Just c), T.CTE_Tuple x y (Just z) ) ->
            toDiff localizer RT.None a x
                |> fmapDiff RT.tuple
                |> applyDiff (toDiff localizer RT.None b y)
                |> applyDiff (fmapDiff List.singleton (toDiff localizer RT.None c z))

        ( T.CTE_Record fields1 ext1, T.CTE_Record fields2 ext2 ) ->
            diffRecord localizer fields1 ext1 fields2 ext2

        ( T.CTE_Type home1 name1 args1, T.CTE_Type home2 name2 args2 ) ->
            if home1 == home2 && name1 == name2 then
                List.map2 (toDiff localizer RT.App) args1 args2
                    |> List.foldr (liftA2 (::)) (pureDiff [])
                    |> fmapDiff (RT.apply ctx (L.toDoc localizer home1 name1))

            else if L.toChars localizer home1 name1 == L.toChars localizer home2 name2 then
                -- start trying to find specific problems (this used to be down on the list)
                different
                    (nameClashToDoc ctx localizer home1 name1 args1)
                    (nameClashToDoc ctx localizer home2 name2 args2)
                    Bag.empty

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( T.CTE_Alias home1 name1 args1 _, T.CTE_Alias home2 name2 args2 _ ) ->
            if home1 == home2 && name1 == name2 then
                List.map2 (toDiff localizer RT.App) (List.map Tuple.second args1) (List.map Tuple.second args2)
                    |> List.foldr (liftA2 (::)) (pureDiff [])
                    |> fmapDiff (RT.apply ctx (L.toDoc localizer home1 name1))

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        -- start trying to find specific problems (moved first check above)
        ( T.CTE_Type home name [ t1 ], t2 ) ->
            if isMaybe home name && isSimilar (toDiff localizer ctx t1 t2) then
                different
                    (RT.apply ctx (D.dullyellow (L.toDoc localizer home name)) [ toDoc localizer RT.App t1 ])
                    (toDoc localizer ctx t2)
                    (Bag.one AnythingFromMaybe)

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( t1, T.CTE_Type home name [ t2 ] ) ->
            if isList home name && isSimilar (toDiff localizer ctx t1 t2) then
                different
                    (toDoc localizer ctx t1)
                    (RT.apply ctx (D.dullyellow (L.toDoc localizer home name)) [ toDoc localizer RT.App t2 ])
                    Bag.empty

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( T.CTE_Alias home1 name1 args1 t1, t2 ) ->
            case diffAliasedRecord localizer t1 t2 of
                Just (Diff _ doc2 status) ->
                    Diff (D.dullyellow (aliasToDoc localizer ctx home1 name1 args1)) doc2 status

                Nothing ->
                    case tipe2 of
                        T.CTE_Type home2 name2 args2 ->
                            if L.toChars localizer home1 name1 == L.toChars localizer home2 name2 then
                                different
                                    (nameClashToDoc ctx localizer home1 name1 (List.map Tuple.second args1))
                                    (nameClashToDoc ctx localizer home2 name2 args2)
                                    Bag.empty

                            else
                                different
                                    (D.dullyellow (toDoc localizer ctx tipe1))
                                    (D.dullyellow (toDoc localizer ctx tipe2))
                                    Bag.empty

                        _ ->
                            different
                                (D.dullyellow (toDoc localizer ctx tipe1))
                                (D.dullyellow (toDoc localizer ctx tipe2))
                                Bag.empty

        ( _, T.CTE_Alias home2 name2 args2 _ ) ->
            case diffAliasedRecord localizer tipe1 tipe2 of
                Just (Diff doc1 _ status) ->
                    Diff doc1 (D.dullyellow (aliasToDoc localizer ctx home2 name2 args2)) status

                Nothing ->
                    case tipe1 of
                        T.CTE_Type home1 name1 args1 ->
                            if L.toChars localizer home1 name1 == L.toChars localizer home2 name2 then
                                different
                                    (nameClashToDoc ctx localizer home1 name1 args1)
                                    (nameClashToDoc ctx localizer home2 name2 (List.map Tuple.second args2))
                                    Bag.empty

                            else
                                different
                                    (D.dullyellow (toDoc localizer ctx tipe1))
                                    (D.dullyellow (toDoc localizer ctx tipe2))
                                    Bag.empty

                        _ ->
                            different
                                (D.dullyellow (toDoc localizer ctx tipe1))
                                (D.dullyellow (toDoc localizer ctx tipe2))
                                Bag.empty

        pair ->
            toDiffOtherwise localizer ctx pair


toDiffOtherwise : T.CRRTL_Localizer -> RT.Context -> ( T.CTE_Type, T.CTE_Type ) -> Diff D.Doc
toDiffOtherwise localizer ctx (( tipe1, tipe2 ) as pair) =
    let
        doc1 : D.Doc
        doc1 =
            D.dullyellow (toDoc localizer ctx tipe1)

        doc2 : D.Doc
        doc2 =
            D.dullyellow (toDoc localizer ctx tipe2)
    in
    different doc1 doc2 <|
        case pair of
            ( T.CTE_RigidVar x, other ) ->
                Bag.one <| BadRigidVar x other

            ( T.CTE_FlexSuper s _, other ) ->
                Bag.one <| BadFlexSuper Have s other

            ( T.CTE_RigidSuper s x, other ) ->
                Bag.one <| BadRigidSuper s x other

            ( other, T.CTE_RigidVar x ) ->
                Bag.one <| BadRigidVar x other

            ( other, T.CTE_FlexSuper s _ ) ->
                Bag.one <| BadFlexSuper Need s other

            ( other, T.CTE_RigidSuper s x ) ->
                Bag.one <| BadRigidSuper s x other

            ( T.CTE_Type home1 name1 [], T.CTE_Type home2 name2 [] ) ->
                if isInt home1 name1 && isFloat home2 name2 then
                    Bag.one <| IntFloat

                else if isFloat home1 name1 && isInt home2 name2 then
                    Bag.one <| IntFloat

                else if isInt home1 name1 && isString home2 name2 then
                    Bag.one <| StringFromInt

                else if isFloat home1 name1 && isString home2 name2 then
                    Bag.one <| StringFromFloat

                else if isString home1 name1 && isInt home2 name2 then
                    Bag.one <| StringToInt

                else if isString home1 name1 && isFloat home2 name2 then
                    Bag.one <| StringToFloat

                else if isBool home2 name2 then
                    Bag.one <| AnythingToBool

                else
                    Bag.empty

            _ ->
                Bag.empty



-- DIFF HELPERS


same : T.CRRTL_Localizer -> RT.Context -> T.CTE_Type -> Diff D.Doc
same localizer ctx tipe =
    let
        doc : D.Doc
        doc =
            toDoc localizer ctx tipe
    in
    Diff doc doc Similar


similar : T.CRRTL_Localizer -> RT.Context -> T.CTE_Type -> T.CTE_Type -> Diff D.Doc
similar localizer ctx t1 t2 =
    Diff (toDoc localizer ctx t1) (toDoc localizer ctx t2) Similar


different : a -> a -> Bag.Bag Problem -> Diff a
different a b problems =
    Diff a b (Different problems)


isSimilar : Diff a -> Bool
isSimilar (Diff _ _ status) =
    case status of
        Similar ->
            True

        Different _ ->
            False



-- IS TYPE?


isBool : T.CEMN_Canonical -> T.CDN_Name -> Bool
isBool home name =
    home == ModuleName.basics && name == Name.bool


isInt : T.CEMN_Canonical -> T.CDN_Name -> Bool
isInt home name =
    home == ModuleName.basics && name == Name.int


isFloat : T.CEMN_Canonical -> T.CDN_Name -> Bool
isFloat home name =
    home == ModuleName.basics && name == Name.float


isString : T.CEMN_Canonical -> T.CDN_Name -> Bool
isString home name =
    home == ModuleName.string && name == Name.string


isChar : T.CEMN_Canonical -> T.CDN_Name -> Bool
isChar home name =
    home == ModuleName.char && name == Name.char


isMaybe : T.CEMN_Canonical -> T.CDN_Name -> Bool
isMaybe home name =
    home == ModuleName.maybe && name == Name.maybe


isList : T.CEMN_Canonical -> T.CDN_Name -> Bool
isList home name =
    home == ModuleName.list && name == Name.list



-- IS SUPER?


isSuper : T.CTE_Super -> T.CTE_Type -> Bool
isSuper super tipe =
    case iteratedDealias tipe of
        T.CTE_Type h n args ->
            case super of
                T.CTE_Number ->
                    isInt h n || isFloat h n

                T.CTE_Comparable ->
                    isInt h n || isFloat h n || isString h n || isChar h n || isList h n && isSuper super (Prelude.head args)

                T.CTE_Appendable ->
                    isString h n || isList h n

                T.CTE_CompAppend ->
                    isString h n || isList h n && isSuper T.CTE_Comparable (Prelude.head args)

        T.CTE_Tuple a b maybeC ->
            case super of
                T.CTE_Number ->
                    False

                T.CTE_Comparable ->
                    isSuper super a && isSuper super b && Maybe.withDefault True (Maybe.map (isSuper super) maybeC)

                T.CTE_Appendable ->
                    False

                T.CTE_CompAppend ->
                    False

        _ ->
            False



-- NAME CLASH


nameClashToDoc : RT.Context -> T.CRRTL_Localizer -> T.CEMN_Canonical -> T.CDN_Name -> List T.CTE_Type -> D.Doc
nameClashToDoc ctx localizer (T.CEMN_Canonical _ home) name args =
    RT.apply ctx
        (D.yellow (D.fromName home) |> D.a (D.dullyellow (D.fromChars "." |> D.a (D.fromName name))))
        (List.map (toDoc localizer RT.App) args)



-- DIFF ALIASED RECORD


diffAliasedRecord : T.CRRTL_Localizer -> T.CTE_Type -> T.CTE_Type -> Maybe (Diff D.Doc)
diffAliasedRecord localizer t1 t2 =
    case ( iteratedDealias t1, iteratedDealias t2 ) of
        ( T.CTE_Record fields1 ext1, T.CTE_Record fields2 ext2 ) ->
            Just (diffRecord localizer fields1 ext1 fields2 ext2)

        _ ->
            Nothing



-- RECORD DIFFS


diffRecord : T.CRRTL_Localizer -> Dict String T.CDN_Name T.CTE_Type -> T.CTE_Extension -> Dict String T.CDN_Name T.CTE_Type -> T.CTE_Extension -> Diff D.Doc
diffRecord localizer fields1 ext1 fields2 ext2 =
    let
        toUnknownDocs : T.CDN_Name -> T.CTE_Type -> ( D.Doc, D.Doc )
        toUnknownDocs field tipe =
            ( D.dullyellow (D.fromName field), toDoc localizer RT.None tipe )

        toOverlapDocs : T.CDN_Name -> T.CTE_Type -> T.CTE_Type -> Diff ( D.Doc, D.Doc )
        toOverlapDocs field t1 t2 =
            fmapDiff (Tuple.pair (D.fromName field)) <| toDiff localizer RT.None t1 t2

        left : Dict String T.CDN_Name ( D.Doc, D.Doc )
        left =
            Dict.map toUnknownDocs (Dict.diff fields1 fields2)

        right : Dict String T.CDN_Name ( D.Doc, D.Doc )
        right =
            Dict.map toUnknownDocs (Dict.diff fields2 fields1)

        fieldsDiff : Diff (List ( D.Doc, D.Doc ))
        fieldsDiff =
            let
                fieldsDiffDict : Diff (Dict String T.CDN_Name ( D.Doc, D.Doc ))
                fieldsDiffDict =
                    let
                        both : Dict String T.CDN_Name (Diff ( D.Doc, D.Doc ))
                        both =
                            Dict.merge compare
                                (\_ _ acc -> acc)
                                (\field t1 t2 acc -> Dict.insert identity field (toOverlapDocs field t1 t2) acc)
                                (\_ _ acc -> acc)
                                fields1
                                fields2
                                Dict.empty

                        sequenceA : Dict String T.CDN_Name (Diff ( D.Doc, D.Doc )) -> Diff (Dict String T.CDN_Name ( D.Doc, D.Doc ))
                        sequenceA =
                            Dict.foldr compare (\k x acc -> applyDiff acc (fmapDiff (Dict.insert identity k) x)) (pureDiff Dict.empty)
                    in
                    if Dict.isEmpty left && Dict.isEmpty right then
                        sequenceA both

                    else
                        liftA2 Dict.union
                            (sequenceA both)
                            (Diff left right (Different Bag.empty))
            in
            fmapDiff (Dict.values compare) fieldsDiffDict

        (Diff doc1 doc2 status) =
            fieldsDiff
                |> fmapDiff RT.record
                |> applyDiff (extToDiff ext1 ext2)
    in
    Diff doc1 doc2 <|
        merge status <|
            case ( hasFixedFields ext1, hasFixedFields ext2 ) of
                ( True, True ) ->
                    let
                        minView : Maybe ( T.CDN_Name, ( D.Doc, D.Doc ) )
                        minView =
                            Dict.toList compare left
                                |> List.sortBy Tuple.first
                                |> List.head
                    in
                    case minView of
                        Just ( f, _ ) ->
                            Different (Bag.one (FieldTypo f (Dict.keys compare fields2)))

                        Nothing ->
                            if Dict.isEmpty right then
                                Similar

                            else
                                Different (Bag.one (FieldsMissing (Dict.keys compare right)))

                ( False, True ) ->
                    let
                        minView : Maybe ( T.CDN_Name, ( D.Doc, D.Doc ) )
                        minView =
                            Dict.toList compare left
                                |> List.sortBy Tuple.first
                                |> List.head
                    in
                    case minView of
                        Just ( f, _ ) ->
                            Different (Bag.one (FieldTypo f (Dict.keys compare fields2)))

                        Nothing ->
                            Similar

                ( True, False ) ->
                    let
                        minView : Maybe ( T.CDN_Name, ( D.Doc, D.Doc ) )
                        minView =
                            Dict.toList compare right
                                |> List.sortBy Tuple.first
                                |> List.head
                    in
                    case minView of
                        Just ( f, _ ) ->
                            Different (Bag.one (FieldTypo f (Dict.keys compare fields1)))

                        Nothing ->
                            Similar

                ( False, False ) ->
                    Similar


hasFixedFields : T.CTE_Extension -> Bool
hasFixedFields ext =
    case ext of
        T.CTE_Closed ->
            True

        T.CTE_FlexOpen _ ->
            False

        T.CTE_RigidOpen _ ->
            True



-- DIFF RECORD EXTENSION


extToDiff : T.CTE_Extension -> T.CTE_Extension -> Diff (Maybe D.Doc)
extToDiff ext1 ext2 =
    let
        status : Status
        status =
            extToStatus ext1 ext2

        extDoc1 : Maybe D.Doc
        extDoc1 =
            extToDoc ext1

        extDoc2 : Maybe D.Doc
        extDoc2 =
            extToDoc ext2
    in
    case status of
        Similar ->
            Diff extDoc1 extDoc2 status

        Different _ ->
            Diff (Maybe.map D.dullyellow extDoc1) (Maybe.map D.dullyellow extDoc2) status


extToStatus : T.CTE_Extension -> T.CTE_Extension -> Status
extToStatus ext1 ext2 =
    case ext1 of
        T.CTE_Closed ->
            case ext2 of
                T.CTE_Closed ->
                    Similar

                T.CTE_FlexOpen _ ->
                    Similar

                T.CTE_RigidOpen _ ->
                    Different Bag.empty

        T.CTE_FlexOpen _ ->
            Similar

        T.CTE_RigidOpen x ->
            case ext2 of
                T.CTE_Closed ->
                    Different Bag.empty

                T.CTE_FlexOpen _ ->
                    Similar

                T.CTE_RigidOpen y ->
                    if x == y then
                        Similar

                    else
                        Different (Bag.one (BadRigidVar x (T.CTE_RigidVar y)))



-- ENCODERS and DECODERS


typeEncoder : T.CTE_Type -> Encode.Value
typeEncoder type_ =
    case type_ of
        T.CTE_Lambda x y zs ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "x", typeEncoder x )
                , ( "y", typeEncoder y )
                , ( "zs", Encode.list typeEncoder zs )
                ]

        T.CTE_Infinite ->
            Encode.object
                [ ( "type", Encode.string "Infinite" )
                ]

        T.CTE_Error ->
            Encode.object
                [ ( "type", Encode.string "Error" )
                ]

        T.CTE_FlexVar name ->
            Encode.object
                [ ( "type", Encode.string "FlexVar" )
                , ( "name", Encode.string name )
                ]

        T.CTE_FlexSuper s x ->
            Encode.object
                [ ( "type", Encode.string "FlexSuper" )
                , ( "s", superEncoder s )
                , ( "x", Encode.string x )
                ]

        T.CTE_RigidVar name ->
            Encode.object
                [ ( "type", Encode.string "RigidVar" )
                , ( "name", Encode.string name )
                ]

        T.CTE_RigidSuper s x ->
            Encode.object
                [ ( "type", Encode.string "RigidSuper" )
                , ( "s", superEncoder s )
                , ( "x", Encode.string x )
                ]

        T.CTE_Type home name args ->
            Encode.object
                [ ( "type", Encode.string "Type" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        T.CTE_Record msgType decoder ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "msgType", EncodeX.assocListDict compare Encode.string typeEncoder msgType )
                , ( "decoder", extensionEncoder decoder )
                ]

        T.CTE_Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        T.CTE_Tuple a b maybeC ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                , ( "maybeC", EncodeX.maybe typeEncoder maybeC )
                ]

        T.CTE_Alias home name args tipe ->
            Encode.object
                [ ( "type", Encode.string "Alias" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list (EncodeX.jsonPair Encode.string typeEncoder) args )
                , ( "tipe", typeEncoder tipe )
                ]


typeDecoder : Decode.Decoder T.CTE_Type
typeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Lambda" ->
                        Decode.map3 T.CTE_Lambda
                            (Decode.field "x" typeDecoder)
                            (Decode.field "y" typeDecoder)
                            (Decode.field "zs" (Decode.list typeDecoder))

                    "Infinite" ->
                        Decode.succeed T.CTE_Infinite

                    "Error" ->
                        Decode.succeed T.CTE_Error

                    "FlexVar" ->
                        Decode.map T.CTE_FlexVar (Decode.field "name" Decode.string)

                    "FlexSuper" ->
                        Decode.map2 T.CTE_FlexSuper
                            (Decode.field "s" superDecoder)
                            (Decode.field "x" Decode.string)

                    "RigidVar" ->
                        Decode.map T.CTE_RigidVar (Decode.field "name" Decode.string)

                    "RigidSuper" ->
                        Decode.map2 T.CTE_RigidSuper
                            (Decode.field "s" superDecoder)
                            (Decode.field "x" Decode.string)

                    "Type" ->
                        Decode.map3 T.CTE_Type
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "Record" ->
                        Decode.map2 T.CTE_Record
                            (Decode.field "msgType" (DecodeX.assocListDict identity Decode.string typeDecoder))
                            (Decode.field "decoder" extensionDecoder)

                    "Unit" ->
                        Decode.succeed T.CTE_Unit

                    "Tuple" ->
                        Decode.map3 T.CTE_Tuple
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)
                            (Decode.field "maybeC" (Decode.maybe typeDecoder))

                    "Alias" ->
                        Decode.map4 T.CTE_Alias
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list (DecodeX.jsonPair Decode.string typeDecoder)))
                            (Decode.field "tipe" typeDecoder)

                    _ ->
                        Decode.fail ("Unknown Type's type: " ++ type_)
            )


superEncoder : T.CTE_Super -> Encode.Value
superEncoder super =
    case super of
        T.CTE_Number ->
            Encode.string "Number"

        T.CTE_Comparable ->
            Encode.string "Comparable"

        T.CTE_Appendable ->
            Encode.string "Appendable"

        T.CTE_CompAppend ->
            Encode.string "CompAppend"


superDecoder : Decode.Decoder T.CTE_Super
superDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Number" ->
                        Decode.succeed T.CTE_Number

                    "Comparable" ->
                        Decode.succeed T.CTE_Comparable

                    "Appendable" ->
                        Decode.succeed T.CTE_Appendable

                    "CompAppend" ->
                        Decode.succeed T.CTE_CompAppend

                    _ ->
                        Decode.fail ("Unknown Super: " ++ str)
            )


extensionEncoder : T.CTE_Extension -> Encode.Value
extensionEncoder extension =
    case extension of
        T.CTE_Closed ->
            Encode.object
                [ ( "type", Encode.string "Closed" )
                ]

        T.CTE_FlexOpen x ->
            Encode.object
                [ ( "type", Encode.string "FlexOpen" )
                , ( "x", Encode.string x )
                ]

        T.CTE_RigidOpen x ->
            Encode.object
                [ ( "type", Encode.string "RigidOpen" )
                , ( "x", Encode.string x )
                ]


extensionDecoder : Decode.Decoder T.CTE_Extension
extensionDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Closed" ->
                        Decode.succeed T.CTE_Closed

                    "FlexOpen" ->
                        Decode.map T.CTE_FlexOpen (Decode.field "x" Decode.string)

                    "RigidOpen" ->
                        Decode.map T.CTE_RigidOpen (Decode.field "x" Decode.string)

                    _ ->
                        Decode.fail ("Unknown Extension's type: " ++ type_)
            )
