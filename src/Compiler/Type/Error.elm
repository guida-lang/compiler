module Compiler.Type.Error exposing
    ( Direction(..)
    , Extension(..)
    , Problem(..)
    , Super(..)
    , Type(..)
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
import Compiler.Data.Name as Name exposing (Name)
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
import System.TypeCheck.IO as IO



-- ERROR TYPES


type Type
    = Lambda Type Type (List Type)
    | Infinite
    | Error
    | FlexVar Name
    | FlexSuper Super Name
    | RigidVar Name
    | RigidSuper Super Name
    | Type IO.Canonical Name (List Type)
    | Record (Dict String Name Type) Extension
    | Unit
    | Tuple Type Type (Maybe Type)
    | Alias IO.Canonical Name (List ( Name, Type )) Type


type Super
    = Number
    | Comparable
    | Appendable
    | CompAppend


type Extension
    = Closed
    | FlexOpen Name
    | RigidOpen Name


iteratedDealias : Type -> Type
iteratedDealias tipe =
    case tipe of
        Alias _ _ _ real ->
            iteratedDealias real

        _ ->
            tipe



-- TO DOC


toDoc : L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer ctx tipe =
    case tipe of
        Lambda a b cs ->
            RT.lambda ctx
                (toDoc localizer RT.Func a)
                (toDoc localizer RT.Func b)
                (List.map (toDoc localizer RT.Func) cs)

        Infinite ->
            D.fromChars "∞"

        Error ->
            D.fromChars "?"

        FlexVar name ->
            D.fromName name

        FlexSuper _ name ->
            D.fromName name

        RigidVar name ->
            D.fromName name

        RigidSuper _ name ->
            D.fromName name

        Type home name args ->
            RT.apply ctx
                (L.toDoc localizer home name)
                (List.map (toDoc localizer RT.App) args)

        Record fields ext ->
            RT.record (fieldsToDocs localizer fields) (extToDoc ext)

        Unit ->
            D.fromChars "()"

        Tuple a b maybeC ->
            RT.tuple
                (toDoc localizer RT.None a)
                (toDoc localizer RT.None b)
                (List.map (toDoc localizer RT.None) (Maybe.toList maybeC))

        Alias home name args _ ->
            aliasToDoc localizer ctx home name args


aliasToDoc : L.Localizer -> RT.Context -> IO.Canonical -> Name -> List ( Name, Type ) -> D.Doc
aliasToDoc localizer ctx home name args =
    RT.apply ctx
        (L.toDoc localizer home name)
        (List.map (toDoc localizer RT.App << Tuple.second) args)


fieldsToDocs : L.Localizer -> Dict String Name Type -> List ( D.Doc, D.Doc )
fieldsToDocs localizer fields =
    Dict.foldr compare (addField localizer) [] fields


addField : L.Localizer -> Name -> Type -> List ( D.Doc, D.Doc ) -> List ( D.Doc, D.Doc )
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


extToDoc : Extension -> Maybe D.Doc
extToDoc ext =
    case ext of
        Closed ->
            Nothing

        FlexOpen x ->
            Just (D.fromName x)

        RigidOpen x ->
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
    | BadFlexSuper Direction Super Type
    | BadRigidVar Name Type
    | BadRigidSuper Super Name Type
    | FieldTypo Name (List Name)
    | FieldsMissing (List Name)


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


toComparison : L.Localizer -> Type -> Type -> ( D.Doc, D.Doc, List Problem )
toComparison localizer tipe1 tipe2 =
    case toDiff localizer RT.None tipe1 tipe2 of
        Diff doc1 doc2 Similar ->
            ( doc1, doc2, [] )

        Diff doc1 doc2 (Different problems) ->
            ( doc1, doc2, Bag.toList problems )


toDiff : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff localizer ctx tipe1 tipe2 =
    case ( tipe1, tipe2 ) of
        ( Unit, Unit ) ->
            same localizer ctx tipe1

        ( Error, Error ) ->
            same localizer ctx tipe1

        ( Infinite, Infinite ) ->
            same localizer ctx tipe1

        ( FlexVar x, FlexVar y ) ->
            if x == y then
                same localizer ctx tipe1

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( FlexSuper _ x, FlexSuper _ y ) ->
            if x == y then
                same localizer ctx tipe1

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( RigidVar x, RigidVar y ) ->
            if x == y then
                same localizer ctx tipe1

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( RigidSuper _ x, RigidSuper _ y ) ->
            if x == y then
                same localizer ctx tipe1

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( FlexVar _, _ ) ->
            similar localizer ctx tipe1 tipe2

        ( _, FlexVar _ ) ->
            similar localizer ctx tipe1 tipe2

        ( FlexSuper s _, t ) ->
            if isSuper s t then
                similar localizer ctx tipe1 tipe2

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( t, FlexSuper s _ ) ->
            if isSuper s t then
                similar localizer ctx tipe1 tipe2

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( Lambda a b cs, Lambda x y zs ) ->
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
                    f : Type -> D.Doc
                    f =
                        toDoc localizer RT.Func
                in
                different
                    (D.dullyellow (RT.lambda ctx (f a) (f b) (List.map f cs)))
                    (D.dullyellow (RT.lambda ctx (f x) (f y) (List.map f zs)))
                    (Bag.one (ArityMismatch (2 + List.length cs) (2 + List.length zs)))

        ( Tuple a b Nothing, Tuple x y Nothing ) ->
            toDiff localizer RT.None a x
                |> fmapDiff RT.tuple
                |> applyDiff (toDiff localizer RT.None b y)
                |> applyDiff (Diff [] [] Similar)

        ( Tuple a b (Just c), Tuple x y (Just z) ) ->
            toDiff localizer RT.None a x
                |> fmapDiff RT.tuple
                |> applyDiff (toDiff localizer RT.None b y)
                |> applyDiff (fmapDiff List.singleton (toDiff localizer RT.None c z))

        ( Record fields1 ext1, Record fields2 ext2 ) ->
            diffRecord localizer fields1 ext1 fields2 ext2

        ( Type home1 name1 args1, Type home2 name2 args2 ) ->
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

        ( Alias home1 name1 args1 _, Alias home2 name2 args2 _ ) ->
            if home1 == home2 && name1 == name2 then
                List.map2 (toDiff localizer RT.App) (List.map Tuple.second args1) (List.map Tuple.second args2)
                    |> List.foldr (liftA2 (::)) (pureDiff [])
                    |> fmapDiff (RT.apply ctx (L.toDoc localizer home1 name1))

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        -- start trying to find specific problems (moved first check above)
        ( Type home name [ t1 ], t2 ) ->
            if isMaybe home name && isSimilar (toDiff localizer ctx t1 t2) then
                different
                    (RT.apply ctx (D.dullyellow (L.toDoc localizer home name)) [ toDoc localizer RT.App t1 ])
                    (toDoc localizer ctx t2)
                    (Bag.one AnythingFromMaybe)

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( t1, Type home name [ t2 ] ) ->
            if isList home name && isSimilar (toDiff localizer ctx t1 t2) then
                different
                    (toDoc localizer ctx t1)
                    (RT.apply ctx (D.dullyellow (L.toDoc localizer home name)) [ toDoc localizer RT.App t2 ])
                    Bag.empty

            else
                toDiffOtherwise localizer ctx ( tipe1, tipe2 )

        ( Alias home1 name1 args1 t1, t2 ) ->
            case diffAliasedRecord localizer t1 t2 of
                Just (Diff _ doc2 status) ->
                    Diff (D.dullyellow (aliasToDoc localizer ctx home1 name1 args1)) doc2 status

                Nothing ->
                    case tipe2 of
                        Type home2 name2 args2 ->
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

        ( _, Alias home2 name2 args2 _ ) ->
            case diffAliasedRecord localizer tipe1 tipe2 of
                Just (Diff doc1 _ status) ->
                    Diff doc1 (D.dullyellow (aliasToDoc localizer ctx home2 name2 args2)) status

                Nothing ->
                    case tipe1 of
                        Type home1 name1 args1 ->
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


toDiffOtherwise : L.Localizer -> RT.Context -> ( Type, Type ) -> Diff D.Doc
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
            ( RigidVar x, other ) ->
                Bag.one <| BadRigidVar x other

            ( FlexSuper s _, other ) ->
                Bag.one <| BadFlexSuper Have s other

            ( RigidSuper s x, other ) ->
                Bag.one <| BadRigidSuper s x other

            ( other, RigidVar x ) ->
                Bag.one <| BadRigidVar x other

            ( other, FlexSuper s _ ) ->
                Bag.one <| BadFlexSuper Need s other

            ( other, RigidSuper s x ) ->
                Bag.one <| BadRigidSuper s x other

            ( Type home1 name1 [], Type home2 name2 [] ) ->
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


same : L.Localizer -> RT.Context -> Type -> Diff D.Doc
same localizer ctx tipe =
    let
        doc : D.Doc
        doc =
            toDoc localizer ctx tipe
    in
    Diff doc doc Similar


similar : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
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


isBool : IO.Canonical -> Name -> Bool
isBool home name =
    home == ModuleName.basics && name == Name.bool


isInt : IO.Canonical -> Name -> Bool
isInt home name =
    home == ModuleName.basics && name == Name.int


isFloat : IO.Canonical -> Name -> Bool
isFloat home name =
    home == ModuleName.basics && name == Name.float


isString : IO.Canonical -> Name -> Bool
isString home name =
    home == ModuleName.string && name == Name.string


isChar : IO.Canonical -> Name -> Bool
isChar home name =
    home == ModuleName.char && name == Name.char


isMaybe : IO.Canonical -> Name -> Bool
isMaybe home name =
    home == ModuleName.maybe && name == Name.maybe


isList : IO.Canonical -> Name -> Bool
isList home name =
    home == ModuleName.list && name == Name.list



-- IS SUPER?


isSuper : Super -> Type -> Bool
isSuper super tipe =
    case iteratedDealias tipe of
        Type h n args ->
            case super of
                Number ->
                    isInt h n || isFloat h n

                Comparable ->
                    isInt h n || isFloat h n || isString h n || isChar h n || isList h n && isSuper super (Prelude.head args)

                Appendable ->
                    isString h n || isList h n

                CompAppend ->
                    isString h n || isList h n && isSuper Comparable (Prelude.head args)

        Tuple a b maybeC ->
            case super of
                Number ->
                    False

                Comparable ->
                    isSuper super a && isSuper super b && Maybe.withDefault True (Maybe.map (isSuper super) maybeC)

                Appendable ->
                    False

                CompAppend ->
                    False

        _ ->
            False



-- NAME CLASH


nameClashToDoc : RT.Context -> L.Localizer -> IO.Canonical -> Name -> List Type -> D.Doc
nameClashToDoc ctx localizer (IO.Canonical _ home) name args =
    RT.apply ctx
        (D.yellow (D.fromName home) |> D.a (D.dullyellow (D.fromChars "." |> D.a (D.fromName name))))
        (List.map (toDoc localizer RT.App) args)



-- DIFF ALIASED RECORD


diffAliasedRecord : L.Localizer -> Type -> Type -> Maybe (Diff D.Doc)
diffAliasedRecord localizer t1 t2 =
    case ( iteratedDealias t1, iteratedDealias t2 ) of
        ( Record fields1 ext1, Record fields2 ext2 ) ->
            Just (diffRecord localizer fields1 ext1 fields2 ext2)

        _ ->
            Nothing



-- RECORD DIFFS


diffRecord : L.Localizer -> Dict String Name Type -> Extension -> Dict String Name Type -> Extension -> Diff D.Doc
diffRecord localizer fields1 ext1 fields2 ext2 =
    let
        toUnknownDocs : Name -> Type -> ( D.Doc, D.Doc )
        toUnknownDocs field tipe =
            ( D.dullyellow (D.fromName field), toDoc localizer RT.None tipe )

        toOverlapDocs : Name -> Type -> Type -> Diff ( D.Doc, D.Doc )
        toOverlapDocs field t1 t2 =
            fmapDiff (Tuple.pair (D.fromName field)) <| toDiff localizer RT.None t1 t2

        left : Dict String Name ( D.Doc, D.Doc )
        left =
            Dict.map toUnknownDocs (Dict.diff fields1 fields2)

        right : Dict String Name ( D.Doc, D.Doc )
        right =
            Dict.map toUnknownDocs (Dict.diff fields2 fields1)

        fieldsDiff : Diff (List ( D.Doc, D.Doc ))
        fieldsDiff =
            let
                fieldsDiffDict : Diff (Dict String Name ( D.Doc, D.Doc ))
                fieldsDiffDict =
                    let
                        both : Dict String Name (Diff ( D.Doc, D.Doc ))
                        both =
                            Dict.merge compare
                                (\_ _ acc -> acc)
                                (\field t1 t2 acc -> Dict.insert identity field (toOverlapDocs field t1 t2) acc)
                                (\_ _ acc -> acc)
                                fields1
                                fields2
                                Dict.empty

                        sequenceA : Dict String Name (Diff ( D.Doc, D.Doc )) -> Diff (Dict String Name ( D.Doc, D.Doc ))
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
                        minView : Maybe ( Name, ( D.Doc, D.Doc ) )
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
                        minView : Maybe ( Name, ( D.Doc, D.Doc ) )
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
                        minView : Maybe ( Name, ( D.Doc, D.Doc ) )
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


hasFixedFields : Extension -> Bool
hasFixedFields ext =
    case ext of
        Closed ->
            True

        FlexOpen _ ->
            False

        RigidOpen _ ->
            True



-- DIFF RECORD EXTENSION


extToDiff : Extension -> Extension -> Diff (Maybe D.Doc)
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


extToStatus : Extension -> Extension -> Status
extToStatus ext1 ext2 =
    case ext1 of
        Closed ->
            case ext2 of
                Closed ->
                    Similar

                FlexOpen _ ->
                    Similar

                RigidOpen _ ->
                    Different Bag.empty

        FlexOpen _ ->
            Similar

        RigidOpen x ->
            case ext2 of
                Closed ->
                    Different Bag.empty

                FlexOpen _ ->
                    Similar

                RigidOpen y ->
                    if x == y then
                        Similar

                    else
                        Different (Bag.one (BadRigidVar x (RigidVar y)))



-- ENCODERS and DECODERS


typeEncoder : Type -> Encode.Value
typeEncoder type_ =
    case type_ of
        Lambda x y zs ->
            Encode.object
                [ ( "type", Encode.string "Lambda" )
                , ( "x", typeEncoder x )
                , ( "y", typeEncoder y )
                , ( "zs", Encode.list typeEncoder zs )
                ]

        Infinite ->
            Encode.object
                [ ( "type", Encode.string "Infinite" )
                ]

        Error ->
            Encode.object
                [ ( "type", Encode.string "Error" )
                ]

        FlexVar name ->
            Encode.object
                [ ( "type", Encode.string "FlexVar" )
                , ( "name", Encode.string name )
                ]

        FlexSuper s x ->
            Encode.object
                [ ( "type", Encode.string "FlexSuper" )
                , ( "s", superEncoder s )
                , ( "x", Encode.string x )
                ]

        RigidVar name ->
            Encode.object
                [ ( "type", Encode.string "RigidVar" )
                , ( "name", Encode.string name )
                ]

        RigidSuper s x ->
            Encode.object
                [ ( "type", Encode.string "RigidSuper" )
                , ( "s", superEncoder s )
                , ( "x", Encode.string x )
                ]

        Type home name args ->
            Encode.object
                [ ( "type", Encode.string "Type" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list typeEncoder args )
                ]

        Record msgType decoder ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "msgType", EncodeX.assocListDict compare Encode.string typeEncoder msgType )
                , ( "decoder", extensionEncoder decoder )
                ]

        Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        Tuple a b maybeC ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", typeEncoder a )
                , ( "b", typeEncoder b )
                , ( "maybeC", EncodeX.maybe typeEncoder maybeC )
                ]

        Alias home name args tipe ->
            Encode.object
                [ ( "type", Encode.string "Alias" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                , ( "args", Encode.list (EncodeX.jsonPair Encode.string typeEncoder) args )
                , ( "tipe", typeEncoder tipe )
                ]


typeDecoder : Decode.Decoder Type
typeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Lambda" ->
                        Decode.map3 Lambda
                            (Decode.field "x" typeDecoder)
                            (Decode.field "y" typeDecoder)
                            (Decode.field "zs" (Decode.list typeDecoder))

                    "Infinite" ->
                        Decode.succeed Infinite

                    "Error" ->
                        Decode.succeed Error

                    "FlexVar" ->
                        Decode.map FlexVar (Decode.field "name" Decode.string)

                    "FlexSuper" ->
                        Decode.map2 FlexSuper
                            (Decode.field "s" superDecoder)
                            (Decode.field "x" Decode.string)

                    "RigidVar" ->
                        Decode.map RigidVar (Decode.field "name" Decode.string)

                    "RigidSuper" ->
                        Decode.map2 RigidSuper
                            (Decode.field "s" superDecoder)
                            (Decode.field "x" Decode.string)

                    "Type" ->
                        Decode.map3 Type
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list typeDecoder))

                    "Record" ->
                        Decode.map2 Record
                            (Decode.field "msgType" (DecodeX.assocListDict identity Decode.string typeDecoder))
                            (Decode.field "decoder" extensionDecoder)

                    "Unit" ->
                        Decode.succeed Unit

                    "Tuple" ->
                        Decode.map3 Tuple
                            (Decode.field "a" typeDecoder)
                            (Decode.field "b" typeDecoder)
                            (Decode.field "maybeC" (Decode.maybe typeDecoder))

                    "Alias" ->
                        Decode.map4 Alias
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list (DecodeX.jsonPair Decode.string typeDecoder)))
                            (Decode.field "tipe" typeDecoder)

                    _ ->
                        Decode.fail ("Unknown Type's type: " ++ type_)
            )


superEncoder : Super -> Encode.Value
superEncoder super =
    case super of
        Number ->
            Encode.string "Number"

        Comparable ->
            Encode.string "Comparable"

        Appendable ->
            Encode.string "Appendable"

        CompAppend ->
            Encode.string "CompAppend"


superDecoder : Decode.Decoder Super
superDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Number" ->
                        Decode.succeed Number

                    "Comparable" ->
                        Decode.succeed Comparable

                    "Appendable" ->
                        Decode.succeed Appendable

                    "CompAppend" ->
                        Decode.succeed CompAppend

                    _ ->
                        Decode.fail ("Unknown Super: " ++ str)
            )


extensionEncoder : Extension -> Encode.Value
extensionEncoder extension =
    case extension of
        Closed ->
            Encode.object
                [ ( "type", Encode.string "Closed" )
                ]

        FlexOpen x ->
            Encode.object
                [ ( "type", Encode.string "FlexOpen" )
                , ( "x", Encode.string x )
                ]

        RigidOpen x ->
            Encode.object
                [ ( "type", Encode.string "RigidOpen" )
                , ( "x", Encode.string x )
                ]


extensionDecoder : Decode.Decoder Extension
extensionDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Closed" ->
                        Decode.succeed Closed

                    "FlexOpen" ->
                        Decode.map FlexOpen (Decode.field "x" Decode.string)

                    "RigidOpen" ->
                        Decode.map RigidOpen (Decode.field "x" Decode.string)

                    _ ->
                        Decode.fail ("Unknown Extension's type: " ++ type_)
            )
