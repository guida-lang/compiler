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
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Data.Map as Dict exposing (Dict)
import Prelude
import System.TypeCheck.IO as IO
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



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
    | Tuple Type Type (List Type)
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

        Tuple a b cs ->
            RT.tuple
                (toDoc localizer RT.None a)
                (toDoc localizer RT.None b)
                (List.map (toDoc localizer RT.None) cs)

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

        ( Tuple a b cs, Tuple x y zs ) as pair ->
            toDiffTuple localizer ctx pair ( a, b, cs ) ( x, y, zs ) (pureDiff [])

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


toDiffTuple : L.Localizer -> RT.Context -> ( Type, Type ) -> ( Type, Type, List Type ) -> ( Type, Type, List Type ) -> Diff (List D.Doc) -> Diff D.Doc
toDiffTuple localizer ctx pair ( a, b, cs ) ( x, y, zs ) diffCs =
    case ( cs, zs ) of
        ( [], [] ) ->
            toDiff localizer RT.None a x
                |> fmapDiff RT.tuple
                |> applyDiff (toDiff localizer RT.None b y)
                |> applyDiff diffCs

        ( c :: restCs, z :: restZs ) ->
            fmapDiff (::) (toDiff localizer RT.None c z)
                |> applyDiff diffCs
                |> toDiffTuple localizer ctx pair ( a, b, restCs ) ( x, y, restZs )

        _ ->
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

        Tuple a b cs ->
            case super of
                Number ->
                    False

                Comparable ->
                    List.all (isSuper super) (a :: b :: cs)

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


typeEncoder : Type -> BE.Encoder
typeEncoder type_ =
    case type_ of
        Lambda x y zs ->
            BE.sequence
                [ BE.unsignedInt8 0
                , typeEncoder x
                , typeEncoder y
                , BE.list typeEncoder zs
                ]

        Infinite ->
            BE.unsignedInt8 1

        Error ->
            BE.unsignedInt8 2

        FlexVar name ->
            BE.sequence
                [ BE.unsignedInt8 3
                , BE.string name
                ]

        FlexSuper s x ->
            BE.sequence
                [ BE.unsignedInt8 4
                , superEncoder s
                , BE.string x
                ]

        RigidVar name ->
            BE.sequence
                [ BE.unsignedInt8 5
                , BE.string name
                ]

        RigidSuper s x ->
            BE.sequence
                [ BE.unsignedInt8 6
                , superEncoder s
                , BE.string x
                ]

        Type home name args ->
            BE.sequence
                [ BE.unsignedInt8 7
                , ModuleName.canonicalEncoder home
                , BE.string name
                , BE.list typeEncoder args
                ]

        Record msgType decoder ->
            BE.sequence
                [ BE.unsignedInt8 8
                , BE.assocListDict compare BE.string typeEncoder msgType
                , extensionEncoder decoder
                ]

        Unit ->
            BE.unsignedInt8 9

        Tuple a b cs ->
            BE.sequence
                [ BE.unsignedInt8 10
                , typeEncoder a
                , typeEncoder b
                , BE.list typeEncoder cs
                ]

        Alias home name args tipe ->
            BE.sequence
                [ BE.unsignedInt8 11
                , ModuleName.canonicalEncoder home
                , BE.string name
                , BE.list (BE.jsonPair BE.string typeEncoder) args
                , typeEncoder tipe
                ]


typeDecoder : BD.Decoder Type
typeDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map3 Lambda
                            typeDecoder
                            typeDecoder
                            (BD.list typeDecoder)

                    1 ->
                        BD.succeed Infinite

                    2 ->
                        BD.succeed Error

                    3 ->
                        BD.map FlexVar BD.string

                    4 ->
                        BD.map2 FlexSuper
                            superDecoder
                            BD.string

                    5 ->
                        BD.map RigidVar BD.string

                    6 ->
                        BD.map2 RigidSuper
                            superDecoder
                            BD.string

                    7 ->
                        BD.map3 Type
                            ModuleName.canonicalDecoder
                            BD.string
                            (BD.list typeDecoder)

                    8 ->
                        BD.map2 Record
                            (BD.assocListDict identity BD.string typeDecoder)
                            extensionDecoder

                    9 ->
                        BD.succeed Unit

                    10 ->
                        BD.map3 Tuple
                            typeDecoder
                            typeDecoder
                            (BD.list typeDecoder)

                    11 ->
                        BD.map4 Alias
                            ModuleName.canonicalDecoder
                            BD.string
                            (BD.list (BD.jsonPair BD.string typeDecoder))
                            typeDecoder

                    _ ->
                        BD.fail
            )


superEncoder : Super -> BE.Encoder
superEncoder super =
    BE.unsignedInt8
        (case super of
            Number ->
                0

            Comparable ->
                1

            Appendable ->
                2

            CompAppend ->
                3
        )


superDecoder : BD.Decoder Super
superDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Number

                    1 ->
                        BD.succeed Comparable

                    2 ->
                        BD.succeed Appendable

                    3 ->
                        BD.succeed CompAppend

                    _ ->
                        BD.fail
            )


extensionEncoder : Extension -> BE.Encoder
extensionEncoder extension =
    case extension of
        Closed ->
            BE.unsignedInt8 0

        FlexOpen x ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.string x
                ]

        RigidOpen x ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string x
                ]


extensionDecoder : BD.Decoder Extension
extensionDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Closed

                    1 ->
                        BD.map FlexOpen BD.string

                    2 ->
                        BD.map RigidOpen BD.string

                    _ ->
                        BD.fail
            )
