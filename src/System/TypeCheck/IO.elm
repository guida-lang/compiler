module System.TypeCheck.IO exposing
    ( unsafePerformIO
    , IO, State, pure, apply, fmap, bind, foldrM, foldM, traverseMap, traverseMapWithKey, forM_, mapM_
    , foldMDict, indexedForA, mapM, traverseIndexed, traverseList, traverseMaybe, traverseTuple
    , Step(..), loop
    , Point(..), PointInfo(..)
    , Descriptor(..), Content(..), SuperType(..), Mark(..), Variable, FlatType(..)
    , Canonical(..)
    )

{-| Ref.: <https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html>

@docs unsafePerformIO


# The IO monad

@docs IO, State, pure, apply, fmap, bind, foldrM, foldM, traverseMap, traverseMapWithKey, forM_, mapM_
@docs foldMDict, indexedForA, mapM, traverseIndexed, traverseList, traverseMaybe, traverseTuple


# Loop

@docs Step, loop


# Point

@docs Point, PointInfo


# Compiler.Type.Type

@docs Descriptor, Content, SuperType, Mark, Variable, FlatType


# Compiler.Elm.ModuleName

@docs Canonical

-}

import Array exposing (Array)
import Compiler.Data.Index as Index
import Data.Map as Dict exposing (Dict)


unsafePerformIO : IO a -> a
unsafePerformIO ioA =
    { ioRefsWeight = Array.empty
    , ioRefsPointInfo = Array.empty
    , ioRefsDescriptor = Array.empty
    , ioRefsMVector = Array.empty
    }
        |> ioA
        |> Tuple.second



-- LOOP


type Step state a
    = Loop state
    | Done a


loop : (state -> IO (Step state a)) -> state -> IO a
loop callback loopState ioState =
    case callback loopState ioState of
        ( newIOState, Loop newLoopState ) ->
            loop callback newLoopState newIOState

        ( newIOState, Done a ) ->
            ( newIOState, a )



-- The IO monad


type alias IO a =
    State -> ( State, a )


type alias State =
    { ioRefsWeight : Array Int
    , ioRefsPointInfo : Array PointInfo
    , ioRefsDescriptor : Array Descriptor
    , ioRefsMVector : Array (Array (Maybe (List Variable)))
    }


pure : a -> IO a
pure x =
    \s -> ( s, x )


apply : IO a -> IO (a -> b) -> IO b
apply ma mf =
    bind (\f -> bind (pure << f) ma) mf


fmap : (a -> b) -> IO a -> IO b
fmap fn ma s0 =
    let
        ( s1, a ) =
            ma s0
    in
    ( s1, fn a )


bind : (a -> IO b) -> IO a -> IO b
bind f ma =
    \s0 ->
        let
            ( s1, a ) =
                ma s0
        in
        f a s1


foldrM : (a -> b -> IO b) -> b -> List a -> IO b
foldrM f z0 xs =
    loop (foldrMHelp f) ( xs, z0 )


foldrMHelp : (a -> b -> IO b) -> ( List a, b ) -> IO (Step ( List a, b ) b)
foldrMHelp callback ( list, result ) =
    case list of
        [] ->
            pure (Done result)

        a :: rest ->
            fmap (\b -> Loop ( rest, b )) (callback a result)


foldM : (b -> a -> IO b) -> b -> List a -> IO b
foldM f b list =
    loop (foldMHelp f) ( List.reverse list, b )


foldMHelp : (b -> a -> IO b) -> ( List a, b ) -> IO (Step ( List a, b ) b)
foldMHelp callback ( list, result ) =
    case list of
        [] ->
            pure (Done result)

        a :: rest ->
            fmap (\b -> Loop ( rest, b )) (callback result a)


traverseMap : (k -> comparable) -> (k -> k -> Order) -> (a -> IO b) -> Dict comparable k a -> IO (Dict comparable k b)
traverseMap toComparable keyComparison f =
    traverseMapWithKey toComparable keyComparison (\_ -> f)


traverseMapWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> IO b) -> Dict comparable k a -> IO (Dict comparable k b)
traverseMapWithKey toComparable keyComparison f dict =
    loop (traverseWithKeyHelp toComparable f) ( Dict.toList keyComparison dict, Dict.empty )


traverseWithKeyHelp : (k -> comparable) -> (k -> a -> IO b) -> ( List ( k, a ), Dict comparable k b ) -> IO (Step ( List ( k, a ), Dict comparable k b ) (Dict comparable k b))
traverseWithKeyHelp toComparable callback ( pairs, result ) =
    case pairs of
        [] ->
            pure (Done result)

        ( k, a ) :: rest ->
            fmap (\b -> Loop ( rest, Dict.insert toComparable k b result )) (callback k a)


mapM_ : (a -> IO b) -> List a -> IO ()
mapM_ f list =
    loop (mapMHelp_ f) ( List.reverse list, pure () )


mapMHelp_ : (a -> IO b) -> ( List a, IO () ) -> IO (Step ( List a, IO () ) ())
mapMHelp_ callback ( list, result ) =
    case list of
        [] ->
            fmap Done result

        a :: rest ->
            fmap (\_ -> Loop ( rest, result )) (callback a)


forM_ : List a -> (a -> IO b) -> IO ()
forM_ list f =
    mapM_ f list


foldMDict : (k -> k -> Order) -> (b -> a -> IO b) -> b -> Dict c k a -> IO b
foldMDict keyComparison f b =
    Dict.foldl keyComparison (\_ a -> bind (\acc -> f acc a)) (pure b)


traverseList : (a -> IO b) -> List a -> IO (List b)
traverseList f =
    List.foldr (\a -> bind (\c -> fmap (\va -> va :: c) (f a)))
        (pure [])


traverseTuple : (b -> IO c) -> ( a, b ) -> IO ( a, c )
traverseTuple f ( a, b ) =
    fmap (Tuple.pair a) (f b)


traverseMaybe : (a -> IO b) -> Maybe a -> IO (Maybe b)
traverseMaybe f a =
    case Maybe.map f a of
        Just b ->
            fmap Just b

        Nothing ->
            pure Nothing


mapM : (a -> IO b) -> List a -> IO (List b)
mapM =
    traverseList


traverseIndexed : (Index.ZeroBased -> a -> IO b) -> List a -> IO (List b)
traverseIndexed func xs =
    sequenceAList (Index.indexedMap func xs)


indexedForA : List a -> (Index.ZeroBased -> a -> IO b) -> IO (List b)
indexedForA xs func =
    sequenceAList (Index.indexedMap func xs)


sequenceAList : List (IO a) -> IO (List a)
sequenceAList =
    List.foldr (\x acc -> apply acc (fmap (::) x)) (pure [])



-- POINT


{-| FIXME Compiler.Type.UnionFind
-}
type Point
    = Pt Int


{-| FIXME Compiler.Type.UnionFind
-}
type PointInfo
    = Info Int Int
    | Link Point



-- DESCRIPTORS


{-| FIXME Compiler.Type.Type
-}
type Descriptor
    = Descriptor Content Int Mark (Maybe Variable)


{-| FIXME Compiler.Type.Type
-}
type Content
    = FlexVar (Maybe String)
    | FlexSuper SuperType (Maybe String)
    | RigidVar String
    | RigidSuper SuperType String
    | Structure FlatType
    | Alias Canonical String (List ( String, Variable )) Variable
    | Error


{-| FIXME Compiler.Type.Type
-}
type SuperType
    = Number
    | Comparable
    | Appendable
    | CompAppend



-- MARKS


{-| FIXME Compiler.Type.Type
-}
type Mark
    = Mark Int



-- TYPE PRIMITIVES


{-| FIXME Compiler.Type.Type
-}
type alias Variable =
    Point


{-| FIXME Compiler.Type.Type
-}
type FlatType
    = App1 Canonical String (List Variable)
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Dict String String Variable) Variable
    | Unit1
    | Tuple1 Variable Variable (List Variable)



-- CANONICAL


{-| FIXME Compiler.Elm.ModuleName
-}
type Canonical
    = Canonical ( String, String ) String
