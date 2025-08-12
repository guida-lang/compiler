module Utils.Main exposing
    ( AsyncException(..)
    , ChItem
    , Chan
    , FilePath
    , HttpExceptionContent(..)
    , HttpResponse(..)
    , HttpResponseHeaders
    , HttpStatus(..)
    , LockSharedExclusive(..)
    , MVar(..)
    , ReplCompletion(..)
    , ReplCompletionFunc
    , ReplInputT
    , ReplSettings(..)
    , SomeException(..)
    , ThreadId
    , ZipArchive(..)
    , ZipEntry(..)
    , binaryDecodeFileOrFail
    , binaryEncodeFile
    , bracket_
    , builderHPutBuilder
    , dictMapM_
    , dirCanonicalizePath
    , dirCreateDirectoryIfMissing
    , dirDoesDirectoryExist
    , dirDoesFileExist
    , dirFindExecutable
    , dirGetAppUserDataDirectory
    , dirGetCurrentDirectory
    , dirGetModificationTime
    , dirListDirectory
    , dirRemoveDirectoryRecursive
    , dirRemoveFile
    , dirWithCurrentDirectory
    , eitherLefts
    , envGetArgs
    , envGetProgName
    , envLookupEnv
    , filterM
    , find
    , findMax
    , foldM
    , foldl1_
    , foldr1
    , forkIO
    , fpAddExtension
    , fpAddTrailingPathSeparator
    , fpCombine
    , fpDropExtension
    , fpDropFileName
    , fpIsRelative
    , fpJoinPath
    , fpMakeRelative
    , fpPathSeparator
    , fpSplitDirectories
    , fpSplitExtension
    , fpSplitFileName
    , fpTakeDirectory
    , fpTakeExtension
    , fpTakeFileName
    , httpExceptionContentDecoder
    , httpExceptionContentEncoder
    , httpHLocation
    , httpResponseHeaders
    , httpResponseStatus
    , httpStatusCode
    , indexedZipWithA
    , keysSet
    , liftIOInputT
    , liftInputT
    , lines
    , listGroupBy
    , listLookup
    , listMaximum
    , listTraverse
    , listTraverse_
    , lockWithFileLock
    , mVarDecoder
    , mVarEncoder
    , mapFindMin
    , mapFromKeys
    , mapFromListWith
    , mapInsertWith
    , mapIntersectionWith
    , mapIntersectionWithKey
    , mapLookupMin
    , mapM_
    , mapMapKeys
    , mapMapMaybe
    , mapMinViewWithKey
    , mapTraverse
    , mapTraverseResult
    , mapTraverseWithKey
    , mapTraverseWithKeyResult
    , mapUnionWith
    , mapUnions
    , mapUnionsWith
    , maybeEncoder
    , maybeMapM
    , maybeTraverseTask
    , newChan
    , newEmptyMVar
    , newMVar
    , nodeGetDirname
    , nodeMathRandom
    , nonEmptyListTraverse
    , putMVar
    , readChan
    , readMVar
    , replCompleteWord
    , replGetInputLine
    , replGetInputLineWithInitial
    , replRunInputT
    , replWithInterrupt
    , sequenceADict
    , sequenceDictMaybe
    , sequenceDictResult
    , sequenceDictResult_
    , sequenceListMaybe
    , sequenceNonemptyListResult
    , someExceptionDecoder
    , someExceptionEncoder
    , takeMVar
    , unlines
    , unzip3
    , writeChan
    , zipWithM
    )

import Basics.Extra exposing (flip)
import Compiler.Data.Index as Index
import Compiler.Data.NonEmptyList as NE
import Compiler.Reporting.Result as R
import Control.Monad.State.Strict as State
import Data.Map as Map exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Prelude
import Process
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Time
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Impure as Impure
import Utils.Task.Extra as Task


liftInputT : Task Never () -> ReplInputT ()
liftInputT =
    identity


liftIOInputT : Task Never a -> ReplInputT a
liftIOInputT =
    identity


fpDropFileName : FilePath -> FilePath
fpDropFileName path =
    case List.reverse (String.split "/" path) of
        _ :: tail ->
            List.reverse ("" :: tail)
                |> String.join "/"

        [] ->
            ""


{-| An alias for `</>`.

Combine two paths with a path separator. If the second path starts with a
path separator or a drive letter, then it returns the second.
The intention is that readFile `(dir </> file)` will access the same file
as `setCurrentDirectory dir; readFile file`.

-}
fpCombine : FilePath -> FilePath -> FilePath
fpCombine path1 path2 =
    if String.startsWith "/" path2 || String.startsWith path1 path2 then
        path2

    else
        path1 ++ "/" ++ path2


fpAddExtension : FilePath -> String -> FilePath
fpAddExtension path extension =
    if String.startsWith "." extension then
        path ++ extension

    else
        path ++ "." ++ extension


mapFromListWith : (k -> comparable) -> (a -> a -> a) -> List ( k, a ) -> Dict comparable k a
mapFromListWith toComparable f =
    List.foldl
        (\( k, a ) ->
            Map.update toComparable k (Maybe.map (flip f a))
        )
        Map.empty


maybeEncoder : (a -> BE.Encoder) -> Maybe a -> BE.Encoder
maybeEncoder =
    BE.maybe


eitherLefts : List (Result e a) -> List e
eitherLefts =
    List.filterMap
        (\res ->
            case res of
                Ok _ ->
                    Nothing

                Err e ->
                    Just e
        )


mapFromKeys : (k -> comparable) -> (k -> v) -> List k -> Dict comparable k v
mapFromKeys toComparable f =
    List.map (\k -> ( k, f k ))
        >> Map.fromList toComparable


filterM : (a -> Task Never Bool) -> List a -> Task Never (List a)
filterM p =
    List.foldr
        (\x acc ->
            Task.apply acc
                (Task.fmap
                    (\flg ->
                        if flg then
                            (::) x

                        else
                            identity
                    )
                    (p x)
                )
        )
        (Task.pure [])


find : (k -> comparable) -> k -> Dict comparable k a -> a
find toComparable k items =
    case Map.get toComparable k items of
        Just item ->
            item

        Nothing ->
            crash "Map.!: given key is not an element in the map"


findMax : (k -> k -> Order) -> Dict comparable k a -> ( k, a )
findMax keyComparison items =
    case List.reverse (Map.toList keyComparison items) of
        item :: _ ->
            item

        _ ->
            crash "Error: empty map has no maximal element"


mapLookupMin : Dict comparable comparable a -> Maybe ( comparable, a )
mapLookupMin dict =
    case List.sortBy Tuple.first (Map.toList compare dict) of
        firstElem :: _ ->
            Just firstElem

        _ ->
            Nothing


mapFindMin : Dict comparable comparable a -> ( comparable, a )
mapFindMin dict =
    case List.sortBy Tuple.first (Map.toList compare dict) of
        firstElem :: _ ->
            firstElem

        _ ->
            crash "Error: empty map has no minimal element"


mapInsertWith : (k -> comparable) -> (a -> a -> a) -> k -> a -> Dict comparable k a -> Dict comparable k a
mapInsertWith toComparable f k a =
    Map.update toComparable k (Maybe.map (f a) >> Maybe.withDefault a >> Just)


mapIntersectionWith : (k -> comparable) -> (k -> k -> Order) -> (a -> b -> c) -> Dict comparable k a -> Dict comparable k b -> Dict comparable k c
mapIntersectionWith toComparable keyComparison func =
    mapIntersectionWithKey toComparable keyComparison (\_ -> func)


mapIntersectionWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> b -> c) -> Dict comparable k a -> Dict comparable k b -> Dict comparable k c
mapIntersectionWithKey toComparable keyComparison func dict1 dict2 =
    Map.merge keyComparison (\_ _ -> identity) (\k v1 v2 -> Map.insert toComparable k (func k v1 v2)) (\_ _ -> identity) dict1 dict2 Map.empty


mapUnionWith : (k -> comparable) -> (k -> k -> Order) -> (a -> a -> a) -> Dict comparable k a -> Dict comparable k a -> Dict comparable k a
mapUnionWith toComparable keyComparison f a b =
    Map.merge keyComparison (Map.insert toComparable) (\k va vb -> Map.insert toComparable k (f va vb)) (Map.insert toComparable) a b Map.empty


mapUnionsWith : (k -> comparable) -> (k -> k -> Order) -> (a -> a -> a) -> List (Dict comparable k a) -> Dict comparable k a
mapUnionsWith toComparable keyComparison f =
    List.foldl (mapUnionWith toComparable keyComparison f) Map.empty


mapUnions : List (Dict comparable k a) -> Dict comparable k a
mapUnions =
    List.foldr Map.union Map.empty


foldM : (b -> a -> R.RResult info warnings error b) -> b -> List a -> R.RResult info warnings error b
foldM f b =
    List.foldl (\a -> R.bind (\acc -> f acc a)) (R.ok b)


indexedZipWithA : (Index.ZeroBased -> a -> b -> R.RResult info warnings error c) -> List a -> List b -> R.RResult info warnings error (Index.VerifiedList c)
indexedZipWithA func listX listY =
    case Index.indexedZipWith func listX listY of
        Index.LengthMatch xs ->
            sequenceAList xs
                |> R.fmap Index.LengthMatch

        Index.LengthMismatch x y ->
            R.pure (Index.LengthMismatch x y)


sequenceADict : (k -> comparable) -> (k -> k -> Order) -> Dict comparable k (R.RResult i w e v) -> R.RResult i w e (Dict comparable k v)
sequenceADict toComparable keyComparison =
    Map.foldr keyComparison (\k x acc -> R.apply acc (R.fmap (Map.insert toComparable k) x)) (R.pure Map.empty)


sequenceAList : List (R.RResult i w e v) -> R.RResult i w e (List v)
sequenceAList =
    List.foldr (\x acc -> R.apply acc (R.fmap (::) x)) (R.pure [])


sequenceDictMaybe : (k -> comparable) -> (k -> k -> Order) -> Dict comparable k (Maybe a) -> Maybe (Dict comparable k a)
sequenceDictMaybe toComparable keyComparison =
    Map.foldr keyComparison (\k -> Maybe.map2 (Map.insert toComparable k)) (Just Map.empty)


sequenceDictResult : (k -> comparable) -> (k -> k -> Order) -> Dict comparable k (Result e v) -> Result e (Dict comparable k v)
sequenceDictResult toComparable keyComparison =
    Map.foldr keyComparison (\k -> Result.map2 (Map.insert toComparable k)) (Ok Map.empty)


sequenceDictResult_ : (k -> comparable) -> (k -> k -> Order) -> Dict comparable k (Result e a) -> Result e ()
sequenceDictResult_ toComparable keyComparison =
    sequenceDictResult toComparable keyComparison >> Result.map (\_ -> ())


sequenceListMaybe : List (Maybe a) -> Maybe (List a)
sequenceListMaybe =
    List.foldr (Maybe.map2 (::)) (Just [])


sequenceNonemptyListResult : NE.Nonempty (Result e v) -> Result e (NE.Nonempty v)
sequenceNonemptyListResult (NE.Nonempty x xs) =
    List.foldr (\a acc -> Result.map2 NE.cons a acc) (Result.map NE.singleton x) xs


keysSet : (k -> comparable) -> (k -> k -> Order) -> Dict comparable k a -> EverySet comparable k
keysSet toComparable keyComparison =
    Map.keys keyComparison >> EverySet.fromList toComparable


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 pairs =
    let
        step : ( a, b, c ) -> ( List a, List b, List c ) -> ( List a, List b, List c )
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    List.foldr step ( [], [], [] ) pairs


mapM_ : (a -> Task Never b) -> List a -> Task Never ()
mapM_ f =
    let
        c : a -> Task Never () -> Task Never ()
        c x k =
            Task.bind (\_ -> k) (f x)
    in
    List.foldr c (Task.pure ())


dictMapM_ : (k -> k -> Order) -> (a -> Task Never b) -> Dict c k a -> Task Never ()
dictMapM_ keyComparison f =
    let
        c : k -> a -> Task Never () -> Task Never ()
        c _ x k =
            Task.bind (\_ -> k) (f x)
    in
    Map.foldl keyComparison c (Task.pure ())


maybeMapM : (a -> Maybe b) -> List a -> Maybe (List b)
maybeMapM =
    listMaybeTraverse


mapMapKeys : (k2 -> comparable) -> (k1 -> k1 -> Order) -> (k1 -> k2) -> Dict comparable k1 a -> Dict comparable k2 a
mapMapKeys toComparable keyComparison f =
    Map.fromList toComparable << Map.foldl keyComparison (\k x xs -> ( f k, x ) :: xs) []


mapMinViewWithKey : (k -> comparable) -> (k -> k -> Order) -> (( k, a ) -> comparable) -> Dict comparable k a -> Maybe ( ( k, a ), Dict comparable k a )
mapMinViewWithKey toComparable keyComparison compare dict =
    case List.sortBy compare (Map.toList keyComparison dict) of
        first :: tail ->
            Just ( first, Map.fromList toComparable tail )

        _ ->
            Nothing


mapMapMaybe : (k -> comparable) -> (k -> k -> Order) -> (a -> Maybe b) -> Dict comparable k a -> Dict comparable k b
mapMapMaybe toComparable keyComparison func =
    Map.toList keyComparison
        >> List.filterMap (\( k, a ) -> Maybe.map (Tuple.pair k) (func a))
        >> Map.fromList toComparable


mapTraverse : (k -> comparable) -> (k -> k -> Order) -> (a -> Task Never b) -> Dict comparable k a -> Task Never (Dict comparable k b)
mapTraverse toComparable keyComparison f =
    mapTraverseWithKey toComparable keyComparison (\_ -> f)


mapTraverseWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> Task Never b) -> Dict comparable k a -> Task Never (Dict comparable k b)
mapTraverseWithKey toComparable keyComparison f =
    Map.foldl keyComparison
        (\k a -> Task.bind (\c -> Task.fmap (\va -> Map.insert toComparable k va c) (f k a)))
        (Task.pure Map.empty)


mapTraverseResult : (k -> comparable) -> (k -> k -> Order) -> (a -> Result e b) -> Dict comparable k a -> Result e (Dict comparable k b)
mapTraverseResult toComparable keyComparison f =
    mapTraverseWithKeyResult toComparable keyComparison (\_ -> f)


mapTraverseWithKeyResult : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> Result e b) -> Dict comparable k a -> Result e (Dict comparable k b)
mapTraverseWithKeyResult toComparable keyComparison f =
    Map.foldl keyComparison
        (\k a -> Result.map2 (Map.insert toComparable k) (f k a))
        (Ok Map.empty)


listTraverse : (a -> Task Never b) -> List a -> Task Never (List b)
listTraverse =
    Task.mapM


listMaybeTraverse : (a -> Maybe b) -> List a -> Maybe (List b)
listMaybeTraverse f =
    List.foldr (\a -> Maybe.andThen (\c -> Maybe.map (\va -> va :: c) (f a)))
        (Just [])


nonEmptyListTraverse : (a -> Task Never b) -> NE.Nonempty a -> Task Never (NE.Nonempty b)
nonEmptyListTraverse f (NE.Nonempty x list) =
    List.foldl (\a -> Task.bind (\c -> Task.fmap (\va -> NE.cons va c) (f a)))
        (Task.fmap NE.singleton (f x))
        list


listTraverse_ : (a -> Task Never b) -> List a -> Task Never ()
listTraverse_ f =
    listTraverse f
        >> Task.fmap (\_ -> ())


maybeTraverseTask : (a -> Task x b) -> Maybe a -> Task x (Maybe b)
maybeTraverseTask f a =
    case Maybe.map f a of
        Just b ->
            Task.fmap Just b

        Nothing ->
            Task.pure Nothing


zipWithM : (a -> b -> Maybe c) -> List a -> List b -> Maybe (List c)
zipWithM f xs ys =
    List.map2 f xs ys
        |> Maybe.combine


listGroupBy : (a -> a -> Bool) -> List a -> List (List a)
listGroupBy p list =
    case list of
        [] ->
            []

        x :: xs ->
            xs
                |> List.foldl
                    (\current ( previous, ys, acc ) ->
                        if p previous current then
                            ( current, current :: ys, acc )

                        else
                            ( current, [ current ], ys :: acc )
                    )
                    ( x, [ x ], [] )
                |> (\( _, ys, acc ) ->
                        ys :: acc
                   )
                |> List.map List.reverse
                |> List.reverse


listMaximum : (a -> a -> Order) -> List a -> a
listMaximum compare xs =
    case List.sortWith (flip compare) xs of
        x :: _ ->
            x

        [] ->
            crash "maximum: empty structure"


listLookup : a -> List ( a, b ) -> Maybe b
listLookup key list =
    case list of
        [] ->
            Nothing

        ( x, y ) :: xys ->
            if key == x then
                Just y

            else
                listLookup key xys


foldl1 : (a -> a -> a) -> List a -> a
foldl1 f xs =
    let
        mf : a -> Maybe a -> Maybe a
        mf x m =
            Just
                (case m of
                    Nothing ->
                        x

                    Just y ->
                        f x y
                )
    in
    case List.foldl mf Nothing xs of
        Just a ->
            a

        Nothing ->
            crash "foldl1: empty structure"


foldl1_ : (a -> a -> a) -> List a -> a
foldl1_ f =
    foldl1 (\a b -> f b a)


foldr1 : (a -> a -> a) -> List a -> a
foldr1 f xs =
    let
        mf : a -> Maybe a -> Maybe a
        mf x m =
            Just
                (case m of
                    Nothing ->
                        x

                    Just y ->
                        f x y
                )
    in
    case List.foldr mf Nothing xs of
        Just a ->
            a

        Nothing ->
            crash "foldr1: empty structure"


lines : String -> List String
lines =
    String.split "\n"


unlines : List String -> String
unlines xs =
    String.join "\n" xs ++ "\n"



-- GHC.IO


type alias FilePath =
    String



-- System.FilePath


fpSplitDirectories : String -> List String
fpSplitDirectories path =
    String.split "/" path
        |> List.filter ((/=) "")
        |> (\a ->
                (if String.startsWith "/" path then
                    [ "/" ]

                 else
                    []
                )
                    ++ a
           )


fpSplitExtension : String -> ( String, String )
fpSplitExtension filename =
    case List.reverse (String.split "/" filename) of
        lastPart :: otherParts ->
            case List.reverse (String.indexes "." lastPart) of
                index :: _ ->
                    ( (String.left index lastPart :: otherParts)
                        |> List.reverse
                        |> String.join "/"
                    , String.dropLeft index lastPart
                    )

                [] ->
                    ( filename, "" )

        [] ->
            ( "", "" )


fpJoinPath : List String -> String
fpJoinPath paths =
    case paths of
        "/" :: tail ->
            "/" ++ String.join "/" tail

        _ ->
            String.join "/" paths


fpMakeRelative : FilePath -> FilePath -> FilePath
fpMakeRelative root path =
    if String.startsWith root path then
        String.dropLeft (String.length root + 1) path

    else
        path


fpAddTrailingPathSeparator : FilePath -> FilePath
fpAddTrailingPathSeparator path =
    if String.endsWith "/" path then
        path

    else
        path ++ "/"


fpPathSeparator : Char
fpPathSeparator =
    '/'


fpIsRelative : FilePath -> Bool
fpIsRelative =
    not << String.startsWith "/"


fpTakeFileName : FilePath -> FilePath
fpTakeFileName filename =
    Prelude.last (String.split "/" filename)


fpSplitFileName : FilePath -> ( String, String )
fpSplitFileName filename =
    case List.reverse (String.indexes "/" filename) of
        index :: _ ->
            ( String.left (index + 1) filename, String.dropLeft (index + 1) filename )

        _ ->
            ( "./", filename )


fpTakeExtension : FilePath -> String
fpTakeExtension =
    Tuple.second << fpSplitExtension


fpDropExtension : FilePath -> FilePath
fpDropExtension =
    Tuple.first << fpSplitExtension


fpTakeDirectory : FilePath -> FilePath
fpTakeDirectory filename =
    case List.reverse (String.split "/" filename) of
        [] ->
            "."

        "" :: "" :: [] ->
            "/"

        "" :: _ :: other ->
            String.join "/" (List.reverse other)

        _ :: other ->
            String.join "/" (List.reverse other)



-- System.FileLock


type LockSharedExclusive
    = LockExclusive


lockWithFileLock : String -> LockSharedExclusive -> (() -> Task Never a) -> Task Never a
lockWithFileLock path mode ioFunc =
    case mode of
        LockExclusive ->
            lockFile path
                |> Task.bind ioFunc
                |> Task.bind
                    (\a ->
                        unlockFile path
                            |> Task.fmap (\_ -> a)
                    )


lockFile : FilePath -> Task Never ()
lockFile path =
    Impure.task "lockFile"
        []
        (Impure.StringBody path)
        (Impure.Always ())


unlockFile : FilePath -> Task Never ()
unlockFile path =
    Impure.task "unlockFile"
        []
        (Impure.StringBody path)
        (Impure.Always ())



-- System.Directory


dirDoesFileExist : FilePath -> Task Never Bool
dirDoesFileExist filename =
    Impure.task "dirDoesFileExist"
        []
        (Impure.StringBody filename)
        (Impure.DecoderResolver Decode.bool)


dirFindExecutable : FilePath -> Task Never (Maybe FilePath)
dirFindExecutable filename =
    Impure.task "dirFindExecutable"
        []
        (Impure.StringBody filename)
        (Impure.DecoderResolver (Decode.maybe Decode.string))


dirCreateDirectoryIfMissing : Bool -> FilePath -> Task Never ()
dirCreateDirectoryIfMissing createParents filename =
    Impure.task "dirCreateDirectoryIfMissing"
        []
        (Impure.JsonBody
            (Encode.object
                [ ( "createParents", Encode.bool createParents )
                , ( "filename", Encode.string filename )
                ]
            )
        )
        (Impure.Always ())


dirGetCurrentDirectory : Task Never String
dirGetCurrentDirectory =
    Impure.task "dirGetCurrentDirectory"
        []
        Impure.EmptyBody
        (Impure.StringResolver identity)


dirGetAppUserDataDirectory : FilePath -> Task Never FilePath
dirGetAppUserDataDirectory filename =
    Impure.task "dirGetAppUserDataDirectory"
        []
        (Impure.StringBody filename)
        (Impure.StringResolver identity)


dirGetModificationTime : FilePath -> Task Never Time.Posix
dirGetModificationTime filename =
    Impure.task "dirGetModificationTime"
        []
        (Impure.StringBody filename)
        (Impure.DecoderResolver (Decode.map Time.millisToPosix Decode.int))


dirRemoveFile : FilePath -> Task Never ()
dirRemoveFile path =
    Impure.task "dirRemoveFile"
        []
        (Impure.StringBody path)
        (Impure.Always ())


dirRemoveDirectoryRecursive : FilePath -> Task Never ()
dirRemoveDirectoryRecursive path =
    Impure.task "dirRemoveDirectoryRecursive"
        []
        (Impure.StringBody path)
        (Impure.Always ())


dirDoesDirectoryExist : FilePath -> Task Never Bool
dirDoesDirectoryExist path =
    Impure.task "dirDoesDirectoryExist"
        []
        (Impure.StringBody path)
        (Impure.DecoderResolver Decode.bool)


dirCanonicalizePath : FilePath -> Task Never FilePath
dirCanonicalizePath path =
    Impure.task "dirCanonicalizePath"
        []
        (Impure.StringBody path)
        (Impure.StringResolver identity)


dirWithCurrentDirectory : FilePath -> Task Never a -> Task Never a
dirWithCurrentDirectory dir action =
    dirGetCurrentDirectory
        |> Task.bind
            (\currentDir ->
                bracket_
                    (Impure.task "dirWithCurrentDirectory"
                        []
                        (Impure.StringBody dir)
                        (Impure.Always ())
                    )
                    (Impure.task "dirWithCurrentDirectory"
                        []
                        (Impure.StringBody currentDir)
                        (Impure.Always ())
                    )
                    action
            )


dirListDirectory : FilePath -> Task Never (List FilePath)
dirListDirectory path =
    Impure.task "dirListDirectory"
        []
        (Impure.StringBody path)
        (Impure.DecoderResolver (Decode.list Decode.string))



-- System.Environment


envLookupEnv : String -> Task Never (Maybe String)
envLookupEnv name =
    Impure.task "envLookupEnv"
        []
        (Impure.StringBody name)
        (Impure.DecoderResolver (Decode.maybe Decode.string))


envGetProgName : Task Never String
envGetProgName =
    Task.pure "guida"


envGetArgs : Task Never (List String)
envGetArgs =
    Impure.task "envGetArgs"
        []
        Impure.EmptyBody
        (Impure.DecoderResolver (Decode.list Decode.string))



-- Codec.Archive.Zip


type ZipArchive
    = ZipArchive (List ZipEntry)


type ZipEntry
    = ZipEntry
        { eRelativePath : FilePath
        , eData : String
        }



-- Network.HTTP.Client


type HttpExceptionContent
    = StatusCodeException (HttpResponse ()) String
    | TooManyRedirects (List (HttpResponse ()))
    | ConnectionFailure SomeException


type HttpResponse body
    = HttpResponse
        { responseStatus : HttpStatus
        , responseHeaders : HttpResponseHeaders
        }


type alias HttpResponseHeaders =
    List ( String, String )


httpResponseStatus : HttpResponse body -> HttpStatus
httpResponseStatus (HttpResponse { responseStatus }) =
    responseStatus


httpStatusCode : HttpStatus -> Int
httpStatusCode (HttpStatus statusCode _) =
    statusCode


httpResponseHeaders : HttpResponse body -> HttpResponseHeaders
httpResponseHeaders (HttpResponse { responseHeaders }) =
    responseHeaders


httpHLocation : String
httpHLocation =
    "Location"


type HttpStatus
    = HttpStatus Int String



-- Control.Exception


type SomeException
    = SomeException


type AsyncException
    = UserInterrupt


bracket : Task Never a -> (a -> Task Never b) -> (a -> Task Never c) -> Task Never c
bracket before after thing =
    before
        |> Task.bind
            (\a ->
                thing a
                    |> Task.bind
                        (\r ->
                            after a
                                |> Task.fmap (\_ -> r)
                        )
            )


bracket_ : Task Never a -> Task Never b -> Task Never c -> Task Never c
bracket_ before after thing =
    bracket before (always after) (always thing)



-- Control.Concurrent


type alias ThreadId =
    Process.Id


forkIO : Task Never () -> Task Never ThreadId
forkIO =
    Process.spawn



-- Control.Concurrent.MVar


type MVar a
    = MVar Int


newMVar : (a -> BE.Encoder) -> a -> Task Never (MVar a)
newMVar toEncoder value =
    newEmptyMVar
        |> Task.bind
            (\mvar ->
                putMVar toEncoder mvar value
                    |> Task.fmap (\_ -> mvar)
            )


readMVar : BD.Decoder a -> MVar a -> Task Never a
readMVar decoder (MVar ref) =
    Impure.task "readMVar"
        []
        (Impure.StringBody (String.fromInt ref))
        (Impure.BytesResolver decoder)


modifyMVar : BD.Decoder a -> (a -> BE.Encoder) -> MVar a -> (a -> Task Never ( a, b )) -> Task Never b
modifyMVar decoder toEncoder m io =
    takeMVar decoder m
        |> Task.bind io
        |> Task.bind
            (\( a, b ) ->
                putMVar toEncoder m a
                    |> Task.fmap (\_ -> b)
            )


takeMVar : BD.Decoder a -> MVar a -> Task Never a
takeMVar decoder (MVar ref) =
    Impure.task "takeMVar"
        []
        (Impure.StringBody (String.fromInt ref))
        (Impure.BytesResolver decoder)


putMVar : (a -> BE.Encoder) -> MVar a -> a -> Task Never ()
putMVar encoder (MVar ref) value =
    Impure.task "putMVar"
        [ Http.header "id" (String.fromInt ref) ]
        (Impure.BytesBody (encoder value))
        (Impure.Always ())


newEmptyMVar : Task Never (MVar a)
newEmptyMVar =
    Impure.task "newEmptyMVar"
        []
        Impure.EmptyBody
        (Impure.DecoderResolver (Decode.map MVar Decode.int))



-- Control.Concurrent.Chan


type Chan a
    = Chan (MVar (Stream a)) (MVar (Stream a))


type alias Stream a =
    MVar (ChItem a)


type ChItem a
    = ChItem a (Stream a)


newChan : (MVar (ChItem a) -> BE.Encoder) -> Task Never (Chan a)
newChan toEncoder =
    newEmptyMVar
        |> Task.bind
            (\hole ->
                newMVar toEncoder hole
                    |> Task.bind
                        (\readVar ->
                            newMVar toEncoder hole
                                |> Task.fmap
                                    (\writeVar ->
                                        Chan readVar writeVar
                                    )
                        )
            )


readChan : BD.Decoder a -> Chan a -> Task Never a
readChan decoder (Chan readVar _) =
    modifyMVar mVarDecoder mVarEncoder readVar <|
        \read_end ->
            readMVar (chItemDecoder decoder) read_end
                |> Task.fmap
                    (\(ChItem val new_read_end) ->
                        -- Use readMVar here, not takeMVar,
                        -- else dupChan doesn't work
                        ( new_read_end, val )
                    )


writeChan : (a -> BE.Encoder) -> Chan a -> a -> Task Never ()
writeChan toEncoder (Chan _ writeVar) val =
    newEmptyMVar
        |> Task.bind
            (\new_hole ->
                takeMVar mVarDecoder writeVar
                    |> Task.bind
                        (\old_hole ->
                            putMVar (chItemEncoder toEncoder) old_hole (ChItem val new_hole)
                                |> Task.bind (\_ -> putMVar mVarEncoder writeVar new_hole)
                        )
            )



-- Data.ByteString.Builder


builderHPutBuilder : IO.Handle -> String -> Task Never ()
builderHPutBuilder =
    IO.hPutStr



-- Data.Binary


binaryDecodeFileOrFail : BD.Decoder a -> FilePath -> Task Never (Result ( Int, String ) a)
binaryDecodeFileOrFail decoder filename =
    Impure.task "binaryDecodeFileOrFail"
        []
        (Impure.StringBody filename)
        (Impure.BytesResolver (BD.map Ok decoder))


binaryEncodeFile : (a -> BE.Encoder) -> FilePath -> a -> Task Never ()
binaryEncodeFile toEncoder path value =
    Impure.task "write"
        [ Http.header "path" path ]
        (Impure.BytesBody (toEncoder value))
        (Impure.Always ())



-- System.Console.Haskeline


type ReplSettings
    = ReplSettings
        { historyFile : Maybe String
        , autoAddHistory : Bool
        , complete : ReplCompletionFunc
        }


type alias ReplInputT a =
    Task Never a


type ReplCompletion
    = ReplCompletion String String Bool


type ReplCompletionFunc
    = ReplCompletionFunc


replRunInputT : ReplSettings -> ReplInputT Exit.ExitCode -> State.StateT s Exit.ExitCode
replRunInputT _ io =
    State.liftIO io


replWithInterrupt : ReplInputT a -> ReplInputT a
replWithInterrupt =
    identity


replCompleteWord : Maybe Char -> String -> (String -> State.StateT a (List ReplCompletion)) -> ReplCompletionFunc
replCompleteWord _ _ _ =
    -- FIXME
    ReplCompletionFunc


replGetInputLine : String -> ReplInputT (Maybe String)
replGetInputLine prompt =
    Impure.task "replGetInputLine"
        []
        (Impure.StringBody prompt)
        (Impure.DecoderResolver (Decode.maybe Decode.string))


replGetInputLineWithInitial : String -> ( String, String ) -> ReplInputT (Maybe String)
replGetInputLineWithInitial prompt ( left, right ) =
    replGetInputLine (left ++ prompt ++ right)



-- NODE


nodeGetDirname : Task Never String
nodeGetDirname =
    Impure.task "nodeGetDirname"
        []
        Impure.EmptyBody
        (Impure.StringResolver identity)


nodeMathRandom : Task Never Float
nodeMathRandom =
    Impure.task "nodeMathRandom"
        []
        Impure.EmptyBody
        (Impure.DecoderResolver Decode.float)



-- ENCODERS and DECODERS


mVarDecoder : BD.Decoder (MVar a)
mVarDecoder =
    BD.map MVar BD.int


mVarEncoder : MVar a -> BE.Encoder
mVarEncoder (MVar ref) =
    BE.int ref


chItemEncoder : (a -> BE.Encoder) -> ChItem a -> BE.Encoder
chItemEncoder valueEncoder (ChItem value hole) =
    BE.sequence
        [ valueEncoder value
        , mVarEncoder hole
        ]


chItemDecoder : BD.Decoder a -> BD.Decoder (ChItem a)
chItemDecoder decoder =
    BD.map2 ChItem
        decoder
        mVarDecoder


someExceptionEncoder : SomeException -> BE.Encoder
someExceptionEncoder _ =
    BE.unsignedInt8 0


someExceptionDecoder : BD.Decoder SomeException
someExceptionDecoder =
    BD.unsignedInt8
        |> BD.map (\_ -> SomeException)


httpResponseEncoder : HttpResponse body -> BE.Encoder
httpResponseEncoder (HttpResponse httpResponse) =
    BE.sequence
        [ httpStatusEncoder httpResponse.responseStatus
        , httpResponseHeadersEncoder httpResponse.responseHeaders
        ]


httpResponseDecoder : BD.Decoder (HttpResponse body)
httpResponseDecoder =
    BD.map2
        (\responseStatus responseHeaders ->
            HttpResponse
                { responseStatus = responseStatus
                , responseHeaders = responseHeaders
                }
        )
        httpStatusDecoder
        httpResponseHeadersDecoder


httpStatusEncoder : HttpStatus -> BE.Encoder
httpStatusEncoder (HttpStatus statusCode statusMessage) =
    BE.sequence
        [ BE.int statusCode
        , BE.string statusMessage
        ]


httpStatusDecoder : BD.Decoder HttpStatus
httpStatusDecoder =
    BD.map2 HttpStatus
        BD.int
        BD.string


httpResponseHeadersEncoder : HttpResponseHeaders -> BE.Encoder
httpResponseHeadersEncoder =
    BE.list (BE.jsonPair BE.string BE.string)


httpResponseHeadersDecoder : BD.Decoder HttpResponseHeaders
httpResponseHeadersDecoder =
    BD.list (BD.jsonPair BD.string BD.string)


httpExceptionContentEncoder : HttpExceptionContent -> BE.Encoder
httpExceptionContentEncoder httpExceptionContent =
    case httpExceptionContent of
        StatusCodeException response body ->
            BE.sequence
                [ BE.unsignedInt8 0
                , httpResponseEncoder response
                , BE.string body
                ]

        TooManyRedirects responses ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.list httpResponseEncoder responses
                ]

        ConnectionFailure someException ->
            BE.sequence
                [ BE.unsignedInt8 2
                , someExceptionEncoder someException
                ]


httpExceptionContentDecoder : BD.Decoder HttpExceptionContent
httpExceptionContentDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 StatusCodeException
                            httpResponseDecoder
                            BD.string

                    1 ->
                        BD.map TooManyRedirects (BD.list httpResponseDecoder)

                    2 ->
                        BD.map ConnectionFailure someExceptionDecoder

                    _ ->
                        BD.fail
            )
