module Utils.Main exposing
    ( ChItem
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
    , dirRemoveDirectoryRecursive
    , dirRemoveFile
    , dirWithCurrentDirectory
    , eitherLefts
    , envGetArgs
    , envGetProgName
    , envLookupEnv
    , exitFailure
    , exitSuccess
    , filterM
    , find
    , foldM
    , foldl1_
    , foldr1
    , forM_
    , forkIO
    , fpAddExtension
    , fpAddTrailingPathSeparator
    , fpDropFileName
    , fpForwardSlash
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
    , httpExceptionContentCodec
    , httpExceptionContentDecoder
    , httpExceptionContentEncoder
    , httpHLocation
    , httpResponseHeaders
    , httpResponseStatus
    , httpStatusCode
    , indexedForA
    , indexedTraverse
    , indexedZipWithA
    , ioDictFoldM
    , ioFoldM
    , ioFoldrM
    , keysSet
    , liftIOInputT
    , liftInputT
    , lines
    , listGroupBy
    , listLookup
    , listMaximum
    , listTraverse
    , listTraverseStateT
    , listTraverse_
    , lockWithFileLock
    , mVarCodec
    , mVarDecoder
    , mVarEncoder
    , mapFindMin
    , mapFromKeys
    , mapFromListWith
    , mapInsertWith
    , mapIntersectionWith
    , mapIntersectionWithKey
    , mapLookupMin
    , mapM
    , mapM_
    , mapMapMaybe
    , mapMinViewWithKey
    , mapTraverse
    , mapTraverseResult
    , mapTraverseStateT
    , mapTraverseWithKey
    , mapTraverseWithKeyResult
    , mapUnionWith
    , mapUnions
    , mapUnionsWith
    , maybeEncoder
    , maybeMapM
    , maybeTraverse
    , maybeTraverseStateT
    , maybeTraverseTask
    , newChan
    , newEmptyMVar
    , newMVar
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
    , shaAndArchiveDecoder
    , someExceptionCodec
    , someExceptionDecoder
    , someExceptionEncoder
    , stateGet
    , statePut
    , takeMVar
    , tupleTraverse
    , tupleTraverseStateT
    , unlines
    , unzip3
    , writeChan
    , zipERelativePath
    , zipFromEntry
    , zipWithM
    , zipZEntries
    )

import Basics.Extra exposing (flip)
import Builder.Reporting.Task as Task exposing (Task)
import Compiler.Data.Index as Index
import Compiler.Data.NonEmptyList as NE
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Reporting.Result as R
import Data.IO as IO exposing (IO(..))
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Prelude
import Serialize exposing (Codec)
import Time
import Utils.Crash exposing (crash)


liftInputT : IO () -> ReplInputT ()
liftInputT =
    identity


liftIOInputT : IO a -> ReplInputT a
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


fpForwardSlash : FilePath -> FilePath -> FilePath
fpForwardSlash path1 path2 =
    if String.startsWith path1 path2 then
        path2

    else
        path1 ++ "/" ++ path2


fpAddExtension : FilePath -> String -> FilePath
fpAddExtension path extension =
    if String.startsWith "." extension then
        path ++ extension

    else
        path ++ "." ++ extension


mapFromListWith : (k -> k -> Order) -> (a -> a -> a) -> List ( k, a ) -> Dict k a
mapFromListWith keyComparison f =
    List.foldl
        (\( k, a ) ->
            Dict.update keyComparison k (Maybe.map (flip f a))
        )
        Dict.empty


maybeEncoder : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybeEncoder encoder maybeValue =
    case maybeValue of
        Just value ->
            encoder value

        Nothing ->
            Encode.null


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


mapFromKeys : (k -> k -> Order) -> (k -> v) -> List k -> Dict k v
mapFromKeys keyComparison f =
    List.map (\k -> ( k, f k ))
        >> Dict.fromList keyComparison


filterM : (a -> IO Bool) -> List a -> IO (List a)
filterM p =
    List.foldr
        (\x acc ->
            IO.apply acc
                (IO.fmap
                    (\flg ->
                        if flg then
                            (::) x

                        else
                            identity
                    )
                    (p x)
                )
        )
        (IO.pure [])


find : k -> Dict k a -> a
find k items =
    case Dict.get k items of
        Just item ->
            item

        Nothing ->
            crash "Map.!: given key is not an element in the map"


mapLookupMin : Dict comparable a -> Maybe ( comparable, a )
mapLookupMin dict =
    case List.sortBy Tuple.first (Dict.toList dict) of
        firstElem :: _ ->
            Just firstElem

        _ ->
            Nothing


mapFindMin : Dict comparable a -> ( comparable, a )
mapFindMin dict =
    case List.sortBy Tuple.first (Dict.toList dict) of
        firstElem :: _ ->
            firstElem

        _ ->
            crash "Error: empty map has no minimal element"


mapInsertWith : (k -> k -> Order) -> (a -> a -> a) -> k -> a -> Dict k a -> Dict k a
mapInsertWith keyComparison f k a =
    Dict.update keyComparison k (Maybe.map (f a) >> Maybe.withDefault a >> Just)


mapIntersectionWith : (k -> k -> Order) -> (a -> b -> c) -> Dict k a -> Dict k b -> Dict k c
mapIntersectionWith keyComparison func =
    mapIntersectionWithKey keyComparison (\_ -> func)


mapIntersectionWithKey : (k -> k -> Order) -> (k -> a -> b -> c) -> Dict k a -> Dict k b -> Dict k c
mapIntersectionWithKey keyComparison func dict1 dict2 =
    Dict.merge (\_ _ -> identity) (\k v1 v2 -> Dict.insert keyComparison k (func k v1 v2)) (\_ _ -> identity) dict1 dict2 Dict.empty


mapUnionWith : (k -> k -> Order) -> (a -> a -> a) -> Dict k a -> Dict k a -> Dict k a
mapUnionWith keyComparison f a b =
    Dict.merge (Dict.insert keyComparison) (\k va vb -> Dict.insert keyComparison k (f va vb)) (Dict.insert keyComparison) a b Dict.empty


mapUnionsWith : (k -> k -> Order) -> (a -> a -> a) -> List (Dict k a) -> Dict k a
mapUnionsWith keyComparison f =
    List.foldl (mapUnionWith keyComparison f) Dict.empty


mapUnions : (k -> k -> Order) -> List (Dict k a) -> Dict k a
mapUnions keyComparison =
    List.foldr (Dict.union keyComparison) Dict.empty


foldM : (b -> a -> R.RResult info warnings error b) -> b -> List a -> R.RResult info warnings error b
foldM f b =
    List.foldl (\a -> R.bind (\acc -> f acc a)) (R.ok b)


ioFoldM : (b -> a -> IO b) -> b -> List a -> IO b
ioFoldM f b =
    List.foldl (\a -> IO.bind (\acc -> f acc a)) (IO.pure b)


ioFoldrM : (a -> b -> IO b) -> b -> List a -> IO b
ioFoldrM f b =
    List.foldr (IO.bind << f) (IO.pure b)


ioDictFoldM : (b -> a -> IO b) -> b -> Dict k a -> IO b
ioDictFoldM f b =
    Dict.foldl (\_ a -> IO.bind (\acc -> f acc a)) (IO.pure b)


indexedTraverse : (Index.ZeroBased -> a -> IO b) -> List a -> IO (List b)
indexedTraverse func xs =
    sequenceAListIO (Index.indexedMap func xs)


indexedZipWithA : (Index.ZeroBased -> a -> b -> R.RResult info warnings error c) -> List a -> List b -> R.RResult info warnings error (Index.VerifiedList c)
indexedZipWithA func listX listY =
    case Index.indexedZipWith func listX listY of
        Index.LengthMatch xs ->
            sequenceAList xs
                |> R.fmap Index.LengthMatch

        Index.LengthMismatch x y ->
            R.pure (Index.LengthMismatch x y)


indexedForA : List a -> (Index.ZeroBased -> a -> IO b) -> IO (List b)
indexedForA xs func =
    sequenceAListIO (Index.indexedMap func xs)


sequenceADict : (k -> k -> Order) -> Dict k (R.RResult i w e v) -> R.RResult i w e (Dict k v)
sequenceADict keyComparison =
    Dict.foldr (\k x acc -> R.apply acc (R.fmap (Dict.insert keyComparison k) x)) (R.pure Dict.empty)


sequenceAList : List (R.RResult i w e v) -> R.RResult i w e (List v)
sequenceAList =
    List.foldr (\x acc -> R.apply acc (R.fmap (::) x)) (R.pure [])


sequenceAListIO : List (IO a) -> IO (List a)
sequenceAListIO =
    List.foldr (\x acc -> IO.apply acc (IO.fmap (::) x)) (IO.pure [])


sequenceDictMaybe : (k -> k -> Order) -> Dict k (Maybe a) -> Maybe (Dict k a)
sequenceDictMaybe keyComparison =
    Dict.foldr (\k -> Maybe.map2 (Dict.insert keyComparison k)) (Just Dict.empty)


sequenceDictResult : (k -> k -> Order) -> Dict k (Result e v) -> Result e (Dict k v)
sequenceDictResult keyComparison =
    Dict.foldr (\k -> Result.map2 (Dict.insert keyComparison k)) (Ok Dict.empty)


sequenceDictResult_ : (k -> k -> Order) -> Dict k (Result e a) -> Result e ()
sequenceDictResult_ keyComparison =
    sequenceDictResult keyComparison >> Result.map (\_ -> ())


sequenceListMaybe : List (Maybe a) -> Maybe (List a)
sequenceListMaybe =
    List.foldr (Maybe.map2 (::)) (Just [])


sequenceNonemptyListResult : NE.Nonempty (Result e v) -> Result e (NE.Nonempty v)
sequenceNonemptyListResult (NE.Nonempty x xs) =
    List.foldr (\a acc -> Result.map2 NE.cons a acc) (Result.map NE.singleton x) xs


keysSet : (k -> k -> Order) -> Dict k a -> EverySet k
keysSet keyComparison =
    Dict.keys >> EverySet.fromList keyComparison


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 pairs =
    let
        step : ( a, b, c ) -> ( List a, List b, List c ) -> ( List a, List b, List c )
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    List.foldr step ( [], [], [] ) pairs


mapM_ : (a -> IO b) -> List a -> IO ()
mapM_ f =
    let
        c : a -> IO () -> IO ()
        c x k =
            IO.bind (\_ -> k) (f x)
    in
    List.foldr c (IO.pure ())


dictMapM_ : (a -> IO b) -> Dict k a -> IO ()
dictMapM_ f =
    let
        c : k -> a -> IO () -> IO ()
        c _ x k =
            IO.bind (\_ -> k) (f x)
    in
    Dict.foldl c (IO.pure ())


mapM : (a -> IO b) -> List a -> IO (List b)
mapM =
    listTraverse


maybeMapM : (a -> Maybe b) -> List a -> Maybe (List b)
maybeMapM =
    listMaybeTraverse


mapMinViewWithKey : (k -> k -> Order) -> (( k, a ) -> comparable) -> Dict k a -> Maybe ( ( k, a ), Dict k a )
mapMinViewWithKey keyComparison compare dict =
    case List.sortBy compare (Dict.toList dict) of
        first :: tail ->
            Just ( first, Dict.fromList keyComparison tail )

        _ ->
            Nothing


mapMapMaybe : (k -> k -> Order) -> (a -> Maybe b) -> Dict k a -> Dict k b
mapMapMaybe keyComparison func =
    Dict.toList
        >> List.filterMap (\( k, a ) -> Maybe.map (Tuple.pair k) (func a))
        >> Dict.fromList keyComparison


forM_ : List a -> (a -> IO b) -> IO ()
forM_ list f =
    mapM_ f list


mapTraverse : (k -> k -> Order) -> (a -> IO b) -> Dict k a -> IO (Dict k b)
mapTraverse keyComparison f =
    mapTraverseWithKey keyComparison (\_ -> f)


mapTraverseWithKey : (k -> k -> Order) -> (k -> a -> IO b) -> Dict k a -> IO (Dict k b)
mapTraverseWithKey keyComparison f =
    Dict.foldl (\k a -> IO.bind (\c -> IO.fmap (\va -> Dict.insert keyComparison k va c) (f k a)))
        (IO.pure Dict.empty)


mapTraverseResult : (k -> k -> Order) -> (a -> Result e b) -> Dict k a -> Result e (Dict k b)
mapTraverseResult keyComparison f =
    mapTraverseWithKeyResult keyComparison (\_ -> f)


mapTraverseWithKeyResult : (k -> k -> Order) -> (k -> a -> Result e b) -> Dict k a -> Result e (Dict k b)
mapTraverseWithKeyResult keyComparison f =
    Dict.foldl (\k a -> Result.map2 (Dict.insert keyComparison k) (f k a))
        (Ok Dict.empty)


mapTraverseStateT : (k -> k -> Order) -> (a -> IO.StateT s b) -> Dict k a -> IO.StateT s (Dict k b)
mapTraverseStateT keyComparison f =
    mapTraverseWithKeyStateT keyComparison (\_ -> f)


mapTraverseWithKeyStateT : (k -> k -> Order) -> (k -> a -> IO.StateT s b) -> Dict k a -> IO.StateT s (Dict k b)
mapTraverseWithKeyStateT keyComparison f =
    Dict.foldl (\k a -> IO.bindStateT (\c -> IO.fmapStateT (\va -> Dict.insert keyComparison k va c) (f k a)))
        (IO.pureStateT Dict.empty)


listTraverse : (a -> IO b) -> List a -> IO (List b)
listTraverse f =
    List.foldr (\a -> IO.bind (\c -> IO.fmap (\va -> va :: c) (f a)))
        (IO.pure [])


listMaybeTraverse : (a -> Maybe b) -> List a -> Maybe (List b)
listMaybeTraverse f =
    List.foldr (\a -> Maybe.andThen (\c -> Maybe.map (\va -> va :: c) (f a)))
        (Just [])


nonEmptyListTraverse : (a -> IO b) -> NE.Nonempty a -> IO (NE.Nonempty b)
nonEmptyListTraverse f (NE.Nonempty x list) =
    List.foldl (\a -> IO.bind (\c -> IO.fmap (\va -> NE.cons va c) (f a)))
        (IO.fmap NE.singleton (f x))
        list


listTraverse_ : (a -> IO b) -> List a -> IO ()
listTraverse_ f =
    listTraverse f
        >> IO.fmap (\_ -> ())


listTraverseStateT : (a -> IO.StateT s b) -> List a -> IO.StateT s (List b)
listTraverseStateT f =
    List.foldr (\a -> IO.bindStateT (\c -> IO.fmapStateT (\va -> va :: c) (f a)))
        (IO.pureStateT [])


tupleTraverse : (b -> IO c) -> ( a, b ) -> IO ( a, c )
tupleTraverse f ( a, b ) =
    IO.fmap (Tuple.pair a) (f b)


tupleTraverseStateT : (b -> IO.StateT s c) -> ( a, b ) -> IO.StateT s ( a, c )
tupleTraverseStateT f ( a, b ) =
    IO.fmapStateT (Tuple.pair a) (f b)


maybeTraverse : (a -> IO b) -> Maybe a -> IO (Maybe b)
maybeTraverse f a =
    case Maybe.map f a of
        Just b ->
            IO.fmap Just b

        Nothing ->
            IO.pure Nothing


maybeTraverseStateT : (a -> IO.StateT s b) -> Maybe a -> IO.StateT s (Maybe b)
maybeTraverseStateT f a =
    case Maybe.map f a of
        Just b ->
            IO.fmapStateT Just b

        Nothing ->
            IO.pureStateT Nothing


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
    if String.startsWith path root then
        String.dropLeft (String.length root) path

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
    String.startsWith "/"


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


lockWithFileLock : String -> LockSharedExclusive -> (() -> IO a) -> IO a
lockWithFileLock path mode ioFunc =
    case mode of
        LockExclusive ->
            lockFile path
                |> IO.bind ioFunc
                |> IO.bind
                    (\a ->
                        unlockFile path
                            |> IO.fmap (\_ -> a)
                    )


lockFile : String -> IO ()
lockFile path =
    IO.make (Decode.succeed ()) (IO.LockFile path)


unlockFile : String -> IO ()
unlockFile path =
    IO.make (Decode.succeed ()) (IO.UnlockFile path)



-- System.Directory


dirDoesFileExist : FilePath -> IO Bool
dirDoesFileExist filename =
    IO.make Decode.bool (IO.DirDoesFileExist filename)


dirFindExecutable : FilePath -> IO (Maybe FilePath)
dirFindExecutable filename =
    IO.make (Decode.maybe Decode.string) (IO.DirFindExecutable filename)


dirCreateDirectoryIfMissing : Bool -> FilePath -> IO ()
dirCreateDirectoryIfMissing createParents filename =
    IO.make (Decode.succeed ()) (IO.DirCreateDirectoryIfMissing createParents filename)


dirGetCurrentDirectory : IO String
dirGetCurrentDirectory =
    IO.make Decode.string IO.DirGetCurrentDirectory


dirGetAppUserDataDirectory : FilePath -> IO FilePath
dirGetAppUserDataDirectory filename =
    IO.make Decode.string (IO.DirGetAppUserDataDirectory filename)


dirGetModificationTime : FilePath -> IO Time.Posix
dirGetModificationTime filename =
    IO.make (Decode.map Time.millisToPosix Decode.int) (IO.DirGetModificationTime filename)


dirRemoveFile : FilePath -> IO ()
dirRemoveFile path =
    IO.make (Decode.succeed ()) (IO.DirRemoveFile path)


dirRemoveDirectoryRecursive : FilePath -> IO ()
dirRemoveDirectoryRecursive path =
    IO.make (Decode.succeed ()) (IO.DirRemoveDirectoryRecursive path)


dirDoesDirectoryExist : FilePath -> IO Bool
dirDoesDirectoryExist path =
    IO.make Decode.bool (IO.DirDoesDirectoryExist path)


dirCanonicalizePath : FilePath -> IO FilePath
dirCanonicalizePath path =
    IO.make Decode.string (IO.DirCanonicalizePath path)


dirWithCurrentDirectory : FilePath -> IO a -> IO a
dirWithCurrentDirectory dir action =
    dirGetCurrentDirectory
        |> IO.bind
            (\currentDir ->
                bracket_
                    (IO.make (Decode.succeed ()) (IO.DirWithCurrentDirectory dir))
                    (IO.make (Decode.succeed ()) (IO.DirWithCurrentDirectory currentDir))
                    action
            )



-- System.Environment


envLookupEnv : String -> IO (Maybe String)
envLookupEnv name =
    IO.make (Decode.maybe Decode.string) (IO.EnvLookupEnv name)


envGetProgName : IO String
envGetProgName =
    IO.make Decode.string IO.EnvGetProgName


envGetArgs : IO (List String)
envGetArgs =
    IO.make (Decode.list Decode.string) IO.EnvGetArgs



-- Codec.Archive.Zip


type ZipArchive
    = ZipArchive (List ZipEntry)


type ZipEntry
    = ZipEntry
        { eRelativePath : FilePath
        , eData : String
        }


zipZEntries : ZipArchive -> List ZipEntry
zipZEntries (ZipArchive entries) =
    entries


zipERelativePath : ZipEntry -> FilePath
zipERelativePath (ZipEntry { eRelativePath }) =
    eRelativePath


zipFromEntry : ZipEntry -> String
zipFromEntry (ZipEntry { eData }) =
    eData


zipArchiveDecoder : Decode.Decoder ZipArchive
zipArchiveDecoder =
    Decode.map ZipArchive (Decode.list zipEntryDecoder)


shaAndArchiveDecoder : Decode.Decoder ( String, ZipArchive )
shaAndArchiveDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "sha" Decode.string)
        (Decode.field "archive" zipArchiveDecoder)


zipEntryDecoder : Decode.Decoder ZipEntry
zipEntryDecoder =
    Decode.map2
        (\eRelativePath eData ->
            ZipEntry
                { eRelativePath = eRelativePath
                , eData = eData
                }
        )
        (Decode.field "eRelativePath" Decode.string)
        (Decode.field "eData" Decode.string)



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


bracket : IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing =
    before
        |> IO.bind
            (\a ->
                thing a
                    |> IO.bind
                        (\r ->
                            after a
                                |> IO.fmap (\_ -> r)
                        )
            )


bracket_ : IO a -> IO b -> IO c -> IO c
bracket_ before after thing =
    bracket before (always after) (always thing)



-- Control.Concurrent


type ThreadId
    = ThreadId


forkIO : IO () -> IO ThreadId
forkIO ioArg =
    IO
        (\next ->
            Decode.succeed
                ( IO.Process (next ThreadId)
                , IO.NoOp
                , Just ioArg
                )
        )



-- Control.Concurrent.MVar


type MVar a
    = MVar Int


newMVar : Codec e a -> a -> IO (MVar a)
newMVar codec value =
    newEmptyMVar
        |> IO.bind
            (\mvar ->
                putMVar codec mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar : Codec e a -> MVar a -> IO a
readMVar codec (MVar ref) =
    IO.make (Serialize.getJsonDecoder (\_ -> "failure on readMVar...") codec) (IO.ReadMVar ref)


modifyMVar : Codec e a -> MVar a -> (a -> IO ( a, b )) -> IO b
modifyMVar codec m io =
    takeMVar codec m
        |> IO.bind io
        |> IO.bind
            (\( a, b ) ->
                putMVar codec m a
                    |> IO.fmap (\_ -> b)
            )


takeMVar : Codec e a -> MVar a -> IO a
takeMVar codec (MVar ref) =
    IO.make (Serialize.getJsonDecoder (\_ -> "failure on takeMVar") codec) (IO.TakeMVar ref)


putMVar : Codec e a -> MVar a -> a -> IO ()
putMVar codec (MVar ref) value =
    IO.make (Decode.succeed ()) (IO.PutMVar ref (Serialize.encodeToJson codec value))


newEmptyMVar : IO (MVar a)
newEmptyMVar =
    IO.make (Decode.map MVar Decode.int) IO.NewEmptyMVar



-- Control.Concurrent.Chan


type Chan a
    = Chan (MVar (Stream a)) (MVar (Stream a))


type alias Stream a =
    MVar (ChItem a)


type ChItem a
    = ChItem a (Stream a)


newChan : Codec e (MVar (ChItem a)) -> IO (Chan a)
newChan codec =
    newEmptyMVar
        |> IO.bind
            (\hole ->
                newMVar codec hole
                    |> IO.bind
                        (\readVar ->
                            newMVar codec hole
                                |> IO.fmap
                                    (\writeVar ->
                                        Chan readVar writeVar
                                    )
                        )
            )


readChan : Codec e a -> Chan a -> IO a
readChan codec (Chan readVar _) =
    modifyMVar mVarCodec readVar <|
        \read_end ->
            readMVar (chItemCodec codec) read_end
                |> IO.fmap
                    (\(ChItem val new_read_end) ->
                        -- Use readMVar here, not takeMVar,
                        -- else dupChan doesn't work
                        ( new_read_end, val )
                    )


writeChan : Codec e a -> Chan a -> a -> IO ()
writeChan codec (Chan _ writeVar) val =
    newEmptyMVar
        |> IO.bind
            (\new_hole ->
                takeMVar mVarCodec writeVar
                    |> IO.bind
                        (\old_hole ->
                            putMVar (chItemCodec codec) old_hole (ChItem val new_hole)
                                |> IO.bind (\_ -> putMVar mVarCodec writeVar new_hole)
                        )
            )



-- System.Exit


exitFailure : IO a
exitFailure =
    IO.make (Decode.fail "exitFailure") (IO.Exit "exitFailure" 1)


exitSuccess : IO a
exitSuccess =
    IO.make (Decode.fail "exitSuccess") (IO.Exit "exitSuccess" 0)



-- Data.ByteString.Builder


builderHPutBuilder : IO.Handle -> String -> IO ()
builderHPutBuilder handle str =
    IO.make (Decode.succeed ()) (IO.HPutStr handle str)



-- Data.Binary


binaryDecodeFileOrFail : Codec e a -> FilePath -> IO (Result ( Int, String ) a)
binaryDecodeFileOrFail codec filename =
    IO.make
        (Decode.oneOf
            [ Decode.map Ok (Serialize.getJsonDecoder (\_ -> "Could not find file " ++ filename) codec)
            , Decode.succeed (Err ( 0, "Could not find file " ++ filename ))
            ]
        )
        (IO.BinaryDecodeFileOrFail filename)


binaryEncodeFile : Codec e a -> FilePath -> a -> IO ()
binaryEncodeFile codec path value =
    IO.make (Decode.succeed ()) (IO.Write path (Serialize.encodeToJson codec value))



-- System.Console.Haskeline


type ReplSettings
    = ReplSettings
        { historyFile : Maybe String
        , autoAddHistory : Bool
        , complete : ReplCompletionFunc
        }


type alias ReplInputT a =
    IO a


type ReplCompletion
    = ReplCompletion String String Bool


type ReplCompletionFunc
    = ReplCompletionFunc


replRunInputT : ReplSettings -> ReplInputT IO.ExitCode -> IO.StateT s IO.ExitCode
replRunInputT _ io =
    IO.liftIO io


replWithInterrupt : ReplInputT a -> ReplInputT a
replWithInterrupt =
    identity


replCompleteWord : Maybe Char -> String -> (String -> IO.StateT a (List ReplCompletion)) -> ReplCompletionFunc
replCompleteWord _ _ _ =
    -- FIXME
    ReplCompletionFunc


replGetInputLine : String -> ReplInputT (Maybe String)
replGetInputLine prompt =
    IO.make (Decode.maybe Decode.string) (IO.ReplGetInputLine prompt)


replGetInputLineWithInitial : String -> ( String, String ) -> ReplInputT (Maybe String)
replGetInputLineWithInitial prompt ( left, right ) =
    IO.make (Decode.maybe Decode.string) (IO.ReplGetInputLineWithInitial prompt ( left, right ))



-- Control.Monad.State.Class


stateGet : Decode.Decoder s -> IO.StateT s s
stateGet decoder =
    let
        io : IO s
        io =
            IO.make decoder IO.StateGet
    in
    IO.StateT (\_ -> IO.fmap (\s -> ( s, s )) io)


statePut : (s -> Encode.Value) -> s -> IO ()
statePut encoder s =
    IO.make (Decode.succeed ()) (IO.StatePut (encoder s))



-- ENCODERS and DECODERS


mVarEncoder : MVar a -> Encode.Value
mVarEncoder (MVar ref) =
    Encode.int ref


mVarDecoder : Decode.Decoder (MVar a)
mVarDecoder =
    Decode.map MVar Decode.int


mVarCodec : Codec e (MVar a)
mVarCodec =
    Serialize.int |> Serialize.map MVar (\(MVar ref) -> ref)


chItemCodec : Codec e a -> Codec e (ChItem a)
chItemCodec codec =
    Serialize.customType
        (\chItemCodecEncoder (ChItem value hole) ->
            chItemCodecEncoder value hole
        )
        |> Serialize.variant2 ChItem codec mVarCodec
        |> Serialize.finishCustomType


someExceptionEncoder : SomeException -> Encode.Value
someExceptionEncoder _ =
    Encode.object [ ( "type", Encode.string "SomeException" ) ]


someExceptionDecoder : Decode.Decoder SomeException
someExceptionDecoder =
    Decode.succeed SomeException


someExceptionCodec : Codec e SomeException
someExceptionCodec =
    Debug.todo "someExceptionCodec"


httpResponseEncoder : HttpResponse body -> Encode.Value
httpResponseEncoder (HttpResponse httpResponse) =
    Encode.object
        [ ( "type", Encode.string "HttpResponse" )
        , ( "responseStatus", httpStatusEncoder httpResponse.responseStatus )
        , ( "responseHeaders", httpResponseHeadersEncoder httpResponse.responseHeaders )
        ]


httpResponseDecoder : Decode.Decoder (HttpResponse body)
httpResponseDecoder =
    Decode.map2
        (\responseStatus responseHeaders ->
            HttpResponse
                { responseStatus = responseStatus
                , responseHeaders = responseHeaders
                }
        )
        (Decode.field "responseStatus" httpStatusDecoder)
        (Decode.field "responseHeaders" httpResponseHeadersDecoder)


httpStatusEncoder : HttpStatus -> Encode.Value
httpStatusEncoder (HttpStatus statusCode statusMessage) =
    Encode.object
        [ ( "type", Encode.string "HttpStatus" )
        , ( "statusCode", Encode.int statusCode )
        , ( "statusMessage", Encode.string statusMessage )
        ]


httpStatusDecoder : Decode.Decoder HttpStatus
httpStatusDecoder =
    Decode.map2 HttpStatus
        (Decode.field "statusCode" Decode.int)
        (Decode.field "statusMessage" Decode.string)


httpResponseHeadersEncoder : HttpResponseHeaders -> Encode.Value
httpResponseHeadersEncoder =
    Encode.list (E.jsonPair Encode.string Encode.string)


httpResponseHeadersDecoder : Decode.Decoder HttpResponseHeaders
httpResponseHeadersDecoder =
    Decode.list (D.jsonPair Decode.string Decode.string)


httpExceptionContentEncoder : HttpExceptionContent -> Encode.Value
httpExceptionContentEncoder httpExceptionContent =
    case httpExceptionContent of
        StatusCodeException response body ->
            Encode.object
                [ ( "type", Encode.string "StatusCodeException" )
                , ( "response", httpResponseEncoder response )
                , ( "body", Encode.string body )
                ]

        TooManyRedirects responses ->
            Encode.object
                [ ( "type", Encode.string "TooManyRedirects" )
                , ( "responses", Encode.list httpResponseEncoder responses )
                ]

        ConnectionFailure someException ->
            Encode.object
                [ ( "type", Encode.string "ConnectionFailure" )
                , ( "someException", someExceptionEncoder someException )
                ]


httpExceptionContentDecoder : Decode.Decoder HttpExceptionContent
httpExceptionContentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "StatusCodeException" ->
                        Decode.map2 StatusCodeException
                            (Decode.field "response" httpResponseDecoder)
                            (Decode.field "body" Decode.string)

                    "TooManyRedirects" ->
                        Decode.map TooManyRedirects (Decode.field "responses" (Decode.list httpResponseDecoder))

                    "ConnectionFailure" ->
                        Decode.map ConnectionFailure (Decode.field "someException" someExceptionDecoder)

                    _ ->
                        Decode.fail ("Failed to decode HttpExceptionContent's type: " ++ type_)
            )


httpExceptionContentCodec : Codec e HttpExceptionContent
httpExceptionContentCodec =
    Debug.todo "httpExceptionContentCodec"
