module Utils.Main exposing
    ( AsyncException(..)
    , Chan
    , FilePath
    , HTTPResponse(..)
    , LockSharedExclusive(..)
    , MVar(..)
    , ReplCompletion(..)
    , ReplInputT
    , ReplSettings(..)
    , SomeException(..)
    , StdStream(..)
    , ThreadId
    , ZipArchive(..)
    , ZipEntry(..)
    , binaryDecodeFileOrFail
    , binaryEncodeFile
    , bsHPut
    , bsReadFile
    , builderHPutBuilder
    , chItemDecoder
    , dictMapM_
    , dirCanonicalizePath
    , dirCreateDirectoryIfMissing
    , dirDoesDirectoryExist
    , dirDoesFileExist
    , dirFindExecutable
    , dirGetAppUserDataDirectory
    , dirGetCurrentDirectory
    , dirGetDirectoryContents
    , dirGetModificationTime
    , dirRemoveDirectoryRecursive
    , dirRemoveFile
    , dirWithCurrentDirectory
    , dropWhile
    , eitherLefts
    , envGetArgs
    , envGetProgName
    , envLookupEnv
    , exitFailure
    , exitSuccess
    , filterM
    , find
    , flip
    , foldM
    , foldl1
    , foldl1_
    , foldr1
    , forMArray
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
    , fromException
    , head
    , httpResponseDecoder
    , httpResponseEncoder
    , indexedForA
    , indexedTraverse
    , indexedZipWithA
    , init
    , ioDictFoldM
    , ioFoldM
    , ioFoldrM
    , keysSet
    , last
    , lift
    , lines
    , listGroupBy
    , listLookup
    , listMaximum
    , listTraverse
    , listTraverseStateT
    , listTraverse_
    , lockWithFileLock
    , mVarDecoder
    , mVarEncoder
    , mapFindMin
    , mapFromKeys
    , mapFromKeysA
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
    , maybe
    , maybeEncoder
    , maybeTraverse
    , maybeTraverseStateT
    , maybeTraverseTask
    , newChan
    , newEmptyMVar
    , newMVar
    , nonEmptyListTraverse
    , procProc
    , procWaitForProcess
    , procWithCreateProcess
    , putMVar
    , putStrLn
    , readChan
    , readMVar
    , replCompleteWord
    , replGetInputLine
    , replGetInputLineWithInitial
    , replHandleInterrupt
    , replRunInputT
    , replWithInterrupt
    , sequenceADict
    , sequenceAList
    , sequenceAListIO
    , sequenceDictMaybe
    , sequenceDictResult
    , sequenceDictResult_
    , sequenceListMaybe
    , sequenceNonemptyListResult
    , someExceptionDecoder
    , someExceptionEncoder
    , stateGet
    , statePut
    , takeMVar
    , takeWhile
    , throw
    , tupleTraverse
    , tupleTraverseStateT
    , unlines
    , unzip3
    , writeChan
    , zipArchiveDecoder
    , zipERelativePath
    , zipFromEntry
    , zipZEntries
    )

import Array exposing (Array)
import Data.IO as IO exposing (IO(..), IORef(..))
import Data.Index as Index
import Data.Map as Dict exposing (Dict)
import Data.NonEmptyList as NE
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Reporting.Result as R
import Reporting.Task as Task exposing (Task)
import Time
import Utils.Crash exposing (crash)


lift : ()
lift =
    Debug.todo "lift"


bsHPut : IO.Handle -> String -> IO ()
bsHPut handle str =
    IO.make (Decode.succeed ()) (IO.HPutStr handle str)


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


dropWhile : (a -> Bool) -> List a -> List a
dropWhile p xs =
    Debug.todo "dropWhile"


takeWhile : (a -> Bool) -> List a -> List a
takeWhile p xs =
    Debug.todo "takeWhile"


mapFromKeysA : (k -> IO v) -> List k -> IO (Dict k v)
mapFromKeysA _ _ =
    Debug.todo "mapFromKeysA"


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


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


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
            crash ("Map.!: given key is not an element in the map (key:`" ++ Debug.toString k ++ "`, keys: `" ++ Debug.toString (Dict.keys items) ++ "`)")


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


head : List a -> a
head items =
    case List.head items of
        Just item ->
            item

        Nothing ->
            crash "*** Exception: Prelude.head: empty list"


init : List a -> List a
init items =
    case List.init items of
        Just initItems ->
            initItems

        Nothing ->
            crash "*** Exception: Prelude.init: empty list"


last : List a -> a
last items =
    case List.last items of
        Just item ->
            item

        Nothing ->
            crash "*** Exception: Prelude.last: empty list"


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


maybe : b -> (a -> b) -> Maybe a -> b
maybe defaultValue f =
    Maybe.map f
        >> Maybe.withDefault defaultValue


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 pairs =
    let
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    List.foldr step ( [], [], [] ) pairs


mapM_ : (a -> IO b) -> List a -> IO ()
mapM_ f =
    let
        c x k =
            IO.bind (\_ -> k) (f x)
    in
    List.foldr c (IO.pure ())


dictMapM_ : (a -> IO b) -> Dict k a -> IO ()
dictMapM_ f =
    let
        c _ x k =
            IO.bind (\_ -> k) (f x)
    in
    Dict.foldl c (IO.pure ())


mapM : (a -> IO b) -> List a -> IO (List b)
mapM =
    listTraverse


mapMArray : (a -> IO b) -> Array a -> IO (Array b)
mapMArray =
    arrayTraverse


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


forMArray : Array a -> (a -> IO b) -> IO (Array b)
forMArray array f =
    mapMArray f array


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


arrayTraverse : (a -> IO b) -> Array a -> IO (Array b)
arrayTraverse f =
    Array.foldl (\a -> IO.bind (\c -> IO.fmap (\va -> Array.push va c) (f a)))
        (IO.pure Array.empty)


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


listGroupBy : (a -> a -> Bool) -> List a -> List (List a)
listGroupBy _ _ =
    Debug.todo "listGroupBy"


listMaximum : List comparable -> comparable
listMaximum xs =
    case List.maximum xs of
        Just x ->
            x

        Nothing ->
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



-- Prelude


putStrLn : String -> IO ()
putStrLn s =
    IO.make (Decode.succeed ()) (IO.PutStrLn s)



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


fpMakeRelative : String -> String -> String
fpMakeRelative _ =
    Debug.todo "fpMakeRelative"


fpAddTrailingPathSeparator : FilePath -> FilePath
fpAddTrailingPathSeparator _ =
    Debug.todo "fpAddTrailingPathSeparator"


fpPathSeparator : Char
fpPathSeparator =
    '/'


fpIsRelative : FilePath -> Bool
fpIsRelative =
    String.startsWith "/"


fpTakeFileName : FilePath -> FilePath
fpTakeFileName filename =
    last (String.split "/" filename)


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
    = LockShared
    | LockExclusive


lockWithFileLock : String -> LockSharedExclusive -> (() -> IO a) -> IO a
lockWithFileLock path mode ioFunc =
    case mode of
        LockShared ->
            crash "lockWithFileLock for `LockShared` is not implemeted!"

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
dirRemoveDirectoryRecursive _ =
    Debug.todo "dirRemoveDirectoryRecursive"


dirDoesDirectoryExist : FilePath -> IO Bool
dirDoesDirectoryExist path =
    IO.make Decode.bool (IO.DirDoesDirectoryExist path)


dirCanonicalizePath : FilePath -> IO FilePath
dirCanonicalizePath path =
    IO.make Decode.string (IO.DirCanonicalizePath path)


dirGetDirectoryContents : FilePath -> IO (List FilePath)
dirGetDirectoryContents _ =
    Debug.todo "dirGetDirectoryContents"


dirWithCurrentDirectory : FilePath -> IO a -> IO a
dirWithCurrentDirectory _ _ =
    Debug.todo "dirWithCurrentDirectory"



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



-- Network.HTTP.Client.Types


type HTTPResponse body
    = HTTPResponse



-- Control.Exception


type SomeException
    = SomeException


type AsyncException
    = UserInterrupt


fromException : SomeException -> Maybe e
fromException _ =
    Debug.todo "fromException"


throw : e -> a
throw _ =
    Debug.todo "throw"



-- Control.Concurrent


type ThreadId
    = ThreadId


forkIO : IO () -> IO ThreadId
forkIO ioArg =
    IO
        (\next ->
            ( IO.Process (Decode.succeed (next ThreadId))
            , IO.NoOp
            , Just ioArg
            )
        )



-- Control.Concurrent.MVar


type MVar a
    = MVar Int


newMVar : (a -> Encode.Value) -> a -> IO (MVar a)
newMVar encoder value =
    newEmptyMVar
        |> IO.bind
            (\mvar ->
                putMVar encoder mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar : Decode.Decoder a -> MVar a -> IO a
readMVar decoder (MVar ref) =
    IO.make decoder (IO.ReadMVar ref)


modifyMVar : Decode.Decoder a -> (a -> Encode.Value) -> MVar a -> (a -> IO ( a, b )) -> IO b
modifyMVar decoder encoder m io =
    takeMVar decoder m
        |> IO.bind io
        |> IO.bind
            (\( a, b ) ->
                putMVar encoder m a
                    |> IO.fmap (\_ -> b)
            )


takeMVar : Decode.Decoder a -> MVar a -> IO a
takeMVar decoder (MVar ref) =
    IO.make decoder (IO.TakeMVar ref)


putMVar : (a -> Encode.Value) -> MVar a -> a -> IO ()
putMVar encoder (MVar ref) value =
    IO.make (Decode.succeed ()) (IO.PutMVar ref (encoder value))


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


newChan : (MVar (ChItem a) -> Encode.Value) -> IO (Chan a)
newChan encoder =
    newEmptyMVar
        |> IO.bind
            (\hole ->
                newMVar encoder hole
                    |> IO.bind
                        (\readVar ->
                            newMVar encoder hole
                                |> IO.fmap
                                    (\writeVar ->
                                        Chan readVar writeVar
                                    )
                        )
            )


readChan : Decode.Decoder a -> Chan a -> IO a
readChan decoder (Chan readVar _) =
    modifyMVar mVarDecoder mVarEncoder readVar <|
        \read_end ->
            readMVar (chItemDecoder decoder) read_end
                |> IO.fmap
                    (\(ChItem val new_read_end) ->
                        -- Use readMVar here, not takeMVar,
                        -- else dupChan doesn't work
                        ( new_read_end, val )
                    )


writeChan : (a -> Encode.Value) -> Chan a -> a -> IO ()
writeChan encoder (Chan _ writeVar) val =
    newEmptyMVar
        |> IO.bind
            (\new_hole ->
                takeMVar mVarDecoder writeVar
                    |> IO.bind
                        (\old_hole ->
                            putMVar (chItemEncoder encoder) old_hole (ChItem val new_hole)
                                |> IO.bind (\_ -> putMVar mVarEncoder writeVar new_hole)
                        )
            )



-- System.Exit


exitFailure : IO a
exitFailure =
    IO.make (Decode.fail "exitFailure") (IO.Exit "exitFailure" 1)


exitSuccess : IO a
exitSuccess =
    IO.make (Decode.fail "exitSuccess") (IO.Exit "exitSuccess" 0)



-- Data.ByteString


bsReadFile : String -> IO String
bsReadFile _ =
    Debug.todo "bsReadFile"



-- Data.ByteString.Builder


builderHPutBuilder : IO.Handle -> String -> IO ()
builderHPutBuilder _ _ =
    Debug.todo "builderHPutBuilder"



-- Data.Binary


binaryDecodeFileOrFail : Decode.Decoder a -> FilePath -> IO (Result ( Int, String ) a)
binaryDecodeFileOrFail decoder filename =
    IO.make
        (Decode.oneOf
            [ Decode.map Ok decoder
            , Decode.succeed (Err ( 0, "Could not find file " ++ filename ))
            ]
        )
        (IO.BinaryDecodeFileOrFail filename)


binaryEncodeFile : (a -> Encode.Value) -> FilePath -> a -> IO ()
binaryEncodeFile encoder path value =
    IO.make (Decode.succeed ()) (IO.Write path (encoder value))



-- System.Console.Haskeline


type ReplSettings
    = ReplSettings
        { historyFile : Maybe String
        , autoAddHistory : Bool
        , complete : ReplCompletionFunc
        }


type ReplInputT a
    = ReplInputT


type ReplCompletion
    = ReplCompletion String String Bool


type ReplCompletionFunc
    = ReplCompletionFunc


replRunInputT : ReplSettings -> ReplInputT a -> IO a
replRunInputT =
    Debug.todo "replRunInputT"


replWithInterrupt : ReplInputT a -> ReplInputT a
replWithInterrupt =
    Debug.todo "replWithInterrupt"


replHandleInterrupt : IO a -> IO a -> IO a
replHandleInterrupt =
    Debug.todo "replHandleInterrupt"


replCompleteWord : Maybe Char -> String -> (String -> IO.StateT a (List ReplCompletion)) -> ReplCompletionFunc
replCompleteWord =
    Debug.todo "replCompleteWord"


replGetInputLine : String -> ReplInputT (Maybe String)
replGetInputLine =
    Debug.todo "replGetInputLine"


replGetInputLineWithInitial : String -> ( String, String ) -> ReplInputT (Maybe String)
replGetInputLineWithInitial =
    Debug.todo "replGetInputLineWithInitial"



-- System.Process


type CreateProcess
    = CreateProcess


type StdStream
    = CreatePipe


type ProcessHandle
    = ProcessHandle


procProc : FilePath -> List String -> { std_in : StdStream } -> CreateProcess
procProc =
    Debug.todo "procProc"


procWithCreateProcess : CreateProcess -> (Maybe IO.Handle -> Maybe IO.Handle -> Maybe IO.Handle -> ProcessHandle -> IO IO.ExitCode) -> IO IO.ExitCode
procWithCreateProcess =
    Debug.todo "procWithCreateProcess"


procWaitForProcess : ProcessHandle -> IO IO.ExitCode
procWaitForProcess =
    Debug.todo "procWaitForProcess"



-- Control.Monad.State.Class


stateGet : IO.StateT s a
stateGet =
    Debug.todo "stateGet"


statePut : s -> IO.StateT s ()
statePut =
    Debug.todo "statePut"



-- ENCODERS and DECODERS


mVarDecoder : Decode.Decoder (MVar a)
mVarDecoder =
    Decode.map MVar Decode.int


mVarEncoder : MVar a -> Encode.Value
mVarEncoder (MVar ref) =
    Encode.int ref


chItemEncoder : (a -> Encode.Value) -> ChItem a -> Encode.Value
chItemEncoder valueEncoder (ChItem value hole) =
    Encode.object
        [ ( "type", Encode.string "ChItem" )
        , ( "value", valueEncoder value )
        , ( "hole", mVarEncoder hole )
        ]


chItemDecoder : Decode.Decoder a -> Decode.Decoder (ChItem a)
chItemDecoder decoder =
    Decode.map2 ChItem (Decode.field "value" decoder) (Decode.field "hole" mVarDecoder)


someExceptionEncoder : SomeException -> Encode.Value
someExceptionEncoder _ =
    Encode.object [ ( "type", Encode.string "SomeException" ) ]


someExceptionDecoder : Decode.Decoder SomeException
someExceptionDecoder =
    Decode.succeed SomeException


httpResponseEncoder : HTTPResponse a -> Encode.Value
httpResponseEncoder _ =
    Encode.object [ ( "type", Encode.string "HTTPResponse" ) ]


httpResponseDecoder : Decode.Decoder (HTTPResponse a)
httpResponseDecoder =
    Decode.succeed HTTPResponse
