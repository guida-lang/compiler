module Utils.Main exposing
    ( AsyncException(..)
    , Chan
    , Chan_ResultBMsgBResultArtifacts
    , Chan_ResultBMsgBResultUnit
    , LockSharedExclusive(..)
    , ReplCompletion(..)
    , ReplCompletionFunc
    , ReplInputT
    , ReplSettings(..)
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
    , filterM
    , find
    , foldM
    , foldl1_
    , foldr1
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
    , mVarDecoder_BB_BResult
    , mVarDecoder_BB_CachedInterface
    , mVarDecoder_CED_Dep
    , mVarDecoder_Maybe_BED_DResult
    , mVarDecoder_Maybe_BED_Status
    , mVarDecoder_Maybe_CECTE_Types
    , mVarEncoder
    , mVarEncoder_BB_BResult
    , mVarEncoder_BB_CachedInterface
    , mVarEncoder_CED_Dep
    , mVarEncoder_Maybe_BED_DResult
    , mVarEncoder_Maybe_BED_Status
    , mVarEncoder_Maybe_CECTE_Types
    , mapFindMin
    , mapFromKeys
    , mapFromListWith
    , mapInsertWith
    , mapIntersectionWith
    , mapIntersectionWithKey
    , mapLookupMin
    , mapM_
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
    , newChan_ResultBMsgBResultArtifacts
    , newChan_ResultBMsgBResultUnit
    , newEmptyMVar
    , newEmptyMVar_BB_BResult
    , newEmptyMVar_BB_CachedInterface
    , newEmptyMVar_BB_Status
    , newEmptyMVar_BB_StatusDict
    , newEmptyMVar_BED_StatusDict
    , newEmptyMVar_CED_Dep
    , newEmptyMVar_DictNameMVarDep
    , newEmptyMVar_DictRawMVarMaybeDResult
    , newEmptyMVar_ListMVar
    , newEmptyMVar_Maybe_BB_Dependencies
    , newEmptyMVar_Maybe_BED_DResult
    , newEmptyMVar_Maybe_BED_Status
    , newEmptyMVar_Maybe_CASTO_GlobalGraph
    , newEmptyMVar_Maybe_CASTO_LocalGraph
    , newEmptyMVar_Maybe_CECTE_Types
    , newEmptyMVar_ResultBMsgBResultArtifacts
    , newEmptyMVar_ResultRegistryProblemEnv
    , newEmptyMVar_Unit
    , newMVar
    , newMVar_BB_BResult
    , newMVar_BB_CachedInterface
    , newMVar_BB_Status
    , newMVar_BB_StatusDict
    , newMVar_BED_StatusDict
    , newMVar_CED_Dep
    , newMVar_DictNameMVarDep
    , newMVar_DictRawMVarMaybeDResult
    , newMVar_ListMVar
    , newMVar_Maybe_BB_Dependencies
    , newMVar_Maybe_CASTO_GlobalGraph
    , newMVar_Maybe_CASTO_LocalGraph
    , newMVar_Maybe_CECTE_Types
    , newMVar_ResultRegistryProblemEnv
    , newMVar_StreamResultBMsgBResultArtifacts
    , newMVar_Unit
    , nonEmptyListTraverse
    , putMVar
    , putMVar_BB_BResult
    , putMVar_BB_CachedInterface
    , putMVar_BB_Status
    , putMVar_BB_StatusDict
    , putMVar_BED_StatusDict
    , putMVar_CED_Dep
    , putMVar_ChItemResultBMsgBResultUnit
    , putMVar_DictNameMVarDep
    , putMVar_DictRawMVarMaybeDResult
    , putMVar_ListMVar
    , putMVar_Maybe_BB_Dependencies
    , putMVar_Maybe_BED_DResult
    , putMVar_Maybe_BED_Status
    , putMVar_Maybe_CASTO_GlobalGraph
    , putMVar_Maybe_CASTO_LocalGraph
    , putMVar_Maybe_CECTE_Types
    , putMVar_ResultRegistryProblemEnv
    , putMVar_StreamResultBMsgBResultUnit
    , putMVar_Unit
    , readChan
    , readChan_ResultBMsgBResultArtifacts
    , readChan_ResultBMsgBResultUnit
    , readMVar
    , readMVar_BB_BResult
    , readMVar_BB_CachedInterface
    , readMVar_BB_Status
    , readMVar_BB_StatusDict
    , readMVar_BED_StatusDict
    , readMVar_CED_Dep
    , readMVar_DictNameMVarDep
    , readMVar_DictRawMVarMaybeDResult
    , readMVar_ListMVar
    , readMVar_Maybe_BB_Dependencies
    , readMVar_Maybe_BED_DResult
    , readMVar_Maybe_BED_Status
    , readMVar_Maybe_CASTO_GlobalGraph
    , readMVar_Maybe_CASTO_LocalGraph
    , readMVar_Maybe_CECTE_Types
    , readMVar_ResultRegistryProblemEnv
    , readMVar_Unit
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
    , takeMVar_BB_CachedInterface
    , takeMVar_BB_StatusDict
    , takeMVar_BED_StatusDict
    , takeMVar_CED_Dep
    , takeMVar_DictNameMVarDep
    , takeMVar_DictRawMVarMaybeDResult
    , takeMVar_ListMVar
    , takeMVar_Maybe_BB_Dependencies
    , takeMVar_Maybe_CECTE_Types
    , takeMVar_ResultRegistryProblemEnv
    , takeMVar_Unit
    , unlines
    , unzip3
    , writeChan
    , writeChan_ResultBMsgBResultArtifacts
    , writeChan_ResultBMsgBResultUnit
    , zipWithM
    )

import Array
import Basics.Extra exposing (flip)
import Builder.Reporting.Task as Task exposing (Task)
import Compiler.Data.Index as Index
import Compiler.Data.NonEmptyList as NE
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Reporting.Result as R
import Control.Monad.State.Strict as State
import Data.Map as Map exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Prelude
import System.Exit as Exit
import System.IO as IO
import Time
import Types as T exposing (IO(..))
import Utils.Crash exposing (crash)


liftInputT : IO () -> ReplInputT ()
liftInputT =
    identity


liftIOInputT : IO a -> ReplInputT a
liftIOInputT =
    identity


fpDropFileName : T.FilePath -> T.FilePath
fpDropFileName path =
    case List.reverse (String.split "/" path) of
        _ :: tail ->
            List.reverse ("" :: tail)
                |> String.join "/"

        [] ->
            ""


fpForwardSlash : T.FilePath -> T.FilePath -> T.FilePath
fpForwardSlash path1 path2 =
    if String.startsWith path1 path2 then
        path2

    else
        path1 ++ "/" ++ path2


fpAddExtension : T.FilePath -> String -> T.FilePath
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


mapFromKeys : (k -> comparable) -> (k -> v) -> List k -> Dict comparable k v
mapFromKeys toComparable f =
    List.map (\k -> ( k, f k ))
        >> Map.fromList toComparable


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


find : (k -> comparable) -> k -> Dict comparable k a -> a
find toComparable k items =
    case Map.get toComparable k items of
        Just item ->
            item

        Nothing ->
            crash "Map.!: given key is not an element in the map"


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


indexedZipWithA : (T.CDI_ZeroBased -> a -> b -> R.RResult info warnings error c) -> List a -> List b -> R.RResult info warnings error (Index.VerifiedList c)
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


mapM_ : (a -> IO b) -> List a -> IO ()
mapM_ f =
    let
        c : a -> IO () -> IO ()
        c x k =
            IO.bind (\_ -> k) (f x)
    in
    List.foldr c (IO.pure ())


dictMapM_ : (k -> k -> Order) -> (a -> IO b) -> Dict c k a -> IO ()
dictMapM_ keyComparison f =
    let
        c : k -> a -> IO () -> IO ()
        c _ x k =
            IO.bind (\_ -> k) (f x)
    in
    Map.foldl keyComparison c (IO.pure ())


maybeMapM : (a -> Maybe b) -> List a -> Maybe (List b)
maybeMapM =
    listMaybeTraverse


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


mapTraverse : (k -> comparable) -> (k -> k -> Order) -> (a -> IO b) -> Dict comparable k a -> IO (Dict comparable k b)
mapTraverse toComparable keyComparison f =
    mapTraverseWithKey toComparable keyComparison (\_ -> f)


mapTraverseWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> IO b) -> Dict comparable k a -> IO (Dict comparable k b)
mapTraverseWithKey toComparable keyComparison f =
    Map.foldl keyComparison
        (\k a -> IO.bind (\c -> IO.fmap (\va -> Map.insert toComparable k va c) (f k a)))
        (IO.pure Map.empty)


mapTraverseResult : (k -> comparable) -> (k -> k -> Order) -> (a -> Result e b) -> Dict comparable k a -> Result e (Dict comparable k b)
mapTraverseResult toComparable keyComparison f =
    mapTraverseWithKeyResult toComparable keyComparison (\_ -> f)


mapTraverseWithKeyResult : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> Result e b) -> Dict comparable k a -> Result e (Dict comparable k b)
mapTraverseWithKeyResult toComparable keyComparison f =
    Map.foldl keyComparison
        (\k a -> Result.map2 (Map.insert toComparable k) (f k a))
        (Ok Map.empty)


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


fpMakeRelative : T.FilePath -> T.FilePath -> T.FilePath
fpMakeRelative root path =
    if String.startsWith path root then
        String.dropLeft (String.length root) path

    else
        path


fpAddTrailingPathSeparator : T.FilePath -> T.FilePath
fpAddTrailingPathSeparator path =
    if String.endsWith "/" path then
        path

    else
        path ++ "/"


fpPathSeparator : Char
fpPathSeparator =
    '/'


fpIsRelative : T.FilePath -> Bool
fpIsRelative =
    String.startsWith "/"


fpTakeFileName : T.FilePath -> T.FilePath
fpTakeFileName filename =
    Prelude.last (String.split "/" filename)


fpSplitFileName : T.FilePath -> ( String, String )
fpSplitFileName filename =
    case List.reverse (String.indexes "/" filename) of
        index :: _ ->
            ( String.left (index + 1) filename, String.dropLeft (index + 1) filename )

        _ ->
            ( "./", filename )


fpTakeExtension : T.FilePath -> String
fpTakeExtension =
    Tuple.second << fpSplitExtension


fpTakeDirectory : T.FilePath -> T.FilePath
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


lockFile : T.FilePath -> IO ()
lockFile path =
    IO (\_ s -> ( s, T.LockFile IO.pure path ))


unlockFile : T.FilePath -> IO ()
unlockFile path =
    IO (\_ s -> ( s, T.UnlockFile IO.pure path ))



-- System.Directory


dirDoesFileExist : T.FilePath -> IO Bool
dirDoesFileExist filename =
    IO (\_ s -> ( s, T.DirDoesFileExist IO.pure filename ))


dirFindExecutable : T.FilePath -> IO (Maybe T.FilePath)
dirFindExecutable filename =
    IO (\_ s -> ( s, T.DirFindExecutable IO.pure filename ))


dirCreateDirectoryIfMissing : Bool -> T.FilePath -> IO ()
dirCreateDirectoryIfMissing createParents filename =
    IO (\_ s -> ( s, T.DirCreateDirectoryIfMissing IO.pure createParents filename ))


dirGetCurrentDirectory : IO String
dirGetCurrentDirectory =
    IO (\_ s -> ( s, T.Pure s.currentDirectory ))


dirGetAppUserDataDirectory : T.FilePath -> IO T.FilePath
dirGetAppUserDataDirectory filename =
    IO (\_ s -> ( s, T.Pure (s.homedir ++ "/." ++ filename) ))


dirGetModificationTime : T.FilePath -> IO Time.Posix
dirGetModificationTime filename =
    IO (\_ s -> ( s, T.DirGetModificationTime IO.pure filename ))
        |> IO.fmap Time.millisToPosix


dirRemoveFile : T.FilePath -> IO ()
dirRemoveFile path =
    IO (\_ s -> ( s, T.DirRemoveFile IO.pure path ))


dirRemoveDirectoryRecursive : T.FilePath -> IO ()
dirRemoveDirectoryRecursive path =
    IO (\_ s -> ( s, T.DirRemoveDirectoryRecursive IO.pure path ))


dirDoesDirectoryExist : T.FilePath -> IO Bool
dirDoesDirectoryExist path =
    IO (\_ s -> ( s, T.DirDoesDirectoryExist IO.pure path ))


dirCanonicalizePath : T.FilePath -> IO T.FilePath
dirCanonicalizePath path =
    IO (\_ s -> ( s, T.DirCanonicalizePath IO.pure path ))


dirWithCurrentDirectory : T.FilePath -> IO a -> IO a
dirWithCurrentDirectory dir action =
    dirGetCurrentDirectory
        |> IO.bind
            (\currentDir ->
                bracket_
                    (IO (\_ s -> ( s, T.DirWithCurrentDirectory IO.pure dir )))
                    (IO (\_ s -> ( s, T.DirWithCurrentDirectory IO.pure currentDir )))
                    action
            )



-- System.Environment


envLookupEnv : String -> IO (Maybe String)
envLookupEnv name =
    IO (\_ s -> ( s, T.Pure (Dict.get name s.envVars) ))


envGetProgName : IO String
envGetProgName =
    IO (\_ s -> ( s, T.Pure s.progName ))


envGetArgs : IO (List String)
envGetArgs =
    IO (\_ s -> ( s, T.Pure s.args ))



-- Codec.Archive.Zip


type ZipArchive
    = ZipArchive (List ZipEntry)


type ZipEntry
    = ZipEntry
        { eRelativePath : T.FilePath
        , eData : String
        }



-- Network.HTTP.Client


httpResponseStatus : T.UM_HttpResponse body -> T.UM_HttpStatus
httpResponseStatus (T.UM_HttpResponse { responseStatus }) =
    responseStatus


httpStatusCode : T.UM_HttpStatus -> Int
httpStatusCode (T.UM_HttpStatus statusCode _) =
    statusCode


httpResponseHeaders : T.UM_HttpResponse body -> T.UM_HttpResponseHeaders
httpResponseHeaders (T.UM_HttpResponse { responseHeaders }) =
    responseHeaders


httpHLocation : String
httpHLocation =
    "Location"



-- Control.Exception


type AsyncException
    = UserInterrupt


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
    IO (\_ s -> ( s, T.ForkIO (\() -> IO.pure ThreadId) ioArg ))



-- Control.Concurrent.MVar


newMVar : (a -> Encode.Value) -> a -> IO (T.MVar a)
newMVar encoder value =
    newEmptyMVar
        |> IO.bind
            (\mvar ->
                putMVar encoder mvar value
                    |> IO.fmap (\_ -> mvar)
            )


newMVar_StreamResultBMsgBResultUnit : T.MVar_ChItemResultBMsgBResultUnit -> IO T.MVar_StreamResultBMsgBResultUnit
newMVar_StreamResultBMsgBResultUnit value =
    newEmptyMVar_StreamResultBMsgBResultUnit
        |> IO.bind
            (\mvar ->
                putMVar_StreamResultBMsgBResultUnit mvar value
                    |> IO.fmap (\_ -> mvar)
            )


newMVar_StreamResultBMsgBResultArtifacts : T.MVar_ChItemResultBMsgBResultArtifacts -> IO T.MVar_StreamResultBMsgBResultArtifacts
newMVar_StreamResultBMsgBResultArtifacts value =
    newEmptyMVar_StreamResultBMsgBResultArtifacts
        |> IO.bind
            (\mvar ->
                putMVar_StreamResultBMsgBResultArtifacts mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar : Decode.Decoder a -> T.MVar a -> IO a
readMVar decoder (T.MVar ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber index ] } s.mVars }
                            , T.ReadMVar IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar: invalid ref"
        )
        |> IO.fmap
            (\encodedValue ->
                case Decode.decodeValue decoder encodedValue of
                    Ok value ->
                        value

                    Err _ ->
                        crash "Utils.Main.readMVar: invalid value"
            )


readMVar_ChItemResultBMsgBResultUnit : T.MVar_ChItemResultBMsgBResultUnit -> IO T.ChItem_ResultBMsgBResultUnit
readMVar_ChItemResultBMsgBResultUnit (T.MVar_ChItemResultBMsgBResultUnit ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_ChItemResultBMsgBResultUnit of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_ChItemResultBMsgBResultUnit IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_ChItemResultBMsgBResultUnit = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_ChItemResultBMsgBResultUnit index ] } s.mVars_ChItemResultBMsgBResultUnit }
                            , T.ReadMVar_ChItemResultBMsgBResultUnit IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_ChItemResultBMsgBResultUnit: invalid ref"
        )


readMVar_ChItemResultBMsgBResultArtifacts : T.MVar_ChItemResultBMsgBResultArtifacts -> IO T.ChItem_ResultBMsgBResultArtifacts
readMVar_ChItemResultBMsgBResultArtifacts (T.MVar_ChItemResultBMsgBResultArtifacts ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_ChItemResultBMsgBResultArtifacts of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_ChItemResultBMsgBResultArtifacts IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_ChItemResultBMsgBResultArtifacts = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_ChItemResultBMsgBResultArtifacts index ] } s.mVars_ChItemResultBMsgBResultArtifacts }
                            , T.ReadMVar_ChItemResultBMsgBResultArtifacts IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_ChItemResultBMsgBResultArtifacts: invalid ref"
        )


modifyMVar : Decode.Decoder a -> (a -> Encode.Value) -> T.MVar a -> (a -> IO ( a, b )) -> IO b
modifyMVar decoder encoder m io =
    takeMVar decoder m
        |> IO.bind io
        |> IO.bind
            (\( a, b ) ->
                putMVar encoder m a
                    |> IO.fmap (\_ -> b)
            )


modifyMVar_StreamResultBMsgBResultUnit_ResultBMsgBResultUnit :
    T.MVar_StreamResultBMsgBResultUnit
    -> (T.MVar_ChItemResultBMsgBResultUnit -> IO ( T.MVar_ChItemResultBMsgBResultUnit, Result T.BR_BMsg (T.BR_BResult ()) ))
    -> IO (Result T.BR_BMsg (T.BR_BResult ()))
modifyMVar_StreamResultBMsgBResultUnit_ResultBMsgBResultUnit m io =
    takeMVar_StreamResultBMsgBResultUnit m
        |> IO.bind io
        |> IO.bind
            (\( a, b ) ->
                putMVar_StreamResultBMsgBResultUnit m a
                    |> IO.fmap (\_ -> b)
            )


modifyMVar_StreamResultBMsgBResultArtifacts_ResultBMsgBResultArtifacts :
    T.MVar_StreamResultBMsgBResultArtifacts
    -> (T.MVar_ChItemResultBMsgBResultArtifacts -> IO ( T.MVar_ChItemResultBMsgBResultArtifacts, Result T.BR_BMsg (T.BR_BResult T.BB_Artifacts) ))
    -> IO (Result T.BR_BMsg (T.BR_BResult T.BB_Artifacts))
modifyMVar_StreamResultBMsgBResultArtifacts_ResultBMsgBResultArtifacts m io =
    takeMVar_StreamResultBMsgBResultArtifacts m
        |> IO.bind io
        |> IO.bind
            (\( a, b ) ->
                putMVar_StreamResultBMsgBResultArtifacts m a
                    |> IO.fmap (\_ -> b)
            )


takeMVar : Decode.Decoder a -> T.MVar a -> IO a
takeMVar decoder (T.MVar ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars }
                                    , T.TakeMVar IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars = Array.set ref { mVar | value = Nothing } s.mVars }
                                    , T.TakeMVar IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber index ] } s.mVars }
                            , T.TakeMVar IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar: invalid ref"
        )
        |> IO.fmap
            (\encodedValue ->
                case Decode.decodeValue decoder encodedValue of
                    Ok value ->
                        value

                    Err _ ->
                        crash "Utils.Main.takeMVar: invalid value"
            )


takeMVar_StreamResultBMsgBResultUnit : T.MVar_StreamResultBMsgBResultUnit -> IO T.MVar_ChItemResultBMsgBResultUnit
takeMVar_StreamResultBMsgBResultUnit (T.MVar_StreamResultBMsgBResultUnit ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_StreamResultBMsgBResultUnit of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_StreamResultBMsgBResultUnit putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_StreamResultBMsgBResultUnit = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_StreamResultBMsgBResultUnit }
                                    , T.TakeMVar_StreamResultBMsgBResultUnit IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_StreamResultBMsgBResultUnit = Array.set ref { mVar | value = Nothing } s.mVars_StreamResultBMsgBResultUnit }
                                    , T.TakeMVar_StreamResultBMsgBResultUnit IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_StreamResultBMsgBResultUnit = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_StreamResultBMsgBResultUnit index ] } s.mVars_StreamResultBMsgBResultUnit }
                            , T.TakeMVar_StreamResultBMsgBResultUnit IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_StreamResultBMsgBResultUnit: invalid ref"
        )


takeMVar_StreamResultBMsgBResultArtifacts : T.MVar_StreamResultBMsgBResultArtifacts -> IO T.MVar_ChItemResultBMsgBResultArtifacts
takeMVar_StreamResultBMsgBResultArtifacts (T.MVar_StreamResultBMsgBResultArtifacts ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_StreamResultBMsgBResultArtifacts of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_StreamResultBMsgBResultArtifacts putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_StreamResultBMsgBResultArtifacts = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_StreamResultBMsgBResultArtifacts }
                                    , T.TakeMVar_StreamResultBMsgBResultArtifacts IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_StreamResultBMsgBResultArtifacts = Array.set ref { mVar | value = Nothing } s.mVars_StreamResultBMsgBResultArtifacts }
                                    , T.TakeMVar_StreamResultBMsgBResultArtifacts IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_StreamResultBMsgBResultArtifacts = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_StreamResultBMsgBResultArtifacts index ] } s.mVars_StreamResultBMsgBResultArtifacts }
                            , T.TakeMVar_StreamResultBMsgBResultArtifacts IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_StreamResultBMsgBResultArtifacts: invalid ref"
        )


putMVar : (a -> Encode.Value) -> T.MVar a -> a -> IO ()
putMVar encoder (T.MVar ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber index (encoder value) ] } s.mVars }
                            , T.PutMVar IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just (encoder value) } s.mVars }
                            , T.PutMVar IO.pure readIndexes (Just (encoder value))
                            )

                Nothing ->
                    crash "Utils.Main.putMVar: invalid ref"
        )


putMVar_StreamResultBMsgBResultUnit : T.MVar_StreamResultBMsgBResultUnit -> T.MVar_ChItemResultBMsgBResultUnit -> IO ()
putMVar_StreamResultBMsgBResultUnit (T.MVar_StreamResultBMsgBResultUnit ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_StreamResultBMsgBResultUnit of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_StreamResultBMsgBResultUnit = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_StreamResultBMsgBResultUnit index value ] } s.mVars_StreamResultBMsgBResultUnit }
                            , T.PutMVar_StreamResultBMsgBResultUnit IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_StreamResultBMsgBResultUnit readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_StreamResultBMsgBResultUnit = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_StreamResultBMsgBResultUnit }
                            , T.PutMVar_StreamResultBMsgBResultUnit IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_StreamResultBMsgBResultUnit: invalid ref"
        )


putMVar_ChItemResultBMsgBResultUnit : T.MVar_ChItemResultBMsgBResultUnit -> T.ChItem_ResultBMsgBResultUnit -> IO ()
putMVar_ChItemResultBMsgBResultUnit (T.MVar_ChItemResultBMsgBResultUnit ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_ChItemResultBMsgBResultUnit of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_ChItemResultBMsgBResultUnit = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_ChItemResultBMsgBResultUnit index value ] } s.mVars_ChItemResultBMsgBResultUnit }
                            , T.PutMVar_ChItemResultBMsgBResultUnit IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_ChItemResultBMsgBResultUnit readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_ChItemResultBMsgBResultUnit = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_ChItemResultBMsgBResultUnit }
                            , T.PutMVar_ChItemResultBMsgBResultUnit IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_ChItemResultBMsgBResultUnit: invalid ref"
        )


putMVar_StreamResultBMsgBResultArtifacts : T.MVar_StreamResultBMsgBResultArtifacts -> T.MVar_ChItemResultBMsgBResultArtifacts -> IO ()
putMVar_StreamResultBMsgBResultArtifacts (T.MVar_StreamResultBMsgBResultArtifacts ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_StreamResultBMsgBResultArtifacts of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_StreamResultBMsgBResultArtifacts = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_StreamResultBMsgBResultArtifacts index value ] } s.mVars_StreamResultBMsgBResultArtifacts }
                            , T.PutMVar_StreamResultBMsgBResultArtifacts IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_StreamResultBMsgBResultArtifacts readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_StreamResultBMsgBResultArtifacts = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_StreamResultBMsgBResultArtifacts }
                            , T.PutMVar_StreamResultBMsgBResultArtifacts IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_StreamResultBMsgBResultArtifacts: invalid ref"
        )


putMVar_ChItemResultBMsgBResultArtifacts : T.MVar_ChItemResultBMsgBResultArtifacts -> T.ChItem_ResultBMsgBResultArtifacts -> IO ()
putMVar_ChItemResultBMsgBResultArtifacts (T.MVar_ChItemResultBMsgBResultArtifacts ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_ChItemResultBMsgBResultArtifacts of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_ChItemResultBMsgBResultArtifacts = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_ChItemResultBMsgBResultArtifacts index value ] } s.mVars_ChItemResultBMsgBResultArtifacts }
                            , T.PutMVar_ChItemResultBMsgBResultArtifacts IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_ChItemResultBMsgBResultArtifacts readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_ChItemResultBMsgBResultArtifacts = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_ChItemResultBMsgBResultArtifacts }
                            , T.PutMVar_ChItemResultBMsgBResultArtifacts IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_ChItemResultBMsgBResultArtifacts: invalid ref"
        )


newEmptyMVar : IO (T.MVar a)
newEmptyMVar =
    IO
        (\_ s ->
            ( { s | mVars = Array.push { subscribers = [], value = Nothing } s.mVars }
            , T.NewEmptyMVar IO.pure (Array.length s.mVars)
            )
        )
        |> IO.fmap T.MVar


newEmptyMVar_StreamResultBMsgBResultUnit : IO T.MVar_StreamResultBMsgBResultUnit
newEmptyMVar_StreamResultBMsgBResultUnit =
    IO
        (\_ s ->
            ( { s | mVars_StreamResultBMsgBResultUnit = Array.push { subscribers = [], value = Nothing } s.mVars_StreamResultBMsgBResultUnit }
            , T.NewEmptyMVar_StreamResultBMsgBResultUnit IO.pure (Array.length s.mVars_StreamResultBMsgBResultUnit)
            )
        )
        |> IO.fmap T.MVar_StreamResultBMsgBResultUnit


newEmptyMVar_ChItemResultBMsgBResultUnit : IO T.MVar_ChItemResultBMsgBResultUnit
newEmptyMVar_ChItemResultBMsgBResultUnit =
    IO
        (\_ s ->
            ( { s | mVars_ChItemResultBMsgBResultUnit = Array.push { subscribers = [], value = Nothing } s.mVars_ChItemResultBMsgBResultUnit }
            , T.NewEmptyMVar_ChItemResultBMsgBResultUnit IO.pure (Array.length s.mVars_ChItemResultBMsgBResultUnit)
            )
        )
        |> IO.fmap T.MVar_ChItemResultBMsgBResultUnit


newEmptyMVar_StreamResultBMsgBResultArtifacts : IO T.MVar_StreamResultBMsgBResultArtifacts
newEmptyMVar_StreamResultBMsgBResultArtifacts =
    IO
        (\_ s ->
            ( { s | mVars_StreamResultBMsgBResultArtifacts = Array.push { subscribers = [], value = Nothing } s.mVars_StreamResultBMsgBResultArtifacts }
            , T.NewEmptyMVar_StreamResultBMsgBResultArtifacts IO.pure (Array.length s.mVars_StreamResultBMsgBResultArtifacts)
            )
        )
        |> IO.fmap T.MVar_StreamResultBMsgBResultArtifacts


newEmptyMVar_ChItemResultBMsgBResultArtifacts : IO T.MVar_ChItemResultBMsgBResultArtifacts
newEmptyMVar_ChItemResultBMsgBResultArtifacts =
    IO
        (\_ s ->
            ( { s | mVars_ChItemResultBMsgBResultArtifacts = Array.push { subscribers = [], value = Nothing } s.mVars_ChItemResultBMsgBResultArtifacts }
            , T.NewEmptyMVar_ChItemResultBMsgBResultArtifacts IO.pure (Array.length s.mVars_ChItemResultBMsgBResultArtifacts)
            )
        )
        |> IO.fmap T.MVar_ChItemResultBMsgBResultArtifacts


newEmptyMVar_ResultBMsgBResultArtifacts : IO T.MVar_ResultBMsgBResultArtifacts
newEmptyMVar_ResultBMsgBResultArtifacts =
    IO
        (\_ s ->
            ( { s | mVars_ResultBMsgBResultArtifacts = Array.push { subscribers = [], value = Nothing } s.mVars_ResultBMsgBResultArtifacts }
            , T.NewEmptyMVar_ResultBMsgBResultArtifacts IO.pure (Array.length s.mVars_ResultBMsgBResultArtifacts)
            )
        )
        |> IO.fmap T.MVar_ResultBMsgBResultArtifacts



-- Control.Concurrent.MVar (Maybe T.BED_Status)


readMVar_Maybe_BED_Status : T.MVar_Maybe_BED_Status -> IO (Maybe T.BED_Status)
readMVar_Maybe_BED_Status (T.MVar_Maybe_BED_Status ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_BED_Status of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Maybe_BED_Status IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Maybe_BED_Status = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Maybe_BED_Status index ] } s.mVars_Maybe_BED_Status }
                            , T.ReadMVar_Maybe_BED_Status IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar: invalid ref"
        )


putMVar_Maybe_BED_Status : T.MVar_Maybe_BED_Status -> Maybe T.BED_Status -> IO ()
putMVar_Maybe_BED_Status (T.MVar_Maybe_BED_Status ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_BED_Status of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Maybe_BED_Status = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Maybe_BED_Status index value ] } s.mVars_Maybe_BED_Status }
                            , T.PutMVar_Maybe_BED_Status IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Maybe_BED_Status readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Maybe_BED_Status = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Maybe_BED_Status }
                            , T.PutMVar_Maybe_BED_Status IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar: invalid ref"
        )


newEmptyMVar_Maybe_BED_Status : IO T.MVar_Maybe_BED_Status
newEmptyMVar_Maybe_BED_Status =
    IO
        (\_ s ->
            ( { s | mVars_Maybe_BED_Status = Array.push { subscribers = [], value = Nothing } s.mVars_Maybe_BED_Status }
            , T.NewEmptyMVar_Maybe_BED_Status IO.pure (Array.length s.mVars_Maybe_BED_Status)
            )
        )
        |> IO.fmap T.MVar_Maybe_BED_Status



-- Control.Concurrent.MVar (Maybe T.BED_DResult)


readMVar_Maybe_BED_DResult : T.MVar_Maybe_BED_DResult -> IO (Maybe T.BED_DResult)
readMVar_Maybe_BED_DResult (T.MVar_Maybe_BED_DResult ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_BED_DResult of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Maybe_BED_DResult IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Maybe_BED_DResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Maybe_BED_DResult index ] } s.mVars_Maybe_BED_DResult }
                            , T.ReadMVar_Maybe_BED_DResult IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar: invalid ref"
        )


putMVar_Maybe_BED_DResult : T.MVar_Maybe_BED_DResult -> Maybe T.BED_DResult -> IO ()
putMVar_Maybe_BED_DResult (T.MVar_Maybe_BED_DResult ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_BED_DResult of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Maybe_BED_DResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Maybe_BED_DResult index value ] } s.mVars_Maybe_BED_DResult }
                            , T.PutMVar_Maybe_BED_DResult IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Maybe_BED_DResult readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Maybe_BED_DResult = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Maybe_BED_DResult }
                            , T.PutMVar_Maybe_BED_DResult IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar: invalid ref"
        )


newEmptyMVar_Maybe_BED_DResult : IO T.MVar_Maybe_BED_DResult
newEmptyMVar_Maybe_BED_DResult =
    IO
        (\_ s ->
            ( { s | mVars_Maybe_BED_DResult = Array.push { subscribers = [], value = Nothing } s.mVars_Maybe_BED_DResult }
            , T.NewEmptyMVar_Maybe_BED_DResult IO.pure (Array.length s.mVars_Maybe_BED_DResult)
            )
        )
        |> IO.fmap T.MVar_Maybe_BED_DResult



-- Control.Concurrent.MVar (Maybe T.CASTO_LocalGraph)


newMVar_Maybe_CASTO_LocalGraph : Maybe T.CASTO_LocalGraph -> IO T.MVar_Maybe_CASTO_LocalGraph
newMVar_Maybe_CASTO_LocalGraph value =
    newEmptyMVar_Maybe_CASTO_LocalGraph
        |> IO.bind
            (\mvar ->
                putMVar_Maybe_CASTO_LocalGraph mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_Maybe_CASTO_LocalGraph : T.MVar_Maybe_CASTO_LocalGraph -> IO (Maybe T.CASTO_LocalGraph)
readMVar_Maybe_CASTO_LocalGraph (T.MVar_Maybe_CASTO_LocalGraph ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_CASTO_LocalGraph of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Maybe_CASTO_LocalGraph IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Maybe_CASTO_LocalGraph = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Maybe_CASTO_LocalGraph index ] } s.mVars_Maybe_CASTO_LocalGraph }
                            , T.ReadMVar_Maybe_CASTO_LocalGraph IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_Maybe_CASTO_LocalGraph: invalid ref"
        )


putMVar_Maybe_CASTO_LocalGraph : T.MVar_Maybe_CASTO_LocalGraph -> Maybe T.CASTO_LocalGraph -> IO ()
putMVar_Maybe_CASTO_LocalGraph (T.MVar_Maybe_CASTO_LocalGraph ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_CASTO_LocalGraph of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Maybe_CASTO_LocalGraph = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Maybe_CASTO_LocalGraph index value ] } s.mVars_Maybe_CASTO_LocalGraph }
                            , T.PutMVar_Maybe_CASTO_LocalGraph IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Maybe_CASTO_LocalGraph readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Maybe_CASTO_LocalGraph = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Maybe_CASTO_LocalGraph }
                            , T.PutMVar_Maybe_CASTO_LocalGraph IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_Maybe_CASTO_LocalGraph: invalid ref"
        )


newEmptyMVar_Maybe_CASTO_LocalGraph : IO T.MVar_Maybe_CASTO_LocalGraph
newEmptyMVar_Maybe_CASTO_LocalGraph =
    IO
        (\_ s ->
            ( { s | mVars_Maybe_CASTO_LocalGraph = Array.push { subscribers = [], value = Nothing } s.mVars_Maybe_CASTO_LocalGraph }
            , T.NewEmptyMVar_Maybe_CASTO_LocalGraph IO.pure (Array.length s.mVars_Maybe_CASTO_LocalGraph)
            )
        )
        |> IO.fmap T.MVar_Maybe_CASTO_LocalGraph



-- Control.Concurrent.MVar (Maybe T.CASTO_GlobalGraph)


newMVar_Maybe_CASTO_GlobalGraph : Maybe T.CASTO_GlobalGraph -> IO T.MVar_Maybe_CASTO_GlobalGraph
newMVar_Maybe_CASTO_GlobalGraph value =
    newEmptyMVar_Maybe_CASTO_GlobalGraph
        |> IO.bind
            (\mvar ->
                putMVar_Maybe_CASTO_GlobalGraph mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_Maybe_CASTO_GlobalGraph : T.MVar_Maybe_CASTO_GlobalGraph -> IO (Maybe T.CASTO_GlobalGraph)
readMVar_Maybe_CASTO_GlobalGraph (T.MVar_Maybe_CASTO_GlobalGraph ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_CASTO_GlobalGraph of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Maybe_CASTO_GlobalGraph IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Maybe_CASTO_GlobalGraph = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Maybe_CASTO_GlobalGraph index ] } s.mVars_Maybe_CASTO_GlobalGraph }
                            , T.ReadMVar_Maybe_CASTO_GlobalGraph IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_Maybe_CASTO_GlobalGraph: invalid ref"
        )


putMVar_Maybe_CASTO_GlobalGraph : T.MVar_Maybe_CASTO_GlobalGraph -> Maybe T.CASTO_GlobalGraph -> IO ()
putMVar_Maybe_CASTO_GlobalGraph (T.MVar_Maybe_CASTO_GlobalGraph ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_CASTO_GlobalGraph of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Maybe_CASTO_GlobalGraph = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Maybe_CASTO_GlobalGraph index value ] } s.mVars_Maybe_CASTO_GlobalGraph }
                            , T.PutMVar_Maybe_CASTO_GlobalGraph IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Maybe_CASTO_GlobalGraph readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Maybe_CASTO_GlobalGraph = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Maybe_CASTO_GlobalGraph }
                            , T.PutMVar_Maybe_CASTO_GlobalGraph IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_Maybe_CASTO_GlobalGraph: invalid ref"
        )


newEmptyMVar_Maybe_CASTO_GlobalGraph : IO T.MVar_Maybe_CASTO_GlobalGraph
newEmptyMVar_Maybe_CASTO_GlobalGraph =
    IO
        (\_ s ->
            ( { s | mVars_Maybe_CASTO_GlobalGraph = Array.push { subscribers = [], value = Nothing } s.mVars_Maybe_CASTO_GlobalGraph }
            , T.NewEmptyMVar_Maybe_CASTO_GlobalGraph IO.pure (Array.length s.mVars_Maybe_CASTO_GlobalGraph)
            )
        )
        |> IO.fmap T.MVar_Maybe_CASTO_GlobalGraph



-- Control.Concurrent.MVar (T.BB_BResult)


newMVar_BB_BResult : T.BB_BResult -> IO T.MVar_BB_BResult
newMVar_BB_BResult value =
    newEmptyMVar_BB_BResult
        |> IO.bind
            (\mvar ->
                putMVar_BB_BResult mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_BB_BResult : T.MVar_BB_BResult -> IO T.BB_BResult
readMVar_BB_BResult (T.MVar_BB_BResult ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_BResult of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_BB_BResult IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_BB_BResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_BB_BResult index ] } s.mVars_BB_BResult }
                            , T.ReadMVar_BB_BResult IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_BB_BResult: invalid ref"
        )


putMVar_BB_BResult : T.MVar_BB_BResult -> T.BB_BResult -> IO ()
putMVar_BB_BResult (T.MVar_BB_BResult ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_BResult of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_BB_BResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_BB_BResult index value ] } s.mVars_BB_BResult }
                            , T.PutMVar_BB_BResult IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_BB_BResult readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_BB_BResult = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_BB_BResult }
                            , T.PutMVar_BB_BResult IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_BB_BResult: invalid ref"
        )


newEmptyMVar_BB_BResult : IO T.MVar_BB_BResult
newEmptyMVar_BB_BResult =
    IO
        (\_ s ->
            ( { s | mVars_BB_BResult = Array.push { subscribers = [], value = Nothing } s.mVars_BB_BResult }
            , T.NewEmptyMVar_BB_BResult IO.pure (Array.length s.mVars_BB_BResult)
            )
        )
        |> IO.fmap T.MVar_BB_BResult



-- Control.Concurrent.MVar (T.BB_Status)


newMVar_BB_Status : T.BB_Status -> IO T.MVar_BB_Status
newMVar_BB_Status value =
    newEmptyMVar_BB_Status
        |> IO.bind
            (\mvar ->
                putMVar_BB_Status mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_BB_Status : T.MVar_BB_Status -> IO T.BB_Status
readMVar_BB_Status (T.MVar_BB_Status ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_Status of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_BB_Status IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_BB_Status = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_BB_Status index ] } s.mVars_BB_Status }
                            , T.ReadMVar_BB_Status IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_BB_Status: invalid ref"
        )


putMVar_BB_Status : T.MVar_BB_Status -> T.BB_Status -> IO ()
putMVar_BB_Status (T.MVar_BB_Status ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_Status of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_BB_Status = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_BB_Status index value ] } s.mVars_BB_Status }
                            , T.PutMVar_BB_Status IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_BB_Status readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_BB_Status = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_BB_Status }
                            , T.PutMVar_BB_Status IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_BB_Status: invalid ref"
        )


newEmptyMVar_BB_Status : IO T.MVar_BB_Status
newEmptyMVar_BB_Status =
    IO
        (\_ s ->
            ( { s | mVars_BB_Status = Array.push { subscribers = [], value = Nothing } s.mVars_BB_Status }
            , T.NewEmptyMVar_BB_Status IO.pure (Array.length s.mVars_BB_Status)
            )
        )
        |> IO.fmap T.MVar_BB_Status



-- Control.Concurrent.MVar (T.BB_StatusDict)


newMVar_BB_StatusDict : T.BB_StatusDict -> IO T.MVar_BB_StatusDict
newMVar_BB_StatusDict value =
    newEmptyMVar_BB_StatusDict
        |> IO.bind
            (\mvar ->
                putMVar_BB_StatusDict mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_BB_StatusDict : T.MVar_BB_StatusDict -> IO T.BB_StatusDict
readMVar_BB_StatusDict (T.MVar_BB_StatusDict ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_StatusDict of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_BB_StatusDict IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_BB_StatusDict = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_BB_StatusDict index ] } s.mVars_BB_StatusDict }
                            , T.ReadMVar_BB_StatusDict IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_BB_StatusDict: invalid ref"
        )


takeMVar_BB_StatusDict : T.MVar_BB_StatusDict -> IO T.BB_StatusDict
takeMVar_BB_StatusDict (T.MVar_BB_StatusDict ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_StatusDict of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_BB_StatusDict putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_BB_StatusDict = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_BB_StatusDict }
                                    , T.TakeMVar_BB_StatusDict IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_BB_StatusDict = Array.set ref { mVar | value = Nothing } s.mVars_BB_StatusDict }
                                    , T.TakeMVar_BB_StatusDict IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_BB_StatusDict = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_BB_StatusDict index ] } s.mVars_BB_StatusDict }
                            , T.TakeMVar_BB_StatusDict IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_BB_StatusDict: invalid ref"
        )


putMVar_BB_StatusDict : T.MVar_BB_StatusDict -> T.BB_StatusDict -> IO ()
putMVar_BB_StatusDict (T.MVar_BB_StatusDict ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_StatusDict of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_BB_StatusDict = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_BB_StatusDict index value ] } s.mVars_BB_StatusDict }
                            , T.PutMVar_BB_StatusDict IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_BB_StatusDict readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_BB_StatusDict = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_BB_StatusDict }
                            , T.PutMVar_BB_StatusDict IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_BB_StatusDict: invalid ref"
        )


newEmptyMVar_BB_StatusDict : IO T.MVar_BB_StatusDict
newEmptyMVar_BB_StatusDict =
    IO
        (\_ s ->
            ( { s | mVars_BB_StatusDict = Array.push { subscribers = [], value = Nothing } s.mVars_BB_StatusDict }
            , T.NewEmptyMVar_BB_StatusDict IO.pure (Array.length s.mVars_BB_StatusDict)
            )
        )
        |> IO.fmap T.MVar_BB_StatusDict



-- Control.Concurrent.MVar (Result T.BRE_RegistryProblem T.BDS_Env)


newMVar_ResultRegistryProblemEnv : Result T.BRE_RegistryProblem T.BDS_Env -> IO T.MVar_ResultRegistryProblemEnv
newMVar_ResultRegistryProblemEnv value =
    newEmptyMVar_ResultRegistryProblemEnv
        |> IO.bind
            (\mvar ->
                putMVar_ResultRegistryProblemEnv mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_ResultRegistryProblemEnv : T.MVar_ResultRegistryProblemEnv -> IO (Result T.BRE_RegistryProblem T.BDS_Env)
readMVar_ResultRegistryProblemEnv (T.MVar_ResultRegistryProblemEnv ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_ResultRegistryProblemEnv of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_ResultRegistryProblemEnv IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_ResultRegistryProblemEnv = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_ResultRegistryProblemEnv index ] } s.mVars_ResultRegistryProblemEnv }
                            , T.ReadMVar_ResultRegistryProblemEnv IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_ResultRegistryProblemEnv: invalid ref"
        )


takeMVar_ResultRegistryProblemEnv : T.MVar_ResultRegistryProblemEnv -> IO (Result T.BRE_RegistryProblem T.BDS_Env)
takeMVar_ResultRegistryProblemEnv (T.MVar_ResultRegistryProblemEnv ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_ResultRegistryProblemEnv of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_ResultRegistryProblemEnv putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_ResultRegistryProblemEnv = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_ResultRegistryProblemEnv }
                                    , T.TakeMVar_ResultRegistryProblemEnv IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_ResultRegistryProblemEnv = Array.set ref { mVar | value = Nothing } s.mVars_ResultRegistryProblemEnv }
                                    , T.TakeMVar_ResultRegistryProblemEnv IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_ResultRegistryProblemEnv = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_ResultRegistryProblemEnv index ] } s.mVars_ResultRegistryProblemEnv }
                            , T.TakeMVar_ResultRegistryProblemEnv IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_ResultRegistryProblemEnv: invalid ref"
        )


putMVar_ResultRegistryProblemEnv : T.MVar_ResultRegistryProblemEnv -> Result T.BRE_RegistryProblem T.BDS_Env -> IO ()
putMVar_ResultRegistryProblemEnv (T.MVar_ResultRegistryProblemEnv ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_ResultRegistryProblemEnv of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_ResultRegistryProblemEnv = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_ResultRegistryProblemEnv index value ] } s.mVars_ResultRegistryProblemEnv }
                            , T.PutMVar_ResultRegistryProblemEnv IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_ResultRegistryProblemEnv readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_ResultRegistryProblemEnv = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_ResultRegistryProblemEnv }
                            , T.PutMVar_ResultRegistryProblemEnv IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_ResultRegistryProblemEnv: invalid ref"
        )


newEmptyMVar_ResultRegistryProblemEnv : IO T.MVar_ResultRegistryProblemEnv
newEmptyMVar_ResultRegistryProblemEnv =
    IO
        (\_ s ->
            ( { s | mVars_ResultRegistryProblemEnv = Array.push { subscribers = [], value = Nothing } s.mVars_ResultRegistryProblemEnv }
            , T.NewEmptyMVar_ResultRegistryProblemEnv IO.pure (Array.length s.mVars_ResultRegistryProblemEnv)
            )
        )
        |> IO.fmap T.MVar_ResultRegistryProblemEnv



-- Control.Concurrent.MVar (T.CED_Dep)


newMVar_CED_Dep : T.CED_Dep -> IO T.MVar_CED_Dep
newMVar_CED_Dep value =
    newEmptyMVar_CED_Dep
        |> IO.bind
            (\mvar ->
                putMVar_CED_Dep mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_CED_Dep : T.MVar_CED_Dep -> IO T.CED_Dep
readMVar_CED_Dep (T.MVar_CED_Dep ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_CED_Dep of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_CED_Dep IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_CED_Dep = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_CED_Dep index ] } s.mVars_CED_Dep }
                            , T.ReadMVar_CED_Dep IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_CED_Dep: invalid ref"
        )


takeMVar_CED_Dep : T.MVar_CED_Dep -> IO T.CED_Dep
takeMVar_CED_Dep (T.MVar_CED_Dep ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_CED_Dep of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_CED_Dep putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_CED_Dep = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_CED_Dep }
                                    , T.TakeMVar_CED_Dep IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_CED_Dep = Array.set ref { mVar | value = Nothing } s.mVars_CED_Dep }
                                    , T.TakeMVar_CED_Dep IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_CED_Dep = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_CED_Dep index ] } s.mVars_CED_Dep }
                            , T.TakeMVar_CED_Dep IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_CED_Dep: invalid ref"
        )


putMVar_CED_Dep : T.MVar_CED_Dep -> T.CED_Dep -> IO ()
putMVar_CED_Dep (T.MVar_CED_Dep ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_CED_Dep of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_CED_Dep = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_CED_Dep index value ] } s.mVars_CED_Dep }
                            , T.PutMVar_CED_Dep IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_CED_Dep readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_CED_Dep = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_CED_Dep }
                            , T.PutMVar_CED_Dep IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_CED_Dep: invalid ref"
        )


newEmptyMVar_CED_Dep : IO T.MVar_CED_Dep
newEmptyMVar_CED_Dep =
    IO
        (\_ s ->
            ( { s | mVars_CED_Dep = Array.push { subscribers = [], value = Nothing } s.mVars_CED_Dep }
            , T.NewEmptyMVar_CED_Dep IO.pure (Array.length s.mVars_CED_Dep)
            )
        )
        |> IO.fmap T.MVar_CED_Dep



-- Control.Concurrent.MVar (Maybe T.CECTE_Types)


newMVar_Maybe_CECTE_Types : Maybe T.CECTE_Types -> IO T.MVar_Maybe_CECTE_Types
newMVar_Maybe_CECTE_Types value =
    newEmptyMVar_Maybe_CECTE_Types
        |> IO.bind
            (\mvar ->
                putMVar_Maybe_CECTE_Types mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_Maybe_CECTE_Types : T.MVar_Maybe_CECTE_Types -> IO (Maybe T.CECTE_Types)
readMVar_Maybe_CECTE_Types (T.MVar_Maybe_CECTE_Types ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_CECTE_Types of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Maybe_CECTE_Types IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Maybe_CECTE_Types = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Maybe_CECTE_Types index ] } s.mVars_Maybe_CECTE_Types }
                            , T.ReadMVar_Maybe_CECTE_Types IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_Maybe_CECTE_Types: invalid ref"
        )


takeMVar_Maybe_CECTE_Types : T.MVar_Maybe_CECTE_Types -> IO (Maybe T.CECTE_Types)
takeMVar_Maybe_CECTE_Types (T.MVar_Maybe_CECTE_Types ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_CECTE_Types of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_Maybe_CECTE_Types putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_Maybe_CECTE_Types = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_Maybe_CECTE_Types }
                                    , T.TakeMVar_Maybe_CECTE_Types IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_Maybe_CECTE_Types = Array.set ref { mVar | value = Nothing } s.mVars_Maybe_CECTE_Types }
                                    , T.TakeMVar_Maybe_CECTE_Types IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_Maybe_CECTE_Types = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_Maybe_CECTE_Types index ] } s.mVars_Maybe_CECTE_Types }
                            , T.TakeMVar_Maybe_CECTE_Types IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_Maybe_CECTE_Types: invalid ref"
        )


putMVar_Maybe_CECTE_Types : T.MVar_Maybe_CECTE_Types -> Maybe T.CECTE_Types -> IO ()
putMVar_Maybe_CECTE_Types (T.MVar_Maybe_CECTE_Types ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_CECTE_Types of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Maybe_CECTE_Types = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Maybe_CECTE_Types index value ] } s.mVars_Maybe_CECTE_Types }
                            , T.PutMVar_Maybe_CECTE_Types IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Maybe_CECTE_Types readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Maybe_CECTE_Types = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Maybe_CECTE_Types }
                            , T.PutMVar_Maybe_CECTE_Types IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_Maybe_CECTE_Types: invalid ref"
        )


newEmptyMVar_Maybe_CECTE_Types : IO T.MVar_Maybe_CECTE_Types
newEmptyMVar_Maybe_CECTE_Types =
    IO
        (\_ s ->
            ( { s | mVars_Maybe_CECTE_Types = Array.push { subscribers = [], value = Nothing } s.mVars_Maybe_CECTE_Types }
            , T.NewEmptyMVar_Maybe_CECTE_Types IO.pure (Array.length s.mVars_Maybe_CECTE_Types)
            )
        )
        |> IO.fmap T.MVar_Maybe_CECTE_Types



-- Control.Concurrent.MVar (Maybe T.BB_Dependencies)


newMVar_Maybe_BB_Dependencies : Maybe T.BB_Dependencies -> IO T.MVar_Maybe_BB_Dependencies
newMVar_Maybe_BB_Dependencies value =
    newEmptyMVar_Maybe_BB_Dependencies
        |> IO.bind
            (\mvar ->
                putMVar_Maybe_BB_Dependencies mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_Maybe_BB_Dependencies : T.MVar_Maybe_BB_Dependencies -> IO (Maybe T.BB_Dependencies)
readMVar_Maybe_BB_Dependencies (T.MVar_Maybe_BB_Dependencies ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_BB_Dependencies of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Maybe_BB_Dependencies IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Maybe_BB_Dependencies = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Maybe_BB_Dependencies index ] } s.mVars_Maybe_BB_Dependencies }
                            , T.ReadMVar_Maybe_BB_Dependencies IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_Maybe_BB_Dependencies: invalid ref"
        )


takeMVar_Maybe_BB_Dependencies : T.MVar_Maybe_BB_Dependencies -> IO (Maybe T.BB_Dependencies)
takeMVar_Maybe_BB_Dependencies (T.MVar_Maybe_BB_Dependencies ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_BB_Dependencies of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_Maybe_BB_Dependencies putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_Maybe_BB_Dependencies = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_Maybe_BB_Dependencies }
                                    , T.TakeMVar_Maybe_BB_Dependencies IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_Maybe_BB_Dependencies = Array.set ref { mVar | value = Nothing } s.mVars_Maybe_BB_Dependencies }
                                    , T.TakeMVar_Maybe_BB_Dependencies IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_Maybe_BB_Dependencies = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_Maybe_BB_Dependencies index ] } s.mVars_Maybe_BB_Dependencies }
                            , T.TakeMVar_Maybe_BB_Dependencies IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_Maybe_BB_Dependencies: invalid ref"
        )


putMVar_Maybe_BB_Dependencies : T.MVar_Maybe_BB_Dependencies -> Maybe T.BB_Dependencies -> IO ()
putMVar_Maybe_BB_Dependencies (T.MVar_Maybe_BB_Dependencies ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Maybe_BB_Dependencies of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Maybe_BB_Dependencies = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Maybe_BB_Dependencies index value ] } s.mVars_Maybe_BB_Dependencies }
                            , T.PutMVar_Maybe_BB_Dependencies IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Maybe_BB_Dependencies readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Maybe_BB_Dependencies = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Maybe_BB_Dependencies }
                            , T.PutMVar_Maybe_BB_Dependencies IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_Maybe_BB_Dependencies: invalid ref"
        )


newEmptyMVar_Maybe_BB_Dependencies : IO T.MVar_Maybe_BB_Dependencies
newEmptyMVar_Maybe_BB_Dependencies =
    IO
        (\_ s ->
            ( { s | mVars_Maybe_BB_Dependencies = Array.push { subscribers = [], value = Nothing } s.mVars_Maybe_BB_Dependencies }
            , T.NewEmptyMVar_Maybe_BB_Dependencies IO.pure (Array.length s.mVars_Maybe_BB_Dependencies)
            )
        )
        |> IO.fmap T.MVar_Maybe_BB_Dependencies



-- Control.Concurrent.MVar (Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep)


newMVar_DictNameMVarDep : Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep -> IO T.MVar_DictNameMVarDep
newMVar_DictNameMVarDep value =
    newEmptyMVar_DictNameMVarDep
        |> IO.bind
            (\mvar ->
                putMVar_DictNameMVarDep mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_DictNameMVarDep : T.MVar_DictNameMVarDep -> IO (Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep)
readMVar_DictNameMVarDep (T.MVar_DictNameMVarDep ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_DictNameMVarDep of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_DictNameMVarDep IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_DictNameMVarDep = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_DictNameMVarDep index ] } s.mVars_DictNameMVarDep }
                            , T.ReadMVar_DictNameMVarDep IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_DictNameMVarDep: invalid ref"
        )


takeMVar_DictNameMVarDep : T.MVar_DictNameMVarDep -> IO (Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep)
takeMVar_DictNameMVarDep (T.MVar_DictNameMVarDep ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_DictNameMVarDep of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_DictNameMVarDep putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_DictNameMVarDep = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_DictNameMVarDep }
                                    , T.TakeMVar_DictNameMVarDep IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_DictNameMVarDep = Array.set ref { mVar | value = Nothing } s.mVars_DictNameMVarDep }
                                    , T.TakeMVar_DictNameMVarDep IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_DictNameMVarDep = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_DictNameMVarDep index ] } s.mVars_DictNameMVarDep }
                            , T.TakeMVar_DictNameMVarDep IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_DictNameMVarDep: invalid ref"
        )


putMVar_DictNameMVarDep : T.MVar_DictNameMVarDep -> Dict ( String, String ) T.CEP_Name T.MVar_CED_Dep -> IO ()
putMVar_DictNameMVarDep (T.MVar_DictNameMVarDep ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_DictNameMVarDep of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_DictNameMVarDep = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_DictNameMVarDep index value ] } s.mVars_DictNameMVarDep }
                            , T.PutMVar_DictNameMVarDep IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_DictNameMVarDep readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_DictNameMVarDep = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_DictNameMVarDep }
                            , T.PutMVar_DictNameMVarDep IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_DictNameMVarDep: invalid ref"
        )


newEmptyMVar_DictNameMVarDep : IO T.MVar_DictNameMVarDep
newEmptyMVar_DictNameMVarDep =
    IO
        (\_ s ->
            ( { s | mVars_DictNameMVarDep = Array.push { subscribers = [], value = Nothing } s.mVars_DictNameMVarDep }
            , T.NewEmptyMVar_DictNameMVarDep IO.pure (Array.length s.mVars_DictNameMVarDep)
            )
        )
        |> IO.fmap T.MVar_DictNameMVarDep



-- Control.Concurrent.MVar (Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult)


newMVar_DictRawMVarMaybeDResult : Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult -> IO T.MVar_DictRawMVarMaybeDResult
newMVar_DictRawMVarMaybeDResult value =
    newEmptyMVar_DictRawMVarMaybeDResult
        |> IO.bind
            (\mvar ->
                putMVar_DictRawMVarMaybeDResult mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_DictRawMVarMaybeDResult : T.MVar_DictRawMVarMaybeDResult -> IO (Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult)
readMVar_DictRawMVarMaybeDResult (T.MVar_DictRawMVarMaybeDResult ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_DictRawMVarMaybeDResult of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_DictRawMVarMaybeDResult IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_DictRawMVarMaybeDResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_DictRawMVarMaybeDResult index ] } s.mVars_DictRawMVarMaybeDResult }
                            , T.ReadMVar_DictRawMVarMaybeDResult IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_DictRawMVarMaybeDResult: invalid ref"
        )


takeMVar_DictRawMVarMaybeDResult : T.MVar_DictRawMVarMaybeDResult -> IO (Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult)
takeMVar_DictRawMVarMaybeDResult (T.MVar_DictRawMVarMaybeDResult ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_DictRawMVarMaybeDResult of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_DictRawMVarMaybeDResult putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_DictRawMVarMaybeDResult = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_DictRawMVarMaybeDResult }
                                    , T.TakeMVar_DictRawMVarMaybeDResult IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_DictRawMVarMaybeDResult = Array.set ref { mVar | value = Nothing } s.mVars_DictRawMVarMaybeDResult }
                                    , T.TakeMVar_DictRawMVarMaybeDResult IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_DictRawMVarMaybeDResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_DictRawMVarMaybeDResult index ] } s.mVars_DictRawMVarMaybeDResult }
                            , T.TakeMVar_DictRawMVarMaybeDResult IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_DictRawMVarMaybeDResult: invalid ref"
        )


putMVar_DictRawMVarMaybeDResult : T.MVar_DictRawMVarMaybeDResult -> Dict String T.CEMN_Raw T.MVar_Maybe_BED_DResult -> IO ()
putMVar_DictRawMVarMaybeDResult (T.MVar_DictRawMVarMaybeDResult ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_DictRawMVarMaybeDResult of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_DictRawMVarMaybeDResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_DictRawMVarMaybeDResult index value ] } s.mVars_DictRawMVarMaybeDResult }
                            , T.PutMVar_DictRawMVarMaybeDResult IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_DictRawMVarMaybeDResult readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_DictRawMVarMaybeDResult = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_DictRawMVarMaybeDResult }
                            , T.PutMVar_DictRawMVarMaybeDResult IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_DictRawMVarMaybeDResult: invalid ref"
        )


newEmptyMVar_DictRawMVarMaybeDResult : IO T.MVar_DictRawMVarMaybeDResult
newEmptyMVar_DictRawMVarMaybeDResult =
    IO
        (\_ s ->
            ( { s | mVars_DictRawMVarMaybeDResult = Array.push { subscribers = [], value = Nothing } s.mVars_DictRawMVarMaybeDResult }
            , T.NewEmptyMVar_DictRawMVarMaybeDResult IO.pure (Array.length s.mVars_DictRawMVarMaybeDResult)
            )
        )
        |> IO.fmap T.MVar_DictRawMVarMaybeDResult



-- Control.Concurrent.MVar (List (T.MVar ()))


newMVar_ListMVar : List (T.MVar ()) -> IO T.MVar_ListMVar
newMVar_ListMVar value =
    newEmptyMVar_ListMVar
        |> IO.bind
            (\mvar ->
                putMVar_ListMVar mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_ListMVar : T.MVar_ListMVar -> IO (List (T.MVar ()))
readMVar_ListMVar (T.MVar_ListMVar ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_ListMVar of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_ListMVar IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_ListMVar = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_ListMVar index ] } s.mVars_ListMVar }
                            , T.ReadMVar_ListMVar IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_ListMVar: invalid ref"
        )


takeMVar_ListMVar : T.MVar_ListMVar -> IO (List (T.MVar ()))
takeMVar_ListMVar (T.MVar_ListMVar ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_ListMVar of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_ListMVar putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_ListMVar = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_ListMVar }
                                    , T.TakeMVar_ListMVar IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_ListMVar = Array.set ref { mVar | value = Nothing } s.mVars_ListMVar }
                                    , T.TakeMVar_ListMVar IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_ListMVar = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_ListMVar index ] } s.mVars_ListMVar }
                            , T.TakeMVar_ListMVar IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_ListMVar: invalid ref"
        )


putMVar_ListMVar : T.MVar_ListMVar -> List (T.MVar ()) -> IO ()
putMVar_ListMVar (T.MVar_ListMVar ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_ListMVar of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_ListMVar = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_ListMVar index value ] } s.mVars_ListMVar }
                            , T.PutMVar_ListMVar IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_ListMVar readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_ListMVar = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_ListMVar }
                            , T.PutMVar_ListMVar IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_ListMVar: invalid ref"
        )


newEmptyMVar_ListMVar : IO T.MVar_ListMVar
newEmptyMVar_ListMVar =
    IO
        (\_ s ->
            ( { s | mVars_ListMVar = Array.push { subscribers = [], value = Nothing } s.mVars_ListMVar }
            , T.NewEmptyMVar_ListMVar IO.pure (Array.length s.mVars_ListMVar)
            )
        )
        |> IO.fmap T.MVar_ListMVar



-- Control.Concurrent.MVar (T.BB_CachedInterface)


newMVar_BB_CachedInterface : T.BB_CachedInterface -> IO T.MVar_BB_CachedInterface
newMVar_BB_CachedInterface value =
    newEmptyMVar_BB_CachedInterface
        |> IO.bind
            (\mvar ->
                putMVar_BB_CachedInterface mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_BB_CachedInterface : T.MVar_BB_CachedInterface -> IO T.BB_CachedInterface
readMVar_BB_CachedInterface (T.MVar_BB_CachedInterface ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_CachedInterface of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_BB_CachedInterface IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_BB_CachedInterface = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_BB_CachedInterface index ] } s.mVars_BB_CachedInterface }
                            , T.ReadMVar_BB_CachedInterface IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_BB_CachedInterface: invalid ref"
        )


takeMVar_BB_CachedInterface : T.MVar_BB_CachedInterface -> IO T.BB_CachedInterface
takeMVar_BB_CachedInterface (T.MVar_BB_CachedInterface ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_CachedInterface of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_BB_CachedInterface putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_BB_CachedInterface = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_BB_CachedInterface }
                                    , T.TakeMVar_BB_CachedInterface IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_BB_CachedInterface = Array.set ref { mVar | value = Nothing } s.mVars_BB_CachedInterface }
                                    , T.TakeMVar_BB_CachedInterface IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_BB_CachedInterface = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_BB_CachedInterface index ] } s.mVars_BB_CachedInterface }
                            , T.TakeMVar_BB_CachedInterface IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_BB_CachedInterface: invalid ref"
        )


putMVar_BB_CachedInterface : T.MVar_BB_CachedInterface -> T.BB_CachedInterface -> IO ()
putMVar_BB_CachedInterface (T.MVar_BB_CachedInterface ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_CachedInterface of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_BB_CachedInterface = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_BB_CachedInterface index value ] } s.mVars_BB_CachedInterface }
                            , T.PutMVar_BB_CachedInterface IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_BB_CachedInterface readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_BB_CachedInterface = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_BB_CachedInterface }
                            , T.PutMVar_BB_CachedInterface IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_BB_CachedInterface: invalid ref"
        )


newEmptyMVar_BB_CachedInterface : IO T.MVar_BB_CachedInterface
newEmptyMVar_BB_CachedInterface =
    IO
        (\_ s ->
            ( { s | mVars_BB_CachedInterface = Array.push { subscribers = [], value = Nothing } s.mVars_BB_CachedInterface }
            , T.NewEmptyMVar_BB_CachedInterface IO.pure (Array.length s.mVars_BB_CachedInterface)
            )
        )
        |> IO.fmap T.MVar_BB_CachedInterface



-- Control.Concurrent.MVar (T.BED_StatusDict)


newMVar_BED_StatusDict : T.BED_StatusDict -> IO T.MVar_BED_StatusDict
newMVar_BED_StatusDict value =
    newEmptyMVar_BED_StatusDict
        |> IO.bind
            (\mvar ->
                putMVar_BED_StatusDict mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_BED_StatusDict : T.MVar_BED_StatusDict -> IO T.BED_StatusDict
readMVar_BED_StatusDict (T.MVar_BED_StatusDict ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BED_StatusDict of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_BED_StatusDict IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_BED_StatusDict = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_BED_StatusDict index ] } s.mVars_BED_StatusDict }
                            , T.ReadMVar_BED_StatusDict IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_BED_StatusDict: invalid ref"
        )


takeMVar_BED_StatusDict : T.MVar_BED_StatusDict -> IO T.BED_StatusDict
takeMVar_BED_StatusDict (T.MVar_BED_StatusDict ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BED_StatusDict of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_BED_StatusDict putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_BED_StatusDict = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_BED_StatusDict }
                                    , T.TakeMVar_BED_StatusDict IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_BED_StatusDict = Array.set ref { mVar | value = Nothing } s.mVars_BED_StatusDict }
                                    , T.TakeMVar_BED_StatusDict IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_BED_StatusDict = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_BED_StatusDict index ] } s.mVars_BED_StatusDict }
                            , T.TakeMVar_BED_StatusDict IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_BED_StatusDict: invalid ref"
        )


putMVar_BED_StatusDict : T.MVar_BED_StatusDict -> T.BED_StatusDict -> IO ()
putMVar_BED_StatusDict (T.MVar_BED_StatusDict ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_BED_StatusDict of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_BED_StatusDict = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_BED_StatusDict index value ] } s.mVars_BED_StatusDict }
                            , T.PutMVar_BED_StatusDict IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_BED_StatusDict readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_BED_StatusDict = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_BED_StatusDict }
                            , T.PutMVar_BED_StatusDict IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_BED_StatusDict: invalid ref"
        )


newEmptyMVar_BED_StatusDict : IO T.MVar_BED_StatusDict
newEmptyMVar_BED_StatusDict =
    IO
        (\_ s ->
            ( { s | mVars_BED_StatusDict = Array.push { subscribers = [], value = Nothing } s.mVars_BED_StatusDict }
            , T.NewEmptyMVar_BED_StatusDict IO.pure (Array.length s.mVars_BED_StatusDict)
            )
        )
        |> IO.fmap T.MVar_BED_StatusDict



-- Control.Concurrent.MVar (Unit)


newMVar_Unit : () -> IO T.MVar_Unit
newMVar_Unit value =
    newEmptyMVar_Unit
        |> IO.bind
            (\mvar ->
                putMVar_Unit mvar value
                    |> IO.fmap (\_ -> mvar)
            )


readMVar_Unit : T.MVar_Unit -> IO ()
readMVar_Unit (T.MVar_Unit ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Unit of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Unit IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Unit = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Unit index ] } s.mVars_Unit }
                            , T.ReadMVar_Unit IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_Unit: invalid ref"
        )


takeMVar_Unit : T.MVar_Unit -> IO ()
takeMVar_Unit (T.MVar_Unit ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Unit of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_Unit putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_Unit = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_Unit }
                                    , T.TakeMVar_Unit IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_Unit = Array.set ref { mVar | value = Nothing } s.mVars_Unit }
                                    , T.TakeMVar_Unit IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_Unit = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_Unit index ] } s.mVars_Unit }
                            , T.TakeMVar_Unit IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_Unit: invalid ref"
        )


putMVar_Unit : T.MVar_Unit -> () -> IO ()
putMVar_Unit (T.MVar_Unit ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Unit of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Unit = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Unit index value ] } s.mVars_Unit }
                            , T.PutMVar_Unit IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Unit readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Unit = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Unit }
                            , T.PutMVar_Unit IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_Unit: invalid ref"
        )


newEmptyMVar_Unit : IO T.MVar_Unit
newEmptyMVar_Unit =
    IO
        (\_ s ->
            ( { s | mVars_Unit = Array.push { subscribers = [], value = Nothing } s.mVars_Unit }
            , T.NewEmptyMVar_Unit IO.pure (Array.length s.mVars_Unit)
            )
        )
        |> IO.fmap T.MVar_Unit



-- Control.Concurrent.Chan


type Chan a
    = Chan (T.MVar (T.Stream a)) (T.MVar (T.Stream a))


type Chan_ResultBMsgBResultUnit
    = Chan_ResultBMsgBResultUnit T.MVar_StreamResultBMsgBResultUnit T.MVar_StreamResultBMsgBResultUnit


type Chan_ResultBMsgBResultArtifacts
    = Chan_ResultBMsgBResultArtifacts T.MVar_StreamResultBMsgBResultArtifacts T.MVar_StreamResultBMsgBResultArtifacts


newChan : (T.Stream a -> Encode.Value) -> IO (Chan a)
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


newChan_ResultBMsgBResultUnit : IO Chan_ResultBMsgBResultUnit
newChan_ResultBMsgBResultUnit =
    newEmptyMVar_ChItemResultBMsgBResultUnit
        |> IO.bind
            (\hole ->
                newMVar_StreamResultBMsgBResultUnit hole
                    |> IO.bind
                        (\readVar ->
                            newMVar_StreamResultBMsgBResultUnit hole
                                |> IO.fmap
                                    (\writeVar ->
                                        Chan_ResultBMsgBResultUnit readVar writeVar
                                    )
                        )
            )


newChan_ResultBMsgBResultArtifacts : IO Chan_ResultBMsgBResultArtifacts
newChan_ResultBMsgBResultArtifacts =
    newEmptyMVar_ChItemResultBMsgBResultArtifacts
        |> IO.bind
            (\hole ->
                newMVar_StreamResultBMsgBResultArtifacts hole
                    |> IO.bind
                        (\readVar ->
                            newMVar_StreamResultBMsgBResultArtifacts hole
                                |> IO.fmap
                                    (\writeVar ->
                                        Chan_ResultBMsgBResultArtifacts readVar writeVar
                                    )
                        )
            )


readChan : Decode.Decoder a -> Chan a -> IO a
readChan decoder (Chan readVar _) =
    modifyMVar mVarDecoder mVarEncoder readVar <|
        \read_end ->
            readMVar (chItemDecoder decoder) read_end
                |> IO.fmap
                    (\(T.ChItem val new_read_end) ->
                        -- Use readMVar here, not takeMVar,
                        -- else dupChan doesn't work
                        ( new_read_end, val )
                    )


readChan_ResultBMsgBResultUnit : Chan_ResultBMsgBResultUnit -> IO (Result T.BR_BMsg (T.BR_BResult ()))
readChan_ResultBMsgBResultUnit (Chan_ResultBMsgBResultUnit readVar _) =
    modifyMVar_StreamResultBMsgBResultUnit_ResultBMsgBResultUnit readVar <|
        \read_end ->
            readMVar_ChItemResultBMsgBResultUnit read_end
                |> IO.fmap
                    (\(T.ChItem_ResultBMsgBResultUnit val new_read_end) ->
                        -- Use readMVar here, not takeMVar,
                        -- else dupChan doesn't work
                        ( new_read_end, val )
                    )


readChan_ResultBMsgBResultArtifacts : Chan_ResultBMsgBResultArtifacts -> IO (Result T.BR_BMsg (T.BR_BResult T.BB_Artifacts))
readChan_ResultBMsgBResultArtifacts (Chan_ResultBMsgBResultArtifacts readVar _) =
    modifyMVar_StreamResultBMsgBResultArtifacts_ResultBMsgBResultArtifacts readVar <|
        \read_end ->
            readMVar_ChItemResultBMsgBResultArtifacts read_end
                |> IO.fmap
                    (\(T.ChItem_ResultBMsgBResultArtifacts val new_read_end) ->
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
                            putMVar (chItemEncoder encoder) old_hole (T.ChItem val new_hole)
                                |> IO.bind (\_ -> putMVar mVarEncoder writeVar new_hole)
                        )
            )


writeChan_ResultBMsgBResultUnit : Chan_ResultBMsgBResultUnit -> Result T.BR_BMsg (T.BR_BResult ()) -> IO ()
writeChan_ResultBMsgBResultUnit (Chan_ResultBMsgBResultUnit _ writeVar) val =
    newEmptyMVar_ChItemResultBMsgBResultUnit
        |> IO.bind
            (\new_hole ->
                takeMVar_StreamResultBMsgBResultUnit writeVar
                    |> IO.bind
                        (\old_hole ->
                            putMVar_ChItemResultBMsgBResultUnit old_hole (T.ChItem_ResultBMsgBResultUnit val new_hole)
                                |> IO.bind (\_ -> putMVar_StreamResultBMsgBResultUnit writeVar new_hole)
                        )
            )


writeChan_ResultBMsgBResultArtifacts : Chan_ResultBMsgBResultArtifacts -> Result T.BR_BMsg (T.BR_BResult T.BB_Artifacts) -> IO ()
writeChan_ResultBMsgBResultArtifacts (Chan_ResultBMsgBResultArtifacts _ writeVar) val =
    newEmptyMVar_ChItemResultBMsgBResultArtifacts
        |> IO.bind
            (\new_hole ->
                takeMVar_StreamResultBMsgBResultArtifacts writeVar
                    |> IO.bind
                        (\old_hole ->
                            putMVar_ChItemResultBMsgBResultArtifacts old_hole (T.ChItem_ResultBMsgBResultArtifacts val new_hole)
                                |> IO.bind (\_ -> putMVar_StreamResultBMsgBResultArtifacts writeVar new_hole)
                        )
            )



-- Data.ByteString.Builder


builderHPutBuilder : T.Handle -> String -> IO ()
builderHPutBuilder handle str =
    IO (\_ s -> ( s, T.HPutStr IO.pure handle str ))



-- Data.Binary


binaryDecodeFileOrFail : Decode.Decoder a -> T.FilePath -> IO (Result ( Int, String ) a)
binaryDecodeFileOrFail decoder filename =
    IO (\_ s -> ( s, T.BinaryDecodeFileOrFail IO.pure filename ))
        |> IO.fmap
            (Decode.decodeValue decoder
                >> Result.mapError (\_ -> ( 0, "Could not find file " ++ filename ))
            )


binaryEncodeFile : (a -> Encode.Value) -> T.FilePath -> a -> IO ()
binaryEncodeFile encoder path value =
    IO (\_ s -> ( s, T.Write IO.pure path (encoder value) ))



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
    IO (\_ s -> ( s, T.ReplGetInputLine IO.pure prompt ))


replGetInputLineWithInitial : String -> ( String, String ) -> ReplInputT (Maybe String)
replGetInputLineWithInitial prompt ( left, right ) =
    IO (\_ s -> ( s, T.ReplGetInputLineWithInitial IO.pure prompt left right ))



-- ENCODERS and DECODERS


mVarDecoder : Decode.Decoder (T.MVar a)
mVarDecoder =
    Decode.map T.MVar Decode.int


mVarDecoder_Maybe_BED_Status : Decode.Decoder T.MVar_Maybe_BED_Status
mVarDecoder_Maybe_BED_Status =
    Decode.map T.MVar_Maybe_BED_Status Decode.int


mVarDecoder_Maybe_BED_DResult : Decode.Decoder T.MVar_Maybe_BED_DResult
mVarDecoder_Maybe_BED_DResult =
    Decode.map T.MVar_Maybe_BED_DResult Decode.int


mVarDecoder_BB_BResult : Decode.Decoder T.MVar_BB_BResult
mVarDecoder_BB_BResult =
    Decode.map T.MVar_BB_BResult Decode.int


mVarDecoder_CED_Dep : Decode.Decoder T.MVar_CED_Dep
mVarDecoder_CED_Dep =
    Decode.map T.MVar_CED_Dep Decode.int


mVarDecoder_Maybe_CECTE_Types : Decode.Decoder T.MVar_Maybe_CECTE_Types
mVarDecoder_Maybe_CECTE_Types =
    Decode.map T.MVar_Maybe_CECTE_Types Decode.int


mVarDecoder_BB_CachedInterface : Decode.Decoder T.MVar_BB_CachedInterface
mVarDecoder_BB_CachedInterface =
    Decode.map T.MVar_BB_CachedInterface Decode.int


mVarEncoder : T.MVar a -> Encode.Value
mVarEncoder (T.MVar ref) =
    Encode.int ref


mVarEncoder_Maybe_BED_Status : T.MVar_Maybe_BED_Status -> Encode.Value
mVarEncoder_Maybe_BED_Status (T.MVar_Maybe_BED_Status ref) =
    Encode.int ref


mVarEncoder_Maybe_BED_DResult : T.MVar_Maybe_BED_DResult -> Encode.Value
mVarEncoder_Maybe_BED_DResult (T.MVar_Maybe_BED_DResult ref) =
    Encode.int ref


mVarEncoder_BB_BResult : T.MVar_BB_BResult -> Encode.Value
mVarEncoder_BB_BResult (T.MVar_BB_BResult ref) =
    Encode.int ref


mVarEncoder_CED_Dep : T.MVar_CED_Dep -> Encode.Value
mVarEncoder_CED_Dep (T.MVar_CED_Dep ref) =
    Encode.int ref


mVarEncoder_Maybe_CECTE_Types : T.MVar_Maybe_CECTE_Types -> Encode.Value
mVarEncoder_Maybe_CECTE_Types (T.MVar_Maybe_CECTE_Types ref) =
    Encode.int ref


mVarEncoder_BB_CachedInterface : T.MVar_BB_CachedInterface -> Encode.Value
mVarEncoder_BB_CachedInterface (T.MVar_BB_CachedInterface ref) =
    Encode.int ref


chItemEncoder : (a -> Encode.Value) -> T.ChItem a -> Encode.Value
chItemEncoder valueEncoder (T.ChItem value hole) =
    Encode.object
        [ ( "type", Encode.string "ChItem" )
        , ( "value", valueEncoder value )
        , ( "hole", mVarEncoder hole )
        ]


chItemDecoder : Decode.Decoder a -> Decode.Decoder (T.ChItem a)
chItemDecoder decoder =
    Decode.map2 T.ChItem (Decode.field "value" decoder) (Decode.field "hole" mVarDecoder)


someExceptionEncoder : T.UM_SomeException -> Encode.Value
someExceptionEncoder _ =
    Encode.object [ ( "type", Encode.string "SomeException" ) ]


someExceptionDecoder : Decode.Decoder T.UM_SomeException
someExceptionDecoder =
    Decode.succeed T.UM_SomeException


httpResponseEncoder : T.UM_HttpResponse body -> Encode.Value
httpResponseEncoder (T.UM_HttpResponse httpResponse) =
    Encode.object
        [ ( "type", Encode.string "HttpResponse" )
        , ( "responseStatus", httpStatusEncoder httpResponse.responseStatus )
        , ( "responseHeaders", httpResponseHeadersEncoder httpResponse.responseHeaders )
        ]


httpResponseDecoder : Decode.Decoder (T.UM_HttpResponse body)
httpResponseDecoder =
    Decode.map2
        (\responseStatus responseHeaders ->
            T.UM_HttpResponse
                { responseStatus = responseStatus
                , responseHeaders = responseHeaders
                }
        )
        (Decode.field "responseStatus" httpStatusDecoder)
        (Decode.field "responseHeaders" httpResponseHeadersDecoder)


httpStatusEncoder : T.UM_HttpStatus -> Encode.Value
httpStatusEncoder (T.UM_HttpStatus statusCode statusMessage) =
    Encode.object
        [ ( "type", Encode.string "HttpStatus" )
        , ( "statusCode", Encode.int statusCode )
        , ( "statusMessage", Encode.string statusMessage )
        ]


httpStatusDecoder : Decode.Decoder T.UM_HttpStatus
httpStatusDecoder =
    Decode.map2 T.UM_HttpStatus
        (Decode.field "statusCode" Decode.int)
        (Decode.field "statusMessage" Decode.string)


httpResponseHeadersEncoder : T.UM_HttpResponseHeaders -> Encode.Value
httpResponseHeadersEncoder =
    Encode.list (E.jsonPair Encode.string Encode.string)


httpResponseHeadersDecoder : Decode.Decoder T.UM_HttpResponseHeaders
httpResponseHeadersDecoder =
    Decode.list (D.jsonPair Decode.string Decode.string)


httpExceptionContentEncoder : T.UM_HttpExceptionContent -> Encode.Value
httpExceptionContentEncoder httpExceptionContent =
    case httpExceptionContent of
        T.UM_StatusCodeException response body ->
            Encode.object
                [ ( "type", Encode.string "StatusCodeException" )
                , ( "response", httpResponseEncoder response )
                , ( "body", Encode.string body )
                ]

        T.UM_TooManyRedirects responses ->
            Encode.object
                [ ( "type", Encode.string "TooManyRedirects" )
                , ( "responses", Encode.list httpResponseEncoder responses )
                ]

        T.UM_ConnectionFailure someException ->
            Encode.object
                [ ( "type", Encode.string "ConnectionFailure" )
                , ( "someException", someExceptionEncoder someException )
                ]


httpExceptionContentDecoder : Decode.Decoder T.UM_HttpExceptionContent
httpExceptionContentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "StatusCodeException" ->
                        Decode.map2 T.UM_StatusCodeException
                            (Decode.field "response" httpResponseDecoder)
                            (Decode.field "body" Decode.string)

                    "TooManyRedirects" ->
                        Decode.map T.UM_TooManyRedirects (Decode.field "responses" (Decode.list httpResponseDecoder))

                    "ConnectionFailure" ->
                        Decode.map T.UM_ConnectionFailure (Decode.field "someException" someExceptionDecoder)

                    _ ->
                        Decode.fail ("Failed to decode HttpExceptionContent's type: " ++ type_)
            )
