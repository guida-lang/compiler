module Utils.Main exposing
    ( Chan_Maybe_DMsg
    , Chan_ResultBMsgBResultArtifacts
    , Chan_ResultBMsgBResultDocumentation
    , Chan_ResultBMsgBResultUnit
    , LockSharedExclusive(..)
    , ReplCompletion(..)
    , ReplCompletionFunc
    , ReplInputT
    , ReplSettings(..)
    , ThreadId
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
    , maybeMapM
    , maybeTraverseTask
    , newChan_Maybe_DMsg
    , newChan_ResultBMsgBResultArtifacts
    , newChan_ResultBMsgBResultDocumentation
    , newChan_ResultBMsgBResultUnit
    , newEmptyMVar_BB_BResult
    , newEmptyMVar_BB_ResultDict
    , newEmptyMVar_BB_RootResult
    , newEmptyMVar_BB_RootStatus
    , newEmptyMVar_BB_Status
    , newEmptyMVar_BB_StatusDict
    , newEmptyMVar_BED_StatusDict
    , newEmptyMVar_CED_Dep
    , newEmptyMVar_DictNameMVarDep
    , newEmptyMVar_DictRawMVarMaybeDResult
    , newEmptyMVar_Manager
    , newEmptyMVar_MaybeDep
    , newEmptyMVar_Maybe_BB_Dependencies
    , newEmptyMVar_Maybe_BED_DResult
    , newEmptyMVar_Maybe_BED_Status
    , newEmptyMVar_Maybe_CASTO_GlobalGraph
    , newEmptyMVar_Maybe_CASTO_LocalGraph
    , newEmptyMVar_Maybe_CECTE_Types
    , newEmptyMVar_ResultRegistryProblemEnv
    , newEmptyMVar_Result_BuildProjectProblem_RootInfo
    , newEmptyMVar_Unit
    , newMVar_BB_CachedInterface
    , newMVar_BB_StatusDict
    , newMVar_ListMVar
    , newMVar_Maybe_BB_Dependencies
    , newMVar_Maybe_CASTO_GlobalGraph
    , newMVar_Maybe_CASTO_LocalGraph
    , newMVar_Maybe_CECTE_Types
    , newMVar_Unit
    , nonEmptyListTraverse
    , putMVar_BB_BResult
    , putMVar_BB_CachedInterface
    , putMVar_BB_ResultDict
    , putMVar_BB_RootResult
    , putMVar_BB_RootStatus
    , putMVar_BB_Status
    , putMVar_BB_StatusDict
    , putMVar_BED_StatusDict
    , putMVar_CED_Dep
    , putMVar_DictNameMVarDep
    , putMVar_DictRawMVarMaybeDResult
    , putMVar_ListMVar
    , putMVar_Manager
    , putMVar_MaybeDep
    , putMVar_Maybe_BB_Dependencies
    , putMVar_Maybe_BED_DResult
    , putMVar_Maybe_BED_Status
    , putMVar_Maybe_CASTO_GlobalGraph
    , putMVar_Maybe_CASTO_LocalGraph
    , putMVar_Maybe_CECTE_Types
    , putMVar_ResultRegistryProblemEnv
    , putMVar_Result_BuildProjectProblem_RootInfo
    , putMVar_Unit
    , readChan_Maybe_DMsg
    , readChan_ResultBMsgBResultArtifacts
    , readChan_ResultBMsgBResultDocumentation
    , readChan_ResultBMsgBResultUnit
    , readMVar_BB_BResult
    , readMVar_BB_CachedInterface
    , readMVar_BB_ResultDict
    , readMVar_BB_RootResult
    , readMVar_BB_RootStatus
    , readMVar_BB_Status
    , readMVar_BB_StatusDict
    , readMVar_BED_StatusDict
    , readMVar_CED_Dep
    , readMVar_DictNameMVarDep
    , readMVar_DictRawMVarMaybeDResult
    , readMVar_Manager
    , readMVar_MaybeDep
    , readMVar_Maybe_BB_Dependencies
    , readMVar_Maybe_BED_DResult
    , readMVar_Maybe_BED_Status
    , readMVar_Maybe_CASTO_GlobalGraph
    , readMVar_Maybe_CASTO_LocalGraph
    , readMVar_Maybe_CECTE_Types
    , readMVar_ResultRegistryProblemEnv
    , readMVar_Result_BuildProjectProblem_RootInfo
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
    , takeMVar_BB_CachedInterface
    , takeMVar_BB_StatusDict
    , takeMVar_BED_StatusDict
    , takeMVar_ListMVar
    , takeMVar_Unit
    , unlines
    , unzip3
    , writeChan_Maybe_DMsg
    , writeChan_ResultBMsgBResultArtifacts
    , writeChan_ResultBMsgBResultDocumentation
    , writeChan_ResultBMsgBResultUnit
    , zipWithM
    )

import Array
import Basics.Extra exposing (flip)
import Builder.Reporting.Task as Task exposing (Task)
import Compiler.Data.Index as Index
import Compiler.Data.NonEmptyList as NE
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


newMVar_Stream_Maybe_DMsg : T.MVar_ChItem_Maybe_DMsg -> IO T.MVar_Stream_Maybe_DMsg
newMVar_Stream_Maybe_DMsg value =
    newEmptyMVar_Stream_Maybe_DMsg
        |> IO.bind
            (\mvar ->
                putMVar_Stream_Maybe_DMsg mvar value
                    |> IO.fmap (\_ -> mvar)
            )


newMVar_StreamResultBMsgBResultDocumentation : T.MVar_ChItemResultBMsgBResultDocumentation -> IO T.MVar_StreamResultBMsgBResultDocumentation
newMVar_StreamResultBMsgBResultDocumentation value =
    newEmptyMVar_StreamResultBMsgBResultDocumentation
        |> IO.bind
            (\mvar ->
                putMVar_StreamResultBMsgBResultDocumentation mvar value
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


readMVar_Result_BuildProjectProblem_RootInfo : T.MVar_Result_BuildProjectProblem_RootInfo -> IO (Result T.BRE_BuildProjectProblem T.BB_RootInfo)
readMVar_Result_BuildProjectProblem_RootInfo (T.MVar_Result_BuildProjectProblem_RootInfo ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Result_BuildProjectProblem_RootInfo of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Result_BuildProjectProblem_RootInfo IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Result_BuildProjectProblem_RootInfo = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Result_BuildProjectProblem_RootInfo index ] } s.mVars_Result_BuildProjectProblem_RootInfo }
                            , T.ReadMVar_Result_BuildProjectProblem_RootInfo IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_Result_BuildProjectProblem_RootInfo: invalid ref"
        )


readMVar_Manager : T.MVar_Manager -> IO T.BH_Manager
readMVar_Manager (T.MVar_Manager ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Manager of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_Manager IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_Manager = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_Manager index ] } s.mVars_Manager }
                            , T.ReadMVar_Manager IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_Manager: invalid ref"
        )


readMVar_BB_ResultDict : T.MVar_BB_ResultDict -> IO T.BB_ResultDict
readMVar_BB_ResultDict (T.MVar_BB_ResultDict ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_ResultDict of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_BB_ResultDict IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_BB_ResultDict = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_BB_ResultDict index ] } s.mVars_BB_ResultDict }
                            , T.ReadMVar_BB_ResultDict IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_BB_ResultDict: invalid ref"
        )


readMVar_ChItem_Maybe_DMsg : T.MVar_ChItem_Maybe_DMsg -> IO T.ChItem_Maybe_DMsg
readMVar_ChItem_Maybe_DMsg (T.MVar_ChItem_Maybe_DMsg ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_ChItem_Maybe_DMsg of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_ChItem_Maybe_DMsg IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_ChItem_Maybe_DMsg = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_ChItem_Maybe_DMsg index ] } s.mVars_ChItem_Maybe_DMsg }
                            , T.ReadMVar_ChItem_Maybe_DMsg IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_ChItem_Maybe_DMsg: invalid ref"
        )


readMVar_ChItemResultBMsgBResultDocumentation : T.MVar_ChItemResultBMsgBResultDocumentation -> IO T.ChItem_ResultBMsgBResultDocumentation
readMVar_ChItemResultBMsgBResultDocumentation (T.MVar_ChItemResultBMsgBResultDocumentation ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_ChItemResultBMsgBResultDocumentation of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_ChItemResultBMsgBResultDocumentation IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_ChItemResultBMsgBResultDocumentation = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_ChItemResultBMsgBResultDocumentation index ] } s.mVars_ChItemResultBMsgBResultDocumentation }
                            , T.ReadMVar_ChItemResultBMsgBResultDocumentation IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_ChItemResultBMsgBResultDocumentation: invalid ref"
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


modifyMVar_Stream_Maybe_DMsg_Maybe_DMsg :
    T.MVar_Stream_Maybe_DMsg
    -> (T.MVar_ChItem_Maybe_DMsg -> IO ( T.MVar_ChItem_Maybe_DMsg, Maybe T.BR_DMsg ))
    -> IO (Maybe T.BR_DMsg)
modifyMVar_Stream_Maybe_DMsg_Maybe_DMsg m io =
    takeMVar_Stream_Maybe_DMsg m
        |> IO.bind io
        |> IO.bind
            (\( a, b ) ->
                putMVar_Stream_Maybe_DMsg m a
                    |> IO.fmap (\_ -> b)
            )


modifyMVar_StreamResultBMsgBResultDocumentation_ResultBMsgBResultDocumentation :
    T.MVar_StreamResultBMsgBResultDocumentation
    -> (T.MVar_ChItemResultBMsgBResultDocumentation -> IO ( T.MVar_ChItemResultBMsgBResultDocumentation, Result T.BR_BMsg (T.BR_BResult T.CED_Documentation) ))
    -> IO (Result T.BR_BMsg (T.BR_BResult T.CED_Documentation))
modifyMVar_StreamResultBMsgBResultDocumentation_ResultBMsgBResultDocumentation m io =
    takeMVar_StreamResultBMsgBResultDocumentation m
        |> IO.bind io
        |> IO.bind
            (\( a, b ) ->
                putMVar_StreamResultBMsgBResultDocumentation m a
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


takeMVar_Stream_Maybe_DMsg : T.MVar_Stream_Maybe_DMsg -> IO T.MVar_ChItem_Maybe_DMsg
takeMVar_Stream_Maybe_DMsg (T.MVar_Stream_Maybe_DMsg ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_Stream_Maybe_DMsg of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_Stream_Maybe_DMsg putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_Stream_Maybe_DMsg = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_Stream_Maybe_DMsg }
                                    , T.TakeMVar_Stream_Maybe_DMsg IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_Stream_Maybe_DMsg = Array.set ref { mVar | value = Nothing } s.mVars_Stream_Maybe_DMsg }
                                    , T.TakeMVar_Stream_Maybe_DMsg IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_Stream_Maybe_DMsg = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_Stream_Maybe_DMsg index ] } s.mVars_Stream_Maybe_DMsg }
                            , T.TakeMVar_Stream_Maybe_DMsg IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_Stream_Maybe_DMsg: invalid ref"
        )


takeMVar_StreamResultBMsgBResultDocumentation : T.MVar_StreamResultBMsgBResultDocumentation -> IO T.MVar_ChItemResultBMsgBResultDocumentation
takeMVar_StreamResultBMsgBResultDocumentation (T.MVar_StreamResultBMsgBResultDocumentation ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_StreamResultBMsgBResultDocumentation of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            case mVar.subscribers of
                                (T.PutMVarSubscriber_StreamResultBMsgBResultDocumentation putIndex putValue) :: restSubscribers ->
                                    ( { s | mVars_StreamResultBMsgBResultDocumentation = Array.set ref { mVar | subscribers = restSubscribers, value = Just putValue } s.mVars_StreamResultBMsgBResultDocumentation }
                                    , T.TakeMVar_StreamResultBMsgBResultDocumentation IO.pure (Just value) (Just putIndex)
                                    )

                                _ ->
                                    ( { s | mVars_StreamResultBMsgBResultDocumentation = Array.set ref { mVar | value = Nothing } s.mVars_StreamResultBMsgBResultDocumentation }
                                    , T.TakeMVar_StreamResultBMsgBResultDocumentation IO.pure (Just value) Nothing
                                    )

                        Nothing ->
                            ( { s | mVars_StreamResultBMsgBResultDocumentation = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.TakeMVarSubscriber_StreamResultBMsgBResultDocumentation index ] } s.mVars_StreamResultBMsgBResultDocumentation }
                            , T.TakeMVar_StreamResultBMsgBResultDocumentation IO.pure Nothing Nothing
                            )

                Nothing ->
                    crash "Utils.Main.takeMVar_StreamResultBMsgBResultDocumentation: invalid ref"
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


putMVar_Manager : T.MVar_Manager -> T.BH_Manager -> IO ()
putMVar_Manager (T.MVar_Manager ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Manager of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Manager = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Manager index value ] } s.mVars_Manager }
                            , T.PutMVar_Manager IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Manager readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Manager = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Manager }
                            , T.PutMVar_Manager IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_Manager: invalid ref"
        )


putMVar_BB_ResultDict : T.MVar_BB_ResultDict -> T.BB_ResultDict -> IO ()
putMVar_BB_ResultDict (T.MVar_BB_ResultDict ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_ResultDict of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_BB_ResultDict = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_BB_ResultDict index value ] } s.mVars_BB_ResultDict }
                            , T.PutMVar_BB_ResultDict IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_BB_ResultDict readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_BB_ResultDict = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_BB_ResultDict }
                            , T.PutMVar_BB_ResultDict IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_BB_ResultDict: invalid ref"
        )


putMVar_Stream_Maybe_DMsg : T.MVar_Stream_Maybe_DMsg -> T.MVar_ChItem_Maybe_DMsg -> IO ()
putMVar_Stream_Maybe_DMsg (T.MVar_Stream_Maybe_DMsg ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Stream_Maybe_DMsg of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Stream_Maybe_DMsg = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Stream_Maybe_DMsg index value ] } s.mVars_Stream_Maybe_DMsg }
                            , T.PutMVar_Stream_Maybe_DMsg IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Stream_Maybe_DMsg = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Stream_Maybe_DMsg }
                            , T.PutMVar_Stream_Maybe_DMsg IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_Stream_Maybe_DMsg: invalid ref"
        )


putMVar_ChItem_Maybe_DMsg : T.MVar_ChItem_Maybe_DMsg -> T.ChItem_Maybe_DMsg -> IO ()
putMVar_ChItem_Maybe_DMsg (T.MVar_ChItem_Maybe_DMsg ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_ChItem_Maybe_DMsg of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_ChItem_Maybe_DMsg = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_ChItem_Maybe_DMsg index value ] } s.mVars_ChItem_Maybe_DMsg }
                            , T.PutMVar_ChItem_Maybe_DMsg IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_ChItem_Maybe_DMsg readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_ChItem_Maybe_DMsg = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_ChItem_Maybe_DMsg }
                            , T.PutMVar_ChItem_Maybe_DMsg IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_ChItem_Maybe_DMsg: invalid ref"
        )


putMVar_StreamResultBMsgBResultDocumentation : T.MVar_StreamResultBMsgBResultDocumentation -> T.MVar_ChItemResultBMsgBResultDocumentation -> IO ()
putMVar_StreamResultBMsgBResultDocumentation (T.MVar_StreamResultBMsgBResultDocumentation ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_StreamResultBMsgBResultDocumentation of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_StreamResultBMsgBResultDocumentation = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_StreamResultBMsgBResultDocumentation index value ] } s.mVars_StreamResultBMsgBResultDocumentation }
                            , T.PutMVar_StreamResultBMsgBResultDocumentation IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_StreamResultBMsgBResultDocumentation = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_StreamResultBMsgBResultDocumentation }
                            , T.PutMVar_StreamResultBMsgBResultDocumentation IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_StreamResultBMsgBResultDocumentation: invalid ref"
        )


putMVar_ChItemResultBMsgBResultDocumentation : T.MVar_ChItemResultBMsgBResultDocumentation -> T.ChItem_ResultBMsgBResultDocumentation -> IO ()
putMVar_ChItemResultBMsgBResultDocumentation (T.MVar_ChItemResultBMsgBResultDocumentation ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_ChItemResultBMsgBResultDocumentation of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_ChItemResultBMsgBResultDocumentation = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_ChItemResultBMsgBResultDocumentation index value ] } s.mVars_ChItemResultBMsgBResultDocumentation }
                            , T.PutMVar_ChItemResultBMsgBResultDocumentation IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_ChItemResultBMsgBResultDocumentation readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_ChItemResultBMsgBResultDocumentation = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_ChItemResultBMsgBResultDocumentation }
                            , T.PutMVar_ChItemResultBMsgBResultDocumentation IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_ChItemResultBMsgBResultDocumentation: invalid ref"
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


newEmptyMVar_Manager : IO T.MVar_Manager
newEmptyMVar_Manager =
    IO
        (\_ s ->
            ( { s | mVars_Manager = Array.push { subscribers = [], value = Nothing } s.mVars_Manager }
            , T.NewEmptyMVar_Manager IO.pure (Array.length s.mVars_Manager)
            )
        )
        |> IO.fmap T.MVar_Manager


newEmptyMVar_BB_ResultDict : IO T.MVar_BB_ResultDict
newEmptyMVar_BB_ResultDict =
    IO
        (\_ s ->
            ( { s | mVars_BB_ResultDict = Array.push { subscribers = [], value = Nothing } s.mVars_BB_ResultDict }
            , T.NewEmptyMVar_BB_ResultDict IO.pure (Array.length s.mVars_BB_ResultDict)
            )
        )
        |> IO.fmap T.MVar_BB_ResultDict


newEmptyMVar_Stream_Maybe_DMsg : IO T.MVar_Stream_Maybe_DMsg
newEmptyMVar_Stream_Maybe_DMsg =
    IO
        (\_ s ->
            ( { s | mVars_Stream_Maybe_DMsg = Array.push { subscribers = [], value = Nothing } s.mVars_Stream_Maybe_DMsg }
            , T.NewEmptyMVar_Stream_Maybe_DMsg IO.pure (Array.length s.mVars_Stream_Maybe_DMsg)
            )
        )
        |> IO.fmap T.MVar_Stream_Maybe_DMsg


newEmptyMVar_ChItem_Maybe_DMsg : IO T.MVar_ChItem_Maybe_DMsg
newEmptyMVar_ChItem_Maybe_DMsg =
    IO
        (\_ s ->
            ( { s | mVars_ChItem_Maybe_DMsg = Array.push { subscribers = [], value = Nothing } s.mVars_ChItem_Maybe_DMsg }
            , T.NewEmptyMVar_ChItem_Maybe_DMsg IO.pure (Array.length s.mVars_ChItem_Maybe_DMsg)
            )
        )
        |> IO.fmap T.MVar_ChItem_Maybe_DMsg


newEmptyMVar_StreamResultBMsgBResultDocumentation : IO T.MVar_StreamResultBMsgBResultDocumentation
newEmptyMVar_StreamResultBMsgBResultDocumentation =
    IO
        (\_ s ->
            ( { s | mVars_StreamResultBMsgBResultDocumentation = Array.push { subscribers = [], value = Nothing } s.mVars_StreamResultBMsgBResultDocumentation }
            , T.NewEmptyMVar_StreamResultBMsgBResultDocumentation IO.pure (Array.length s.mVars_StreamResultBMsgBResultDocumentation)
            )
        )
        |> IO.fmap T.MVar_StreamResultBMsgBResultDocumentation


newEmptyMVar_ChItemResultBMsgBResultDocumentation : IO T.MVar_ChItemResultBMsgBResultDocumentation
newEmptyMVar_ChItemResultBMsgBResultDocumentation =
    IO
        (\_ s ->
            ( { s | mVars_ChItemResultBMsgBResultDocumentation = Array.push { subscribers = [], value = Nothing } s.mVars_ChItemResultBMsgBResultDocumentation }
            , T.NewEmptyMVar_ChItemResultBMsgBResultDocumentation IO.pure (Array.length s.mVars_ChItemResultBMsgBResultDocumentation)
            )
        )
        |> IO.fmap T.MVar_ChItemResultBMsgBResultDocumentation


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


readMVar_MaybeDep : T.MVar_MaybeDep -> IO (Maybe T.BB_Dep)
readMVar_MaybeDep (T.MVar_MaybeDep ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_MaybeDep of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_MaybeDep IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_MaybeDep = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_MaybeDep index ] } s.mVars_MaybeDep }
                            , T.ReadMVar_MaybeDep IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_MaybeDep: invalid ref"
        )


readMVar_BB_RootResult : T.MVar_BB_RootResult -> IO T.BB_RootResult
readMVar_BB_RootResult (T.MVar_BB_RootResult ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_RootResult of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_BB_RootResult IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_BB_RootResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_BB_RootResult index ] } s.mVars_BB_RootResult }
                            , T.ReadMVar_BB_RootResult IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_BB_RootResult: invalid ref"
        )


readMVar_BB_RootStatus : T.MVar_BB_RootStatus -> IO T.BB_RootStatus
readMVar_BB_RootStatus (T.MVar_BB_RootStatus ref) =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_RootStatus of
                Just mVar ->
                    case mVar.value of
                        Just value ->
                            ( s, T.ReadMVar_BB_RootStatus IO.pure (Just value) )

                        Nothing ->
                            ( { s | mVars_BB_RootStatus = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.ReadMVarSubscriber_BB_RootStatus index ] } s.mVars_BB_RootStatus }
                            , T.ReadMVar_BB_RootStatus IO.pure Nothing
                            )

                Nothing ->
                    crash "Utils.Main.readMVar_BB_RootStatus: invalid ref"
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


putMVar_Result_BuildProjectProblem_RootInfo : T.MVar_Result_BuildProjectProblem_RootInfo -> Result T.BRE_BuildProjectProblem T.BB_RootInfo -> IO ()
putMVar_Result_BuildProjectProblem_RootInfo (T.MVar_Result_BuildProjectProblem_RootInfo ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_Result_BuildProjectProblem_RootInfo of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_Result_BuildProjectProblem_RootInfo = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_Result_BuildProjectProblem_RootInfo index value ] } s.mVars_Result_BuildProjectProblem_RootInfo }
                            , T.PutMVar_Result_BuildProjectProblem_RootInfo IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_Result_BuildProjectProblem_RootInfo readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_Result_BuildProjectProblem_RootInfo = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_Result_BuildProjectProblem_RootInfo }
                            , T.PutMVar_Result_BuildProjectProblem_RootInfo IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_Result_BuildProjectProblem_RootInfo: invalid ref"
        )


putMVar_MaybeDep : T.MVar_MaybeDep -> Maybe T.BB_Dep -> IO ()
putMVar_MaybeDep (T.MVar_MaybeDep ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_MaybeDep of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_MaybeDep = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_MaybeDep index value ] } s.mVars_MaybeDep }
                            , T.PutMVar_MaybeDep IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_MaybeDep readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_MaybeDep = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_MaybeDep }
                            , T.PutMVar_MaybeDep IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_MaybeDep: invalid ref"
        )


putMVar_BB_RootResult : T.MVar_BB_RootResult -> T.BB_RootResult -> IO ()
putMVar_BB_RootResult (T.MVar_BB_RootResult ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_RootResult of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_BB_RootResult = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_BB_RootResult index value ] } s.mVars_BB_RootResult }
                            , T.PutMVar_BB_RootResult IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_BB_RootResult readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_BB_RootResult = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_BB_RootResult }
                            , T.PutMVar_BB_RootResult IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_BB_RootResult: invalid ref"
        )


putMVar_BB_RootStatus : T.MVar_BB_RootStatus -> T.BB_RootStatus -> IO ()
putMVar_BB_RootStatus (T.MVar_BB_RootStatus ref) value =
    IO
        (\index s ->
            case Array.get ref s.mVars_BB_RootStatus of
                Just mVar ->
                    case mVar.value of
                        Just _ ->
                            ( { s | mVars_BB_RootStatus = Array.set ref { mVar | subscribers = mVar.subscribers ++ [ T.PutMVarSubscriber_BB_RootStatus index value ] } s.mVars_BB_RootStatus }
                            , T.PutMVar_BB_RootStatus IO.pure [] Nothing
                            )

                        Nothing ->
                            let
                                ( filteredSubscribers, readIndexes ) =
                                    List.foldr
                                        (\subscriber ( filteredSubscribersAcc, readIndexesAcc ) ->
                                            case subscriber of
                                                T.ReadMVarSubscriber_BB_RootStatus readIndex ->
                                                    ( filteredSubscribersAcc, readIndex :: readIndexesAcc )

                                                _ ->
                                                    ( subscriber :: filteredSubscribersAcc, readIndexesAcc )
                                        )
                                        ( [], [] )
                                        mVar.subscribers
                            in
                            ( { s | mVars_BB_RootStatus = Array.set ref { mVar | subscribers = filteredSubscribers, value = Just value } s.mVars_BB_RootStatus }
                            , T.PutMVar_BB_RootStatus IO.pure readIndexes (Just value)
                            )

                Nothing ->
                    crash "Utils.Main.putMVar_BB_RootStatus: invalid ref"
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


newEmptyMVar_Result_BuildProjectProblem_RootInfo : IO T.MVar_Result_BuildProjectProblem_RootInfo
newEmptyMVar_Result_BuildProjectProblem_RootInfo =
    IO
        (\_ s ->
            ( { s | mVars_Result_BuildProjectProblem_RootInfo = Array.push { subscribers = [], value = Nothing } s.mVars_Result_BuildProjectProblem_RootInfo }
            , T.NewEmptyMVar_Result_BuildProjectProblem_RootInfo IO.pure (Array.length s.mVars_Result_BuildProjectProblem_RootInfo)
            )
        )
        |> IO.fmap T.MVar_Result_BuildProjectProblem_RootInfo


newEmptyMVar_MaybeDep : IO T.MVar_MaybeDep
newEmptyMVar_MaybeDep =
    IO
        (\_ s ->
            ( { s | mVars_MaybeDep = Array.push { subscribers = [], value = Nothing } s.mVars_MaybeDep }
            , T.NewEmptyMVar_MaybeDep IO.pure (Array.length s.mVars_MaybeDep)
            )
        )
        |> IO.fmap T.MVar_MaybeDep


newEmptyMVar_BB_RootResult : IO T.MVar_BB_RootResult
newEmptyMVar_BB_RootResult =
    IO
        (\_ s ->
            ( { s | mVars_BB_RootResult = Array.push { subscribers = [], value = Nothing } s.mVars_BB_RootResult }
            , T.NewEmptyMVar_BB_RootResult IO.pure (Array.length s.mVars_BB_RootResult)
            )
        )
        |> IO.fmap T.MVar_BB_RootResult


newEmptyMVar_BB_RootStatus : IO T.MVar_BB_RootStatus
newEmptyMVar_BB_RootStatus =
    IO
        (\_ s ->
            ( { s | mVars_BB_RootStatus = Array.push { subscribers = [], value = Nothing } s.mVars_BB_RootStatus }
            , T.NewEmptyMVar_BB_RootStatus IO.pure (Array.length s.mVars_BB_RootStatus)
            )
        )
        |> IO.fmap T.MVar_BB_RootStatus


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



-- Control.Concurrent.MVar (List T.MVar_Unit)


newMVar_ListMVar : List T.MVar_Unit -> IO T.MVar_ListMVar
newMVar_ListMVar value =
    newEmptyMVar_ListMVar
        |> IO.bind
            (\mvar ->
                putMVar_ListMVar mvar value
                    |> IO.fmap (\_ -> mvar)
            )


takeMVar_ListMVar : T.MVar_ListMVar -> IO (List T.MVar_Unit)
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


putMVar_ListMVar : T.MVar_ListMVar -> List T.MVar_Unit -> IO ()
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


type Chan_Maybe_DMsg
    = Chan_Maybe_DMsg T.MVar_Stream_Maybe_DMsg T.MVar_Stream_Maybe_DMsg


type Chan_ResultBMsgBResultDocumentation
    = Chan_ResultBMsgBResultDocumentation T.MVar_StreamResultBMsgBResultDocumentation T.MVar_StreamResultBMsgBResultDocumentation


type Chan_ResultBMsgBResultUnit
    = Chan_ResultBMsgBResultUnit T.MVar_StreamResultBMsgBResultUnit T.MVar_StreamResultBMsgBResultUnit


type Chan_ResultBMsgBResultArtifacts
    = Chan_ResultBMsgBResultArtifacts T.MVar_StreamResultBMsgBResultArtifacts T.MVar_StreamResultBMsgBResultArtifacts


newChan_Maybe_DMsg : IO Chan_Maybe_DMsg
newChan_Maybe_DMsg =
    newEmptyMVar_ChItem_Maybe_DMsg
        |> IO.bind
            (\hole ->
                newMVar_Stream_Maybe_DMsg hole
                    |> IO.bind
                        (\readVar ->
                            newMVar_Stream_Maybe_DMsg hole
                                |> IO.fmap
                                    (\writeVar ->
                                        Chan_Maybe_DMsg readVar writeVar
                                    )
                        )
            )


newChan_ResultBMsgBResultDocumentation : IO Chan_ResultBMsgBResultDocumentation
newChan_ResultBMsgBResultDocumentation =
    newEmptyMVar_ChItemResultBMsgBResultDocumentation
        |> IO.bind
            (\hole ->
                newMVar_StreamResultBMsgBResultDocumentation hole
                    |> IO.bind
                        (\readVar ->
                            newMVar_StreamResultBMsgBResultDocumentation hole
                                |> IO.fmap
                                    (\writeVar ->
                                        Chan_ResultBMsgBResultDocumentation readVar writeVar
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


readChan_Maybe_DMsg : Chan_Maybe_DMsg -> IO (Maybe T.BR_DMsg)
readChan_Maybe_DMsg (Chan_Maybe_DMsg readVar _) =
    modifyMVar_Stream_Maybe_DMsg_Maybe_DMsg readVar <|
        \read_end ->
            readMVar_ChItem_Maybe_DMsg read_end
                |> IO.fmap
                    (\(T.ChItem_Maybe_DMsg val new_read_end) ->
                        -- Use readMVar here, not takeMVar,
                        -- else dupChan doesn't work
                        ( new_read_end, val )
                    )


readChan_ResultBMsgBResultDocumentation : Chan_ResultBMsgBResultDocumentation -> IO (Result T.BR_BMsg (T.BR_BResult T.CED_Documentation))
readChan_ResultBMsgBResultDocumentation (Chan_ResultBMsgBResultDocumentation readVar _) =
    modifyMVar_StreamResultBMsgBResultDocumentation_ResultBMsgBResultDocumentation readVar <|
        \read_end ->
            readMVar_ChItemResultBMsgBResultDocumentation read_end
                |> IO.fmap
                    (\(T.ChItem_ResultBMsgBResultDocumentation val new_read_end) ->
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


writeChan_Maybe_DMsg : Chan_Maybe_DMsg -> Maybe T.BR_DMsg -> IO ()
writeChan_Maybe_DMsg (Chan_Maybe_DMsg _ writeVar) val =
    newEmptyMVar_ChItem_Maybe_DMsg
        |> IO.bind
            (\new_hole ->
                takeMVar_Stream_Maybe_DMsg writeVar
                    |> IO.bind
                        (\old_hole ->
                            putMVar_ChItem_Maybe_DMsg old_hole (T.ChItem_Maybe_DMsg val new_hole)
                                |> IO.bind (\_ -> putMVar_Stream_Maybe_DMsg writeVar new_hole)
                        )
            )


writeChan_ResultBMsgBResultDocumentation : Chan_ResultBMsgBResultDocumentation -> Result T.BR_BMsg (T.BR_BResult T.CED_Documentation) -> IO ()
writeChan_ResultBMsgBResultDocumentation (Chan_ResultBMsgBResultDocumentation _ writeVar) val =
    newEmptyMVar_ChItemResultBMsgBResultDocumentation
        |> IO.bind
            (\new_hole ->
                takeMVar_StreamResultBMsgBResultDocumentation writeVar
                    |> IO.bind
                        (\old_hole ->
                            putMVar_ChItemResultBMsgBResultDocumentation old_hole (T.ChItem_ResultBMsgBResultDocumentation val new_hole)
                                |> IO.bind (\_ -> putMVar_StreamResultBMsgBResultDocumentation writeVar new_hole)
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
