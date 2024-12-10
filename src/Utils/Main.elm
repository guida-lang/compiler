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
    , mVarEncoder
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
    , someExceptionDecoder
    , someExceptionEncoder
    , takeMVar
    , unlines
    , unzip3
    , writeChan
    , zipWithM
    )

import Basics.Extra exposing (flip)
import Builder.Reporting.Task as Task exposing (Task)
import Compiler.Data.Index as Index
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Version exposing (toComparable)
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Reporting.Result as R
import Control.Monad.State.Strict as State
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Prelude
import System.Exit as Exit
import System.IO as IO exposing (IO(..))
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


mapFromListWith : (k -> comparable) -> (a -> a -> a) -> List ( k, a ) -> Dict comparable k a
mapFromListWith toComparable f =
    List.foldl
        (\( k, a ) ->
            Dict.update toComparable k (Maybe.map (flip f a))
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


mapFromKeys : (k -> comparable) -> (k -> v) -> List k -> Dict comparable k v
mapFromKeys toComparable f =
    List.map (\k -> ( k, f k ))
        >> Dict.fromList toComparable


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
    case Dict.get toComparable k items of
        Just item ->
            item

        Nothing ->
            crash "Map.!: given key is not an element in the map"


mapLookupMin : Dict comparable comparable a -> Maybe ( comparable, a )
mapLookupMin dict =
    case List.sortBy Tuple.first (Dict.toList compare dict) of
        firstElem :: _ ->
            Just firstElem

        _ ->
            Nothing


mapFindMin : Dict comparable comparable a -> ( comparable, a )
mapFindMin dict =
    case List.sortBy Tuple.first (Dict.toList compare dict) of
        firstElem :: _ ->
            firstElem

        _ ->
            crash "Error: empty map has no minimal element"


mapInsertWith : (k -> comparable) -> (a -> a -> a) -> k -> a -> Dict comparable k a -> Dict comparable k a
mapInsertWith toComparable f k a =
    Dict.update toComparable k (Maybe.map (f a) >> Maybe.withDefault a >> Just)


mapIntersectionWith : (k -> comparable) -> (k -> k -> Order) -> (a -> b -> c) -> Dict comparable k a -> Dict comparable k b -> Dict comparable k c
mapIntersectionWith toComparable keyComparison func =
    mapIntersectionWithKey toComparable keyComparison (\_ -> func)


mapIntersectionWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> b -> c) -> Dict comparable k a -> Dict comparable k b -> Dict comparable k c
mapIntersectionWithKey toComparable keyComparison func dict1 dict2 =
    Dict.merge keyComparison (\_ _ -> identity) (\k v1 v2 -> Dict.insert toComparable k (func k v1 v2)) (\_ _ -> identity) dict1 dict2 Dict.empty


mapUnionWith : (k -> comparable) -> (k -> k -> Order) -> (a -> a -> a) -> Dict comparable k a -> Dict comparable k a -> Dict comparable k a
mapUnionWith toComparable keyComparison f a b =
    Dict.merge keyComparison (Dict.insert toComparable) (\k va vb -> Dict.insert toComparable k (f va vb)) (Dict.insert toComparable) a b Dict.empty


mapUnionsWith : (k -> comparable) -> (k -> k -> Order) -> (a -> a -> a) -> List (Dict comparable k a) -> Dict comparable k a
mapUnionsWith toComparable keyComparison f =
    List.foldl (mapUnionWith toComparable keyComparison f) Dict.empty


mapUnions : List (Dict comparable k a) -> Dict comparable k a
mapUnions =
    List.foldr Dict.union Dict.empty


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
    Dict.foldr keyComparison (\k x acc -> R.apply acc (R.fmap (Dict.insert toComparable k) x)) (R.pure Dict.empty)


sequenceAList : List (R.RResult i w e v) -> R.RResult i w e (List v)
sequenceAList =
    List.foldr (\x acc -> R.apply acc (R.fmap (::) x)) (R.pure [])


sequenceDictMaybe : (k -> comparable) -> (k -> k -> Order) -> Dict comparable k (Maybe a) -> Maybe (Dict comparable k a)
sequenceDictMaybe toComparable keyComparison =
    Dict.foldr keyComparison (\k -> Maybe.map2 (Dict.insert toComparable k)) (Just Dict.empty)


sequenceDictResult : (k -> comparable) -> (k -> k -> Order) -> Dict comparable k (Result e v) -> Result e (Dict comparable k v)
sequenceDictResult toComparable keyComparison =
    Dict.foldr keyComparison (\k -> Result.map2 (Dict.insert toComparable k)) (Ok Dict.empty)


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
    Dict.keys keyComparison >> EverySet.fromList toComparable


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
    Dict.foldl keyComparison c (IO.pure ())


maybeMapM : (a -> Maybe b) -> List a -> Maybe (List b)
maybeMapM =
    listMaybeTraverse


mapMinViewWithKey : (k -> comparable) -> (k -> k -> Order) -> (( k, a ) -> comparable) -> Dict comparable k a -> Maybe ( ( k, a ), Dict comparable k a )
mapMinViewWithKey toComparable keyComparison compare dict =
    case List.sortBy compare (Dict.toList keyComparison dict) of
        first :: tail ->
            Just ( first, Dict.fromList toComparable tail )

        _ ->
            Nothing


mapMapMaybe : (k -> comparable) -> (k -> k -> Order) -> (a -> Maybe b) -> Dict comparable k a -> Dict comparable k b
mapMapMaybe toComparable keyComparison func =
    Dict.toList keyComparison
        >> List.filterMap (\( k, a ) -> Maybe.map (Tuple.pair k) (func a))
        >> Dict.fromList toComparable


mapTraverse : (k -> comparable) -> (k -> k -> Order) -> (a -> IO b) -> Dict comparable k a -> IO (Dict comparable k b)
mapTraverse toComparable keyComparison f =
    mapTraverseWithKey toComparable keyComparison (\_ -> f)


mapTraverseWithKey : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> IO b) -> Dict comparable k a -> IO (Dict comparable k b)
mapTraverseWithKey toComparable keyComparison f =
    Dict.foldl keyComparison
        (\k a -> IO.bind (\c -> IO.fmap (\va -> Dict.insert toComparable k va c) (f k a)))
        (IO.pure Dict.empty)


mapTraverseResult : (k -> comparable) -> (k -> k -> Order) -> (a -> Result e b) -> Dict comparable k a -> Result e (Dict comparable k b)
mapTraverseResult toComparable keyComparison f =
    mapTraverseWithKeyResult toComparable keyComparison (\_ -> f)


mapTraverseWithKeyResult : (k -> comparable) -> (k -> k -> Order) -> (k -> a -> Result e b) -> Dict comparable k a -> Result e (Dict comparable k b)
mapTraverseWithKeyResult toComparable keyComparison f =
    Dict.foldl keyComparison
        (\k a -> Result.map2 (Dict.insert toComparable k) (f k a))
        (Ok Dict.empty)


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


lockFile : FilePath -> IO ()
lockFile path =
    IO (\s -> ( s, IO.LockFile IO.pure path ))


unlockFile : FilePath -> IO ()
unlockFile path =
    IO (\s -> ( s, IO.UnlockFile IO.pure path ))



-- System.Directory


dirDoesFileExist : FilePath -> IO Bool
dirDoesFileExist filename =
    IO (\s -> ( s, IO.DirDoesFileExist IO.pure filename ))


dirFindExecutable : FilePath -> IO (Maybe FilePath)
dirFindExecutable filename =
    IO (\s -> ( s, IO.DirFindExecutable IO.pure filename ))


dirCreateDirectoryIfMissing : Bool -> FilePath -> IO ()
dirCreateDirectoryIfMissing createParents filename =
    IO (\s -> ( s, IO.DirCreateDirectoryIfMissing IO.pure createParents filename ))


dirGetCurrentDirectory : IO String
dirGetCurrentDirectory =
    IO (\s -> ( s, IO.Pure s.currentDirectory ))


dirGetAppUserDataDirectory : FilePath -> IO FilePath
dirGetAppUserDataDirectory filename =
    IO (\s -> ( s, IO.Pure (s.homedir ++ "/." ++ filename) ))


dirGetModificationTime : FilePath -> IO Time.Posix
dirGetModificationTime filename =
    IO (\s -> ( s, IO.DirGetModificationTime IO.pure filename ))
        |> IO.fmap Time.millisToPosix


dirRemoveFile : FilePath -> IO ()
dirRemoveFile path =
    IO (\s -> ( s, IO.DirRemoveFile IO.pure path ))


dirRemoveDirectoryRecursive : FilePath -> IO ()
dirRemoveDirectoryRecursive path =
    IO (\s -> ( s, IO.DirRemoveDirectoryRecursive IO.pure path ))


dirDoesDirectoryExist : FilePath -> IO Bool
dirDoesDirectoryExist path =
    IO (\s -> ( s, IO.DirDoesDirectoryExist IO.pure path ))


dirCanonicalizePath : FilePath -> IO FilePath
dirCanonicalizePath path =
    IO (\s -> ( s, IO.DirCanonicalizePath IO.pure path ))


dirWithCurrentDirectory : FilePath -> IO a -> IO a
dirWithCurrentDirectory dir action =
    dirGetCurrentDirectory
        |> IO.bind
            (\currentDir ->
                bracket_
                    (IO (\s -> ( s, IO.DirWithCurrentDirectory IO.pure dir )))
                    (IO (\s -> ( s, IO.DirWithCurrentDirectory IO.pure currentDir )))
                    action
            )



-- System.Environment


envLookupEnv : String -> IO (Maybe String)
envLookupEnv name =
    IO (\s -> ( s, IO.Pure (Dict.get identity name s.envVars) ))


envGetProgName : IO String
envGetProgName =
    IO (\s -> ( s, IO.Pure s.progName ))


envGetArgs : IO (List String)
envGetArgs =
    IO (\s -> ( s, IO.Pure s.args ))



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
    IO (\s -> ( s, IO.ForkIO (\() -> IO.pure ThreadId) ioArg ))



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
    IO (\s -> ( s, IO.ReadMVar IO.pure ref ))
        |> IO.fmap
            (\encodedValue ->
                case Decode.decodeValue decoder encodedValue of
                    Ok value ->
                        value

                    Err _ ->
                        crash "Utils.Main.readMVar: invalid value"
            )


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
    IO (\s -> ( s, IO.TakeMVar IO.pure ref ))
        |> IO.fmap
            (\encodedValue ->
                case Decode.decodeValue decoder encodedValue of
                    Ok value ->
                        value

                    Err _ ->
                        crash "Utils.Main.takeMVar: invalid value"
            )


putMVar : (a -> Encode.Value) -> MVar a -> a -> IO ()
putMVar encoder (MVar ref) value =
    IO (\s -> ( s, IO.PutMVar IO.pure ref (encoder value) ))


newEmptyMVar : IO (MVar a)
newEmptyMVar =
    IO (\s -> ( s, IO.NewEmptyMVar IO.pure ))
        |> IO.fmap MVar



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



-- Data.ByteString.Builder


builderHPutBuilder : IO.Handle -> String -> IO ()
builderHPutBuilder handle str =
    IO (\s -> ( s, IO.HPutStr IO.pure handle str ))



-- Data.Binary


binaryDecodeFileOrFail : Decode.Decoder a -> FilePath -> IO (Result ( Int, String ) a)
binaryDecodeFileOrFail decoder filename =
    IO (\s -> ( s, IO.BinaryDecodeFileOrFail IO.pure filename ))
        |> IO.fmap
            (Decode.decodeValue decoder
                >> Result.mapError (\_ -> ( 0, "Could not find file " ++ filename ))
            )


binaryEncodeFile : (a -> Encode.Value) -> FilePath -> a -> IO ()
binaryEncodeFile encoder path value =
    IO (\s -> ( s, IO.Write IO.pure path (encoder value) ))



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
    IO (\s -> ( s, IO.ReplGetInputLine IO.pure prompt ))


replGetInputLineWithInitial : String -> ( String, String ) -> ReplInputT (Maybe String)
replGetInputLineWithInitial prompt ( left, right ) =
    IO (\s -> ( s, IO.ReplGetInputLineWithInitial IO.pure prompt left right ))



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
