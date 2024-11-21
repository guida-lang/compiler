module Builder.Stuff exposing
    ( PackageCache
    , details
    , elmi
    , elmo
    , findRoot
    , getElmHome
    , getPackageCache
    , getReplCache
    , interfaces
    , objects
    , package
    , packageCacheDecoder
    , packageCacheEncoder
    , prepublishDir
    , registry
    , withRegistryLock
    , withRootLock
    )

import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Json.Decode as Decode
import Json.Encode as Encode
import Prelude
import System.IO as IO exposing (IO)
import Utils.Main as Utils



-- PATHS


stuff : String -> String
stuff root =
    root ++ "/guida-stuff/" ++ compilerVersion


details : String -> String
details root =
    stuff root ++ "/d.json"


interfaces : String -> String
interfaces root =
    stuff root ++ "/i.json"


objects : String -> String
objects root =
    stuff root ++ "/o.json"


prepublishDir : String -> String
prepublishDir root =
    stuff root ++ "/prepublish"


compilerVersion : String
compilerVersion =
    V.toChars V.compiler



-- ELMI and ELMO


elmi : String -> ModuleName.Raw -> String
elmi root name =
    toArtifactPath root name "elmi"


elmo : String -> ModuleName.Raw -> String
elmo root name =
    toArtifactPath root name "elmo"


toArtifactPath : String -> ModuleName.Raw -> String -> String
toArtifactPath root name ext =
    Utils.fpForwardSlash (stuff root) (Utils.fpAddExtension (ModuleName.toHyphenPath name) ext)



-- ROOT


findRoot : IO (Maybe String)
findRoot =
    Utils.dirGetCurrentDirectory
        |> IO.bind
            (\dir ->
                findRootHelp (Utils.fpSplitDirectories dir)
            )


findRootHelp : List String -> IO (Maybe String)
findRootHelp dirs =
    case dirs of
        [] ->
            IO.pure Nothing

        _ :: _ ->
            Utils.dirDoesFileExist (Utils.fpJoinPath dirs ++ "/elm.json")
                |> IO.bind
                    (\exists ->
                        if exists then
                            IO.pure (Just (Utils.fpJoinPath dirs))

                        else
                            findRootHelp (Prelude.init dirs)
                    )



-- LOCKS


withRootLock : String -> IO a -> IO a
withRootLock root work =
    let
        dir : String
        dir =
            stuff root
    in
    Utils.dirCreateDirectoryIfMissing True dir
        |> IO.bind
            (\_ ->
                Utils.lockWithFileLock (dir ++ "/lock") Utils.LockExclusive (\_ -> work)
            )


withRegistryLock : PackageCache -> IO a -> IO a
withRegistryLock (PackageCache dir) work =
    Utils.lockWithFileLock (dir ++ "/lock") Utils.LockExclusive (\_ -> work)



-- PACKAGE CACHES


type PackageCache
    = PackageCache String


getPackageCache : IO PackageCache
getPackageCache =
    IO.fmap PackageCache (getCacheDir "packages")


registry : PackageCache -> String
registry (PackageCache dir) =
    Utils.fpForwardSlash dir "registry.json"


package : PackageCache -> Pkg.Name -> V.Version -> String
package (PackageCache dir) name version =
    Utils.fpForwardSlash dir (Utils.fpForwardSlash (Pkg.toString name) (V.toChars version))



-- CACHE


getReplCache : IO String
getReplCache =
    getCacheDir "repl"


getCacheDir : String -> IO String
getCacheDir projectName =
    getElmHome
        |> IO.bind
            (\home ->
                let
                    root : Utils.FilePath
                    root =
                        Utils.fpForwardSlash home (Utils.fpForwardSlash compilerVersion projectName)
                in
                Utils.dirCreateDirectoryIfMissing True root
                    |> IO.fmap (\_ -> root)
            )


getElmHome : IO String
getElmHome =
    Utils.envLookupEnv "GUIDA_HOME"
        |> IO.bind
            (\maybeCustomHome ->
                case maybeCustomHome of
                    Just customHome ->
                        IO.pure customHome

                    Nothing ->
                        Utils.dirGetAppUserDataDirectory "guida"
            )



-- ENCODERS and DECODERS


packageCacheEncoder : PackageCache -> Encode.Value
packageCacheEncoder (PackageCache dir) =
    Encode.object
        [ ( "type", Encode.string "PackageCache" )
        , ( "dir", Encode.string dir )
        ]


packageCacheDecoder : Decode.Decoder PackageCache
packageCacheDecoder =
    Decode.map PackageCache (Decode.field "dir" Decode.string)
