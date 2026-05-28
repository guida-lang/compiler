module Builder.Stuff exposing
    ( Level(..)
    , PackageCache
    , Root(..)
    , details
    , findRoot
    , findRootIn
    , getGuidaHome
    , getPackageCache
    , getReplCache
    , guidai
    , guidao
    , guidaw
    , interfaces
    , isRootGuida
    , objects
    , package
    , packageCacheDecoder
    , packageCacheEncoder
    , prepublishDir
    , registry
    , rootFilename
    , rootMap
    , rootPath
    , rootProjectFilePath
    , rootToTarget
    , testDir
    , withRegistryLock
    , withRootLock
    )

{-| Utilities for locating build roots and the compiler's storage paths.

This module defines the `Root` value for package or application roots,
path helpers for the local `guida-stuff` cache, and file locking helpers
used during builds and package registry access.

-}

import Compiler.Generate.Target as Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Prelude
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- PATHS


{-| Build storage root for a project.

The `guida-stuff` directory holds build caches, compiled artifacts, and
other intermediate state for a given project root.

-}
stuff : String -> String
stuff root =
    root ++ "/guida-stuff/" ++ compilerVersion


{-| Path to the cached details file for a project root.
-}
details : String -> String
details root =
    stuff root ++ "/d.dat"


{-| Path to the compiled interface cache file.
-}
interfaces : String -> String
interfaces root =
    stuff root ++ "/i.dat"


{-| Path to the compiled object cache file.
-}
objects : String -> String
objects root =
    stuff root ++ "/o.dat"


{-| Directory used when preparing a package for publishing.
-}
prepublishDir : String -> String
prepublishDir root =
    stuff root ++ "/prepublish"


{-| Directory used to store temporary REPL cache files.
-}
testDir : String -> String
testDir root =
    stuff root ++ "/test"


{-| Compiler version string used in `guida-stuff` paths.

This value is derived from `Compiler.Guida.Version.compiler` and is
embedded in per-project cache directory names so that artifacts from
different compiler versions do not collide.

-}
compilerVersion : String
compilerVersion =
    V.toChars V.compiler



-- GUIDAI and GUIDAO


{-| Path to the compiled module interface file for a project root.
-}
guidai : String -> ModuleName.Raw -> String
guidai root name =
    toArtifactPath root name "guidai"


{-| Path to the compiled module local graph file for a project root.
-}
guidaw : String -> ModuleName.Raw -> String
guidaw root name =
    toArtifactPath root name "guidaw"


{-| Path to the compiled module object file for a project root.
-}
guidao : String -> ModuleName.Raw -> String
guidao root name =
    toArtifactPath root name "guidao"


{-| Build the artifact path for a named module with an extension.
-}
toArtifactPath : String -> ModuleName.Raw -> String -> String
toArtifactPath root name ext =
    Utils.fpCombine (stuff root) (Utils.fpAddExtension (ModuleName.toHyphenPath name) ext)



-- ROOT


{-| Distinguish whether an Elm root is the current project root or a
package dependency root.
-}
type Level
    = TopLevel
    | Dependency


{-| A discovered project root, either from Guida or Elm sources.

`GuidaRoot` represents a project containing `guida.json`.
`ElmRoot` represents a project containing `elm.json` and carries a `Level`
indicating whether it is the current project or a dependency.

-}
type Root
    = GuidaRoot String
    | ElmRoot Level String


{-| Convert a root into the compilation target for the current build.
-}
rootToTarget : Root -> Target
rootToTarget root =
    case root of
        GuidaRoot _ ->
            Target.GuidaTarget

        ElmRoot TopLevel _ ->
            Target.ElmTarget

        ElmRoot Dependency _ ->
            crash "Error when trying to use dependency root information to determine target."


{-| Return `True` when the root is a Guida project root.
-}
isRootGuida : Root -> Bool
isRootGuida root =
    case root of
        GuidaRoot _ ->
            True

        ElmRoot _ _ ->
            False


{-| The project file path for a root, either `guida.json` or `elm.json`.
-}
rootProjectFilePath : Root -> Utils.FilePath
rootProjectFilePath root =
    case root of
        GuidaRoot path ->
            path ++ "/guida.json"

        ElmRoot _ path ->
            path ++ "/elm.json"


{-| The project file name for a root.
-}
rootFilename : Root -> String
rootFilename root =
    case root of
        GuidaRoot _ ->
            "guida.json"

        ElmRoot _ _ ->
            "elm.json"


{-| The filesystem path to the root directory.
-}
rootPath : Root -> Utils.FilePath
rootPath root =
    case root of
        GuidaRoot path ->
            path

        ElmRoot _ path ->
            path


{-| Transform the filesystem path of a root while preserving its type.
-}
rootMap : (String -> String) -> Root -> Root
rootMap f root =
    case root of
        GuidaRoot path ->
            GuidaRoot (f path)

        ElmRoot level path ->
            ElmRoot level (f path)


{-| Discover the nearest project root from the current directory.
-}
findRoot : Task Never (Maybe Root)
findRoot =
    Utils.dirGetCurrentDirectory
        |> Task.andThen
            (\dir ->
                findRootHelp (Utils.fpSplitDirectories dir)
            )


{-| Helper that walks upward through a list of directory path segments
to find the nearest project root. It checks for `guida.json` first and
then `elm.json` in each directory; returns `Nothing` when no root is
found.
-}
findRootHelp : List String -> Task Never (Maybe Root)
findRootHelp dirs =
    case dirs of
        [] ->
            Task.succeed Nothing

        _ :: _ ->
            Utils.dirDoesFileExist (Utils.fpJoinPath dirs ++ "/guida.json")
                |> Task.andThen
                    (\guidaExists ->
                        if guidaExists then
                            Task.succeed (Just (GuidaRoot (Utils.fpJoinPath dirs)))

                        else
                            Utils.dirDoesFileExist (Utils.fpJoinPath dirs ++ "/elm.json")
                                |> Task.andThen
                                    (\elmExists ->
                                        if elmExists then
                                            Task.succeed (Just (ElmRoot TopLevel (Utils.fpJoinPath dirs)))

                                        else
                                            findRootHelp (Prelude.init dirs)
                                    )
                    )


{-| Discover a project root by walking upwards from a specific path.
-}
findRootIn : Utils.FilePath -> Task Never (Maybe Root)
findRootIn path =
    findRootHelp (Utils.fpSplitDirectories path)



-- LOCKS


{-| Acquire an exclusive build lock for a project root.

This creates the lock directory under `guida-stuff` and holds a file lock
for the duration of the provided work task.

-}
withRootLock : String -> Task Never a -> Task Never a
withRootLock root work =
    let
        dir : String
        dir =
            stuff root
    in
    Utils.dirCreateDirectoryIfMissing True dir
        |> Task.andThen
            (\_ ->
                Utils.lockWithFileLock (dir ++ "/lock") Utils.LockExclusive (\_ -> work)
            )


{-| Acquire an exclusive lock for registry operations in the package cache.
-}
withRegistryLock : PackageCache -> Task Never a -> Task Never a
withRegistryLock (PackageCache dir) work =
    Utils.lockWithFileLock (dir ++ "/lock") Utils.LockExclusive (\_ -> work)



-- PACKAGE CACHES


{-| Location of the global package cache used by Guida.
-}
type PackageCache
    = PackageCache String


{-| Resolve the package cache directory, creating it if needed.
-}
getPackageCache : Task Never PackageCache
getPackageCache =
    Task.map PackageCache (getCacheDir "packages")


{-| Path to the registry index file within the package cache.
-}
registry : PackageCache -> String
registry (PackageCache dir) =
    Utils.fpCombine dir "registry.dat"


{-| Path to a specific package version within the package cache.
-}
package : PackageCache -> Pkg.Name -> V.Version -> String
package (PackageCache dir) name version =
    Utils.fpCombine dir (Utils.fpCombine (Pkg.toString name) (V.toChars version))



-- CACHE


{-| Resolve the REPL cache directory for the current user.
-}
getReplCache : Task Never String
getReplCache =
    getCacheDir "repl"


{-| Resolve a named subdirectory in the user's Guida home cache.
-}
getCacheDir : String -> Task Never String
getCacheDir projectName =
    getGuidaHome
        |> Task.andThen
            (\home ->
                let
                    root : Utils.FilePath
                    root =
                        Utils.fpCombine home (Utils.fpCombine compilerVersion projectName)
                in
                Utils.dirCreateDirectoryIfMissing True root
                    |> Task.map (\_ -> root)
            )


{-| Determine the base Guida cache directory for the current user.

If the `GUIDA_HOME` environment variable is set, it is used. Otherwise the
platform-specific application data directory for `guida` is returned.

-}
getGuidaHome : Task Never String
getGuidaHome =
    Utils.envLookupEnv "GUIDA_HOME"
        |> Task.andThen
            (\maybeCustomHome ->
                case maybeCustomHome of
                    Just customHome ->
                        Task.succeed customHome

                    Nothing ->
                        Utils.dirGetAppUserDataDirectory "guida"
            )



-- ENCODERS and DECODERS


packageCacheEncoder : PackageCache -> BE.Encoder
packageCacheEncoder (PackageCache dir) =
    BE.string dir


packageCacheDecoder : BD.Decoder PackageCache
packageCacheDecoder =
    BD.map PackageCache BD.string
