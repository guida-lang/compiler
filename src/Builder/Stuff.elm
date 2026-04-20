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

import Compiler.Generate.Target as Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
import Prelude
import System.Directory as Dir
import System.Environment as Env
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Crash exposing (crash)
import Utils.Main as Utils
import Utils.Task.Extra as Task



-- PATHS


stuff : String -> String
stuff root =
    root ++ "/guida-stuff/" ++ compilerVersion


details : String -> String
details root =
    stuff root ++ "/d.dat"


interfaces : String -> String
interfaces root =
    stuff root ++ "/i.dat"


objects : String -> String
objects root =
    stuff root ++ "/o.dat"


prepublishDir : String -> String
prepublishDir root =
    stuff root ++ "/prepublish"


testDir : String -> String
testDir root =
    stuff root ++ "/test"


compilerVersion : String
compilerVersion =
    V.toChars V.compiler



-- GUIDAI and GUIDAO


guidai : String -> ModuleName.Raw -> String
guidai root name =
    toArtifactPath root name "guidai"


guidao : String -> ModuleName.Raw -> String
guidao root name =
    toArtifactPath root name "guidao"


toArtifactPath : String -> ModuleName.Raw -> String -> String
toArtifactPath root name ext =
    Utils.fpCombine (stuff root) (Utils.fpAddExtension (ModuleName.toHyphenPath name) ext)



-- ROOT


type Level
    = TopLevel
    | Dependency


type Root
    = GuidaRoot String
    | ElmRoot Level String


rootToTarget : Root -> Target
rootToTarget root =
    case root of
        GuidaRoot _ ->
            Target.GuidaTarget

        ElmRoot TopLevel _ ->
            Target.ElmTarget

        ElmRoot Dependency _ ->
            crash "Error when trying to use dependency root information to determine target."


isRootGuida : Root -> Bool
isRootGuida root =
    case root of
        GuidaRoot _ ->
            True

        ElmRoot _ _ ->
            False


rootProjectFilePath : Root -> Utils.FilePath
rootProjectFilePath root =
    case root of
        GuidaRoot path ->
            path ++ "/guida.json"

        ElmRoot _ path ->
            path ++ "/elm.json"


rootFilename : Root -> String
rootFilename root =
    case root of
        GuidaRoot _ ->
            "guida.json"

        ElmRoot _ _ ->
            "elm.json"


rootPath : Root -> Utils.FilePath
rootPath root =
    case root of
        GuidaRoot path ->
            path

        ElmRoot _ path ->
            path


rootMap : (String -> String) -> Root -> Root
rootMap f root =
    case root of
        GuidaRoot path ->
            GuidaRoot (f path)

        ElmRoot level path ->
            ElmRoot level (f path)


findRoot : Task Never (Maybe Root)
findRoot =
    Dir.getCurrentDirectory
        |> Task.andThen
            (\dir ->
                findRootHelp (Utils.fpSplitDirectories dir)
            )


findRootHelp : List String -> Task Never (Maybe Root)
findRootHelp dirs =
    case dirs of
        [] ->
            Task.succeed Nothing

        _ :: _ ->
            Dir.doesFileExist (Utils.fpJoinPath dirs ++ "/guida.json")
                |> Task.andThen
                    (\guidaExists ->
                        if guidaExists then
                            Task.succeed (Just (GuidaRoot (Utils.fpJoinPath dirs)))

                        else
                            Dir.doesFileExist (Utils.fpJoinPath dirs ++ "/elm.json")
                                |> Task.andThen
                                    (\elmExists ->
                                        if elmExists then
                                            Task.succeed (Just (ElmRoot TopLevel (Utils.fpJoinPath dirs)))

                                        else
                                            findRootHelp (Prelude.init dirs)
                                    )
                    )


findRootIn : Utils.FilePath -> Task Never (Maybe Root)
findRootIn path =
    findRootHelp (Utils.fpSplitDirectories path)



-- LOCKS


withRootLock : String -> Task x a -> Task x a
withRootLock root work =
    let
        dir : String
        dir =
            stuff root
    in
    Dir.createDirectoryIfMissing True dir
        |> Task.io
        |> Task.andThen
            (\_ ->
                Utils.lockWithFileLock (dir ++ "/lock") Utils.LockExclusive (\_ -> work)
            )


withRegistryLock : PackageCache -> Task Never a -> Task Never a
withRegistryLock (PackageCache dir) work =
    Utils.lockWithFileLock (dir ++ "/lock") Utils.LockExclusive (\_ -> work)



-- PACKAGE CACHES


type PackageCache
    = PackageCache String


getPackageCache : Task Never PackageCache
getPackageCache =
    Task.map PackageCache (getCacheDir "packages")


registry : PackageCache -> String
registry (PackageCache dir) =
    Utils.fpCombine dir "registry.dat"


package : PackageCache -> Pkg.Name -> V.Version -> String
package (PackageCache dir) name version =
    Utils.fpCombine dir (Utils.fpCombine (Pkg.toString name) (V.toChars version))



-- CACHE


getReplCache : Task Never String
getReplCache =
    getCacheDir "repl"


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
                Dir.createDirectoryIfMissing True root
                    |> Task.map (\_ -> root)
            )


getGuidaHome : Task Never String
getGuidaHome =
    Env.lookupEnv "GUIDA_HOME"
        |> Task.andThen
            (\maybeCustomHome ->
                case maybeCustomHome of
                    Just customHome ->
                        Task.succeed customHome

                    Nothing ->
                        Dir.getAppUserDataDirectory "guida"
            )



-- ENCODERS and DECODERS


packageCacheEncoder : PackageCache -> BE.Encoder
packageCacheEncoder (PackageCache dir) =
    BE.string dir


packageCacheDecoder : BD.Decoder PackageCache
packageCacheDecoder =
    BD.map PackageCache BD.string
