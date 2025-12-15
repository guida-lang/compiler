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
    Utils.dirGetCurrentDirectory
        |> Task.bind
            (\dir ->
                findRootHelp (Utils.fpSplitDirectories dir)
            )


findRootHelp : List String -> Task Never (Maybe Root)
findRootHelp dirs =
    case dirs of
        [] ->
            Task.pure Nothing

        _ :: _ ->
            Utils.dirDoesFileExist (Utils.fpJoinPath dirs ++ "/guida.json")
                |> Task.bind
                    (\guidaExists ->
                        if guidaExists then
                            Task.pure (Just (GuidaRoot (Utils.fpJoinPath dirs)))

                        else
                            Utils.dirDoesFileExist (Utils.fpJoinPath dirs ++ "/elm.json")
                                |> Task.bind
                                    (\elmExists ->
                                        if elmExists then
                                            Task.pure (Just (ElmRoot TopLevel (Utils.fpJoinPath dirs)))

                                        else
                                            findRootHelp (Prelude.init dirs)
                                    )
                    )


findRootIn : Utils.FilePath -> Task Never (Maybe Root)
findRootIn path =
    Utils.dirDoesFileExist (path ++ "/guida.json")
        |> Task.bind
            (\guidaExists ->
                if guidaExists then
                    Task.pure (Just (GuidaRoot path))

                else
                    Utils.dirDoesFileExist (path ++ "/elm.json")
                        |> Task.bind
                            (\elmExists ->
                                if elmExists then
                                    Task.pure (Just (ElmRoot Dependency path))

                                else
                                    Task.pure Nothing
                            )
            )



-- LOCKS


withRootLock : String -> Task Never a -> Task Never a
withRootLock root work =
    let
        dir : String
        dir =
            stuff root
    in
    Utils.dirCreateDirectoryIfMissing True dir
        |> Task.bind
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
    Task.fmap PackageCache (getCacheDir "packages")


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
        |> Task.bind
            (\home ->
                let
                    root : Utils.FilePath
                    root =
                        Utils.fpCombine home (Utils.fpCombine compilerVersion projectName)
                in
                Utils.dirCreateDirectoryIfMissing True root
                    |> Task.fmap (\_ -> root)
            )


getGuidaHome : Task Never String
getGuidaHome =
    Utils.envLookupEnv "GUIDA_HOME"
        |> Task.bind
            (\maybeCustomHome ->
                case maybeCustomHome of
                    Just customHome ->
                        Task.pure customHome

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
