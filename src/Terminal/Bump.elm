module Terminal.Bump exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Deps.Bump as Bump
import Builder.Deps.Diff as Diff
import Builder.Deps.Registry as Registry
import Builder.Deps.Website as Website
import Builder.Guida.Details as Details
import Builder.Guida.Outline as Outline
import Builder.Http as Http
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help as Help
import Builder.Stuff as Stuff
import Compiler.Data.NonEmptyList as NE
import Compiler.Guida.Docs as Docs
import Compiler.Guida.Magnitude as M
import Compiler.Guida.Version as V
import Compiler.Reporting.Doc as D
import Prelude
import System.Exit as Exit
import System.IO as IO
import Task exposing (Task)
import Utils.Main as Utils
import Utils.System.IO as IO
import Utils.Task.Extra as Task



-- RUN


run : () -> () -> Task Exit.ExitCode ()
run () () =
    Reporting.attempt Exit.bumpToReport <|
        Task.run (Task.andThen bump getEnv)



-- ENV


type Env
    = Env Stuff.Root Stuff.PackageCache Http.Manager Registry.Registry Outline.PkgOutline


getEnv : Task Exit.Bump Env
getEnv =
    Task.io Stuff.findRoot
        |> Task.andThen
            (\maybeRoot ->
                case maybeRoot of
                    Nothing ->
                        Task.fail Exit.BumpNoOutline

                    Just root ->
                        Task.io Stuff.getPackageCache
                            |> Task.andThen
                                (\cache ->
                                    Task.io Http.getManager
                                        |> Task.andThen
                                            (\manager ->
                                                Task.eio Exit.BumpMustHaveLatestRegistry (Registry.latest manager cache)
                                                    |> Task.andThen
                                                        (\registry ->
                                                            Task.eio Exit.BumpBadOutline (Outline.read root)
                                                                |> Task.andThen
                                                                    (\outline ->
                                                                        case outline of
                                                                            Outline.App appOutline ->
                                                                                Task.io Website.domain
                                                                                    |> Task.andThen
                                                                                        (\registryDomain ->
                                                                                            Task.fail
                                                                                                (case appOutline of
                                                                                                    Outline.GuidaAppOutline _ _ _ _ _ _ ->
                                                                                                        Exit.BumpGuidaApplication registryDomain

                                                                                                    Outline.ElmAppOutline _ _ _ _ _ _ ->
                                                                                                        Exit.BumpElmApplication registryDomain
                                                                                                )
                                                                                        )

                                                                            Outline.Pkg pkgOutline ->
                                                                                Task.succeed (Env root cache manager registry pkgOutline)
                                                                    )
                                                        )
                                            )
                                )
            )



-- BUMP


bump : Env -> Task Exit.Bump ()
bump ((Env root _ _ registry outline) as env) =
    Task.io Website.domain
        |> Task.andThen
            (\registryDomain ->
                case outline of
                    Outline.GuidaPkgOutline pkg _ _ vsn _ _ _ _ ->
                        case Registry.getVersions Registry.KeepAllVersions pkg registry of
                            Just knownVersions ->
                                let
                                    bumpableVersions : List V.Version
                                    bumpableVersions =
                                        List.map (\( old, _, _ ) -> old) (Bump.getPossibilities knownVersions)
                                in
                                if List.member vsn bumpableVersions then
                                    suggestVersion env

                                else
                                    Task.fail <|
                                        Exit.BumpGuidaUnexpectedVersion registryDomain vsn <|
                                            List.map Prelude.head (Utils.listGroupBy (==) (List.sortWith V.compare bumpableVersions))

                            Nothing ->
                                Task.io <| checkNewPackage root outline

                    Outline.ElmPkgOutline pkg _ _ vsn _ _ _ _ ->
                        case Registry.getVersions Registry.KeepAllVersions pkg registry of
                            Just knownVersions ->
                                let
                                    bumpableVersions : List V.Version
                                    bumpableVersions =
                                        List.map (\( old, _, _ ) -> old) (Bump.getPossibilities knownVersions)
                                in
                                if List.member vsn bumpableVersions then
                                    suggestVersion env

                                else
                                    Task.fail <|
                                        Exit.BumpElmUnexpectedVersion registryDomain vsn <|
                                            List.map Prelude.head (Utils.listGroupBy (==) (List.sortWith V.compare bumpableVersions))

                            Nothing ->
                                Task.io <| checkNewPackage root outline
            )



-- CHECK NEW PACKAGE


checkNewPackage : Stuff.Root -> Outline.PkgOutline -> Task Never ()
checkNewPackage root outline =
    case outline of
        Outline.GuidaPkgOutline _ _ _ version _ _ _ _ ->
            IO.putStrLn Exit.newPackageOverview
                |> Task.andThen
                    (\_ ->
                        if version == V.one then
                            IO.putStrLn "The version number in guida.json is correct so you are all set!"

                        else
                            changeVersion root outline V.one <|
                                (D.fromChars "It looks like the version in guida.json has been changed though!\nWould you like me to change it back to "
                                    |> D.a (D.fromVersion V.one)
                                    |> D.a (D.fromChars "? [Y/n] ")
                                )
                    )

        Outline.ElmPkgOutline _ _ _ version _ _ _ _ ->
            IO.putStrLn Exit.newPackageOverview
                |> Task.andThen
                    (\_ ->
                        if version == V.one then
                            IO.putStrLn "The version number in elm.json is correct so you are all set!"

                        else
                            changeVersion root outline V.one <|
                                (D.fromChars "It looks like the version in elm.json has been changed though!\nWould you like me to change it back to "
                                    |> D.a (D.fromVersion V.one)
                                    |> D.a (D.fromChars "? [Y/n] ")
                                )
                    )



-- SUGGEST VERSION


suggestVersion : Env -> Task Exit.Bump ()
suggestVersion (Env root cache manager _ outline) =
    case outline of
        Outline.GuidaPkgOutline pkg _ _ vsn _ _ _ _ ->
            Task.eio (Exit.BumpCannotFindDocs vsn) (Diff.getDocs cache manager pkg vsn)
                |> Task.andThen
                    (\oldDocs ->
                        generateDocs root outline
                            |> Task.andThen
                                (\newDocs ->
                                    let
                                        changes : Diff.PackageChanges
                                        changes =
                                            Diff.diff oldDocs newDocs

                                        newVersion : V.Version
                                        newVersion =
                                            Diff.bump changes vsn

                                        old : D.Doc
                                        old =
                                            D.fromVersion vsn

                                        new : D.Doc
                                        new =
                                            D.fromVersion newVersion

                                        mag : D.Doc
                                        mag =
                                            D.fromChars <| M.toChars (Diff.toMagnitude changes)
                                    in
                                    Task.io <|
                                        changeVersion root outline newVersion <|
                                            (D.fromChars "Based on your new API, this should be a"
                                                |> D.plus (D.green mag)
                                                |> D.plus (D.fromChars "change (")
                                                |> D.a old
                                                |> D.a (D.fromChars " => ")
                                                |> D.a new
                                                |> D.a (D.fromChars ")\n")
                                                |> D.a (D.fromChars "Bail out of this command and run 'guida diff' for a full explanation.\n")
                                                |> D.a (D.fromChars "\n")
                                                |> D.a (D.fromChars "Should I perform the update (")
                                                |> D.a old
                                                |> D.a (D.fromChars " => ")
                                                |> D.a new
                                                |> D.a (D.fromChars ") in guida.json? [Y/n] ")
                                            )
                                )
                    )

        Outline.ElmPkgOutline pkg _ _ vsn _ _ _ _ ->
            Task.eio (Exit.BumpCannotFindDocs vsn) (Diff.getDocs cache manager pkg vsn)
                |> Task.andThen
                    (\oldDocs ->
                        generateDocs root outline
                            |> Task.andThen
                                (\newDocs ->
                                    let
                                        changes : Diff.PackageChanges
                                        changes =
                                            Diff.diff oldDocs newDocs

                                        newVersion : V.Version
                                        newVersion =
                                            Diff.bump changes vsn

                                        old : D.Doc
                                        old =
                                            D.fromVersion vsn

                                        new : D.Doc
                                        new =
                                            D.fromVersion newVersion

                                        mag : D.Doc
                                        mag =
                                            D.fromChars <| M.toChars (Diff.toMagnitude changes)
                                    in
                                    Task.io <|
                                        changeVersion root outline newVersion <|
                                            (D.fromChars "Based on your new API, this should be a"
                                                |> D.plus (D.green mag)
                                                |> D.plus (D.fromChars "change (")
                                                |> D.a old
                                                |> D.a (D.fromChars " => ")
                                                |> D.a new
                                                |> D.a (D.fromChars ")\n")
                                                |> D.a (D.fromChars "Bail out of this command and run 'guida diff' for a full explanation.\n")
                                                |> D.a (D.fromChars "\n")
                                                |> D.a (D.fromChars "Should I perform the update (")
                                                |> D.a old
                                                |> D.a (D.fromChars " => ")
                                                |> D.a new
                                                |> D.a (D.fromChars ") in elm.json? [Y/n] ")
                                            )
                                )
                    )


generateDocs : Stuff.Root -> Outline.PkgOutline -> Task Exit.Bump Docs.Documentation
generateDocs root outline =
    case outline of
        Outline.GuidaPkgOutline _ _ _ _ exposed _ _ _ ->
            Task.eio Exit.BumpBadDetails
                (BW.withScope (\scope -> Details.load Reporting.silent scope root))
                |> Task.andThen
                    (\details ->
                        case Outline.flattenExposed exposed of
                            [] ->
                                Task.fail <| Exit.BumpGuidaNoExposed

                            e :: es ->
                                Task.eio Exit.BumpBadBuild <|
                                    Build.fromExposed Reporting.silent root details Build.keepDocs (NE.Nonempty e es)
                    )

        Outline.ElmPkgOutline _ _ _ _ exposed _ _ _ ->
            Task.eio Exit.BumpBadDetails
                (BW.withScope (\scope -> Details.load Reporting.silent scope root))
                |> Task.andThen
                    (\details ->
                        case Outline.flattenExposed exposed of
                            [] ->
                                Task.fail <| Exit.BumpElmNoExposed

                            e :: es ->
                                Task.eio Exit.BumpBadBuild <|
                                    Build.fromExposed Reporting.silent root details Build.keepDocs (NE.Nonempty e es)
                    )



-- CHANGE VERSION


changeVersion : Stuff.Root -> Outline.PkgOutline -> V.Version -> D.Doc -> Task Never ()
changeVersion root outline targetVersion question =
    case outline of
        Outline.GuidaPkgOutline name summary license _ exposed deps testDeps guidaVersion ->
            Reporting.ask question
                |> Task.andThen
                    (\approved ->
                        if not approved then
                            IO.putStrLn "Okay, I did not change anything!"

                        else
                            Outline.write root
                                (Outline.Pkg
                                    (Outline.GuidaPkgOutline name summary license targetVersion exposed deps testDeps guidaVersion)
                                )
                                |> Task.andThen
                                    (\_ ->
                                        Help.toStdout
                                            (D.fromChars "Version changed to "
                                                |> D.a (D.green (D.fromVersion targetVersion))
                                                |> D.a (D.fromChars "!\n")
                                            )
                                    )
                    )

        Outline.ElmPkgOutline name summary license _ exposed deps testDeps elmVersion ->
            Reporting.ask question
                |> Task.andThen
                    (\approved ->
                        if not approved then
                            IO.putStrLn "Okay, I did not change anything!"

                        else
                            Outline.write root
                                (Outline.Pkg
                                    (Outline.ElmPkgOutline name summary license targetVersion exposed deps testDeps elmVersion)
                                )
                                |> Task.andThen
                                    (\_ ->
                                        Help.toStdout
                                            (D.fromChars "Version changed to "
                                                |> D.a (D.green (D.fromVersion targetVersion))
                                                |> D.a (D.fromChars "!\n")
                                            )
                                    )
                    )
