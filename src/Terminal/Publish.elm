module Terminal.Publish exposing (run)

import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Deps.Bump as Bump
import Builder.Deps.Diff as Diff
import Builder.Deps.Registry as Registry
import Builder.Deps.Website as Website
import Builder.Elm.Details as Details
import Builder.Elm.Outline as Outline
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help as Help
import Builder.Stuff as Stuff
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Docs as Docs
import Compiler.Elm.Magnitude as M
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Json.String as Json
import Compiler.Reporting.Doc as D
import List.Extra as List
import System.Exit as Exit
import System.IO as IO
import System.Process as Process
import Task exposing (Task)
import Utils.Main as Utils exposing (FilePath)
import Utils.Task.Extra as Task



-- RUN


{-| TODO mandate no "exposing (..)" in packages to make
optimization to skip builds in Elm.Details always valid
-}
run : () -> () -> Task Never ()
run () () =
    Reporting.attempt Exit.publishToReport <|
        Task.run (Task.bind publish getEnv)



-- ENV


type Env
    = Env FilePath Stuff.PackageCache Http.Manager Registry.Registry Outline.Outline


getEnv : Task Exit.Publish Env
getEnv =
    Task.mio Exit.PublishNoOutline Stuff.findRoot
        |> Task.bind
            (\root ->
                Task.io Stuff.getPackageCache
                    |> Task.bind
                        (\cache ->
                            Task.io Http.getManager
                                |> Task.bind
                                    (\manager ->
                                        Task.eio Exit.PublishMustHaveLatestRegistry (Registry.latest manager cache)
                                            |> Task.bind
                                                (\registry ->
                                                    Task.eio Exit.PublishBadOutline (Outline.read root)
                                                        |> Task.fmap
                                                            (\outline ->
                                                                Env root cache manager registry outline
                                                            )
                                                )
                                    )
                        )
            )



-- PUBLISH


publish : Env -> Task Exit.Publish ()
publish ((Env root _ manager registry outline) as env) =
    case outline of
        Outline.App _ ->
            Task.throw Exit.PublishApplication

        Outline.Pkg (Outline.PkgOutline pkg summary _ vsn exposed _ _ _) ->
            let
                maybeKnownVersions : Maybe Registry.KnownVersions
                maybeKnownVersions =
                    Registry.getVersions pkg registry
            in
            reportPublishStart pkg vsn maybeKnownVersions
                |> Task.bind
                    (\_ ->
                        if noExposed exposed then
                            Task.throw Exit.PublishNoExposed

                        else
                            Task.pure ()
                    )
                |> Task.bind
                    (\_ ->
                        if badSummary summary then
                            Task.throw Exit.PublishNoSummary

                        else
                            Task.pure ()
                    )
                |> Task.bind (\_ -> verifyReadme root)
                |> Task.bind (\_ -> verifyLicense root)
                |> Task.bind (\_ -> verifyBuild root)
                |> Task.bind
                    (\docs ->
                        verifyVersion env pkg vsn docs maybeKnownVersions
                            |> Task.bind (\_ -> getGit)
                            |> Task.bind
                                (\git ->
                                    verifyTag git manager pkg vsn
                                        |> Task.bind
                                            (\commitHash ->
                                                verifyNoChanges git commitHash vsn
                                                    |> Task.bind
                                                        (\_ ->
                                                            verifyZip env pkg vsn
                                                                |> Task.bind
                                                                    (\zipHash ->
                                                                        Task.io (IO.putStrLn "")
                                                                            |> Task.bind (\_ -> register manager pkg vsn docs commitHash zipHash)
                                                                            |> Task.bind (\_ -> Task.io (IO.putStrLn "Success!"))
                                                                    )
                                                        )
                                            )
                                )
                    )



-- VERIFY SUMMARY


badSummary : String -> Bool
badSummary summary =
    Json.isEmpty summary || Outline.defaultSummary == summary


noExposed : Outline.Exposed -> Bool
noExposed exposed =
    case exposed of
        Outline.ExposedList modules ->
            List.isEmpty modules

        Outline.ExposedDict chunks ->
            List.all (List.isEmpty << Tuple.second) chunks



-- VERIFY README


verifyReadme : String -> Task Exit.Publish ()
verifyReadme root =
    let
        readmePath : String
        readmePath =
            root ++ "/README.md"
    in
    reportReadmeCheck <|
        (File.exists readmePath
            |> Task.bind
                (\exists ->
                    if exists then
                        IO.withFile readmePath IO.ReadMode IO.hFileSize
                            |> Task.fmap
                                (\size ->
                                    if size < 300 then
                                        Err Exit.PublishShortReadme

                                    else
                                        Ok ()
                                )

                    else
                        Task.pure (Err Exit.PublishNoReadme)
                )
        )



-- VERIFY LICENSE


verifyLicense : String -> Task Exit.Publish ()
verifyLicense root =
    let
        licensePath : String
        licensePath =
            root ++ "/LICENSE"
    in
    reportLicenseCheck <|
        (File.exists licensePath
            |> Task.fmap
                (\exists ->
                    if exists then
                        Ok ()

                    else
                        Err Exit.PublishNoLicense
                )
        )



-- VERIFY BUILD


verifyBuild : String -> Task Exit.Publish Docs.Documentation
verifyBuild root =
    reportBuildCheck <|
        BW.withScope <|
            \scope ->
                Task.run
                    (Task.eio Exit.PublishBadDetails
                        (Details.load Reporting.silent scope root)
                        |> Task.bind
                            (\((Details.Details _ outline _ _ _ _) as details) ->
                                (case outline of
                                    Details.ValidApp _ ->
                                        Task.throw Exit.PublishApplication

                                    Details.ValidPkg _ [] _ ->
                                        Task.throw Exit.PublishNoExposed

                                    Details.ValidPkg _ (e :: es) _ ->
                                        Task.pure (NE.Nonempty e es)
                                )
                                    |> Task.bind
                                        (\exposed ->
                                            Task.eio Exit.PublishBuildProblem <|
                                                Build.fromExposed Docs.bytesDecoder Docs.bytesEncoder Reporting.silent root details Build.keepDocs exposed
                                        )
                            )
                    )



-- GET GIT


type Git
    = Git (List String -> Task Never Exit.ExitCode)


getGit : Task Exit.Publish Git
getGit =
    Task.io (Utils.dirFindExecutable "git")
        |> Task.bind
            (\maybeGit ->
                case maybeGit of
                    Nothing ->
                        Task.throw Exit.PublishNoGit

                    Just git ->
                        Task.pure <|
                            Git
                                (\args ->
                                    let
                                        process : { cmdspec : Process.CmdSpec, std_in : Process.StdStream, std_out : Process.StdStream, std_err : Process.StdStream }
                                        process =
                                            Process.proc git args
                                                |> (\cp ->
                                                        { cp
                                                            | std_in = Process.CreatePipe
                                                            , std_out = Process.CreatePipe
                                                            , std_err = Process.CreatePipe
                                                        }
                                                   )
                                    in
                                    Process.withCreateProcess process
                                        (\_ _ _ handle ->
                                            Process.waitForProcess handle
                                        )
                                )
            )



-- VERIFY GITHUB TAG


verifyTag : Git -> Http.Manager -> Pkg.Name -> V.Version -> Task Exit.Publish String
verifyTag (Git run_) manager pkg vsn =
    reportTagCheck vsn
        -- https://stackoverflow.com/questions/1064499/how-to-list-all-git-tags
        (run_ [ "show", "--name-only", V.toChars vsn, "--" ]
            |> Task.bind
                (\exitCode ->
                    case exitCode of
                        Exit.ExitFailure _ ->
                            Task.pure (Err (Exit.PublishMissingTag vsn))

                        Exit.ExitSuccess ->
                            let
                                url : String
                                url =
                                    toTagUrl pkg vsn
                            in
                            Http.get manager url [ Http.accept "application/json" ] (Exit.PublishCannotGetTag vsn) <|
                                \body ->
                                    case D.fromByteString commitHashDecoder body of
                                        Ok hash ->
                                            Task.pure <| Ok hash

                                        Err _ ->
                                            Task.pure <| Err (Exit.PublishCannotGetTagData vsn url body)
                )
        )


toTagUrl : Pkg.Name -> V.Version -> String
toTagUrl pkg vsn =
    "https://api.github.com/repos/" ++ Pkg.toUrl pkg ++ "/git/refs/tags/" ++ V.toChars vsn


commitHashDecoder : D.Decoder e String
commitHashDecoder =
    D.field "object" (D.field "sha" D.string)



-- VERIFY NO LOCAL CHANGES SINCE TAG


verifyNoChanges : Git -> String -> V.Version -> Task Exit.Publish ()
verifyNoChanges (Git run_) commitHash vsn =
    reportLocalChangesCheck <|
        -- https://stackoverflow.com/questions/3878624/how-do-i-programmatically-determine-if-there-are-uncommited-changes
        (run_ [ "diff-index", "--quiet", commitHash, "--" ]
            |> Task.fmap
                (\exitCode ->
                    case exitCode of
                        Exit.ExitSuccess ->
                            Ok ()

                        Exit.ExitFailure _ ->
                            Err (Exit.PublishLocalChanges vsn)
                )
        )



-- VERIFY THAT ZIP BUILDS / COMPUTE HASH


verifyZip : Env -> Pkg.Name -> V.Version -> Task Exit.Publish Http.Sha
verifyZip (Env root _ manager _ _) pkg vsn =
    withPrepublishDir root <|
        \prepublishDir ->
            let
                url : String
                url =
                    toZipUrl pkg vsn
            in
            reportDownloadCheck
                (Http.getArchive manager
                    url
                    Exit.PublishCannotGetZip
                    (Exit.PublishCannotDecodeZip url)
                    (Task.pure << Ok)
                )
                |> Task.bind
                    (\( sha, archive ) ->
                        Task.io (File.writePackage prepublishDir archive)
                            |> Task.bind
                                (\_ ->
                                    reportZipBuildCheck <|
                                        Utils.dirWithCurrentDirectory prepublishDir <|
                                            verifyZipBuild prepublishDir
                                )
                            |> Task.fmap (\_ -> sha)
                    )


toZipUrl : Pkg.Name -> V.Version -> String
toZipUrl pkg vsn =
    "https://github.com/" ++ Pkg.toUrl pkg ++ "/zipball/" ++ V.toChars vsn ++ "/"


withPrepublishDir : String -> (String -> Task x a) -> Task x a
withPrepublishDir root callback =
    let
        dir : String
        dir =
            Stuff.prepublishDir root
    in
    Task.eio identity <|
        Utils.bracket_
            (Utils.dirCreateDirectoryIfMissing True dir)
            (Utils.dirRemoveDirectoryRecursive dir)
            (Task.run (callback dir))


verifyZipBuild : FilePath -> Task Never (Result Exit.Publish ())
verifyZipBuild root =
    BW.withScope
        (\scope ->
            Task.run
                (Task.eio Exit.PublishZipBadDetails
                    (Details.load Reporting.silent scope root)
                    |> Task.bind
                        (\((Details.Details _ outline _ _ _ _) as details) ->
                            (case outline of
                                Details.ValidApp _ ->
                                    Task.throw Exit.PublishZipApplication

                                Details.ValidPkg _ [] _ ->
                                    Task.throw Exit.PublishZipNoExposed

                                Details.ValidPkg _ (e :: es) _ ->
                                    Task.pure (NE.Nonempty e es)
                            )
                                |> Task.bind
                                    (\exposed ->
                                        Task.eio Exit.PublishZipBuildProblem
                                            (Build.fromExposed Docs.bytesDecoder Docs.bytesEncoder Reporting.silent root details Build.keepDocs exposed)
                                            |> Task.fmap (\_ -> ())
                                    )
                        )
                )
        )



-- VERIFY VERSION


type GoodVersion
    = GoodStart
    | GoodBump V.Version M.Magnitude


verifyVersion : Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Maybe Registry.KnownVersions -> Task Exit.Publish ()
verifyVersion env pkg vsn newDocs publishedVersions =
    reportSemverCheck vsn <|
        case publishedVersions of
            Nothing ->
                if vsn == V.one then
                    Task.pure <| Ok GoodStart

                else
                    Task.pure <| Err <| Exit.PublishNotInitialVersion vsn

            Just ((Registry.KnownVersions latest previous) as knownVersions) ->
                if vsn == latest || List.member vsn previous then
                    Task.pure <| Err <| Exit.PublishAlreadyPublished vsn

                else
                    verifyBump env pkg vsn newDocs knownVersions


verifyBump : Env -> Pkg.Name -> V.Version -> Docs.Documentation -> Registry.KnownVersions -> Task Never (Result Exit.Publish GoodVersion)
verifyBump (Env _ cache manager _ _) pkg vsn newDocs ((Registry.KnownVersions latest _) as knownVersions) =
    case List.find (\( _, new, _ ) -> vsn == new) (Bump.getPossibilities knownVersions) of
        Nothing ->
            Task.pure <|
                Err <|
                    Exit.PublishInvalidBump vsn latest

        Just ( old, new, magnitude ) ->
            Diff.getDocs cache manager pkg old
                |> Task.fmap
                    (\result ->
                        case result of
                            Err dp ->
                                Err <| Exit.PublishCannotGetDocs old new dp

                            Ok oldDocs ->
                                let
                                    changes : Diff.PackageChanges
                                    changes =
                                        Diff.diff oldDocs newDocs

                                    realNew : V.Version
                                    realNew =
                                        Diff.bump changes old
                                in
                                if new == realNew then
                                    Ok <| GoodBump old magnitude

                                else
                                    Err <|
                                        Exit.PublishBadBump old new magnitude realNew (Diff.toMagnitude changes)
                    )



-- REGISTER PACKAGES


register : Http.Manager -> Pkg.Name -> V.Version -> Docs.Documentation -> String -> Http.Sha -> Task Exit.Publish ()
register manager pkg vsn docs commitHash sha =
    Website.route "/register"
        [ ( "name", Pkg.toChars pkg )
        , ( "version", V.toChars vsn )
        , ( "commit-hash", commitHash )
        ]
        |> Task.bind
            (\url ->
                Http.upload manager
                    url
                    [ Http.filePart "elm.json" "elm.json"
                    , Http.jsonPart "docs.json" "docs.json" (Docs.jsonEncoder docs)
                    , Http.filePart "README.md" "README.md"
                    , Http.stringPart "github-hash" (Http.shaToChars sha)
                    ]
            )
        |> Task.eio Exit.PublishCannotRegister



-- REPORTING


reportPublishStart : Pkg.Name -> V.Version -> Maybe Registry.KnownVersions -> Task x ()
reportPublishStart pkg vsn maybeKnownVersions =
    Task.io <|
        case maybeKnownVersions of
            Nothing ->
                IO.putStrLn <| Exit.newPackageOverview ++ "\nI will now verify that everything is in order...\n"

            Just _ ->
                IO.putStrLn <| "Verifying " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn ++ " ...\n"



-- REPORTING PHASES


reportReadmeCheck : Task Never (Result x a) -> Task x a
reportReadmeCheck =
    reportCheck
        "Looking for README.md"
        "Found README.md"
        "Problem with your README.md"


reportLicenseCheck : Task Never (Result x a) -> Task x a
reportLicenseCheck =
    reportCheck
        "Looking for LICENSE"
        "Found LICENSE"
        "Problem with your LICENSE"


reportBuildCheck : Task Never (Result x a) -> Task x a
reportBuildCheck =
    reportCheck
        "Verifying documentation..."
        "Verified documentation"
        "Problem with documentation"


reportSemverCheck : V.Version -> Task Never (Result x GoodVersion) -> Task x ()
reportSemverCheck version work =
    let
        vsn : String
        vsn =
            V.toChars version

        waiting : String
        waiting =
            "Checking semantic versioning rules. Is " ++ vsn ++ " correct?"

        failure : String
        failure =
            "Version " ++ vsn ++ " is not correct!"

        success : GoodVersion -> String
        success result =
            case result of
                GoodStart ->
                    "All packages start at version " ++ V.toChars V.one

                GoodBump oldVersion magnitude ->
                    "Version number "
                        ++ vsn
                        ++ " verified ("
                        ++ M.toChars magnitude
                        ++ " change, "
                        ++ V.toChars oldVersion
                        ++ " => "
                        ++ vsn
                        ++ ")"
    in
    Task.void <| reportCustomCheck waiting success failure work


reportTagCheck : V.Version -> Task Never (Result x a) -> Task x a
reportTagCheck vsn =
    reportCheck
        ("Is version " ++ V.toChars vsn ++ " tagged on GitHub?")
        ("Version " ++ V.toChars vsn ++ " is tagged on GitHub")
        ("Version " ++ V.toChars vsn ++ " is not tagged on GitHub!")


reportDownloadCheck : Task Never (Result x a) -> Task x a
reportDownloadCheck =
    reportCheck
        "Downloading code from GitHub..."
        "Code downloaded successfully from GitHub"
        "Could not download code from GitHub!"


reportLocalChangesCheck : Task Never (Result x a) -> Task x a
reportLocalChangesCheck =
    reportCheck
        "Checking for uncommitted changes..."
        "No uncommitted changes in local code"
        "Your local code is different than the code tagged on GitHub"


reportZipBuildCheck : Task Never (Result x a) -> Task x a
reportZipBuildCheck =
    reportCheck
        "Verifying downloaded code..."
        "Downloaded code compiles successfully"
        "Cannot compile downloaded code!"


reportCheck : String -> String -> String -> Task Never (Result x a) -> Task x a
reportCheck waiting success failure work =
    reportCustomCheck waiting (\_ -> success) failure work


reportCustomCheck : String -> (a -> String) -> String -> Task Never (Result x a) -> Task x a
reportCustomCheck waiting success failure work =
    let
        putFlush : D.Doc -> Task Never ()
        putFlush doc =
            Help.toStdout doc |> Task.bind (\_ -> IO.hFlush IO.stdout)

        padded : String -> String
        padded message =
            message ++ String.repeat (String.length waiting - String.length message) " "
    in
    Task.eio identity
        (putFlush (D.append (D.fromChars "  ") waitingMark |> D.plus (D.fromChars waiting))
            |> Task.bind
                (\_ ->
                    work
                        |> Task.bind
                            (\result ->
                                putFlush
                                    (case result of
                                        Ok a ->
                                            D.append (D.fromChars "\u{000D}  ") goodMark |> D.plus (D.fromChars (padded (success a) ++ "\n"))

                                        Err _ ->
                                            D.append (D.fromChars "\u{000D}  ") badMark |> D.plus (D.fromChars (padded failure ++ "\n\n"))
                                    )
                                    |> Task.fmap (\_ -> result)
                            )
                )
        )



-- MARKS


goodMark : D.Doc
goodMark =
    D.green <|
        if isWindows then
            D.fromChars "+"

        else
            D.fromChars "●"


badMark : D.Doc
badMark =
    D.red <|
        if isWindows then
            D.fromChars "X"

        else
            D.fromChars "✗"


waitingMark : D.Doc
waitingMark =
    D.dullyellow <|
        if isWindows then
            D.fromChars "-"

        else
            D.fromChars "→"


isWindows : Bool
isWindows =
    -- Info.os == "mingw32"
    False
