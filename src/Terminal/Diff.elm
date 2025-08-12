module Terminal.Diff exposing
    ( Args(..)
    , run
    )

import Basics.Extra exposing (flip)
import Builder.BackgroundWriter as BW
import Builder.Build as Build
import Builder.Deps.Diff as DD exposing (Changes(..), ModuleChanges(..), PackageChanges(..))
import Builder.Deps.Registry as Registry
import Builder.Elm.Details as Details exposing (Details(..))
import Builder.Elm.Outline as Outline
import Builder.Http as Http
import Builder.Reporting as Reporting
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help as Help
import Builder.Stuff as Stuff
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.Name as Name
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Compiler.Type as Type
import Compiler.Elm.Docs as Docs
import Compiler.Elm.Magnitude as M
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Type as Type
import Compiler.Reporting.Render.Type.Localizer as L
import Data.Map as Dict
import Task exposing (Task)
import Utils.Task.Extra as Task



-- RUN


type Args
    = CodeVsLatest
    | CodeVsExactly V.Version
    | LocalInquiry V.Version V.Version
    | GlobalInquiry Pkg.Name V.Version V.Version


run : Args -> () -> Task Never ()
run args () =
    Reporting.attempt Exit.diffToReport
        (Task.run
            (getEnv
                |> Task.bind (\env -> diff env args)
            )
        )



-- ENVIRONMENT


type Env
    = Env (Maybe String) Stuff.PackageCache Http.Manager Registry.Registry


getEnv : Task Exit.Diff Env
getEnv =
    Task.io Stuff.findRoot
        |> Task.bind
            (\maybeRoot ->
                Task.io Stuff.getPackageCache
                    |> Task.bind
                        (\cache ->
                            Task.io Http.getManager
                                |> Task.bind
                                    (\manager ->
                                        Task.eio Exit.DiffMustHaveLatestRegistry (Registry.latest manager cache)
                                            |> Task.fmap (\registry -> Env maybeRoot cache manager registry)
                                    )
                        )
            )



-- DIFF


diff : Env -> Args -> Task Exit.Diff ()
diff ((Env _ _ _ registry) as env) args =
    case args of
        GlobalInquiry name v1 v2 ->
            case Registry.getVersions_ name registry of
                Ok vsns ->
                    getDocs env name vsns (V.min v1 v2)
                        |> Task.bind
                            (\oldDocs ->
                                getDocs env name vsns (V.max v1 v2)
                                    |> Task.bind (\newDocs -> writeDiff oldDocs newDocs)
                            )

                Err suggestions ->
                    Task.throw <| Exit.DiffUnknownPackage name suggestions

        LocalInquiry v1 v2 ->
            readOutline env
                |> Task.bind
                    (\( name, vsns ) ->
                        getDocs env name vsns (V.min v1 v2)
                            |> Task.bind
                                (\oldDocs ->
                                    getDocs env name vsns (V.max v1 v2)
                                        |> Task.bind (\newDocs -> writeDiff oldDocs newDocs)
                                )
                    )

        CodeVsLatest ->
            readOutline env
                |> Task.bind
                    (\( name, vsns ) ->
                        getLatestDocs env name vsns
                            |> Task.bind
                                (\oldDocs ->
                                    generateDocs env
                                        |> Task.bind (\newDocs -> writeDiff oldDocs newDocs)
                                )
                    )

        CodeVsExactly version ->
            readOutline env
                |> Task.bind
                    (\( name, vsns ) ->
                        getDocs env name vsns version
                            |> Task.bind
                                (\oldDocs ->
                                    generateDocs env
                                        |> Task.bind (\newDocs -> writeDiff oldDocs newDocs)
                                )
                    )



-- GET DOCS


getDocs : Env -> Pkg.Name -> Registry.KnownVersions -> V.Version -> Task Exit.Diff Docs.Documentation
getDocs (Env _ cache manager _) name (Registry.KnownVersions latest previous) version =
    if latest == version || List.member version previous then
        Task.eio (Exit.DiffDocsProblem version) <| DD.getDocs cache manager name version

    else
        Task.throw <| Exit.DiffUnknownVersion version (latest :: previous)


getLatestDocs : Env -> Pkg.Name -> Registry.KnownVersions -> Task Exit.Diff Docs.Documentation
getLatestDocs (Env _ cache manager _) name (Registry.KnownVersions latest _) =
    Task.eio (Exit.DiffDocsProblem latest) <| DD.getDocs cache manager name latest



-- READ OUTLINE


readOutline : Env -> Task Exit.Diff ( Pkg.Name, Registry.KnownVersions )
readOutline (Env maybeRoot _ _ registry) =
    case maybeRoot of
        Nothing ->
            Task.throw <| Exit.DiffNoOutline

        Just root ->
            Task.io (Outline.read root)
                |> Task.bind
                    (\result ->
                        case result of
                            Err err ->
                                Task.throw <| Exit.DiffBadOutline err

                            Ok outline ->
                                case outline of
                                    Outline.App _ ->
                                        Task.throw <| Exit.DiffApplication

                                    Outline.Pkg (Outline.PkgOutline pkg _ _ _ _ _ _ _) ->
                                        case Registry.getVersions pkg registry of
                                            Just vsns ->
                                                Task.pure ( pkg, vsns )

                                            Nothing ->
                                                Task.throw Exit.DiffUnpublished
                    )



-- GENERATE DOCS


generateDocs : Env -> Task Exit.Diff Docs.Documentation
generateDocs (Env maybeRoot _ _ _) =
    case maybeRoot of
        Nothing ->
            Task.throw <| Exit.DiffNoOutline

        Just root ->
            Task.eio Exit.DiffBadDetails
                (BW.withScope (\scope -> Details.load Reporting.silent scope root))
                |> Task.bind
                    (\((Details _ outline _ _ _ _) as details) ->
                        case outline of
                            Details.ValidApp _ ->
                                Task.throw Exit.DiffApplication

                            Details.ValidPkg _ exposed _ ->
                                case exposed of
                                    [] ->
                                        Task.throw Exit.DiffNoExposed

                                    e :: es ->
                                        Task.eio Exit.DiffBadBuild <|
                                            Build.fromExposed Docs.bytesDecoder Docs.bytesEncoder Reporting.silent root details Build.keepDocs (NE.Nonempty e es)
                    )



-- WRITE DIFF


writeDiff : Docs.Documentation -> Docs.Documentation -> Task Exit.Diff ()
writeDiff oldDocs newDocs =
    let
        changes : PackageChanges
        changes =
            DD.diff oldDocs newDocs

        localizer : L.Localizer
        localizer =
            L.fromNames (Dict.union oldDocs newDocs)
    in
    Task.io (Help.toStdout (toDoc localizer changes |> D.a (D.fromChars "\n")))



-- TO DOC


toDoc : L.Localizer -> PackageChanges -> D.Doc
toDoc localizer ((PackageChanges added changed removed) as changes) =
    if List.isEmpty added && Dict.isEmpty changed && List.isEmpty removed then
        D.fromChars "No API changes detected, so this is a"
            |> D.plus (D.green (D.fromChars "PATCH"))
            |> D.plus (D.fromChars "change.")

    else
        let
            magDoc : D.Doc
            magDoc =
                D.fromChars (M.toChars (DD.toMagnitude changes))

            header : D.Doc
            header =
                D.fromChars "This is a"
                    |> D.plus (D.green magDoc)
                    |> D.plus (D.fromChars "change.")

            addedChunk : List Chunk
            addedChunk =
                if List.isEmpty added then
                    []

                else
                    [ Chunk "ADDED MODULES" M.MINOR <|
                        D.vcat <|
                            List.map D.fromName added
                    ]

            removedChunk : List Chunk
            removedChunk =
                if List.isEmpty removed then
                    []

                else
                    [ Chunk "REMOVED MODULES" M.MAJOR <|
                        D.vcat <|
                            List.map D.fromName removed
                    ]

            chunks : List Chunk
            chunks =
                addedChunk ++ removedChunk ++ List.map (changesToChunk localizer) (Dict.toList compare changed)
        in
        D.vcat (header :: D.fromChars "" :: List.map chunkToDoc chunks)


type Chunk
    = Chunk String M.Magnitude D.Doc


chunkToDoc : Chunk -> D.Doc
chunkToDoc (Chunk title magnitude details) =
    let
        header : D.Doc
        header =
            D.fromChars "----"
                |> D.plus (D.fromChars title)
                |> D.plus (D.fromChars "-")
                |> D.plus (D.fromChars (M.toChars magnitude))
                |> D.plus (D.fromChars "----")
    in
    D.vcat
        [ D.dullcyan header
        , D.fromChars ""
        , D.indent 4 details
        , D.fromChars ""
        , D.fromChars ""
        ]


changesToChunk : L.Localizer -> ( Name.Name, ModuleChanges ) -> Chunk
changesToChunk localizer ( name, (ModuleChanges unions aliases values binops) as changes ) =
    let
        magnitude : M.Magnitude
        magnitude =
            DD.moduleChangeMagnitude changes

        ( unionAdd, unionChange, unionRemove ) =
            changesToDocTriple compare (unionToDoc localizer) unions

        ( aliasAdd, aliasChange, aliasRemove ) =
            changesToDocTriple compare (aliasToDoc localizer) aliases

        ( valueAdd, valueChange, valueRemove ) =
            changesToDocTriple compare (valueToDoc localizer) values

        ( binopAdd, binopChange, binopRemove ) =
            changesToDocTriple compare (binopToDoc localizer) binops
    in
    Chunk name magnitude <|
        D.vcat <|
            List.intersperse (D.fromChars "") <|
                List.filterMap identity <|
                    [ changesToDoc "Added" unionAdd aliasAdd valueAdd binopAdd
                    , changesToDoc "Removed" unionRemove aliasRemove valueRemove binopRemove
                    , changesToDoc "Changed" unionChange aliasChange valueChange binopChange
                    ]


changesToDocTriple : (k -> k -> Order) -> (k -> v -> D.Doc) -> Changes comparable k v -> ( List D.Doc, List D.Doc, List D.Doc )
changesToDocTriple keyComparison entryToDoc (Changes added changed removed) =
    let
        indented : ( k, v ) -> D.Doc
        indented ( name, value ) =
            D.indent 4 (entryToDoc name value)

        diffed : ( k, ( v, v ) ) -> D.Doc
        diffed ( name, ( oldValue, newValue ) ) =
            D.vcat
                [ D.fromChars "  - " |> D.a (entryToDoc name oldValue)
                , D.fromChars "  + " |> D.a (entryToDoc name newValue)
                , D.fromChars ""
                ]
    in
    ( List.map indented (Dict.toList keyComparison added)
    , List.map diffed (Dict.toList keyComparison changed)
    , List.map indented (Dict.toList keyComparison removed)
    )


changesToDoc : String -> List D.Doc -> List D.Doc -> List D.Doc -> List D.Doc -> Maybe D.Doc
changesToDoc categoryName unions aliases values binops =
    if List.isEmpty unions && List.isEmpty aliases && List.isEmpty values && List.isEmpty binops then
        Nothing

    else
        Just <|
            D.vcat <|
                D.append (D.fromChars categoryName) (D.fromChars ":")
                    :: unions
                    ++ aliases
                    ++ binops
                    ++ values


unionToDoc : L.Localizer -> Name.Name -> Docs.Union -> D.Doc
unionToDoc localizer name (Docs.Union _ tvars ctors) =
    let
        setup : D.Doc
        setup =
            D.fromChars "type"
                |> D.plus (D.fromName name)
                |> D.plus (D.hsep (List.map D.fromName tvars))

        ctorDoc : ( Name.Name, List Type.Type ) -> D.Doc
        ctorDoc ( ctor, tipes ) =
            typeDoc localizer (Type.Type ctor tipes)
    in
    D.hang 4
        (D.sep
            (setup
                :: List.map2 (flip D.plus)
                    (D.fromChars "=" :: List.repeat (List.length ctors - 1) (D.fromChars "|"))
                    (List.map ctorDoc ctors)
            )
        )


aliasToDoc : L.Localizer -> Name.Name -> Docs.Alias -> D.Doc
aliasToDoc localizer name (Docs.Alias _ tvars tipe) =
    let
        declaration : D.Doc
        declaration =
            D.plus (D.fromChars "type")
                (D.plus (D.fromChars "alias")
                    (D.plus (D.hsep (List.map D.fromName (name :: tvars)))
                        (D.fromChars "=")
                    )
                )
    in
    D.hang 4 (D.sep [ declaration, typeDoc localizer tipe ])


valueToDoc : L.Localizer -> Name.Name -> Docs.Value -> D.Doc
valueToDoc localizer name (Docs.Value _ tipe) =
    D.hang 4 <| D.sep [ D.fromName name |> D.plus (D.fromChars ":"), typeDoc localizer tipe ]


binopToDoc : L.Localizer -> Name.Name -> Docs.Binop -> D.Doc
binopToDoc localizer name (Docs.Binop _ tipe associativity n) =
    let
        details : D.Doc
        details =
            D.plus (D.fromChars "    (")
                (D.plus (D.fromName assoc)
                    (D.plus (D.fromChars "/")
                        (D.plus (D.fromInt n)
                            (D.fromChars ")")
                        )
                    )
                )

        assoc : String
        assoc =
            case associativity of
                Binop.Left ->
                    "left"

                Binop.Non ->
                    "non"

                Binop.Right ->
                    "right"
    in
    D.plus (D.fromChars "(")
        (D.plus (D.fromName name)
            (D.plus (D.fromChars ")")
                (D.plus (D.fromChars ":")
                    (D.plus (typeDoc localizer tipe)
                        (D.black details)
                    )
                )
            )
        )


typeDoc : L.Localizer -> Type.Type -> D.Doc
typeDoc localizer tipe =
    Type.toDoc localizer Type.None tipe
