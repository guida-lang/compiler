module Builder.Deps.Registry exposing
    ( KnownVersions(..)
    , Registry(..)
    , fetch
    , getVersions
    , getVersions_
    , latest
    , read
    , registryDecoder
    , registryEncoder
    , update
    )

import Basics.Extra exposing (flip)
import Builder.Deps.Website as Website
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Parse.Primitives as P
import Data.Map as Dict exposing (Dict)
import Task exposing (Task)
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE
import Utils.Task.Extra as Task



-- REGISTRY


type Registry
    = Registry Int (Dict ( String, String ) Pkg.Name KnownVersions)


type KnownVersions
    = KnownVersions V.Version (List V.Version)



-- READ


read : Stuff.PackageCache -> Task Never (Maybe Registry)
read cache =
    File.readBinary registryDecoder (Stuff.registry cache)



-- FETCH


fetch : Http.Manager -> Stuff.PackageCache -> Task Never (Result Exit.RegistryProblem Registry)
fetch manager cache =
    post manager "/all-packages" allPkgsDecoder <|
        \versions ->
            let
                size : Int
                size =
                    Dict.foldr Pkg.compareName (\_ -> addEntry) 0 versions

                registry : Registry
                registry =
                    Registry size versions

                path : String
                path =
                    Stuff.registry cache
            in
            File.writeBinary registryEncoder path registry
                |> Task.fmap (\_ -> registry)


addEntry : KnownVersions -> Int -> Int
addEntry (KnownVersions _ vs) count =
    count + 1 + List.length vs


allPkgsDecoder : D.Decoder () (Dict ( String, String ) Pkg.Name KnownVersions)
allPkgsDecoder =
    let
        keyDecoder : D.KeyDecoder () Pkg.Name
        keyDecoder =
            Pkg.keyDecoder bail

        versionsDecoder : D.Decoder () (List V.Version)
        versionsDecoder =
            D.list (D.mapError (\_ -> ()) V.decoder)

        toKnownVersions : List V.Version -> D.Decoder () KnownVersions
        toKnownVersions versions =
            case List.sortWith (flip V.compare) versions of
                v :: vs ->
                    D.pure (KnownVersions v vs)

                [] ->
                    D.failure ()
    in
    D.dict identity keyDecoder (D.bind toKnownVersions versionsDecoder)



-- UPDATE


update : Http.Manager -> Stuff.PackageCache -> Registry -> Task Never (Result Exit.RegistryProblem Registry)
update manager cache ((Registry size packages) as oldRegistry) =
    post manager ("/all-packages/since/" ++ String.fromInt size) (D.list newPkgDecoder) <|
        \news ->
            case news of
                [] ->
                    Task.pure oldRegistry

                _ :: _ ->
                    let
                        newSize : Int
                        newSize =
                            size + List.length news

                        newPkgs : Dict ( String, String ) Pkg.Name KnownVersions
                        newPkgs =
                            List.foldr addNew packages news

                        newRegistry : Registry
                        newRegistry =
                            Registry newSize newPkgs
                    in
                    File.writeBinary registryEncoder (Stuff.registry cache) newRegistry
                        |> Task.fmap (\_ -> newRegistry)


addNew : ( Pkg.Name, V.Version ) -> Dict ( String, String ) Pkg.Name KnownVersions -> Dict ( String, String ) Pkg.Name KnownVersions
addNew ( name, version ) versions =
    let
        add : Maybe KnownVersions -> KnownVersions
        add maybeKnowns =
            case maybeKnowns of
                Just (KnownVersions v vs) ->
                    KnownVersions version (v :: vs)

                Nothing ->
                    KnownVersions version []
    in
    Dict.update identity name (Just << add) versions



-- NEW PACKAGE DECODER


newPkgDecoder : D.Decoder () ( Pkg.Name, V.Version )
newPkgDecoder =
    D.customString newPkgParser bail


newPkgParser : P.Parser () ( Pkg.Name, V.Version )
newPkgParser =
    P.specialize (\_ _ _ -> ()) Pkg.parser
        |> P.bind
            (\pkg ->
                P.word1 '@' bail
                    |> P.bind (\_ -> P.specialize (\_ _ _ -> ()) V.parser)
                    |> P.fmap (\vsn -> ( pkg, vsn ))
            )


bail : a -> b -> ()
bail _ _ =
    ()



-- LATEST


latest : Http.Manager -> Stuff.PackageCache -> Task Never (Result Exit.RegistryProblem Registry)
latest manager cache =
    read cache
        |> Task.bind
            (\maybeOldRegistry ->
                case maybeOldRegistry of
                    Just oldRegistry ->
                        update manager cache oldRegistry

                    Nothing ->
                        fetch manager cache
            )



-- GET VERSIONS


getVersions : Pkg.Name -> Registry -> Maybe KnownVersions
getVersions name (Registry _ versions) =
    Dict.get identity name versions


getVersions_ : Pkg.Name -> Registry -> Result (List Pkg.Name) KnownVersions
getVersions_ name (Registry _ versions) =
    case Dict.get identity name versions of
        Just kvs ->
            Ok kvs

        Nothing ->
            Err (Pkg.nearbyNames name (Dict.keys compare versions))



-- POST


post : Http.Manager -> String -> D.Decoder x a -> (a -> Task Never b) -> Task Never (Result Exit.RegistryProblem b)
post manager path decoder callback =
    Website.route path []
        |> Task.bind
            (\url ->
                Http.post manager url [] Exit.RP_Http <|
                    \body ->
                        case D.fromByteString decoder body of
                            Ok a ->
                                Task.fmap Ok (callback a)

                            Err _ ->
                                Task.pure <| Err <| Exit.RP_Data url body
            )



-- ENCODERS and DECODERS


registryDecoder : BD.Decoder Registry
registryDecoder =
    BD.map2 Registry
        BD.int
        (BD.assocListDict identity Pkg.nameDecoder knownVersionsDecoder)


registryEncoder : Registry -> BE.Encoder
registryEncoder (Registry size versions) =
    BE.sequence
        [ BE.int size
        , BE.assocListDict Pkg.compareName Pkg.nameEncoder knownVersionsEncoder versions
        ]


knownVersionsDecoder : BD.Decoder KnownVersions
knownVersionsDecoder =
    BD.map2 KnownVersions
        V.versionDecoder
        (BD.list V.versionDecoder)


knownVersionsEncoder : KnownVersions -> BE.Encoder
knownVersionsEncoder (KnownVersions version versions) =
    BE.sequence
        [ V.versionEncoder version
        , BE.list V.versionEncoder versions
        ]
