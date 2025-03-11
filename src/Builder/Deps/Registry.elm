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
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO)



-- REGISTRY


type Registry
    = Registry Int (Dict ( String, String ) Pkg.Name KnownVersions)


type KnownVersions
    = KnownVersions V.Version (List V.Version)


knownVersionsDecoder : Decode.Decoder KnownVersions
knownVersionsDecoder =
    Decode.map2 KnownVersions
        (Decode.field "version" V.jsonDecoder)
        (Decode.field "versions" (Decode.list V.jsonDecoder))


knownVersionsEncoder : KnownVersions -> Encode.Value
knownVersionsEncoder (KnownVersions version versions) =
    Encode.object
        [ ( "version", V.jsonEncoder version )
        , ( "versions", Encode.list V.jsonEncoder versions )
        ]



-- READ


read : Stuff.PackageCache -> IO (Maybe Registry)
read cache =
    File.readBinary registryDecoder (Stuff.registry cache)



-- FETCH


fetch : Http.Manager -> Stuff.PackageCache -> IO (Result Exit.RegistryProblem Registry)
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
                |> IO.fmap (\_ -> registry)


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


update : Http.Manager -> Stuff.PackageCache -> Registry -> IO (Result Exit.RegistryProblem Registry)
update manager cache ((Registry size packages) as oldRegistry) =
    post manager ("/all-packages/since/" ++ String.fromInt size) (D.list newPkgDecoder) <|
        \news ->
            case news of
                [] ->
                    IO.pure oldRegistry

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
                        |> IO.fmap (\_ -> newRegistry)


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


latest : Http.Manager -> Stuff.PackageCache -> IO (Result Exit.RegistryProblem Registry)
latest manager cache =
    read cache
        |> IO.bind
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


post : Http.Manager -> String -> D.Decoder x a -> (a -> IO b) -> IO (Result Exit.RegistryProblem b)
post manager path decoder callback =
    Website.route path []
        |> IO.bind
            (\url ->
                Http.post manager url [] Exit.RP_Http <|
                    \body ->
                        case D.fromByteString decoder body of
                            Ok a ->
                                IO.fmap Ok (callback a)

                            Err _ ->
                                IO.pure <| Err <| Exit.RP_Data url body
            )



-- ENCODERS and DECODERS


registryDecoder : Decode.Decoder Registry
registryDecoder =
    Decode.map2 Registry
        (Decode.field "size" Decode.int)
        (Decode.field "packages" (D.assocListDict identity Pkg.nameDecoder knownVersionsDecoder))


registryEncoder : Registry -> Encode.Value
registryEncoder (Registry size versions) =
    Encode.object
        [ ( "size", Encode.int size )
        , ( "packages", E.assocListDict Pkg.compareName Pkg.nameEncoder knownVersionsEncoder versions )
        ]
