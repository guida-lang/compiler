module Builder.Deps.Registry exposing
    ( fetch
    , getVersions
    , getVersions_
    , latest
    , read
    , update
    )

import Basics.Extra exposing (flip)
import Builder.Deps.Website as Website
import Builder.File as File
import Builder.Http as Http
import Builder.Stuff as Stuff
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO
import Types as T exposing (IO)



-- REGISTRY


knownVersionsDecoder : Decode.Decoder T.BDR_KnownVersions
knownVersionsDecoder =
    Decode.map2 T.BDR_KnownVersions
        (Decode.field "version" V.jsonDecoder)
        (Decode.field "versions" (Decode.list V.jsonDecoder))


knownVersionsEncoder : T.BDR_KnownVersions -> Encode.Value
knownVersionsEncoder (T.BDR_KnownVersions version versions) =
    Encode.object
        [ ( "version", V.jsonEncoder version )
        , ( "versions", Encode.list V.jsonEncoder versions )
        ]



-- READ


read : T.BS_PackageCache -> IO (Maybe T.BDR_Registry)
read cache =
    File.readBinary registryDecoder (Stuff.registry cache)



-- FETCH


fetch : T.BH_Manager -> T.BS_PackageCache -> IO (Result T.BRE_RegistryProblem T.BDR_Registry)
fetch manager cache =
    post manager "/all-packages" allPkgsDecoder <|
        \versions ->
            let
                size : Int
                size =
                    Dict.foldr Pkg.compareName (\_ -> addEntry) 0 versions

                registry : T.BDR_Registry
                registry =
                    T.BDR_Registry size versions

                path : String
                path =
                    Stuff.registry cache
            in
            File.writeBinary registryEncoder path registry
                |> IO.fmap (\_ -> registry)


addEntry : T.BDR_KnownVersions -> Int -> Int
addEntry (T.BDR_KnownVersions _ vs) count =
    count + 1 + List.length vs


allPkgsDecoder : D.Decoder () (Dict ( String, String ) T.CEP_Name T.BDR_KnownVersions)
allPkgsDecoder =
    let
        keyDecoder : D.KeyDecoder () T.CEP_Name
        keyDecoder =
            Pkg.keyDecoder bail

        versionsDecoder : D.Decoder () (List T.CEV_Version)
        versionsDecoder =
            D.list (D.mapError (\_ -> ()) V.decoder)

        toKnownVersions : List T.CEV_Version -> D.Decoder () T.BDR_KnownVersions
        toKnownVersions versions =
            case List.sortWith (flip V.compare) versions of
                v :: vs ->
                    D.pure (T.BDR_KnownVersions v vs)

                [] ->
                    D.failure ()
    in
    D.dict identity keyDecoder (D.bind toKnownVersions versionsDecoder)



-- UPDATE


update : T.BH_Manager -> T.BS_PackageCache -> T.BDR_Registry -> IO (Result T.BRE_RegistryProblem T.BDR_Registry)
update manager cache ((T.BDR_Registry size packages) as oldRegistry) =
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

                        newPkgs : Dict ( String, String ) T.CEP_Name T.BDR_KnownVersions
                        newPkgs =
                            List.foldr addNew packages news

                        newRegistry : T.BDR_Registry
                        newRegistry =
                            T.BDR_Registry newSize newPkgs
                    in
                    File.writeBinary registryEncoder (Stuff.registry cache) newRegistry
                        |> IO.fmap (\_ -> newRegistry)


addNew : ( T.CEP_Name, T.CEV_Version ) -> Dict ( String, String ) T.CEP_Name T.BDR_KnownVersions -> Dict ( String, String ) T.CEP_Name T.BDR_KnownVersions
addNew ( name, version ) versions =
    let
        add : Maybe T.BDR_KnownVersions -> T.BDR_KnownVersions
        add maybeKnowns =
            case maybeKnowns of
                Just (T.BDR_KnownVersions v vs) ->
                    T.BDR_KnownVersions version (v :: vs)

                Nothing ->
                    T.BDR_KnownVersions version []
    in
    Dict.update identity name (Just << add) versions



-- NEW PACKAGE DECODER


newPkgDecoder : D.Decoder () ( T.CEP_Name, T.CEV_Version )
newPkgDecoder =
    D.customString newPkgParser bail


newPkgParser : P.Parser () ( T.CEP_Name, T.CEV_Version )
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


latest : T.BH_Manager -> T.BS_PackageCache -> IO (Result T.BRE_RegistryProblem T.BDR_Registry)
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


getVersions : T.CEP_Name -> T.BDR_Registry -> Maybe T.BDR_KnownVersions
getVersions name (T.BDR_Registry _ versions) =
    Dict.get identity name versions


getVersions_ : T.CEP_Name -> T.BDR_Registry -> Result (List T.CEP_Name) T.BDR_KnownVersions
getVersions_ name (T.BDR_Registry _ versions) =
    case Dict.get identity name versions of
        Just kvs ->
            Ok kvs

        Nothing ->
            Err (Pkg.nearbyNames name (Dict.keys compare versions))



-- POST


post : T.BH_Manager -> String -> D.Decoder x a -> (a -> IO b) -> IO (Result T.BRE_RegistryProblem b)
post manager path decoder callback =
    let
        url : String
        url =
            Website.route path []
    in
    Http.post manager url [] T.BRE_RP_Http <|
        \body ->
            case D.fromByteString decoder body of
                Ok a ->
                    IO.fmap Ok (callback a)

                Err _ ->
                    IO.pure <| Err <| T.BRE_RP_Data url body



-- ENCODERS and DECODERS


registryDecoder : Decode.Decoder T.BDR_Registry
registryDecoder =
    Decode.map2 T.BDR_Registry
        (Decode.field "size" Decode.int)
        (Decode.field "packages" (D.assocListDict identity Pkg.nameDecoder knownVersionsDecoder))


registryEncoder : T.BDR_Registry -> Encode.Value
registryEncoder (T.BDR_Registry size versions) =
    Encode.object
        [ ( "size", Encode.int size )
        , ( "packages", E.assocListDict Pkg.compareName Pkg.nameEncoder knownVersionsEncoder versions )
        ]
