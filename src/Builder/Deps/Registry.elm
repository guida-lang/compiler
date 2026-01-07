module Builder.Deps.Registry exposing
    ( FilterVersions(..)
    , KnownVersions(..)
    , Registry(..)
    , Syntax(..)
    , fetch
    , getVersions
    , getVersions_
    , latest
    , read
    , registryDecoder
    , registryEncoder
    , update
    )

import Builder.Deps.Website as Website
import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit as Exit
import Builder.Stuff as Stuff
import Compiler.Generate.Target as Target exposing (Target)
import Compiler.Guida.Package as Pkg
import Compiler.Guida.Version as V
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
    = KnownVersions ( Syntax, V.Version ) (List ( Syntax, V.Version ))


type Syntax
    = Guida
    | Elm



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

        versionsDecoder : Syntax -> D.Decoder () (List ( Syntax, V.Version ))
        versionsDecoder syntax =
            D.fmap (List.map (Tuple.pair syntax)) (D.list (D.mapError (\_ -> ()) V.decoder))

        toKnownVersions : List ( Syntax, V.Version ) -> D.Decoder () KnownVersions
        toKnownVersions versions =
            case List.sortWith (\( _, v1 ) ( _, v2 ) -> V.compare v2 v1) versions of
                v :: vs ->
                    D.pure (KnownVersions v vs)

                [] ->
                    D.failure ()
    in
    D.oneOf
        [ D.pure Tuple.pair
            |> D.apply (D.field "guida" (D.dict identity keyDecoder (versionsDecoder Guida)))
            |> D.apply (D.field "elm" (D.dict identity keyDecoder (versionsDecoder Elm)))
            |> D.bind
                (\( guidaVersions, elmVersions ) ->
                    Dict.merge compare
                        (\pkgName guidaPkgVersions ->
                            D.bind
                                (\acc ->
                                    toKnownVersions guidaPkgVersions
                                        |> D.fmap (\knownVersions -> Dict.insert identity pkgName knownVersions acc)
                                )
                        )
                        (\pkgName guidaPkgVersions elmPkgVersions ->
                            D.bind
                                (\acc ->
                                    toKnownVersions (guidaPkgVersions ++ elmPkgVersions)
                                        |> D.fmap (\knownVersions -> Dict.insert identity pkgName knownVersions acc)
                                )
                        )
                        (\pkgName elmPkgVersions ->
                            D.bind
                                (\acc ->
                                    toKnownVersions elmPkgVersions
                                        |> D.fmap (\knownVersions -> Dict.insert identity pkgName knownVersions acc)
                                )
                        )
                        guidaVersions
                        elmVersions
                        (D.pure Dict.empty)
                )
        , D.dict identity keyDecoder (D.bind toKnownVersions (versionsDecoder Elm))
        ]



-- UPDATE


update : Http.Manager -> Stuff.PackageCache -> Registry -> Task Never (Result Exit.RegistryProblem Registry)
update manager cache ((Registry size packages) as oldRegistry) =
    post manager ("/all-packages/since/" ++ String.fromInt size) newPkgsDecoder <|
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


addNew : ( Pkg.Name, ( Syntax, V.Version ) ) -> Dict ( String, String ) Pkg.Name KnownVersions -> Dict ( String, String ) Pkg.Name KnownVersions
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


newPkgsDecoder : D.Decoder () (List ( Pkg.Name, ( Syntax, V.Version ) ))
newPkgsDecoder =
    D.oneOf
        [ D.pure List.append
            |> D.apply (D.field "guida" (D.list (D.fmap (\( pkgName, version ) -> ( pkgName, ( Guida, version ) )) newPkgDecoder)))
            |> D.apply (D.field "elm" (D.list (D.fmap (\( pkgName, version ) -> ( pkgName, ( Elm, version ) )) newPkgDecoder)))
        , D.list (D.fmap (\( pkgName, version ) -> ( pkgName, ( Elm, version ) )) newPkgDecoder)
        ]


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


type FilterVersions
    = KeepAllVersions
    | FilterByTarget Target


getVersions : FilterVersions -> Pkg.Name -> Registry -> Maybe KnownVersions
getVersions filterVersions name (Registry _ versions) =
    case filterVersions of
        FilterByTarget Target.ElmTarget ->
            Dict.get identity name versions
                |> Maybe.andThen
                    (\(KnownVersions v vs) ->
                        case List.filter (\( syntax, _ ) -> syntax == Elm) (v :: vs) of
                            goodVersion :: otherGoodVersions ->
                                Just (KnownVersions goodVersion otherGoodVersions)

                            [] ->
                                Nothing
                    )

        _ ->
            Dict.get identity name versions


getVersions_ : FilterVersions -> Pkg.Name -> Registry -> Result (List Pkg.Name) KnownVersions
getVersions_ filterVersions name ((Registry _ versions) as registry) =
    case getVersions filterVersions name registry of
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
        (BD.jsonPair syntaxDecoder V.versionDecoder)
        (BD.list (BD.jsonPair syntaxDecoder V.versionDecoder))


knownVersionsEncoder : KnownVersions -> BE.Encoder
knownVersionsEncoder (KnownVersions version versions) =
    BE.sequence
        [ BE.jsonPair syntaxEncoder V.versionEncoder version
        , BE.list (BE.jsonPair syntaxEncoder V.versionEncoder) versions
        ]


syntaxDecoder : BD.Decoder Syntax
syntaxDecoder =
    BD.string
        |> BD.andThen
            (\syntax ->
                case syntax of
                    "guida" ->
                        BD.succeed Guida

                    "elm" ->
                        BD.succeed Elm

                    _ ->
                        BD.fail
            )


syntaxEncoder : Syntax -> BE.Encoder
syntaxEncoder syntax =
    case syntax of
        Guida ->
            BE.string "guida"

        Elm ->
            BE.string "elm"
