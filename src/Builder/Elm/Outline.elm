module Builder.Elm.Outline exposing
    ( AppOutline(..)
    , Decoder
    , Exposed(..)
    , Outline(..)
    , PkgOutline(..)
    , SrcDir(..)
    , decoder
    , defaultSummary
    , flattenExposed
    , read
    , srcDirDecoder
    , srcDirEncoder
    , write
    )

import Basics.Extra as Basics
import Builder.File as File
import Builder.Reporting.Exit as Exit
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.Constraint as Con
import Compiler.Elm.Licenses as Licenses
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import System.IO as IO exposing (IO)
import Utils.Main as Utils exposing (FilePath)



-- OUTLINE


type Outline
    = App AppOutline
    | Pkg PkgOutline


type AppOutline
    = AppOutline V.Version (NE.Nonempty SrcDir) (Dict Pkg.Name V.Version) (Dict Pkg.Name V.Version) (Dict Pkg.Name V.Version) (Dict Pkg.Name V.Version)


type PkgOutline
    = PkgOutline Pkg.Name String Licenses.License V.Version Exposed (Dict Pkg.Name Con.Constraint) (Dict Pkg.Name Con.Constraint) Con.Constraint


type Exposed
    = ExposedList (List ModuleName.Raw)
    | ExposedDict (List ( String, List ModuleName.Raw ))


type SrcDir
    = AbsoluteSrcDir FilePath
    | RelativeSrcDir FilePath



-- DEFAULTS


defaultSummary : String
defaultSummary =
    "helpful summary of your project, less than 80 characters"



-- HELPERS


flattenExposed : Exposed -> List ModuleName.Raw
flattenExposed exposed =
    case exposed of
        ExposedList names ->
            names

        ExposedDict sections ->
            List.concatMap Tuple.second sections



-- WRITE


write : FilePath -> Outline -> IO ()
write root outline =
    E.write (root ++ "/elm.json") (encode outline)



-- JSON ENCODE


encode : Outline -> E.Value
encode outline =
    case outline of
        App (AppOutline elm srcDirs depsDirect depsTrans testDirect testTrans) ->
            E.object
                [ ( "type", E.string "application" )
                , ( "source-directories", E.list encodeSrcDir (NE.toList srcDirs) )
                , ( "elm-version", V.encode elm )
                , ( "dependencies"
                  , E.object
                        [ ( "direct", encodeDeps V.encode depsDirect )
                        , ( "indirect", encodeDeps V.encode depsTrans )
                        ]
                  )
                , ( "test-dependencies"
                  , E.object
                        [ ( "direct", encodeDeps V.encode testDirect )
                        , ( "indirect", encodeDeps V.encode testTrans )
                        ]
                  )
                ]

        Pkg (PkgOutline name summary license version exposed deps tests elm) ->
            E.object
                [ ( "type", E.string "package" )
                , ( "name", Pkg.encode name )
                , ( "summary", E.string summary )
                , ( "license", Licenses.encode license )
                , ( "version", V.encode version )
                , ( "exposed-modules", encodeExposed exposed )
                , ( "elm-version", Con.encode elm )
                , ( "dependencies", encodeDeps Con.encode deps )
                , ( "test-dependencies", encodeDeps Con.encode tests )
                ]


encodeExposed : Exposed -> E.Value
encodeExposed exposed =
    case exposed of
        ExposedList modules ->
            E.list encodeModule modules

        ExposedDict chunks ->
            E.object (List.map (Tuple.mapSecond (E.list encodeModule)) chunks)


encodeModule : ModuleName.Raw -> E.Value
encodeModule name =
    E.name name


encodeDeps : (a -> E.Value) -> Dict Pkg.Name a -> E.Value
encodeDeps encodeValue deps =
    E.dict Pkg.compareName Pkg.toJsonString encodeValue deps


encodeSrcDir : SrcDir -> E.Value
encodeSrcDir srcDir =
    case srcDir of
        AbsoluteSrcDir dir ->
            E.string dir

        RelativeSrcDir dir ->
            E.string dir



-- PARSE AND VERIFY


read : FilePath -> IO (Result Exit.Outline Outline)
read root =
    File.readUtf8 (root ++ "/elm.json")
        |> IO.bind
            (\bytes ->
                case D.fromByteString decoder bytes of
                    Err err ->
                        IO.pure <| Err (Exit.OutlineHasBadStructure err)

                    Ok outline ->
                        case outline of
                            Pkg (PkgOutline pkg _ _ _ _ deps _ _) ->
                                IO.pure <|
                                    if not (Dict.member Pkg.core deps) && pkg /= Pkg.core then
                                        Err Exit.OutlineNoPkgCore

                                    else
                                        Ok outline

                            App (AppOutline _ srcDirs direct indirect _ _) ->
                                if not (Dict.member Pkg.core direct) then
                                    IO.pure <| Err Exit.OutlineNoAppCore

                                else if not (Dict.member Pkg.json direct) && not (Dict.member Pkg.json indirect) then
                                    IO.pure <| Err Exit.OutlineNoAppJson

                                else
                                    Utils.filterM (isSrcDirMissing root) (NE.toList srcDirs)
                                        |> IO.bind
                                            (\badDirs ->
                                                case List.map toGiven badDirs of
                                                    d :: ds ->
                                                        IO.pure <| Err (Exit.OutlineHasMissingSrcDirs d ds)

                                                    [] ->
                                                        detectDuplicates root (NE.toList srcDirs)
                                                            |> IO.bind
                                                                (\maybeDups ->
                                                                    case maybeDups of
                                                                        Nothing ->
                                                                            IO.pure <| Ok outline

                                                                        Just ( canonicalDir, ( dir1, dir2 ) ) ->
                                                                            IO.pure <| Err (Exit.OutlineHasDuplicateSrcDirs canonicalDir dir1 dir2)
                                                                )
                                            )
            )


isSrcDirMissing : FilePath -> SrcDir -> IO Bool
isSrcDirMissing root srcDir =
    IO.fmap not (Utils.dirDoesDirectoryExist (toAbsolute root srcDir))


toGiven : SrcDir -> FilePath
toGiven srcDir =
    case srcDir of
        AbsoluteSrcDir dir ->
            dir

        RelativeSrcDir dir ->
            dir


toAbsolute : FilePath -> SrcDir -> FilePath
toAbsolute root srcDir =
    case srcDir of
        AbsoluteSrcDir dir ->
            dir

        RelativeSrcDir dir ->
            Utils.fpForwardSlash root dir


detectDuplicates : FilePath -> List SrcDir -> IO (Maybe ( FilePath, ( FilePath, FilePath ) ))
detectDuplicates root srcDirs =
    Utils.listTraverse (toPair root) srcDirs
        |> IO.fmap
            (\pairs ->
                Utils.mapLookupMin <|
                    Utils.mapMapMaybe compare isDup <|
                        Utils.mapFromListWith compare OneOrMore.more pairs
            )


toPair : FilePath -> SrcDir -> IO ( FilePath, OneOrMore.OneOrMore FilePath )
toPair root srcDir =
    Utils.dirCanonicalizePath (toAbsolute root srcDir)
        |> IO.bind
            (\key ->
                IO.pure ( key, OneOrMore.one (toGiven srcDir) )
            )


isDup : OneOrMore.OneOrMore FilePath -> Maybe ( FilePath, FilePath )
isDup paths =
    case paths of
        OneOrMore.One _ ->
            Nothing

        OneOrMore.More a b ->
            Just (OneOrMore.getFirstTwo a b)



-- JSON DECODE


type alias Decoder a =
    D.Decoder Exit.OutlineProblem a


decoder : Decoder Outline
decoder =
    let
        application : String
        application =
            "application"

        package : String
        package =
            "package"
    in
    D.field "type" D.string
        |> D.bind
            (\tipe ->
                if tipe == application then
                    D.fmap App appDecoder

                else if tipe == package then
                    D.fmap Pkg pkgDecoder

                else
                    D.failure Exit.OP_BadType
            )


appDecoder : Decoder AppOutline
appDecoder =
    D.pure AppOutline
        |> D.apply (D.field "elm-version" versionDecoder)
        |> D.apply (D.field "source-directories" dirsDecoder)
        |> D.apply (D.field "dependencies" (D.field "direct" (depsDecoder versionDecoder)))
        |> D.apply (D.field "dependencies" (D.field "indirect" (depsDecoder versionDecoder)))
        |> D.apply (D.field "test-dependencies" (D.field "direct" (depsDecoder versionDecoder)))
        |> D.apply (D.field "test-dependencies" (D.field "indirect" (depsDecoder versionDecoder)))


pkgDecoder : Decoder PkgOutline
pkgDecoder =
    D.pure PkgOutline
        |> D.apply (D.field "name" nameDecoder)
        |> D.apply (D.field "summary" summaryDecoder)
        |> D.apply (D.field "license" (Licenses.decoder Exit.OP_BadLicense))
        |> D.apply (D.field "version" versionDecoder)
        |> D.apply (D.field "exposed-modules" exposedDecoder)
        |> D.apply (D.field "dependencies" (depsDecoder constraintDecoder))
        |> D.apply (D.field "test-dependencies" (depsDecoder constraintDecoder))
        |> D.apply (D.field "elm-version" constraintDecoder)



-- JSON DECODE HELPERS


nameDecoder : Decoder Pkg.Name
nameDecoder =
    D.mapError (Basics.uncurry Exit.OP_BadPkgName) Pkg.decoder


summaryDecoder : Decoder String
summaryDecoder =
    D.customString
        (boundParser 80 Exit.OP_BadSummaryTooLong)
        (\_ _ -> Exit.OP_BadSummaryTooLong)


versionDecoder : Decoder V.Version
versionDecoder =
    D.mapError (Basics.uncurry Exit.OP_BadVersion) V.decoder


constraintDecoder : Decoder Con.Constraint
constraintDecoder =
    D.mapError Exit.OP_BadConstraint Con.decoder


depsDecoder : Decoder a -> Decoder (Dict Pkg.Name a)
depsDecoder valueDecoder =
    D.dict Pkg.compareName (Pkg.keyDecoder Exit.OP_BadDependencyName) valueDecoder


dirsDecoder : Decoder (NE.Nonempty SrcDir)
dirsDecoder =
    D.fmap (NE.map toSrcDir) (D.nonEmptyList D.string Exit.OP_NoSrcDirs)


toSrcDir : FilePath -> SrcDir
toSrcDir path =
    if Utils.fpIsRelative path then
        RelativeSrcDir path

    else
        AbsoluteSrcDir path



-- EXPOSED MODULES DECODER


exposedDecoder : Decoder Exposed
exposedDecoder =
    D.oneOf
        [ D.fmap ExposedList (D.list moduleDecoder)
        , D.fmap ExposedDict (D.pairs headerKeyDecoder (D.list moduleDecoder))
        ]


moduleDecoder : Decoder ModuleName.Raw
moduleDecoder =
    D.mapError (Basics.uncurry Exit.OP_BadModuleName) ModuleName.decoder


headerKeyDecoder : D.KeyDecoder Exit.OutlineProblem String
headerKeyDecoder =
    D.KeyDecoder
        (boundParser 20 Exit.OP_BadModuleHeaderTooLong)
        (\_ _ -> Exit.OP_BadModuleHeaderTooLong)



-- BOUND PARSER


boundParser : Int -> x -> P.Parser x String
boundParser bound tooLong =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                len : Int
                len =
                    end - pos

                newCol : P.Col
                newCol =
                    col + len
            in
            if len < bound then
                Ok (P.POk P.Consumed (String.slice pos end src) (P.State src end end indent row newCol))

            else
                Err (P.PErr P.Consumed row newCol (\_ _ -> tooLong))


srcDirEncoder : SrcDir -> Encode.Value
srcDirEncoder srcDir =
    case srcDir of
        AbsoluteSrcDir dir ->
            Encode.object
                [ ( "type", Encode.string "AbsoluteSrcDir" )
                , ( "dir", Encode.string dir )
                ]

        RelativeSrcDir dir ->
            Encode.object
                [ ( "type", Encode.string "RelativeSrcDir" )
                , ( "dir", Encode.string dir )
                ]


srcDirDecoder : Decode.Decoder SrcDir
srcDirDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "AbsoluteSrcDir" ->
                        Decode.map AbsoluteSrcDir (Decode.field "dir" Decode.string)

                    "RelativeSrcDir" ->
                        Decode.map RelativeSrcDir (Decode.field "dir" Decode.string)

                    _ ->
                        Decode.fail ("Failed to decode SrcDir's type: " ++ type_)
            )
