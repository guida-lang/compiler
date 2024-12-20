module Compiler.Elm.Interface exposing
    ( dependencyInterfaceDecoder
    , dependencyInterfaceEncoder
    , extractAlias
    , extractUnion
    , fromModule
    , interfaceDecoder
    , interfaceEncoder
    , private
    , privatize
    , public
    , toPublicAlias
    , toPublicUnion
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Binop as Binop
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- FROM MODULE


fromModule : T.CEP_Name -> Can.Module -> Dict String T.CDN_Name T.CASTC_Annotation -> T.CEI_Interface
fromModule home (Can.Module _ exports _ _ unions aliases binops _) annotations =
    T.CEI_Interface home
        (restrict exports annotations)
        (restrictUnions exports unions)
        (restrictAliases exports aliases)
        (restrict exports (Dict.map (\_ -> toOp annotations) binops))


restrict : Can.Exports -> Dict String T.CDN_Name a -> Dict String T.CDN_Name a
restrict exports dict =
    case exports of
        Can.ExportEverything _ ->
            dict

        Can.Export explicitExports ->
            Dict.intersection compare dict explicitExports


toOp : Dict String T.CDN_Name T.CASTC_Annotation -> Can.Binop -> T.CEI_Binop
toOp types (Can.Binop_ associativity precedence name) =
    T.CEI_Binop name (Utils.find identity name types) associativity precedence


restrictUnions : Can.Exports -> Dict String T.CDN_Name T.CASTC_Union -> Dict String T.CDN_Name T.CEI_Union
restrictUnions exports unions =
    case exports of
        Can.ExportEverything _ ->
            Dict.map (\_ -> T.CEI_OpenUnion) unions

        Can.Export explicitExports ->
            Dict.merge compare
                (\_ _ result -> result)
                (\k (T.CRA_At _ export) union result ->
                    case export of
                        Can.ExportUnionOpen ->
                            Dict.insert identity k (T.CEI_OpenUnion union) result

                        Can.ExportUnionClosed ->
                            Dict.insert identity k (T.CEI_ClosedUnion union) result

                        _ ->
                            crash "impossible exports discovered in restrictUnions"
                )
                (\k union result -> Dict.insert identity k (T.CEI_PrivateUnion union) result)
                explicitExports
                unions
                Dict.empty


restrictAliases : Can.Exports -> Dict String T.CDN_Name T.CASTC_Alias -> Dict String T.CDN_Name T.CEI_Alias
restrictAliases exports aliases =
    case exports of
        Can.ExportEverything _ ->
            Dict.map (\_ alias -> T.CEI_PublicAlias alias) aliases

        Can.Export explicitExports ->
            Dict.merge compare
                (\_ _ result -> result)
                (\k _ alias result -> Dict.insert identity k (T.CEI_PublicAlias alias) result)
                (\k alias result -> Dict.insert identity k (T.CEI_PrivateAlias alias) result)
                explicitExports
                aliases
                Dict.empty



-- TO PUBLIC


toPublicUnion : T.CEI_Union -> Maybe T.CASTC_Union
toPublicUnion iUnion =
    case iUnion of
        T.CEI_OpenUnion union ->
            Just union

        T.CEI_ClosedUnion (T.CASTC_Union vars _ _ opts) ->
            Just (T.CASTC_Union vars [] 0 opts)

        T.CEI_PrivateUnion _ ->
            Nothing


toPublicAlias : T.CEI_Alias -> Maybe T.CASTC_Alias
toPublicAlias iAlias =
    case iAlias of
        T.CEI_PublicAlias alias ->
            Just alias

        T.CEI_PrivateAlias _ ->
            Nothing



-- DEPENDENCY INTERFACE


public : T.CEI_Interface -> T.CEI_DependencyInterface
public =
    T.CEI_Public


private : T.CEI_Interface -> T.CEI_DependencyInterface
private (T.CEI_Interface pkg _ unions aliases _) =
    T.CEI_Private pkg (Dict.map (\_ -> extractUnion) unions) (Dict.map (\_ -> extractAlias) aliases)


extractUnion : T.CEI_Union -> T.CASTC_Union
extractUnion iUnion =
    case iUnion of
        T.CEI_OpenUnion union ->
            union

        T.CEI_ClosedUnion union ->
            union

        T.CEI_PrivateUnion union ->
            union


extractAlias : T.CEI_Alias -> T.CASTC_Alias
extractAlias iAlias =
    case iAlias of
        T.CEI_PublicAlias alias ->
            alias

        T.CEI_PrivateAlias alias ->
            alias


privatize : T.CEI_DependencyInterface -> T.CEI_DependencyInterface
privatize di =
    case di of
        T.CEI_Public i ->
            private i

        T.CEI_Private _ _ _ ->
            di



-- ENCODERS and DECODERS


interfaceEncoder : T.CEI_Interface -> Encode.Value
interfaceEncoder (T.CEI_Interface home values unions aliases binops) =
    Encode.object
        [ ( "type", Encode.string "Interface" )
        , ( "home", Pkg.nameEncoder home )
        , ( "values", E.assocListDict compare Encode.string Can.annotationEncoder values )
        , ( "unions", E.assocListDict compare Encode.string unionEncoder unions )
        , ( "aliases", E.assocListDict compare Encode.string aliasEncoder aliases )
        , ( "binops", E.assocListDict compare Encode.string binopEncoder binops )
        ]


interfaceDecoder : Decode.Decoder T.CEI_Interface
interfaceDecoder =
    Decode.map5 T.CEI_Interface
        (Decode.field "home" Pkg.nameDecoder)
        (Decode.field "values" (D.assocListDict identity Decode.string Can.annotationDecoder))
        (Decode.field "unions" (D.assocListDict identity Decode.string unionDecoder))
        (Decode.field "aliases" (D.assocListDict identity Decode.string aliasDecoder))
        (Decode.field "binops" (D.assocListDict identity Decode.string binopDecoder))


unionEncoder : T.CEI_Union -> Encode.Value
unionEncoder union_ =
    case union_ of
        T.CEI_OpenUnion union ->
            Encode.object
                [ ( "type", Encode.string "OpenUnion" )
                , ( "union", Can.unionEncoder union )
                ]

        T.CEI_ClosedUnion union ->
            Encode.object
                [ ( "type", Encode.string "ClosedUnion" )
                , ( "union", Can.unionEncoder union )
                ]

        T.CEI_PrivateUnion union ->
            Encode.object
                [ ( "type", Encode.string "ClosedUnion" )
                , ( "union", Can.unionEncoder union )
                ]


unionDecoder : Decode.Decoder T.CEI_Union
unionDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "OpenUnion" ->
                        Decode.map T.CEI_OpenUnion
                            (Decode.field "union" Can.unionDecoder)

                    "ClosedUnion" ->
                        Decode.map T.CEI_ClosedUnion
                            (Decode.field "union" Can.unionDecoder)

                    "PrivateUnion" ->
                        Decode.map T.CEI_ClosedUnion
                            (Decode.field "union" Can.unionDecoder)

                    _ ->
                        Decode.fail ("Unknown Union's type: " ++ type_)
            )


aliasEncoder : T.CEI_Alias -> Encode.Value
aliasEncoder aliasValue =
    case aliasValue of
        T.CEI_PublicAlias alias_ ->
            Encode.object
                [ ( "type", Encode.string "PublicAlias" )
                , ( "alias", Can.aliasEncoder alias_ )
                ]

        T.CEI_PrivateAlias alias_ ->
            Encode.object
                [ ( "type", Encode.string "PrivateAlias" )
                , ( "alias", Can.aliasEncoder alias_ )
                ]


aliasDecoder : Decode.Decoder T.CEI_Alias
aliasDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PublicAlias" ->
                        Decode.map T.CEI_PublicAlias
                            (Decode.field "alias" Can.aliasDecoder)

                    "PrivateAlias" ->
                        Decode.map T.CEI_PrivateAlias
                            (Decode.field "alias" Can.aliasDecoder)

                    _ ->
                        Decode.fail ("Unknown Alias' type: " ++ type_)
            )


binopEncoder : T.CEI_Binop -> Encode.Value
binopEncoder (T.CEI_Binop name annotation associativity precedence) =
    Encode.object
        [ ( "type", Encode.string "Binop" )
        , ( "name", Encode.string name )
        , ( "annotation", Can.annotationEncoder annotation )
        , ( "associativity", Binop.associativityEncoder associativity )
        , ( "precedence", Binop.precedenceEncoder precedence )
        ]


binopDecoder : Decode.Decoder T.CEI_Binop
binopDecoder =
    Decode.map4 T.CEI_Binop
        (Decode.field "name" Decode.string)
        (Decode.field "annotation" Can.annotationDecoder)
        (Decode.field "associativity" Binop.associativityDecoder)
        (Decode.field "precedence" Binop.precedenceDecoder)


dependencyInterfaceEncoder : T.CEI_DependencyInterface -> Encode.Value
dependencyInterfaceEncoder dependencyInterface =
    case dependencyInterface of
        T.CEI_Public i ->
            Encode.object
                [ ( "type", Encode.string "Public" )
                , ( "i", interfaceEncoder i )
                ]

        T.CEI_Private pkg unions aliases ->
            Encode.object
                [ ( "type", Encode.string "Private" )
                , ( "pkg", Pkg.nameEncoder pkg )
                , ( "unions", E.assocListDict compare Encode.string Can.unionEncoder unions )
                , ( "aliases", E.assocListDict compare Encode.string Can.aliasEncoder aliases )
                ]


dependencyInterfaceDecoder : Decode.Decoder T.CEI_DependencyInterface
dependencyInterfaceDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Public" ->
                        Decode.map T.CEI_Public (Decode.field "i" interfaceDecoder)

                    "Private" ->
                        Decode.map3 T.CEI_Private
                            (Decode.field "pkg" Pkg.nameDecoder)
                            (Decode.field "unions" (D.assocListDict identity Decode.string Can.unionDecoder))
                            (Decode.field "aliases" (D.assocListDict identity Decode.string Can.aliasDecoder))

                    _ ->
                        Decode.fail ("Failed to decode DependencyInterface's type: " ++ type_)
            )
