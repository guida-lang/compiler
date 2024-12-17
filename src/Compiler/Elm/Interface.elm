module Compiler.Elm.Interface exposing
    ( CEI_Alias(..)
    , CEI_Binop(..)
    , CEI_Interface(..)
    , CEI_Union(..)
    , DependencyInterface(..)
    , dependencyInterfaceDecoder
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
import Compiler.Data.Name as Name
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- INTERFACE


type CEI_Interface
    = CEI_Interface Pkg.CEP_Name (Dict String Name.CDN_Name Can.CASTC_Annotation) (Dict String Name.CDN_Name CEI_Union) (Dict String Name.CDN_Name CEI_Alias) (Dict String Name.CDN_Name CEI_Binop)


type CEI_Union
    = CEI_OpenUnion Can.CASTC_Union
    | CEI_ClosedUnion Can.CASTC_Union
    | CEI_PrivateUnion Can.CASTC_Union


type CEI_Alias
    = CEI_PublicAlias Can.CASTC_Alias
    | CEI_PrivateAlias Can.CASTC_Alias


type CEI_Binop
    = CEI_Binop Name.CDN_Name Can.CASTC_Annotation Binop.CASTU_Associativity Binop.CASTU_Precedence



-- FROM MODULE


fromModule : Pkg.CEP_Name -> Can.Module -> Dict String Name.CDN_Name Can.CASTC_Annotation -> CEI_Interface
fromModule home (Can.Module _ exports _ _ unions aliases binops _) annotations =
    CEI_Interface home
        (restrict exports annotations)
        (restrictUnions exports unions)
        (restrictAliases exports aliases)
        (restrict exports (Dict.map (\_ -> toOp annotations) binops))


restrict : Can.Exports -> Dict String Name.CDN_Name a -> Dict String Name.CDN_Name a
restrict exports dict =
    case exports of
        Can.ExportEverything _ ->
            dict

        Can.Export explicitExports ->
            Dict.intersection compare dict explicitExports


toOp : Dict String Name.CDN_Name Can.CASTC_Annotation -> Can.Binop -> CEI_Binop
toOp types (Can.Binop_ associativity precedence name) =
    CEI_Binop name (Utils.find identity name types) associativity precedence


restrictUnions : Can.Exports -> Dict String Name.CDN_Name Can.CASTC_Union -> Dict String Name.CDN_Name CEI_Union
restrictUnions exports unions =
    case exports of
        Can.ExportEverything _ ->
            Dict.map (\_ -> CEI_OpenUnion) unions

        Can.Export explicitExports ->
            Dict.merge compare
                (\_ _ result -> result)
                (\k (A.CRA_At _ export) union result ->
                    case export of
                        Can.ExportUnionOpen ->
                            Dict.insert identity k (CEI_OpenUnion union) result

                        Can.ExportUnionClosed ->
                            Dict.insert identity k (CEI_ClosedUnion union) result

                        _ ->
                            crash "impossible exports discovered in restrictUnions"
                )
                (\k union result -> Dict.insert identity k (CEI_PrivateUnion union) result)
                explicitExports
                unions
                Dict.empty


restrictAliases : Can.Exports -> Dict String Name.CDN_Name Can.CASTC_Alias -> Dict String Name.CDN_Name CEI_Alias
restrictAliases exports aliases =
    case exports of
        Can.ExportEverything _ ->
            Dict.map (\_ alias -> CEI_PublicAlias alias) aliases

        Can.Export explicitExports ->
            Dict.merge compare
                (\_ _ result -> result)
                (\k _ alias result -> Dict.insert identity k (CEI_PublicAlias alias) result)
                (\k alias result -> Dict.insert identity k (CEI_PrivateAlias alias) result)
                explicitExports
                aliases
                Dict.empty



-- TO PUBLIC


toPublicUnion : CEI_Union -> Maybe Can.CASTC_Union
toPublicUnion iUnion =
    case iUnion of
        CEI_OpenUnion union ->
            Just union

        CEI_ClosedUnion (Can.CASTC_Union vars _ _ opts) ->
            Just (Can.CASTC_Union vars [] 0 opts)

        CEI_PrivateUnion _ ->
            Nothing


toPublicAlias : CEI_Alias -> Maybe Can.CASTC_Alias
toPublicAlias iAlias =
    case iAlias of
        CEI_PublicAlias alias ->
            Just alias

        CEI_PrivateAlias _ ->
            Nothing



-- DEPENDENCY INTERFACE


type DependencyInterface
    = Public CEI_Interface
    | Private Pkg.CEP_Name (Dict String Name.CDN_Name Can.CASTC_Union) (Dict String Name.CDN_Name Can.CASTC_Alias)


public : CEI_Interface -> DependencyInterface
public =
    Public


private : CEI_Interface -> DependencyInterface
private (CEI_Interface pkg _ unions aliases _) =
    Private pkg (Dict.map (\_ -> extractUnion) unions) (Dict.map (\_ -> extractAlias) aliases)


extractUnion : CEI_Union -> Can.CASTC_Union
extractUnion iUnion =
    case iUnion of
        CEI_OpenUnion union ->
            union

        CEI_ClosedUnion union ->
            union

        CEI_PrivateUnion union ->
            union


extractAlias : CEI_Alias -> Can.CASTC_Alias
extractAlias iAlias =
    case iAlias of
        CEI_PublicAlias alias ->
            alias

        CEI_PrivateAlias alias ->
            alias


privatize : DependencyInterface -> DependencyInterface
privatize di =
    case di of
        Public i ->
            private i

        Private _ _ _ ->
            di



-- ENCODERS and DECODERS


interfaceEncoder : CEI_Interface -> Encode.Value
interfaceEncoder (CEI_Interface home values unions aliases binops) =
    Encode.object
        [ ( "type", Encode.string "Interface" )
        , ( "home", Pkg.nameEncoder home )
        , ( "values", E.assocListDict compare Encode.string Can.annotationEncoder values )
        , ( "unions", E.assocListDict compare Encode.string unionEncoder unions )
        , ( "aliases", E.assocListDict compare Encode.string aliasEncoder aliases )
        , ( "binops", E.assocListDict compare Encode.string binopEncoder binops )
        ]


interfaceDecoder : Decode.Decoder CEI_Interface
interfaceDecoder =
    Decode.map5 CEI_Interface
        (Decode.field "home" Pkg.nameDecoder)
        (Decode.field "values" (D.assocListDict identity Decode.string Can.annotationDecoder))
        (Decode.field "unions" (D.assocListDict identity Decode.string unionDecoder))
        (Decode.field "aliases" (D.assocListDict identity Decode.string aliasDecoder))
        (Decode.field "binops" (D.assocListDict identity Decode.string binopDecoder))


unionEncoder : CEI_Union -> Encode.Value
unionEncoder union_ =
    case union_ of
        CEI_OpenUnion union ->
            Encode.object
                [ ( "type", Encode.string "OpenUnion" )
                , ( "union", Can.unionEncoder union )
                ]

        CEI_ClosedUnion union ->
            Encode.object
                [ ( "type", Encode.string "ClosedUnion" )
                , ( "union", Can.unionEncoder union )
                ]

        CEI_PrivateUnion union ->
            Encode.object
                [ ( "type", Encode.string "ClosedUnion" )
                , ( "union", Can.unionEncoder union )
                ]


unionDecoder : Decode.Decoder CEI_Union
unionDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "OpenUnion" ->
                        Decode.map CEI_OpenUnion
                            (Decode.field "union" Can.unionDecoder)

                    "ClosedUnion" ->
                        Decode.map CEI_ClosedUnion
                            (Decode.field "union" Can.unionDecoder)

                    "PrivateUnion" ->
                        Decode.map CEI_ClosedUnion
                            (Decode.field "union" Can.unionDecoder)

                    _ ->
                        Decode.fail ("Unknown Union's type: " ++ type_)
            )


aliasEncoder : CEI_Alias -> Encode.Value
aliasEncoder aliasValue =
    case aliasValue of
        CEI_PublicAlias alias_ ->
            Encode.object
                [ ( "type", Encode.string "PublicAlias" )
                , ( "alias", Can.aliasEncoder alias_ )
                ]

        CEI_PrivateAlias alias_ ->
            Encode.object
                [ ( "type", Encode.string "PrivateAlias" )
                , ( "alias", Can.aliasEncoder alias_ )
                ]


aliasDecoder : Decode.Decoder CEI_Alias
aliasDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "PublicAlias" ->
                        Decode.map CEI_PublicAlias
                            (Decode.field "alias" Can.aliasDecoder)

                    "PrivateAlias" ->
                        Decode.map CEI_PrivateAlias
                            (Decode.field "alias" Can.aliasDecoder)

                    _ ->
                        Decode.fail ("Unknown Alias' type: " ++ type_)
            )


binopEncoder : CEI_Binop -> Encode.Value
binopEncoder (CEI_Binop name annotation associativity precedence) =
    Encode.object
        [ ( "type", Encode.string "Binop" )
        , ( "name", Encode.string name )
        , ( "annotation", Can.annotationEncoder annotation )
        , ( "associativity", Binop.associativityEncoder associativity )
        , ( "precedence", Binop.precedenceEncoder precedence )
        ]


binopDecoder : Decode.Decoder CEI_Binop
binopDecoder =
    Decode.map4 CEI_Binop
        (Decode.field "name" Decode.string)
        (Decode.field "annotation" Can.annotationDecoder)
        (Decode.field "associativity" Binop.associativityDecoder)
        (Decode.field "precedence" Binop.precedenceDecoder)


dependencyInterfaceEncoder : DependencyInterface -> Encode.Value
dependencyInterfaceEncoder dependencyInterface =
    case dependencyInterface of
        Public i ->
            Encode.object
                [ ( "type", Encode.string "Public" )
                , ( "i", interfaceEncoder i )
                ]

        Private pkg unions aliases ->
            Encode.object
                [ ( "type", Encode.string "Private" )
                , ( "pkg", Pkg.nameEncoder pkg )
                , ( "unions", E.assocListDict compare Encode.string Can.unionEncoder unions )
                , ( "aliases", E.assocListDict compare Encode.string Can.aliasEncoder aliases )
                ]


dependencyInterfaceDecoder : Decode.Decoder DependencyInterface
dependencyInterfaceDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Public" ->
                        Decode.map Public (Decode.field "i" interfaceDecoder)

                    "Private" ->
                        Decode.map3 Private
                            (Decode.field "pkg" Pkg.nameDecoder)
                            (Decode.field "unions" (D.assocListDict identity Decode.string Can.unionDecoder))
                            (Decode.field "aliases" (D.assocListDict identity Decode.string Can.aliasDecoder))

                    _ ->
                        Decode.fail ("Failed to decode DependencyInterface's type: " ++ type_)
            )
