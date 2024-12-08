module Compiler.Elm.Docs exposing
    ( Alias(..)
    , Binop(..)
    , Comment
    , Documentation
    , Error(..)
    , Module(..)
    , Union(..)
    , Value(..)
    , decoder
    , encode
    , fromModule
    , jsonDecoder
    , jsonEncoder
    , jsonModuleDecoder
    , jsonModuleEncoder
    )

import Basics.Extra exposing (flip)
import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.Compiler.Type as Type
import Compiler.Elm.Compiler.Type.Extract as Extract
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Json.String as Json
import Compiler.Parse.Primitives as P exposing (Col, Row, word1)
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Docs as E
import Compiler.Reporting.Result as Result
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import System.TypeCheck.IO as IO
import Utils.Main as Utils



-- DOCUMENTATION


type alias Documentation =
    Dict Name Module


type Module
    = Module Name Comment (Dict Name Union) (Dict Name Alias) (Dict Name Value) (Dict Name Binop)


type alias Comment =
    String


type Alias
    = Alias Comment (List Name) Type.Type


type Union
    = Union Comment (List Name) (List ( Name, List Type.Type ))


type Value
    = Value Comment Type.Type


type Binop
    = Binop Comment Type.Type Binop.Associativity Binop.Precedence



-- JSON


encode : Documentation -> E.Value
encode docs =
    E.list encodeModule (Dict.values docs)


encodeModule : Module -> E.Value
encodeModule (Module name comment unions aliases values binops) =
    E.object
        [ ( "name", ModuleName.encode name )
        , ( "comment", E.string comment )
        , ( "unions", E.list encodeUnion (Dict.toList unions) )
        , ( "aliases", E.list encodeAlias (Dict.toList aliases) )
        , ( "values", E.list encodeValue (Dict.toList values) )
        , ( "binops", E.list encodeBinop (Dict.toList binops) )
        ]


type Error
    = BadAssociativity
    | BadModuleName
    | BadType


decoder : D.Decoder Error Documentation
decoder =
    D.fmap toDict (D.list moduleDecoder)


toDict : List Module -> Documentation
toDict modules =
    Dict.fromList compare (List.map toDictHelp modules)


toDictHelp : Module -> ( Name.Name, Module )
toDictHelp ((Module name _ _ _ _ _) as modul) =
    ( name, modul )


moduleDecoder : D.Decoder Error Module
moduleDecoder =
    D.pure Module
        |> D.apply (D.field "name" moduleNameDecoder)
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "unions" (dictDecoder union))
        |> D.apply (D.field "aliases" (dictDecoder alias_))
        |> D.apply (D.field "values" (dictDecoder value))
        |> D.apply (D.field "binops" (dictDecoder binop))


dictDecoder : D.Decoder Error a -> D.Decoder Error (Dict Name a)
dictDecoder entryDecoder =
    D.fmap (Dict.fromList compare) (D.list (named entryDecoder))


named : D.Decoder Error a -> D.Decoder Error ( Name.Name, a )
named entryDecoder =
    D.pure Tuple.pair
        |> D.apply (D.field "name" nameDecoder)
        |> D.apply entryDecoder


nameDecoder : D.Decoder e Name
nameDecoder =
    D.string


moduleNameDecoder : D.Decoder Error ModuleName.Raw
moduleNameDecoder =
    D.mapError (always BadModuleName) ModuleName.decoder


typeDecoder : D.Decoder Error Type.Type
typeDecoder =
    D.mapError (always BadType) Type.decoder



-- UNION JSON


encodeUnion : ( Name, Union ) -> E.Value
encodeUnion ( name, Union comment args cases ) =
    E.object
        [ ( "name", E.name name )
        , ( "comment", E.string comment )
        , ( "args", E.list E.name args )
        , ( "cases", E.list encodeCase cases )
        ]


union : D.Decoder Error Union
union =
    D.pure Union
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "args" (D.list nameDecoder))
        |> D.apply (D.field "cases" (D.list caseDecoder))


encodeCase : ( Name, List Type.Type ) -> E.Value
encodeCase ( tag, args ) =
    E.list identity [ E.name tag, E.list Type.encode args ]


caseDecoder : D.Decoder Error ( Name.Name, List Type.Type )
caseDecoder =
    D.pair nameDecoder (D.list typeDecoder)



-- ALIAS JSON


encodeAlias : ( Name, Alias ) -> E.Value
encodeAlias ( name, Alias comment args tipe ) =
    E.object
        [ ( "name", E.name name )
        , ( "comment", E.string comment )
        , ( "args", E.list E.name args )
        , ( "type", Type.encode tipe )
        ]


alias_ : D.Decoder Error Alias
alias_ =
    D.pure Alias
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "args" (D.list nameDecoder))
        |> D.apply (D.field "type" typeDecoder)



-- VALUE JSON


encodeValue : ( Name.Name, Value ) -> E.Value
encodeValue ( name, Value comment tipe ) =
    E.object
        [ ( "name", E.name name )
        , ( "comment", E.string comment )
        , ( "type", Type.encode tipe )
        ]


value : D.Decoder Error Value
value =
    D.pure Value
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "type" typeDecoder)



-- BINOP JSON


encodeBinop : ( Name, Binop ) -> E.Value
encodeBinop ( name, Binop comment tipe assoc prec ) =
    E.object
        [ ( "name", E.name name )
        , ( "comment", E.string comment )
        , ( "type", Type.encode tipe )
        , ( "associativity", encodeAssoc assoc )
        , ( "precedence", encodePrec prec )
        ]


binop : D.Decoder Error Binop
binop =
    D.pure Binop
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "type" typeDecoder)
        |> D.apply (D.field "associativity" assocDecoder)
        |> D.apply (D.field "precedence" precDecoder)



-- ASSOCIATIVITY JSON


encodeAssoc : Binop.Associativity -> E.Value
encodeAssoc assoc =
    case assoc of
        Binop.Left ->
            E.string "left"

        Binop.Non ->
            E.string "non"

        Binop.Right ->
            E.string "right"


assocDecoder : D.Decoder Error Binop.Associativity
assocDecoder =
    let
        left : String
        left =
            "left"

        non : String
        non =
            "non"

        right : String
        right =
            "right"
    in
    D.string
        |> D.bind
            (\str ->
                if str == left then
                    D.pure Binop.Left

                else if str == non then
                    D.pure Binop.Non

                else if str == right then
                    D.pure Binop.Right

                else
                    D.failure BadAssociativity
            )



-- PRECEDENCE JSON


encodePrec : Binop.Precedence -> E.Value
encodePrec n =
    E.int n


precDecoder : D.Decoder Error Binop.Precedence
precDecoder =
    D.int



-- FROM MODULE


fromModule : Can.Module -> Result E.Error Module
fromModule ((Can.Module _ exports docs _ _ _ _ _) as modul) =
    case exports of
        Can.ExportEverything region ->
            Err (E.ImplicitExposing region)

        Can.Export exportDict ->
            case docs of
                Src.NoDocs region ->
                    Err (E.NoDocs region)

                Src.YesDocs overview comments ->
                    parseOverview overview
                        |> Result.andThen (checkNames exportDict)
                        |> Result.andThen (\_ -> checkDefs exportDict overview (Dict.fromList compare comments) modul)



-- PARSE OVERVIEW


parseOverview : Src.Comment -> Result E.Error (List (A.Located Name.Name))
parseOverview (Src.Comment snippet) =
    case P.fromSnippet (chompOverview []) E.BadEnd snippet of
        Err err ->
            Err (E.SyntaxProblem err)

        Ok names ->
            Ok names


type alias Parser a =
    P.Parser E.SyntaxProblem a


chompOverview : List (A.Located Name.Name) -> Parser (List (A.Located Name.Name))
chompOverview names =
    chompUntilDocs
        |> P.bind
            (\isDocs ->
                if isDocs then
                    Space.chomp E.Space
                        |> P.bind (\_ -> P.bind chompOverview (chompDocs names))

                else
                    P.pure names
            )


chompDocs : List (A.Located Name.Name) -> Parser (List (A.Located Name.Name))
chompDocs names =
    P.addLocation
        (P.oneOf E.Name
            [ Var.lower E.Name
            , Var.upper E.Name
            , chompOperator
            ]
        )
        |> P.bind
            (\name ->
                Space.chomp E.Space
                    |> P.bind
                        (\_ ->
                            P.oneOfWithFallback
                                [ P.getPosition
                                    |> P.bind
                                        (\pos ->
                                            Space.checkIndent pos E.Comma
                                                |> P.bind
                                                    (\_ ->
                                                        word1 ',' E.Comma
                                                            |> P.bind
                                                                (\_ ->
                                                                    Space.chomp E.Space
                                                                        |> P.bind
                                                                            (\_ ->
                                                                                chompDocs (name :: names)
                                                                            )
                                                                )
                                                    )
                                        )
                                ]
                                (name :: names)
                        )
            )


chompOperator : Parser Name
chompOperator =
    word1 '(' E.Op
        |> P.bind
            (\_ ->
                Symbol.operator E.Op E.OpBad
                    |> P.bind
                        (\op ->
                            word1 ')' E.Op
                                |> P.fmap (\_ -> op)
                        )
            )



-- TODO add rule that @docs must be after newline in 0.20
--


chompUntilDocs : Parser Bool
chompUntilDocs =
    P.Parser
        (\(P.State src pos end indent row col) ->
            let
                ( ( isDocs, newPos ), ( newRow, newCol ) ) =
                    untilDocs src pos end row col

                newState : P.State
                newState =
                    P.State src newPos end indent newRow newCol
            in
            Ok (P.POk P.Consumed isDocs newState)
        )


untilDocs : String -> Int -> Int -> Row -> Col -> ( ( Bool, Int ), ( Row, Col ) )
untilDocs src pos end row col =
    if pos >= end then
        ( ( False, pos ), ( row, col ) )

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if word == '\n' then
            untilDocs src (pos + 1) end (row + 1) 1

        else
            let
                pos5 : Int
                pos5 =
                    pos + 5
            in
            if
                (pos5 <= end)
                    && (P.unsafeIndex src pos == '@')
                    && (P.unsafeIndex src (pos + 1) == 'd')
                    && (P.unsafeIndex src (pos + 2) == 'o')
                    && (P.unsafeIndex src (pos + 3) == 'c')
                    && (P.unsafeIndex src (pos + 4) == 's')
                    && (Var.getInnerWidth src pos5 end == 0)
            then
                ( ( True, pos5 ), ( row, col + 5 ) )

            else
                let
                    newPos : Int
                    newPos =
                        pos + P.getCharWidth word
                in
                untilDocs src newPos end row (col + 1)



-- CHECK NAMES


checkNames : Dict Name (A.Located Can.Export) -> List (A.Located Name) -> Result E.Error ()
checkNames exports names =
    let
        docs : DocNameRegions
        docs =
            List.foldl addName Dict.empty names

        loneExport : Name -> A.Located Can.Export -> Result.RResult i w E.NameProblem A.Region -> Result.RResult i w E.NameProblem A.Region
        loneExport name export_ _ =
            onlyInExports name export_

        checkBoth : Name -> A.Located Can.Export -> OneOrMore.OneOrMore A.Region -> Result.RResult i w E.NameProblem A.Region -> Result.RResult i w E.NameProblem A.Region
        checkBoth n _ r _ =
            isUnique n r

        loneDoc : Name -> OneOrMore.OneOrMore A.Region -> Result.RResult i w E.NameProblem A.Region -> Result.RResult i w E.NameProblem A.Region
        loneDoc name regions _ =
            onlyInDocs name regions
    in
    case Result.run (Dict.merge loneExport checkBoth loneDoc exports docs (Result.ok A.zero)) of
        ( _, Ok _ ) ->
            Ok ()

        ( _, Err es ) ->
            Err (E.NameProblems (OneOrMore.destruct NE.Nonempty es))


type alias DocNameRegions =
    Dict Name (OneOrMore.OneOrMore A.Region)


addName : A.Located Name -> DocNameRegions -> DocNameRegions
addName (A.At region name) dict =
    Utils.mapInsertWith compare OneOrMore.more name (OneOrMore.one region) dict


isUnique : Name -> OneOrMore.OneOrMore A.Region -> Result.RResult i w E.NameProblem A.Region
isUnique name regions =
    case regions of
        OneOrMore.One region ->
            Result.ok region

        OneOrMore.More left right ->
            let
                ( r1, r2 ) =
                    OneOrMore.getFirstTwo left right
            in
            Result.throw (E.NameDuplicate name r1 r2)


onlyInDocs : Name -> OneOrMore.OneOrMore A.Region -> Result.RResult i w E.NameProblem a
onlyInDocs name regions =
    isUnique name regions
        |> Result.bind
            (\region ->
                Result.throw (E.NameOnlyInDocs name region)
            )


onlyInExports : Name -> A.Located Can.Export -> Result.RResult i w E.NameProblem a
onlyInExports name (A.At region _) =
    Result.throw (E.NameOnlyInExports name region)



-- CHECK DEFS


checkDefs : Dict Name (A.Located Can.Export) -> Src.Comment -> Dict Name Src.Comment -> Can.Module -> Result E.Error Module
checkDefs exportDict overview comments (Can.Module name _ _ decls unions aliases infixes effects) =
    let
        types : Types
        types =
            gatherTypes decls Dict.empty

        info : Info
        info =
            Info comments types unions aliases infixes effects
    in
    case Result.run (Result.mapTraverseWithKey compare (checkExport info) exportDict) of
        ( _, Err problems ) ->
            Err (E.DefProblems (OneOrMore.destruct NE.Nonempty problems))

        ( _, Ok inserters ) ->
            Ok (Dict.foldr (\_ -> (<|)) (emptyModule name overview) inserters)


emptyModule : IO.Canonical -> Src.Comment -> Module
emptyModule (IO.Canonical _ name) (Src.Comment overview) =
    Module name (Json.fromComment overview) Dict.empty Dict.empty Dict.empty Dict.empty


type Info
    = Info (Dict Name.Name Src.Comment) (Dict Name.Name (Result A.Region Can.Type)) (Dict Name.Name Can.Union) (Dict Name.Name Can.Alias) (Dict Name.Name Can.Binop) Can.Effects


checkExport : Info -> Name -> A.Located Can.Export -> Result.RResult i w E.DefProblem (Module -> Module)
checkExport ((Info _ _ iUnions iAliases iBinops _) as info) name (A.At region export) =
    case export of
        Can.ExportValue ->
            getType name info
                |> Result.bind
                    (\tipe ->
                        getComment region name info
                            |> Result.bind
                                (\comment ->
                                    Result.ok
                                        (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                            Module
                                                mName
                                                mComment
                                                mUnions
                                                mAliases
                                                (Dict.insert compare name (Value comment tipe) mValues)
                                                mBinops
                                        )
                                )
                    )

        Can.ExportBinop ->
            let
                (Can.Binop_ assoc prec realName) =
                    Utils.find name iBinops
            in
            getType realName info
                |> Result.bind
                    (\tipe ->
                        getComment region realName info
                            |> Result.bind
                                (\comment ->
                                    Result.ok
                                        (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                            Module
                                                mName
                                                mComment
                                                mUnions
                                                mAliases
                                                mValues
                                                (Dict.insert compare name (Binop comment tipe assoc prec) mBinops)
                                        )
                                )
                    )

        Can.ExportAlias ->
            let
                (Can.Alias tvars tipe) =
                    Utils.find name iAliases
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                Module mName
                                    mComment
                                    mUnions
                                    (Dict.insert compare name (Alias comment tvars (Extract.fromType tipe)) mAliases)
                                    mValues
                                    mBinops
                            )
                    )

        Can.ExportUnionOpen ->
            let
                (Can.Union tvars ctors _ _) =
                    Utils.find name iUnions
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                Module mName
                                    mComment
                                    (Dict.insert compare name (Union comment tvars (List.map dector ctors)) mUnions)
                                    mAliases
                                    mValues
                                    mBinops
                            )
                    )

        Can.ExportUnionClosed ->
            let
                (Can.Union tvars _ _ _) =
                    Utils.find name iUnions
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                Module mName
                                    mComment
                                    (Dict.insert compare name (Union comment tvars []) mUnions)
                                    mAliases
                                    mValues
                                    mBinops
                            )
                    )

        Can.ExportPort ->
            getType name info
                |> Result.bind
                    (\tipe ->
                        getComment region name info
                            |> Result.bind
                                (\comment ->
                                    Result.ok
                                        (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                            Module mName
                                                mComment
                                                mUnions
                                                mAliases
                                                (Dict.insert compare name (Value comment tipe) mValues)
                                                mBinops
                                        )
                                )
                    )


getComment : A.Region -> Name.Name -> Info -> Result.RResult i w E.DefProblem Comment
getComment region name (Info iComments _ _ _ _ _) =
    case Dict.get name iComments of
        Nothing ->
            Result.throw (E.NoComment name region)

        Just (Src.Comment snippet) ->
            Result.ok (Json.fromComment snippet)


getType : Name.Name -> Info -> Result.RResult i w E.DefProblem Type.Type
getType name (Info _ iValues _ _ _ _) =
    case Utils.find name iValues of
        Err region ->
            Result.throw (E.NoAnnotation name region)

        Ok tipe ->
            Result.ok (Extract.fromType tipe)


dector : Can.Ctor -> ( Name, List Type.Type )
dector (Can.Ctor name _ _ args) =
    ( name, List.map Extract.fromType args )



-- GATHER TYPES


type alias Types =
    Dict Name.Name (Result A.Region Can.Type)


gatherTypes : Can.Decls -> Types -> Types
gatherTypes decls types =
    case decls of
        Can.Declare def subDecls ->
            gatherTypes subDecls (addDef types def)

        Can.DeclareRec def defs subDecls ->
            gatherTypes subDecls (List.foldl (flip addDef) (addDef types def) defs)

        Can.SaveTheEnvironment ->
            types


addDef : Types -> Can.Def -> Types
addDef types def =
    case def of
        Can.Def (A.At region name) _ _ ->
            Dict.insert compare name (Err region) types

        Can.TypedDef (A.At _ name) _ typedArgs _ resultType ->
            let
                tipe : Can.Type
                tipe =
                    List.foldr Can.TLambda resultType (List.map Tuple.second typedArgs)
            in
            Dict.insert compare name (Ok tipe) types



-- ENCODERS and DECODERS


jsonEncoder : Documentation -> Encode.Value
jsonEncoder =
    E.toJsonValue << encode


jsonDecoder : Decode.Decoder Documentation
jsonDecoder =
    Decode.map toDict (Decode.list jsonModuleDecoder)


jsonModuleEncoder : Module -> Encode.Value
jsonModuleEncoder (Module name comment unions aliases values binops) =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "comment", Encode.string comment )
        , ( "unions", E.assocListDict Encode.string jsonUnionEncoder unions )
        , ( "aliases", E.assocListDict Encode.string jsonAliasEncoder aliases )
        , ( "values", E.assocListDict Encode.string jsonValueEncoder values )
        , ( "binops", E.assocListDict Encode.string jsonBinopEncoder binops )
        ]


jsonModuleDecoder : Decode.Decoder Module
jsonModuleDecoder =
    Decode.map6 Module
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "unions" (D.assocListDict compare Decode.string jsonUnionDecoder))
        (Decode.field "aliases" (D.assocListDict compare Decode.string jsonAliasDecoder))
        (Decode.field "values" (D.assocListDict compare Decode.string jsonValueDecoder))
        (Decode.field "binops" (D.assocListDict compare Decode.string jsonBinopDecoder))


jsonUnionEncoder : Union -> Encode.Value
jsonUnionEncoder (Union comment args cases) =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "args", Encode.list Encode.string args )
        , ( "cases", Encode.list (E.jsonPair Encode.string (Encode.list Type.jsonEncoder)) cases )
        ]


jsonUnionDecoder : Decode.Decoder Union
jsonUnionDecoder =
    Decode.map3 Union
        (Decode.field "comment" Decode.string)
        (Decode.field "args" (Decode.list Decode.string))
        (Decode.field "cases" (Decode.list (D.jsonPair Decode.string (Decode.list Type.jsonDecoder))))


jsonAliasEncoder : Alias -> Encode.Value
jsonAliasEncoder (Alias comment args type_) =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "args", Encode.list Encode.string args )
        , ( "type", Type.jsonEncoder type_ )
        ]


jsonAliasDecoder : Decode.Decoder Alias
jsonAliasDecoder =
    Decode.map3 Alias
        (Decode.field "comment" Decode.string)
        (Decode.field "args" (Decode.list Decode.string))
        (Decode.field "type" Type.jsonDecoder)


jsonValueEncoder : Value -> Encode.Value
jsonValueEncoder (Value comment type_) =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "type", Type.jsonEncoder type_ )
        ]


jsonValueDecoder : Decode.Decoder Value
jsonValueDecoder =
    Decode.map2 Value
        (Decode.field "comment" Decode.string)
        (Decode.field "type" Type.jsonDecoder)


jsonBinopEncoder : Binop -> Encode.Value
jsonBinopEncoder (Binop comment type_ associativity precedence) =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "type", Type.jsonEncoder type_ )
        , ( "associativity", Binop.associativityEncoder associativity )
        , ( "precedence", Binop.precedenceEncoder precedence )
        ]


jsonBinopDecoder : Decode.Decoder Binop
jsonBinopDecoder =
    Decode.map4 Binop
        (Decode.field "comment" Decode.string)
        (Decode.field "type" Type.jsonDecoder)
        (Decode.field "associativity" Binop.associativityDecoder)
        (Decode.field "precedence" Binop.precedenceDecoder)
