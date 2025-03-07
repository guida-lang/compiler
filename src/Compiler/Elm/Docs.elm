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
    , jsonCodec
    , moduleCodec
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
import Compiler.Serialize as S
import Data.Map as Dict exposing (Dict)
import Serialize exposing (Codec)
import System.TypeCheck.IO as IO
import Utils.Main as Utils



-- DOCUMENTATION


type alias Documentation =
    Dict String Name Module


type Module
    = Module Name Comment (Dict String Name Union) (Dict String Name Alias) (Dict String Name Value) (Dict String Name Binop)


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
    E.list encodeModule (Dict.values compare docs)


encodeModule : Module -> E.Value
encodeModule (Module name comment unions aliases values binops) =
    E.object
        [ ( "name", ModuleName.encode name )
        , ( "comment", E.string comment )
        , ( "unions", E.list encodeUnion (Dict.toList compare unions) )
        , ( "aliases", E.list encodeAlias (Dict.toList compare aliases) )
        , ( "values", E.list encodeValue (Dict.toList compare values) )
        , ( "binops", E.list encodeBinop (Dict.toList compare binops) )
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
    Dict.fromList identity (List.map toDictHelp modules)


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


dictDecoder : D.Decoder Error a -> D.Decoder Error (Dict String Name a)
dictDecoder entryDecoder =
    D.fmap (Dict.fromList identity) (D.list (named entryDecoder))


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
                        |> Result.andThen (\_ -> checkDefs exportDict overview (Dict.fromList identity comments) modul)



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
chompOverview =
    P.loop chompOverviewHelp


chompOverviewHelp : List (A.Located Name.Name) -> Parser (P.Step (List (A.Located Name.Name)) (List (A.Located Name.Name)))
chompOverviewHelp names =
    chompUntilDocs
        |> P.bind
            (\isDocs ->
                if isDocs then
                    Space.chomp E.Space
                        |> P.bind (\_ -> chompDocs names)
                        |> P.fmap P.Loop

                else
                    P.pure (P.Done names)
            )


chompDocs : List (A.Located Name.Name) -> Parser (List (A.Located Name.Name))
chompDocs =
    P.loop chompDocsHelp


chompDocsHelp : List (A.Located Name.Name) -> Parser (P.Step (List (A.Located Name.Name)) (List (A.Located Name.Name)))
chompDocsHelp names =
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
                                                                        |> P.fmap (\_ -> P.Loop (name :: names))
                                                                )
                                                    )
                                        )
                                ]
                                (P.Done (name :: names))
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
            P.Cok isDocs newState
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


checkNames : Dict String Name (A.Located Can.Export) -> List (A.Located Name) -> Result E.Error ()
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
    case Result.run (Dict.merge compare loneExport checkBoth loneDoc exports docs (Result.ok A.zero)) of
        ( _, Ok _ ) ->
            Ok ()

        ( _, Err es ) ->
            Err (E.NameProblems (OneOrMore.destruct NE.Nonempty es))


type alias DocNameRegions =
    Dict String Name (OneOrMore.OneOrMore A.Region)


addName : A.Located Name -> DocNameRegions -> DocNameRegions
addName (A.At region name) dict =
    Utils.mapInsertWith identity OneOrMore.more name (OneOrMore.one region) dict


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


checkDefs : Dict String Name (A.Located Can.Export) -> Src.Comment -> Dict String Name Src.Comment -> Can.Module -> Result E.Error Module
checkDefs exportDict overview comments (Can.Module name _ _ decls unions aliases infixes effects) =
    let
        types : Types
        types =
            gatherTypes decls Dict.empty

        info : Info
        info =
            Info comments types unions aliases infixes effects
    in
    case Result.run (Result.mapTraverseWithKey identity compare (checkExport info) exportDict) of
        ( _, Err problems ) ->
            Err (E.DefProblems (OneOrMore.destruct NE.Nonempty problems))

        ( _, Ok inserters ) ->
            Ok (Dict.foldr compare (\_ -> (<|)) (emptyModule name overview) inserters)


emptyModule : IO.Canonical -> Src.Comment -> Module
emptyModule (IO.Canonical _ name) (Src.Comment overview) =
    Module name (Json.fromComment overview) Dict.empty Dict.empty Dict.empty Dict.empty


type Info
    = Info (Dict String Name.Name Src.Comment) (Dict String Name.Name (Result A.Region Can.Type)) (Dict String Name.Name Can.Union) (Dict String Name.Name Can.Alias) (Dict String Name.Name Can.Binop) Can.Effects


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
                                                (Dict.insert identity name (Value comment tipe) mValues)
                                                mBinops
                                        )
                                )
                    )

        Can.ExportBinop ->
            let
                (Can.Binop_ assoc prec realName) =
                    Utils.find identity name iBinops
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
                                                (Dict.insert identity name (Binop comment tipe assoc prec) mBinops)
                                        )
                                )
                    )

        Can.ExportAlias ->
            let
                (Can.Alias tvars tipe) =
                    Utils.find identity name iAliases
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                Module mName
                                    mComment
                                    mUnions
                                    (Dict.insert identity name (Alias comment tvars (Extract.fromType tipe)) mAliases)
                                    mValues
                                    mBinops
                            )
                    )

        Can.ExportUnionOpen ->
            let
                (Can.Union tvars ctors _ _) =
                    Utils.find identity name iUnions
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                Module mName
                                    mComment
                                    (Dict.insert identity name (Union comment tvars (List.map dector ctors)) mUnions)
                                    mAliases
                                    mValues
                                    mBinops
                            )
                    )

        Can.ExportUnionClosed ->
            let
                (Can.Union tvars _ _ _) =
                    Utils.find identity name iUnions
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(Module mName mComment mUnions mAliases mValues mBinops) ->
                                Module mName
                                    mComment
                                    (Dict.insert identity name (Union comment tvars []) mUnions)
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
                                                (Dict.insert identity name (Value comment tipe) mValues)
                                                mBinops
                                        )
                                )
                    )


getComment : A.Region -> Name.Name -> Info -> Result.RResult i w E.DefProblem Comment
getComment region name (Info iComments _ _ _ _ _) =
    case Dict.get identity name iComments of
        Nothing ->
            Result.throw (E.NoComment name region)

        Just (Src.Comment snippet) ->
            Result.ok (Json.fromComment snippet)


getType : Name.Name -> Info -> Result.RResult i w E.DefProblem Type.Type
getType name (Info _ iValues _ _ _ _) =
    case Utils.find identity name iValues of
        Err region ->
            Result.throw (E.NoAnnotation name region)

        Ok tipe ->
            Result.ok (Extract.fromType tipe)


dector : Can.Ctor -> ( Name, List Type.Type )
dector (Can.Ctor name _ _ args) =
    ( name, List.map Extract.fromType args )



-- GATHER TYPES


type alias Types =
    Dict String Name.Name (Result A.Region Can.Type)


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
            Dict.insert identity name (Err region) types

        Can.TypedDef (A.At _ name) _ typedArgs _ resultType ->
            let
                tipe : Can.Type
                tipe =
                    List.foldr Can.TLambda resultType (List.map Tuple.second typedArgs)
            in
            Dict.insert identity name (Ok tipe) types



-- ENCODERS and DECODERS


jsonCodec : Codec e Documentation
jsonCodec =
    S.assocListDict identity compare Serialize.string moduleCodec


moduleCodec : Codec e Module
moduleCodec =
    Serialize.customType
        (\moduleEncoder (Module name comment unions aliases values binops) ->
            moduleEncoder name comment unions aliases values binops
        )
        |> Serialize.variant6
            Module
            Serialize.string
            Serialize.string
            (S.assocListDict identity compare Serialize.string unionCodec)
            (S.assocListDict identity compare Serialize.string aliasCodec)
            (S.assocListDict identity compare Serialize.string valueCodec)
            (S.assocListDict identity compare Serialize.string binopCodec)
        |> Serialize.finishCustomType


unionCodec : Codec e Union
unionCodec =
    Serialize.customType
        (\unionEncoder (Union comment args cases) ->
            unionEncoder comment args cases
        )
        |> Serialize.variant3
            Union
            Serialize.string
            (Serialize.list Serialize.string)
            (Serialize.list (Serialize.tuple Serialize.string (Serialize.list Type.codec)))
        |> Serialize.finishCustomType


aliasCodec : Codec e Alias
aliasCodec =
    Serialize.customType
        (\aliasEncoder (Alias comment args type_) ->
            aliasEncoder comment args type_
        )
        |> Serialize.variant3 Alias Serialize.string (Serialize.list Serialize.string) Type.codec
        |> Serialize.finishCustomType


valueCodec : Codec e Value
valueCodec =
    Serialize.customType
        (\valueEncoder (Value comment type_) ->
            valueEncoder comment type_
        )
        |> Serialize.variant2 Value Serialize.string Type.codec
        |> Serialize.finishCustomType


binopCodec : Codec e Binop
binopCodec =
    Serialize.customType
        (\binopEncoder (Binop comment type_ associativity precedence) ->
            binopEncoder comment type_ associativity precedence
        )
        |> Serialize.variant4 Binop Serialize.string Type.codec Binop.associativityCodec Binop.precedenceCodec
        |> Serialize.finishCustomType
