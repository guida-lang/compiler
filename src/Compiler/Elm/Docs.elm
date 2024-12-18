module Compiler.Elm.Docs exposing
    ( CED_Alias(..)
    , CED_Binop(..)
    , CED_Comment
    , CED_Module(..)
    , CED_Union(..)
    , CED_Value(..)
    , Documentation
    , Error(..)
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
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.Compiler.Type as Type
import Compiler.Elm.Compiler.Type.Extract as Extract
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Json.String as Json
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Docs as E
import Compiler.Reporting.Result as Result
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T
import Utils.Main as Utils



-- DOCUMENTATION


type alias Documentation =
    Dict String T.CDN_Name CED_Module


type CED_Module
    = CED_Module T.CDN_Name CED_Comment (Dict String T.CDN_Name CED_Union) (Dict String T.CDN_Name CED_Alias) (Dict String T.CDN_Name CED_Value) (Dict String T.CDN_Name CED_Binop)


type alias CED_Comment =
    String


type CED_Alias
    = CED_Alias CED_Comment (List T.CDN_Name) Type.CECT_Type


type CED_Union
    = CED_Union CED_Comment (List T.CDN_Name) (List ( T.CDN_Name, List Type.CECT_Type ))


type CED_Value
    = CED_Value CED_Comment Type.CECT_Type


type CED_Binop
    = CED_Binop CED_Comment Type.CECT_Type T.CASTUB_Associativity T.CASTUB_Precedence



-- JSON


encode : Documentation -> E.Value
encode docs =
    E.list encodeModule (Dict.values compare docs)


encodeModule : CED_Module -> E.Value
encodeModule (CED_Module name comment unions aliases values binops) =
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


toDict : List CED_Module -> Documentation
toDict modules =
    Dict.fromList identity (List.map toDictHelp modules)


toDictHelp : CED_Module -> ( T.CDN_Name, CED_Module )
toDictHelp ((CED_Module name _ _ _ _ _) as modul) =
    ( name, modul )


moduleDecoder : D.Decoder Error CED_Module
moduleDecoder =
    D.pure CED_Module
        |> D.apply (D.field "name" moduleNameDecoder)
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "unions" (dictDecoder union))
        |> D.apply (D.field "aliases" (dictDecoder alias_))
        |> D.apply (D.field "values" (dictDecoder value))
        |> D.apply (D.field "binops" (dictDecoder binop))


dictDecoder : D.Decoder Error a -> D.Decoder Error (Dict String T.CDN_Name a)
dictDecoder entryDecoder =
    D.fmap (Dict.fromList identity) (D.list (named entryDecoder))


named : D.Decoder Error a -> D.Decoder Error ( T.CDN_Name, a )
named entryDecoder =
    D.pure Tuple.pair
        |> D.apply (D.field "name" nameDecoder)
        |> D.apply entryDecoder


nameDecoder : D.Decoder e T.CDN_Name
nameDecoder =
    D.string


moduleNameDecoder : D.Decoder Error T.CEMN_Raw
moduleNameDecoder =
    D.mapError (always BadModuleName) ModuleName.decoder


typeDecoder : D.Decoder Error Type.CECT_Type
typeDecoder =
    D.mapError (always BadType) Type.decoder



-- UNION JSON


encodeUnion : ( T.CDN_Name, CED_Union ) -> E.Value
encodeUnion ( name, CED_Union comment args cases ) =
    E.object
        [ ( "name", E.name name )
        , ( "comment", E.string comment )
        , ( "args", E.list E.name args )
        , ( "cases", E.list encodeCase cases )
        ]


union : D.Decoder Error CED_Union
union =
    D.pure CED_Union
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "args" (D.list nameDecoder))
        |> D.apply (D.field "cases" (D.list caseDecoder))


encodeCase : ( T.CDN_Name, List Type.CECT_Type ) -> E.Value
encodeCase ( tag, args ) =
    E.list identity [ E.name tag, E.list Type.encode args ]


caseDecoder : D.Decoder Error ( T.CDN_Name, List Type.CECT_Type )
caseDecoder =
    D.pair nameDecoder (D.list typeDecoder)



-- ALIAS JSON


encodeAlias : ( T.CDN_Name, CED_Alias ) -> E.Value
encodeAlias ( name, CED_Alias comment args tipe ) =
    E.object
        [ ( "name", E.name name )
        , ( "comment", E.string comment )
        , ( "args", E.list E.name args )
        , ( "type", Type.encode tipe )
        ]


alias_ : D.Decoder Error CED_Alias
alias_ =
    D.pure CED_Alias
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "args" (D.list nameDecoder))
        |> D.apply (D.field "type" typeDecoder)



-- VALUE JSON


encodeValue : ( T.CDN_Name, CED_Value ) -> E.Value
encodeValue ( name, CED_Value comment tipe ) =
    E.object
        [ ( "name", E.name name )
        , ( "comment", E.string comment )
        , ( "type", Type.encode tipe )
        ]


value : D.Decoder Error CED_Value
value =
    D.pure CED_Value
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "type" typeDecoder)



-- BINOP JSON


encodeBinop : ( T.CDN_Name, CED_Binop ) -> E.Value
encodeBinop ( name, CED_Binop comment tipe assoc prec ) =
    E.object
        [ ( "name", E.name name )
        , ( "comment", E.string comment )
        , ( "type", Type.encode tipe )
        , ( "associativity", encodeAssoc assoc )
        , ( "precedence", encodePrec prec )
        ]


binop : D.Decoder Error CED_Binop
binop =
    D.pure CED_Binop
        |> D.apply (D.field "comment" D.string)
        |> D.apply (D.field "type" typeDecoder)
        |> D.apply (D.field "associativity" assocDecoder)
        |> D.apply (D.field "precedence" precDecoder)



-- ASSOCIATIVITY JSON


encodeAssoc : T.CASTUB_Associativity -> E.Value
encodeAssoc assoc =
    case assoc of
        T.CASTUB_Left ->
            E.string "left"

        T.CASTUB_Non ->
            E.string "non"

        T.CASTUB_Right ->
            E.string "right"


assocDecoder : D.Decoder Error T.CASTUB_Associativity
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
                    D.pure T.CASTUB_Left

                else if str == non then
                    D.pure T.CASTUB_Non

                else if str == right then
                    D.pure T.CASTUB_Right

                else
                    D.failure BadAssociativity
            )



-- PRECEDENCE JSON


encodePrec : T.CASTUB_Precedence -> E.Value
encodePrec n =
    E.int n


precDecoder : D.Decoder Error T.CASTUB_Precedence
precDecoder =
    D.int



-- FROM MODULE


fromModule : Can.Module -> Result E.CRED_Error CED_Module
fromModule ((Can.Module _ exports docs _ _ _ _ _) as modul) =
    case exports of
        Can.ExportEverything region ->
            Err (E.CRED_ImplicitExposing region)

        Can.Export exportDict ->
            case docs of
                T.CASTS_NoDocs region ->
                    Err (E.CRED_NoDocs region)

                T.CASTS_YesDocs overview comments ->
                    parseOverview overview
                        |> Result.andThen (checkNames exportDict)
                        |> Result.andThen (\_ -> checkDefs exportDict overview (Dict.fromList identity comments) modul)



-- PARSE OVERVIEW


parseOverview : T.CASTS_Comment -> Result E.CRED_Error (List (T.CRA_Located T.CDN_Name))
parseOverview (T.CASTS_Comment snippet) =
    case P.fromSnippet (chompOverview []) E.CRED_BadEnd snippet of
        Err err ->
            Err (E.CRED_SyntaxProblem err)

        Ok names ->
            Ok names


type alias Parser a =
    P.Parser E.CRED_SyntaxProblem a


chompOverview : List (T.CRA_Located T.CDN_Name) -> Parser (List (T.CRA_Located T.CDN_Name))
chompOverview names =
    chompUntilDocs
        |> P.bind
            (\isDocs ->
                if isDocs then
                    Space.chomp E.CRED_Space
                        |> P.bind (\_ -> P.bind chompOverview (chompDocs names))

                else
                    P.pure names
            )


chompDocs : List (T.CRA_Located T.CDN_Name) -> Parser (List (T.CRA_Located T.CDN_Name))
chompDocs names =
    P.addLocation
        (P.oneOf E.CRED_Name
            [ Var.lower E.CRED_Name
            , Var.upper E.CRED_Name
            , chompOperator
            ]
        )
        |> P.bind
            (\name ->
                Space.chomp E.CRED_Space
                    |> P.bind
                        (\_ ->
                            P.oneOfWithFallback
                                [ P.getPosition
                                    |> P.bind
                                        (\pos ->
                                            Space.checkIndent pos E.CRED_Comma
                                                |> P.bind
                                                    (\_ ->
                                                        P.word1 ',' E.CRED_Comma
                                                            |> P.bind
                                                                (\_ ->
                                                                    Space.chomp E.CRED_Space
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


chompOperator : Parser T.CDN_Name
chompOperator =
    P.word1 '(' E.CRED_Op
        |> P.bind
            (\_ ->
                Symbol.operator E.CRED_Op E.CRED_OpBad
                    |> P.bind
                        (\op ->
                            P.word1 ')' E.CRED_Op
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


untilDocs : String -> Int -> Int -> T.CPP_Row -> T.CPP_Col -> ( ( Bool, Int ), ( T.CPP_Row, T.CPP_Col ) )
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


checkNames : Dict String T.CDN_Name (T.CRA_Located Can.Export) -> List (T.CRA_Located T.CDN_Name) -> Result E.CRED_Error ()
checkNames exports names =
    let
        docs : DocNameRegions
        docs =
            List.foldl addName Dict.empty names

        loneExport : T.CDN_Name -> T.CRA_Located Can.Export -> Result.RResult i w E.CRED_NameProblem T.CRA_Region -> Result.RResult i w E.CRED_NameProblem T.CRA_Region
        loneExport name export_ _ =
            onlyInExports name export_

        checkBoth : T.CDN_Name -> T.CRA_Located Can.Export -> OneOrMore.OneOrMore T.CRA_Region -> Result.RResult i w E.CRED_NameProblem T.CRA_Region -> Result.RResult i w E.CRED_NameProblem T.CRA_Region
        checkBoth n _ r _ =
            isUnique n r

        loneDoc : T.CDN_Name -> OneOrMore.OneOrMore T.CRA_Region -> Result.RResult i w E.CRED_NameProblem T.CRA_Region -> Result.RResult i w E.CRED_NameProblem T.CRA_Region
        loneDoc name regions _ =
            onlyInDocs name regions
    in
    case Result.run (Dict.merge compare loneExport checkBoth loneDoc exports docs (Result.ok A.zero)) of
        ( _, Ok _ ) ->
            Ok ()

        ( _, Err es ) ->
            Err (E.CRED_NameProblems (OneOrMore.destruct NE.Nonempty es))


type alias DocNameRegions =
    Dict String T.CDN_Name (OneOrMore.OneOrMore T.CRA_Region)


addName : T.CRA_Located T.CDN_Name -> DocNameRegions -> DocNameRegions
addName (T.CRA_At region name) dict =
    Utils.mapInsertWith identity OneOrMore.more name (OneOrMore.one region) dict


isUnique : T.CDN_Name -> OneOrMore.OneOrMore T.CRA_Region -> Result.RResult i w E.CRED_NameProblem T.CRA_Region
isUnique name regions =
    case regions of
        OneOrMore.One region ->
            Result.ok region

        OneOrMore.More left right ->
            let
                ( r1, r2 ) =
                    OneOrMore.getFirstTwo left right
            in
            Result.throw (E.CRED_NameDuplicate name r1 r2)


onlyInDocs : T.CDN_Name -> OneOrMore.OneOrMore T.CRA_Region -> Result.RResult i w E.CRED_NameProblem a
onlyInDocs name regions =
    isUnique name regions
        |> Result.bind
            (\region ->
                Result.throw (E.CRED_NameOnlyInDocs name region)
            )


onlyInExports : T.CDN_Name -> T.CRA_Located Can.Export -> Result.RResult i w E.CRED_NameProblem a
onlyInExports name (T.CRA_At region _) =
    Result.throw (E.CRED_NameOnlyInExports name region)



-- CHECK DEFS


checkDefs : Dict String T.CDN_Name (T.CRA_Located Can.Export) -> T.CASTS_Comment -> Dict String T.CDN_Name T.CASTS_Comment -> Can.Module -> Result E.CRED_Error CED_Module
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
            Err (E.CRED_DefProblems (OneOrMore.destruct NE.Nonempty problems))

        ( _, Ok inserters ) ->
            Ok (Dict.foldr compare (\_ -> (<|)) (emptyModule name overview) inserters)


emptyModule : T.CEMN_Canonical -> T.CASTS_Comment -> CED_Module
emptyModule (T.CEMN_Canonical _ name) (T.CASTS_Comment overview) =
    CED_Module name (Json.fromComment overview) Dict.empty Dict.empty Dict.empty Dict.empty


type Info
    = Info (Dict String T.CDN_Name T.CASTS_Comment) (Dict String T.CDN_Name (Result T.CRA_Region T.CASTC_Type)) (Dict String T.CDN_Name T.CASTC_Union) (Dict String T.CDN_Name T.CASTC_Alias) (Dict String T.CDN_Name Can.Binop) Can.Effects


checkExport : Info -> T.CDN_Name -> T.CRA_Located Can.Export -> Result.RResult i w E.CRED_DefProblem (CED_Module -> CED_Module)
checkExport ((Info _ _ iUnions iAliases iBinops _) as info) name (T.CRA_At region export) =
    case export of
        Can.ExportValue ->
            getType name info
                |> Result.bind
                    (\tipe ->
                        getComment region name info
                            |> Result.bind
                                (\comment ->
                                    Result.ok
                                        (\(CED_Module mName mComment mUnions mAliases mValues mBinops) ->
                                            CED_Module
                                                mName
                                                mComment
                                                mUnions
                                                mAliases
                                                (Dict.insert identity name (CED_Value comment tipe) mValues)
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
                                        (\(CED_Module mName mComment mUnions mAliases mValues mBinops) ->
                                            CED_Module
                                                mName
                                                mComment
                                                mUnions
                                                mAliases
                                                mValues
                                                (Dict.insert identity name (CED_Binop comment tipe assoc prec) mBinops)
                                        )
                                )
                    )

        Can.ExportAlias ->
            let
                (T.CASTC_Alias tvars tipe) =
                    Utils.find identity name iAliases
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(CED_Module mName mComment mUnions mAliases mValues mBinops) ->
                                CED_Module mName
                                    mComment
                                    mUnions
                                    (Dict.insert identity name (CED_Alias comment tvars (Extract.fromType tipe)) mAliases)
                                    mValues
                                    mBinops
                            )
                    )

        Can.ExportUnionOpen ->
            let
                (T.CASTC_Union tvars ctors _ _) =
                    Utils.find identity name iUnions
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(CED_Module mName mComment mUnions mAliases mValues mBinops) ->
                                CED_Module mName
                                    mComment
                                    (Dict.insert identity name (CED_Union comment tvars (List.map dector ctors)) mUnions)
                                    mAliases
                                    mValues
                                    mBinops
                            )
                    )

        Can.ExportUnionClosed ->
            let
                (T.CASTC_Union tvars _ _ _) =
                    Utils.find identity name iUnions
            in
            getComment region name info
                |> Result.bind
                    (\comment ->
                        Result.ok
                            (\(CED_Module mName mComment mUnions mAliases mValues mBinops) ->
                                CED_Module mName
                                    mComment
                                    (Dict.insert identity name (CED_Union comment tvars []) mUnions)
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
                                        (\(CED_Module mName mComment mUnions mAliases mValues mBinops) ->
                                            CED_Module mName
                                                mComment
                                                mUnions
                                                mAliases
                                                (Dict.insert identity name (CED_Value comment tipe) mValues)
                                                mBinops
                                        )
                                )
                    )


getComment : T.CRA_Region -> T.CDN_Name -> Info -> Result.RResult i w E.CRED_DefProblem CED_Comment
getComment region name (Info iComments _ _ _ _ _) =
    case Dict.get identity name iComments of
        Nothing ->
            Result.throw (E.CRED_NoComment name region)

        Just (T.CASTS_Comment snippet) ->
            Result.ok (Json.fromComment snippet)


getType : T.CDN_Name -> Info -> Result.RResult i w E.CRED_DefProblem Type.CECT_Type
getType name (Info _ iValues _ _ _ _) =
    case Utils.find identity name iValues of
        Err region ->
            Result.throw (E.CRED_NoAnnotation name region)

        Ok tipe ->
            Result.ok (Extract.fromType tipe)


dector : T.CASTC_Ctor -> ( T.CDN_Name, List Type.CECT_Type )
dector (T.CASTC_Ctor name _ _ args) =
    ( name, List.map Extract.fromType args )



-- GATHER TYPES


type alias Types =
    Dict String T.CDN_Name (Result T.CRA_Region T.CASTC_Type)


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
        Can.Def (T.CRA_At region name) _ _ ->
            Dict.insert identity name (Err region) types

        Can.TypedDef (T.CRA_At _ name) _ typedArgs _ resultType ->
            let
                tipe : T.CASTC_Type
                tipe =
                    List.foldr T.CASTC_TLambda resultType (List.map Tuple.second typedArgs)
            in
            Dict.insert identity name (Ok tipe) types



-- ENCODERS and DECODERS


jsonEncoder : Documentation -> Encode.Value
jsonEncoder =
    E.toJsonValue << encode


jsonDecoder : Decode.Decoder Documentation
jsonDecoder =
    Decode.map toDict (Decode.list jsonModuleDecoder)


jsonModuleEncoder : CED_Module -> Encode.Value
jsonModuleEncoder (CED_Module name comment unions aliases values binops) =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "comment", Encode.string comment )
        , ( "unions", E.assocListDict compare Encode.string jsonUnionEncoder unions )
        , ( "aliases", E.assocListDict compare Encode.string jsonAliasEncoder aliases )
        , ( "values", E.assocListDict compare Encode.string jsonValueEncoder values )
        , ( "binops", E.assocListDict compare Encode.string jsonBinopEncoder binops )
        ]


jsonModuleDecoder : Decode.Decoder CED_Module
jsonModuleDecoder =
    Decode.map6 CED_Module
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "unions" (D.assocListDict identity Decode.string jsonUnionDecoder))
        (Decode.field "aliases" (D.assocListDict identity Decode.string jsonAliasDecoder))
        (Decode.field "values" (D.assocListDict identity Decode.string jsonValueDecoder))
        (Decode.field "binops" (D.assocListDict identity Decode.string jsonBinopDecoder))


jsonUnionEncoder : CED_Union -> Encode.Value
jsonUnionEncoder (CED_Union comment args cases) =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "args", Encode.list Encode.string args )
        , ( "cases", Encode.list (E.jsonPair Encode.string (Encode.list Type.jsonEncoder)) cases )
        ]


jsonUnionDecoder : Decode.Decoder CED_Union
jsonUnionDecoder =
    Decode.map3 CED_Union
        (Decode.field "comment" Decode.string)
        (Decode.field "args" (Decode.list Decode.string))
        (Decode.field "cases" (Decode.list (D.jsonPair Decode.string (Decode.list Type.jsonDecoder))))


jsonAliasEncoder : CED_Alias -> Encode.Value
jsonAliasEncoder (CED_Alias comment args type_) =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "args", Encode.list Encode.string args )
        , ( "type", Type.jsonEncoder type_ )
        ]


jsonAliasDecoder : Decode.Decoder CED_Alias
jsonAliasDecoder =
    Decode.map3 CED_Alias
        (Decode.field "comment" Decode.string)
        (Decode.field "args" (Decode.list Decode.string))
        (Decode.field "type" Type.jsonDecoder)


jsonValueEncoder : CED_Value -> Encode.Value
jsonValueEncoder (CED_Value comment type_) =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "type", Type.jsonEncoder type_ )
        ]


jsonValueDecoder : Decode.Decoder CED_Value
jsonValueDecoder =
    Decode.map2 CED_Value
        (Decode.field "comment" Decode.string)
        (Decode.field "type" Type.jsonDecoder)


jsonBinopEncoder : CED_Binop -> Encode.Value
jsonBinopEncoder (CED_Binop comment type_ associativity precedence) =
    Encode.object
        [ ( "comment", Encode.string comment )
        , ( "type", Type.jsonEncoder type_ )
        , ( "associativity", Binop.associativityEncoder associativity )
        , ( "precedence", Binop.precedenceEncoder precedence )
        ]


jsonBinopDecoder : Decode.Decoder CED_Binop
jsonBinopDecoder =
    Decode.map4 CED_Binop
        (Decode.field "comment" Decode.string)
        (Decode.field "type" Type.jsonDecoder)
        (Decode.field "associativity" Binop.associativityDecoder)
        (Decode.field "precedence" Binop.precedenceDecoder)
