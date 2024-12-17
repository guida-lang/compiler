module Compiler.Elm.Kernel exposing
    ( Content(..)
    , Foreigns
    , chunkDecoder
    , chunkEncoder
    , countFields
    , fromByteString
    )

import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Parse.Module as Module
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Variable as Var
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T
import Utils.Crash exposing (crash)



-- COUNT FIELDS


countFields : List T.CEK_Chunk -> Dict String T.CDN_Name Int
countFields chunks =
    List.foldr addField Dict.empty chunks


addField : T.CEK_Chunk -> Dict String T.CDN_Name Int -> Dict String T.CDN_Name Int
addField chunk fields =
    case chunk of
        T.CEK_JS _ ->
            fields

        T.CEK_ElmVar _ _ ->
            fields

        T.CEK_JsVar _ _ ->
            fields

        T.CEK_ElmField f ->
            Dict.update identity
                f
                (Maybe.map ((+) 1)
                    >> Maybe.withDefault 1
                    >> Just
                )
                fields

        T.CEK_JsField _ ->
            fields

        T.CEK_JsEnum _ ->
            fields

        T.CEK_Debug ->
            fields

        T.CEK_Prod ->
            fields



-- FROM FILE


type Content
    = Content (List T.CASTS_Import) (List T.CEK_Chunk)


type alias Foreigns =
    Dict String T.CEMN_Raw T.CEP_Name


fromByteString : T.CEP_Name -> Foreigns -> String -> Maybe Content
fromByteString pkg foreigns bytes =
    case P.fromByteString (parser pkg foreigns) toError bytes of
        Ok content ->
            Just content

        Err () ->
            Nothing


parser : T.CEP_Name -> Foreigns -> P.Parser () Content
parser pkg foreigns =
    P.word2 '/' '*' toError
        |> P.bind (\_ -> Space.chomp ignoreError)
        |> P.bind (\_ -> Space.checkFreshLine toError)
        |> P.bind (\_ -> P.specialize ignoreError (Module.chompImports []))
        |> P.bind
            (\imports ->
                P.word2 '*' '/' toError
                    |> P.bind (\_ -> parseChunks (toVarTable pkg foreigns imports) Dict.empty Dict.empty)
                    |> P.fmap (\chunks -> Content imports chunks)
            )


toError : T.CPP_Row -> T.CPP_Col -> ()
toError _ _ =
    ()


ignoreError : a -> T.CPP_Row -> T.CPP_Col -> ()
ignoreError _ _ _ =
    ()



-- PARSE CHUNKS


parseChunks : VarTable -> Enums -> Fields -> P.Parser () (List T.CEK_Chunk)
parseChunks vtable enums fields =
    P.Parser
        (\(P.State src pos end indent row col) ->
            let
                ( ( chunks, newPos ), ( newRow, newCol ) ) =
                    chompChunks vtable enums fields src pos end row col pos []
            in
            if newPos == end then
                Ok (P.POk P.Consumed chunks (P.State src newPos end indent newRow newCol))

            else
                Err (P.PErr P.Consumed row col toError)
        )


chompChunks : VarTable -> Enums -> Fields -> String -> Int -> Int -> T.CPP_Row -> T.CPP_Col -> Int -> List T.CEK_Chunk -> ( ( List T.CEK_Chunk, Int ), ( T.CPP_Row, T.CPP_Col ) )
chompChunks vs es fs src pos end row col lastPos revChunks =
    if pos >= end then
        let
            js : String
            js =
                toByteString src lastPos end
        in
        ( ( List.reverse (T.CEK_JS js :: revChunks), pos ), ( row, col ) )

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if word == '_' then
            let
                pos1 : Int
                pos1 =
                    pos + 1

                pos3 : Int
                pos3 =
                    pos + 3
            in
            if pos3 <= end && P.unsafeIndex src pos1 == '_' then
                let
                    js : String
                    js =
                        toByteString src lastPos pos
                in
                chompTag vs es fs src pos3 end row (col + 3) (T.CEK_JS js :: revChunks)

            else
                chompChunks vs es fs src pos1 end row (col + 1) lastPos revChunks

        else if word == '\n' then
            chompChunks vs es fs src (pos + 1) end (row + 1) 1 lastPos revChunks

        else
            let
                newPos : Int
                newPos =
                    pos + P.getCharWidth word
            in
            chompChunks vs es fs src newPos end row (col + 1) lastPos revChunks


type alias Enums =
    Dict Int Int (Dict String T.CDN_Name Int)


type alias Fields =
    Dict String T.CDN_Name Int


toByteString : String -> Int -> Int -> String
toByteString src pos end =
    let
        off : Int
        off =
            -- pos - unsafeForeignPtrToPtr src
            pos

        len : Int
        len =
            end - pos
    in
    String.slice off (off + len) src


chompTag : VarTable -> Enums -> Fields -> String -> Int -> Int -> T.CPP_Row -> T.CPP_Col -> List T.CEK_Chunk -> ( ( List T.CEK_Chunk, Int ), ( T.CPP_Row, T.CPP_Col ) )
chompTag vs es fs src pos end row col revChunks =
    let
        ( newPos, newCol ) =
            Var.chompInnerChars src pos end col

        tagPos : Int
        tagPos =
            pos + -1

        word : Char
        word =
            P.unsafeIndex src tagPos
    in
    if word == '$' then
        let
            name : T.CDN_Name
            name =
                Name.fromPtr src pos newPos
        in
        chompChunks vs es fs src newPos end row newCol newPos <|
            (T.CEK_ElmField name :: revChunks)

    else
        let
            name : T.CDN_Name
            name =
                Name.fromPtr src tagPos newPos

            code : Int
            code =
                Char.toCode word
        in
        if code >= 0x30 && code <= 0x39 then
            let
                ( enum, newEnums ) =
                    lookupEnum (Char.fromCode (code - 0x30)) name es
            in
            chompChunks vs newEnums fs src newPos end row newCol newPos <|
                (T.CEK_JsEnum enum :: revChunks)

        else if code >= 0x61 && code <= 0x7A then
            let
                ( field, newFields ) =
                    lookupField name fs
            in
            chompChunks vs es newFields src newPos end row newCol newPos <|
                (T.CEK_JsField field :: revChunks)

        else if name == "DEBUG" then
            chompChunks vs es fs src newPos end row newCol newPos (T.CEK_Debug :: revChunks)

        else if name == "PROD" then
            chompChunks vs es fs src newPos end row newCol newPos (T.CEK_Prod :: revChunks)

        else
            case Dict.get identity name vs of
                Just chunk ->
                    chompChunks vs es fs src newPos end row newCol newPos (chunk :: revChunks)

                Nothing ->
                    ( ( revChunks, pos ), ( row, col ) )


lookupField : T.CDN_Name -> Fields -> ( Int, Fields )
lookupField name fields =
    case Dict.get identity name fields of
        Just n ->
            ( n, fields )

        Nothing ->
            let
                n : Int
                n =
                    Dict.size fields
            in
            ( n, Dict.insert identity name n fields )


lookupEnum : Char -> T.CDN_Name -> Enums -> ( Int, Enums )
lookupEnum word var allEnums =
    let
        code : Int
        code =
            Char.toCode word

        enums : Dict String T.CDN_Name Int
        enums =
            Dict.get identity code allEnums
                |> Maybe.withDefault Dict.empty
    in
    case Dict.get identity var enums of
        Just n ->
            ( n, allEnums )

        Nothing ->
            let
                n : Int
                n =
                    Dict.size enums
            in
            ( n, Dict.insert identity code (Dict.insert identity var n enums) allEnums )



-- PROCESS IMPORTS


type alias VarTable =
    Dict String T.CDN_Name T.CEK_Chunk


toVarTable : T.CEP_Name -> Foreigns -> List T.CASTS_Import -> VarTable
toVarTable pkg foreigns imports =
    List.foldl (addImport pkg foreigns) Dict.empty imports


addImport : T.CEP_Name -> Foreigns -> T.CASTS_Import -> VarTable -> VarTable
addImport pkg foreigns (T.CASTS_Import (T.CRA_At _ importName) maybeAlias exposing_) vtable =
    if Name.isKernel importName then
        case maybeAlias of
            Just _ ->
                crash ("cannot use `as` with kernel import of: " ++ importName)

            Nothing ->
                let
                    home : T.CDN_Name
                    home =
                        Name.getKernel importName

                    add : T.CDN_Name -> Dict String T.CDN_Name T.CEK_Chunk -> Dict String T.CDN_Name T.CEK_Chunk
                    add name table =
                        Dict.insert identity (Name.sepBy '_' home name) (T.CEK_JsVar home name) table
                in
                List.foldl add vtable (toNames exposing_)

    else
        let
            home : T.CEMN_Canonical
            home =
                T.CEMN_Canonical (Dict.get identity importName foreigns |> Maybe.withDefault pkg) importName

            prefix : T.CDN_Name
            prefix =
                toPrefix importName maybeAlias

            add : T.CDN_Name -> Dict String T.CDN_Name T.CEK_Chunk -> Dict String T.CDN_Name T.CEK_Chunk
            add name table =
                Dict.insert identity (Name.sepBy '_' prefix name) (T.CEK_ElmVar home name) table
        in
        List.foldl add vtable (toNames exposing_)


toPrefix : T.CDN_Name -> Maybe T.CDN_Name -> T.CDN_Name
toPrefix home maybeAlias =
    case maybeAlias of
        Just alias ->
            alias

        Nothing ->
            if Name.hasDot home then
                crash ("kernel imports with dots need an alias: " ++ home)

            else
                home


toNames : T.CASTS_Exposing -> List T.CDN_Name
toNames exposing_ =
    case exposing_ of
        T.CASTS_Open ->
            crash "cannot have `exposing (..)` in kernel code."

        T.CASTS_Explicit exposedList ->
            List.map toName exposedList


toName : T.CASTS_Exposed -> T.CDN_Name
toName exposed =
    case exposed of
        T.CASTS_Lower (T.CRA_At _ name) ->
            name

        T.CASTS_Upper (T.CRA_At _ name) T.CASTS_Private ->
            name

        T.CASTS_Upper _ (T.CASTS_Public _) ->
            crash "cannot have Maybe(..) syntax in kernel code header"

        T.CASTS_Operator _ _ ->
            crash "cannot use binops in kernel code"



-- ENCODERS and DECODERS


chunkEncoder : T.CEK_Chunk -> Encode.Value
chunkEncoder chunk =
    case chunk of
        T.CEK_JS javascript ->
            Encode.object
                [ ( "type", Encode.string "JS" )
                , ( "javascript", Encode.string javascript )
                ]

        T.CEK_ElmVar home name ->
            Encode.object
                [ ( "type", Encode.string "ElmVar" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                ]

        T.CEK_JsVar home name ->
            Encode.object
                [ ( "type", Encode.string "JsVar" )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                ]

        T.CEK_ElmField name ->
            Encode.object
                [ ( "type", Encode.string "ElmField" )
                , ( "name", Encode.string name )
                ]

        T.CEK_JsField int ->
            Encode.object
                [ ( "type", Encode.string "JsField" )
                , ( "int", Encode.int int )
                ]

        T.CEK_JsEnum int ->
            Encode.object
                [ ( "type", Encode.string "JsEnum" )
                , ( "int", Encode.int int )
                ]

        T.CEK_Debug ->
            Encode.object
                [ ( "type", Encode.string "Debug" )
                ]

        T.CEK_Prod ->
            Encode.object
                [ ( "type", Encode.string "Prod" )
                ]


chunkDecoder : Decode.Decoder T.CEK_Chunk
chunkDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "JS" ->
                        Decode.map T.CEK_JS (Decode.field "javascript" Decode.string)

                    "ElmVar" ->
                        Decode.map2 T.CEK_ElmVar
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)

                    "JsVar" ->
                        Decode.map2 T.CEK_JsVar
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)

                    "ElmField" ->
                        Decode.map T.CEK_ElmField (Decode.field "name" Decode.string)

                    "JsField" ->
                        Decode.map T.CEK_JsField (Decode.field "int" Decode.int)

                    "JsEnum" ->
                        Decode.map T.CEK_JsEnum (Decode.field "int" Decode.int)

                    "Debug" ->
                        Decode.succeed T.CEK_Debug

                    "Prod" ->
                        Decode.succeed T.CEK_Prod

                    _ ->
                        Decode.fail ("Unknown Chunk's type: " ++ type_)
            )
