module API.GetDefinitionLocation exposing
    ( Location
    , Position
    , run
    )

import Builder.File as File
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Generate.Target as Target
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task


type alias Location =
    { uri : String
    , range :
        { start : Position
        , end : Position
        }
    }


type alias Position =
    { line : Int
    , character : Int
    }



-- RUN


run : String -> Int -> Int -> Task Never (Result () Location)
run uri line character =
    Stuff.findRootIn (Utils.fpDropFileName uri)
        |> Task.andThen
            (\maybeRoot ->
                case maybeRoot of
                    Just root ->
                        runHelp root uri line character

                    Nothing ->
                        Task.succeed (Err ())
            )


runHelp : Stuff.Root -> String -> Int -> Int -> Task Never (Result () Location)
runHelp _ uri requestLine requestChar =
    -- Step 1: Read the source file at the provided URI
    File.readUtf8 uri
        |> Task.andThen
            (\content ->
                -- Step 2: Parse the source code to find the symbol at the given position
                let
                    syntaxVersion : SV.SyntaxVersion
                    syntaxVersion =
                        SV.fileSyntaxVersion uri

                    parseResult : Result String Src.Module
                    parseResult =
                        case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                            Ok modul ->
                                Ok modul

                            Err _ ->
                                Err "Parse error"

                    symbolAtPosition : Result () (A.Located String)
                    symbolAtPosition =
                        case parseResult of
                            Ok (Src.Module _ _ _ _ _ values unions aliases _ _) ->
                                findSymbolAt requestLine requestChar values unions aliases

                            Err _ ->
                                Err ()

                    result : Result () Location
                    result =
                        symbolAtPosition
                            |> Result.map (regionToLocation uri)
                in
                -- Step 3: Resolve the definition through canonicalization (found symbol location)
                -- Step 4: Return the location of the definition
                Task.succeed result
            )


regionToLocation : String -> A.Located String -> Location
regionToLocation uri (A.At (A.Region (A.Position startLine startChar) (A.Position endLine endChar)) _) =
    { uri = uri
    , range =
        { start = { line = startLine - 1, character = startChar - 1 }
        , end = { line = endLine - 1, character = endChar - 1 }
        }
    }


findSymbolAt : Int -> Int -> List (A.Located Src.Value) -> List (A.Located Src.Union) -> List (A.Located Src.Alias) -> Result () (A.Located String)
findSymbolAt line char values unions aliases =
    let
        valueMatch : Maybe (A.Located String)
        valueMatch =
            List.filterMap (matchValue line char) values |> List.head
    in
    case valueMatch of
        Just v ->
            Ok v

        Nothing ->
            let
                unionMatch : Maybe (A.Located String)
                unionMatch =
                    List.filterMap (matchUnion line char) unions |> List.head
            in
            case unionMatch of
                Just u ->
                    Ok u

                Nothing ->
                    let
                        aliasMatch : Maybe (A.Located String)
                        aliasMatch =
                            List.filterMap (matchAlias line char) aliases |> List.head
                    in
                    case aliasMatch of
                        Just a ->
                            Ok a

                        Nothing ->
                            Err ()


matchValue : Int -> Int -> A.Located Src.Value -> Maybe (A.Located String)
matchValue line char (A.At region (Src.Value _ ( _, A.At nameRegion name ) _ _ _)) =
    if regionContainsPosition nameRegion line char then
        Just (A.At region name)

    else
        Nothing


matchUnion : Int -> Int -> A.Located Src.Union -> Maybe (A.Located String)
matchUnion line char (A.At region (Src.Union ( _, A.At nameRegion name ) _ _)) =
    if regionContainsPosition nameRegion line char then
        Just (A.At region name)

    else
        Nothing


matchAlias : Int -> Int -> A.Located Src.Alias -> Maybe (A.Located String)
matchAlias line char (A.At region (Src.Alias _ ( _, A.At nameRegion name ) _ _)) =
    if regionContainsPosition nameRegion line char then
        Just (A.At region name)

    else
        Nothing


regionContainsPosition : A.Region -> Int -> Int -> Bool
regionContainsPosition (A.Region (A.Position startLine startCol) (A.Position endLine endCol)) line char =
    if line == startLine && line == endLine then
        startCol <= char && char < endCol

    else if line == startLine then
        startCol <= char

    else if line == endLine then
        char < endCol

    else
        line > startLine && line < endLine
