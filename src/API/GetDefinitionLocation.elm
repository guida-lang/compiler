module API.GetDefinitionLocation exposing
    ( Location
    , Position
    , run
    )

import Builder.File as File
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.Name exposing (Name)
import Compiler.Generate.Target as Target
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import List.Extra as List
import Task exposing (Task)
import Utils.Main as Utils
import Utils.Task.Extra as Task


type alias Location =
    { path : String
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
run path line character =
    Stuff.findRootIn (Utils.fpDropFileName path)
        |> Task.bind
            (\maybeRoot ->
                case maybeRoot of
                    Just root ->
                        runHelp root path (line + 1) (character + 1)

                    Nothing ->
                        Task.pure (Err ())
            )


runHelp : Stuff.Root -> String -> Int -> Int -> Task Never (Result () Location)
runHelp _ path requestLine requestChar =
    -- Step 1: Read the source file at the provided URI
    File.readUtf8 path
        |> Task.bind
            (\content ->
                -- Step 2: Parse the source code to find the symbol at the given position
                let
                    syntaxVersion : SV.SyntaxVersion
                    syntaxVersion =
                        SV.fileSyntaxVersion path

                    parseResult : Result String Src.Module
                    parseResult =
                        case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                            Ok modul ->
                                Ok modul

                            Err _ ->
                                Err "Parse error"

                    symbolAtPosition : Result () A.Region
                    symbolAtPosition =
                        case parseResult of
                            Ok (Src.Module _ _ _ _ _ values unions aliases _ _) ->
                                findSymbolAt requestLine requestChar values unions aliases

                            Err _ ->
                                Err ()

                    result : Result () Location
                    result =
                        symbolAtPosition
                            |> Result.map (regionToLocation path)
                in
                -- Step 3: Resolve the definition through canonicalization (found symbol location)
                -- Step 4: Return the location of the definition
                Task.pure result
            )


regionToLocation : String -> A.Region -> Location
regionToLocation path (A.Region (A.Position startLine startChar) (A.Position endLine endChar)) =
    { path = path
    , range =
        { start = { line = startLine - 1, character = startChar - 1 }
        , end = { line = endLine - 1, character = endChar - 1 }
        }
    }


findSymbolAt : Int -> Int -> List (A.Located Src.Value) -> List (A.Located Src.Union) -> List (A.Located Src.Alias) -> Result () A.Region
findSymbolAt line char values unions aliases =
    -- let
    --     valueMatch : Maybe (A.Located String)
    --     valueMatch =
    --         List.filterMap (matchValue line char) values |> List.head
    -- in
    case findSymbolAtValues line char values of
        Just symbol ->
            case findDefinitionForSymbol symbol values of
                Just region ->
                    Ok region

                Nothing ->
                    Err ()

        -- Ok v
        Nothing ->
            -- let
            --     unionMatch : Maybe (A.Located String)
            --     unionMatch =
            --         List.filterMap (matchUnion line char) unions |> List.head
            -- in
            -- case unionMatch of
            --     Just u ->
            --         Ok u
            --     Nothing ->
            --         let
            --             aliasMatch : Maybe (A.Located String)
            --             aliasMatch =
            --                 List.filterMap (matchAlias line char) aliases |> List.head
            --         in
            --         case aliasMatch of
            --             Just a ->
            --                 Ok a
            --             Nothing ->
            --                 Err ()
            Err ()


type Symbol
    = Var Src.VarType Name
    | VarQual Src.VarType Name Name


findSymbolAtValues : Int -> Int -> List (A.Located Src.Value) -> Maybe Symbol
findSymbolAtValues line char values =
    case values of
        [] ->
            Nothing

        v :: vs ->
            case findSymbolAtValue line char v of
                Just symbol ->
                    Just symbol

                Nothing ->
                    findSymbolAtValues line char vs


findSymbolAtValue : Int -> Int -> A.Located Src.Value -> Maybe Symbol
findSymbolAtValue line char (A.At region (Src.Value _ _ _ ( _, body ) _)) =
    if regionContainsPosition region line char then
        findSymbolAtExpr line char body

    else
        Nothing


findSymbolAtExpr : Int -> Int -> Src.Expr -> Maybe Symbol
findSymbolAtExpr line char (A.At region expr_) =
    if regionContainsPosition region line char then
        case expr_ of
            Src.Var varType name ->
                Just (Var varType name)

            Src.VarQual varType prefix name ->
                Just (VarQual varType prefix name)

            Src.List list _ ->
                List.stoppableFoldl
                    (\( _, listExpr ) acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char listExpr)
                    )
                    Nothing
                    list

            Src.Negate negatedExpr ->
                findSymbolAtExpr line char negatedExpr

            Src.Binops ops final ->
                List.stoppableFoldl
                    (\opExpr acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char opExpr)
                    )
                    Nothing
                    (final :: List.map Tuple.first ops)

            Src.Lambda _ ( _, body ) ->
                findSymbolAtExpr line char body

            Src.Call func args ->
                List.stoppableFoldl
                    (\callExpr acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char callExpr)
                    )
                    Nothing
                    (func :: List.map Src.c1Value args)

            Src.If firstBranch branches ( _, finally ) ->
                List.stoppableFoldl
                    (\ifExpr acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char ifExpr)
                    )
                    Nothing
                    (finally :: List.concatMap (\( _, ( ( _, condition ), ( _, branch ) ) ) -> [ condition, branch ]) (firstBranch :: branches))

            Src.Let defs _ body ->
                let
                    defToExpr ( _, def ) =
                        case A.toValue def of
                            Src.Define _ _ ( _, defBody ) _ ->
                                defBody

                            Src.Destruct _ ( _, defBody ) ->
                                defBody
                in
                List.stoppableFoldl
                    (\letExpr acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char letExpr)
                    )
                    Nothing
                    (body :: List.map defToExpr defs)

            Src.Case ( _, subject ) clauses ->
                List.stoppableFoldl
                    (\caseExpr acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char caseExpr)
                    )
                    Nothing
                    (subject :: List.map (Tuple.second >> Src.c1Value) clauses)

            Src.Access record _ ->
                findSymbolAtExpr line char record

            Src.Update ( _, name ) ( _, fields ) ->
                List.stoppableFoldl
                    (\field acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char field)
                    )
                    Nothing
                    (name :: List.map (Src.c2EolValue >> Tuple.second >> Src.c1Value) fields)

            Src.Record ( _, fields ) ->
                List.stoppableFoldl
                    (\( _, ( _, ( _, field ) ) ) acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char field)
                    )
                    Nothing
                    fields

            Src.Tuple a b cs ->
                List.stoppableFoldl
                    (\( _, tupleExpr ) acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtExpr line char tupleExpr)
                    )
                    Nothing
                    (a :: b :: cs)

            Src.Parens ( _, firstExpr ) ->
                findSymbolAtExpr line char firstExpr

            _ ->
                Nothing

    else
        Nothing


findDefinitionForSymbol : Symbol -> List (A.Located Src.Value) -> Maybe A.Region
findDefinitionForSymbol symbol values =
    case symbol of
        Var Src.LowVar name ->
            List.stoppableFoldl
                (\(A.At _ (Src.Value _ ( _, A.At region valueName ) _ _ _)) acc ->
                    if valueName == name then
                        List.Stop (Just region)

                    else
                        List.Continue acc
                )
                Nothing
                values

        -- VarQual varType prefix name ->
        --     List.filterMap
        --         (\(A.At region (Src.Value _ ( _, A.At _ valueName ) _ _ _)) ->
        --             if valueName == name then
        --                 Just region
        --             else
        --                 Nothing
        --         )
        --         values
        --         |> List.head
        _ ->
            -- Debug.todo "not implemented"
            Nothing


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
