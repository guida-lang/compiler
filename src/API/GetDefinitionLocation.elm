module API.GetDefinitionLocation exposing
    ( Location
    , Position
    , run
    )

import Builder.File as File
import Builder.Guida.Outline as Outline
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.Name exposing (Name)
import Compiler.Generate.Target as Target
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Parse.Module as Parse
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Data.Map as Dict
import List.Extra as List
import Maybe.Extra as Maybe
import System.TypeCheck.IO as TypeCheck
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
runHelp root path requestLine requestChar =
    File.readUtf8 path
        |> Task.bind
            (\content ->
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
                in
                case parseResult of
                    Ok (Src.Module _ _ _ _ imports values unions aliases _ _) ->
                        findSymbolAt root path requestLine requestChar imports values unions aliases
                            |> Task.fmap (Result.map regionToLocation)

                    Err _ ->
                        Task.pure (Err ())
            )


regionToLocation : ( String, A.Region ) -> Location
regionToLocation ( path, A.Region (A.Position startLine startChar) (A.Position endLine endChar) ) =
    { path = path
    , range =
        { start = { line = startLine - 1, character = startChar - 1 }
        , end = { line = endLine - 1, character = endChar - 1 }
        }
    }


findSymbolAt : Stuff.Root -> String -> Int -> Int -> List Src.Import -> List (A.Located Src.Value) -> List (A.Located Src.Union) -> List (A.Located Src.Alias) -> Task Never (Result () ( String, A.Region ))
findSymbolAt root path line char imports values _ _ =
    case findSymbolAtValues line char values of
        Just symbol ->
            findDefinitionForSymbol root path symbol imports values
                |> Task.fmap
                    (\definition ->
                        case definition of
                            Just pathAndRegion ->
                                Ok pathAndRegion

                            Nothing ->
                                Err ()
                    )

        Nothing ->
            Task.pure (Err ())


type Symbol
    = Var Src.VarType Name
    | VarQual Src.VarType Name Name
    | Type Name


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
findSymbolAtValue line char (A.At region (Src.Value _ _ _ ( _, body ) maybeType)) =
    if regionContainsPosition region line char then
        case maybeType of
            Just ( _, ( _, type_ ) ) ->
                Maybe.or (findSymbolAtType line char type_) <|
                    findSymbolAtExpr line char body

            Nothing ->
                findSymbolAtExpr line char body

    else
        Nothing


findSymbolAtType : Int -> Int -> Src.Type -> Maybe Symbol
findSymbolAtType line char (A.At region type_) =
    if regionContainsPosition region line char then
        case type_ of
            Src.TLambda ( _, arg ) ( _, result ) ->
                List.stoppableFoldl
                    (\lambdaType acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtType line char lambdaType)
                    )
                    Nothing
                    [ arg, result ]

            Src.TVar _ ->
                Nothing

            Src.TType nameRegion name args ->
                if regionContainsPosition nameRegion line char then
                    Just (Type name)

                else
                    List.stoppableFoldl
                        (\( _, argType ) acc ->
                            case acc of
                                Just _ ->
                                    List.Stop acc

                                Nothing ->
                                    List.Continue (findSymbolAtType line char argType)
                        )
                        Nothing
                        args

            Src.TTypeQual _ _ _ _ ->
                -- Src.TTypeQual A.Region Name Name (List (C1 Type))
                Nothing

            Src.TRecord _ _ _ ->
                -- Src.TRecord (List (C2 ( C1 (A.Located Name), C1 Type ))) (Maybe (C2 (A.Located Name))) FComments
                Nothing

            Src.TUnit ->
                Nothing

            Src.TTuple a b cs ->
                List.stoppableFoldl
                    (\( _, tupleType ) acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtType line char tupleType)
                    )
                    Nothing
                    (a :: b :: cs)

            Src.TParens ( _, tipe_ ) ->
                findSymbolAtType line char tipe_

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


findDefinitionForSymbol : Stuff.Root -> String -> Symbol -> List Src.Import -> List (A.Located Src.Value) -> Task Never (Maybe ( String, A.Region ))
findDefinitionForSymbol root initialPath symbol imports values =
    case symbol of
        Var Src.LowVar name ->
            Task.pure <|
                List.stoppableFoldl
                    (\(A.At _ (Src.Value _ ( _, A.At region valueName ) _ _ _)) acc ->
                        if valueName == name then
                            List.Stop (Just ( initialPath, region ))

                        else
                            List.Continue acc
                    )
                    Nothing
                    values

        Var Src.CapVar _ ->
            -- TODO
            Task.pure Nothing

        VarQual _ prefix name ->
            Outline.getAllModulePaths root
                |> Task.fmap
                    (\allModulePaths ->
                        imports
                            |> List.map
                                (\(Src.Import ( _, A.At _ importName ) maybeAlias _) ->
                                    ( maybeAlias
                                        |> Maybe.map Tuple.second
                                        |> Maybe.withDefault importName
                                    , importName
                                    )
                                )
                            |> Dict.fromList identity
                            |> Dict.get identity prefix
                            |> Maybe.andThen
                                (\modulePath ->
                                    List.stoppableFoldl
                                        (\( TypeCheck.Canonical _ home, filePath ) _ ->
                                            if home == modulePath then
                                                List.Stop (Just filePath)

                                            else
                                                List.Continue Nothing
                                        )
                                        Nothing
                                        (Dict.toList ModuleName.compareCanonical allModulePaths)
                                )
                    )
                |> Task.bind
                    (\maybePath ->
                        case maybePath of
                            Just path ->
                                File.readUtf8 path
                                    |> Task.fmap
                                        (\content ->
                                            let
                                                syntaxVersion : SV.SyntaxVersion
                                                syntaxVersion =
                                                    SV.fileSyntaxVersion path
                                            in
                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                Ok (Src.Module _ _ (A.At _ exports) _ _ targetValues _ _ _ _) ->
                                                    case exports of
                                                        Src.Open _ _ ->
                                                            List.stoppableFoldl
                                                                (\(A.At _ (Src.Value _ ( _, A.At valueRegion valueName ) _ _ _)) _ ->
                                                                    if valueName == name then
                                                                        List.Stop (Just ( path, valueRegion ))

                                                                    else
                                                                        List.Continue Nothing
                                                                )
                                                                Nothing
                                                                targetValues

                                                        Src.Explicit (A.At _ exposedList) ->
                                                            List.stoppableFoldl
                                                                (\( _, exposed ) _ ->
                                                                    case exposed of
                                                                        Src.Lower (A.At _ exposedName) ->
                                                                            if exposedName == name then
                                                                                let
                                                                                    foundRegion =
                                                                                        List.stoppableFoldl
                                                                                            (\(A.At _ (Src.Value _ ( _, A.At valueRegion valueName ) _ _ _)) _ ->
                                                                                                if valueName == name then
                                                                                                    List.Stop (Just ( path, valueRegion ))

                                                                                                else
                                                                                                    List.Continue Nothing
                                                                                            )
                                                                                            Nothing
                                                                                            targetValues
                                                                                in
                                                                                case foundRegion of
                                                                                    Just region ->
                                                                                        List.Stop (Just region)

                                                                                    Nothing ->
                                                                                        List.Continue Nothing

                                                                            else
                                                                                List.Continue Nothing

                                                                        _ ->
                                                                            List.Continue Nothing
                                                                )
                                                                Nothing
                                                                exposedList

                                                Err _ ->
                                                    Nothing
                                        )

                            Nothing ->
                                Task.pure Nothing
                    )

        Type _ ->
            -- TODO
            Task.pure Nothing


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
