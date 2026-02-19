module API.LanguageServerProtocol exposing
    ( Flags(..)
    , Location
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
import Compiler.Guida.Package as Pkg
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



-- FLAGS


type Flags
    = GetDefinitionLocation String Int Int



-- RUN


run : Flags -> Task Never (Result () Location)
run flags =
    case flags of
        GetDefinitionLocation path line character ->
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


type ImportSymbol
    = OnModuleName String String A.Region
    | OnExposedValue String String A.Region


findSymbolAt : Stuff.Root -> String -> Int -> Int -> List Src.Import -> List (A.Located Src.Value) -> List (A.Located Src.Union) -> List (A.Located Src.Alias) -> Task Never (Result () ( String, A.Region ))
findSymbolAt root path line char imports values unions aliases =
    let
        findImportSymbol : Maybe ImportSymbol
        findImportSymbol =
            List.stoppableFoldl
                (\(Src.Import ( _, A.At nameRegion importName ) _ ( _, exposing_ )) acc ->
                    if regionContainsPosition nameRegion line char then
                        List.Stop (Just (OnModuleName importName importName nameRegion))

                    else
                        case exposing_ of
                            Src.Open _ _ ->
                                List.Continue acc

                            Src.Explicit (A.At _ exposedList) ->
                                let
                                    exposedWithRegion =
                                        List.map
                                            (\c2 ->
                                                let
                                                    e =
                                                        Src.c2Value c2
                                                in
                                                case e of
                                                    Src.Lower (A.At regionName exposedName) ->
                                                        ( regionName, e )

                                                    Src.Upper (A.At regionName exposedName) _ ->
                                                        ( regionName, e )

                                                    _ ->
                                                        ( A.Region (A.Position 0 0) (A.Position 0 0), e )
                                            )
                                            exposedList

                                    found =
                                        List.stoppableFoldl
                                            (\( region, e ) innerAcc ->
                                                if regionContainsPosition region line char then
                                                    case e of
                                                        Src.Lower (A.At regionName exposedName) ->
                                                            List.Stop (Just (OnExposedValue exposedName importName regionName))

                                                        Src.Upper (A.At regionName exposedName) _ ->
                                                            List.Stop (Just (OnExposedValue exposedName importName regionName))

                                                        _ ->
                                                            List.Continue innerAcc

                                                else
                                                    List.Continue innerAcc
                                            )
                                            Nothing
                                            exposedWithRegion
                                in
                                case found of
                                    Just result ->
                                        List.Stop (Just result)

                                    Nothing ->
                                        List.Continue acc
                )
                Nothing
                imports
    in
    case findImportSymbol of
        Just (OnModuleName _ importName nameRegion) ->
            Outline.getAllModulePaths root
                |> Task.andThen
                    (\allModulePaths ->
                        case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName importName) allModulePaths of
                            Just importPath ->
                                File.readUtf8 importPath
                                    |> Task.map
                                        (\content ->
                                            let
                                                syntaxVersion =
                                                    SV.fileSyntaxVersion importPath
                                            in
                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                Ok (Src.Module _ maybeName _ _ _ _ _ _ _ _) ->
                                                    case maybeName of
                                                        Just (A.At moduleRegion _) ->
                                                            Ok ( importPath, moduleRegion )

                                                        Nothing ->
                                                            Err ()

                                                _ ->
                                                    Err ()
                                        )

                            Nothing ->
                                Task.pure (Err ())
                    )

        Just (OnExposedValue exposedName importName regionName) ->
            Outline.getAllModulePaths root
                |> Task.andThen
                    (\allModulePaths ->
                        case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName importName) allModulePaths of
                            Just importPath ->
                                File.readUtf8 importPath
                                    |> Task.map
                                        (\content ->
                                            let
                                                syntaxVersion =
                                                    SV.fileSyntaxVersion importPath
                                            in
                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                Ok (Src.Module _ _ _ _ _ targetValues _ _ _ _) ->
                                                    let
                                                        found =
                                                            List.stoppableFoldl
                                                                (\(A.At _ (Src.Value _ ( _, A.At region valueName ) _ _ _)) acc ->
                                                                    if valueName == exposedName then
                                                                        List.Stop (Just ( importPath, region ))

                                                                    else
                                                                        List.Continue acc
                                                                )
                                                                Nothing
                                                                targetValues
                                                    in
                                                    case found of
                                                        Just result ->
                                                            Ok result

                                                        Nothing ->
                                                            Err ()

                                                _ ->
                                                    Err ()
                                        )

                            Nothing ->
                                Task.pure (Err ())
                    )

        Nothing ->
            case findSymbolAtValues line char values of
                Just symbol ->
                    findDefinitionForSymbol root path symbol imports values unions aliases
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
    | TypeQual Name Name


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

            Src.TTypeQual nameRegion home name args ->
                if regionContainsPosition nameRegion line char then
                    Just (TypeQual home name)

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

            Src.TRecord fields _ _ ->
                List.stoppableFoldl
                    (\( _, ( _, ( _, fieldType ) ) ) acc ->
                        case acc of
                            Just _ ->
                                List.Stop acc

                            Nothing ->
                                List.Continue (findSymbolAtType line char fieldType)
                    )
                    Nothing
                    fields

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


findDefinitionForSymbol : Stuff.Root -> String -> Symbol -> List Src.Import -> List (A.Located Src.Value) -> List (A.Located Src.Union) -> List (A.Located Src.Alias) -> Task Never (Maybe ( String, A.Region ))
findDefinitionForSymbol root initialPath symbol imports values unions aliases =
    case symbol of
        Var Src.LowVar name ->
            let
                -- 1. Check local definitions
                maybeLocal =
                    List.stoppableFoldl
                        (\(A.At _ (Src.Value _ ( _, A.At region valueName ) _ _ _)) acc ->
                            if valueName == name then
                                List.Stop (Just ( initialPath, region ))

                            else
                                List.Continue acc
                        )
                        Nothing
                        values
            in
            case maybeLocal of
                Just found ->
                    Task.pure (Just found)

                Nothing ->
                    -- 2. Check imports and their exposing lists
                    Outline.getAllModulePaths root
                        |> Task.andThen
                            (\allModulePaths ->
                                let
                                    findInImport : Src.Import -> Task Never (Maybe ( String, A.Region ))
                                    findInImport (Src.Import ( _, A.At _ importName ) _ ( _, exposing_ )) =
                                        let
                                            isExposed =
                                                case exposing_ of
                                                    Src.Open _ _ ->
                                                        True

                                                    Src.Explicit (A.At _ exposedList) ->
                                                        List.any
                                                            (\( _, e ) ->
                                                                case e of
                                                                    Src.Lower (A.At _ exposedName) ->
                                                                        exposedName == name

                                                                    _ ->
                                                                        False
                                                            )
                                                            exposedList
                                        in
                                        if isExposed then
                                            case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName importName) allModulePaths of
                                                Nothing ->
                                                    Task.pure Nothing

                                                Just path ->
                                                    File.readUtf8 path
                                                        |> Task.map
                                                            (\content ->
                                                                let
                                                                    syntaxVersion =
                                                                        SV.fileSyntaxVersion path
                                                                in
                                                                case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                                    Ok (Src.Module _ _ (A.At _ _) _ _ targetValues _ _ _ _) ->
                                                                        List.stoppableFoldl
                                                                            (\(A.At _ (Src.Value _ ( _, A.At region valueName ) _ _ _)) acc ->
                                                                                if valueName == name then
                                                                                    List.Stop (Just ( path, region ))

                                                                                else
                                                                                    List.Continue acc
                                                                            )
                                                                            Nothing
                                                                            targetValues

                                                                    Err _ ->
                                                                        Nothing
                                                            )

                                        else
                                            Task.pure Nothing

                                    -- Try each import in order
                                    tryImports : List Src.Import -> Task Never (Maybe ( String, A.Region ))
                                    tryImports importsList =
                                        case importsList of
                                            [] ->
                                                Task.pure Nothing

                                            i :: is ->
                                                findInImport i
                                                    |> Task.andThen
                                                        (\res ->
                                                            case res of
                                                                Just _ ->
                                                                    Task.pure res

                                                                Nothing ->
                                                                    tryImports is
                                                        )
                                in
                                tryImports imports
                            )

        Var Src.CapVar name ->
            Outline.getAllModulePaths root
                |> Task.andThen
                    (\allModulePaths ->
                        let
                            findCtorInImport (Src.Import ( _, A.At _ importName ) _ _) =
                                case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName importName) allModulePaths of
                                    Nothing ->
                                        Task.pure Nothing

                                    Just path ->
                                        File.readUtf8 path
                                            |> Task.map
                                                (\content ->
                                                    let
                                                        syntaxVersion =
                                                            SV.fileSyntaxVersion path
                                                    in
                                                    case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                        Ok (Src.Module _ _ (A.At _ exports) _ _ _ targetUnions _ _ _) ->
                                                            let
                                                                findCtor =
                                                                    List.stoppableFoldl
                                                                        (\(A.At _ (Src.Union ( _, A.At _ unionName ) _ ctors)) acc ->
                                                                            case
                                                                                List.stoppableFoldl
                                                                                    (\( A.At ctorRegion ctorName, _ ) acc2 ->
                                                                                        if ctorName == name then
                                                                                            List.Stop (Just ( path, ctorRegion ))

                                                                                        else
                                                                                            List.Continue acc2
                                                                                    )
                                                                                    Nothing
                                                                                    (List.map Src.c2EolValue ctors)
                                                                            of
                                                                                Just foundCtor ->
                                                                                    List.Stop (Just foundCtor)

                                                                                Nothing ->
                                                                                    List.Continue acc
                                                                        )
                                                                        Nothing
                                                                        targetUnions
                                                            in
                                                            case exports of
                                                                Src.Open _ _ ->
                                                                    findCtor

                                                                Src.Explicit (A.At _ exposedList) ->
                                                                    let
                                                                        isExposed =
                                                                            List.any
                                                                                (\( _, e ) ->
                                                                                    case e of
                                                                                        Src.Upper (A.At _ exposedName) ( _, Src.Public _ ) ->
                                                                                            -- U(..) exposes all ctors for union U
                                                                                            List.any
                                                                                                (\(A.At _ (Src.Union ( _, A.At _ unionName ) ctors _)) ->
                                                                                                    (unionName == exposedName)
                                                                                                        && List.any
                                                                                                            (\c ->
                                                                                                                case Src.c1Value c of
                                                                                                                    A.At _ ctorName ->
                                                                                                                        ctorName == name
                                                                                                            )
                                                                                                            ctors
                                                                                                )
                                                                                                targetUnions

                                                                                        Src.Upper (A.At _ exposedName) ( _, Src.Private ) ->
                                                                                            -- U2 is directly exposed
                                                                                            name == exposedName

                                                                                        _ ->
                                                                                            False
                                                                                )
                                                                                exposedList
                                                                    in
                                                                    if isExposed then
                                                                        findCtor

                                                                    else
                                                                        Nothing

                                                        Err _ ->
                                                            Nothing
                                                )

                            tryImports importsList =
                                case importsList of
                                    [] ->
                                        Task.pure Nothing

                                    i :: is ->
                                        findCtorInImport i
                                            |> Task.andThen
                                                (\res ->
                                                    case res of
                                                        Just _ ->
                                                            Task.pure res

                                                        Nothing ->
                                                            tryImports is
                                                )
                        in
                        tryImports imports
                    )

        VarQual Src.CapVar prefix name ->
            Outline.getAllModulePaths root
                |> Task.andThen
                    (\allModulePaths ->
                        case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName prefix) allModulePaths of
                            Nothing ->
                                Task.pure Nothing

                            Just path ->
                                File.readUtf8 path
                                    |> Task.map
                                        (\content ->
                                            let
                                                syntaxVersion =
                                                    SV.fileSyntaxVersion path
                                            in
                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                Ok (Src.Module _ _ (A.At _ exports) _ _ _ targetUnions _ _ _) ->
                                                    let
                                                        findCtor =
                                                            List.stoppableFoldl
                                                                (\(A.At _ (Src.Union _ _ ctors)) acc ->
                                                                    let
                                                                        maybeFoundCtor =
                                                                            List.stoppableFoldl
                                                                                (\( A.At ctorRegion ctorName, _ ) acc2 ->
                                                                                    if ctorName == name then
                                                                                        List.Stop (Just ( path, ctorRegion ))

                                                                                    else
                                                                                        List.Continue acc2
                                                                                )
                                                                                Nothing
                                                                                (List.map Src.c2EolValue ctors)
                                                                    in
                                                                    case maybeFoundCtor of
                                                                        Just foundCtor ->
                                                                            List.Stop (Just foundCtor)

                                                                        Nothing ->
                                                                            List.Continue acc
                                                                )
                                                                Nothing
                                                                targetUnions
                                                    in
                                                    case exports of
                                                        Src.Open _ _ ->
                                                            findCtor

                                                        Src.Explicit (A.At _ exposedList) ->
                                                            let
                                                                isExposed =
                                                                    List.any
                                                                        (\( _, e ) ->
                                                                            case e of
                                                                                Src.Upper (A.At _ exposedName) ( _, Src.Public _ ) ->
                                                                                    -- U(..) exposes all ctors for union U
                                                                                    List.any
                                                                                        (\(A.At _ (Src.Union ( _, A.At _ unionName ) ctors _)) ->
                                                                                            unionName
                                                                                                == exposedName
                                                                                                && List.any
                                                                                                    (\c ->
                                                                                                        case Src.c1Value c of
                                                                                                            A.At _ ctorName ->
                                                                                                                ctorName == name
                                                                                                    )
                                                                                                    ctors
                                                                                        )
                                                                                        targetUnions

                                                                                Src.Upper (A.At _ exposedName) ( _, Src.Private ) ->
                                                                                    -- U1 is directly exposed
                                                                                    name == exposedName

                                                                                _ ->
                                                                                    False
                                                                        )
                                                                        exposedList
                                                            in
                                                            if isExposed then
                                                                findCtor

                                                            else
                                                                Nothing

                                                Err _ ->
                                                    Nothing
                                        )
                    )

        VarQual Src.LowVar prefix name ->
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
                                                                                    foundPathAndRegion : Maybe ( Utils.FilePath, A.Region )
                                                                                    foundPathAndRegion =
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
                                                                                case foundPathAndRegion of
                                                                                    Just pathAndRegion ->
                                                                                        List.Stop (Just pathAndRegion)

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

        Type name ->
            let
                maybeUnionRegion : Maybe ( String, A.Region )
                maybeUnionRegion =
                    List.stoppableFoldl
                        (\(A.At _ union) _ ->
                            case union of
                                Src.Union ( _, A.At regionName unionName ) _ _ ->
                                    if unionName == name then
                                        List.Stop (Just ( initialPath, regionName ))

                                    else
                                        List.Continue Nothing
                        )
                        Nothing
                        unions

                maybeAliasRegion : Maybe ( String, A.Region )
                maybeAliasRegion =
                    List.stoppableFoldl
                        (\(A.At _ alias) _ ->
                            case alias of
                                Src.Alias _ ( _, A.At regionName aliasName ) _ _ ->
                                    if aliasName == name then
                                        List.Stop (Just ( initialPath, regionName ))

                                    else
                                        List.Continue Nothing
                        )
                        Nothing
                        aliases
            in
            case Maybe.or maybeUnionRegion maybeAliasRegion of
                Just found ->
                    Task.pure (Just found)

                Nothing ->
                    -- Check imports for external types
                    Outline.getAllModulePaths root
                        |> Task.andThen
                            (\allModulePaths ->
                                let
                                    findTypeInImport (Src.Import ( _, A.At _ importName ) _ ( _, exposing_ )) =
                                        let
                                            isExposed =
                                                case exposing_ of
                                                    Src.Open _ _ ->
                                                        True

                                                    Src.Explicit (A.At _ exposedList) ->
                                                        List.any
                                                            (\( _, e ) ->
                                                                case e of
                                                                    Src.Upper (A.At _ exposedName) _ ->
                                                                        exposedName == name

                                                                    _ ->
                                                                        False
                                                            )
                                                            exposedList
                                        in
                                        if isExposed then
                                            case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName importName) allModulePaths of
                                                Nothing ->
                                                    Task.pure Nothing

                                                Just path ->
                                                    File.readUtf8 path
                                                        |> Task.map
                                                            (\content ->
                                                                let
                                                                    syntaxVersion =
                                                                        SV.fileSyntaxVersion path
                                                                in
                                                                case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                                    Ok (Src.Module _ _ (A.At _ exports) _ _ _ extUnions extAliases _ _) ->
                                                                        let
                                                                            unionRegion =
                                                                                List.stoppableFoldl
                                                                                    (\(A.At _ (Src.Union ( _, A.At regionName unionName ) _ _)) acc ->
                                                                                        if unionName == name then
                                                                                            List.Stop (Just ( path, regionName ))

                                                                                        else
                                                                                            List.Continue acc
                                                                                    )
                                                                                    Nothing
                                                                                    extUnions

                                                                            aliasRegion =
                                                                                List.stoppableFoldl
                                                                                    (\(A.At _ (Src.Alias _ ( _, A.At regionName aliasName ) _ _)) acc ->
                                                                                        if aliasName == name then
                                                                                            List.Stop (Just ( path, regionName ))

                                                                                        else
                                                                                            List.Continue acc
                                                                                    )
                                                                                    Nothing
                                                                                    extAliases
                                                                        in
                                                                        Maybe.or unionRegion aliasRegion

                                                                    Err _ ->
                                                                        Nothing
                                                            )

                                        else
                                            Task.pure Nothing

                                    tryImports importsList =
                                        case importsList of
                                            [] ->
                                                Task.pure Nothing

                                            i :: is ->
                                                findTypeInImport i
                                                    |> Task.andThen
                                                        (\res ->
                                                            case res of
                                                                Just _ ->
                                                                    Task.pure res

                                                                Nothing ->
                                                                    tryImports is
                                                        )
                                in
                                tryImports imports
                            )

        TypeQual prefix name ->
            Outline.getAllModulePaths root
                |> Task.andThen
                    (\allModulePaths ->
                        case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName prefix) allModulePaths of
                            Nothing ->
                                Task.pure Nothing

                            Just path ->
                                File.readUtf8 path
                                    |> Task.map
                                        (\content ->
                                            let
                                                syntaxVersion =
                                                    SV.fileSyntaxVersion path
                                            in
                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                Ok (Src.Module _ _ (A.At _ exports) _ _ _ targetUnions targetAliases _ _) ->
                                                    let
                                                        findType =
                                                            let
                                                                unionRegion =
                                                                    List.stoppableFoldl
                                                                        (\(A.At _ (Src.Union ( _, A.At regionName unionName ) _ _)) acc ->
                                                                            if unionName == name then
                                                                                List.Stop (Just ( path, regionName ))

                                                                            else
                                                                                List.Continue acc
                                                                        )
                                                                        Nothing
                                                                        targetUnions

                                                                aliasRegion =
                                                                    List.stoppableFoldl
                                                                        (\(A.At _ (Src.Alias _ ( _, A.At regionName aliasName ) _ _)) acc ->
                                                                            if aliasName == name then
                                                                                List.Stop (Just ( path, regionName ))

                                                                            else
                                                                                List.Continue acc
                                                                        )
                                                                        Nothing
                                                                        targetAliases
                                                            in
                                                            Maybe.or unionRegion aliasRegion
                                                    in
                                                    case exports of
                                                        Src.Open _ _ ->
                                                            findType

                                                        Src.Explicit (A.At _ exposedList) ->
                                                            let
                                                                isExposed =
                                                                    List.any
                                                                        (\( _, e ) ->
                                                                            case e of
                                                                                Src.Upper (A.At _ exposedName) _ ->
                                                                                    exposedName == name

                                                                                _ ->
                                                                                    False
                                                                        )
                                                                        exposedList
                                                            in
                                                            if isExposed then
                                                                findType

                                                            else
                                                                Nothing

                                                Err _ ->
                                                    Nothing
                                        )
                    )


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
