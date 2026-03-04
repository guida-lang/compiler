module API.LanguageServerProtocol exposing
    ( HoverInformation
    , Location
    , Position
    , findReferences
    , getDefinitionLocation
    , getHoverInformation
    )

import Builder.File as File
import Builder.Guida.Outline as Outline
import Builder.Stuff as Stuff
import Compiler.AST.Source as Src
import Compiler.Data.Name exposing (Name)
import Compiler.Generate.Target as Target
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Guida.Package as Pkg
import Compiler.Json.String as Json
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


type alias HoverInformation =
    { documentation : String
    , range : Maybe { start : Position, end : Position }
    }



-- RUN


getDefinitionLocation : String -> Int -> Int -> Task Never (Result () Location)
getDefinitionLocation path line character =
    Stuff.findRootIn (Utils.fpDropFileName path)
        |> Task.bind
            (\maybeRoot ->
                case maybeRoot of
                    Just root ->
                        getDefinitionLocationHelp root path (line + 1) (character + 1)

                    Nothing ->
                        Task.pure (Err ())
            )


getHoverInformation : String -> Int -> Int -> Task Never (Result () (Maybe HoverInformation))
getHoverInformation path line character =
    Stuff.findRootIn (Utils.fpDropFileName path)
        |> Task.bind
            (\maybeRoot ->
                case maybeRoot of
                    Just root ->
                        getHoverInformationHelp root path (line + 1) (character + 1)

                    Nothing ->
                        Task.pure (Err ())
            )


findReferences : String -> Int -> Int -> Task Never (Result () (List Location))
findReferences path line character =
    Stuff.findRootIn (Utils.fpDropFileName path)
        |> Task.bind
            (\maybeRoot ->
                case maybeRoot of
                    Just root ->
                        findReferencesHelp root path (line + 1) (character + 1)

                    Nothing ->
                        Task.pure (Err ())
            )


findReferencesHelp : Stuff.Root -> String -> Int -> Int -> Task Never (Result () (List Location))
findReferencesHelp root path requestLine requestChar =
    Stuff.withRootLock (Stuff.rootPath root)
        (File.readUtf8 path
            |> Task.bind
                (\content ->
                    let
                        syntaxVersion : SV.SyntaxVersion
                        syntaxVersion =
                            SV.fileSyntaxVersion path
                    in
                    case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                        Ok (Src.Module _ _ _ _ _ values _ _ _ _) ->
                            case findLocalReferenceTarget requestLine requestChar values of
                                Just ( targetName, declarationRegion, body ) ->
                                    let
                                        matches : List A.Region
                                        matches =
                                            collectVarReferences targetName body
                                    in
                                    if List.isEmpty matches then
                                        [ regionToLocation ( path, declarationRegion ) ]
                                            |> Ok
                                            |> Task.pure

                                    else
                                        let
                                            argLoc : List Location
                                            argLoc =
                                                [ regionToLocation ( path, declarationRegion ) ]
                                        in
                                        (argLoc ++ List.map (\region -> regionToLocation ( path, region )) matches)
                                            |> Ok
                                            |> Task.pure

                                Nothing ->
                                    Task.pure (Err ())

                        Err _ ->
                            Task.pure (Err ())
                )
        )


findPatternRegionAt : Int -> Int -> Src.Pattern -> Maybe A.Region
findPatternRegionAt line char (A.At region pattern_) =
    if regionContainsPosition region line char then
        case pattern_ of
            Src.PVar _ ->
                Just region

            Src.PAnything _ ->
                Just region

            Src.PAlias innerPattern ( _, A.At aliasRegion _ ) ->
                if regionContainsPosition aliasRegion line char then
                    Just aliasRegion

                else
                    findPatternRegionAt line char (Src.c1Value innerPattern)

            Src.PParens ( _, innerPattern ) ->
                findPatternRegionAt line char innerPattern

            Src.PTuple a b cs ->
                findPatternRegionInList line char (List.map Tuple.second (a :: b :: cs))

            Src.PList ( _, patterns ) ->
                findPatternRegionInList line char (List.map Src.c2Value patterns)

            Src.PCons left right ->
                case findPatternRegionAt line char (Src.c0EolValue left) of
                    Just leftRegion ->
                        Just leftRegion

                    Nothing ->
                        findPatternRegionAt line char (Src.c2EolValue right)

            _ ->
                Nothing

    else
        Nothing


findPatternRegionInList : Int -> Int -> List Src.Pattern -> Maybe A.Region
findPatternRegionInList line char patterns =
    case patterns of
        [] ->
            Nothing

        pattern_ :: rest ->
            case findPatternRegionAt line char pattern_ of
                Just foundRegion ->
                    Just foundRegion

                Nothing ->
                    findPatternRegionInList line char rest


findLocalReferenceTarget : Int -> Int -> List (A.Located Src.Value) -> Maybe ( Name, A.Region, Src.Expr )
findLocalReferenceTarget line char values =
    case values of
        [] ->
            Nothing

        (A.At valueRegion (Src.Value _ _ args ( _, body ) _)) :: rest ->
            if regionContainsPosition valueRegion line char then
                case findPatternBinderInArgs line char args of
                    Just name ->
                        case findPatternRegionByNameInArgs name args of
                            Just declarationRegion ->
                                Just ( name, declarationRegion, body )

                            Nothing ->
                                findLocalReferenceTarget line char rest

                    Nothing ->
                        case findLocalBodyReferenceTarget line char args body of
                            Just target ->
                                Just target

                            Nothing ->
                                findLocalReferenceTarget line char rest

            else
                findLocalReferenceTarget line char rest


findLocalBodyReferenceTarget : Int -> Int -> List (Src.C1 Src.Pattern) -> Src.Expr -> Maybe ( Name, A.Region, Src.Expr )
findLocalBodyReferenceTarget line char args body =
    findBodyUsageNameAt line char body
        |> Maybe.andThen
            (\name ->
                let
                    argNames : List Name
                    argNames =
                        collectPatternBinderNamesInArgs args
                in
                if List.member name argNames then
                    findPatternRegionByNameInArgs name args
                        |> Maybe.map (\declarationRegion -> ( name, declarationRegion, body ))

                else
                    findBodyUsageRegionAt line char body
                        |> Maybe.map (\usageRegion -> ( name, usageRegion, body ))
            )


collectPatternBinderNamesInArgs : List (Src.C1 Src.Pattern) -> List Name
collectPatternBinderNamesInArgs args =
    List.concatMap (collectPatternBinderNames << Src.c1Value) args


collectPatternBinderNames : Src.Pattern -> List Name
collectPatternBinderNames (A.At _ pattern_) =
    case pattern_ of
        Src.PVar name ->
            [ name ]

        Src.PAnything name ->
            [ name ]

        Src.PAlias innerPattern ( _, A.At _ aliasName ) ->
            aliasName :: collectPatternBinderNames (Src.c1Value innerPattern)

        Src.PParens ( _, innerPattern ) ->
            collectPatternBinderNames innerPattern

        Src.PTuple a b cs ->
            List.concatMap (collectPatternBinderNames << Tuple.second) (a :: b :: cs)

        Src.PList ( _, patterns ) ->
            List.concatMap (collectPatternBinderNames << Src.c2Value) patterns

        Src.PCons left right ->
            collectPatternBinderNames (Src.c0EolValue left)
                ++ collectPatternBinderNames (Src.c2EolValue right)

        _ ->
            []


findPatternRegionByNameInArgs : Name -> List (Src.C1 Src.Pattern) -> Maybe A.Region
findPatternRegionByNameInArgs targetName args =
    case args of
        [] ->
            Nothing

        pattern_ :: rest ->
            case findPatternRegionByName targetName (Src.c1Value pattern_) of
                Just region ->
                    Just region

                Nothing ->
                    findPatternRegionByNameInArgs targetName rest


findPatternRegionByName : Name -> Src.Pattern -> Maybe A.Region
findPatternRegionByName targetName (A.At region pattern_) =
    case pattern_ of
        Src.PVar name ->
            if name == targetName then
                Just region

            else
                Nothing

        Src.PAnything name ->
            if name == targetName then
                Just region

            else
                Nothing

        Src.PAlias innerPattern ( _, A.At aliasRegion aliasName ) ->
            if aliasName == targetName then
                Just aliasRegion

            else
                findPatternRegionByName targetName (Src.c1Value innerPattern)

        Src.PParens ( _, innerPattern ) ->
            findPatternRegionByName targetName innerPattern

        Src.PTuple a b cs ->
            findPatternRegionByNameInList targetName (List.map Tuple.second (a :: b :: cs))

        Src.PList ( _, patterns ) ->
            findPatternRegionByNameInList targetName (List.map Src.c2Value patterns)

        Src.PCons left right ->
            case findPatternRegionByName targetName (Src.c0EolValue left) of
                Just foundRegion ->
                    Just foundRegion

                Nothing ->
                    findPatternRegionByName targetName (Src.c2EolValue right)

        _ ->
            Nothing


findPatternRegionByNameInList : Name -> List Src.Pattern -> Maybe A.Region
findPatternRegionByNameInList targetName patterns =
    case patterns of
        [] ->
            Nothing

        pattern_ :: rest ->
            case findPatternRegionByName targetName pattern_ of
                Just foundRegion ->
                    Just foundRegion

                Nothing ->
                    findPatternRegionByNameInList targetName rest


findBodyUsageNameAt : Int -> Int -> Src.Expr -> Maybe Name
findBodyUsageNameAt line char (A.At region expr_) =
    if regionContainsPosition region line char then
        case expr_ of
            Src.Var Src.LowVar name ->
                Just name

            Src.Var Src.CapVar name ->
                Just name

            Src.VarQual _ _ _ ->
                Nothing

            Src.List entries _ ->
                findNameAtInList line char (List.map Src.c2EolValue entries)

            Src.Negate expr ->
                findBodyUsageNameAt line char expr

            Src.Binops ops final ->
                findNameAtInList line char (List.map Tuple.first ops ++ [ final ])

            Src.Lambda _ ( _, body ) ->
                findBodyUsageNameAt line char body

            Src.Call fn args ->
                findNameAtInList line char (fn :: List.map Src.c1Value args)

            Src.If firstBranch branches ( _, finalExpr ) ->
                let
                    branchExprs : List Src.Expr
                    branchExprs =
                        List.concatMap
                            (\( _, ( ( _, condition ), ( _, branchExpr ) ) ) ->
                                [ condition, branchExpr ]
                            )
                            (firstBranch :: branches)
                in
                findNameAtInList line char (branchExprs ++ [ finalExpr ])

            Src.Let defs _ body ->
                let
                    defBodies : List Src.Expr
                    defBodies =
                        List.map
                            (\( _, def ) ->
                                case A.toValue def of
                                    Src.Define _ _ ( _, defBody ) _ ->
                                        defBody

                                    Src.Destruct _ ( _, defBody ) ->
                                        defBody
                            )
                            defs
                in
                findNameAtInList line char (defBodies ++ [ body ])

            Src.Case ( _, subject ) clauses ->
                let
                    clauseExprs : List Src.Expr
                    clauseExprs =
                        List.map (Tuple.second >> Src.c1Value) clauses
                in
                findNameAtInList line char (subject :: clauseExprs)

            Src.Access record _ ->
                findBodyUsageNameAt line char record

            Src.Update ( _, nameExpr ) ( _, fields ) ->
                findNameAtInList line char <|
                    (nameExpr :: List.map (Src.c2EolValue >> Tuple.second >> Src.c1Value) fields)

            Src.Record ( _, fields ) ->
                findNameAtInList line char <|
                    List.map (Tuple.second << Tuple.second << Src.c2EolValue) fields

            Src.Tuple a b cs ->
                findNameAtInList line char (List.map Tuple.second (a :: b :: cs))

            Src.Parens ( _, expr ) ->
                findBodyUsageNameAt line char expr

            _ ->
                Nothing

    else
        Nothing


findBodyUsageRegionAt : Int -> Int -> Src.Expr -> Maybe A.Region
findBodyUsageRegionAt line char (A.At region expr_) =
    if regionContainsPosition region line char then
        case expr_ of
            Src.Var Src.LowVar _ ->
                Just region

            Src.Var Src.CapVar _ ->
                Just region

            Src.VarQual _ _ _ ->
                Nothing

            Src.List entries _ ->
                findRegionAtInList line char (List.map Src.c2EolValue entries)

            Src.Negate expr ->
                findBodyUsageRegionAt line char expr

            Src.Binops ops final ->
                findRegionAtInList line char (List.map Tuple.first ops ++ [ final ])

            Src.Lambda _ ( _, body ) ->
                findBodyUsageRegionAt line char body

            Src.Call fn args ->
                findRegionAtInList line char (fn :: List.map Src.c1Value args)

            Src.If firstBranch branches ( _, finalExpr ) ->
                let
                    branchExprs : List Src.Expr
                    branchExprs =
                        List.concatMap
                            (\( _, ( ( _, condition ), ( _, branchExpr ) ) ) ->
                                [ condition, branchExpr ]
                            )
                            (firstBranch :: branches)
                in
                findRegionAtInList line char (branchExprs ++ [ finalExpr ])

            Src.Let defs _ body ->
                let
                    defBodies : List Src.Expr
                    defBodies =
                        List.map
                            (\( _, def ) ->
                                case A.toValue def of
                                    Src.Define _ _ ( _, defBody ) _ ->
                                        defBody

                                    Src.Destruct _ ( _, defBody ) ->
                                        defBody
                            )
                            defs
                in
                findRegionAtInList line char (defBodies ++ [ body ])

            Src.Case ( _, subject ) clauses ->
                let
                    clauseExprs : List Src.Expr
                    clauseExprs =
                        List.map (Tuple.second >> Src.c1Value) clauses
                in
                findRegionAtInList line char (subject :: clauseExprs)

            Src.Access record _ ->
                findBodyUsageRegionAt line char record

            Src.Update ( _, nameExpr ) ( _, fields ) ->
                findRegionAtInList line char <|
                    (nameExpr :: List.map (Src.c2EolValue >> Tuple.second >> Src.c1Value) fields)

            Src.Record ( _, fields ) ->
                findRegionAtInList line char <|
                    List.map (Tuple.second << Tuple.second << Src.c2EolValue) fields

            Src.Tuple a b cs ->
                findRegionAtInList line char (List.map Tuple.second (a :: b :: cs))

            Src.Parens ( _, expr ) ->
                findBodyUsageRegionAt line char expr

            _ ->
                Nothing

    else
        Nothing


findRegionAtInList : Int -> Int -> List Src.Expr -> Maybe A.Region
findRegionAtInList line char expressions =
    case expressions of
        [] ->
            Nothing

        expression :: rest ->
            case findBodyUsageRegionAt line char expression of
                Just region ->
                    Just region

                Nothing ->
                    findRegionAtInList line char rest


findNameAtInList : Int -> Int -> List Src.Expr -> Maybe Name
findNameAtInList line char expressions =
    case expressions of
        [] ->
            Nothing

        expression :: rest ->
            case findBodyUsageNameAt line char expression of
                Just name ->
                    Just name

                Nothing ->
                    findNameAtInList line char rest


findPatternBinderInArgs : Int -> Int -> List (Src.C1 Src.Pattern) -> Maybe Name
findPatternBinderInArgs line char args =
    case args of
        [] ->
            Nothing

        pattern_ :: rest ->
            case findPatternBinderAt line char (Src.c1Value pattern_) of
                Just name ->
                    Just name

                Nothing ->
                    findPatternBinderInArgs line char rest


findPatternBinderAt : Int -> Int -> Src.Pattern -> Maybe Name
findPatternBinderAt line char (A.At region pattern_) =
    if regionContainsPosition region line char then
        case pattern_ of
            Src.PVar name ->
                Just name

            Src.PAnything name ->
                Just name

            Src.PAlias innerPattern ( _, A.At aliasRegion aliasName ) ->
                if regionContainsPosition aliasRegion line char then
                    Just aliasName

                else
                    findPatternBinderAt line char (Src.c1Value innerPattern)

            Src.PParens ( _, innerPattern ) ->
                findPatternBinderAt line char innerPattern

            Src.PTuple a b cs ->
                findPatternBinderInList line char (List.map Tuple.second (a :: b :: cs))

            Src.PList ( _, patterns ) ->
                findPatternBinderInList line char (List.map Src.c2Value patterns)

            Src.PCons left right ->
                case findPatternBinderAt line char (Src.c0EolValue left) of
                    Just name ->
                        Just name

                    Nothing ->
                        findPatternBinderAt line char (Src.c2EolValue right)

            _ ->
                Nothing

    else
        Nothing


findPatternBinderInList : Int -> Int -> List Src.Pattern -> Maybe Name
findPatternBinderInList line char patterns =
    case patterns of
        [] ->
            Nothing

        pattern_ :: rest ->
            case findPatternBinderAt line char pattern_ of
                Just name ->
                    Just name

                Nothing ->
                    findPatternBinderInList line char rest


collectVarReferences : Name -> Src.Expr -> List A.Region
collectVarReferences targetName (A.At region expr_) =
    case expr_ of
        Src.Var Src.LowVar name ->
            if name == targetName then
                [ region ]

            else
                []

        Src.Var Src.CapVar name ->
            if name == targetName then
                [ region ]

            else
                []

        Src.VarQual _ _ _ ->
            []

        Src.List entries _ ->
            List.concatMap (collectVarReferences targetName << Src.c2EolValue) entries

        Src.Negate expr ->
            collectVarReferences targetName expr

        Src.Binops ops final ->
            List.concatMap (collectVarReferences targetName) (List.map Tuple.first ops ++ [ final ])

        Src.Lambda _ ( _, body ) ->
            collectVarReferences targetName body

        Src.Call fn args ->
            List.concatMap (collectVarReferences targetName) (fn :: List.map Src.c1Value args)

        Src.If firstBranch branches ( _, finalExpr ) ->
            let
                branchExprs : List Src.Expr
                branchExprs =
                    List.concatMap
                        (\( _, ( ( _, condition ), ( _, branchExpr ) ) ) ->
                            [ condition, branchExpr ]
                        )
                        (firstBranch :: branches)
            in
            List.concatMap (collectVarReferences targetName) (branchExprs ++ [ finalExpr ])

        Src.Let defs _ body ->
            let
                defBodies : List Src.Expr
                defBodies =
                    List.map
                        (\( _, def ) ->
                            case A.toValue def of
                                Src.Define _ _ ( _, defBody ) _ ->
                                    defBody

                                Src.Destruct _ ( _, defBody ) ->
                                    defBody
                        )
                        defs
            in
            List.concatMap (collectVarReferences targetName) (defBodies ++ [ body ])

        Src.Case ( _, subject ) clauses ->
            let
                clauseExprs : List Src.Expr
                clauseExprs =
                    List.map (Tuple.second >> Src.c1Value) clauses
            in
            List.concatMap (collectVarReferences targetName) (subject :: clauseExprs)

        Src.Access record _ ->
            collectVarReferences targetName record

        Src.Update ( _, nameExpr ) ( _, fields ) ->
            List.concatMap (collectVarReferences targetName)
                (nameExpr :: List.map (Src.c2EolValue >> Tuple.second >> Src.c1Value) fields)

        Src.Record ( _, fields ) ->
            List.concatMap (collectVarReferences targetName)
                (List.map (Tuple.second << Tuple.second << Src.c2EolValue) fields)

        Src.Tuple a b cs ->
            List.concatMap (collectVarReferences targetName) (List.map Tuple.second (a :: b :: cs))

        Src.Parens ( _, expr ) ->
            collectVarReferences targetName expr

        _ ->
            []



-- GET DEFINITION LOCATION


getDefinitionLocationHelp : Stuff.Root -> String -> Int -> Int -> Task Never (Result () Location)
getDefinitionLocationHelp root path requestLine requestChar =
    Stuff.withRootLock (Stuff.rootPath root)
        (File.readUtf8 path
            |> Task.bind
                (\content ->
                    let
                        syntaxVersion : SV.SyntaxVersion
                        syntaxVersion =
                            SV.fileSyntaxVersion path
                    in
                    case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                        Ok (Src.Module _ _ _ _ imports values unions aliases _ _) ->
                            findSymbolAt root path requestLine requestChar imports values unions aliases
                                |> Task.fmap (Result.map regionToLocation)

                        Err _ ->
                            Task.pure (Err ())
                )
        )



-- GET HOVER INFORMATION


getHoverInformationHelp : Stuff.Root -> String -> Int -> Int -> Task Never (Result () (Maybe HoverInformation))
getHoverInformationHelp root path requestLine requestChar =
    Stuff.withRootLock (Stuff.rootPath root)
        (File.readUtf8 path
            |> Task.bind
                (\content ->
                    let
                        syntaxVersion : SV.SyntaxVersion
                        syntaxVersion =
                            SV.fileSyntaxVersion path
                    in
                    case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                        Ok (Src.Module _ _ _ docs imports values unions aliases _ _) ->
                            -- First, try to find hover locally (on a definition in this file)
                            case findLocalValueHover requestLine requestChar docs values of
                                Just hover ->
                                    Task.pure (Ok (Just hover))

                                Nothing ->
                                    -- If not found locally, search via symbol resolution
                                    findSymbolAt root path requestLine requestChar imports values unions aliases
                                        |> Task.bind
                                            (\symbolResult ->
                                                case symbolResult of
                                                    Ok ( defPath, defRegion ) ->
                                                        File.readUtf8 defPath
                                                            |> Task.bind
                                                                (\defContent ->
                                                                    let
                                                                        defSyntaxVersion : SV.SyntaxVersion
                                                                        defSyntaxVersion =
                                                                            SV.fileSyntaxVersion defPath
                                                                    in
                                                                    case Parse.fromByteString Target.GuidaTarget defSyntaxVersion Parse.Application defContent of
                                                                        Ok (Src.Module _ _ _ defDocs _ defValues _ _ _ _) ->
                                                                            Task.pure (Ok (extractHoverFromDocs defRegion defDocs defValues))

                                                                        Err _ ->
                                                                            Task.pure (Err ())
                                                                )

                                                    Err _ ->
                                                        Task.pure (Err ())
                                            )

                        Err _ ->
                            Task.pure (Err ())
                )
        )


findLocalValueHover : Int -> Int -> Src.Docs -> List (A.Located Src.Value) -> Maybe HoverInformation
findLocalValueHover requestLine requestChar docs values =
    case values of
        [] ->
            Nothing

        (A.At valueRegion (Src.Value _ ( _, A.At nameRegion _ ) _ _ _)) :: rest ->
            if regionContainsPosition valueRegion requestLine requestChar || regionContainsPosition nameRegion requestLine requestChar then
                extractHoverFromDocs valueRegion docs values

            else
                findLocalValueHover requestLine requestChar docs rest


extractHoverFromDocs : A.Region -> Src.Docs -> List (A.Located Src.Value) -> Maybe HoverInformation
extractHoverFromDocs targetRegion docs values =
    -- First, find the value name that matches the target region
    findValueNameAtRegion targetRegion values
        |> Maybe.andThen
            (\valueName ->
                -- Look up the documentation for this value name in the Docs
                case docs of
                    Src.YesDocs _ valueDocs ->
                        findDocForName valueName valueDocs
                            |> Maybe.map (\(Src.Comment snippet) -> { documentation = Json.fromComment snippet, range = Nothing })

                    Src.NoDocs _ valueDocs ->
                        findDocForName valueName valueDocs
                            |> Maybe.map (\(Src.Comment snippet) -> { documentation = Json.fromComment snippet, range = Nothing })
            )


findDocForName : Name -> List ( Name, Src.Comment ) -> Maybe Src.Comment
findDocForName targetName docs =
    case docs of
        [] ->
            Nothing

        ( name, comment ) :: rest ->
            if name == targetName then
                Just comment

            else
                findDocForName targetName rest


findValueNameAtRegion : A.Region -> List (A.Located Src.Value) -> Maybe Name
findValueNameAtRegion targetRegion values =
    case values of
        [] ->
            Nothing

        (A.At valueRegion (Src.Value _ ( _, A.At nameRegion valueName ) _ _ _)) :: rest ->
            if valueRegion == targetRegion || nameRegion == targetRegion then
                Just valueName

            else
                findValueNameAtRegion targetRegion rest


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
                                    exposedWithRegion : List ( A.Region, Src.Exposed )
                                    exposedWithRegion =
                                        List.map
                                            (\c2 ->
                                                let
                                                    e : Src.Exposed
                                                    e =
                                                        Src.c2Value c2
                                                in
                                                case e of
                                                    Src.Lower (A.At regionName _) ->
                                                        ( regionName, e )

                                                    Src.Upper (A.At regionName _) _ ->
                                                        ( regionName, e )

                                                    _ ->
                                                        ( A.Region (A.Position 0 0) (A.Position 0 0), e )
                                            )
                                            exposedList

                                    found : Maybe ImportSymbol
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
        Just (OnModuleName _ importName _) ->
            Outline.getAllModulePaths root
                |> Task.andThen
                    (\allModulePaths ->
                        case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName importName) allModulePaths of
                            Just importPath ->
                                File.readUtf8 importPath
                                    |> Task.map
                                        (\content ->
                                            let
                                                syntaxVersion : SV.SyntaxVersion
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

        Just (OnExposedValue exposedName importName _) ->
            Outline.getAllModulePaths root
                |> Task.andThen
                    (\allModulePaths ->
                        case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName importName) allModulePaths of
                            Just importPath ->
                                File.readUtf8 importPath
                                    |> Task.map
                                        (\content ->
                                            let
                                                syntaxVersion : SV.SyntaxVersion
                                                syntaxVersion =
                                                    SV.fileSyntaxVersion importPath
                                            in
                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                Ok (Src.Module _ _ _ _ _ targetValues _ _ _ _) ->
                                                    let
                                                        found : Maybe ( Utils.FilePath, A.Region )
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
                    defToExpr : ( a, A.Located Src.Def ) -> Src.Expr
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
                maybeLocal : Maybe ( String, A.Region )
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
                                            isExposed : Bool
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
                                                                    syntaxVersion : SV.SyntaxVersion
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
            let
                -- 1. Check local definitions
                maybeLocal : Maybe ( String, A.Region )
                maybeLocal =
                    List.stoppableFoldl
                        (\(A.At _ (Src.Union ( _, A.At _ _ ) _ ctors)) acc ->
                            case
                                List.stoppableFoldl
                                    (\( A.At ctorRegion ctorName, _ ) acc2 ->
                                        if ctorName == name then
                                            List.Stop (Just ( initialPath, ctorRegion ))

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
                        unions
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
                                    findCtorInImport : Src.Import -> Task Never (Maybe ( Utils.FilePath, A.Region ))
                                    findCtorInImport (Src.Import ( _, A.At _ importName ) _ _) =
                                        case Dict.get ModuleName.toComparableCanonical (TypeCheck.Canonical Pkg.dummyName importName) allModulePaths of
                                            Nothing ->
                                                Task.pure Nothing

                                            Just path ->
                                                File.readUtf8 path
                                                    |> Task.map
                                                        (\content ->
                                                            let
                                                                syntaxVersion : SV.SyntaxVersion
                                                                syntaxVersion =
                                                                    SV.fileSyntaxVersion path
                                                            in
                                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                                Ok (Src.Module _ _ (A.At _ exports) _ _ _ targetUnions _ _ _) ->
                                                                    let
                                                                        findCtor : Maybe ( Utils.FilePath, A.Region )
                                                                        findCtor =
                                                                            List.stoppableFoldl
                                                                                (\(A.At _ (Src.Union ( _, A.At _ _ ) _ ctors)) acc ->
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
                                                                                isExposed : Bool
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

                                    tryImports : List Src.Import -> Task Never (Maybe ( Utils.FilePath, A.Region ))
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
                                                syntaxVersion : SV.SyntaxVersion
                                                syntaxVersion =
                                                    SV.fileSyntaxVersion path
                                            in
                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                Ok (Src.Module _ _ (A.At _ exports) _ _ _ targetUnions targetAliases _ _) ->
                                                    let
                                                        findCtor : Maybe ( Utils.FilePath, A.Region )
                                                        findCtor =
                                                            List.stoppableFoldl
                                                                (\(A.At _ (Src.Union _ _ ctors)) acc ->
                                                                    let
                                                                        maybeFoundCtor : Maybe ( Utils.FilePath, A.Region )
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

                                                        findAlias : Maybe ( Utils.FilePath, A.Region )
                                                        findAlias =
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
                                                    case exports of
                                                        Src.Open _ _ ->
                                                            Maybe.or findCtor findAlias

                                                        Src.Explicit (A.At _ exposedList) ->
                                                            let
                                                                isExposed : Bool
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
                                                                Maybe.or findCtor findAlias

                                                            else
                                                                Nothing

                                                Err _ ->
                                                    Nothing
                                        )
                    )

        VarQual Src.LowVar prefix name ->
            Outline.getAllModulePaths root
                |> Task.andThen
                    (\allModulePaths ->
                        let
                            maybeModulePathAndPkg : Maybe ( Utils.FilePath, Pkg.Name )
                            maybeModulePathAndPkg =
                                Dict.foldl ModuleName.compareCanonical
                                    (\(TypeCheck.Canonical pkgName home) path acc ->
                                        if prefix == home then
                                            Just ( path, pkgName )

                                        else
                                            acc
                                    )
                                    Nothing
                                    allModulePaths
                        in
                        case maybeModulePathAndPkg of
                            Nothing ->
                                Task.pure Nothing

                            Just ( path, pkgName ) ->
                                let
                                    getProjectType : Task Never (Maybe Parse.ProjectType)
                                    getProjectType =
                                        if pkgName == Pkg.dummyName then
                                            -- Current project: read outline to determine if app or package
                                            Outline.read root
                                                |> Task.fmap
                                                    (\outlineResult ->
                                                        case outlineResult of
                                                            Ok (Outline.App _) ->
                                                                Just Parse.Application

                                                            Ok (Outline.Pkg (Outline.GuidaPkgOutline pkgOutlineName _ _ _ _ _ _ _)) ->
                                                                Just (Parse.Package pkgOutlineName)

                                                            Ok (Outline.Pkg (Outline.ElmPkgOutline pkgOutlineName _ _ _ _ _ _ _)) ->
                                                                Just (Parse.Package pkgOutlineName)

                                                            Err _ ->
                                                                Just Parse.Application
                                                    )

                                        else
                                            -- Dependency: use its package name
                                            Task.pure (Just (Parse.Package pkgName))
                                in
                                getProjectType
                                    |> Task.andThen
                                        (\maybeProjectType ->
                                            case maybeProjectType of
                                                Nothing ->
                                                    Task.pure Nothing

                                                Just projectType ->
                                                    File.readUtf8 path
                                                        |> Task.map
                                                            (\content ->
                                                                let
                                                                    syntaxVersion : SV.SyntaxVersion
                                                                    syntaxVersion =
                                                                        SV.fileSyntaxVersion path
                                                                in
                                                                case Parse.fromByteString Target.GuidaTarget syntaxVersion projectType content of
                                                                    Ok (Src.Module _ _ (A.At _ exports) _ _ targetValues _ _ _ effects) ->
                                                                        let
                                                                            findValue : Maybe ( Utils.FilePath, A.Region )
                                                                            findValue =
                                                                                List.stoppableFoldl
                                                                                    (\(A.At _ (Src.Value _ ( _, A.At valueRegion valueName ) _ _ _)) acc ->
                                                                                        if valueName == name then
                                                                                            List.Stop (Just ( path, valueRegion ))

                                                                                        else
                                                                                            List.Continue acc
                                                                                    )
                                                                                    Nothing
                                                                                    targetValues

                                                                            findPort : Maybe ( Utils.FilePath, A.Region )
                                                                            findPort =
                                                                                case effects of
                                                                                    Src.Ports portsList ->
                                                                                        List.stoppableFoldl
                                                                                            (\(Src.Port _ ( _, A.At regionPort portName ) _) acc ->
                                                                                                if portName == name then
                                                                                                    List.Stop (Just ( path, regionPort ))

                                                                                                else
                                                                                                    List.Continue acc
                                                                                            )
                                                                                            Nothing
                                                                                            portsList

                                                                                    _ ->
                                                                                        Nothing
                                                                        in
                                                                        case exports of
                                                                            Src.Open _ _ ->
                                                                                Maybe.or findValue findPort

                                                                            Src.Explicit (A.At _ exposedList) ->
                                                                                let
                                                                                    isExposed : Bool
                                                                                    isExposed =
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
                                                                                    Maybe.or findValue findPort

                                                                                else
                                                                                    Nothing

                                                                    Err _ ->
                                                                        Nothing
                                                            )
                                        )
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
                                    findTypeInImport : Src.Import -> Task Never (Maybe ( String, A.Region ))
                                    findTypeInImport (Src.Import ( _, A.At _ importName ) _ ( _, exposing_ )) =
                                        let
                                            isExposed : Bool
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
                                                                    syntaxVersion : SV.SyntaxVersion
                                                                    syntaxVersion =
                                                                        SV.fileSyntaxVersion path
                                                                in
                                                                case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                                    Ok (Src.Module _ _ (A.At _ _) _ _ _ extUnions extAliases _ _) ->
                                                                        let
                                                                            unionRegion : Maybe ( Utils.FilePath, A.Region )
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

                                                                            aliasRegion : Maybe ( Utils.FilePath, A.Region )
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

                                    tryImports : List Src.Import -> Task Never (Maybe ( String, A.Region ))
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
                                                syntaxVersion : SV.SyntaxVersion
                                                syntaxVersion =
                                                    SV.fileSyntaxVersion path
                                            in
                                            case Parse.fromByteString Target.GuidaTarget syntaxVersion Parse.Application content of
                                                Ok (Src.Module _ _ (A.At _ exports) _ _ _ targetUnions targetAliases _ _) ->
                                                    let
                                                        findType : Maybe ( Utils.FilePath, A.Region )
                                                        findType =
                                                            let
                                                                unionRegion : Maybe ( Utils.FilePath, A.Region )
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

                                                                aliasRegion : Maybe ( Utils.FilePath, A.Region )
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
                                                                isExposed : Bool
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
        startCol <= char && char <= endCol

    else if line == startLine then
        startCol <= char

    else if line == endLine then
        char < endCol

    else
        line > startLine && line < endLine
