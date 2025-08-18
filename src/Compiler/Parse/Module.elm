module Compiler.Parse.Module exposing
    ( Effects(..)
    , Header
    , Module
    , ProjectType(..)
    , chompImport
    , chompImports
    , chompModule
    , defaultHeader
    , fromByteString
    , isKernel
    )

import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Imports as Imports
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- FROM BYTE STRING


fromByteString : SyntaxVersion -> ProjectType -> String -> Result E.Error Src.Module
fromByteString syntaxVersion projectType source =
    case P.fromByteString (chompModule syntaxVersion projectType) E.ModuleBadEnd source of
        Ok modul ->
            checkModule syntaxVersion projectType modul

        Err err ->
            Err (E.ParseError err)



-- PROJECT TYPE


type ProjectType
    = Package Pkg.Name
    | Application


isCore : ProjectType -> Bool
isCore projectType =
    case projectType of
        Package pkg ->
            pkg == Pkg.core

        Application ->
            False


isKernel : ProjectType -> Bool
isKernel projectType =
    case projectType of
        Package pkg ->
            Pkg.isKernel pkg

        Application ->
            False



-- MODULE


type alias Module =
    { initialComments : Src.FComments
    , header : Maybe Header
    , imports : Src.C1 (List (Src.C1 Src.Import))
    , infixes : List (Src.C1 (A.Located Src.Infix))
    , decls : List (Src.C2 Decl.Decl)
    }


chompModule : SyntaxVersion -> ProjectType -> P.Parser E.Module Module
chompModule syntaxVersion projectType =
    chompHeader
        |> P.bind
            (\( ( initialComments, headerComments ), header ) ->
                chompImports
                    (if isCore projectType then
                        []

                     else
                        Imports.defaults
                    )
                    |> P.bind
                        (\imports ->
                            (if isKernel projectType then
                                chompInfixes []

                             else
                                P.pure []
                            )
                                |> P.bind
                                    (\infixes ->
                                        P.specialize E.Declarations (chompDecls syntaxVersion)
                                            |> P.fmap
                                                (\decls ->
                                                    Module
                                                        initialComments
                                                        header
                                                        ( headerComments, imports )
                                                        infixes
                                                        decls
                                                )
                                    )
                        )
            )



-- CHECK MODULE


checkModule : SyntaxVersion -> ProjectType -> Module -> Result E.Error Src.Module
checkModule syntaxVersion projectType module_ =
    let
        ( ( values, unions ), ( aliases, ports ) ) =
            categorizeDecls [] [] [] [] (List.map Src.c2Value module_.decls)

        ( _, imports ) =
            module_.imports
    in
    case module_.header of
        Just ({ effects, docs } as header) ->
            let
                ( _, name ) =
                    header.name

                ( _, exports ) =
                    header.exports
            in
            checkEffects projectType ports effects
                |> Result.map
                    (Src.Module syntaxVersion
                        (Just name)
                        exports
                        (toDocs docs (List.map Src.c2Value module_.decls))
                        (List.map Src.c1Value imports)
                        values
                        unions
                        aliases
                        (List.map Src.c1Value module_.infixes)
                    )

        Nothing ->
            Ok
                (Src.Module syntaxVersion
                    Nothing
                    (A.At A.one (Src.Open [] []))
                    (toDocs (Err A.one) (List.map Src.c2Value module_.decls))
                    (List.map Src.c1Value imports)
                    values
                    unions
                    aliases
                    (List.map Src.c1Value module_.infixes)
                    (case ports of
                        [] ->
                            Src.NoEffects

                        _ ->
                            Src.Ports ports
                    )
                )


checkEffects : ProjectType -> List Src.Port -> Effects -> Result E.Error Src.Effects
checkEffects projectType ports effects =
    case effects of
        NoEffects region ->
            case ports of
                [] ->
                    Ok Src.NoEffects

                (Src.Port _ ( _, name ) _) :: _ ->
                    case projectType of
                        Package _ ->
                            Err (E.NoPortsInPackage name)

                        Application ->
                            Err (E.UnexpectedPort region)

        Ports region _ ->
            case projectType of
                Package _ ->
                    Err (E.NoPortModulesInPackage region)

                Application ->
                    case ports of
                        [] ->
                            Err (E.NoPorts region)

                        _ :: _ ->
                            Ok (Src.Ports ports)

        Manager region _ ( _, manager ) ->
            if isKernel projectType then
                case ports of
                    [] ->
                        Ok (Src.Manager region manager)

                    _ :: _ ->
                        Err (E.UnexpectedPort region)

            else
                Err (E.NoEffectsOutsideKernel region)


categorizeDecls : List (A.Located Src.Value) -> List (A.Located Src.Union) -> List (A.Located Src.Alias) -> List Src.Port -> List Decl.Decl -> ( ( List (A.Located Src.Value), List (A.Located Src.Union) ), ( List (A.Located Src.Alias), List Src.Port ) )
categorizeDecls values unions aliases ports decls =
    case decls of
        [] ->
            ( ( values, unions ), ( aliases, ports ) )

        decl :: otherDecls ->
            case decl of
                Decl.Value _ value ->
                    categorizeDecls (value :: values) unions aliases ports otherDecls

                Decl.Union _ union ->
                    categorizeDecls values (union :: unions) aliases ports otherDecls

                Decl.Alias _ alias_ ->
                    categorizeDecls values unions (alias_ :: aliases) ports otherDecls

                Decl.Port _ port_ ->
                    categorizeDecls values unions aliases (port_ :: ports) otherDecls



-- TO DOCS


toDocs : Result A.Region Src.Comment -> List Decl.Decl -> Src.Docs
toDocs comment decls =
    case comment of
        Ok overview ->
            Src.YesDocs overview (getComments decls [])

        Err region ->
            Src.NoDocs region (getComments decls [])


getComments : List Decl.Decl -> List ( Name.Name, Src.Comment ) -> List ( Name.Name, Src.Comment )
getComments decls comments =
    case decls of
        [] ->
            comments

        decl :: otherDecls ->
            case decl of
                Decl.Value c (A.At _ (Src.Value _ ( _, n ) _ _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Union c (A.At _ (Src.Union ( _, n ) _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Alias c (A.At _ (Src.Alias _ ( _, n ) _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Port c (Src.Port _ ( _, n ) _) ->
                    getComments otherDecls (addComment c n comments)


addComment : Maybe Src.Comment -> A.Located Name.Name -> List ( Name.Name, Src.Comment ) -> List ( Name.Name, Src.Comment )
addComment maybeComment (A.At _ name) comments =
    case maybeComment of
        Just comment ->
            ( name, comment ) :: comments

        Nothing ->
            comments



-- FRESH LINES


freshLine : (Row -> Col -> E.Module) -> P.Parser E.Module Src.FComments
freshLine toFreshLineError =
    Space.chomp E.ModuleSpace
        |> P.bind
            (\comments ->
                Space.checkFreshLine toFreshLineError
                    |> P.fmap (\_ -> comments)
            )



-- CHOMP DECLARATIONS


chompDecls : SyntaxVersion -> P.Parser E.Decl (List (Src.C2 Decl.Decl))
chompDecls syntaxVersion =
    Decl.declaration syntaxVersion
        |> P.bind (\( decl, _ ) -> P.loop (chompDeclsHelp syntaxVersion) [ decl ])


chompDeclsHelp : SyntaxVersion -> List (Src.C2 Decl.Decl) -> P.Parser E.Decl (P.Step (List (Src.C2 Decl.Decl)) (List (Src.C2 Decl.Decl)))
chompDeclsHelp syntaxVersion decls =
    P.oneOfWithFallback
        [ Space.checkFreshLine E.DeclStart
            |> P.bind
                (\_ ->
                    Decl.declaration syntaxVersion
                        |> P.fmap (\( decl, _ ) -> P.Loop (decl :: decls))
                )
        ]
        (P.Done (List.reverse decls))


chompInfixes : List (Src.C1 (A.Located Src.Infix)) -> P.Parser E.Module (List (Src.C1 (A.Located Src.Infix)))
chompInfixes infixes =
    P.oneOfWithFallback
        [ Decl.infix_
            |> P.bind (\binop -> chompInfixes (binop :: infixes))
        ]
        infixes



-- MODULE DOC COMMENT


chompModuleDocCommentSpace : P.Parser E.Module (Src.C1 (Result A.Region Src.Comment))
chompModuleDocCommentSpace =
    P.addLocation (freshLine E.FreshLine)
        |> P.bind
            (\(A.At region beforeComments) ->
                P.oneOfWithFallback
                    [ Space.docComment E.ImportStart E.ModuleSpace
                        |> P.bind
                            (\docComment ->
                                Space.chomp E.ModuleSpace
                                    |> P.bind
                                        (\afterComments ->
                                            Space.checkFreshLine E.FreshLine
                                                |> P.fmap
                                                    (\_ ->
                                                        ( beforeComments ++ afterComments
                                                        , Ok docComment
                                                        )
                                                    )
                                        )
                            )
                    ]
                    ( beforeComments, Err region )
            )



-- HEADER


type alias Header =
    { name : Src.C2 (A.Located Name.Name)
    , effects : Effects
    , exports : Src.C2 (A.Located Src.Exposing)
    , docs : Result A.Region Src.Comment
    }


defaultHeader : Header
defaultHeader =
    { name = ( ( [], [] ), A.At A.zero Name.mainModule )
    , effects = NoEffects A.zero
    , exports = ( ( [], [] ), A.At A.zero (Src.Open [] []) )
    , docs = Err A.zero
    }


type Effects
    = NoEffects A.Region
    | Ports A.Region Src.FComments
    | Manager A.Region Src.FComments (Src.C1 Src.Manager)


chompHeader : P.Parser E.Module (Src.C2 (Maybe Header))
chompHeader =
    freshLine E.FreshLine
        |> P.bind
            (\initialComments ->
                P.getPosition
                    |> P.bind
                        (\start ->
                            P.oneOfWithFallback
                                [ -- module MyThing exposing (..)
                                  Keyword.module_ E.ModuleProblem
                                    |> P.bind (\_ -> P.getPosition)
                                    |> P.bind
                                        (\effectEnd ->
                                            Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
                                                |> P.bind
                                                    (\beforeNameComments ->
                                                        P.addLocation (Var.moduleName E.ModuleName)
                                                            |> P.bind
                                                                (\name ->
                                                                    Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
                                                                        |> P.bind
                                                                            (\afterNameComments ->
                                                                                Keyword.exposing_ E.ModuleProblem
                                                                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem)
                                                                                    |> P.bind
                                                                                        (\afterExportsComments ->
                                                                                            P.addLocation (P.specialize E.ModuleExposing exposing_)
                                                                                                |> P.bind
                                                                                                    (\exports ->
                                                                                                        chompModuleDocCommentSpace
                                                                                                            |> P.fmap
                                                                                                                (\( headerComments, docComment ) ->
                                                                                                                    ( ( initialComments, headerComments )
                                                                                                                    , Just <|
                                                                                                                        Header ( ( beforeNameComments, afterNameComments ), name )
                                                                                                                            (NoEffects (A.Region start effectEnd))
                                                                                                                            ( ( [], afterExportsComments ), exports )
                                                                                                                            docComment
                                                                                                                    )
                                                                                                                )
                                                                                                    )
                                                                                        )
                                                                            )
                                                                )
                                                    )
                                        )
                                , -- port module MyThing exposing (..)
                                  Keyword.port_ E.PortModuleProblem
                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem)
                                    |> P.bind
                                        (\postPortComments ->
                                            Keyword.module_ E.PortModuleProblem
                                                |> P.bind (\_ -> P.getPosition)
                                                |> P.bind
                                                    (\effectEnd ->
                                                        Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                                            |> P.bind
                                                                (\beforeNameComments ->
                                                                    P.addLocation (Var.moduleName E.PortModuleName)
                                                                        |> P.bind
                                                                            (\name ->
                                                                                Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                                                                    |> P.bind
                                                                                        (\afterNameComments ->
                                                                                            Keyword.exposing_ E.PortModuleProblem
                                                                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem)
                                                                                                |> P.bind
                                                                                                    (\postExportsComments ->
                                                                                                        P.addLocation (P.specialize E.PortModuleExposing exposing_)
                                                                                                            |> P.bind
                                                                                                                (\exports ->
                                                                                                                    chompModuleDocCommentSpace
                                                                                                                        |> P.fmap
                                                                                                                            (\( headerComments, docComment ) ->
                                                                                                                                ( ( initialComments, headerComments )
                                                                                                                                , Just <|
                                                                                                                                    Header ( ( beforeNameComments, afterNameComments ), name )
                                                                                                                                        (Ports (A.Region start effectEnd) postPortComments)
                                                                                                                                        ( ( [], postExportsComments ), exports )
                                                                                                                                        docComment
                                                                                                                                )
                                                                                                                            )
                                                                                                                )
                                                                                                    )
                                                                                        )
                                                                            )
                                                                )
                                                    )
                                        )
                                , -- effect module MyThing where { command = MyCmd } exposing (..)
                                  Keyword.effect_ E.Effect
                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                    |> P.bind
                                        (\postEffectComments ->
                                            Keyword.module_ E.Effect
                                                |> P.bind (\_ -> P.getPosition)
                                                |> P.bind
                                                    (\effectEnd ->
                                                        Space.chompAndCheckIndent E.ModuleSpace E.Effect
                                                            |> P.bind
                                                                (\beforeNameComments ->
                                                                    P.addLocation (Var.moduleName E.ModuleName)
                                                                        |> P.bind
                                                                            (\name ->
                                                                                Space.chompAndCheckIndent E.ModuleSpace E.Effect
                                                                                    |> P.bind
                                                                                        (\afterNameComments ->
                                                                                            Keyword.where_ E.Effect
                                                                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                                                                                |> P.bind
                                                                                                    (\postWhereComments ->
                                                                                                        chompManager
                                                                                                            |> P.bind
                                                                                                                (\( beforeExportsComments, manager ) ->
                                                                                                                    Space.chompAndCheckIndent E.ModuleSpace E.Effect
                                                                                                                        |> P.bind
                                                                                                                            (\_ ->
                                                                                                                                Keyword.exposing_ E.Effect
                                                                                                                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                                                                                                                    |> P.bind
                                                                                                                                        (\afterExportsComments ->
                                                                                                                                            P.addLocation (P.specialize (\_ -> E.Effect) exposing_)
                                                                                                                                                |> P.bind
                                                                                                                                                    (\exports ->
                                                                                                                                                        chompModuleDocCommentSpace
                                                                                                                                                            |> P.fmap
                                                                                                                                                                (\( headerComments, docComment ) ->
                                                                                                                                                                    ( ( initialComments, headerComments )
                                                                                                                                                                    , Just <|
                                                                                                                                                                        Header ( ( beforeNameComments, afterNameComments ), name )
                                                                                                                                                                            (Manager (A.Region start effectEnd) postEffectComments ( postWhereComments, manager ))
                                                                                                                                                                            ( ( beforeExportsComments, afterExportsComments ), exports )
                                                                                                                                                                            docComment
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                    )
                                                                                                                                        )
                                                                                                                            )
                                                                                                                )
                                                                                                    )
                                                                                        )
                                                                            )
                                                                )
                                                    )
                                        )
                                ]
                                -- default header
                                ( ( initialComments, [] ), Nothing )
                        )
            )


chompManager : P.Parser E.Module (Src.C1 Src.Manager)
chompManager =
    P.word1 '{' E.Effect
        |> P.bind (\_ -> spaces_em)
        |> P.bind
            (\postOpeningBracketComments ->
                P.oneOf E.Effect
                    [ chompCommand
                        |> P.bind
                            (\cmd ->
                                spaces_em
                                    |> P.bind
                                        (\trailingComments ->
                                            P.oneOf E.Effect
                                                [ P.word1 '}' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.fmap
                                                        (\postClosingBracketComments ->
                                                            ( postClosingBracketComments
                                                            , Src.Cmd ( ( postOpeningBracketComments, trailingComments ), cmd )
                                                            )
                                                        )
                                                , P.word1 ',' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.bind
                                                        (\postCommaComments ->
                                                            chompSubscription
                                                                |> P.bind
                                                                    (\sub ->
                                                                        spaces_em
                                                                            |> P.bind
                                                                                (\preClosingBracketComments ->
                                                                                    P.word1 '}' E.Effect
                                                                                        |> P.bind (\_ -> spaces_em)
                                                                                        |> P.fmap
                                                                                            (\postClosingBracketComments ->
                                                                                                ( postClosingBracketComments
                                                                                                , Src.Fx
                                                                                                    ( ( postOpeningBracketComments, trailingComments ), cmd )
                                                                                                    ( ( postCommaComments, preClosingBracketComments ), sub )
                                                                                                )
                                                                                            )
                                                                                )
                                                                    )
                                                        )
                                                ]
                                        )
                            )
                    , chompSubscription
                        |> P.bind
                            (\sub ->
                                spaces_em
                                    |> P.bind
                                        (\trailingComments ->
                                            P.oneOf E.Effect
                                                [ P.word1 '}' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.fmap
                                                        (\postClosingBracketComments ->
                                                            ( postClosingBracketComments
                                                            , Src.Sub ( ( postOpeningBracketComments, trailingComments ), sub )
                                                            )
                                                        )
                                                , P.word1 ',' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.bind
                                                        (\postCommaComments ->
                                                            chompCommand
                                                                |> P.bind
                                                                    (\cmd ->
                                                                        spaces_em
                                                                            |> P.bind
                                                                                (\preClosingBracketComments ->
                                                                                    P.word1 '}' E.Effect
                                                                                        |> P.bind (\_ -> spaces_em)
                                                                                        |> P.fmap
                                                                                            (\postClosingBracketComments ->
                                                                                                ( postClosingBracketComments
                                                                                                , Src.Fx
                                                                                                    ( ( postCommaComments, preClosingBracketComments ), cmd )
                                                                                                    ( ( postOpeningBracketComments, trailingComments ), sub )
                                                                                                )
                                                                                            )
                                                                                )
                                                                    )
                                                        )
                                                ]
                                        )
                            )
                    ]
            )


chompCommand : P.Parser E.Module (Src.C2 (A.Located Name.Name))
chompCommand =
    Keyword.command_ E.Effect
        |> P.bind (\_ -> spaces_em)
        |> P.bind
            (\beforeEqualComments ->
                P.word1 '=' E.Effect
                    |> P.bind (\_ -> spaces_em)
                    |> P.bind
                        (\afterEqualComments ->
                            P.addLocation (Var.upper E.Effect)
                                |> P.fmap (\command -> ( ( beforeEqualComments, afterEqualComments ), command ))
                        )
            )


chompSubscription : P.Parser E.Module (Src.C2 (A.Located Name.Name))
chompSubscription =
    Keyword.subscription_ E.Effect
        |> P.bind (\_ -> spaces_em)
        |> P.bind
            (\beforeEqualComments ->
                P.word1 '=' E.Effect
                    |> P.bind (\_ -> spaces_em)
                    |> P.bind
                        (\afterEqualComments ->
                            P.addLocation (Var.upper E.Effect)
                                |> P.fmap (\subscription -> ( ( beforeEqualComments, afterEqualComments ), subscription ))
                        )
            )


spaces_em : P.Parser E.Module Src.FComments
spaces_em =
    Space.chompAndCheckIndent E.ModuleSpace E.Effect



-- IMPORTS


chompImports : List (Src.C1 Src.Import) -> P.Parser E.Module (List (Src.C1 Src.Import))
chompImports is =
    P.oneOfWithFallback
        [ chompImport
            |> P.bind (\i -> chompImports (i :: is))
        ]
        (List.reverse is)


chompImport : P.Parser E.Module (Src.C1 Src.Import)
chompImport =
    Keyword.import_ E.ImportStart
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName)
        |> P.bind
            (\preNameComments ->
                P.addLocation (Var.moduleName E.ImportName)
                    |> P.bind
                        (\((A.At (A.Region _ end) _) as name) ->
                            Space.chomp E.ModuleSpace
                                |> P.bind
                                    (\trailingComments ->
                                        P.oneOf E.ImportEnd
                                            [ Space.checkFreshLine E.ImportEnd
                                                |> P.fmap (\_ -> ( trailingComments, Src.Import ( preNameComments, name ) Nothing ( ( [], [] ), Src.Explicit (A.At A.zero []) ) ))
                                            , Space.checkIndent end E.ImportEnd
                                                |> P.bind
                                                    (\_ ->
                                                        P.oneOf E.ImportAs
                                                            [ chompAs ( preNameComments, name ) trailingComments
                                                            , chompExposing ( preNameComments, name ) Nothing [] trailingComments
                                                            ]
                                                    )
                                            ]
                                    )
                        )
            )


chompAs : Src.C1 (A.Located Name.Name) -> Src.FComments -> P.Parser E.Module (Src.C1 Src.Import)
chompAs name trailingComments =
    Keyword.as_ E.ImportAs
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias)
        |> P.bind
            (\postAliasComments ->
                Var.upper E.ImportAlias
                    |> P.bind
                        (\alias_ ->
                            P.getPosition
                                |> P.bind
                                    (\end ->
                                        Space.chomp E.ModuleSpace
                                            |> P.bind
                                                (\preExposedComments ->
                                                    P.oneOf E.ImportEnd
                                                        [ Space.checkFreshLine E.ImportEnd
                                                            |> P.fmap (\_ -> ( preExposedComments, Src.Import name (Just ( ( trailingComments, postAliasComments ), alias_ )) ( ( [], [] ), Src.Explicit (A.At A.zero []) ) ))
                                                        , Space.checkIndent end E.ImportEnd
                                                            |> P.bind (\_ -> chompExposing name (Just ( postAliasComments, alias_ )) trailingComments preExposedComments)
                                                        ]
                                                )
                                    )
                        )
            )


chompExposing : Src.C1 (A.Located Name.Name) -> Maybe (Src.C1 Name.Name) -> Src.FComments -> Src.FComments -> P.Parser E.Module (Src.C1 Src.Import)
chompExposing name maybeAlias trailingComments preExposedComments =
    Keyword.exposing_ E.ImportExposing
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList)
        |> P.bind
            (\postExposedComments ->
                P.specialize E.ImportExposingList exposing_
                    |> P.bind
                        (\exposed ->
                            freshLine E.ImportEnd
                                |> P.fmap (\comments -> ( comments, Src.Import name (Maybe.map (\( postAliasComments, alias_ ) -> ( ( trailingComments, postAliasComments ), alias_ )) maybeAlias) ( ( preExposedComments, postExposedComments ), exposed ) ))
                        )
            )



-- LISTING


exposing_ : P.Parser E.Exposing Src.Exposing
exposing_ =
    P.word1 '(' E.ExposingStart
        |> P.bind (\_ -> P.getPosition)
        |> P.bind
            (\start ->
                Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
                    |> P.bind
                        (\preExposedComments ->
                            P.oneOf E.ExposingValue
                                [ P.word2 '.' '.' E.ExposingValue
                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd)
                                    |> P.bind
                                        (\postComments ->
                                            P.word1 ')' E.ExposingEnd
                                                |> P.fmap (\_ -> Src.Open preExposedComments postComments)
                                        )
                                , chompExposed
                                    |> P.bind
                                        (\exposed ->
                                            Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                                |> P.bind
                                                    (\postExposedComments ->
                                                        P.loop (exposingHelp start) [ ( ( preExposedComments, postExposedComments ), exposed ) ]
                                                    )
                                        )
                                ]
                        )
            )


exposingHelp : A.Position -> List (Src.C2 Src.Exposed) -> P.Parser E.Exposing (P.Step (List (Src.C2 Src.Exposed)) Src.Exposing)
exposingHelp start revExposed =
    P.oneOf E.ExposingEnd
        [ P.word1 ',' E.ExposingEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue)
            |> P.bind
                (\preExposedComments ->
                    chompExposed
                        |> P.bind
                            (\exposed ->
                                Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                    |> P.fmap
                                        (\postExposedComments ->
                                            P.Loop (( ( preExposedComments, postExposedComments ), exposed ) :: revExposed)
                                        )
                            )
                )
        , P.word1 ')' E.ExposingEnd
            |> P.bind (\_ -> P.getPosition)
            |> P.fmap (\end -> P.Done (Src.Explicit (A.At (A.Region start end) (List.reverse revExposed))))
        ]


chompExposed : P.Parser E.Exposing Src.Exposed
chompExposed =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.ExposingValue
                    [ Var.lower E.ExposingValue
                        |> P.bind
                            (\name ->
                                P.getPosition
                                    |> P.fmap (\end -> Src.Lower <| A.at start end name)
                            )
                    , P.word1 '(' E.ExposingValue
                        |> P.bind (\_ -> Symbol.operator E.ExposingOperator E.ExposingOperatorReserved)
                        |> P.bind
                            (\op ->
                                P.word1 ')' E.ExposingOperatorRightParen
                                    |> P.bind (\_ -> P.getPosition)
                                    |> P.fmap (\end -> Src.Operator (A.Region start end) op)
                            )
                    , Var.upper E.ExposingValue
                        |> P.bind
                            (\name ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                                |> P.bind
                                                    (\privacyComments ->
                                                        privacy
                                                            |> P.fmap (Src.Upper (A.at start end name) << Tuple.pair privacyComments)
                                                    )
                                        )
                            )
                    ]
            )


privacy : P.Parser E.Exposing Src.Privacy
privacy =
    P.oneOfWithFallback
        [ P.word1 '(' E.ExposingTypePrivacy
            |> P.bind (\_ -> Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy)
            |> P.bind (\_ -> P.getPosition)
            |> P.bind
                (\start ->
                    P.word2 '.' '.' E.ExposingTypePrivacy
                        |> P.bind (\_ -> P.getPosition)
                        |> P.bind
                            (\end ->
                                Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
                                    |> P.bind (\_ -> P.word1 ')' E.ExposingTypePrivacy)
                                    |> P.fmap (\_ -> Src.Public (A.Region start end))
                            )
                )
        ]
        Src.Private
