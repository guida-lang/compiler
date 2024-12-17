module Compiler.Parse.Module exposing
    ( ProjectType(..)
    , chompImport
    , chompImports
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
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- FROM BYTE STRING


fromByteString : ProjectType -> String -> Result E.Error Src.CASTS_Module
fromByteString projectType source =
    case P.fromByteString (chompModule projectType) E.ModuleBadEnd source of
        Ok modul ->
            checkModule projectType modul

        Err err ->
            Err (E.ParseError err)



-- PROJECT TYPE


type ProjectType
    = Package Pkg.CEP_Name
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
    { header : Maybe Header
    , imports : List Src.CASTS_Import
    , infixes : List (A.CRA_Located Src.CASTS_Infix)
    , decls : List Decl.Decl
    }


chompModule : ProjectType -> P.Parser E.Module Module
chompModule projectType =
    chompHeader
        |> P.bind
            (\header ->
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
                                        P.specialize E.Declarations (chompDecls [])
                                            |> P.fmap
                                                (\decls ->
                                                    Module
                                                        header
                                                        imports
                                                        infixes
                                                        decls
                                                )
                                    )
                        )
            )



-- CHECK MODULE


checkModule : ProjectType -> Module -> Result E.Error Src.CASTS_Module
checkModule projectType module_ =
    let
        ( ( values, unions ), ( aliases, ports ) ) =
            categorizeDecls [] [] [] [] module_.decls
    in
    case module_.header of
        Just { name, effects, exports, docs } ->
            checkEffects projectType ports effects
                |> Result.map
                    (Src.CASTS_Module
                        (Just name)
                        exports
                        (toDocs docs module_.decls)
                        module_.imports
                        values
                        unions
                        aliases
                        module_.infixes
                    )

        Nothing ->
            Ok
                (Src.CASTS_Module
                    Nothing
                    (A.CRA_At A.one Src.CASTS_Open)
                    (Src.CASTS_NoDocs A.one)
                    module_.imports
                    values
                    unions
                    aliases
                    module_.infixes
                    (case ports of
                        [] ->
                            Src.CASTS_NoEffects

                        _ ->
                            Src.CASTS_Ports ports
                    )
                )


checkEffects : ProjectType -> List Src.CASTS_Port -> Effects -> Result E.Error Src.CASTS_Effects
checkEffects projectType ports effects =
    case effects of
        NoEffects region ->
            case ports of
                [] ->
                    Ok Src.CASTS_NoEffects

                (Src.CASTS_Port name _) :: _ ->
                    case projectType of
                        Package _ ->
                            Err (E.NoPortsInPackage name)

                        Application ->
                            Err (E.UnexpectedPort region)

        Ports region ->
            case projectType of
                Package _ ->
                    Err (E.NoPortModulesInPackage region)

                Application ->
                    case ports of
                        [] ->
                            Err (E.NoPorts region)

                        _ :: _ ->
                            Ok (Src.CASTS_Ports ports)

        Manager region manager ->
            if isKernel projectType then
                case ports of
                    [] ->
                        Ok (Src.CASTS_Manager region manager)

                    _ :: _ ->
                        Err (E.UnexpectedPort region)

            else
                Err (E.NoEffectsOutsideKernel region)


categorizeDecls : List (A.CRA_Located Src.CASTS_Value) -> List (A.CRA_Located Src.CASTS_Union) -> List (A.CRA_Located Src.CASTS_Alias) -> List Src.CASTS_Port -> List Decl.Decl -> ( ( List (A.CRA_Located Src.CASTS_Value), List (A.CRA_Located Src.CASTS_Union) ), ( List (A.CRA_Located Src.CASTS_Alias), List Src.CASTS_Port ) )
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


toDocs : Result A.CRA_Region Src.CASTS_Comment -> List Decl.Decl -> Src.CASTS_Docs
toDocs comment decls =
    case comment of
        Ok overview ->
            Src.CASTS_YesDocs overview (getComments decls [])

        Err region ->
            Src.CASTS_NoDocs region


getComments : List Decl.Decl -> List ( Name.CDN_Name, Src.CASTS_Comment ) -> List ( Name.CDN_Name, Src.CASTS_Comment )
getComments decls comments =
    case decls of
        [] ->
            comments

        decl :: otherDecls ->
            case decl of
                Decl.Value c (A.CRA_At _ (Src.CASTS_Value n _ _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Union c (A.CRA_At _ (Src.CASTS_Union n _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Alias c (A.CRA_At _ (Src.CASTS_Alias n _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Port c (Src.CASTS_Port n _) ->
                    getComments otherDecls (addComment c n comments)


addComment : Maybe Src.CASTS_Comment -> A.CRA_Located Name.CDN_Name -> List ( Name.CDN_Name, Src.CASTS_Comment ) -> List ( Name.CDN_Name, Src.CASTS_Comment )
addComment maybeComment (A.CRA_At _ name) comments =
    case maybeComment of
        Just comment ->
            ( name, comment ) :: comments

        Nothing ->
            comments



-- FRESH LINES


freshLine : (Row -> Col -> E.Module) -> P.Parser E.Module ()
freshLine toFreshLineError =
    Space.chomp E.ModuleSpace
        |> P.bind (\_ -> Space.checkFreshLine toFreshLineError)



-- CHOMP DECLARATIONS


chompDecls : List Decl.Decl -> P.Parser E.Decl (List Decl.Decl)
chompDecls decls =
    Decl.declaration
        |> P.bind
            (\( decl, _ ) ->
                P.oneOfWithFallback
                    [ Space.checkFreshLine E.DeclStart
                        |> P.bind (\_ -> chompDecls (decl :: decls))
                    ]
                    (List.reverse (decl :: decls))
            )


chompInfixes : List (A.CRA_Located Src.CASTS_Infix) -> P.Parser E.Module (List (A.CRA_Located Src.CASTS_Infix))
chompInfixes infixes =
    P.oneOfWithFallback
        [ Decl.infix_
            |> P.bind (\binop -> chompInfixes (binop :: infixes))
        ]
        infixes



-- MODULE DOC COMMENT


chompModuleDocCommentSpace : P.Parser E.Module (Result A.CRA_Region Src.CASTS_Comment)
chompModuleDocCommentSpace =
    P.addLocation (freshLine E.FreshLine)
        |> P.bind
            (\(A.CRA_At region ()) ->
                P.oneOfWithFallback
                    [ Space.docComment E.ImportStart E.ModuleSpace
                        |> P.bind
                            (\docComment ->
                                Space.chomp E.ModuleSpace
                                    |> P.bind (\_ -> Space.checkFreshLine E.FreshLine)
                                    |> P.fmap (\_ -> Ok docComment)
                            )
                    ]
                    (Err region)
            )



-- HEADER


type alias Header =
    { name : A.CRA_Located Name.CDN_Name
    , effects : Effects
    , exports : A.CRA_Located Src.CASTS_Exposing
    , docs : Result A.CRA_Region Src.CASTS_Comment
    }


type Effects
    = NoEffects A.CRA_Region
    | Ports A.CRA_Region
    | Manager A.CRA_Region Src.CASTS_Manager


chompHeader : P.Parser E.Module (Maybe Header)
chompHeader =
    freshLine E.FreshLine
        |> P.bind (\_ -> P.getPosition)
        |> P.bind
            (\start ->
                P.oneOfWithFallback
                    [ -- module MyThing exposing (..)
                      Keyword.module_ E.ModuleProblem
                        |> P.bind (\_ -> P.getPosition)
                        |> P.bind
                            (\effectEnd ->
                                Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
                                    |> P.bind (\_ -> P.addLocation (Var.moduleName E.ModuleName))
                                    |> P.bind
                                        (\name ->
                                            Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
                                                |> P.bind (\_ -> Keyword.exposing_ E.ModuleProblem)
                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem)
                                                |> P.bind (\_ -> P.addLocation (P.specialize E.ModuleExposing exposing_))
                                                |> P.bind
                                                    (\exports ->
                                                        chompModuleDocCommentSpace
                                                            |> P.fmap
                                                                (\comment ->
                                                                    Just <|
                                                                        Header
                                                                            name
                                                                            (NoEffects (A.CRA_Region start effectEnd))
                                                                            exports
                                                                            comment
                                                                )
                                                    )
                                        )
                            )
                    , -- port module MyThing exposing (..)
                      Keyword.port_ E.PortModuleProblem
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem)
                        |> P.bind (\_ -> Keyword.module_ E.PortModuleProblem)
                        |> P.bind (\_ -> P.getPosition)
                        |> P.bind
                            (\effectEnd ->
                                Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                    |> P.bind (\_ -> P.addLocation (Var.moduleName E.PortModuleName))
                                    |> P.bind
                                        (\name ->
                                            Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                                |> P.bind (\_ -> Keyword.exposing_ E.PortModuleProblem)
                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem)
                                                |> P.bind (\_ -> P.addLocation (P.specialize E.PortModuleExposing exposing_))
                                                |> P.bind
                                                    (\exports ->
                                                        chompModuleDocCommentSpace
                                                            |> P.fmap
                                                                (\comment ->
                                                                    Just <|
                                                                        Header
                                                                            name
                                                                            (Ports (A.CRA_Region start effectEnd))
                                                                            exports
                                                                            comment
                                                                )
                                                    )
                                        )
                            )
                    , -- effect module MyThing where { command = MyCmd } exposing (..)
                      Keyword.effect_ E.Effect
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                        |> P.bind (\_ -> Keyword.module_ E.Effect)
                        |> P.bind (\_ -> P.getPosition)
                        |> P.bind
                            (\effectEnd ->
                                Space.chompAndCheckIndent E.ModuleSpace E.Effect
                                    |> P.bind (\_ -> P.addLocation (Var.moduleName E.ModuleName))
                                    |> P.bind
                                        (\name ->
                                            Space.chompAndCheckIndent E.ModuleSpace E.Effect
                                                |> P.bind (\_ -> Keyword.where_ E.Effect)
                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                                |> P.bind (\_ -> chompManager)
                                                |> P.bind
                                                    (\manager ->
                                                        Space.chompAndCheckIndent E.ModuleSpace E.Effect
                                                            |> P.bind (\_ -> Keyword.exposing_ E.Effect)
                                                            |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.Effect)
                                                            |> P.bind (\_ -> P.addLocation (P.specialize (\_ -> E.Effect) exposing_))
                                                            |> P.bind
                                                                (\exports ->
                                                                    chompModuleDocCommentSpace
                                                                        |> P.fmap
                                                                            (\comment ->
                                                                                Just <|
                                                                                    Header name (Manager (A.CRA_Region start effectEnd) manager) exports comment
                                                                            )
                                                                )
                                                    )
                                        )
                            )
                    ]
                    -- default header
                    Nothing
            )


chompManager : P.Parser E.Module Src.CASTS_Manager
chompManager =
    P.word1 '{' E.Effect
        |> P.bind (\_ -> spaces_em)
        |> P.bind
            (\_ ->
                P.oneOf E.Effect
                    [ chompCommand
                        |> P.bind
                            (\cmd ->
                                spaces_em
                                    |> P.bind
                                        (\_ ->
                                            P.oneOf E.Effect
                                                [ P.word1 '}' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.fmap (\_ -> Src.CASTS_Cmd cmd)
                                                , P.word1 ',' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.bind (\_ -> chompSubscription)
                                                    |> P.bind
                                                        (\sub ->
                                                            spaces_em
                                                                |> P.bind (\_ -> P.word1 '}' E.Effect)
                                                                |> P.bind (\_ -> spaces_em)
                                                                |> P.fmap (\_ -> Src.CASTS_Fx cmd sub)
                                                        )
                                                ]
                                        )
                            )
                    , chompSubscription
                        |> P.bind
                            (\sub ->
                                spaces_em
                                    |> P.bind
                                        (\_ ->
                                            P.oneOf E.Effect
                                                [ P.word1 '}' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.fmap (\_ -> Src.CASTS_Sub sub)
                                                , P.word1 ',' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.bind (\_ -> chompCommand)
                                                    |> P.bind
                                                        (\cmd ->
                                                            spaces_em
                                                                |> P.bind (\_ -> P.word1 '}' E.Effect)
                                                                |> P.bind (\_ -> spaces_em)
                                                                |> P.fmap (\_ -> Src.CASTS_Fx cmd sub)
                                                        )
                                                ]
                                        )
                            )
                    ]
            )


chompCommand : P.Parser E.Module (A.CRA_Located Name.CDN_Name)
chompCommand =
    Keyword.command_ E.Effect
        |> P.bind (\_ -> spaces_em)
        |> P.bind (\_ -> P.word1 '=' E.Effect)
        |> P.bind (\_ -> spaces_em)
        |> P.bind (\_ -> P.addLocation (Var.upper E.Effect))


chompSubscription : P.Parser E.Module (A.CRA_Located Name.CDN_Name)
chompSubscription =
    Keyword.subscription_ E.Effect
        |> P.bind (\_ -> spaces_em)
        |> P.bind (\_ -> P.word1 '=' E.Effect)
        |> P.bind (\_ -> spaces_em)
        |> P.bind (\_ -> P.addLocation (Var.upper E.Effect))


spaces_em : P.Parser E.Module ()
spaces_em =
    Space.chompAndCheckIndent E.ModuleSpace E.Effect



-- IMPORTS


chompImports : List Src.CASTS_Import -> P.Parser E.Module (List Src.CASTS_Import)
chompImports is =
    P.oneOfWithFallback
        [ chompImport
            |> P.bind (\i -> chompImports (i :: is))
        ]
        (List.reverse is)


chompImport : P.Parser E.Module Src.CASTS_Import
chompImport =
    Keyword.import_ E.ImportStart
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName)
        |> P.bind (\_ -> P.addLocation (Var.moduleName E.ImportName))
        |> P.bind
            (\((A.CRA_At (A.CRA_Region _ end) _) as name) ->
                Space.chomp E.ModuleSpace
                    |> P.bind
                        (\_ ->
                            P.oneOf E.ImportEnd
                                [ Space.checkFreshLine E.ImportEnd
                                    |> P.fmap (\_ -> Src.CASTS_Import name Nothing (Src.CASTS_Explicit []))
                                , Space.checkIndent end E.ImportEnd
                                    |> P.bind
                                        (\_ ->
                                            P.oneOf E.ImportAs
                                                [ chompAs name
                                                , chompExposing name Nothing
                                                ]
                                        )
                                ]
                        )
            )


chompAs : A.CRA_Located Name.CDN_Name -> P.Parser E.Module Src.CASTS_Import
chompAs name =
    Keyword.as_ E.ImportAs
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias)
        |> P.bind (\_ -> Var.upper E.ImportAlias)
        |> P.bind
            (\alias ->
                P.getPosition
                    |> P.bind
                        (\end ->
                            Space.chomp E.ModuleSpace
                                |> P.bind
                                    (\_ ->
                                        P.oneOf E.ImportEnd
                                            [ Space.checkFreshLine E.ImportEnd
                                                |> P.fmap (\_ -> Src.CASTS_Import name (Just alias) (Src.CASTS_Explicit []))
                                            , Space.checkIndent end E.ImportEnd
                                                |> P.bind (\_ -> chompExposing name (Just alias))
                                            ]
                                    )
                        )
            )


chompExposing : A.CRA_Located Name.CDN_Name -> Maybe Name.CDN_Name -> P.Parser E.Module Src.CASTS_Import
chompExposing name maybeAlias =
    Keyword.exposing_ E.ImportExposing
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList)
        |> P.bind (\_ -> P.specialize E.ImportExposingList exposing_)
        |> P.bind
            (\exposed ->
                freshLine E.ImportEnd
                    |> P.fmap (\_ -> Src.CASTS_Import name maybeAlias exposed)
            )



-- LISTING


exposing_ : P.Parser E.Exposing Src.CASTS_Exposing
exposing_ =
    P.word1 '(' E.ExposingStart
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue)
        |> P.bind
            (\_ ->
                P.oneOf E.ExposingValue
                    [ P.word2 '.' '.' E.ExposingValue
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd)
                        |> P.bind (\_ -> P.word1 ')' E.ExposingEnd)
                        |> P.fmap (\_ -> Src.CASTS_Open)
                    , chompExposed
                        |> P.bind
                            (\exposed ->
                                Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                    |> P.bind (\_ -> exposingHelp [ exposed ])
                            )
                    ]
            )


exposingHelp : List Src.CASTS_Exposed -> P.Parser E.Exposing Src.CASTS_Exposing
exposingHelp revExposed =
    P.oneOf E.ExposingEnd
        [ P.word1 ',' E.ExposingEnd
            |> P.bind (\_ -> Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue)
            |> P.bind (\_ -> chompExposed)
            |> P.bind
                (\exposed ->
                    Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                        |> P.bind (\_ -> exposingHelp (exposed :: revExposed))
                )
        , P.word1 ')' E.ExposingEnd
            |> P.fmap (\_ -> Src.CASTS_Explicit (List.reverse revExposed))
        ]


chompExposed : P.Parser E.Exposing Src.CASTS_Exposed
chompExposed =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.ExposingValue
                    [ Var.lower E.ExposingValue
                        |> P.bind
                            (\name ->
                                P.getPosition
                                    |> P.fmap (\end -> Src.CASTS_Lower <| A.at start end name)
                            )
                    , P.word1 '(' E.ExposingValue
                        |> P.bind (\_ -> Symbol.operator E.ExposingOperator E.ExposingOperatorReserved)
                        |> P.bind
                            (\op ->
                                P.word1 ')' E.ExposingOperatorRightParen
                                    |> P.bind (\_ -> P.getPosition)
                                    |> P.fmap (\end -> Src.CASTS_Operator (A.CRA_Region start end) op)
                            )
                    , Var.upper E.ExposingValue
                        |> P.bind
                            (\name ->
                                P.getPosition
                                    |> P.bind
                                        (\end ->
                                            Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                                |> P.bind
                                                    (\_ ->
                                                        privacy
                                                            |> P.fmap (Src.CASTS_Upper (A.at start end name))
                                                    )
                                        )
                            )
                    ]
            )


privacy : P.Parser E.Exposing Src.CASTS_Privacy
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
                                    |> P.fmap (\_ -> Src.CASTS_Public (A.CRA_Region start end))
                            )
                )
        ]
        Src.CASTS_Private
