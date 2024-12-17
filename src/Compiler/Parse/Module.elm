module Compiler.Parse.Module exposing
    ( ProjectType(..)
    , chompImport
    , chompImports
    , fromByteString
    , isKernel
    )

import Compiler.Elm.Compiler.Imports as Imports
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Types as T



-- FROM BYTE STRING


fromByteString : ProjectType -> String -> Result E.Error T.CASTS_Module
fromByteString projectType source =
    case P.fromByteString (chompModule projectType) E.ModuleBadEnd source of
        Ok modul ->
            checkModule projectType modul

        Err err ->
            Err (E.ParseError err)



-- PROJECT TYPE


type ProjectType
    = Package T.CEP_Name
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
    , imports : List T.CASTS_Import
    , infixes : List (T.CRA_Located T.CASTS_Infix)
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


checkModule : ProjectType -> Module -> Result E.Error T.CASTS_Module
checkModule projectType module_ =
    let
        ( ( values, unions ), ( aliases, ports ) ) =
            categorizeDecls [] [] [] [] module_.decls
    in
    case module_.header of
        Just { name, effects, exports, docs } ->
            checkEffects projectType ports effects
                |> Result.map
                    (T.CASTS_Module
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
                (T.CASTS_Module
                    Nothing
                    (T.CRA_At A.one T.CASTS_Open)
                    (T.CASTS_NoDocs A.one)
                    module_.imports
                    values
                    unions
                    aliases
                    module_.infixes
                    (case ports of
                        [] ->
                            T.CASTS_NoEffects

                        _ ->
                            T.CASTS_Ports ports
                    )
                )


checkEffects : ProjectType -> List T.CASTS_Port -> Effects -> Result E.Error T.CASTS_Effects
checkEffects projectType ports effects =
    case effects of
        NoEffects region ->
            case ports of
                [] ->
                    Ok T.CASTS_NoEffects

                (T.CASTS_Port name _) :: _ ->
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
                            Ok (T.CASTS_Ports ports)

        Manager region manager ->
            if isKernel projectType then
                case ports of
                    [] ->
                        Ok (T.CASTS_Manager region manager)

                    _ :: _ ->
                        Err (E.UnexpectedPort region)

            else
                Err (E.NoEffectsOutsideKernel region)


categorizeDecls : List (T.CRA_Located T.CASTS_Value) -> List (T.CRA_Located T.CASTS_Union) -> List (T.CRA_Located T.CASTS_Alias) -> List T.CASTS_Port -> List Decl.Decl -> ( ( List (T.CRA_Located T.CASTS_Value), List (T.CRA_Located T.CASTS_Union) ), ( List (T.CRA_Located T.CASTS_Alias), List T.CASTS_Port ) )
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


toDocs : Result T.CRA_Region T.CASTS_Comment -> List Decl.Decl -> T.CASTS_Docs
toDocs comment decls =
    case comment of
        Ok overview ->
            T.CASTS_YesDocs overview (getComments decls [])

        Err region ->
            T.CASTS_NoDocs region


getComments : List Decl.Decl -> List ( T.CDN_Name, T.CASTS_Comment ) -> List ( T.CDN_Name, T.CASTS_Comment )
getComments decls comments =
    case decls of
        [] ->
            comments

        decl :: otherDecls ->
            case decl of
                Decl.Value c (T.CRA_At _ (T.CASTS_Value n _ _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Union c (T.CRA_At _ (T.CASTS_Union n _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Alias c (T.CRA_At _ (T.CASTS_Alias n _ _)) ->
                    getComments otherDecls (addComment c n comments)

                Decl.Port c (T.CASTS_Port n _) ->
                    getComments otherDecls (addComment c n comments)


addComment : Maybe T.CASTS_Comment -> T.CRA_Located T.CDN_Name -> List ( T.CDN_Name, T.CASTS_Comment ) -> List ( T.CDN_Name, T.CASTS_Comment )
addComment maybeComment (T.CRA_At _ name) comments =
    case maybeComment of
        Just comment ->
            ( name, comment ) :: comments

        Nothing ->
            comments



-- FRESH LINES


freshLine : (T.CPP_Row -> T.CPP_Col -> E.Module) -> P.Parser E.Module ()
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


chompInfixes : List (T.CRA_Located T.CASTS_Infix) -> P.Parser E.Module (List (T.CRA_Located T.CASTS_Infix))
chompInfixes infixes =
    P.oneOfWithFallback
        [ Decl.infix_
            |> P.bind (\binop -> chompInfixes (binop :: infixes))
        ]
        infixes



-- MODULE DOC COMMENT


chompModuleDocCommentSpace : P.Parser E.Module (Result T.CRA_Region T.CASTS_Comment)
chompModuleDocCommentSpace =
    P.addLocation (freshLine E.FreshLine)
        |> P.bind
            (\(T.CRA_At region ()) ->
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
    { name : T.CRA_Located T.CDN_Name
    , effects : Effects
    , exports : T.CRA_Located T.CASTS_Exposing
    , docs : Result T.CRA_Region T.CASTS_Comment
    }


type Effects
    = NoEffects T.CRA_Region
    | Ports T.CRA_Region
    | Manager T.CRA_Region T.CASTS_Manager


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
                                                                            (NoEffects (T.CRA_Region start effectEnd))
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
                                                                            (Ports (T.CRA_Region start effectEnd))
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
                                                                                    Header name (Manager (T.CRA_Region start effectEnd) manager) exports comment
                                                                            )
                                                                )
                                                    )
                                        )
                            )
                    ]
                    -- default header
                    Nothing
            )


chompManager : P.Parser E.Module T.CASTS_Manager
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
                                                    |> P.fmap (\_ -> T.CASTS_Cmd cmd)
                                                , P.word1 ',' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.bind (\_ -> chompSubscription)
                                                    |> P.bind
                                                        (\sub ->
                                                            spaces_em
                                                                |> P.bind (\_ -> P.word1 '}' E.Effect)
                                                                |> P.bind (\_ -> spaces_em)
                                                                |> P.fmap (\_ -> T.CASTS_Fx cmd sub)
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
                                                    |> P.fmap (\_ -> T.CASTS_Sub sub)
                                                , P.word1 ',' E.Effect
                                                    |> P.bind (\_ -> spaces_em)
                                                    |> P.bind (\_ -> chompCommand)
                                                    |> P.bind
                                                        (\cmd ->
                                                            spaces_em
                                                                |> P.bind (\_ -> P.word1 '}' E.Effect)
                                                                |> P.bind (\_ -> spaces_em)
                                                                |> P.fmap (\_ -> T.CASTS_Fx cmd sub)
                                                        )
                                                ]
                                        )
                            )
                    ]
            )


chompCommand : P.Parser E.Module (T.CRA_Located T.CDN_Name)
chompCommand =
    Keyword.command_ E.Effect
        |> P.bind (\_ -> spaces_em)
        |> P.bind (\_ -> P.word1 '=' E.Effect)
        |> P.bind (\_ -> spaces_em)
        |> P.bind (\_ -> P.addLocation (Var.upper E.Effect))


chompSubscription : P.Parser E.Module (T.CRA_Located T.CDN_Name)
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


chompImports : List T.CASTS_Import -> P.Parser E.Module (List T.CASTS_Import)
chompImports is =
    P.oneOfWithFallback
        [ chompImport
            |> P.bind (\i -> chompImports (i :: is))
        ]
        (List.reverse is)


chompImport : P.Parser E.Module T.CASTS_Import
chompImport =
    Keyword.import_ E.ImportStart
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName)
        |> P.bind (\_ -> P.addLocation (Var.moduleName E.ImportName))
        |> P.bind
            (\((T.CRA_At (T.CRA_Region _ end) _) as name) ->
                Space.chomp E.ModuleSpace
                    |> P.bind
                        (\_ ->
                            P.oneOf E.ImportEnd
                                [ Space.checkFreshLine E.ImportEnd
                                    |> P.fmap (\_ -> T.CASTS_Import name Nothing (T.CASTS_Explicit []))
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


chompAs : T.CRA_Located T.CDN_Name -> P.Parser E.Module T.CASTS_Import
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
                                                |> P.fmap (\_ -> T.CASTS_Import name (Just alias) (T.CASTS_Explicit []))
                                            , Space.checkIndent end E.ImportEnd
                                                |> P.bind (\_ -> chompExposing name (Just alias))
                                            ]
                                    )
                        )
            )


chompExposing : T.CRA_Located T.CDN_Name -> Maybe T.CDN_Name -> P.Parser E.Module T.CASTS_Import
chompExposing name maybeAlias =
    Keyword.exposing_ E.ImportExposing
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList)
        |> P.bind (\_ -> P.specialize E.ImportExposingList exposing_)
        |> P.bind
            (\exposed ->
                freshLine E.ImportEnd
                    |> P.fmap (\_ -> T.CASTS_Import name maybeAlias exposed)
            )



-- LISTING


exposing_ : P.Parser E.Exposing T.CASTS_Exposing
exposing_ =
    P.word1 '(' E.ExposingStart
        |> P.bind (\_ -> Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue)
        |> P.bind
            (\_ ->
                P.oneOf E.ExposingValue
                    [ P.word2 '.' '.' E.ExposingValue
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd)
                        |> P.bind (\_ -> P.word1 ')' E.ExposingEnd)
                        |> P.fmap (\_ -> T.CASTS_Open)
                    , chompExposed
                        |> P.bind
                            (\exposed ->
                                Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                    |> P.bind (\_ -> exposingHelp [ exposed ])
                            )
                    ]
            )


exposingHelp : List T.CASTS_Exposed -> P.Parser E.Exposing T.CASTS_Exposing
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
            |> P.fmap (\_ -> T.CASTS_Explicit (List.reverse revExposed))
        ]


chompExposed : P.Parser E.Exposing T.CASTS_Exposed
chompExposed =
    P.getPosition
        |> P.bind
            (\start ->
                P.oneOf E.ExposingValue
                    [ Var.lower E.ExposingValue
                        |> P.bind
                            (\name ->
                                P.getPosition
                                    |> P.fmap (\end -> T.CASTS_Lower <| A.at start end name)
                            )
                    , P.word1 '(' E.ExposingValue
                        |> P.bind (\_ -> Symbol.operator E.ExposingOperator E.ExposingOperatorReserved)
                        |> P.bind
                            (\op ->
                                P.word1 ')' E.ExposingOperatorRightParen
                                    |> P.bind (\_ -> P.getPosition)
                                    |> P.fmap (\end -> T.CASTS_Operator (T.CRA_Region start end) op)
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
                                                            |> P.fmap (T.CASTS_Upper (A.at start end name))
                                                    )
                                        )
                            )
                    ]
            )


privacy : P.Parser E.Exposing T.CASTS_Privacy
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
                                    |> P.fmap (\_ -> T.CASTS_Public (T.CRA_Region start end))
                            )
                )
        ]
        T.CASTS_Private
