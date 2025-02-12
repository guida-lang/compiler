module Compiler.Parse.Declaration exposing
    ( Decl(..)
    , declaration
    , infix_
    )

import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.Name exposing (Name)
import Compiler.Parse.Expression as Expr
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
import Compiler.Parse.Type as Type
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E



-- DECLARATION


type Decl
    = Value (Maybe Src.Comment) (A.Located Src.Value)
    | Union (Maybe Src.Comment) (A.Located Src.Union)
    | Alias (Maybe Src.Comment) (A.Located Src.Alias)
    | Port (Maybe Src.Comment) Src.Port


declaration : SyntaxVersion -> Space.Parser E.Decl Decl
declaration syntaxVersion =
    chompDocComment
        |> P.bind
            (\maybeDocs ->
                P.getPosition
                    |> P.bind
                        (\start ->
                            P.oneOf E.DeclStart
                                [ typeDecl maybeDocs start
                                , portDecl maybeDocs
                                , valueDecl syntaxVersion maybeDocs start
                                ]
                        )
            )



-- DOC COMMENT


chompDocComment : P.Parser E.Decl (Maybe Src.Comment)
chompDocComment =
    P.oneOfWithFallback
        [ Space.docComment E.DeclStart E.DeclSpace
            |> P.bind
                (\docComment ->
                    Space.chomp E.DeclSpace
                        |> P.bind (\_ -> Space.checkFreshLine E.DeclFreshLineAfterDocComment)
                        |> P.fmap (\_ -> Just docComment)
                )
        ]
        Nothing



-- DEFINITION and ANNOTATION


valueDecl : SyntaxVersion -> Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Decl
valueDecl syntaxVersion maybeDocs start =
    Var.lower E.DeclStart
        |> P.bind
            (\name ->
                P.getPosition
                    |> P.bind
                        (\end ->
                            P.specialize (E.DeclDef name) <|
                                (Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                                    |> P.bind
                                        (\_ ->
                                            P.oneOf E.DeclDefEquals
                                                [ P.word1 ':' E.DeclDefEquals
                                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentType)
                                                    |> P.bind (\_ -> P.specialize E.DeclDefType Type.expression)
                                                    |> P.bind
                                                        (\( tipe, _ ) ->
                                                            Space.checkFreshLine E.DeclDefNameRepeat
                                                                |> P.bind (\_ -> chompMatchingName name)
                                                                |> P.bind
                                                                    (\defName ->
                                                                        Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                                                                            |> P.bind (\_ -> chompDefArgsAndBody syntaxVersion maybeDocs start defName (Just tipe) [])
                                                                    )
                                                        )
                                                , chompDefArgsAndBody syntaxVersion maybeDocs start (A.at start end name) Nothing []
                                                ]
                                        )
                                )
                        )
            )


chompDefArgsAndBody : SyntaxVersion -> Maybe Src.Comment -> A.Position -> A.Located Name -> Maybe Src.Type -> List Src.Pattern -> Space.Parser E.DeclDef Decl
chompDefArgsAndBody syntaxVersion maybeDocs start name tipe revArgs =
    P.oneOf E.DeclDefEquals
        [ P.specialize E.DeclDefArg (Pattern.term syntaxVersion)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                        |> P.bind (\_ -> chompDefArgsAndBody syntaxVersion maybeDocs start name tipe (arg :: revArgs))
                )
        , P.word1 '=' E.DeclDefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody)
            |> P.bind (\_ -> P.specialize E.DeclDefBody (Expr.expression syntaxVersion))
            |> P.fmap
                (\( body, end ) ->
                    let
                        value : Src.Value
                        value =
                            Src.Value name (List.reverse revArgs) body tipe

                        avalue : A.Located Src.Value
                        avalue =
                            A.at start end value
                    in
                    ( Value maybeDocs avalue, end )
                )
        ]


chompMatchingName : Name -> P.Parser E.DeclDef (A.Located Name)
chompMatchingName expectedName =
    let
        (P.Parser parserL) =
            Var.lower E.DeclDefNameRepeat
    in
    P.Parser <|
        \((P.State _ _ _ _ sr sc) as state) ->
            case parserL state of
                P.Cok name ((P.State _ _ _ _ er ec) as newState) ->
                    if expectedName == name then
                        P.Cok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState

                    else
                        P.Cerr sr sc (E.DeclDefNameMatch name)

                P.Eok name ((P.State _ _ _ _ er ec) as newState) ->
                    if expectedName == name then
                        P.Eok (A.At (A.Region (A.Position sr sc) (A.Position er ec)) name) newState

                    else
                        P.Eerr sr sc (E.DeclDefNameMatch name)

                P.Cerr r c t ->
                    P.Cerr r c t

                P.Eerr r c t ->
                    P.Eerr r c t



-- TYPE DECLARATIONS


typeDecl : Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Decl
typeDecl maybeDocs start =
    P.inContext E.DeclType (Keyword.type_ E.DeclStart) <|
        (Space.chompAndCheckIndent E.DT_Space E.DT_IndentName
            |> P.bind
                (\_ ->
                    P.oneOf E.DT_Name
                        [ P.inContext E.DT_Alias (Keyword.alias_ E.DT_Name) <|
                            (Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                                |> P.bind (\_ -> chompAliasNameToEquals)
                                |> P.bind
                                    (\( name, args ) ->
                                        P.specialize E.AliasBody Type.expression
                                            |> P.fmap
                                                (\( tipe, end ) ->
                                                    let
                                                        alias_ : A.Located Src.Alias
                                                        alias_ =
                                                            A.at start end (Src.Alias name args tipe)
                                                    in
                                                    ( Alias maybeDocs alias_, end )
                                                )
                                    )
                            )
                        , P.specialize E.DT_Union <|
                            (chompCustomNameToEquals
                                |> P.bind
                                    (\( name, args ) ->
                                        Type.variant
                                            |> P.bind
                                                (\( firstVariant, firstEnd ) ->
                                                    chompVariants [ firstVariant ] firstEnd
                                                        |> P.fmap
                                                            (\( variants, end ) ->
                                                                let
                                                                    union : A.Located Src.Union
                                                                    union =
                                                                        A.at start end (Src.Union name args variants)
                                                                in
                                                                ( Union maybeDocs union, end )
                                                            )
                                                )
                                    )
                            )
                        ]
                )
        )



-- TYPE ALIASES


chompAliasNameToEquals : P.Parser E.TypeAlias ( A.Located Name, List (A.Located Name) )
chompAliasNameToEquals =
    P.addLocation (Var.upper E.AliasName)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                    |> P.bind (\_ -> chompAliasNameToEqualsHelp name [])
            )


chompAliasNameToEqualsHelp : A.Located Name -> List (A.Located Name) -> P.Parser E.TypeAlias ( A.Located Name, List (A.Located Name) )
chompAliasNameToEqualsHelp name args =
    P.oneOf E.AliasEquals
        [ P.addLocation (Var.lower E.AliasEquals)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                        |> P.bind (\_ -> chompAliasNameToEqualsHelp name (arg :: args))
                )
        , P.word1 '=' E.AliasEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.AliasSpace E.AliasIndentBody)
            |> P.fmap (\_ -> ( name, List.reverse args ))
        ]



-- CUSTOM TYPES


chompCustomNameToEquals : P.Parser E.CustomType ( A.Located Name, List (A.Located Name) )
chompCustomNameToEquals =
    P.addLocation (Var.upper E.CT_Name)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                    |> P.bind (\_ -> chompCustomNameToEqualsHelp name [])
            )


chompCustomNameToEqualsHelp : A.Located Name -> List (A.Located Name) -> P.Parser E.CustomType ( A.Located Name, List (A.Located Name) )
chompCustomNameToEqualsHelp name args =
    P.oneOf E.CT_Equals
        [ P.addLocation (Var.lower E.CT_Equals)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                        |> P.bind (\_ -> chompCustomNameToEqualsHelp name (arg :: args))
                )
        , P.word1 '=' E.CT_Equals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterEquals)
            |> P.fmap (\_ -> ( name, List.reverse args ))
        ]


chompVariants : List ( A.Located Name, List Src.Type ) -> A.Position -> Space.Parser E.CustomType (List ( A.Located Name, List Src.Type ))
chompVariants variants end =
    P.oneOfWithFallback
        [ Space.checkIndent end E.CT_IndentBar
            |> P.bind (\_ -> P.word1 '|' E.CT_Bar)
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterBar)
            |> P.bind (\_ -> Type.variant)
            |> P.bind (\( variant, newEnd ) -> chompVariants (variant :: variants) newEnd)
        ]
        ( List.reverse variants, end )



-- PORT


portDecl : Maybe Src.Comment -> Space.Parser E.Decl Decl
portDecl maybeDocs =
    P.inContext E.Port (Keyword.port_ E.DeclStart) <|
        (Space.chompAndCheckIndent E.PortSpace E.PortIndentName
            |> P.bind (\_ -> P.addLocation (Var.lower E.PortName))
            |> P.bind
                (\name ->
                    Space.chompAndCheckIndent E.PortSpace E.PortIndentColon
                        |> P.bind (\_ -> P.word1 ':' E.PortColon)
                        |> P.bind (\_ -> Space.chompAndCheckIndent E.PortSpace E.PortIndentType)
                        |> P.bind
                            (\_ ->
                                P.specialize E.PortType Type.expression
                                    |> P.fmap
                                        (\( tipe, end ) ->
                                            ( Port maybeDocs (Src.Port name tipe)
                                            , end
                                            )
                                        )
                            )
                )
        )



-- INFIX
-- INVARIANT: always chomps to a freshline
--


infix_ : P.Parser E.Module (A.Located Src.Infix)
infix_ =
    let
        err : P.Row -> P.Col -> E.Module
        err =
            E.Infix

        err_ : a -> P.Row -> P.Col -> E.Module
        err_ =
            \_ -> E.Infix
    in
    P.getPosition
        |> P.bind
            (\start ->
                Keyword.infix_ err
                    |> P.bind (\_ -> Space.chompAndCheckIndent err_ err)
                    |> P.bind
                        (\_ ->
                            P.oneOf err
                                [ Keyword.left_ err |> P.fmap (\_ -> Binop.Left)
                                , Keyword.right_ err |> P.fmap (\_ -> Binop.Right)
                                , Keyword.non_ err |> P.fmap (\_ -> Binop.Non)
                                ]
                        )
                    |> P.bind
                        (\associativity ->
                            Space.chompAndCheckIndent err_ err
                                |> P.bind (\_ -> Number.precedence err)
                                |> P.bind
                                    (\precedence ->
                                        Space.chompAndCheckIndent err_ err
                                            |> P.bind (\_ -> P.word1 '(' err)
                                            |> P.bind (\_ -> Symbol.operator err err_)
                                            |> P.bind
                                                (\op ->
                                                    P.word1 ')' err
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent err_ err)
                                                        |> P.bind (\_ -> P.word1 '=' err)
                                                        |> P.bind (\_ -> Space.chompAndCheckIndent err_ err)
                                                        |> P.bind (\_ -> Var.lower err)
                                                        |> P.bind
                                                            (\name ->
                                                                P.getPosition
                                                                    |> P.bind
                                                                        (\end ->
                                                                            Space.chomp err_
                                                                                |> P.bind (\_ -> Space.checkFreshLine err)
                                                                                |> P.fmap (\_ -> A.at start end (Src.Infix op associativity precedence name))
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )
