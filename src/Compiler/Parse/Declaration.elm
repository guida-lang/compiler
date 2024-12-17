module Compiler.Parse.Declaration exposing
    ( Decl(..)
    , declaration
    , infix_
    )

import Compiler.Parse.Expression as Expr
import Compiler.Parse.Keyword as Keyword
import Compiler.Parse.Number as Number
import Compiler.Parse.Pattern as Pattern
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Symbol as Symbol
import Compiler.Parse.Type as Type
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Syntax as E
import Types as T



-- DECLARATION


type Decl
    = Value (Maybe T.CASTS_Comment) (T.CRA_Located T.CASTS_Value)
    | Union (Maybe T.CASTS_Comment) (T.CRA_Located T.CASTS_Union)
    | Alias (Maybe T.CASTS_Comment) (T.CRA_Located T.CASTS_Alias)
    | Port (Maybe T.CASTS_Comment) T.CASTS_Port


declaration : Space.Parser E.Decl Decl
declaration =
    chompDocComment
        |> P.bind
            (\maybeDocs ->
                P.getPosition
                    |> P.bind
                        (\start ->
                            P.oneOf E.DeclStart
                                [ typeDecl maybeDocs start
                                , portDecl maybeDocs
                                , valueDecl maybeDocs start
                                ]
                        )
            )



-- DOC COMMENT


chompDocComment : P.Parser E.Decl (Maybe T.CASTS_Comment)
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


valueDecl : Maybe T.CASTS_Comment -> T.CRA_Position -> Space.Parser E.Decl Decl
valueDecl maybeDocs start =
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
                                                                            |> P.bind (\_ -> chompDefArgsAndBody maybeDocs start defName (Just tipe) [])
                                                                    )
                                                        )
                                                , chompDefArgsAndBody maybeDocs start (A.at start end name) Nothing []
                                                ]
                                        )
                                )
                        )
            )


chompDefArgsAndBody : Maybe T.CASTS_Comment -> T.CRA_Position -> T.CRA_Located T.CDN_Name -> Maybe T.CASTS_Type -> List T.CASTS_Pattern -> Space.Parser E.DeclDef Decl
chompDefArgsAndBody maybeDocs start name tipe revArgs =
    P.oneOf E.DeclDefEquals
        [ P.specialize E.DeclDefArg Pattern.term
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                        |> P.bind (\_ -> chompDefArgsAndBody maybeDocs start name tipe (arg :: revArgs))
                )
        , P.word1 '=' E.DeclDefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody)
            |> P.bind (\_ -> P.specialize E.DeclDefBody Expr.expression)
            |> P.fmap
                (\( body, end ) ->
                    let
                        value : T.CASTS_Value
                        value =
                            T.CASTS_Value name (List.reverse revArgs) body tipe

                        avalue : T.CRA_Located T.CASTS_Value
                        avalue =
                            A.at start end value
                    in
                    ( Value maybeDocs avalue, end )
                )
        ]


chompMatchingName : T.CDN_Name -> P.Parser E.DeclDef (T.CRA_Located T.CDN_Name)
chompMatchingName expectedName =
    let
        (P.Parser parserL) =
            Var.lower E.DeclDefNameRepeat
    in
    P.Parser <|
        \((P.State _ _ _ _ sr sc) as state) ->
            Result.andThen
                (\(P.POk status name ((P.State _ _ _ _ er ec) as newState)) ->
                    if expectedName == name then
                        Ok (P.POk status (T.CRA_At (T.CRA_Region (T.CRA_Position sr sc) (T.CRA_Position er ec)) name) newState)

                    else
                        Err (P.PErr status sr sc (E.DeclDefNameMatch name))
                )
                (parserL state)



-- TYPE DECLARATIONS


typeDecl : Maybe T.CASTS_Comment -> T.CRA_Position -> Space.Parser E.Decl Decl
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
                                                        alias : T.CRA_Located T.CASTS_Alias
                                                        alias =
                                                            A.at start end (T.CASTS_Alias name args tipe)
                                                    in
                                                    ( Alias maybeDocs alias, end )
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
                                                                    union : T.CRA_Located T.CASTS_Union
                                                                    union =
                                                                        A.at start end (T.CASTS_Union name args variants)
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


chompAliasNameToEquals : P.Parser E.TypeAlias ( T.CRA_Located T.CDN_Name, List (T.CRA_Located T.CDN_Name) )
chompAliasNameToEquals =
    P.addLocation (Var.upper E.AliasName)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                    |> P.bind (\_ -> chompAliasNameToEqualsHelp name [])
            )


chompAliasNameToEqualsHelp : T.CRA_Located T.CDN_Name -> List (T.CRA_Located T.CDN_Name) -> P.Parser E.TypeAlias ( T.CRA_Located T.CDN_Name, List (T.CRA_Located T.CDN_Name) )
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


chompCustomNameToEquals : P.Parser E.CustomType ( T.CRA_Located T.CDN_Name, List (T.CRA_Located T.CDN_Name) )
chompCustomNameToEquals =
    P.addLocation (Var.upper E.CT_Name)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                    |> P.bind (\_ -> chompCustomNameToEqualsHelp name [])
            )


chompCustomNameToEqualsHelp : T.CRA_Located T.CDN_Name -> List (T.CRA_Located T.CDN_Name) -> P.Parser E.CustomType ( T.CRA_Located T.CDN_Name, List (T.CRA_Located T.CDN_Name) )
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


chompVariants : List ( T.CRA_Located T.CDN_Name, List T.CASTS_Type ) -> T.CRA_Position -> Space.Parser E.CustomType (List ( T.CRA_Located T.CDN_Name, List T.CASTS_Type ))
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


portDecl : Maybe T.CASTS_Comment -> Space.Parser E.Decl Decl
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
                                            ( Port maybeDocs (T.CASTS_Port name tipe)
                                            , end
                                            )
                                        )
                            )
                )
        )



-- INFIX
-- INVARIANT: always chomps to a freshline
--


infix_ : P.Parser E.Module (T.CRA_Located T.CASTS_Infix)
infix_ =
    let
        err : T.CPP_Row -> T.CPP_Col -> E.Module
        err =
            E.Infix

        err_ : a -> T.CPP_Row -> T.CPP_Col -> E.Module
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
                                [ Keyword.left_ err |> P.fmap (\_ -> T.CASTUB_Left)
                                , Keyword.right_ err |> P.fmap (\_ -> T.CASTUB_Right)
                                , Keyword.non_ err |> P.fmap (\_ -> T.CASTUB_Non)
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
                                                                                |> P.fmap (\_ -> A.at start end (T.CASTS_Infix op associativity precedence name))
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )
