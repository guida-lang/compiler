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
import Types as T



-- DECLARATION


type Decl
    = Value (Maybe T.CASTS_Comment) (T.CRA_Located T.CASTS_Value)
    | Union (Maybe T.CASTS_Comment) (T.CRA_Located T.CASTS_Union)
    | Alias (Maybe T.CASTS_Comment) (T.CRA_Located T.CASTS_Alias)
    | Port (Maybe T.CASTS_Comment) T.CASTS_Port


declaration : Space.Parser T.CRES_Decl Decl
declaration =
    chompDocComment
        |> P.bind
            (\maybeDocs ->
                P.getPosition
                    |> P.bind
                        (\start ->
                            P.oneOf T.CRES_DeclStart
                                [ typeDecl maybeDocs start
                                , portDecl maybeDocs
                                , valueDecl maybeDocs start
                                ]
                        )
            )



-- DOC COMMENT


chompDocComment : P.Parser T.CRES_Decl (Maybe T.CASTS_Comment)
chompDocComment =
    P.oneOfWithFallback
        [ Space.docComment T.CRES_DeclStart T.CRES_DeclSpace
            |> P.bind
                (\docComment ->
                    Space.chomp T.CRES_DeclSpace
                        |> P.bind (\_ -> Space.checkFreshLine T.CRES_DeclFreshLineAfterDocComment)
                        |> P.fmap (\_ -> Just docComment)
                )
        ]
        Nothing



-- DEFINITION and ANNOTATION


valueDecl : Maybe T.CASTS_Comment -> T.CRA_Position -> Space.Parser T.CRES_Decl Decl
valueDecl maybeDocs start =
    Var.lower T.CRES_DeclStart
        |> P.bind
            (\name ->
                P.getPosition
                    |> P.bind
                        (\end ->
                            P.specialize (T.CRES_DeclDef name) <|
                                (Space.chompAndCheckIndent T.CRES_DeclDefSpace T.CRES_DeclDefIndentEquals
                                    |> P.bind
                                        (\_ ->
                                            P.oneOf T.CRES_DeclDefEquals
                                                [ P.word1 ':' T.CRES_DeclDefEquals
                                                    |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_DeclDefSpace T.CRES_DeclDefIndentType)
                                                    |> P.bind (\_ -> P.specialize T.CRES_DeclDefType Type.expression)
                                                    |> P.bind
                                                        (\( tipe, _ ) ->
                                                            Space.checkFreshLine T.CRES_DeclDefNameRepeat
                                                                |> P.bind (\_ -> chompMatchingName name)
                                                                |> P.bind
                                                                    (\defName ->
                                                                        Space.chompAndCheckIndent T.CRES_DeclDefSpace T.CRES_DeclDefIndentEquals
                                                                            |> P.bind (\_ -> chompDefArgsAndBody maybeDocs start defName (Just tipe) [])
                                                                    )
                                                        )
                                                , chompDefArgsAndBody maybeDocs start (A.at start end name) Nothing []
                                                ]
                                        )
                                )
                        )
            )


chompDefArgsAndBody : Maybe T.CASTS_Comment -> T.CRA_Position -> T.CRA_Located T.CDN_Name -> Maybe T.CASTS_Type -> List T.CASTS_Pattern -> Space.Parser T.CRES_DeclDef Decl
chompDefArgsAndBody maybeDocs start name tipe revArgs =
    P.oneOf T.CRES_DeclDefEquals
        [ P.specialize T.CRES_DeclDefArg Pattern.term
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent T.CRES_DeclDefSpace T.CRES_DeclDefIndentEquals
                        |> P.bind (\_ -> chompDefArgsAndBody maybeDocs start name tipe (arg :: revArgs))
                )
        , P.word1 '=' T.CRES_DeclDefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_DeclDefSpace T.CRES_DeclDefIndentBody)
            |> P.bind (\_ -> P.specialize T.CRES_DeclDefBody Expr.expression)
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


chompMatchingName : T.CDN_Name -> P.Parser T.CRES_DeclDef (T.CRA_Located T.CDN_Name)
chompMatchingName expectedName =
    let
        (P.Parser parserL) =
            Var.lower T.CRES_DeclDefNameRepeat
    in
    P.Parser <|
        \((P.State _ _ _ _ sr sc) as state) ->
            Result.andThen
                (\(P.POk status name ((P.State _ _ _ _ er ec) as newState)) ->
                    if expectedName == name then
                        Ok (P.POk status (T.CRA_At (T.CRA_Region (T.CRA_Position sr sc) (T.CRA_Position er ec)) name) newState)

                    else
                        Err (P.PErr status sr sc (T.CRES_DeclDefNameMatch name))
                )
                (parserL state)



-- TYPE DECLARATIONS


typeDecl : Maybe T.CASTS_Comment -> T.CRA_Position -> Space.Parser T.CRES_Decl Decl
typeDecl maybeDocs start =
    P.inContext T.CRES_DeclType (Keyword.type_ T.CRES_DeclStart) <|
        (Space.chompAndCheckIndent T.CRES_DT_Space T.CRES_DT_IndentName
            |> P.bind
                (\_ ->
                    P.oneOf T.CRES_DT_Name
                        [ P.inContext T.CRES_DT_Alias (Keyword.alias_ T.CRES_DT_Name) <|
                            (Space.chompAndCheckIndent T.CRES_AliasSpace T.CRES_AliasIndentEquals
                                |> P.bind (\_ -> chompAliasNameToEquals)
                                |> P.bind
                                    (\( name, args ) ->
                                        P.specialize T.CRES_AliasBody Type.expression
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
                        , P.specialize T.CRES_DT_Union <|
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


chompAliasNameToEquals : P.Parser T.CRES_TypeAlias ( T.CRA_Located T.CDN_Name, List (T.CRA_Located T.CDN_Name) )
chompAliasNameToEquals =
    P.addLocation (Var.upper T.CRES_AliasName)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent T.CRES_AliasSpace T.CRES_AliasIndentEquals
                    |> P.bind (\_ -> chompAliasNameToEqualsHelp name [])
            )


chompAliasNameToEqualsHelp : T.CRA_Located T.CDN_Name -> List (T.CRA_Located T.CDN_Name) -> P.Parser T.CRES_TypeAlias ( T.CRA_Located T.CDN_Name, List (T.CRA_Located T.CDN_Name) )
chompAliasNameToEqualsHelp name args =
    P.oneOf T.CRES_AliasEquals
        [ P.addLocation (Var.lower T.CRES_AliasEquals)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent T.CRES_AliasSpace T.CRES_AliasIndentEquals
                        |> P.bind (\_ -> chompAliasNameToEqualsHelp name (arg :: args))
                )
        , P.word1 '=' T.CRES_AliasEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_AliasSpace T.CRES_AliasIndentBody)
            |> P.fmap (\_ -> ( name, List.reverse args ))
        ]



-- CUSTOM TYPES


chompCustomNameToEquals : P.Parser T.CRES_CustomType ( T.CRA_Located T.CDN_Name, List (T.CRA_Located T.CDN_Name) )
chompCustomNameToEquals =
    P.addLocation (Var.upper T.CRES_CT_Name)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent T.CRES_CT_Space T.CRES_CT_IndentEquals
                    |> P.bind (\_ -> chompCustomNameToEqualsHelp name [])
            )


chompCustomNameToEqualsHelp : T.CRA_Located T.CDN_Name -> List (T.CRA_Located T.CDN_Name) -> P.Parser T.CRES_CustomType ( T.CRA_Located T.CDN_Name, List (T.CRA_Located T.CDN_Name) )
chompCustomNameToEqualsHelp name args =
    P.oneOf T.CRES_CT_Equals
        [ P.addLocation (Var.lower T.CRES_CT_Equals)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent T.CRES_CT_Space T.CRES_CT_IndentEquals
                        |> P.bind (\_ -> chompCustomNameToEqualsHelp name (arg :: args))
                )
        , P.word1 '=' T.CRES_CT_Equals
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_CT_Space T.CRES_CT_IndentAfterEquals)
            |> P.fmap (\_ -> ( name, List.reverse args ))
        ]


chompVariants : List ( T.CRA_Located T.CDN_Name, List T.CASTS_Type ) -> T.CRA_Position -> Space.Parser T.CRES_CustomType (List ( T.CRA_Located T.CDN_Name, List T.CASTS_Type ))
chompVariants variants end =
    P.oneOfWithFallback
        [ Space.checkIndent end T.CRES_CT_IndentBar
            |> P.bind (\_ -> P.word1 '|' T.CRES_CT_Bar)
            |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_CT_Space T.CRES_CT_IndentAfterBar)
            |> P.bind (\_ -> Type.variant)
            |> P.bind (\( variant, newEnd ) -> chompVariants (variant :: variants) newEnd)
        ]
        ( List.reverse variants, end )



-- PORT


portDecl : Maybe T.CASTS_Comment -> Space.Parser T.CRES_Decl Decl
portDecl maybeDocs =
    P.inContext T.CRES_Port (Keyword.port_ T.CRES_DeclStart) <|
        (Space.chompAndCheckIndent T.CRES_PortSpace T.CRES_PortIndentName
            |> P.bind (\_ -> P.addLocation (Var.lower T.CRES_PortName))
            |> P.bind
                (\name ->
                    Space.chompAndCheckIndent T.CRES_PortSpace T.CRES_PortIndentColon
                        |> P.bind (\_ -> P.word1 ':' T.CRES_PortColon)
                        |> P.bind (\_ -> Space.chompAndCheckIndent T.CRES_PortSpace T.CRES_PortIndentType)
                        |> P.bind
                            (\_ ->
                                P.specialize T.CRES_PortType Type.expression
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


infix_ : P.Parser T.CRES_Module (T.CRA_Located T.CASTS_Infix)
infix_ =
    let
        err : T.CPP_Row -> T.CPP_Col -> T.CRES_Module
        err =
            T.CRES_Infix

        err_ : a -> T.CPP_Row -> T.CPP_Col -> T.CRES_Module
        err_ =
            \_ -> T.CRES_Infix
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
