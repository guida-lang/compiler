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


declaration : SyntaxVersion -> Space.Parser E.Decl (Src.C2 Decl)
declaration syntaxVersion =
    chompDocComment
        |> P.bind
            (\( docComments, maybeDocs ) ->
                P.getPosition
                    |> P.bind
                        (\start ->
                            P.oneOf E.DeclStart
                                [ typeDecl maybeDocs start
                                , portDecl maybeDocs
                                , valueDecl syntaxVersion maybeDocs docComments start
                                ]
                        )
            )



-- DOC COMMENT


chompDocComment : P.Parser E.Decl (Src.C1 (Maybe Src.Comment))
chompDocComment =
    P.oneOfWithFallback
        [ Space.docComment E.DeclStart E.DeclSpace
            |> P.bind
                (\docComment ->
                    Space.chomp E.DeclSpace
                        |> P.bind
                            (\comments ->
                                Space.checkFreshLine E.DeclFreshLineAfterDocComment
                                    |> P.fmap (\_ -> ( comments, Just docComment ))
                            )
                )
        ]
        ( [], Nothing )



-- DEFINITION and ANNOTATION


valueDecl : SyntaxVersion -> Maybe Src.Comment -> Src.FComments -> A.Position -> Space.Parser E.Decl (Src.C2 Decl)
valueDecl syntaxVersion maybeDocs docComments start =
    Var.lower E.DeclStart
        |> P.bind
            (\name ->
                P.getPosition
                    |> P.bind
                        (\end ->
                            P.specialize (E.DeclDef name) <|
                                (Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                                    |> P.bind
                                        (\postNameComments ->
                                            P.oneOf E.DeclDefEquals
                                                [ P.word1 ':' E.DeclDefEquals
                                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentType)
                                                    |> P.bind
                                                        (\preTypeComments ->
                                                            P.specialize E.DeclDefType (Type.expression preTypeComments)
                                                                |> P.bind
                                                                    (\( ( ( preTipeComments, postTipeComments, _ ), tipe ), _ ) ->
                                                                        Space.checkFreshLine E.DeclDefNameRepeat
                                                                            |> P.bind (\_ -> chompMatchingName name)
                                                                            |> P.bind
                                                                                (\defName ->
                                                                                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                                                                                        |> P.bind
                                                                                            (\preArgComments ->
                                                                                                chompDefArgsAndBody syntaxVersion maybeDocs docComments start defName (Just ( postTipeComments, ( ( postNameComments, preTipeComments ), tipe ) )) preArgComments []
                                                                                            )
                                                                                )
                                                                    )
                                                        )
                                                , chompDefArgsAndBody syntaxVersion maybeDocs docComments start (A.at start end name) Nothing postNameComments []
                                                ]
                                        )
                                )
                        )
            )


chompDefArgsAndBody : SyntaxVersion -> Maybe Src.Comment -> Src.FComments -> A.Position -> A.Located Name -> Maybe (Src.C1 (Src.C2 Src.Type)) -> Src.FComments -> List (Src.C1 Src.Pattern) -> Space.Parser E.DeclDef (Src.C2 Decl)
chompDefArgsAndBody syntaxVersion maybeDocs docComments start name tipe preArgComments revArgs =
    P.oneOf E.DeclDefEquals
        [ P.specialize E.DeclDefArg (Pattern.term syntaxVersion)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                        |> P.bind
                            (\postArgComments ->
                                chompDefArgsAndBody syntaxVersion maybeDocs docComments start name tipe postArgComments (( preArgComments, arg ) :: revArgs)
                            )
                )
        , P.word1 '=' E.DeclDefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody)
            |> P.bind
                (\preBodyComments ->
                    P.specialize E.DeclDefBody (Expr.expression syntaxVersion)
                        |> P.fmap
                            (\( ( trailingComments, body ), end ) ->
                                let
                                    value : Src.Value
                                    value =
                                        Src.Value docComments ( preArgComments, name ) (List.reverse revArgs) ( preBodyComments, body ) tipe

                                    avalue : A.Located Src.Value
                                    avalue =
                                        A.at start end value
                                in
                                ( ( ( [], trailingComments ), Value maybeDocs avalue ), end )
                            )
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


typeDecl : Maybe Src.Comment -> A.Position -> Space.Parser E.Decl (Src.C2 Decl)
typeDecl maybeDocs start =
    P.inContext E.DeclType (Keyword.type_ E.DeclStart) <|
        (Space.chompAndCheckIndent E.DT_Space E.DT_IndentName
            |> P.bind
                (\postTypeComments ->
                    P.oneOf E.DT_Name
                        [ P.inContext E.DT_Alias (Keyword.alias_ E.DT_Name) <|
                            (Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                                |> P.bind
                                    (\preComments ->
                                        chompAliasNameToEquals
                                            |> P.bind
                                                (\( ( name, args, postComments ), preTypeComments ) ->
                                                    P.specialize E.AliasBody (Type.expression [])
                                                        |> P.fmap
                                                            (\( ( _, tipe ), end ) ->
                                                                let
                                                                    alias_ : A.Located Src.Alias
                                                                    alias_ =
                                                                        A.at start end (Src.Alias postTypeComments ( ( preComments, postComments ), name ) args ( preTypeComments, tipe ))
                                                                in
                                                                ( ( ( [], [] ), Alias maybeDocs alias_ ), end )
                                                            )
                                                )
                                    )
                            )
                        , P.specialize E.DT_Union <|
                            (chompCustomNameToEquals postTypeComments
                                |> P.bind
                                    (\( preVariantsComments, ( name, args ) ) ->
                                        Type.variant preVariantsComments
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
                                                                ( ( ( [], [] ), Union maybeDocs union ), end )
                                                            )
                                                )
                                    )
                            )
                        ]
                )
        )



-- TYPE ALIASES


chompAliasNameToEquals : P.Parser E.TypeAlias ( ( A.Located Name, List (Src.C1 (A.Located Name)), Src.FComments ), Src.FComments )
chompAliasNameToEquals =
    P.addLocation (Var.upper E.AliasName)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                    |> P.bind
                        (\comments ->
                            chompAliasNameToEqualsHelp name [] comments
                        )
            )


chompAliasNameToEqualsHelp : A.Located Name -> List (Src.C1 (A.Located Name)) -> Src.FComments -> P.Parser E.TypeAlias ( ( A.Located Name, List (Src.C1 (A.Located Name)), Src.FComments ), Src.FComments )
chompAliasNameToEqualsHelp name args comments =
    P.oneOf E.AliasEquals
        [ P.addLocation (Var.lower E.AliasEquals)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                        |> P.bind
                            (\postComments ->
                                chompAliasNameToEqualsHelp name (( comments, arg ) :: args) postComments
                            )
                )
        , P.word1 '=' E.AliasEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.AliasSpace E.AliasIndentBody)
            |> P.fmap (\preBodyComments -> ( ( name, List.reverse args, comments ), preBodyComments ))
        ]



-- CUSTOM TYPES


chompCustomNameToEquals : Src.FComments -> P.Parser E.CustomType (Src.C1 ( Src.C2 (A.Located Name), List (Src.C1 (A.Located Name)) ))
chompCustomNameToEquals preNameComments =
    P.addLocation (Var.upper E.CT_Name)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                    |> P.bind (\trailingComments -> chompCustomNameToEqualsHelp trailingComments ( preNameComments, name ) [])
            )


chompCustomNameToEqualsHelp : Src.FComments -> Src.C1 (A.Located Name) -> List (Src.C1 (A.Located Name)) -> P.Parser E.CustomType (Src.C1 ( Src.C2 (A.Located Name), List (Src.C1 (A.Located Name)) ))
chompCustomNameToEqualsHelp trailingComments (( preNameComments, name_ ) as name) args =
    P.oneOf E.CT_Equals
        [ P.addLocation (Var.lower E.CT_Equals)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                        |> P.bind (\postArgComments -> chompCustomNameToEqualsHelp postArgComments name (( trailingComments, arg ) :: args))
                )
        , P.word1 '=' E.CT_Equals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterEquals)
            |> P.fmap (\postEqualComments -> ( postEqualComments, ( ( ( preNameComments, trailingComments ), name_ ), List.reverse args ) ))
        ]


chompVariants : List (Src.C2Eol ( A.Located Name, List (Src.C1 Src.Type) )) -> A.Position -> Space.Parser E.CustomType (List (Src.C2Eol ( A.Located Name, List (Src.C1 Src.Type) )))
chompVariants variants end =
    P.oneOfWithFallback
        [ Space.checkIndent end E.CT_IndentBar
            |> P.bind (\_ -> P.word1 '|' E.CT_Bar)
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterBar)
            |> P.bind (\preTypeComments -> Type.variant preTypeComments)
            |> P.bind (\( variant, newEnd ) -> chompVariants (variant :: variants) newEnd)
        ]
        ( List.reverse variants, end )



-- PORT


portDecl : Maybe Src.Comment -> Space.Parser E.Decl (Src.C2 Decl)
portDecl maybeDocs =
    P.inContext E.Port (Keyword.port_ E.DeclStart) <|
        (Space.chompAndCheckIndent E.PortSpace E.PortIndentName
            |> P.bind
                (\preNameComments ->
                    P.addLocation (Var.lower E.PortName)
                        |> P.bind
                            (\name ->
                                Space.chompAndCheckIndent E.PortSpace E.PortIndentColon
                                    |> P.bind
                                        (\postNameComments ->
                                            P.word1 ':' E.PortColon
                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.PortSpace E.PortIndentType)
                                                |> P.bind
                                                    (\typeComments ->
                                                        P.specialize E.PortType (Type.expression [])
                                                            |> P.fmap
                                                                (\( ( ( preTipeComments, postTipeComments, _ ), tipe ), end ) ->
                                                                    ( ( ( preTipeComments, postTipeComments ), Port maybeDocs (Src.Port typeComments ( ( preNameComments, postNameComments ), name ) tipe) )
                                                                    , end
                                                                    )
                                                                )
                                                    )
                                        )
                            )
                )
        )



-- INFIX
-- INVARIANT: always chomps to a freshline
--


infix_ : P.Parser E.Module (Src.C1 (A.Located Src.Infix))
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
                        (\preBinopComments ->
                            P.oneOf err
                                [ Keyword.left_ err |> P.fmap (\_ -> Binop.Left)
                                , Keyword.right_ err |> P.fmap (\_ -> Binop.Right)
                                , Keyword.non_ err |> P.fmap (\_ -> Binop.Non)
                                ]
                                |> P.fmap (Tuple.pair preBinopComments)
                        )
                    |> P.bind
                        (\associativity ->
                            Space.chompAndCheckIndent err_ err
                                |> P.bind
                                    (\prePrecedenceComments ->
                                        Number.precedence err
                                            |> P.fmap (Tuple.pair prePrecedenceComments)
                                    )
                                |> P.bind
                                    (\precedence ->
                                        Space.chompAndCheckIndent err_ err
                                            |> P.bind
                                                (\preOpComments ->
                                                    P.word1 '(' err
                                                        |> P.bind (\_ -> Symbol.operator err err_)
                                                        |> P.bind
                                                            (\op ->
                                                                P.word1 ')' err
                                                                    |> P.bind (\_ -> Space.chompAndCheckIndent err_ err)
                                                                    |> P.bind
                                                                        (\postOpComments ->
                                                                            P.word1 '=' err
                                                                                |> P.bind (\_ -> Space.chompAndCheckIndent err_ err)
                                                                                |> P.bind
                                                                                    (\preNameComments ->
                                                                                        Var.lower err
                                                                                            |> P.bind
                                                                                                (\name ->
                                                                                                    P.getPosition
                                                                                                        |> P.bind
                                                                                                            (\end ->
                                                                                                                Space.chomp err_
                                                                                                                    |> P.bind
                                                                                                                        (\comments ->
                                                                                                                            Space.checkFreshLine err
                                                                                                                                |> P.fmap (\_ -> ( comments, A.at start end (Src.Infix ( ( preOpComments, postOpComments ), op ) associativity precedence ( preNameComments, name )) ))
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
