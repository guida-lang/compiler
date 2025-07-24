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
                                    |> P.fmap (\( typeDecl_, position ) -> ( ( [], [], typeDecl_ ), position ))
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
                                let
                                    _ =
                                        Debug.log "c108" comments
                                in
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
                                            let
                                                _ =
                                                    Debug.log "c1" postNameComments
                                            in
                                            P.oneOf E.DeclDefEquals
                                                [ P.word1 ':' E.DeclDefEquals
                                                    |> P.bind (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentType)
                                                    |> P.bind
                                                        (\preTypeComments ->
                                                            let
                                                                _ =
                                                                    Debug.log "c2" preTypeComments
                                                            in
                                                            P.specialize E.DeclDefType (Type.expression [])
                                                                |> P.bind
                                                                    (\( ( postTipeComments, tipe ), _ ) ->
                                                                        let
                                                                            _ =
                                                                                Debug.log "valueDecl1" ( postTipeComments, tipe )
                                                                        in
                                                                        Space.checkFreshLine E.DeclDefNameRepeat
                                                                            |> P.bind (\_ -> chompMatchingName name)
                                                                            |> P.bind
                                                                                (\defName ->
                                                                                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                                                                                        |> P.bind
                                                                                            (\preArgComments ->
                                                                                                let
                                                                                                    _ =
                                                                                                        Debug.log "c3" ( preArgComments, ( maybeDocs, docComments, ( postNameComments, defName ) ), ( preTypeComments, tipe ) )
                                                                                                in
                                                                                                chompDefArgsAndBody syntaxVersion maybeDocs docComments start ( postNameComments, defName ) (Just ( preTypeComments, tipe )) preArgComments []
                                                                                                    |> P.fmap (Debug.log "HERE!!! after c3")
                                                                                            )
                                                                                )
                                                                    )
                                                        )
                                                , chompDefArgsAndBody syntaxVersion maybeDocs docComments start ( [], A.at start end name ) Nothing [] []
                                                    |> P.fmap (Debug.log "HERE!!!2")
                                                ]
                                        )
                                )
                        )
            )


chompDefArgsAndBody : SyntaxVersion -> Maybe Src.Comment -> Src.FComments -> A.Position -> Src.C1 (A.Located Name) -> Maybe (Src.C1 Src.Type) -> Src.FComments -> List (Src.C1 Src.Pattern) -> Space.Parser E.DeclDef (Src.C2 Decl)
chompDefArgsAndBody syntaxVersion maybeDocs docComments start name tipe preArgComments revArgs =
    P.oneOf E.DeclDefEquals
        [ P.specialize E.DeclDefArg (Pattern.term syntaxVersion)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentEquals
                        |> P.bind
                            (\postArgComments ->
                                let
                                    _ =
                                        Debug.log "c4" postArgComments
                                in
                                chompDefArgsAndBody syntaxVersion maybeDocs docComments start name tipe postArgComments (( preArgComments, arg ) :: revArgs)
                            )
                )
        , P.word1 '=' E.DeclDefEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.DeclDefSpace E.DeclDefIndentBody)
            |> P.bind
                (\preBodyComments ->
                    let
                        _ =
                            Debug.log "c5" preBodyComments
                    in
                    P.specialize E.DeclDefBody (Expr.expression syntaxVersion)
                        |> P.fmap
                            (\( ( trailingComments, body ), end ) ->
                                let
                                    value : Src.Value
                                    value =
                                        Src.Value docComments name (List.reverse revArgs) ( preBodyComments, body ) tipe

                                    avalue : A.Located Src.Value
                                    avalue =
                                        A.at start end value
                                in
                                ( ( [], trailingComments, Value maybeDocs avalue ), end )
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


typeDecl : Maybe Src.Comment -> A.Position -> Space.Parser E.Decl Decl
typeDecl maybeDocs start =
    P.inContext E.DeclType (Keyword.type_ E.DeclStart) <|
        (Space.chompAndCheckIndent E.DT_Space E.DT_IndentName
            |> P.bind
                (\preAlias ->
                    let
                        _ =
                            Debug.log "c6" preAlias
                    in
                    P.oneOf E.DT_Name
                        [ P.inContext E.DT_Alias (Keyword.alias_ E.DT_Name) <|
                            (Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                                |> P.bind
                                    (\preComments ->
                                        let
                                            _ =
                                                Debug.log "c7" preComments
                                        in
                                        chompAliasNameToEquals
                                            |> P.bind
                                                (\( ( name, args, postComments ), preTypeComments ) ->
                                                    P.specialize E.AliasBody (Type.expression [])
                                                        |> P.fmap
                                                            (\( ( _, tipe ), end ) ->
                                                                let
                                                                    alias_ : A.Located Src.Alias
                                                                    alias_ =
                                                                        A.at start end (Src.Alias preAlias ( preComments, postComments, name ) args ( preTypeComments, tipe ))
                                                                in
                                                                ( Alias maybeDocs alias_, end )
                                                            )
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


chompAliasNameToEquals : P.Parser E.TypeAlias ( ( A.Located Name, List (Src.C1 (A.Located Name)), Src.FComments ), Src.FComments )
chompAliasNameToEquals =
    P.addLocation (Var.upper E.AliasName)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.AliasSpace E.AliasIndentEquals
                    |> P.bind
                        (\comments ->
                            let
                                _ =
                                    Debug.log "c8" comments
                            in
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
                                let
                                    _ =
                                        Debug.log "c9" postComments
                                in
                                chompAliasNameToEqualsHelp name (( comments, arg ) :: args) postComments
                            )
                )
        , P.word1 '=' E.AliasEquals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.AliasSpace E.AliasIndentBody)
            |> P.fmap
                (\preTypeComments ->
                    let
                        _ =
                            Debug.log "c10" preTypeComments
                    in
                    ( ( name, List.reverse args, comments ), preTypeComments )
                )
        ]



-- CUSTOM TYPES


chompCustomNameToEquals : P.Parser E.CustomType ( A.Located Name, List (A.Located Name) )
chompCustomNameToEquals =
    P.addLocation (Var.upper E.CT_Name)
        |> P.bind
            (\name ->
                Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                    |> P.bind
                        (\c11 ->
                            let
                                _ =
                                    Debug.log "c11" c11
                            in
                            chompCustomNameToEqualsHelp name []
                        )
            )


chompCustomNameToEqualsHelp : A.Located Name -> List (A.Located Name) -> P.Parser E.CustomType ( A.Located Name, List (A.Located Name) )
chompCustomNameToEqualsHelp name args =
    P.oneOf E.CT_Equals
        [ P.addLocation (Var.lower E.CT_Equals)
            |> P.bind
                (\arg ->
                    Space.chompAndCheckIndent E.CT_Space E.CT_IndentEquals
                        |> P.bind
                            (\c12 ->
                                let
                                    _ =
                                        Debug.log "c12" c12
                                in
                                chompCustomNameToEqualsHelp name (arg :: args)
                            )
                )
        , P.word1 '=' E.CT_Equals
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterEquals)
            |> P.fmap
                (\c13 ->
                    let
                        _ =
                            Debug.log "c13" c13
                    in
                    ( name, List.reverse args )
                )
        ]


chompVariants : List ( A.Located Name, List Src.Type ) -> A.Position -> Space.Parser E.CustomType (List ( A.Located Name, List Src.Type ))
chompVariants variants end =
    P.oneOfWithFallback
        [ Space.checkIndent end E.CT_IndentBar
            |> P.bind (\_ -> P.word1 '|' E.CT_Bar)
            |> P.bind (\_ -> Space.chompAndCheckIndent E.CT_Space E.CT_IndentAfterBar)
            |> P.bind
                (\c14 ->
                    let
                        _ =
                            Debug.log "c14" c14
                    in
                    Type.variant
                )
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
                    let
                        _ =
                            Debug.log "c15" preNameComments
                    in
                    P.addLocation (Var.lower E.PortName)
                        |> P.bind
                            (\name ->
                                Space.chompAndCheckIndent E.PortSpace E.PortIndentColon
                                    |> P.bind
                                        (\postNameComments ->
                                            let
                                                _ =
                                                    Debug.log "c16" postNameComments
                                            in
                                            P.word1 ':' E.PortColon
                                                |> P.bind (\_ -> Space.chompAndCheckIndent E.PortSpace E.PortIndentType)
                                                |> P.bind
                                                    (\typeComments ->
                                                        let
                                                            _ =
                                                                Debug.log "c17" typeComments
                                                        in
                                                        P.specialize E.PortType (Type.expression [])
                                                            |> P.fmap
                                                                (\( ( ( preTipeComments, postTipeComments, _ ), tipe ), end ) ->
                                                                    ( ( preTipeComments, postTipeComments, Port maybeDocs (Src.Port typeComments ( preNameComments, postNameComments, name ) tipe) )
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
                                                                                                                            let
                                                                                                                                _ =
                                                                                                                                    Debug.log "c109" comments
                                                                                                                            in
                                                                                                                            Space.checkFreshLine err
                                                                                                                                |> P.fmap (\_ -> ( comments, A.at start end (Src.Infix ( preOpComments, postOpComments, op ) associativity precedence ( preNameComments, name )) ))
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
