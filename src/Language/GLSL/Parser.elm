module Language.GLSL.Parser exposing (parse)

{-| Reference: <https://hackage.haskell.org/package/language-glsl-0.3.0/docs/src/Language.GLSL.Parser.html>
-}

import Combine
import Combine.Char
import Hex
import Language.GLSL.Syntax
    exposing
        ( CaseLabel(..)
        , Compound(..)
        , Condition(..)
        , Declaration(..)
        , Expr(..)
        , ExternalDeclaration(..)
        , Field(..)
        , FullType(..)
        , FunctionIdentifier(..)
        , FunctionPrototype(..)
        , InitDeclarator(..)
        , IntConstantKind(..)
        , InterpolationQualifier(..)
        , InvariantOrType(..)
        , InvariantQualifier(..)
        , LayoutQualifier(..)
        , LayoutQualifierId(..)
        , ParameterDeclaration(..)
        , ParameterQualifier(..)
        , ParameterTypeQualifier(..)
        , Parameters(..)
        , PrecisionQualifier(..)
        , Statement(..)
        , StorageQualifier(..)
        , StructDeclarator(..)
        , TranslationUnit(..)
        , TypeQualifier(..)
        , TypeSpecifier(..)
        , TypeSpecifierNoPrecision(..)
        , TypeSpecifierNonArray(..)
        )
import Utils.Main as Utils


try : P a -> P a
try =
    identity


letter : P Char
letter =
    Combine.lazy (\() -> Combine.Char.satisfy Char.isAlpha)


alphaNum : P Char
alphaNum =
    Combine.lazy (\() -> Combine.Char.satisfy Char.isAlphaNum)


notFollowedBy : P a -> P ()
notFollowedBy p =
    Combine.maybe p
        |> Combine.andThen
            (\result ->
                case result of
                    Just _ ->
                        Combine.fail "Unexpected match"

                    Nothing ->
                        Combine.succeed ()
            )


type Assoc
    = AssocNone
    | AssocLeft
    | AssocRight


type Operator tok st a
    = Infix (P (a -> a -> a)) Assoc
    | Prefix (P (a -> a))
    | Postfix (P (a -> a))


buildExpressionParser : List (List (Operator Char S Expr)) -> P Expr -> P Expr
buildExpressionParser operators simpleExpr =
    let
        makeParser : List (Operator Char S Expr) -> P Expr -> P Expr
        makeParser ops term =
            let
                -- { rassoc : List (P (Expr -> Expr -> Expr)), lassoc : List (P (Expr -> Expr -> Expr)), nassoc : List (P (Expr -> Expr -> Expr)), prefix : List (P (Expr -> Expr)), postfix : List (P (Expr -> Expr)) }
                { rassoc, lassoc, nassoc, prefix, postfix } =
                    List.foldr splitOp { rassoc = [], lassoc = [], nassoc = [], prefix = [], postfix = [] } ops

                rassocOp : P (Expr -> Expr -> Expr)
                rassocOp =
                    Combine.choice rassoc

                lassocOp : P (Expr -> Expr -> Expr)
                lassocOp =
                    Combine.choice lassoc

                nassocOp : P (Expr -> Expr -> Expr)
                nassocOp =
                    Combine.choice nassoc

                prefixOp : P (Expr -> Expr)
                prefixOp =
                    Combine.choice prefix |> Combine.mapError (\_ -> [ "" ])

                postfixOp : P (Expr -> Expr)
                postfixOp =
                    Combine.choice postfix |> Combine.mapError (\_ -> [ "" ])

                ambiguous : String -> P a -> P b
                ambiguous assoc op =
                    try
                        (op
                            |> Combine.andThen
                                (\_ ->
                                    Combine.fail
                                        ("ambiguous use of a "
                                            ++ assoc
                                            ++ " associative operator"
                                        )
                                )
                        )

                ambiguousRight : P Expr
                ambiguousRight =
                    ambiguous "right" rassocOp

                ambiguousLeft : P Expr
                ambiguousLeft =
                    ambiguous "left" lassocOp

                ambiguousNon : P Expr
                ambiguousNon =
                    ambiguous "non" nassocOp

                termP : P Expr
                termP =
                    prefixP
                        |> Combine.andThen
                            (\pre ->
                                term
                                    |> Combine.andThen
                                        (\x ->
                                            postfixP
                                                |> Combine.map (\post -> post (pre x))
                                        )
                            )

                postfixP : P (Expr -> Expr)
                postfixP =
                    Combine.or postfixOp (Combine.succeed identity)

                prefixP : P (Expr -> Expr)
                prefixP =
                    Combine.or prefixOp (Combine.succeed identity)

                rassocP : Expr -> P Expr
                rassocP x =
                    Combine.choice
                        [ rassocOp
                            |> Combine.andThen
                                (\f ->
                                    termP
                                        |> Combine.andThen rassocP1
                                        |> Combine.map (f x)
                                )
                        , ambiguousLeft
                        , ambiguousNon
                        ]

                rassocP1 : Expr -> P Expr
                rassocP1 x =
                    Combine.or (rassocP x) (Combine.succeed x)

                lassocP : Expr -> P Expr
                lassocP x =
                    Combine.choice
                        [ lassocOp
                            |> Combine.andThen
                                (\f ->
                                    termP
                                        |> Combine.andThen lassocP1
                                        |> Combine.map (f x)
                                )
                        , ambiguousRight
                        , ambiguousNon
                        ]

                lassocP1 : Expr -> P Expr
                lassocP1 x =
                    Combine.or (lassocP x) (Combine.succeed x)

                nassocP : Expr -> P Expr
                nassocP x =
                    nassocOp
                        |> Combine.andThen
                            (\f ->
                                termP
                                    |> Combine.andThen
                                        (\y ->
                                            Combine.choice
                                                [ ambiguousRight
                                                , ambiguousLeft
                                                , ambiguousNon
                                                , Combine.succeed (f x y)
                                                ]
                                        )
                            )
            in
            termP
                |> Combine.andThen
                    (\x ->
                        Combine.choice
                            [ rassocP x
                            , lassocP x
                            , nassocP x
                            , Combine.succeed x
                            ]
                            |> Combine.mapError (\_ -> [ "operator" ])
                    )

        splitOp :
            Operator Char S Expr
            -> { rassoc : List (P (Expr -> Expr -> Expr)), lassoc : List (P (Expr -> Expr -> Expr)), nassoc : List (P (Expr -> Expr -> Expr)), prefix : List (P (Expr -> Expr)), postfix : List (P (Expr -> Expr)) }
            -> { rassoc : List (P (Expr -> Expr -> Expr)), lassoc : List (P (Expr -> Expr -> Expr)), nassoc : List (P (Expr -> Expr -> Expr)), prefix : List (P (Expr -> Expr)), postfix : List (P (Expr -> Expr)) }
        splitOp singleOperator acc =
            case singleOperator of
                Infix op assoc ->
                    case assoc of
                        AssocNone ->
                            { acc | nassoc = op :: acc.nassoc }

                        AssocLeft ->
                            { acc | lassoc = op :: acc.lassoc }

                        AssocRight ->
                            { acc | rassoc = op :: acc.rassoc }

                Prefix op ->
                    { acc | prefix = op :: acc.prefix }

                Postfix op ->
                    { acc | postfix = op :: acc.postfix }
    in
    List.foldl makeParser simpleExpr operators



----------------------------------------------------------------------
-- Parser state, hold a symbol table.
----------------------------------------------------------------------


type alias S =
    ()


type alias P a =
    Combine.Parser S a



----------------------------------------------------------------------
-- Reserved words
----------------------------------------------------------------------
-- List of keywords.


keywords : List String
keywords =
    List.concatMap String.words
        [ "attribute const uniform varying"
        , "layout"
        , "centroid flat smooth noperspective"
        , "break continue do for while switch case default"
        , "if else"
        , "in out inout"
        , "float int void bool true false"
        , "invariant"
        , "discard return"
        , "mat2 mat3 mat4"
        , "mat2x2 mat2x3 mat2x4"
        , "mat3x2 mat3x3 mat3x4"
        , "mat4x2 mat4x3 mat4x4"
        , "vec2 vec3 vec4 ivec2 ivec3 ivec4 bvec2 bvec3 bvec4"
        , "uint uvec2 uvec3 uvec4"
        , "lowp mediump highp precision"
        , "sampler1D sampler2D sampler3D samplerCube"
        , "sampler1DShadow sampler2DShadow samplerCubeShadow"
        , "sampler1DArray sampler2DArray"
        , "sampler1DArrayShadow sampler2DArrayShadow"
        , "isampler1D isampler2D isampler3D isamplerCube"
        , "isampler1DArray isampler2DArray"
        , "usampler1D usampler2D usampler3D usamplerCube"
        , "usampler1DArray usampler2DArray"
        , "sampler2DRect sampler2DRectShadow isampler2DRect usampler2DRect"
        , "samplerBuffer isamplerBuffer usamplerBuffer"
        , "sampler2DMS isampler2DMS usampler2DMS"
        , "sampler2DMSArray isampler2DMSArray usampler2DMSArray"
        , "struct"
        ]



-- List of keywords reserved for future use.


reservedWords : List String
reservedWords =
    List.concatMap String.words
        [ "common partition active"
        , "asm"
        , "class union enum typedef template this packed"
        , "goto"
        , "inline noinline volatile public static extern external interface"
        , "long short double half fixed unsigned superp"
        , "input output"
        , "hvec2 hvec3 hvec4 dvec2 dvec3 dvec4 fvec2 fvec3 fvec4"
        , "sampler3DRect"
        , "filter"
        , "image1D image2D image3D imageCube"
        , "iimage1D iimage2D iimage3D iimageCube"
        , "uimage1D uimage2D uimage3D uimageCube"
        , "image1DArray image2DArray"
        , "iimage1DArray iimage2DArray uimage1DArray uimage2DArray"
        , "image1DShadow image2DShadow"
        , "image1DArrayShadow image2DArrayShadow"
        , "imageBuffer iimageBuffer uimageBuffer"
        , "sizeof cast"
        , "namespace using"
        , "row_major"
        ]



----------------------------------------------------------------------
-- Convenience parsers
----------------------------------------------------------------------


comment : P ()
comment =
    Combine.lazy
        (\() ->
            Combine.Char.char '/'
                |> Combine.keep
                    (Combine.choice
                        [ Combine.Char.char '*'
                            |> Combine.ignore (Combine.manyTill Combine.Char.anyChar (try <| Combine.string "*/"))
                            |> Combine.onsuccess ()
                        , Combine.Char.char '/'
                            |> Combine.ignore
                                (Combine.manyTill Combine.Char.anyChar
                                    (Combine.choice
                                        [ Combine.Char.newline |> Combine.onsuccess ()
                                        , Combine.end
                                        ]
                                    )
                                )
                            |> Combine.onsuccess ()
                        ]
                    )
        )


blank : P ()
blank =
    Combine.lazy
        (\() ->
            Combine.or
                comment
                (Combine.whitespace |> Combine.onsuccess ())
        )


lexeme : P a -> P a
lexeme p =
    p
        |> Combine.ignore (Combine.skipMany blank)


parse : String -> Result (Combine.ParseError ()) (Combine.ParseOk () TranslationUnit)
parse =
    Combine.parse
        (Combine.skipMany blank
            |> Combine.keep translationUnit
            |> Combine.ignore Combine.end
        )



----------------------------------------------------------------------
-- Lexical elements (tokens)
----------------------------------------------------------------------


semicolon : P ()
semicolon =
    Combine.lazy (\() -> lexeme (Combine.Char.char ';' |> Combine.onsuccess ()))


comma : P ()
comma =
    Combine.lazy (\() -> lexeme (Combine.Char.char ',' |> Combine.onsuccess ()))


colon : P ()
colon =
    Combine.lazy (\() -> lexeme (Combine.Char.char ':' |> Combine.onsuccess ()))


lbrace : P ()
lbrace =
    Combine.lazy (\() -> lexeme (Combine.Char.char '{' |> Combine.onsuccess ()))


rbrace : P ()
rbrace =
    Combine.lazy (\() -> lexeme (Combine.Char.char '}' |> Combine.onsuccess ()))


lbracket : P ()
lbracket =
    Combine.lazy (\() -> lexeme (Combine.Char.char '[' |> Combine.onsuccess ()))


rbracket : P ()
rbracket =
    Combine.lazy (\() -> lexeme (Combine.Char.char ']' |> Combine.onsuccess ()))


lparen : P ()
lparen =
    Combine.lazy (\() -> lexeme (Combine.Char.char '(' |> Combine.onsuccess ()))


rparen : P ()
rparen =
    Combine.lazy (\() -> lexeme (Combine.Char.char ')' |> Combine.onsuccess ()))



-- Try to parse a given string, making sure it is not a
-- prefix of an identifier.


keyword : String -> P ()
keyword w =
    Combine.lazy
        (\() ->
            lexeme <|
                try
                    (Combine.string w
                        |> Combine.keep (notFollowedBy identifierTail)
                    )
        )



-- Parses and returns an identifier.
-- TODO an identifier can't start with "gl_" unless
-- it is to redeclare a predeclared "gl_" identifier.


identifier : P String
identifier =
    Combine.lazy
        (\() ->
            let
                check : String -> P String
                check i =
                    if List.member i reservedWords then
                        Combine.fail (i ++ " is reserved")

                    else if List.member i keywords then
                        Combine.fail (i ++ " is a keyword")

                    else
                        checkUnderscore i (String.toList i)

                checkUnderscore : String -> List Char -> P String
                checkUnderscore i i2 =
                    case i2 of
                        '_' :: '_' :: _ ->
                            Combine.fail (i ++ " is reserved (two consecutive underscores)")

                        _ :: cs ->
                            checkUnderscore i cs

                        [] ->
                            Combine.succeed i
            in
            lexeme
                (identifierHead
                    |> Combine.andThen
                        (\h ->
                            Combine.many identifierTail
                                |> Combine.andThen
                                    (\t ->
                                        check (String.fromList (h :: t))
                                    )
                        )
                )
        )



-- TODO the size of the int should fit its type.


intConstant : P Expr
intConstant =
    Combine.lazy
        (\() ->
            Combine.choice
                [ hexadecimal
                , octal
                , badOctal
                    |> Combine.andThen (\_ -> Combine.fail "Invalid octal number")
                , decimal
                ]
        )


floatingConstant : P Expr
floatingConstant =
    Combine.lazy
        (\() ->
            Combine.choice
                [ floatExponent
                , floatPoint
                , pointFloat
                ]
        )



-- Try to parse a given string, and allow identifier characters
-- (or anything else) to directly follow.


operator : String -> P String
operator =
    lexeme << try << Combine.string



----------------------------------------------------------------------
-- Lexical elements helpers
----------------------------------------------------------------------


identifierHead : P Char
identifierHead =
    Combine.lazy (\() -> Combine.or letter (Combine.Char.char '_'))


identifierTail : P Char
identifierTail =
    Combine.lazy (\() -> Combine.or alphaNum (Combine.Char.char '_'))


hexadecimal : P Expr
hexadecimal =
    Combine.lazy
        (\() ->
            lexeme
                (try
                    (Combine.Char.char '0'
                        |> Combine.ignore (Combine.Char.oneOf [ 'X', 'x' ])
                        |> Combine.keep (Combine.many1 Combine.Char.hexDigit)
                        |> Combine.andThen
                            (\d ->
                                -- TODO
                                Combine.maybe (Combine.Char.oneOf [ 'U', 'u' ])
                                    |> Combine.andThen
                                        (\_ ->
                                            case Hex.fromString (String.fromList d) of
                                                Ok val ->
                                                    Combine.succeed (IntConstant Hexadecimal val)

                                                Err err ->
                                                    Combine.fail err
                                        )
                            )
                    )
                )
        )


octal : P Expr
octal =
    Combine.lazy
        (\() ->
            lexeme
                (try
                    (Combine.Char.char '0'
                        |> Combine.keep (Combine.many1 Combine.Char.octDigit)
                        |> Combine.andThen
                            (\d ->
                                -- TODO
                                Combine.maybe (Combine.Char.oneOf [ 'U', 'u' ])
                                    |> Combine.andThen
                                        (\_ ->
                                            case octFromString (String.fromList d) of
                                                Ok val ->
                                                    Combine.succeed (IntConstant Octal val)

                                                Err err ->
                                                    Combine.fail err
                                        )
                            )
                    )
                )
        )


octFromString : String -> Result String Int
octFromString _ =
    Debug.todo "octFromString"


badOctal : P ()
badOctal =
    Combine.lazy
        (\() ->
            lexeme
                (try
                    (Combine.Char.char '0'
                        |> Combine.keep (Combine.many1 Combine.Char.hexDigit)
                        |> Combine.onsuccess ()
                    )
                )
        )


decimal : P Expr
decimal =
    Combine.lazy
        (\() ->
            lexeme
                (try
                    (Combine.many1 Combine.Char.digit
                        |> Combine.ignore (notFollowedBy (Combine.or (Combine.Char.char '.') (exponent |> Combine.onsuccess ' ')))
                        |> Combine.ignore
                            -- TODO
                            (Combine.maybe (Combine.Char.oneOf [ 'U', 'u' ]))
                        |> Combine.andThen
                            (\d ->
                                case String.toInt (String.fromList d) of
                                    Just val ->
                                        Combine.succeed (IntConstant Decimal val)

                                    Nothing ->
                                        Combine.fail "Invalid decimal number"
                            )
                    )
                )
        )


floatExponent : P Expr
floatExponent =
    Combine.lazy
        (\() ->
            lexeme
                (try
                    (Combine.many1 Combine.Char.digit
                        |> Combine.andThen
                            (\d ->
                                exponent
                                    |> Combine.andThen
                                        (\e ->
                                            Combine.maybe (Combine.Char.oneOf [ 'F', 'f' ])
                                                -- TODO
                                                |> Combine.andThen
                                                    (\_ ->
                                                        case String.toFloat (String.fromList d ++ e) of
                                                            Just val ->
                                                                Combine.succeed (FloatConstant val)

                                                            Nothing ->
                                                                Combine.fail "Invalid float exponent number"
                                                    )
                                        )
                            )
                    )
                )
        )


floatPoint : P Expr
floatPoint =
    Combine.lazy
        (\() ->
            lexeme
                (try
                    (Combine.many1 Combine.Char.digit
                        |> Combine.andThen
                            (\d ->
                                Combine.Char.char '.'
                                    |> Combine.andThen
                                        (\_ ->
                                            Combine.many Combine.Char.digit
                                                |> Combine.andThen
                                                    (\d_ ->
                                                        let
                                                            d__ =
                                                                if List.isEmpty d_ then
                                                                    "0"

                                                                else
                                                                    String.fromList d_
                                                        in
                                                        Combine.maybe exponent
                                                            |> Combine.andThen
                                                                (\e ->
                                                                    Combine.maybe (Combine.Char.oneOf [ 'F', 'f' ])
                                                                        -- TODO
                                                                        |> Combine.andThen
                                                                            (\_ ->
                                                                                case String.toFloat (String.fromList d ++ "." ++ d__ ++ Maybe.withDefault "" e) of
                                                                                    Just val ->
                                                                                        Combine.succeed (FloatConstant val)

                                                                                    Nothing ->
                                                                                        Combine.fail "Invalid float point number"
                                                                            )
                                                                )
                                                    )
                                        )
                            )
                    )
                )
        )


pointFloat : P Expr
pointFloat =
    Combine.lazy
        (\() ->
            lexeme
                (try
                    (Combine.Char.char '.'
                        |> Combine.andThen
                            (\_ ->
                                Combine.many1 Combine.Char.digit
                                    |> Combine.andThen
                                        (\d ->
                                            Combine.maybe exponent
                                                |> Combine.andThen
                                                    (\e ->
                                                        Combine.maybe (Combine.Char.oneOf [ 'F', 'f' ])
                                                            |> Combine.andThen
                                                                (\_ ->
                                                                    case String.toFloat ("0." ++ String.fromList d ++ Maybe.withDefault "" e) of
                                                                        Just val ->
                                                                            Combine.succeed (FloatConstant val)

                                                                        Nothing ->
                                                                            Combine.fail "Invalid point float number"
                                                                )
                                                    )
                                        )
                            )
                    )
                )
        )


exponent : P String
exponent =
    Combine.lazy
        (\() ->
            lexeme
                (try
                    (Combine.Char.oneOf [ 'E', 'e' ]
                        |> Combine.andThen
                            (\_ ->
                                Combine.maybe (Combine.Char.oneOf [ '+', '-' ])
                                    |> Combine.andThen
                                        (\s ->
                                            Combine.many1 Combine.Char.digit
                                                |> Combine.map
                                                    (\d ->
                                                        "e"
                                                            ++ Maybe.withDefault "" (Maybe.map String.fromChar s)
                                                            ++ String.fromList d
                                                    )
                                        )
                            )
                    )
                )
        )



----------------------------------------------------------------------
-- Tables for buildExpressionParser
----------------------------------------------------------------------


infixLeft : String -> (a -> a -> a) -> Operator Char S a
infixLeft s r =
    Infix (lexeme (try <| Combine.string s) |> Combine.onsuccess r) AssocLeft


infixLeft_ : String -> (a -> a -> a) -> Operator Char S a
infixLeft_ s r =
    Infix
        (lexeme
            (try (Combine.string s)
                |> Combine.ignore (notFollowedBy (Combine.Char.char '='))
            )
            |> Combine.onsuccess r
        )
        AssocLeft


infixLeft__ : Char -> (a -> a -> a) -> Operator Char S a
infixLeft__ c r =
    Infix
        (lexeme
            (try (Combine.Char.char c)
                |> Combine.ignore (notFollowedBy (Combine.Char.oneOf [ c, '=' ]))
            )
            |> Combine.onsuccess r
        )
        AssocLeft


infixRight : String -> (a -> a -> a) -> Operator Char S a
infixRight s r =
    Infix (lexeme (try <| Combine.string s) |> Combine.onsuccess r) AssocRight


conditionalTable : List (List (Operator Char S Expr))
conditionalTable =
    [ [ infixLeft_ "*" Mul, infixLeft_ "/" Div, infixLeft_ "%" Mod ]
    , [ infixLeft_ "+" Add, infixLeft_ "-" Sub ]
    , [ infixLeft_ "<<" LeftShift, infixLeft_ ">>" RightShift ]
    , [ infixLeft_ "<" Lt
      , infixLeft_ ">" Gt
      , infixLeft "<=" Lte
      , infixLeft ">=" Gte
      ]
    , [ infixLeft "==" Equ, infixLeft "!=" Neq ]
    , [ infixLeft__ '&' BitAnd ]
    , [ infixLeft_ "^" BitXor ]
    , [ infixLeft__ '|' BitOr ]
    , [ infixLeft "&&" And ]
    , [ infixLeft "||" Or ]
    ]


assignmentTable : List (List (Operator Char S Expr))
assignmentTable =
    [ [ infixRight "=" Equal ]
    , [ infixRight "+=" AddAssign ]
    , [ infixRight "-=" SubAssign ]
    , [ infixRight "*=" MulAssign ]
    , [ infixRight "/=" DivAssign ]
    , [ infixRight "%=" ModAssign ]
    , [ infixRight "<<=" LeftAssign ]
    , [ infixRight ">>=" RightAssign ]
    , [ infixRight "&=" AndAssign ]
    , [ infixRight "^=" XorAssign ]
    , [ infixRight "|=" OrAssign ]
    ]


expressionTable : List (List (Operator Char S Expr))
expressionTable =
    [ [ infixLeft "," Sequence ]
    ]



----------------------------------------------------------------------
-- Grammar
----------------------------------------------------------------------


primaryExpression : P Expr
primaryExpression =
    Combine.lazy
        (\() ->
            Combine.choice
                [ Combine.map Variable (try identifier)

                -- int constant
                , intConstant

                -- uint constant
                -- float constant
                , floatingConstant

                -- bool constant
                , keyword "true" |> Combine.onsuccess (BoolConstant True)
                , keyword "false" |> Combine.onsuccess (BoolConstant False)

                -- expression within parentheses
                , Combine.between lparen rparen expression
                ]
        )


postfixExpression : P Expr
postfixExpression =
    Combine.lazy
        (\() ->
            Combine.or
                (try
                    (functionCallGeneric
                        |> Combine.map (\( i, p ) -> FunctionCall i p)
                    )
                )
                primaryExpression
                |> Combine.andThen
                    (\e ->
                        Combine.many
                            (Combine.choice
                                [ Combine.between lbracket rbracket integerExpression
                                    |> Combine.map (Utils.flip Bracket)
                                , dotFunctionCallGeneric
                                , dotFieldSelection
                                , operator "++" |> Combine.onsuccess PostInc
                                , operator "--" |> Combine.onsuccess PostDec
                                ]
                            )
                            |> Combine.map (\p -> List.foldl (<|) e p)
                    )
        )


dotFunctionCallGeneric : P (Expr -> Expr)
dotFunctionCallGeneric =
    Combine.lazy
        (\() ->
            lexeme (try (Combine.string ".") |> Combine.keep functionCallGeneric)
                |> Combine.map (\( i, p ) e -> MethodCall e i p)
        )


dotFieldSelection : P (Expr -> Expr)
dotFieldSelection =
    Combine.lazy
        (\() ->
            lexeme (try (Combine.string ".") |> Combine.keep identifier)
                |> Combine.map (Utils.flip FieldSelection)
        )


integerExpression : P Expr
integerExpression =
    Combine.lazy (\() -> expression)



-- Those productions are pushed inside postfixExpression.
-- functionCall = functionCallOrMethod
-- functionCallOrMethod = functionCallGeneric <|> postfixExpression DOT functionCallGeneric


functionCallGeneric : P ( FunctionIdentifier, Parameters )
functionCallGeneric =
    Combine.lazy
        (\() ->
            functionCallHeader
                |> Combine.andThen
                    (\i ->
                        Combine.choice
                            [ keyword "void" |> Combine.onsuccess ParamVoid
                            , Combine.sepBy comma assignmentExpression
                                |> Combine.map Params
                            ]
                            |> Combine.andThen
                                (\p ->
                                    rparen
                                        |> Combine.onsuccess ( i, p )
                                )
                    )
        )



-- Those productions are pushed inside functionCallGeneric.
-- functionCallHeaderNoParameters = undefined
-- functionCallHeaderWithParameters = undefined


functionCallHeader : P FunctionIdentifier
functionCallHeader =
    Combine.lazy
        (\() ->
            functionIdentifier
                |> Combine.andThen
                    (\i ->
                        lparen
                            |> Combine.onsuccess i
                    )
        )


functionIdentifier : P FunctionIdentifier
functionIdentifier =
    Combine.lazy
        (\() ->
            Combine.choice
                [ try identifier |> Combine.map FuncId

                -- TODO if the 'identifier' is declared as a type, should be this case
                , typeSpecifier |> Combine.map FuncIdTypeSpec

                -- no need for fieldSelection
                ]
        )


unaryExpression : P Expr
unaryExpression =
    Combine.lazy
        (\() ->
            Combine.many
                (Combine.choice
                    [ operator "++" |> Combine.onsuccess PreInc
                    , operator "--" |> Combine.onsuccess PreDec
                    , operator "+" |> Combine.onsuccess UnaryPlus
                    , operator "-" |> Combine.onsuccess UnaryNegate
                    , operator "!" |> Combine.onsuccess UnaryNot
                    , operator "~" |> Combine.onsuccess UnaryOneComplement
                    ]
                )
                |> Combine.andThen
                    (\p ->
                        postfixExpression
                            |> Combine.map
                                (\e ->
                                    List.foldr (<|) e p
                                )
                    )
        )



-- inside unaryExpression
-- unaryOperator = choice
-- implemented throught buildExpressionParser
-- multiplicativeExpression = undefined
-- additiveExpression = undefined
-- shiftExpression = undefined
-- relationalExpression = undefined
-- equalityExpression = undefined
-- andExpression = undefined
-- exclusiveOrExpression = undefined
-- inclusiveOrExpression = undefined
-- logicalAndExpression = undefined
-- logicalXorExpression = undefined
-- logicalOrExpression = undefined


conditionalExpression : P Expr
conditionalExpression =
    Combine.lazy
        (\() ->
            buildExpressionParser conditionalTable unaryExpression
                |> Combine.andThen
                    (\loe ->
                        Combine.maybe
                            (lexeme (Combine.string "?")
                                |> Combine.keep expression
                                |> Combine.ignore (lexeme (Combine.string ":"))
                                |> Combine.andThen
                                    (\e ->
                                        assignmentExpression
                                            |> Combine.map (\a -> ( e, a ))
                                    )
                            )
                            |> Combine.map
                                (\ter ->
                                    case ter of
                                        Nothing ->
                                            loe

                                        Just ( e, a ) ->
                                            Selection loe e a
                                )
                    )
        )


assignmentExpression : P Expr
assignmentExpression =
    Combine.lazy (\() -> buildExpressionParser assignmentTable conditionalExpression)


expression : P Expr
expression =
    Combine.lazy (\() -> buildExpressionParser expressionTable assignmentExpression)


constantExpression : P Expr
constantExpression =
    Combine.lazy (\() -> conditionalExpression)



-- The GLSL grammar include here function definition but we don't
-- do this here because they should occur only at top level (page 28).
-- Function definitions are handled in externalDefinition instead.


declaration : P Declaration
declaration =
    Combine.lazy
        (\() ->
            let
                idecl =
                    identifier
                        |> Combine.andThen
                            (\i ->
                                Combine.maybe (Combine.between lbracket rbracket (Combine.maybe constantExpression))
                                    |> Combine.andThen
                                        (\m ->
                                            Combine.maybe (lexeme (Combine.string "=") |> Combine.keep initializer)
                                                |> Combine.map (InitDecl i m)
                                        )
                            )
            in
            Combine.choice
                [ try
                    (fullySpecifiedType
                        |> Combine.andThen
                            (\t ->
                                Combine.sepBy comma idecl
                                    |> Combine.ignore semicolon
                                    |> Combine.map (InitDeclaration (TypeDeclarator t))
                            )
                    )
                , keyword "invariant"
                    |> Combine.keep (Combine.sepBy comma idecl)
                    |> Combine.ignore semicolon
                    |> Combine.map (InitDeclaration InvariantDeclarator)
                , keyword "precision"
                    |> Combine.keep precisionQualifier
                    |> Combine.andThen
                        (\q ->
                            typeSpecifierNoPrecision
                                |> Combine.ignore semicolon
                                |> Combine.map (Precision q)
                        )
                , typeQualifier
                    |> Combine.andThen
                        (\q ->
                            Combine.choice
                                [ semicolon |> Combine.onsuccess (TQ q)
                                , identifier
                                    |> Combine.ignore lbrace
                                    |> Combine.andThen
                                        (\i ->
                                            structDeclarationList
                                                |> Combine.ignore rbrace
                                                |> Combine.andThen
                                                    (\s ->
                                                        Combine.maybe
                                                            (identifier
                                                                |> Combine.andThen
                                                                    (\j ->
                                                                        Combine.maybe (Combine.between lbracket rbracket (Combine.maybe constantExpression))
                                                                            |> Combine.map (\n -> ( j, n ))
                                                                    )
                                                            )
                                                            |> Combine.ignore semicolon
                                                            |> Combine.map (\m -> Block q i s m)
                                                    )
                                        )
                                ]
                        )
                ]
        )


functionPrototype : P FunctionPrototype
functionPrototype =
    Combine.lazy
        (\() ->
            functionDeclarator
                |> Combine.andThen
                    (\( t, i, p ) ->
                        rparen
                            |> Combine.onsuccess (FuncProt t i p)
                    )
        )


functionDeclarator : P ( FullType, String, List ParameterDeclaration )
functionDeclarator =
    Combine.lazy
        (\() ->
            functionHeader
                |> Combine.andThen
                    (\( t, i ) ->
                        Combine.sepBy comma parameterDeclaration
                            |> Combine.map (\p -> ( t, i, p ))
                    )
        )



-- inside functionDeclarator
-- functionHeaderWithParameters = undefined


functionHeader : P ( FullType, String )
functionHeader =
    Combine.lazy
        (\() ->
            fullySpecifiedType
                |> Combine.andThen
                    (\t ->
                        identifier
                            |> Combine.andThen
                                (\i ->
                                    lparen
                                        |> Combine.onsuccess ( t, i )
                                )
                    )
        )



-- inside parameterDeclaration
-- parameterDeclarator = undefined
-- expanding parameterDeclarator and parameterTypeSpecifier, the rule is:
-- parameterDeclaration:
--   parameterTypeQualifier [parameterQualifier] typeSpecifier identifier[[e]]
--                          [parameterQualifier] typeSpecifier identifier[[e]]
--   parameterTypeQualifier [parameterQualifier] typeSpecifier
--                          [parameterQualifier] typeSpecifier
-- which is simply
--   [parameterTypeQualifier] [parameterQualifier] typeSpecifier [identifier[[e]]]


parameterDeclaration : P ParameterDeclaration
parameterDeclaration =
    Combine.lazy
        (\() ->
            Combine.maybe parameterTypeQualifier
                |> Combine.andThen
                    (\tq ->
                        Combine.maybe parameterQualifier
                            |> Combine.andThen
                                (\q ->
                                    typeSpecifier
                                        |> Combine.andThen
                                            (\s ->
                                                Combine.maybe
                                                    (identifier
                                                        |> Combine.andThen
                                                            (\i ->
                                                                -- FIXME can't the bracket be empty, i.e. a[] ?
                                                                Combine.maybe (Combine.between lbracket rbracket constantExpression)
                                                                    |> Combine.map (\b -> ( i, b ))
                                                            )
                                                    )
                                                    |> Combine.map (\m -> ParameterDeclaration tq q s m)
                                            )
                                )
                    )
        )


parameterQualifier : P ParameterQualifier
parameterQualifier =
    Combine.lazy
        (\() ->
            Combine.choice
                -- "empty" case handled in the caller
                [ (try << lexeme << Combine.string) "inout" |> Combine.onsuccess InOutParameter
                , (try << lexeme << Combine.string) "in" |> Combine.onsuccess InParameter
                , (try << lexeme << Combine.string) "out" |> Combine.onsuccess OutParameter
                ]
        )



-- inside parameterDeclaration
-- parameterTypeSpecifier = typeSpecifier
-- FIXME not correct w.r.t. the specs.
-- The specs allow
--   int
--   int, foo
--   invariant foo, bar[]
-- and disallow
--   invariant bar[]
-- It is not used, it is inside declaration.
-- initDeclaratorList = undefined
-- inside initDeclaratorList
-- singleDeclaration = undefined


fullySpecifiedType : P FullType
fullySpecifiedType =
    Combine.lazy
        (\() ->
            Combine.choice
                [ try typeSpecifier |> Combine.map (FullType Nothing)
                , typeQualifier
                    |> Combine.andThen
                        (\q ->
                            typeSpecifier
                                |> Combine.map (FullType (Just q))
                        )
                ]
        )


invariantQualifier : P InvariantQualifier
invariantQualifier =
    Combine.lazy (\() -> keyword "invariant" |> Combine.onsuccess Invariant)


interpolationQualifier : P InterpolationQualifier
interpolationQualifier =
    Combine.lazy
        (\() ->
            Combine.choice
                [ keyword "smooth" |> Combine.onsuccess Smooth
                , keyword "flat" |> Combine.onsuccess Flat
                , keyword "noperspective" |> Combine.onsuccess NoPerspective
                ]
        )


layoutQualifier : P LayoutQualifier
layoutQualifier =
    Combine.lazy
        (\() ->
            Combine.sequence [ keyword "layout", lparen ]
                |> Combine.keep (Combine.sepBy comma layoutQualifierId)
                |> Combine.ignore rparen
                |> Combine.map Layout
        )



-- implemented directly in layoutQualifier
-- layoutQualifierIdList = undefined


layoutQualifierId : P LayoutQualifierId
layoutQualifierId =
    Combine.lazy
        (\() ->
            identifier
                |> Combine.andThen
                    (\i ->
                        Combine.maybe (lexeme (Combine.string "=") |> Combine.keep intConstant)
                            |> Combine.map (LayoutQualId i)
                    )
        )


parameterTypeQualifier : P ParameterTypeQualifier
parameterTypeQualifier =
    Combine.lazy (\() -> keyword "const" |> Combine.onsuccess ConstParameter)



-- sto
-- lay [sto]
-- int [sto]
-- inv [sto]
-- inv int sto


typeQualifier : P TypeQualifier
typeQualifier =
    Combine.lazy
        (\() ->
            Combine.choice
                [ storageQualifier
                    |> Combine.map TypeQualSto
                , layoutQualifier
                    |> Combine.andThen
                        (\l ->
                            Combine.maybe storageQualifier
                                |> Combine.map (TypeQualLay l)
                        )
                , interpolationQualifier
                    |> Combine.andThen
                        (\i ->
                            Combine.maybe storageQualifier
                                |> Combine.map (TypeQualInt i)
                        )
                , invariantQualifier
                    |> Combine.andThen
                        (\i ->
                            Combine.choice
                                [ interpolationQualifier
                                    |> Combine.andThen
                                        (\j ->
                                            storageQualifier
                                                |> Combine.map (TypeQualInv3 i j)
                                        )
                                , Combine.maybe storageQualifier
                                    |> Combine.map (TypeQualInv i)
                                ]
                        )
                ]
        )



-- TODO see 4.3 for restrictions


storageQualifier : P StorageQualifier
storageQualifier =
    Combine.lazy
        (\() ->
            Combine.choice
                [ keyword "const" |> Combine.onsuccess Const
                , keyword "attribute" |> Combine.onsuccess Attribute -- TODO vertex only, is deprecated
                , keyword "varying" |> Combine.onsuccess Varying -- deprecated
                , keyword "in" |> Combine.onsuccess In
                , keyword "out" |> Combine.onsuccess Out
                , keyword "centroid"
                    |> Combine.keep
                        (Combine.choice
                            [ keyword "varying" |> Combine.onsuccess CentroidVarying -- deprecated
                            , keyword "in" |> Combine.onsuccess CentroidIn
                            , keyword "out" |> Combine.onsuccess CentroidOut
                            ]
                        )
                , keyword "uniform" |> Combine.onsuccess Uniform
                ]
        )


typeSpecifier : P TypeSpecifier
typeSpecifier =
    Combine.lazy
        (\() ->
            Combine.choice
                [ try precisionQualifier
                    |> Combine.andThen
                        (\q ->
                            typeSpecifierNoPrecision
                                |> Combine.map (\s -> TypeSpec (Just q) s)
                        )
                , typeSpecifierNoPrecision |> Combine.map (TypeSpec Nothing)
                ]
        )


typeSpecifierNoPrecision : P TypeSpecifierNoPrecision
typeSpecifierNoPrecision =
    Combine.lazy
        (\() ->
            typeSpecifierNonArray
                |> Combine.andThen
                    (\s ->
                        Combine.choice
                            [ try (Combine.sequence [ lbracket, rbracket ])
                                |> Combine.onsuccess (TypeSpecNoPrecision s (Just Nothing))
                            , lbracket
                                |> Combine.keep constantExpression
                                |> Combine.andThen
                                    (\c ->
                                        rbracket
                                            |> Combine.onsuccess (TypeSpecNoPrecision s (Just (Just c)))
                                    )
                            , Combine.succeed (TypeSpecNoPrecision s Nothing)
                            ]
                    )
        )



-- Basic types, structs, and user-defined types.


typeSpecifierNonArray : P TypeSpecifierNonArray
typeSpecifierNonArray =
    Combine.lazy
        (\() ->
            Combine.choice
                [ keyword "void" |> Combine.onsuccess Void
                , keyword "float" |> Combine.onsuccess Float
                , keyword "int" |> Combine.onsuccess Int
                , keyword "uint" |> Combine.onsuccess UInt
                , keyword "bool" |> Combine.onsuccess Bool
                , keyword "vec2" |> Combine.onsuccess Vec2
                , keyword "vec3" |> Combine.onsuccess Vec3
                , keyword "vec4" |> Combine.onsuccess Vec4
                , keyword "bvec2" |> Combine.onsuccess BVec2
                , keyword "bvec3" |> Combine.onsuccess BVec3
                , keyword "bvec4" |> Combine.onsuccess BVec4
                , keyword "ivec2" |> Combine.onsuccess IVec2
                , keyword "ivec3" |> Combine.onsuccess IVec3
                , keyword "ivec4" |> Combine.onsuccess IVec4
                , keyword "uvec2" |> Combine.onsuccess UVec2
                , keyword "uvec3" |> Combine.onsuccess UVec3
                , keyword "uvec4" |> Combine.onsuccess UVec4
                , keyword "mat2" |> Combine.onsuccess Mat2
                , keyword "mat3" |> Combine.onsuccess Mat3
                , keyword "mat4" |> Combine.onsuccess Mat4
                , keyword "mat2x2" |> Combine.onsuccess Mat2x2
                , keyword "mat2x3" |> Combine.onsuccess Mat2x3
                , keyword "mat2x4" |> Combine.onsuccess Mat2x4
                , keyword "mat3x2" |> Combine.onsuccess Mat3x2
                , keyword "mat3x3" |> Combine.onsuccess Mat3x3
                , keyword "mat3x4" |> Combine.onsuccess Mat3x4
                , keyword "mat4x2" |> Combine.onsuccess Mat4x2
                , keyword "mat4x3" |> Combine.onsuccess Mat4x3
                , keyword "mat4x4" |> Combine.onsuccess Mat4x4
                , keyword "sampler1D" |> Combine.onsuccess Sampler1D
                , keyword "sampler2D" |> Combine.onsuccess Sampler2D
                , keyword "sampler3D" |> Combine.onsuccess Sampler3D
                , keyword "samplerCube" |> Combine.onsuccess SamplerCube
                , keyword "sampler1DShadow" |> Combine.onsuccess Sampler1DShadow
                , keyword "sampler2DShadow" |> Combine.onsuccess Sampler2DShadow
                , keyword "samplerCubeShadow" |> Combine.onsuccess SamplerCubeShadow
                , keyword "sampler1DArray" |> Combine.onsuccess Sampler1DArray
                , keyword "sampler2DArray" |> Combine.onsuccess Sampler2DArray
                , keyword "sampler1DArrayShadow" |> Combine.onsuccess Sampler1DArrayShadow
                , keyword "sampler2DArrayShadow" |> Combine.onsuccess Sampler2DArrayShadow
                , keyword "isampler1D" |> Combine.onsuccess ISampler1D
                , keyword "isampler2D" |> Combine.onsuccess ISampler2D
                , keyword "isampler3D" |> Combine.onsuccess ISampler3D
                , keyword "isamplerCube" |> Combine.onsuccess ISamplerCube
                , keyword "isampler1DArray" |> Combine.onsuccess ISampler1DArray
                , keyword "isampler2DArray" |> Combine.onsuccess ISampler2DArray
                , keyword "usampler1D" |> Combine.onsuccess USampler1D
                , keyword "usampler2D" |> Combine.onsuccess USampler2D
                , keyword "usampler3D" |> Combine.onsuccess USampler3D
                , keyword "usamplerCube" |> Combine.onsuccess USamplerCube
                , keyword "usampler1DArray" |> Combine.onsuccess USampler1DArray
                , keyword "usampler2DArray" |> Combine.onsuccess USampler2DArray
                , keyword "sampler2DRect" |> Combine.onsuccess Sampler2DRect
                , keyword "sampler2DRectShadow" |> Combine.onsuccess Sampler2DRectShadow
                , keyword "isampler2DRect" |> Combine.onsuccess ISampler2DRect
                , keyword "usampler2DRect" |> Combine.onsuccess USampler2DRect
                , keyword "samplerBuffer" |> Combine.onsuccess SamplerBuffer
                , keyword "isamplerBuffer" |> Combine.onsuccess ISamplerBuffer
                , keyword "usamplerBuffer" |> Combine.onsuccess USamplerBuffer
                , keyword "sampler2DMS" |> Combine.onsuccess Sampler2DMS
                , keyword "isampler2DMS" |> Combine.onsuccess ISampler2DMS
                , keyword "usampler2DMS" |> Combine.onsuccess USampler2DMS
                , keyword "sampler2DMSArray" |> Combine.onsuccess Sampler2DMSArray
                , keyword "isampler2DMSArray" |> Combine.onsuccess ISampler2DMSArray
                , keyword "usampler2DMSArray" |> Combine.onsuccess USampler2DMSArray
                , structSpecifier
                , identifier |> Combine.map TypeName -- verify if it is declared
                ]
        )


precisionQualifier : P PrecisionQualifier
precisionQualifier =
    Combine.lazy
        (\() ->
            Combine.choice
                [ keyword "highp" |> Combine.onsuccess HighP
                , keyword "mediump" |> Combine.onsuccess MediumP
                , keyword "lowp" |> Combine.onsuccess LowP
                ]
        )


structSpecifier : P TypeSpecifierNonArray
structSpecifier =
    Combine.lazy
        (\() ->
            keyword "struct"
                |> Combine.keep (Combine.maybe identifier)
                |> Combine.andThen
                    (\i ->
                        lbrace
                            |> Combine.keep structDeclarationList
                            |> Combine.andThen
                                (\d ->
                                    rbrace
                                        |> Combine.map (\_ -> StructSpecifier i d)
                                )
                    )
        )


structDeclarationList : P (List Field)
structDeclarationList =
    Combine.lazy (\() -> Combine.many1 structDeclaration)


structDeclaration : P Field
structDeclaration =
    Combine.lazy
        (\() ->
            Combine.maybe typeQualifier
                |> Combine.andThen
                    (\q ->
                        typeSpecifier
                            |> Combine.andThen
                                (\s ->
                                    structDeclaratorList
                                        |> Combine.andThen
                                            (\l ->
                                                semicolon
                                                    |> Combine.map (\_ -> Field q s l)
                                            )
                                )
                    )
        )


structDeclaratorList : P (List StructDeclarator)
structDeclaratorList =
    Combine.lazy (\() -> Combine.sepBy comma structDeclarator)


structDeclarator : P StructDeclarator
structDeclarator =
    Combine.lazy
        (\() ->
            identifier
                |> Combine.andThen
                    (\i ->
                        Combine.choice
                            [ lbracket
                                |> Combine.keep (Combine.maybe constantExpression)
                                |> Combine.ignore rbracket
                                |> Combine.map (\e -> StructDeclarator i (Just e))
                            , Combine.succeed (StructDeclarator i Nothing)
                            ]
                    )
        )


initializer : P Expr
initializer =
    Combine.lazy (\() -> assignmentExpression)


declarationStatement : P Declaration
declarationStatement =
    Combine.lazy (\() -> declaration)


statement : P Statement
statement =
    Combine.lazy
        (\() ->
            Combine.or (Combine.map CompoundStatement (Combine.lazy (\() -> compoundStatement)))
                (Combine.lazy (\() -> simpleStatement))
        )


simpleStatement : P Statement
simpleStatement =
    Combine.lazy
        (\() ->
            Combine.choice
                [ declarationStatement |> Combine.map DeclarationStatement
                , expressionStatement |> Combine.map ExpressionStatement
                , selectionStatement
                , switchStatement
                , caseLabel |> Combine.map CaseLabel
                , iterationStatement
                , jumpStatement
                ]
        )


compoundStatement : P Compound
compoundStatement =
    Combine.lazy
        (\() ->
            Combine.choice
                [ try (Combine.sequence [ lbrace, rbrace ])
                    |> Combine.onsuccess (Compound [])
                , Combine.between lbrace rbrace (Combine.lazy (\() -> statementList))
                    |> Combine.map Compound
                ]
        )


statementNoNewScope : P Statement
statementNoNewScope =
    Combine.lazy
        (\() ->
            Combine.or
                (Combine.map CompoundStatement compoundStatementNoNewScope)
                simpleStatement
        )


compoundStatementNoNewScope : P Compound
compoundStatementNoNewScope =
    Combine.lazy (\() -> compoundStatement)


statementList : P (List Statement)
statementList =
    Combine.lazy (\() -> Combine.many1 (Combine.lazy (\() -> statement)))


expressionStatement : P (Maybe Expr)
expressionStatement =
    Combine.lazy
        (\() ->
            Combine.choice
                [ semicolon |> Combine.onsuccess Nothing
                , expression |> Combine.andThen (\e -> semicolon |> Combine.onsuccess (Just e))
                ]
        )


selectionStatement : P Statement
selectionStatement =
    Combine.lazy
        (\() ->
            keyword "if"
                |> Combine.ignore lparen
                |> Combine.keep expression
                |> Combine.ignore rparen
                |> Combine.andThen
                    (\c ->
                        statement
                            |> Combine.andThen
                                (\t ->
                                    Combine.maybe (keyword "else" |> Combine.keep statement)
                                        |> Combine.map (SelectionStatement c t)
                                )
                    )
        )



-- inside selectionStatement
-- selectionRestStatement = undefined


condition : P Condition
condition =
    Combine.lazy
        (\() ->
            Combine.choice
                [ expression
                    |> Combine.map Condition
                , fullySpecifiedType
                    |> Combine.andThen
                        (\t ->
                            identifier
                                |> Combine.ignore (lexeme (Combine.string "="))
                                |> Combine.andThen
                                    (\i ->
                                        initializer
                                            |> Combine.map (InitializedCondition t i)
                                    )
                        )
                ]
        )


switchStatement : P Statement
switchStatement =
    Combine.lazy
        (\() ->
            keyword "switch"
                |> Combine.ignore lparen
                |> Combine.keep expression
                |> Combine.ignore rparen
                |> Combine.ignore lbrace
                |> Combine.andThen
                    (\e ->
                        switchStatementList
                            |> Combine.ignore rbrace
                            |> Combine.map (SwitchStatement e)
                    )
        )


switchStatementList : P (List Statement)
switchStatementList =
    Combine.lazy (\() -> Combine.many statement)


caseLabel : P CaseLabel
caseLabel =
    Combine.lazy
        (\() ->
            Combine.choice
                [ keyword "case"
                    |> Combine.keep expression
                    |> Combine.andThen
                        (\e ->
                            colon
                                |> Combine.onsuccess (Case e)
                        )
                , keyword "default"
                    |> Combine.keep (Combine.succeed Default)
                ]
        )


iterationStatement : P Statement
iterationStatement =
    Combine.lazy
        (\() ->
            Combine.choice
                [ keyword "while"
                    |> Combine.ignore lparen
                    |> Combine.keep condition
                    |> Combine.ignore rparen
                    |> Combine.andThen
                        (\c ->
                            statementNoNewScope
                                |> Combine.map (While c)
                        )
                , keyword "do"
                    |> Combine.keep statement
                    |> Combine.ignore (keyword "while")
                    |> Combine.ignore lparen
                    |> Combine.andThen
                        (\s ->
                            expression
                                |> Combine.ignore rparen
                                |> Combine.ignore semicolon
                                |> Combine.map (DoWhile s)
                        )
                , keyword "for"
                    |> Combine.ignore lparen
                    |> Combine.keep forInitStatement
                    |> Combine.andThen
                        (\i ->
                            Combine.maybe condition
                                |> Combine.ignore semicolon
                                |> Combine.andThen
                                    (\c ->
                                        Combine.maybe expression
                                            |> Combine.ignore rparen
                                            |> Combine.andThen
                                                (\e ->
                                                    statementNoNewScope
                                                        |> Combine.map (For i c e)
                                                )
                                    )
                        )
                ]
        )


forInitStatement : P (Result (Maybe Expr) Declaration)
forInitStatement =
    Combine.or (expressionStatement |> Combine.map Err)
        (declarationStatement |> Combine.map Ok)



-- inside iterationStatement
-- conditionOp = undefined
-- inside iterationStatement
-- forRestStatement = undefined


jumpStatement : P Statement
jumpStatement =
    Combine.choice
        [ Combine.sequence [ keyword "continue", semicolon ] |> Combine.onsuccess Continue
        , Combine.sequence [ keyword "break", semicolon ] |> Combine.onsuccess Break
        , try (Combine.sequence [ keyword "return", semicolon ]) |> Combine.onsuccess (Return Nothing)
        , keyword "return"
            |> Combine.keep expression
            |> Combine.andThen
                (\e ->
                    semicolon
                        |> Combine.onsuccess (Return (Just e))
                )
        , Combine.sequence [ keyword "discard", semicolon ] |> Combine.onsuccess Discard
        ]


translationUnit : P TranslationUnit
translationUnit =
    Combine.lazy (\() -> Combine.map TranslationUnit (Combine.many1 externalDeclaration))


externalDeclaration : P ExternalDeclaration
externalDeclaration =
    Combine.lazy
        (\() ->
            Combine.choice
                [ functionPrototype
                    |> Combine.andThen
                        (\p ->
                            Combine.choice
                                [ semicolon |> Combine.onsuccess (FunctionDeclaration p)
                                , compoundStatementNoNewScope |> Combine.map (FunctionDefinition p)
                                ]
                        )
                , Combine.map Declaration declaration
                ]
        )



-- inside externalDeclaration, used only in tests


functionDefinition : P ExternalDeclaration
functionDefinition =
    Combine.lazy
        (\() ->
            functionPrototype
                |> Combine.andThen
                    (\fp ->
                        compoundStatementNoNewScope
                            |> Combine.map (FunctionDefinition fp)
                    )
        )
