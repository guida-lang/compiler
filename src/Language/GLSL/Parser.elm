module Language.GLSL.Parser exposing (parse)

import Hex
import Language.GLSL.Syntax exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Utils.Main as Utils


char : Char -> Parser Char
char c =
    Parser.getChompedString (Parser.symbol (String.fromChar c))
        |> extractCharHelper


try : Parser a -> Parser a
try =
    Parser.backtrackable


choice : List (Parser a) -> Parser a
choice =
    Parser.oneOf


optionMaybe : Parser a -> Parser (Maybe a)
optionMaybe p =
    Parser.oneOf
        [ Parser.map Just p
        , Parser.succeed Nothing
        ]


between : Parser () -> Parser () -> Parser a -> Parser a
between open close p =
    Parser.succeed identity
        |. open
        |= p
        |. close


many : Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p revStmts =
    Parser.oneOf
        [ Parser.succeed (\stmt -> Parser.Loop (stmt :: revStmts))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revStmts))
        ]


many1 : Parser a -> Parser (List a)
many1 p =
    Parser.succeed (::)
        |= p
        |= many p


string : String -> P String
string =
    Parser.getChompedString << Parser.keyword


notFollowedBy : P a -> P ()
notFollowedBy p =
    try
        (Parser.oneOf
            [ try p
                |> Parser.andThen (\c -> Parser.problem (Debug.toString c))
            , Parser.succeed ()
            ]
        )


hexDigit : P Char
hexDigit =
    Parser.getChompedString (Parser.chompIf Char.isHexDigit)
        |> extractCharHelper


oneOf : String -> P Char
oneOf cs =
    Parser.getChompedString (Parser.chompIf (\c -> String.contains (String.fromChar c) cs))
        |> extractCharHelper


letter : P Char
letter =
    Parser.getChompedString (Parser.chompIf Char.isAlpha)
        |> extractCharHelper


extractCharHelper : P String -> P Char
extractCharHelper =
    Parser.andThen
        (\cs ->
            case String.toList cs of
                c :: [] ->
                    Parser.succeed c

                _ ->
                    Parser.problem "Failed to extract single char..."
        )


octDigit : P Char
octDigit =
    Parser.getChompedString (Parser.chompIf Char.isOctDigit)
        |> extractCharHelper


alphaNum : P Char
alphaNum =
    Parser.getChompedString (Parser.chompIf Char.isAlphaNum)
        |> extractCharHelper


digit : P Char
digit =
    Parser.getChompedString (Parser.chompIf Char.isDigit)
        |> extractCharHelper


sepBy : P a -> P sep -> P (List a)
sepBy p sep =
    Parser.oneOf
        [ sepBy1 p sep
        , Parser.succeed []
        ]


sepBy1 : P a -> P sep -> P (List a)
sepBy1 p sep =
    Parser.succeed (::)
        |= p
        |= many
            (Parser.succeed identity
                |. sep
                |= p
            )


octFromString : String -> Result String Int
octFromString _ =
    Debug.todo "octFromString"


type Assoc
    = AssocNone
    | AssocLeft
    | AssocRight


type Operator tok st a
    = Infix (P (a -> a -> a)) Assoc
    | Prefix (P (a -> a))
    | Postfix (P (a -> a))


buildExpressionParser : List (List (Operator Char S a)) -> P a -> P a
buildExpressionParser operators simpleExpr =
    -- let
    --     makeParser ops term =
    --         let
    --             { rassoc, lassoc, nassoc, prefix, postfix } =
    --                 List.foldr splitOp { rassoc = [], lassoc = [], nassoc = [], prefix = [], postfix = [] } ops
    --             rassocOp =
    --                 choice rassoc
    --             lassocOp =
    --                 choice lassoc
    --             nassocOp =
    --                 choice nassoc
    --             prefixOp =
    --                 -- Parser.succeed identity
    --                 --     |= choice prefix
    --                 --     |. Parser.problem ""
    --                 Debug.todo "prefixOp"
    --             postfixOp =
    --                 -- Parser.succeed identity
    --                 --     |= choice postfix
    --                 --     |. Parser.problem ""
    --                 Debug.todo "postfixOp"
    --             ambiguous assoc op =
    --                 try
    --                     (Parser.succeed identity
    --                         |= op
    --                         |. Parser.problem ("ambiguous use of a " ++ assoc ++ " associative operator")
    --                     )
    --             ambiguousRight =
    --                 ambiguous "right" rassocOp
    --             ambiguousLeft =
    --                 ambiguous "left" lassocOp
    --             ambiguousNon =
    --                 ambiguous "non" nassocOp
    --             termP =
    --                 Parser.succeed (\pre x post -> post (pre x))
    --                     |= prefixP
    --                     |= term
    --                     |= postfixP
    --             postfixP =
    --                 Parser.oneOf
    --                     [ postfixOp
    --                     , Parser.succeed identity
    --                     ]
    --             prefixP =
    --                 Parser.oneOf
    --                     [ prefixOp
    --                     , Parser.succeed identity
    --                     ]
    --             rassocP x =
    --                 -- Parser.oneOf
    --                 --     [ Parser.succeed (\f y -> f x y)
    --                 --         |= rassocOp
    --                 --         |= Parser.andThen rassocP1 termP
    --                 --     , ambiguousLeft
    --                 --     , ambiguousNon
    --                 --     , Parser.succeed x
    --                 --     ]
    --                 Debug.todo "rassocP"
    --             rassocP1 x =
    --                 Parser.oneOf
    --                     [ rassocP x
    --                     , Parser.succeed x
    --                     ]
    --             lassocP x =
    --                 -- Parser.oneOf
    --                 --     [ Parser.succeed (\f y -> f x y)
    --                 --         |= lassocOp
    --                 --         |= Parser.andThen lassocP1 termP
    --                 --     , ambiguousRight
    --                 --     , ambiguousNon
    --                 --     , Parser.succeed x
    --                 --     ]
    --                 Debug.todo "lassocP"
    --             lassocP1 x =
    --                 Parser.oneOf
    --                     [ lassocP x
    --                     , Parser.succeed x
    --                     ]
    --             nassocP x =
    --                 -- Parser.succeed Tuple.pair
    --                 --     |= nassocOp
    --                 --     |= termP
    --                 --     |> Parser.andThen
    --                 --         (\( f, y ) ->
    --                 --             Parser.oneOf
    --                 --                 [ ambiguousRight
    --                 --                 , ambiguousLeft
    --                 --                 , ambiguousNon
    --                 --                 , Parser.succeed (f x y)
    --                 --                 ]
    --                 --         )
    --                 Debug.todo "nassocP"
    --         in
    --         Parser.succeed identity
    --             |= termP
    --             |> Parser.andThen
    --                 (\x ->
    --                     Parser.succeed identity
    --                         |= Parser.oneOf
    --                             [ rassocP x
    --                             , lassocP x
    --                             , nassocP x
    --                             , Parser.succeed x
    --                             ]
    --                         |. Parser.problem "operator"
    --                 )
    --     splitOp singleOperator acc =
    --         case singleOperator of
    --             Infix op assoc ->
    --                 case assoc of
    --                     AssocNone ->
    --                         { acc | nassoc = op :: acc.nassoc }
    --                     AssocLeft ->
    --                         { acc | lassoc = op :: acc.lassoc }
    --                     AssocRight ->
    --                         { acc | rassoc = op :: acc.rassoc }
    --             Prefix op ->
    --                 { acc | prefix = op :: acc.prefix }
    --             Postfix op ->
    --                 { acc | postfix = op :: acc.postfix }
    -- in
    -- List.foldl makeParser simpleExpr operators
    Debug.todo "buildExpressionParser"



----------------------------------------------------------------------
-- Parser state, hold a symbol table.
----------------------------------------------------------------------


type S
    = S


type alias P a =
    Parser a



----------------------------------------------------------------------
-- Reserved words
----------------------------------------------------------------------
-- List of keywords.


keywords : List String
keywords =
    List.concat <|
        List.map String.words <|
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
    List.concat <|
        List.map String.words <|
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
    Parser.oneOf
        [ Parser.lineComment "//"
        , Parser.multiComment "/*" "*/" Parser.NotNestable
        ]


blank : P ()
blank =
    Parser.oneOf
        [ try comment
        , Parser.spaces
        ]



-- Acts like p and discards any following space character.


lexeme : P a -> P a
lexeme p =
    Parser.succeed identity
        |= p
        |. Parser.spaces


parse : String -> Result (List Parser.DeadEnd) TranslationUnit
parse =
    Parser.run
        (Parser.succeed identity
            |. Parser.spaces
            |= translationUnit
            |. Parser.end
        )



----------------------------------------------------------------------
-- Lexical elements (tokens)
----------------------------------------------------------------------


semicolon : P ()
semicolon =
    Parser.succeed ()
        |. lexeme (char ';')


comma : P ()
comma =
    Parser.succeed ()
        |. lexeme (char ',')


colon : P ()
colon =
    Parser.succeed ()
        |. lexeme (char ':')


lbrace : P ()
lbrace =
    Parser.succeed ()
        |. lexeme (char '{')


rbrace : P ()
rbrace =
    Parser.succeed ()
        |. lexeme (char '}')


lbracket : P ()
lbracket =
    Parser.succeed ()
        |. lexeme (char '[')


rbracket : P ()
rbracket =
    Parser.succeed ()
        |. lexeme (char ']')


lparen : P ()
lparen =
    Parser.succeed ()
        |. lexeme (char '(')


rparen : P ()
rparen =
    Parser.succeed ()
        |. lexeme (char ')')



-- Try to parse a given string, making sure it is not a
-- prefix of an identifier.


keyword : String -> P ()
keyword w =
    lexeme <|
        try
            (Parser.succeed ()
                |. string w
                |. notFollowedBy identifierTail
            )



-- Parses and returns an identifier.
-- TODO an identifier can't start with "gl_" unless
-- it is to redeclare a predeclared "gl_" identifier.


identifier : P String
identifier =
    let
        check : String -> P String
        check i =
            if List.member i reservedWords then
                Parser.problem (i ++ " is reserved")

            else if List.member i keywords then
                Parser.problem (i ++ " is a keyword")

            else
                checkUnderscore i (String.toList i)

        checkUnderscore : String -> List Char -> P String
        checkUnderscore i i2 =
            case i2 of
                '_' :: '_' :: _ ->
                    Parser.problem (i ++ " is reserved (two consecutive underscores)")

                _ :: cs ->
                    checkUnderscore i cs

                [] ->
                    Parser.succeed i
    in
    lexeme
        (Parser.succeed (\h t -> String.fromList (h :: t))
            |= identifierHead
            |= many identifierTail
            |> Parser.andThen check
        )



-- TODO the size of the int should fit its type.


intConstant : P Expr
intConstant =
    choice
        [ hexadecimal
        , octal
        , badOctal |> Parser.andThen (\_ -> Parser.problem "Invalid octal number")
        , decimal
        ]


floatingConstant : P Expr
floatingConstant =
    choice
        [ floatExponent
        , floatPoint
        , pointFloat
        ]



-- Try to parse a given string, and allow identifier characters
-- (or anything else) to directly follow.


operator : String -> P String
operator =
    lexeme << try << string



----------------------------------------------------------------------
-- Lexical elements helpers
----------------------------------------------------------------------


identifierHead : P Char
identifierHead =
    Parser.oneOf
        [ letter
        , char '_'
        ]


identifierTail : P Char
identifierTail =
    Parser.oneOf
        [ alphaNum
        , char '_'
        ]


hexadecimal : P Expr
hexadecimal =
    lexeme <|
        try <|
            Parser.andThen
                (\d ->
                    case Hex.fromString d of
                        Ok val ->
                            Parser.succeed (IntConstant Hexadecimal val)

                        Err err ->
                            Parser.problem err
                )
                (Parser.succeed identity
                    |. Parser.keyword "0"
                    |. oneOf "Xx"
                    |= Parser.getChompedString (Parser.chompWhile Char.isHexDigit)
                    -- TODO
                    |. optionMaybe (oneOf "Uu")
                )


octal : P Expr
octal =
    lexeme <|
        try <|
            Parser.andThen
                (\d ->
                    case octFromString d of
                        Ok val ->
                            Parser.succeed (IntConstant Octal val)

                        Err err ->
                            Parser.problem err
                )
                (Parser.succeed String.fromList
                    |. char '0'
                    |= many1 octDigit
                    -- TODO
                    |. optionMaybe (oneOf "Uu")
                )


badOctal : P ()
badOctal =
    lexeme <|
        try <|
            Parser.succeed ()
                |. char '0'
                |. many1 hexDigit


decimal : P Expr
decimal =
    lexeme <|
        try <|
            Parser.andThen
                (\d ->
                    case String.toInt d of
                        Just val ->
                            Parser.succeed (IntConstant Decimal val)

                        Nothing ->
                            Parser.problem "Invalid decimal number"
                )
                (Parser.succeed String.fromList
                    |= many1 digit
                    |. notFollowedBy
                        (Parser.oneOf
                            [ char '.'
                            , Parser.succeed ' '
                                |. exponent
                            ]
                        )
                    -- TODO
                    |. optionMaybe (oneOf "Uu")
                )


floatExponent : P Expr
floatExponent =
    lexeme <|
        try <|
            Parser.andThen
                (\( d, e ) ->
                    case String.toFloat (String.fromList d ++ e) of
                        Just val ->
                            Parser.succeed (FloatConstant val)

                        Nothing ->
                            Parser.problem "Invalid float exponent number"
                )
                (Parser.succeed Tuple.pair
                    |= many1 digit
                    |= exponent
                    -- TODO
                    |. optionMaybe (oneOf "Ff")
                )


floatPoint : P Expr
floatPoint =
    lexeme <|
        try <|
            Parser.andThen
                (\( d, d_, e ) ->
                    let
                        d__ =
                            if String.isEmpty d_ then
                                "0"

                            else
                                d_
                    in
                    case String.toFloat (d ++ "." ++ d__ ++ Maybe.withDefault "" e) of
                        Just val ->
                            Parser.succeed (FloatConstant val)

                        Nothing ->
                            Parser.problem "Invalid float point number"
                )
                (Parser.succeed (\d d_ e -> ( String.fromList d, String.fromList d_, e ))
                    |= many1 digit
                    |. char '.'
                    |= many digit
                    |= optionMaybe exponent
                    -- TODO
                    |. optionMaybe (oneOf "Ff")
                )


pointFloat : P Expr
pointFloat =
    lexeme <|
        try <|
            Parser.andThen
                (\( d, e ) ->
                    case String.toFloat ("0." ++ d ++ Maybe.withDefault "" e) of
                        Just val ->
                            Parser.succeed (FloatConstant val)

                        Nothing ->
                            Parser.problem "Invalid point float number"
                )
                (Parser.succeed (\d e -> ( String.fromList d, e ))
                    |. char '.'
                    |= many1 digit
                    |= optionMaybe exponent
                    -- TODO
                    |. optionMaybe (oneOf "Ff")
                )


exponent : P String
exponent =
    lexeme <|
        try <|
            Parser.succeed (\s d -> "e" ++ Maybe.withDefault "" s ++ d)
                |. Parser.oneOf [ Parser.keyword "U", Parser.keyword "u" ]
                |= optionMaybe (Parser.getChompedString (Parser.oneOf [ Parser.keyword "+", Parser.keyword "-" ]))
                |= Parser.getChompedString (Parser.chompWhile Char.isDigit)



----------------------------------------------------------------------
-- Tables for buildExpressionParser
----------------------------------------------------------------------


infixLeft : String -> (a -> a -> a) -> Operator Char S a
infixLeft s r =
    Infix
        (Parser.succeed r
            |. lexeme (try (string s))
        )
        AssocLeft


infixLeft_ : String -> (a -> a -> a) -> Operator Char S a
infixLeft_ s r =
    Infix
        (Parser.succeed r
            |. lexeme
                (Parser.succeed ()
                    |. try (string s)
                    |. notFollowedBy (char '=')
                )
        )
        AssocLeft


infixLeft__ : Char -> (a -> a -> a) -> Operator Char S a
infixLeft__ c r =
    Infix
        (Parser.succeed r
            |. lexeme
                (Parser.succeed ()
                    |. try (char c)
                    |. notFollowedBy (oneOf (String.cons c "="))
                )
        )
        AssocLeft


infixRight : String -> (a -> a -> a) -> Operator Char S a
infixRight s r =
    Infix
        (Parser.succeed r
            |. lexeme (try (string s))
        )
        AssocRight


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
    choice
        [ Parser.map Variable (try identifier)

        -- int constant
        , intConstant

        -- uint constant
        -- float constant
        , floatingConstant

        -- bool constant
        , Parser.succeed (BoolConstant True)
            |. keyword "true"
        , Parser.succeed (BoolConstant False)
            |. keyword "false"

        -- expression within parentheses
        , between lparen rparen expression
        ]


postfixExpression : P Expr
postfixExpression =
    Parser.succeed (List.foldl (<|))
        |= Parser.oneOf
            [ try
                (Parser.succeed (\( i, p ) -> FunctionCall i p)
                    |= Parser.lazy (\_ -> functionCallGeneric)
                )
            , Parser.lazy (\_ -> primaryExpression)
            ]
        |= many
            (choice
                [ Parser.succeed (Utils.flip Bracket)
                    |= between lbracket rbracket (Parser.lazy (\_ -> integerExpression))
                , Parser.lazy (\_ -> dotFunctionCallGeneric)
                , dotFieldSelection
                , Parser.succeed PostInc
                    |. operator "++"
                , Parser.succeed PostDec
                    |. operator "--"
                ]
            )


dotFunctionCallGeneric : P (Expr -> Expr)
dotFunctionCallGeneric =
    Parser.succeed (\( i, p ) e -> MethodCall e i p)
        |= lexeme
            (Parser.succeed identity
                |. try (string ".")
                |= Parser.lazy (\_ -> functionCallGeneric)
            )


dotFieldSelection : P (Expr -> Expr)
dotFieldSelection =
    Parser.succeed (Utils.flip FieldSelection)
        |= lexeme
            (Parser.succeed identity
                |. try (string ".")
                |= identifier
            )


integerExpression : P Expr
integerExpression =
    Parser.lazy (\_ -> expression)



-- Those productions are pushed inside postfixExpression.
-- functionCall = functionCallOrMethod
-- functionCallOrMethod = functionCallGeneric <|> postfixExpression DOT functionCallGeneric


functionCallGeneric : P ( FunctionIdentifier, Parameters )
functionCallGeneric =
    Parser.succeed Tuple.pair
        |= Parser.lazy (\_ -> functionCallHeader)
        |= choice
            [ Parser.succeed ParamVoid
                |. keyword "void"
            , Parser.succeed Params
                |= sepBy assignmentExpression comma
            ]
        |. rparen



-- Those productions are pushed inside functionCallGeneric.
-- functionCallHeaderNoParameters = undefined
-- functionCallHeaderWithParameters = undefined


functionCallHeader : P FunctionIdentifier
functionCallHeader =
    Parser.succeed identity
        |= Parser.lazy (\_ -> functionIdentifier)
        |. lparen


functionIdentifier : P FunctionIdentifier
functionIdentifier =
    choice
        [ Parser.succeed FuncId
            |= try (Parser.lazy (\_ -> identifier))

        -- TODO if the 'identifier' is declared as a type, should be this case
        , Parser.succeed FuncIdTypeSpec
            |= Parser.lazy (\_ -> typeSpecifier)

        -- no need for fieldSelection
        ]


unaryExpression : P Expr
unaryExpression =
    Parser.succeed (\p e -> List.foldr (<|) e p)
        |= many
            (choice
                [ Parser.succeed PreInc
                    |. operator "++"
                , Parser.succeed PreDec
                    |. operator "--"
                , Parser.succeed UnaryPlus
                    |. operator "+"
                , Parser.succeed UnaryNegate
                    |. operator "-"
                , Parser.succeed UnaryNot
                    |. operator "!"
                , Parser.succeed UnaryOneComplement
                    |. operator "~"
                ]
            )
        |= Parser.lazy (\_ -> postfixExpression)



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
    Parser.succeed Tuple.pair
        |= buildExpressionParser conditionalTable (Parser.lazy (\_ -> unaryExpression))
        |= optionMaybe
            (Parser.succeed Tuple.pair
                |. lexeme (string "?")
                |= expression
                |. lexeme (string ":")
                |= assignmentExpression
            )
        |> Parser.map
            (\( loe, ter ) ->
                case ter of
                    Nothing ->
                        loe

                    Just ( e, a ) ->
                        Selection loe e a
            )


assignmentExpression : P Expr
assignmentExpression =
    buildExpressionParser assignmentTable (Parser.lazy (\_ -> conditionalExpression))


expression : P Expr
expression =
    buildExpressionParser expressionTable assignmentExpression


constantExpression : P Expr
constantExpression =
    Parser.lazy (\_ -> conditionalExpression)



-- The GLSL grammar include here function definition but we don't
-- do this here because they should occur only at top level (page 28).
-- Function definitions are handled in externalDefinition instead.


declaration : P Declaration
declaration =
    let
        idecl =
            Parser.succeed InitDecl
                |= identifier
                |= optionMaybe (between lbracket rbracket (optionMaybe constantExpression))
                |= optionMaybe (Parser.succeed identity |. lexeme (string "=") |= initializer)
    in
    choice
        [ try <|
            Parser.succeed (\t l -> InitDeclaration (TypeDeclarator t) l)
                |= fullySpecifiedType
                |= sepBy idecl comma
                |. semicolon
        , Parser.succeed (InitDeclaration InvariantDeclarator)
            |. keyword "invariant"
            |= sepBy idecl comma
            |. semicolon
        , Parser.succeed Precision
            |. keyword "precision"
            |= precisionQualifier
            |= Parser.lazy (\_ -> typeSpecifierNoPrecision)
            |. semicolon
        , typeQualifier
            |> Parser.andThen
                (\q ->
                    choice
                        [ Parser.succeed (TQ q)
                            |. semicolon
                        , Parser.succeed (Block q)
                            |= identifier
                            |. lbrace
                            |= Parser.lazy (\_ -> structDeclarationList)
                            |. rbrace
                            |= optionMaybe
                                (Parser.succeed Tuple.pair
                                    |= identifier
                                    |= optionMaybe
                                        (between lbracket
                                            rbracket
                                            (optionMaybe constantExpression)
                                        )
                                )
                            |. semicolon
                        ]
                )
        ]


functionPrototype : P FunctionPrototype
functionPrototype =
    Parser.succeed (\( t, i, p ) -> FuncProt t i p)
        |= functionDeclarator
        |. rparen


functionDeclarator : P ( FullType, String, List ParameterDeclaration )
functionDeclarator =
    Parser.succeed (\( t, i ) p -> ( t, i, p ))
        |= functionHeader
        |= sepBy parameterDeclaration comma



-- inside functionDeclarator
-- functionHeaderWithParameters = undefined


functionHeader : P ( FullType, String )
functionHeader =
    Parser.succeed (\t i -> ( t, i ))
        |= fullySpecifiedType
        |= identifier
        |. lparen



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
    Parser.succeed ParameterDeclaration
        |= optionMaybe parameterTypeQualifier
        |= optionMaybe parameterQualifier
        |= typeSpecifier
        |= optionMaybe
            (Parser.succeed Tuple.pair
                |= identifier
                -- FIXME can't the bracket be empty, i.e. a[] ?
                |= optionMaybe (between lbracket rbracket constantExpression)
            )


parameterQualifier : P ParameterQualifier
parameterQualifier =
    choice
        -- "empty" case handled in the caller
        [ Parser.succeed InOutParameter
            |. (try << lexeme << string) "inout"
        , Parser.succeed InParameter
            |. (try << lexeme << string) "in"
        , Parser.succeed OutParameter
            |. (try << lexeme << string) "out"
        ]



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
    choice
        [ Parser.succeed (FullType Nothing)
            |= try typeSpecifier
        , Parser.succeed (\q s -> FullType (Just q) s)
            |= typeQualifier
            |= typeSpecifier
        ]


invariantQualifier : P InvariantQualifier
invariantQualifier =
    Parser.succeed Invariant
        |. keyword "invariant"


interpolationQualifier : P InterpolationQualifier
interpolationQualifier =
    choice
        [ Parser.succeed Smooth
            |. keyword "smooth"
        , Parser.succeed Flat
            |. keyword "flat"
        , Parser.succeed NoPerspective
            |. keyword "noperspective"
        ]


layoutQualifier : P LayoutQualifier
layoutQualifier =
    Parser.succeed Layout
        |. keyword "layout"
        |. lparen
        |= sepBy layoutQualifierId comma
        |. rparen



-- implemented directly in layoutQualifier
-- layoutQualifierIdList = undefined


layoutQualifierId : P LayoutQualifierId
layoutQualifierId =
    Parser.succeed LayoutQualId
        |= identifier
        |= optionMaybe
            (Parser.succeed identity
                |. lexeme (string "=")
                |= intConstant
            )


parameterTypeQualifier : P ParameterTypeQualifier
parameterTypeQualifier =
    Parser.succeed ConstParameter
        |. keyword "const"



-- sto
-- lay [sto]
-- int [sto]
-- inv [sto]
-- inv int sto


typeQualifier : P TypeQualifier
typeQualifier =
    choice
        [ Parser.succeed TypeQualSto
            |= storageQualifier
        , Parser.succeed TypeQualLay
            |= layoutQualifier
            |= optionMaybe storageQualifier
        , Parser.succeed TypeQualInt
            |= interpolationQualifier
            |= optionMaybe storageQualifier
        , invariantQualifier
            |> Parser.andThen
                (\i ->
                    choice
                        [ Parser.succeed (TypeQualInv3 i)
                            |= interpolationQualifier
                            |= storageQualifier
                        , Parser.succeed (TypeQualInv i)
                            |= optionMaybe storageQualifier
                        ]
                )
        ]



-- TODO see 4.3 for restrictions


storageQualifier : P StorageQualifier
storageQualifier =
    choice
        [ Parser.succeed Const
            |. keyword "const"

        -- TODO vertex only, is deprecated
        , Parser.succeed Attribute
            |. keyword "attribute"

        -- deprecated
        , Parser.succeed Varying
            |. keyword "varying"
        , Parser.succeed In
            |. keyword "in"
        , Parser.succeed Out
            |. keyword "out"
        , Parser.succeed identity
            |. keyword "centroid"
            |= choice
                [ -- deprecated
                  Parser.succeed CentroidVarying
                    |. keyword "varying"
                , Parser.succeed CentroidIn
                    |. keyword "in"
                , Parser.succeed CentroidOut
                    |. keyword "out"
                ]
        , Parser.succeed Uniform
            |. keyword "uniform"
        ]


typeSpecifier : P TypeSpecifier
typeSpecifier =
    choice
        [ Parser.succeed (\q s -> TypeSpec (Just q) s)
            |= try precisionQualifier
            |= Parser.lazy (\_ -> typeSpecifierNoPrecision)
        , Parser.succeed (TypeSpec Nothing)
            |= Parser.lazy (\_ -> typeSpecifierNoPrecision)
        ]


typeSpecifierNoPrecision : P TypeSpecifierNoPrecision
typeSpecifierNoPrecision =
    Parser.lazy (\_ -> typeSpecifierNonArray)
        |> Parser.andThen
            (\s ->
                choice
                    [ Parser.succeed (TypeSpecNoPrecision s (Just Nothing))
                        |. try (Parser.succeed () |. lbracket |. rbracket)
                    , Parser.succeed (TypeSpecNoPrecision s << Just << Just)
                        |. lbracket
                        |= constantExpression
                        |. rbracket
                    , Parser.succeed (TypeSpecNoPrecision s Nothing)
                    ]
            )



-- Basic types, structs, and user-defined types.


typeSpecifierNonArray : P TypeSpecifierNonArray
typeSpecifierNonArray =
    choice
        [ Parser.succeed Void
            |. keyword "void"
        , Parser.succeed Float
            |. keyword "float"
        , Parser.succeed Int
            |. keyword "int"
        , Parser.succeed UInt
            |. keyword "uint"
        , Parser.succeed Bool
            |. keyword "bool"
        , Parser.succeed Vec2
            |. keyword "vec2"
        , Parser.succeed Vec3
            |. keyword "vec3"
        , Parser.succeed Vec4
            |. keyword "vec4"
        , Parser.succeed BVec2
            |. keyword "bvec2"
        , Parser.succeed BVec3
            |. keyword "bvec3"
        , Parser.succeed BVec4
            |. keyword "bvec4"
        , Parser.succeed IVec2
            |. keyword "ivec2"
        , Parser.succeed IVec3
            |. keyword "ivec3"
        , Parser.succeed IVec4
            |. keyword "ivec4"
        , Parser.succeed UVec2
            |. keyword "uvec2"
        , Parser.succeed UVec3
            |. keyword "uvec3"
        , Parser.succeed UVec4
            |. keyword "uvec4"
        , Parser.succeed Mat2
            |. keyword "mat2"
        , Parser.succeed Mat3
            |. keyword "mat3"
        , Parser.succeed Mat4
            |. keyword "mat4"
        , Parser.succeed Mat2x2
            |. keyword "mat2x2"
        , Parser.succeed Mat2x3
            |. keyword "mat2x3"
        , Parser.succeed Mat2x4
            |. keyword "mat2x4"
        , Parser.succeed Mat3x2
            |. keyword "mat3x2"
        , Parser.succeed Mat3x3
            |. keyword "mat3x3"
        , Parser.succeed Mat3x4
            |. keyword "mat3x4"
        , Parser.succeed Mat4x2
            |. keyword "mat4x2"
        , Parser.succeed Mat4x3
            |. keyword "mat4x3"
        , Parser.succeed Mat4x4
            |. keyword "mat4x4"
        , Parser.succeed Sampler1D
            |. keyword "sampler1D"
        , Parser.succeed Sampler2D
            |. keyword "sampler2D"
        , Parser.succeed Sampler3D
            |. keyword "sampler3D"
        , Parser.succeed SamplerCube
            |. keyword "samplerCube"
        , Parser.succeed Sampler1DShadow
            |. keyword "sampler1DShadow"
        , Parser.succeed Sampler2DShadow
            |. keyword "sampler2DShadow"
        , Parser.succeed SamplerCubeShadow
            |. keyword "samplerCubeShadow"
        , Parser.succeed Sampler1DArray
            |. keyword "sampler1DArray"
        , Parser.succeed Sampler2DArray
            |. keyword "sampler2DArray"
        , Parser.succeed Sampler1DArrayShadow
            |. keyword "sampler1DArrayShadow"
        , Parser.succeed Sampler2DArrayShadow
            |. keyword "sampler2DArrayShadow"
        , Parser.succeed ISampler1D
            |. keyword "isampler1D"
        , Parser.succeed ISampler2D
            |. keyword "isampler2D"
        , Parser.succeed ISampler3D
            |. keyword "isampler3D"
        , Parser.succeed ISamplerCube
            |. keyword "isamplerCube"
        , Parser.succeed ISampler1DArray
            |. keyword "isampler1DArray"
        , Parser.succeed ISampler2DArray
            |. keyword "isampler2DArray"
        , Parser.succeed USampler1D
            |. keyword "usampler1D"
        , Parser.succeed USampler2D
            |. keyword "usampler2D"
        , Parser.succeed USampler3D
            |. keyword "usampler3D"
        , Parser.succeed USamplerCube
            |. keyword "usamplerCube"
        , Parser.succeed USampler1DArray
            |. keyword "usampler1DArray"
        , Parser.succeed USampler2DArray
            |. keyword "usampler2DArray"
        , Parser.succeed Sampler2DRect
            |. keyword "sampler2DRect"
        , Parser.succeed Sampler2DRectShadow
            |. keyword "sampler2DRectShadow"
        , Parser.succeed ISampler2DRect
            |. keyword "isampler2DRect"
        , Parser.succeed USampler2DRect
            |. keyword "usampler2DRect"
        , Parser.succeed SamplerBuffer
            |. keyword "samplerBuffer"
        , Parser.succeed ISamplerBuffer
            |. keyword "isamplerBuffer"
        , Parser.succeed USamplerBuffer
            |. keyword "usamplerBuffer"
        , Parser.succeed Sampler2DMS
            |. keyword "sampler2DMS"
        , Parser.succeed ISampler2DMS
            |. keyword "isampler2DMS"
        , Parser.succeed USampler2DMS
            |. keyword "usampler2DMS"
        , Parser.succeed Sampler2DMSArray
            |. keyword "sampler2DMSArray"
        , Parser.succeed ISampler2DMSArray
            |. keyword "isampler2DMSArray"
        , Parser.succeed USampler2DMSArray
            |. keyword "usampler2DMSArray"
        , Parser.lazy (\_ -> structSpecifier)

        -- verify if it is declared
        , Parser.succeed TypeName
            |= identifier
        ]


precisionQualifier : P PrecisionQualifier
precisionQualifier =
    choice
        [ Parser.succeed HighP
            |. keyword "highp"
        , Parser.succeed MediumP
            |. keyword "mediump"
        , Parser.succeed LowP
            |. keyword "lowp"
        ]


structSpecifier : P TypeSpecifierNonArray
structSpecifier =
    Parser.succeed StructSpecifier
        |. keyword "struct"
        |= optionMaybe identifier
        |. lbrace
        |= Parser.lazy (\_ -> structDeclarationList)
        |. rbrace


structDeclarationList : P (List Field)
structDeclarationList =
    many1 (Parser.lazy (\_ -> structDeclaration))


structDeclaration : P Field
structDeclaration =
    Parser.succeed Field
        |= optionMaybe typeQualifier
        |= typeSpecifier
        |= Parser.lazy (\_ -> structDeclaratorList)
        |. semicolon


structDeclaratorList : P (List StructDeclarator)
structDeclaratorList =
    sepBy (Parser.lazy (\_ -> structDeclarator)) comma


structDeclarator : P StructDeclarator
structDeclarator =
    -- Parser.lazy (\_ -> identifier)
    --     |> Parser.andThen
    --         (\i ->
    --             choice
    --                 [ Parser.succeed (\e -> StructDeclarator i (Just e))
    --                     |. lbracket
    --                     |= optionMaybe (Parser.lazy (\_ -> constantExpression))
    --                     |. rbracket
    --                 , Parser.succeed (StructDeclarator i Nothing)
    --                 ]
    --         )
    Parser.lazy (\_ -> Debug.todo "structDeclarator")


initializer : P Expr
initializer =
    -- assignmentExpression
    Debug.todo "initializer"


declarationStatement : P Declaration
declarationStatement =
    -- declaration
    Debug.todo "declarationStatement"


statement : P Statement
statement =
    -- Parser.oneOf
    --     [ Parser.map CompoundStatement (Parser.lazy (\_ -> compoundStatement))
    --     , Parser.lazy (\_ -> simpleStatement)
    --     ]
    Debug.todo "statement"


simpleStatement : P Statement
simpleStatement =
    choice
        [ Parser.succeed DeclarationStatement
            |= declarationStatement
        , Parser.succeed ExpressionStatement
            |= expressionStatement
        , selectionStatement
        , switchStatement
        , Parser.succeed CaseLabel
            |= caseLabel
        , iterationStatement
        , jumpStatement
        ]


compoundStatement : P Compound
compoundStatement =
    choice
        [ Parser.succeed (Compound [])
            |. try
                (Parser.succeed ()
                    |. lbrace
                    |. rbrace
                )
        , Parser.succeed Compound
            |= between lbrace rbrace (Parser.lazy (\_ -> statementList))
        ]


statementNoNewScope : P Statement
statementNoNewScope =
    Parser.oneOf
        [ Parser.map CompoundStatement compoundStatementNoNewScope
        , simpleStatement
        ]


compoundStatementNoNewScope : P Compound
compoundStatementNoNewScope =
    compoundStatement


statementList : P (List Statement)
statementList =
    many1 statement


expressionStatement : P (Maybe Expr)
expressionStatement =
    choice
        [ Parser.succeed Nothing |. semicolon
        , expression
            |> Parser.andThen (\e -> Parser.succeed (Just e) |. semicolon)
        ]


selectionStatement : P Statement
selectionStatement =
    Parser.succeed SelectionStatement
        |. keyword "if"
        |. lparen
        |= expression
        |. rparen
        |= statement
        |= optionMaybe
            (Parser.succeed identity
                |. keyword "else"
                |= statement
            )



-- inside selectionStatement
-- selectionRestStatement = undefined


condition : P Condition
condition =
    choice
        [ Parser.succeed Condition
            |= expression
        , Parser.succeed InitializedCondition
            |= fullySpecifiedType
            |= identifier
            |. lexeme (string "=")
            |= initializer
        ]


switchStatement : P Statement
switchStatement =
    Parser.succeed SwitchStatement
        |. keyword "switch"
        |. lparen
        |= expression
        |. rparen
        |. lbrace
        |= switchStatementList
        |. rbrace


switchStatementList : P (List Statement)
switchStatementList =
    many statement


caseLabel : P CaseLabel
caseLabel =
    choice
        [ Parser.succeed identity
            |. keyword "case"
            |= expression
            |> Parser.andThen
                (\e ->
                    Parser.succeed (Case e)
                        |. colon
                )
        , Parser.succeed Default
            |. keyword "default"
            |. colon
        ]


iterationStatement : P Statement
iterationStatement =
    choice
        [ Parser.succeed While
            |. keyword "while"
            |. lparen
            |= condition
            |. rparen
            |= Parser.lazy (\_ -> statementNoNewScope)
        , Parser.succeed DoWhile
            |. keyword "do"
            |= statement
            |. keyword "while"
            |. lparen
            |= expression
            |. rparen
            |. semicolon
        , Parser.succeed For
            |. keyword "for"
            |. lparen
            |= forInitStatement
            |= optionMaybe condition
            |. semicolon
            |= optionMaybe expression
            |. rparen
            |= Parser.lazy (\_ -> statementNoNewScope)
        ]


forInitStatement : P (Result (Maybe Expr) Declaration)
forInitStatement =
    Parser.oneOf
        [ Parser.succeed Err
            |= expressionStatement
        , Parser.succeed Ok
            |= declarationStatement
        ]



-- inside iterationStatement
-- conditionOp = undefined
-- inside iterationStatement
-- forRestStatement = undefined


jumpStatement : P Statement
jumpStatement =
    choice
        [ Parser.succeed Continue
            |. keyword "continue"
            |. semicolon
        , Parser.succeed Break
            |. keyword "break"
            |. semicolon
        , Parser.succeed (Return Nothing)
            |. try
                (Parser.succeed ()
                    |. keyword "return"
                    |. semicolon
                )
        , Parser.succeed identity
            |. keyword "return"
            |= expression
            |> Parser.andThen
                (\e ->
                    Parser.succeed (Return <| Just e)
                        |. semicolon
                )
        , Parser.succeed Discard
            |. keyword "discard"
            |. semicolon
        ]


translationUnit : P TranslationUnit
translationUnit =
    Parser.map TranslationUnit (many1 externalDeclaration)


externalDeclaration : P ExternalDeclaration
externalDeclaration =
    choice
        [ --  try functionPrototype
          -- |> Parser.andThen
          --     (\p ->
          --         choice
          --             [ Parser.succeed (FunctionDeclaration p)
          --                 |. semicolon
          --             , Parser.succeed (FunctionDefinition p)
          --                 |= compoundStatementNoNewScope
          --             ]
          --     )
          Parser.map Declaration declaration
        ]



-- inside externalDeclaration, used only in tests


functionDefinition : P ExternalDeclaration
functionDefinition =
    Parser.succeed FunctionDefinition
        |= functionPrototype
        |= compoundStatementNoNewScope
