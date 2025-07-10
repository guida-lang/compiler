module Common.Format.Cheapskate.ParserCombinators exposing (..)


type Position = Position  Int Int

showPosition : Position -> String
showPosition  (Position ln cn) = "line " ++ String.fromInt ln ++ " column " ++ String.fromInt cn

-- the String indicates what the parser was expecting
type ParseError = ParseError Position String

type ParserState = ParserState { subject  : Text
                               , position : Position
                               , lastChar : Maybe Char
                               }

advance : ParserState -> Text -> ParserState
advance = 
    
    -- let
    --     go :: ParserState -> Char -> ParserState
    --     go st c = st{ subject = T.drop 1 (subject st)
    --                     , position = case c of
    --                                     '\n' -> Position { line =
    --                                                 line (position st) + 1
    --                                                 , column = 1 }
    --                                     _    -> Position { line =
    --                                                 line (position st)
    --                                                 , column =
    --                                                 column (position st) + 1
    --                                                 }
    --                     , lastChar = Just c }
    -- in
    -- T.foldl' go
    Debug.todo "advance"

type Parser a = Parser (ParserState -> Result ParseError (ParserState, a))

-- instance Functor Parser where
--   fmap f (Parser g) = Parser $ \st ->
--     case g st of
--          Right (st', x) -> Right (st', f x)
--          Left e         -> Left e

-- instance Applicative Parser where
--   pure x = Parser $ \st -> Right (st, x)
--   (Parser f) <*> (Parser g) = Parser $ \st ->
--     case f st of
--          Left e         -> Left e
--          Right (st', h) -> case g st' of
--                                 Right (st'', x) -> Right (st'', h x)
--                                 Left e          -> Left e

-- instance Alternative Parser where
--   empty = Parser $ \st -> Left $ ParseError (position st) "(empty)"
--   (Parser f) <|> (Parser g) = Parser $ \st ->
--     case f st of
--          Right res                 -> Right res
--          Left (ParseError pos msg) ->
--            case g st of
--              Right res                   -> Right res
--              Left (ParseError pos' msg') -> Left $
--                case () of
--                   -- return error for farthest match
--                   _ | pos' > pos  -> ParseError pos' msg'
--                     | pos' < pos  -> ParseError pos msg
--                     | otherwise {- pos' == pos -}
--                                   -> ParseError pos (msg ++ " or " ++ msg')

-- instance Monad Parser where
--   return x = Parser $ \st -> Right (st, x)
--   p >>= g = Parser $ \st ->
--     case evalParser p st of
--          Left e        -> Left e
--          Right (st',x) -> evalParser (g x) st'

-- instance MonadFail Parser where
--   fail e = Parser $ \st -> Left $ ParseError (position st) e

-- instance MonadPlus Parser where
--   mzero = Parser $ \st -> Left $ ParseError (position st) "(mzero)"
--   mplus p1 p2 = Parser $ \st ->
--     case evalParser p1 st of
--          Right res  -> Right res
--          Left _     -> evalParser p2 st

-- (<?>) :: Parser a -> String -> Parser a
-- p <?> msg = Parser $ \st ->
--   let startpos = position st in
--   case evalParser p st of
--        Left (ParseError _ _) ->
--            Left $ ParseError startpos msg
--        Right r                 -> Right r
-- infixl 5 <?>

parse : Parser a -> Text -> Result ParseError a
parse p t =
  fmap Tuple.second ( evalParser p (ParserState { subject  = t
                                     , position = Position 1 1
                                     , lastChar = Nothing }))

failure : ParserState -> String -> Result ParseError (ParserState, a)
failure st msg = Err (ParseError (position st) msg)

success : ParserState -> a -> Result ParseError (ParserState, a)
success st x = Ok (st, x)

satisfy : (Char -> Bool) -> Parser Char
satisfy f =
  let
    g st =
        case T.uncons (subject st) of
                    Just (c, _)  ->
                        if f c then
                         success (advance st (T.singleton c)) c
                        else failure st "character meeting condition"
                    _ -> failure st "character meeting condition"
  in
  Parser g

peekChar : Parser (Maybe Char)
peekChar = Parser (\st ->
             case T.uncons (subject st) of
                  Just (c, _) -> success st (Just c)
                  Nothing     -> success st Nothing)

peekLastChar : Parser (Maybe Char)
peekLastChar = Parser (\st -> success st (lastChar st))

notAfter : (Char -> Bool) -> Parser ()
notAfter f = 
    -- do
    --   mbc <- peekLastChar
    --   case mbc of
    --        Nothing -> return ()
    --        Just c  -> if f c then mzero else return ()
    Debug.todo "notAfter"

-- low-grade version of attoparsec's:
charClass : String -> Set.Set Char
charClass = 
    -- Set.fromList . go
    -- where go (a:'-':b:xs) = [a..b] ++ go xs
    --       go (x:xs) = x : go xs
    --       go _ = ""
    Debug.todo "charClass"

inClass : String -> Char -> Bool
inClass s c = 
  let
        s_ = charClass s
  in
    Set.member c  s_

notInClass : String -> Char -> Bool
notInClass s = not << inClass s

endOfInput : Parser ()
endOfInput = Parser (\st ->
  if T.null (subject st)
     then success st ()
     else failure st "end of input")

char : Char -> Parser Char
char c = satisfy ((==) c)

anyChar : Parser Char
anyChar = satisfy (\_ -> True)

getPosition : Parser Position
getPosition = Parser (\st -> success st (position st))

-- note: this does not actually change the position in the subject;
-- it only changes what column counts as column N.  It is intended
-- to be used in cases where we're parsing a partial line but need to
-- have accurate column information.
setPosition : Position -> Parser ()
setPosition pos = Parser (\st -> success st{ position = pos } ())

takeWhile : (Char -> Bool) -> Parser Text
takeWhile f = Parser (\st ->
  let t = T.takeWhile f (subject st) in
  success (advance st t) t)

takeTill : (Char -> Bool) -> Parser Text
takeTill f = takeWhile (not << f)

takeWhile1 : (Char -> Bool) -> Parser Text
takeWhile1 f = Parser (\st ->
    let
        t = T.takeWhile f (subject st)
    in
    if String.isEmpty t then
  
        failure st "characters satisfying condition"
    else
        success (advance st t) t)

takeText : Parser Text
takeText = Parser (\st ->
  let t = subject st in
  success (advance st t) t)

skip : (Char -> Bool) -> Parser ()
skip f = 
    Parser 
        (\st ->
            case T.uncons (subject st) of
                Just (c, _) ->
                            if f c then
                                success (advance st (T.singleton c)) ()
                            else
                                failure st "character satisfying condition"
                _   -> failure st "character satisfying condition"
        )

skipWhile : (Char -> Bool) -> Parser ()
skipWhile f = Parser (\st ->
  let t_ = T.takeWhile f (subject st) in
  success (advance st t_) ())

string : Text -> Parser Text
string s = Parser (\st ->
    if T.isPrefixOf  (subject st) then
        success (advance st s) s
    else
        failure st "string"
    )

scan : s -> (s -> Char -> Maybe s) -> Parser Text
scan s0 f =
  let
        go s cs st =
         case T.uncons (subject st) of
               Nothing        -> finish st cs
               Just (c, _)    -> case f s c of
                                  Just s_ -> go s_ (c :: cs)
                                              (advance st (T.singleton c))
                                  Nothing -> finish st cs
        finish st cs =
            success st (T.pack (reverse cs))
  in
  Parser (go s0 [])

lookAhead : Parser a -> Parser a
lookAhead p = Parser (\st ->
  case evalParser p st of
       Ok (_,x) -> success st x
       Err _      -> failure st "lookAhead")

notFollowedBy : Parser a -> Parser ()
notFollowedBy p = Parser (\st ->
  case evalParser p st of
       Ok (_,_) -> failure st "notFollowedBy"
       Err _      -> success st ())

-- combinators (definitions borrowed from attoparsec)

option : a -> List a -> List a
option x p = 
    -- p <|> pure x
    Debug.todo "option"

many1 : f a -> f (List a)
many1 p =
    -- liftA2 (::) p (many p)
    Debug.todo "many1"

manyTill : f a -> f b -> f (List a)
manyTill p end = 
--   let go = (end *> pure []) <|> liftA2 (::) p go
--   in
  go

skipMany : f a -> f ()
skipMany p = 
--   let go = (p *> go) <|> pure ()
--   in
    go

skipMany1 : f a -> f ()
skipMany1 p = 
    -- p *> skipMany p
    Debug.todo "skipMany1"

count : Int -> m a -> m [a]
count n p =
    sequence (replicate n p)
