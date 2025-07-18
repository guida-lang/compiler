module Common.Format.Cheapskate.Inlines exposing
    ( pHtmlTag
    , pLinkLabel
    , pReference
    , parseInlines
    )

import Common.Format.Cheapskate.ParserCombinators exposing (..)
import Common.Format.Cheapskate.Types exposing (..)
import Common.Format.Cheapskate.Util exposing (..)
import Set exposing (Set)
import Utils.Crash exposing (crash)



-- import Cheapskate.Util
-- import Data.Char hiding (Space)
-- import qualified Data.Sequence as Seq
-- import Data.Sequence (singleton, (<|), viewl, ViewL(..))
-- import Prelude hiding (takeWhile)
-- import Control.Applicative
-- import Control.Monad
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Set as Set
-- Returns tag type and whole tag.


pHtmlTag : Parser ( HtmlTagType, String )
pHtmlTag =
    char '<'
        |> bind
            (\_ ->
                -- do not end the tag with a > character in a quoted attribute.
                oneOf (char '/' |> fmap (\_ -> True)) (return False)
                    |> bind
                        (\closing ->
                            takeWhile1 (\c -> isAsciiAlphaNum c || c == '?' || c == '!')
                                |> bind
                                    (\tagname ->
                                        let
                                            tagname_ =
                                                String.toLower tagname

                                            attr =
                                                takeWhile isSpace
                                                    |> bind
                                                        (\ss ->
                                                            satisfy Char.isAlpha
                                                                |> bind
                                                                    (\x ->
                                                                        takeWhile (\c -> isAsciiAlphaNum c || c == ':')
                                                                            |> bind
                                                                                (\xs ->
                                                                                    skip ((==) '=')
                                                                                        |> bind (\_ -> oneOf (pQuoted '"') (oneOf (pQuoted '\'') (oneOf (takeWhile1 Char.isAlphaNum) (return ""))))
                                                                                        |> fmap
                                                                                            (\v ->
                                                                                                ss ++ String.fromChar x ++ xs ++ "=" ++ v
                                                                                            )
                                                                                )
                                                                    )
                                                        )
                                        in
                                        many attr
                                            |> fmap String.concat
                                            |> bind
                                                (\attrs ->
                                                    takeWhile (\c -> isSpace c || c == '/')
                                                        |> bind
                                                            (\final ->
                                                                char '>'
                                                                    |> bind
                                                                        (\_ ->
                                                                            let
                                                                                tagtype =
                                                                                    if closing then
                                                                                        Closing tagname_

                                                                                    else
                                                                                        case stringStripSuffix "/" final of
                                                                                            Just _ ->
                                                                                                SelfClosing tagname_

                                                                                            Nothing ->
                                                                                                Opening tagname_
                                                                            in
                                                                            return
                                                                                ( tagtype
                                                                                , String.fromList
                                                                                    ('<'
                                                                                        :: (if closing then
                                                                                                [ '/' ]

                                                                                            else
                                                                                                []
                                                                                           )
                                                                                    )
                                                                                    ++ tagname
                                                                                    ++ attrs
                                                                                    ++ final
                                                                                    ++ ">"
                                                                                )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


isSpace : Char -> Bool
isSpace c =
    c == '\t' || c == '\n' || c == '\u{000D}'


stringStripSuffix : String -> String -> Maybe String
stringStripSuffix p t =
    if String.endsWith p t then
        Just (String.dropRight (String.length p) t)

    else
        Nothing



-- Parses a quoted attribute value.


pQuoted : Char -> Parser String
pQuoted c =
    skip ((==) c)
        |> bind (\_ -> takeTill ((==) c))
        |> bind
            (\contents ->
                skip ((==) c)
                    |> fmap (\_ -> String.fromChar c ++ contents ++ String.fromChar c)
            )



-- Parses an HTML comment. This isn't really correct to spec, but should
-- do for now.


pHtmlComment : Parser String
pHtmlComment =
    string "<!--"
        |> bind (\_ -> manyTill anyChar (string "-->"))
        |> fmap (\rest -> "<!--" ++ String.fromList rest ++ "-->")


{-| A link label [like this]. Note the precedence: code backticks have
precedence over label bracket markers, which have precedence over
\*, \_, and other inline formatting markers.
So, 2 below contains a link while 1 does not:

1.  [a link `with a ](/url)` character
2.  [a link \*with emphasized ](/url) text\*

-}
pLinkLabel : Parser String
pLinkLabel =
    let
        regChunk : Parser String
        regChunk =
            takeWhile1 (\c -> c /= '`' && c /= '[' && c /= ']' && c /= '\\')

        codeChunk : Parser String
        codeChunk =
            fmap Tuple.second pCode_

        bracketed : Parser String
        bracketed =
            lazy (\() -> pLinkLabel)
                |> fmap inBrackets

        inBrackets : String -> String
        inBrackets t =
            "[" ++ t ++ "]"
    in
    char '['
        |> bind
            (\_ ->
                fmap String.concat
                    (manyTill (oneOf regChunk (oneOf pEscaped (oneOf bracketed codeChunk))) (char ']'))
            )



-- A URL in a link or reference.  This may optionally be contained
-- in `<..>`; otherwise whitespace and unbalanced right parentheses
-- aren't allowed.  Newlines aren't allowed in any case.


pLinkUrl : Parser String
pLinkUrl =
    oneOf (char '<' |> bind (\_ -> return True)) (return False)
        |> bind
            (\inPointy ->
                if inPointy then
                    manyTill (pSatisfy (\c -> c /= '\u{000D}' && c /= '\n')) (char '>')
                        |> fmap String.fromList

                else
                    let
                        regChunk : Parser String
                        regChunk =
                            oneOf (takeWhile1 (notInClass " \n()\\")) pEscaped

                        parenChunk : () -> Parser String
                        parenChunk () =
                            char '('
                                |> bind (\_ -> manyTill (oneOf regChunk (lazy parenChunk)) (char ')'))
                                |> fmap (parenthesize << String.concat)

                        parenthesize : String -> String
                        parenthesize x =
                            "(" ++ x ++ ")"
                    in
                    fmap String.concat (many (oneOf regChunk (parenChunk ())))
            )



-- A link title, single or double quoted or in parentheses.
-- Note that Markdown.pl doesn't allow the parenthesized form in
-- inline links -- only in references -- but this restriction seems
-- arbitrary, so we remove it here.


pLinkTitle : Parser String
pLinkTitle =
    satisfy (\c -> c == '"' || c == '\'' || c == '(')
        |> bind
            (\c ->
                peekChar
                    |> bind
                        (\next ->
                            case next of
                                Nothing ->
                                    mzero

                                Just x ->
                                    if isWhitespace x then
                                        mzero

                                    else if x == ')' then
                                        mzero

                                    else
                                        return ()
                        )
                    |> bind
                        (\_ ->
                            let
                                ender : Char
                                ender =
                                    if c == '(' then
                                        ')'

                                    else
                                        c

                                pEnder : Parser Char
                                pEnder =
                                    bind (\_ -> char ender) (nfb (skip Char.isAlphaNum))

                                regChunk : Parser String
                                regChunk =
                                    oneOf (takeWhile1 (\x -> x /= ender && x /= '\\')) pEscaped

                                nestedChunk : Parser String
                                nestedChunk =
                                    lazy (\() -> pLinkTitle)
                                        |> fmap (\x -> String.fromChar c ++ x ++ String.fromChar ender)
                            in
                            fmap String.concat (manyTill (oneOf regChunk nestedChunk) pEnder)
                        )
            )



-- A link reference is a square-bracketed link label, a colon,
-- optional space or newline, a URL, optional space or newline,
-- and an optional link title.  (Note:  we assume the input is
-- pre-stripped, with no leading/trailing spaces.)


pReference : Parser ( String, String, String )
pReference =
    pLinkLabel
        |> bind
            (\lab ->
                char ':'
                    |> bind (\_ -> scanSpnl)
                    |> bind (\_ -> pLinkUrl)
                    |> bind
                        (\url ->
                            option "" (scanSpnl |> bind (\_ -> pLinkTitle))
                                |> bind
                                    (\tit ->
                                        endOfInput
                                            |> fmap (\_ -> ( lab, url, tit ))
                                    )
                        )
            )



-- Parses an escaped character and returns a Text.


pEscaped : Parser String
pEscaped =
    fmap String.fromChar (skip ((==) '\\') |> (\_ -> satisfy isEscapable))



-- Parses a (possibly escaped) character satisfying the predicate.


pSatisfy : (Char -> Bool) -> Parser Char
pSatisfy p =
    -- satisfy (\c -> c /= '\\' && p c)
    --  <|> (char '\\' *> satisfy (\c -> isEscapable c && p c))
    Debug.todo "pSatisfy"



-- Parse a text into inlines, resolving reference links
-- using the reference map.


parseInlines : ReferenceMap -> String -> Inlines
parseInlines refmap t =
    let
        _ =
            Debug.log "parseInlines" t
    in
    case Debug.log "parseInlines1" (parse (fmap List.concat (bind (\_ -> many (pInline refmap)) endOfInput)) t) of
        Err e ->
            -- should not happen
            crash ("parseInlines: " ++ Debug.toString e)

        Ok r ->
            r


pInline : ReferenceMap -> Parser Inlines
pInline refmap =
    oneOf pAsciiStr
        (oneOf pSpace
            -- strong/emph
            (oneOf (pEnclosure '*' refmap)
                (oneOf (notAfter Char.isAlphaNum |> bind (\_ -> pEnclosure '_' refmap))
                    (oneOf pCode
                        (oneOf (pLink refmap)
                            (oneOf (pImage refmap)
                                (oneOf pRawHtml
                                    (oneOf pAutolink
                                        (oneOf pEntity pSym)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )



-- Parse spaces or newlines, and determine whether
-- we have a regular space, a line break (two spaces before
-- a newline), or a soft break (newline without two spaces
-- before).


pSpace : Parser Inlines
pSpace =
    takeWhile1 isWhitespace
        |> bind
            (\ss ->
                return
                    (List.singleton
                        (if String.any ((==) '\n') ss then
                            if String.startsWith "  " ss then
                                LineBreak

                            else
                                SoftBreak

                         else
                            Space
                        )
                    )
            )


isAsciiAlphaNum : Char -> Bool
isAsciiAlphaNum c =
    (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')


pAsciiStr : Parser Inlines
pAsciiStr =
    takeWhile1 isAsciiAlphaNum
        |> bind
            (\t ->
                peekChar
                    |> bind
                        (\mbc ->
                            case mbc of
                                Just ':' ->
                                    if Set.member t schemeSet then
                                        pUri t

                                    else
                                        return (List.singleton (Str t))

                                _ ->
                                    return (List.singleton (Str t))
                        )
            )



-- Catch all -- parse an escaped character, an escaped
-- newline, or any remaining symbol character.


pSym : Parser Inlines
pSym =
    anyChar
        |> bind
            (\c ->
                let
                    ch =
                        List.singleton << Str << String.fromChar
                in
                if c == '\\' then
                    oneOf (fmap ch (satisfy isEscapable))
                        (oneOf (fmap (\_ -> List.singleton LineBreak) (satisfy ((==) '\n')))
                            (return (ch '\\'))
                        )

                else
                    return (ch c)
            )



-- http://www.iana.org/assignments/uri-schemes.html plus
-- the unofficial schemes coap, doi, javascript.


schemes : List String
schemes =
    [ -- unofficial
      "coap"
    , "doi"
    , "javascript"

    -- official
    , "aaa"
    , "aaas"
    , "about"
    , "acap"
    , "cap"
    , "cid"
    , "crid"
    , "data"
    , "dav"
    , "dict"
    , "dns"
    , "file"
    , "ftp"
    , "geo"
    , "go"
    , "gopher"
    , "h323"
    , "http"
    , "https"
    , "iax"
    , "icap"
    , "im"
    , "imap"
    , "info"
    , "ipp"
    , "iris"
    , "iris.beep"
    , "iris.xpc"
    , "iris.xpcs"
    , "iris.lwz"
    , "ldap"
    , "mailto"
    , "mid"
    , "msrp"
    , "msrps"
    , "mtqp"
    , "mupdate"
    , "news"
    , "nfs"
    , "ni"
    , "nih"
    , "nntp"
    , "opaquelocktoken"
    , "pop"
    , "pres"
    , "rtsp"
    , "service"
    , "session"
    , "shttp"
    , "sieve"
    , "sip"
    , "sips"
    , "sms"
    , "snmp"
    , "soap.beep"
    , "soap.beeps"
    , "tag"
    , "tel"
    , "telnet"
    , "tftp"
    , "thismessage"
    , "tn3270"
    , "tip"
    , "tv"
    , "urn"
    , "vemmi"
    , "ws"
    , "wss"
    , "xcon"
    , "xcon-userid"
    , "xmlrpc.beep"
    , "xmlrpc.beeps"
    , "xmpp"
    , "z39.50r"
    , "z39.50s"

    -- provisional
    , "adiumxtra"
    , "afp"
    , "afs"
    , "aim"
    , "apt"
    , "attachment"
    , "aw"
    , "beshare"
    , "bitcoin"
    , "bolo"
    , "callto"
    , "chrome"
    , "chrome-extension"
    , "com-eventbrite-attendee"
    , "content"
    , "cvs"
    , "dlna-playsingle"
    , "dlna-playcontainer"
    , "dtn"
    , "dvb"
    , "ed2k"
    , "facetime"
    , "feed"
    , "finger"
    , "fish"
    , "gg"
    , "git"
    , "gizmoproject"
    , "gtalk"
    , "hcp"
    , "icon"
    , "ipn"
    , "irc"
    , "irc6"
    , "ircs"
    , "itms"
    , "jar"
    , "jms"
    , "keyparc"
    , "lastfm"
    , "ldaps"
    , "magnet"
    , "maps"
    , "market"
    , "message"
    , "mms"
    , "ms-help"
    , "msnim"
    , "mumble"
    , "mvn"
    , "notes"
    , "oid"
    , "palm"
    , "paparazzi"
    , "platform"
    , "proxy"
    , "psyc"
    , "query"
    , "res"
    , "resource"
    , "rmi"
    , "rsync"
    , "rtmp"
    , "secondlife"
    , "sftp"
    , "sgn"
    , "skype"
    , "smb"
    , "soldat"
    , "spotify"
    , "ssh"
    , "steam"
    , "svn"
    , "teamspeak"
    , "things"
    , "udp"
    , "unreal"
    , "ut2004"
    , "ventrilo"
    , "view-source"
    , "webcal"
    , "wtai"
    , "wyciwyg"
    , "xfire"
    , "xri"
    , "ymsgr"
    ]



-- Make them a set for more efficient lookup.


schemeSet : Set String
schemeSet =
    Set.fromList (schemes ++ List.map String.toUpper schemes)



-- Parse a URI, using heuristics to avoid capturing final punctuation.


pUri : String -> Parser Inlines
pUri scheme =
    -- do
    --   _ <- char ':'
    --   x <- scan (OpenParens 0) uriScanner
    --   guard $ not $ T.null x
    --   let (rawuri, endingpunct) =
    --         case T.last x of
    --              c | c `elem` (".;?!:," :: String) ->
    --                (scheme <> ":" <> T.init x, singleton (Str (T.singleton c)))
    --              _ -> (scheme <> ":" <> x, mempty)
    --   return $ autoLink rawuri <> endingpunct
    Debug.todo "pUri"



-- Scan non-ascii characters and ascii characters allowed in a URI.
-- We allow punctuation except when followed by a space, since
-- we don't want the trailing '.' in 'http://google.com.'
-- We want to allow
-- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
-- as a URL, while NOT picking up the closing paren in
-- (http://wikipedia.org)
-- So we include balanced parens in the URL.


type OpenParens
    = OpenParens Int



-- uriScanner : OpenParens -> Char -> Maybe OpenParens
-- uriScanner _ ' '  = Nothing
-- uriScanner _ '\n' = Nothing
-- uriScanner (OpenParens n) '(' = Just (OpenParens (n + 1))
-- uriScanner (OpenParens n) ')'
--   | n > 0 = Just (OpenParens (n - 1))
--   | otherwise = Nothing
-- uriScanner st '+' = Just st
-- uriScanner st '/' = Just st
-- uriScanner _ c | isSpace c = Nothing
-- uriScanner st _ = Just st
-- Parses material enclosed in *s, **s, _s, or __s.
-- Designed to avoid backtracking.


pEnclosure : Char -> ReferenceMap -> Parser Inlines
pEnclosure c refmap =
    -- do
    --   cs <- takeWhile1 (== c)
    --   (Str cs <|) <$> pSpace
    --    <|> case T.length cs of
    --             3  -> pThree c refmap
    --             2  -> pTwo c refmap mempty
    --             1  -> pOne c refmap mempty
    --             _  -> return (singleton $ Str cs)
    Debug.todo "pEnclosure"



-- singleton sequence or empty if contents are empty


single : (Inlines -> Inline) -> Inlines -> Inlines
single constructor ils =
    if List.isEmpty ils then
        []

    else
        List.singleton (constructor ils)



-- parse inlines til you hit a c, and emit Emph.
-- if you never hit a c, emit '*' + inlines parsed.


pOne : Char -> ReferenceMap -> Inlines -> Parser Inlines
pOne c refmap prefix =
    -- do
    --   contents <- msum <$> many ( (nfbChar c >> pInline refmap)
    --                              <|> (string (T.pack [c,c]) >>
    --                                   nfbChar c >> pTwo c refmap mempty) )
    --   (char c >> return (single Emph $ prefix <> contents))
    --     <|> return (singleton (Str (T.singleton c)) <> (prefix <> contents))
    Debug.todo "pOne"



-- parse inlines til you hit two c's, and emit Strong.
-- if you never do hit two c's, emit '**' plus + inlines parsed.


pTwo : Char -> ReferenceMap -> Inlines -> Parser Inlines
pTwo c refmap prefix =
    -- do
    --   let ender = string $ T.pack [c,c]
    --   contents <- msum <$> many (nfb ender >> pInline refmap)
    --   (ender >> return (single Strong $ prefix <> contents))
    --     <|> return (singleton (Str $ T.pack [c,c]) <> (prefix <> contents))
    Debug.todo "pTwo"



-- parse inlines til you hit one c or a sequence of two c's.
-- If one c, emit Emph and then parse pTwo.
-- if two c's, emit Strong and then parse pOne.


pThree : Char -> ReferenceMap -> Parser Inlines
pThree c refmap =
    -- do
    --   contents <- msum <$> (many (nfbChar c >> pInline refmap))
    --   (string (T.pack [c,c]) >> (pOne c refmap (single Strong contents)))
    --    <|> (char c >> (pTwo c refmap (single Emph contents)))
    --    <|> return (singleton (Str $ T.pack [c,c,c]) <> contents)
    Debug.todo "pThree"



-- Inline code span.


pCode : Parser Inlines
pCode =
    fmap Tuple.first pCode_



-- this is factored out because it needed in pLinkLabel.


pCode_ : Parser ( Inlines, String )
pCode_ =
    takeWhile1 ((==) '`')
        |> bind
            (\ticks ->
                let
                    end =
                        string ticks |> bind (\_ -> nfb (char '`'))

                    nonBacktickSpan =
                        takeWhile1 ((/=) '`')

                    backtickSpan =
                        takeWhile1 ((==) '`')
                in
                manyTill (oneOf nonBacktickSpan backtickSpan) end
                    |> fmap String.concat
                    |> fmap
                        (\contents ->
                            ( List.singleton (Code (String.trim contents)), ticks ++ contents ++ ticks )
                        )
            )


pLink : ReferenceMap -> Parser Inlines
pLink refmap =
    -- do
    --   lab <- pLinkLabel
    --   let lab' = parseInlines refmap lab
    --   pInlineLink lab' <|> pReferenceLink refmap lab lab'
    --     -- fallback without backtracking if it's not a link:
    --     <|> return (singleton (Str "[") <> lab' <> singleton (Str "]"))
    Debug.todo "pLink"



-- An inline link: [label](/url "optional title")


pInlineLink : Inlines -> Parser Inlines
pInlineLink lab =
    -- do
    --   _ <- char '('
    --   scanSpaces
    --   url <- pLinkUrl
    --   tit <- option "" $ scanSpnl *> pLinkTitle <* scanSpaces
    --   _ <- char ')'
    --   return $ singleton $ Link lab (Url url) tit
    Debug.todo "pInlineLink"



-- A reference link: [label], [foo][label], or [label][].


pReferenceLink : ReferenceMap -> String -> Inlines -> Parser Inlines
pReferenceLink _ rawlab lab =
    -- do
    --   ref <- option rawlab $ scanSpnl >> pLinkLabel
    --   return $ singleton $ Link lab (Ref ref) ""
    Debug.todo "pReferenceLink"



-- An image:  ! followed by a link.


pImage : ReferenceMap -> Parser Inlines
pImage refmap =
    -- do
    --   _ <- char '!'
    --   (linkToImage <$> pLink refmap) <|> return (singleton (Str "!"))
    Debug.todo "pImage"


linkToImage : Inlines -> Inlines
linkToImage ils =
    -- case viewl ils of
    --       (Link lab (Url url) tit :< x)
    --         | Seq.null x -> singleton (Image lab url tit)
    --       _ -> singleton (Str "!") <> ils
    Debug.todo "linkToImage"



-- An entity.  We store these in a special inline element.
-- This ensures that entities in the input come out as
-- entities in the output. Alternatively we could simply
-- convert them to characters and store them as Str inlines.


pEntity : Parser Inlines
pEntity =
    char '&'
        |> bind (\_ -> oneOf pCharEntity (oneOf pDecEntity pHexEntity))
        |> bind
            (\res ->
                char ';'
                    |> bind (\_ -> return (List.singleton (Entity ("&" ++ res ++ ";"))))
            )


pCharEntity : Parser String
pCharEntity =
    takeWhile1 (\c -> Char.isAlpha c)


pDecEntity : Parser String
pDecEntity =
    char '#'
        |> bind (\_ -> takeWhile1 Char.isDigit)
        |> bind (\res -> return ("#" ++ res))


pHexEntity : Parser String
pHexEntity =
    char '#'
        |> bind (\_ -> oneOf (char 'X') (char 'x'))
        |> bind
            (\x ->
                takeWhile1 Char.isHexDigit
                    |> bind
                        (\res ->
                            return ("#" ++ String.fromChar x ++ res)
                        )
            )



-- Raw HTML tag or comment.


pRawHtml : Parser Inlines
pRawHtml =
    fmap (List.singleton << RawHtml) (oneOf (fmap Tuple.second pHtmlTag) pHtmlComment)



-- A link like this: <http://whatever.com> or <me@mydomain.edu>.
-- Markdown.pl does email obfuscation; we don't bother with that here.


pAutolink : Parser Inlines
pAutolink =
    skip ((==) '<')
        |> bind (\_ -> takeWhile1 (\c -> c /= ':' && c /= '@'))
        |> bind
            (\s ->
                takeWhile1 (\c -> c /= '>' && c /= ' ')
                    |> bind
                        (\rest ->
                            skip ((==) '>')
                                |> bind
                                    (\_ ->
                                        if String.startsWith "@" rest then
                                            return (emailLink (s ++ rest))

                                        else if Set.member s schemeSet then
                                            return (autoLink (s ++ rest))

                                        else
                                            fail "Unknown contents of <>"
                                    )
                        )
            )



--


autoLink : String -> Inlines
autoLink t =
    -- singleton $ Link (toInlines t) (Url t) (T.empty)
    -- where toInlines t' = case parse pToInlines t' of
    --                        Right r   -> r
    --                        Left e    -> error $ "autolink: " ++ show e
    --       pToInlines = mconcat <$> many strOrEntity
    --       strOrEntity = ((singleton . Str) <$> takeWhile1 (/='&'))
    --                  <|> pEntity
    --                  <|> ((singleton . Str) <$> string "&")
    Debug.todo "autoLink"


emailLink : String -> Inlines
emailLink t =
    -- singleton $ Link (singleton $ Str t)
    --                            (Url $ "mailto:" <> t) (T.empty)
    Debug.todo "emailLink"
