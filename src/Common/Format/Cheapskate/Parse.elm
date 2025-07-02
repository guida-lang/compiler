module Common.Format.Cheapskate.Parse exposing (markdown)

-- import Cheapskate.ParserCombinators
-- import Cheapskate.Util
-- import Cheapskate.Inlines
-- import Cheapskate.Types
-- import Data.Char hiding (Space)
-- import qualified Data.Set as Set
-- import Prelude hiding (takeWhile)
-- import Data.Maybe (mapMaybe)
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import Data.Foldable (toList)
-- import Data.Sequence ((|>), viewr, ViewR(..), singleton, Seq)
-- import qualified Data.Sequence as Seq
-- import Control.Monad.RWS
-- import Control.Applicative
-- import qualified Data.Map as M
-- import Data.List (intercalate)
-- | Parses the input as a markdown document.  Note that 'Doc' is an instance
-- of 'ToMarkup', so the document can be converted to 'Html' using 'toHtml'.
-- A simple 'Text' to 'Html' filter would be
--
-- > markdownToHtml :: Text -> Html
-- > markdownToHtml = toHtml . markdown def

import Common.Format.Render.Markdown as Markdown


markdown : Options -> String -> Doc
markdown opts =
    Doc opts << processDocument << processLines



-- General parsing strategy:
--
-- Step 1:  processLines
--
-- We process the input line by line.  Each line modifies the
-- container stack, by adding a leaf to the current open container,
-- sometimes after closing old containers and/or opening new ones.
--
-- To open a container is to add it to the top of the container stack,
-- so that new content will be added under this container.
-- To close a container is to remove it from the container stack and
-- make it a child of the container above it on the container stack.
--
-- When all the input has been processed, we close all open containers
-- except the root (Document) container.  At this point we should also
-- have a ReferenceMap containing any defined link references.
--
-- Step 2:  processDocument
--
-- We then convert this container structure into an AST.  This principally
-- involves (a) gathering consecutive ListItem containers into lists, (b)
-- gathering TextLine nodes that don't belong to verbatim containers into
-- paragraphs, and (c) parsing the inline contents of non-verbatim TextLines.
--------
-- Container stack definitions:


type ContainerStack
    = ContainerStack Container {- top -} (List Container)



{- rest -}


type LineNumber
    = Int



-- Generic type for a container or a leaf.


type Elt
    = C Container
    | L LineNumber Leaf


type Container
    = Container
        { containerType : ContainerType
        , children : List Elt
        }


type ContainerType
    = Document
    | BlockQuote
    | ListItem
        { markerColumn : Int
        , padding : Int
        , listType : Markdown.ListType
        }
    | FencedCode
        { startColumn : Int
        , fence : String
        , info : String
        }
    | IndentedCode
    | RawHtmlBlock
    | Reference



-- instance Show Container where
--   show c = show (containerType c) ++ "\n" ++
--     nest 2 (intercalate "\n" (map showElt $ toList $ children c))


nest : Int -> String -> String
nest num =
    intercalate "\n" << List.map (String.repeat num ' ' (++)) << String.lines


showElt : Elt -> String
showElt elt =
    case elt of
        C c ->
            show c

        L _ (TextLine s) ->
            show s

        L _ lf ->
            show lf



-- Scanners that must be satisfied if the current open container
-- is to be continued on a new line (ignoring lazy continuations).


containerContinue : Container -> Scanner
containerContinue c =
    -- case containerType c of
    --      BlockQuote     -> scanNonindentSpace *> scanBlockquoteStart
    --      IndentedCode   -> scanIndentSpace
    --      FencedCode {startColumn } ->
    --                        scanSpacesToColumn startColumn
    --      RawHtmlBlock   -> nfb scanBlankline
    --      (ListItem{}) as li  -> scanBlankline
    --                        <|>
    --                        (do scanSpacesToColumn
    --                               (markerColumn li + 1)
    --                            _ <- upToCountChars (padding li - 1)
    --                               (==' ')
    --                            return ())
    --      Reference{}    -> nfb scanBlankline >>
    --                        nfb (scanNonindentSpace *> scanReference)
    --      _              -> return ()
    Debug.todo "containerContinue"



-- Defines parsers that open new containers.


containerStart : Bool -> Parser ContainerType
containerStart _ =
    --     scanNonindentSpace *>
    --    (  (BlockQuote <$ scanBlockquoteStart)
    --   <|> parseListMarker
    --    )
    Debug.todo "containerStart"



-- Defines parsers that open new verbatim containers (containers
-- that take only TextLine and BlankLine as children).


verbatimContainerStart : Bool -> Parser ContainerType
verbatimContainerStart lastLineIsText =
    --     scanNonindentSpace *>
    --    (  parseCodeFence
    --   <|> (guard (not lastLineIsText) *> (IndentedCode <$ char ' ' <* nfb scanBlankline))
    --   <|> (guard (not lastLineIsText) *> (RawHtmlBlock <$ parseHtmlBlockStart))
    --   <|> (guard (not lastLineIsText) *> (Reference <$ scanReference))
    --    )
    Debug.todo "verbatimContainerStart"



-- Leaves of the container structure (they don't take children).


type Leaf
    = TextLine String
    | BlankLine String
    | ATXHeader Int String
    | SetextHeader Int String
    | Rule


type ContainerM
    = RWS () Markdown.ReferenceMap ContainerStack



-- Close the whole container stack, leaving only the root Document container.


closeStack : ContainerM Container
closeStack =
    -- do
    --     ContainerStack top rest  <- get
    --     if null rest
    --         then return top
    --         else closeContainer >> closeStack
    Debug.todo "closeStack"



-- Close the top container on the stack.  If the container is a Reference
-- container, attempt to parse the reference and update the reference map.
-- If it is a list item container, move a final BlankLine outside the list
-- item.


closeContainer : ContainerM ()
closeContainer =
    -- do
    --     ContainerStack top rest <- get
    --     case top of
    --         (Container Reference{} cs__) ->
    --             case parse pReference
    --                 (T.strip $ joinLines $ map extractText $ toList cs__) of
    --                 Right (lab, lnk, tit) -> do
    --                     tell (M.singleton (normalizeReference lab) (lnk, tit))
    --                     case rest of
    --                         (Container ct_ cs_ : rs) ->
    --                         put $ ContainerStack (Container ct_ (cs_ |> C top)) rs
    --                         [] -> return ()
    --                 Left _ -> -- pass over in silence if ref doesn't parse?
    --                             case rest of
    --                                 (c:cs) -> put $ ContainerStack c cs
    --                                 []     -> return ()
    --         (Container (ListItem{} as li) cs__) ->
    --             case rest of
    --                 -- move final BlankLine outside of list item
    --                 (Container ct_ cs_ : rs) ->
    --                         case viewr cs__ of
    --                                 (zs :> b@(L _ BlankLine{})) ->
    --                                 put $ ContainerStack
    --                                     (if Seq.null zs
    --                                         then Container ct_ (cs_ |> C (Container li zs))
    --                                         else Container ct_ (cs_ |>
    --                                                 C (Container li zs) |> b)) rs
    --                                 _ -> put $ ContainerStack (Container ct_ (cs_ |> C top)) rs
    --                 [] -> return ()
    --         _ -> case rest of
    --                 (Container ct_ cs_ :: rs) ->
    --                     put $ ContainerStack (Container ct_ (cs_ |> C top)) rs
    --                 [] -> return ()
    Debug.todo "closeContainer"



-- Add a leaf to the top container.


addLeaf : LineNumber -> Leaf -> ContainerM ()
addLeaf lineNum lf =
    -- do
    -- ContainerStack top rest <- get
    -- case (top, lf) of
    --         (Container (ListItem{} as ct) cs, BlankLine{}) ->
    --         case viewr cs of
    --             (_ :> L _ BlankLine{}) -> -- two blanks break out of list item:
    --                 closeContainer >> addLeaf lineNum lf
    --             _ -> put $ ContainerStack (Container ct (cs |> L lineNum lf)) rest
    --         (Container ct cs, _) ->
    --                 put $ ContainerStack (Container ct (cs |> L lineNum lf)) rest
    Debug.todo "addLeaf"



-- Add a container to the container stack.


addContainer : ContainerType -> ContainerM ()
addContainer ct =
    -- modify $ \(ContainerStack top rest) ->
    --     ContainerStack (Container ct mempty) (top:rest)
    Debug.todo "addContainer"



-- Step 2
-- Convert Document container and reference map into an AST.


processDocument : ( Container, ReferenceMap ) -> Blocks
processDocument ( Container ct cs, refmap ) =
    case ct of
        Document ->
            processElts refmap (toList cs)

        _ ->
            error "top level container is not Document"



-- Turn the result of `processLines` into a proper AST.
-- This requires grouping text lines into paragraphs
-- and list items into lists, handling blank lines,
-- parsing inline contents of texts and resolving referencess.


processElts : ReferenceMap -> List Elt -> Markdown.Blocks
processElts refmap elts =
    case elts of
        [] ->
            []

        (L _ lf) :: rest ->
            case lf of
                -- Special handling of @docs lines in Elm:
                TextLine t ->
                    case T.stripPrefix "@docs" t of
                        Just terms1 ->
                            let
                                docs =
                                    terms1 :: map (cleanDoc << extractText) docLines

                                ( docLines, rest_ ) =
                                    span isDocLine rest

                                isDocLine (L _ (TextLine _)) =
                                    True

                                isDocLine _ =
                                    False

                                cleanDoc lin =
                                    case T.stripPrefix "@docs" lin of
                                        Nothing ->
                                            lin

                                        Just stripped ->
                                            stripped
                            in
                            singleton (ElmDocs <| filter ((/=) []) <| fmap (filter ((/=) "") << fmap T.strip << T.splitOn ",") docs)
                                <> processElts refmap rest_

                        Nothing ->
                            -- Gobble text lines and make them into a Para:
                            let
                                txt =
                                    T.stripEnd <|
                                        joinLines <|
                                            map T.stripStart <|
                                                t
                                                    :: map extractText textlines

                                ( textlines, rest_ ) =
                                    span isTextLine rest

                                isTextLine elt =
                                    case elt of
                                        L _ (TextLine s) ->
                                            not (T.isPrefixOf "@docs" s)

                                        _ ->
                                            False
                            in
                            singleton (Para <| parseInlines refmap txt)
                                <> processElts refmap rest_

                -- Blanks at outer level are ignored:
                BlankLine {} ->
                    processElts refmap rest

                -- Headers:
                ATXHeader lvl t ->
                    singleton (Header lvl <| parseInlines refmap t)
                        <> processElts refmap rest

                SetextHeader lvl t ->
                    singleton (Header lvl <| parseInlines refmap t)
                        <> processElts refmap rest

                -- Horizontal rule:
                Rule ->
                    singleton HRule <> processElts refmap rest

        (C (Container ct cs)) :: rest ->
            let
                isBlankLine (L _ BlankLine {}) =
                    True

                isBlankLine _ =
                    False

                tightListItem [] =
                    True

                tightListItem xs =
                    not <| any isBlankLine xs
            in
            case ct of
                Document ->
                    error "Document container found inside Document"

                BlockQuote ->
                    singleton (Blockquote <| processElts refmap (toList cs))
                        <> processElts refmap rest

                -- List item?  Gobble up following list items of the same type
                -- (skipping blank lines), determine whether the list is tight or
                -- loose, and generate a List.
                ListItem { listType } ->
                    let
                        xs =
                            takeListItems rest

                        rest_ =
                            drop (length xs) rest

                        -- take list items as long as list type matches and we
                        -- don't hit two blank lines:
                        takeListItems ys =
                            case ys of
                                (C ((Container ListItem { listType } _) as c)) :: zs ->
                                    if listTypesMatch listType listType_ then
                                        C c :: takeListItems zs

                                    else
                                        []

                                ((L _ (BlankLine _)) as lf) :: ((C (Container ListItem { listType } _)) as c) :: zs ->
                                    if listTypesMatch listType listType_ then
                                        lf :: c :: takeListItems zs

                                    else
                                        []

                                _ ->
                                    []

                        listTypesMatch (Bullet c1) (Bullet c2) =
                            c1 == c2

                        listTypesMatch (Numbered w1 _) (Numbered w2 _) =
                            w1 == w2

                        listTypesMatch _ _ =
                            False

                        -- items = mapMaybe getItem (Container ct cs :: [c | C c <- xs])
                        getItem (Container ListItem {} cs_) =
                            Just <| toList cs_

                        getItem _ =
                            Nothing

                        items_ =
                            map (processElts refmap) items

                        isTight =
                            tightListItem xs && all tightListItem items
                    in
                    singleton (List isTight listType items_) <> processElts refmap rest_

                FencedCode _ _ info_ ->
                    let
                        txt =
                            joinLines <| map extractText <| toList cs

                        attr =
                            CodeAttr x (T.strip y)

                        ( x, y ) =
                            T.break ((==) ' ') info_
                    in
                    singleton (CodeBlock attr txt)
                        <> processElts refmap rest

                IndentedCode ->
                    let
                        txt =
                            joinLines <|
                                stripTrailingEmpties <|
                                    concatMap extractCode cbs

                        stripTrailingEmpties =
                            reverse
                                << dropWhile (T.all ((==) ' '))
                                << reverse

                        -- explanation for next line:  when we parsed
                        -- the blank line, we dropped 0-3 spaces.
                        -- but for this, code block context, we want
                        -- to have dropped 4 spaces. we simply drop
                        -- one more:
                        extractCode (L _ (BlankLine t)) =
                            [ T.drop 1 t ]

                        extractCode (C (Container IndentedCode cs_)) =
                            map extractText <| toList cs_

                        extractCode _ =
                            []

                        ( cbs, rest_ ) =
                            span isIndentedCodeOrBlank
                                (C (Container ct cs) :: rest)

                        isIndentedCodeOrBlank (L _ BlankLine {}) =
                            True

                        isIndentedCodeOrBlank (C (Container IndentedCode _)) =
                            True

                        isIndentedCodeOrBlank _ =
                            False
                    in
                    singleton (CodeBlock (CodeAttr "" "") txt)
                        <> processElts refmap rest_

                RawHtmlBlock ->
                    let
                        txt =
                            joinLines (map extractText (toList cs))
                    in
                    singleton (HtmlBlock txt) <> processElts refmap rest

                -- References have already been taken into account in the reference map,
                -- so we just skip.
                Reference {} ->
                    let
                        refs cs_ =
                            fmap (extractRef << extractText) (toList cs_)

                        extractRef t =
                            case parse pReference (T.strip t) of
                                Right ( lab, lnk, tit ) ->
                                    ( lab, lnk, tit )

                                Left _ ->
                                    ( "??", "??", "??" )

                        processElts_ : List (List ( Text, Text, Text )) -> List Elt -> Blocks
                        processElts_ acc ((C (Container Reference cs)) :: rest_) =
                            processElts_ (refs cs :: acc) rest_

                        processElts_ acc pass =
                            (singleton <| ReferencesBlock <| concat <| reverse acc)
                                <> processElts refmap pass
                    in
                    processElts_ [] (C (Container ct cs) :: rest)


extractText : Elt -> String
extractText elt =
    case elt of
        L _ (TextLine t) ->
            t

        _ ->
            ""



-- Step 1


processLines : Text -> ( Container, ReferenceMap )
processLines t =
    let
        ( doc, refmap ) =
            evalRWS (mapM_ processLine lns >> closeStack) () startState

        -- lns        = zip [1..] (map tabFilter $ T.lines t)
        startState =
            ContainerStack (Container Document mempty) []
    in
    ( doc, refmap )



-- The main block-parsing function.
-- We analyze a line of text and modify the container stack accordingly,
-- adding a new leaf, or closing or opening containers.


processLine : ( LineNumber, Text ) -> ContainerM ()
processLine ( lineNumber, txt ) =
    --   do
    --     ContainerStack top@(Container ct cs) rest <- get
    --     -- Apply the line-start scanners appropriate for each nested container.
    --     -- Return the remainder of the string, and the number of unmatched
    --     -- containers.
    --     let (t', numUnmatched) = tryOpenContainers (reverse $ top:rest) txt
    --     -- Some new containers can be started only after a blank.
    --     let lastLineIsText = numUnmatched == 0 &&
    --                         case viewr cs of
    --                                 (_ :> L _ (TextLine _)) -> True
    --                                 _                       -> False
    --     -- Process the rest of the line in a way that makes sense given
    --     -- the container type at the top of the stack (ct):
    --     case ct of
    --         -- If it's a verbatim line container, add the line.
    --         RawHtmlBlock{} | numUnmatched == 0 -> addLeaf lineNumber (TextLine t')
    --         IndentedCode   | numUnmatched == 0 -> addLeaf lineNumber (TextLine t')
    --         FencedCode{ fence = fence' } ->
    --         -- here we don't check numUnmatched because we allow laziness
    --         if fence' `T.isPrefixOf` t'
    --             -- closing code fence
    --             then closeContainer
    --             else addLeaf lineNumber (TextLine t')
    --         Reference ->
    --         case tryNewContainers lastLineIsText (T.length txt - T.length t') t' of
    --             (ns, lf) -> do
    --             closeContainer
    --             addNew (ns, lf)
    --         -- otherwise, parse the remainder to see if we have new container starts:
    --         _ -> case tryNewContainers lastLineIsText (T.length txt - T.length t') t' of
    --         -- lazy continuation: text line, last line was text, no new containers,
    --         -- some unmatched containers:
    --         ([], TextLine t)
    --             | numUnmatched > 0
    --             , case viewr cs of
    --                     (_ :> L _ (TextLine _)) -> True
    --                     _                       -> False
    --             , ct /= IndentedCode -> addLeaf lineNumber (TextLine t)
    --         -- if it's a setext header line and the top container has a textline
    --         -- as last child, add a setext header:
    --         ([], SetextHeader lev _) | numUnmatched == 0 ->
    --             case viewr cs of
    --                 (cs' :> L _ (TextLine t)) -> -- replace last text line with setext header
    --                 put $ ContainerStack (Container ct
    --                             (cs' |> L lineNumber (SetextHeader lev t))) rest
    --                 -- Note: the following case should not occur, since
    --                 -- we don't add a SetextHeader leaf unless lastLineIsText.
    --                 _ -> error "setext header line without preceding text line"
    --         -- otherwise, close all the unmatched containers, add the new
    --         -- containers, and finally add the new leaf:
    --         (ns, lf) -> do -- close unmatched containers, add new ones
    --             _ <- replicateM numUnmatched closeContainer
    --             addNew (ns, lf)
    --     where
    --         addNew (ns, lf) = do
    --         mapM_ addContainer ns
    --         case (reverse ns, lf) of
    --             -- don't add extra blank at beginning of fenced code block
    --             (FencedCode{}:_,  BlankLine{}) -> return ()
    --             _ -> addLeaf lineNumber lf
    Debug.todo "processLine"



-- Try to match the scanners corresponding to any currently open containers.
-- Return remaining text after matching scanners, plus the number of open
-- containers whose scanners did not match.  (These will be closed unless
-- we have a lazy text line.)


tryOpenContainers : List Container -> Text -> ( Text, Int )
tryOpenContainers cs t =
    --   let scanners [] = Tuple.pair <$> takeText <*> pure 0
    --         scanners (p::ps) = (p *> scanners ps)
    --                       <|> (Tuple.pair <$> takeText <*> pure (length (p::ps)))
    --   in
    case parse (scanners <| map containerContinue cs) t of
        Right ( t_, n ) ->
            ( t_, n )

        Left e ->
            error <|
                "error parsing scanners: "
                    ++ show e



-- Try to match parsers for new containers.  Return list of new
-- container types, and the leaf to add inside the new containers.


tryNewContainers : Bool -> Int -> Text -> ( List ContainerType, Leaf )
tryNewContainers lastLineIsText offset t =
    --   let
    --     newContainers = do
    --           getPosition >>= \pos -> setPosition pos{ column = offset + 1 }
    --           regContainers <- many (containerStart lastLineIsText)
    --           verbatimContainers <- option []
    --                             $ count 1 (verbatimContainerStart lastLineIsText)
    --           if null verbatimContainers
    --              then (,) <$> pure regContainers <*> leaf lastLineIsText
    --              else (,) <$> pure (regContainers ++ verbatimContainers) <*>
    --                             textLineOrBlank
    --   in
    case parse newContainers t of
        Right ( cs, t_ ) ->
            ( cs, t_ )

        Left err ->
            error (show err)


textLineOrBlank : Parser Leaf
textLineOrBlank =
    --   let
    --     consolidate ts | T.all isWhitespace ts = BlankLine ts
    --                        | otherwise        = TextLine  ts
    --   in
    -- consolidate <$> takeText
    Debug.todo "textLineOrBlank"



-- Parse a leaf node.


leaf : Bool -> Parser Leaf
leaf lastLineIsText =
    --     scanNonindentSpace *> (
    --      (ATXHeader <$> parseAtxHeaderStart <*>
    --          (T.strip . removeATXSuffix <$> takeText))
    --    <|> (guard lastLineIsText *> (SetextHeader <$> parseSetextHeaderLine <*> pure mempty))
    --    <|> (Rule <$ scanHRuleLine)
    --    <|> textLineOrBlank
    --   )
    --   where removeATXSuffix t = case T.dropWhileEnd (`elem` (" #" :: String)) t of
    --                                  t' | T.null t' -> t'
    --                                       -- an escaped \#
    --                                     | T.last t' == '\\' -> t' <> "#"
    --                                     | otherwise -> t'
    Debug.todo "leaf"



-- Scanners


scanReference : Scanner
scanReference =
    -- () <$ lookAhead (pLinkLabel >> scanChar ':')
    Debug.todo "scanReference"



-- Scan the beginning of a blockquote:  up to three
-- spaces indent, the `>` character, and an optional space.


scanBlockquoteStart : Scanner
scanBlockquoteStart =
    -- scanChar '>' >> option () (scanChar ' ')
    Debug.todo "scanBlockquoteStart"



-- Parse the sequence of `#` characters that begins an ATX
-- header, and return the number of characters.  We require
-- a space after the initial string of `#`s, as not all markdown
-- implementations do. This is because (a) the ATX reference
-- implementation requires a space, and (b) since we're allowing
-- headers without preceding blank lines, requiring the space
-- avoids accidentally capturing a line like `#8 toggle bolt` as
-- a header.


parseAtxHeaderStart : Parser Int
parseAtxHeaderStart =
    --   do
    --   _ <- char '#'
    --   hashes <- upToCountChars 5 (== '#')
    --   -- hashes must be followed by space unless empty header:
    --   notFollowedBy (skip (/= ' '))
    --   return $ T.length hashes + 1
    Debug.todo "parseAtxHeaderStart"


parseSetextHeaderLine : Parser Int
parseSetextHeaderLine =
    -- do
    --     d <- satisfy (\c -> c == '-' || c == '=')
    --     let lev = if d == '=' then 1 else 2
    --     skipWhile (== d)
    --     scanBlankline
    --     return lev
    Debug.todo "parseSetextHeaderLine"



-- Scan a horizontal rule line: "...three or more hyphens, asterisks,
-- or underscores on a line by themselves. If you wish, you may use
-- spaces between the hyphens or asterisks."


scanHRuleLine : Scanner
scanHRuleLine =
    -- do
    --     c <- satisfy (\c -> c == '*' || c == '_' || c == '-')
    --     _ <- count 2 $ scanSpaces >> skip (== c)
    --     skipWhile (\x -> x == ' ' || x == c)
    --     endOfInput
    Debug.todo "scanHRuleLine"



-- Parse an initial code fence line, returning
-- the fence part and the rest (after any spaces).


parseCodeFence : Parser ContainerType
parseCodeFence =
    -- do
    --     col <- column <$> getPosition
    --     cs <- takeWhile1 (=='`') <|> takeWhile1 (=='~')
    --     guard $ T.length cs >= 3
    --     scanSpaces
    --     rawattr <- takeWhile (\c -> c /= '`' && c /= '~')
    --     endOfInput
    --     return $ FencedCode { startColumn = col
    --                         , fence = cs
    --                         , info = rawattr }
    Debug.todo "parseCodeFence"



-- Parse the start of an HTML block:  either an HTML tag or an
-- HTML comment, with no indentation.


parseHtmlBlockStart : Parser ()
parseHtmlBlockStart =
    -- () <$ lookAhead
    --     ((do t <- pHtmlTag
    --         guard $ f $ fst t
    --         return $ snd t)
    --     <|> string "<!--"
    --     <|> string "-->"
    --     )
    -- where f (Opening name) = name `Set.member` blockHtmlTags
    --     f (SelfClosing name) = name `Set.member` blockHtmlTags
    --     f (Closing name) = name `Set.member` blockHtmlTags
    Debug.todo "parseHtmlBlockStart"



-- List of block level tags for HTML 5.


blockHtmlTags : Set.Set Text
blockHtmlTags =
    Set.fromList
        [ "article"
        , "header"
        , "aside"
        , "hgroup"
        , "blockquote"
        , "hr"
        , "body"
        , "li"
        , "br"
        , "map"
        , "button"
        , "object"
        , "canvas"
        , "ol"
        , "caption"
        , "output"
        , "col"
        , "p"
        , "colgroup"
        , "pre"
        , "dd"
        , "progress"
        , "div"
        , "section"
        , "dl"
        , "table"
        , "dt"
        , "tbody"
        , "embed"
        , "textarea"
        , "fieldset"
        , "tfoot"
        , "figcaption"
        , "th"
        , "figure"
        , "thead"
        , "footer"
        , "footer"
        , "tr"
        , "form"
        , "ul"
        , "h1"
        , "h2"
        , "h3"
        , "h4"
        , "h5"
        , "h6"
        , "video"
        ]



-- Parse a list marker and return the list type.


parseListMarker : Parser ContainerType
parseListMarker =
    -- do
    --   col <- column <$> getPosition
    --   ty <- parseBullet <|> parseListNumber
    --   -- padding is 1 if list marker followed by a blank line
    --   -- or indented code.  otherwise it's the length of the
    --   -- whitespace between the list marker and the following text:
    --   padding_ <- (1 <$ scanBlankline)
    --           <|> (1 <$ (skip ((==) ' ') *> lookAhead (count 4 (char ' '))))
    --           <|> (T.length <$> takeWhile ((==) ' '))
    --   -- text can't immediately follow the list marker:
    --   guard $ padding_ > 0
    --   return $ ListItem { listType = ty
    --                     , markerColumn = col
    --                     , padding = padding_ + listMarkerWidth ty
    --                     }
    Debug.todo "parseListMarker"



-- listMarkerWidth : ListType -> Int
-- listMarkerWidth (Bullet _) = 1
-- listMarkerWidth (Numbered _ n) | n < 10    = 2
--                                | n < 100   = 3
--                                | n < 1000  = 4
--                                | otherwise = 5
-- Parse a bullet and return list type.


parseBullet : Parser Markdown.ListType
parseBullet =
    -- do
    --   c <- satisfy (\c -> c == '+' || c == '*' || c == '-')
    --   unless (c == '+')
    --     $ nfb $ (count 2 $ scanSpaces >> skip (== c)) >>
    --           skipWhile (\x -> x == ' ' || x == c) >> endOfInput -- hrule
    --   return $ Bullet c
    Debug.todo "parseBullet"



-- Parse a list number marker and return list type.


parseListNumber : Parser Markdown.ListType
parseListNumber =
    -- do
    --     num
    --     <- (read . T.unpack)
    --     <$> takeWhile1 isDigit
    --             wrap
    --     <- PeriodFollowing
    --     <$ skip ((==) '.')
    --     <|> ParenFollowing
    --     <$ skip ((==) ')')
    --         return
    --     $ Numbered wrap num
    Debug.todo "parseListNumber"
