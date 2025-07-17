module Common.Format.Cheapskate.Parse exposing (markdown)

import Common.Format.Cheapskate.Inlines exposing (..)
import Common.Format.Cheapskate.ParserCombinators exposing (..)
import Common.Format.Cheapskate.Types exposing (..)
import Common.Format.Cheapskate.Util exposing (..)
import Common.Format.RWS as RWS exposing (RWS)
import Dict exposing (Dict)
import List.Extra as List
import Set exposing (Set)
import Utils.Crash exposing (crash)



-- PARSE


markdown : Options -> String -> Doc
markdown opts =
    Doc opts << processDocument << processLines



{- General parsing strategy:

   Step 1: processLines

   We process the input line by line. Each line modifies the
   container stack, by adding a leaf to the current open container,
   sometimes after closing old containers and/or opening new ones.

   To open a container is to add it to the top of the container stack,
   so that new content will be added under this container.
   To close a container is to remove it from the container stack and
   make it a child of the container above it on the container stack.

   When all the input has been processed, we close all open containers
   except the root (Document) container. At this point we should also
   have a ReferenceMap containing any defined link references.

   Step 2: processDocument

   We then convert this container structure into an AST. This principally
   involves (a) gathering consecutive ListItem containers into lists, (b)
   gathering TextLine nodes that don't belong to verbatim containers into
   paragraphs, and (c) parsing the inline contents of non-verbatim TextLines.

-}


{-| Container stack definitions:
-}
type ContainerStack
    = ContainerStack {- top -} Container {- rest -} (List Container)


type alias LineNumber =
    Int


{-| Generic type for a container or a leaf.
-}
type Elt
    = C Container
    | L LineNumber Leaf


type Container
    = Container ContainerType (List Elt)


type ContainerType
    = Document
    | BlockQuote
    | ListItem
        { markerColumn : Int
        , padding : Int
        , listType : ListType
        }
    | FencedCode
        { startColumn : Int
        , fence : String
        , info : String
        }
    | IndentedCode
    | RawHtmlBlock
    | Reference


nest : Int -> String -> String
nest num =
    String.join "\n"
        << List.map ((++) (String.repeat num " "))
        << String.lines


showElt : Elt -> String
showElt elt =
    case elt of
        C c ->
            Debug.toString c

        L _ (TextLine s) ->
            s

        L _ lf ->
            Debug.toString lf


{-| Scanners that must be satisfied if the current open container
is to be continued on a new line (ignoring lazy continuations).
-}
containerContinue : Container -> Scanner
containerContinue (Container containerType _) =
    case containerType of
        BlockQuote ->
            scanNonindentSpace |> bind (\_ -> scanBlockquoteStart)

        IndentedCode ->
            scanIndentSpace

        FencedCode { startColumn } ->
            scanSpacesToColumn startColumn

        RawHtmlBlock ->
            nfb scanBlankline

        ListItem { markerColumn, padding } ->
            oneOf scanBlankline
                (scanSpacesToColumn (markerColumn + 1)
                    |> bind (\_ -> upToCountChars (padding - 1) ((==) ' '))
                    |> bind (\_ -> return ())
                )

        Reference ->
            nfb scanBlankline
                |> bind (\_ -> nfb (scanNonindentSpace |> bind (\_ -> scanReference)))

        _ ->
            return ()



-- Defines parsers that open new containers.


containerStart : Bool -> Parser ContainerType
containerStart _ =
    scanNonindentSpace
        |> bind
            (\_ ->
                oneOf (fmap (\_ -> BlockQuote) scanBlockquoteStart)
                    parseListMarker
            )



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


type alias ContainerM a =
    RWS () ReferenceMap ContainerStack a



-- Close the whole container stack, leaving only the root Document container.


closeStack : ContainerM Container
closeStack =
    RWS.get
        |> RWS.bind
            (\(ContainerStack top rest) ->
                if List.isEmpty rest then
                    RWS.return top

                else
                    closeContainer |> RWS.bind (\_ -> closeStack)
            )



-- Close the top container on the stack.  If the container is a Reference
-- container, attempt to parse the reference and update the reference map.
-- If it is a list item container, move a final BlankLine outside the list
-- item.


closeContainer : ContainerM ()
closeContainer =
    RWS.get
        |> RWS.bind
            (\(ContainerStack top rest) ->
                -- case top of
                --     Container Reference cs__ ->
                --         case parse pReference (String.trim <| joinLines <| List.map extractText cs__) of
                --             Ok ( lab, lnk, tit ) ->
                --                 RWS.tell (Dict.singleton (normalizeReference lab) ( lnk, tit ))
                --                     |> (\_ ->
                --                             case rest of
                --                                 (Container ct_ cs_) :: rs ->
                --                                     RWS.put (ContainerStack (Container ct_ (cs_ ++ [ C top ])) rs)
                --                                 [] ->
                --                                     RWS.return ()
                --                        )
                --             Err _ ->
                --                 -- pass over in silence if ref doesn't parse?
                --                 case rest of
                --                     c :: cs ->
                --                         RWS.put (ContainerStack c cs)
                --                     [] ->
                --                         RWS.return ()
                --     Container ((ListItem _) as li) cs__ ->
                --         case rest of
                --             -- move final BlankLine outside of list item
                --             (Container ct_ cs_) :: rs ->
                --                 -- case viewr cs__ of
                --                 --     ((L _ (BlankLine _)) :: zs) as b ->
                --                 --         RWS.put
                --                 --             (ContainerStack
                --                 --                 (if List.isEmpty zs then
                --                 --                     Container ct_ (cs_ |> C (Container li zs))
                --                 --                  else
                --                 --                     Container ct_
                --                 --                         (cs_
                --                 --                             |> C (Container li zs)
                --                 --                             |> b
                --                 --                         )
                --                 --                 )
                --                 --                 rs
                --                 --             )
                --                 --     _ ->
                --                 --         RWS.put (ContainerStack (Container ct_ (cs_ |> C top)) rs)
                --                 Debug.todo "closeContainer"
                --             [] ->
                --                 RWS.return ()
                --     _ ->
                --         case rest of
                --             (Container ct_ cs_) :: rs ->
                --                 RWS.put (ContainerStack (Container ct_ (cs_ ++ [ C top ])) rs)
                --             [] ->
                --                 RWS.return ()
                Debug.todo "closeContainer"
            )



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
    -- modify
    --     (\(ContainerStack top rest) ->
    --         ContainerStack (Container ct []) (top :: rest)
    --     )
    Debug.todo "addContainer"



-- Step 2


{-| Convert Document container and reference map into an AST.
-}
processDocument : ( Container, ReferenceMap ) -> Blocks
processDocument ( Container ct cs, refmap ) =
    case ct of
        Document ->
            processElts refmap cs

        _ ->
            crash "top level container is not Document"


{-| Turn the result of `processLines` into a proper AST.
This requires grouping text lines into paragraphs
and list items into lists, handling blank lines,
parsing inline contents of texts and resolving referencess.
-}
processElts : ReferenceMap -> List Elt -> Blocks
processElts refmap elts =
    case elts of
        [] ->
            []

        (L _ lf) :: rest ->
            case lf of
                -- Special handling of @docs lines in Elm:
                TextLine t ->
                    case stripPrefix "@docs" t of
                        Just terms1 ->
                            let
                                docs : List String
                                docs =
                                    terms1 :: List.map (cleanDoc << extractText) docLines

                                ( docLines, rest_ ) =
                                    List.partition isDocLine rest

                                isDocLine : Elt -> Bool
                                isDocLine elt =
                                    case elt of
                                        L _ (TextLine _) ->
                                            True

                                        _ ->
                                            False

                                cleanDoc : String -> String
                                cleanDoc lin =
                                    case stripPrefix "@docs" lin of
                                        Nothing ->
                                            lin

                                        Just stripped ->
                                            stripped
                            in
                            List.singleton (ElmDocs <| List.filter ((/=) []) <| List.map (List.filter ((/=) "") << List.map String.trim << String.split ",") docs)
                                ++ processElts refmap rest_

                        Nothing ->
                            -- Gobble text lines and make them into a Para:
                            let
                                txt =
                                    String.trimRight <|
                                        joinLines <|
                                            List.map String.trimLeft <|
                                                t
                                                    :: List.map extractText textlines

                                ( textlines, rest_ ) =
                                    List.partition isTextLine rest

                                isTextLine : Elt -> Bool
                                isTextLine elt =
                                    case elt of
                                        L _ (TextLine s) ->
                                            not (String.startsWith "@docs" s)

                                        _ ->
                                            False
                            in
                            List.singleton (Para <| parseInlines refmap txt)
                                ++ processElts refmap rest_

                -- Blanks at outer level are ignored:
                BlankLine _ ->
                    processElts refmap rest

                -- Headers:
                ATXHeader lvl t ->
                    List.singleton (Header lvl <| parseInlines refmap t)
                        ++ processElts refmap rest

                SetextHeader lvl t ->
                    List.singleton (Header lvl <| parseInlines refmap t)
                        ++ processElts refmap rest

                -- Horizontal rule:
                Rule ->
                    List.singleton HRule ++ processElts refmap rest

        (C (Container ct cs)) :: rest ->
            let
                isBlankLine : Elt -> Bool
                isBlankLine x =
                    case x of
                        L _ (BlankLine _) ->
                            True

                        _ ->
                            False

                tightListItem : List Elt -> Bool
                tightListItem xs =
                    case xs of
                        [] ->
                            True

                        _ ->
                            not <| List.any isBlankLine xs
            in
            case ct of
                Document ->
                    crash "Document container found inside Document"

                BlockQuote ->
                    List.singleton (Blockquote <| processElts refmap cs)
                        ++ processElts refmap rest

                -- List item?  Gobble up following list items of the same type
                -- (skipping blank lines), determine whether the list is tight or
                -- loose, and generate a List.
                ListItem { listType } ->
                    let
                        xs : List Elt
                        xs =
                            takeListItems rest

                        rest_ : List Elt
                        rest_ =
                            List.drop (List.length xs) rest

                        -- take list items as long as list type matches and we
                        -- don't hit two blank lines:
                        takeListItems ys =
                            case ys of
                                (C ((Container (ListItem li_) _) as c)) :: zs ->
                                    if listTypesMatch li_.listType listType then
                                        C c :: takeListItems zs

                                    else
                                        []

                                ((L _ (BlankLine _)) as lf) :: ((C (Container (ListItem li_) _)) as c) :: zs ->
                                    if listTypesMatch li_.listType listType then
                                        lf :: c :: takeListItems zs

                                    else
                                        []

                                _ ->
                                    []

                        listTypesMatch listType_ listType__ =
                            case ( listType_, listType__ ) of
                                ( Bullet c1, Bullet c2 ) ->
                                    c1 == c2

                                ( Numbered w1 _, Numbered w2 _ ) ->
                                    w1 == w2

                                _ ->
                                    False

                        items : List (List Elt)
                        items =
                            List.filterMap getItem
                                (Container ct cs
                                    :: List.filterMap
                                        (\x ->
                                            case x of
                                                C c ->
                                                    Just c

                                                _ ->
                                                    Nothing
                                        )
                                        xs
                                )

                        getItem : Container -> Maybe (List Elt)
                        getItem container =
                            case container of
                                Container (ListItem _) cs_ ->
                                    Just cs_

                                _ ->
                                    Nothing

                        items_ =
                            List.map (processElts refmap) items

                        isTight =
                            tightListItem xs && List.all tightListItem items
                    in
                    List.singleton (List isTight listType items_) ++ processElts refmap rest_

                FencedCode { info } ->
                    let
                        txt =
                            joinLines <| List.map extractText cs

                        attr =
                            CodeAttr { codeLang = x, codeInfo = String.trim y }

                        ( x, y ) =
                            stringBreak ((==) ' ') info
                    in
                    List.singleton (CodeBlock attr txt)
                        ++ processElts refmap rest

                IndentedCode ->
                    let
                        txt =
                            joinLines <|
                                stripTrailingEmpties <|
                                    List.concatMap extractCode cbs

                        stripTrailingEmpties =
                            List.reverse
                                << List.dropWhile (String.all ((==) ' '))
                                << List.reverse

                        -- explanation for next line:  when we parsed
                        -- the blank line, we dropped 0-3 spaces.
                        -- but for this, code block context, we want
                        -- to have dropped 4 spaces. we simply drop
                        -- one more:
                        extractCode elt =
                            case elt of
                                L _ (BlankLine t) ->
                                    [ String.dropLeft 1 t ]

                                C (Container IndentedCode cs_) ->
                                    List.map extractText cs_

                                _ ->
                                    []

                        ( cbs, rest_ ) =
                            List.break isIndentedCodeOrBlank
                                (C (Container ct cs) :: rest)

                        isIndentedCodeOrBlank elt =
                            case elt of
                                L _ (BlankLine _) ->
                                    True

                                C (Container IndentedCode _) ->
                                    True

                                _ ->
                                    False
                    in
                    List.singleton (CodeBlock (CodeAttr { codeLang = "", codeInfo = "" }) txt)
                        ++ processElts refmap rest_

                RawHtmlBlock ->
                    let
                        txt =
                            joinLines (List.map extractText cs)
                    in
                    List.singleton (HtmlBlock txt) ++ processElts refmap rest

                -- References have already been taken into account in the reference map,
                -- so we just skip.
                Reference ->
                    let
                        refs cs_ =
                            List.map (extractRef << extractText) cs_

                        extractRef t =
                            case parse pReference (String.trim t) of
                                Ok ( lab, lnk, tit ) ->
                                    ( lab, lnk, tit )

                                Err _ ->
                                    ( "??", "??", "??" )

                        processElts_ : List (List ( String, String, String )) -> List Elt -> Blocks
                        processElts_ acc pass =
                            case pass of
                                (C (Container Reference cs_)) :: rest_ ->
                                    processElts_ (refs cs_ :: acc) rest_

                                _ ->
                                    (List.singleton <| ReferencesBlock <| List.concat <| List.reverse acc)
                                        ++ processElts refmap pass
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


processLines : String -> ( Container, ReferenceMap )
processLines t =
    let
        ( doc, refmap ) =
            RWS.evalRWS (RWS.mapM_ processLine lns |> RWS.bind (\_ -> closeStack)) () startState

        lns : List ( LineNumber, String )
        lns =
            List.indexedMap Tuple.pair (List.map tabFilter (String.lines t))

        startState : ContainerStack
        startState =
            ContainerStack (Container Document []) []
    in
    ( doc, refmap )



-- The main block-parsing function.
-- We analyze a line of text and modify the container stack accordingly,
-- adding a new leaf, or closing or opening containers.


processLine : ( LineNumber, String ) -> ContainerM ()
processLine ( lineNumber, txt ) =
    -- -- Apply the line-start scanners appropriate for each nested container.
    -- -- Return the remainder of the string, and the number of unmatched
    -- -- containers.
    -- let
    --     ( t_, numUnmatched ) =
    --         tryOpenContainers (List.reverse (top :: rest)) txt
    --     -- Some new containers can be started only after a blank.
    --     lastLineIsText : Bool
    --     lastLineIsText =
    --         numUnmatched == 0
    --     -- &&
    --     --                     (case viewr cs of
    --     --                             (_ :> L _ (TextLine _)) -> True
    --     --                             _                       -> False)
    --     addNew ( ns, lf ) =
    --         -- do
    --         -- mapM_ addContainer ns
    --         -- case (reverse ns, lf) of
    --         --     -- don't add extra blank at beginning of fenced code block
    --         --     (FencedCode{}:_,  BlankLine{}) -> return ()
    --         --     _ -> addLeaf lineNumber lf
    --         Debug.todo "addNew"
    -- in
    -- -- Process the rest of the line in a way that makes sense given
    -- -- the container type at the top of the stack (ct):
    -- case ct of
    --     -- If it's a verbatim line container, add the line.
    --     RawHtmlBlock ->
    --         if numUnmatched == 0 then
    --             addLeaf lineNumber (TextLine t_)
    --         else
    --             ()
    --     IndentedCode ->
    --         if numUnmatched == 0 then
    --             addLeaf lineNumber (TextLine t_)
    --         else
    --             ()
    --     FencedCode { fence } ->
    --         -- here we don't check numUnmatched because we allow laziness
    --         if
    --             String.startsWith fence t_
    --             -- closing code fence
    --         then
    --             closeContainer
    --         else
    --             addLeaf lineNumber (TextLine t_)
    --     Reference ->
    --         case tryNewContainers lastLineIsText (String.length txt - String.length t_) t_ of
    --             ( ns, lf ) ->
    --                 closeContainer
    --                     >> addNew ( ns, lf )
    --     -- otherwise, parse the remainder to see if we have new container starts:
    --     _ ->
    --         case tryNewContainers lastLineIsText (String.length txt - String.length t_) t_ of
    --             -- -- lazy continuation: text line, last line was text, no new containers,
    --             -- -- some unmatched containers:
    --             -- ([], TextLine t)
    --             --     | numUnmatched > 0
    --             --     , case viewr cs of
    --             --             (_ :> L _ (TextLine _)) -> True
    --             --             _                       -> False
    --             --     , ct /= IndentedCode -> addLeaf lineNumber (TextLine t)
    --             -- -- if it's a setext header line and the top container has a textline
    --             -- -- as last child, add a setext header:
    --             -- ([], SetextHeader lev _) | numUnmatched == 0 ->
    --             --     case viewr cs of
    --             --         (cs' :> L _ (TextLine t)) -> -- replace last text line with setext header
    --             --         put $ ContainerStack (Container ct
    --             --                     (cs' |> L lineNumber (SetextHeader lev t))) rest
    --             --         -- Note: the following case should not occur, since
    --             --         -- we don't add a SetextHeader leaf unless lastLineIsText.
    --             --         _ -> error "setext header line without preceding text line"
    --             -- -- otherwise, close all the unmatched containers, add the new
    --             -- -- containers, and finally add the new leaf:
    --             ( ns, lf ) ->
    --                 -- close unmatched containers, add new ones
    --                 replicateM numUnmatched closeContainer
    --                     >> addNew ( ns, lf )
    Debug.todo "processLine"



-- Try to match the scanners corresponding to any currently open containers.
-- Return remaining text after matching scanners, plus the number of open
-- containers whose scanners did not match.  (These will be closed unless
-- we have a lazy text line.)


tryOpenContainers : List Container -> String -> ( String, Int )
tryOpenContainers cs t =
    let
        scanners : List (Parser a) -> Parser ( String, Int )
        scanners ss =
            case ss of
                [] ->
                    pure Tuple.pair
                        |> apply takeText
                        |> apply (pure 0)

                p :: ps ->
                    oneOf (p |> bind (\_ -> scanners ps)) (fmap Tuple.pair takeText |> apply (pure (List.length (p :: ps))))
    in
    case parse (scanners <| List.map containerContinue cs) t of
        Ok ( t_, n ) ->
            ( t_, n )

        Err e ->
            crash <|
                "error parsing scanners: "
                    ++ Debug.toString e



-- Try to match parsers for new containers.  Return list of new
-- container types, and the leaf to add inside the new containers.


tryNewContainers : Bool -> Int -> String -> ( List ContainerType, Leaf )
tryNewContainers lastLineIsText offset t =
    let
        newContainers =
            -- do
            --   getPosition >>= \pos -> setPosition pos{ column = offset + 1 }
            --   regContainers <- many (containerStart lastLineIsText)
            --   verbatimContainers <- option []
            --                     $ count 1 (verbatimContainerStart lastLineIsText)
            --   if null verbatimContainers
            --      then (,) <$> pure regContainers <*> leaf lastLineIsText
            --      else (,) <$> pure (regContainers ++ verbatimContainers) <*>
            --                     textLineOrBlank
            Debug.todo "newContainers"
    in
    case parse newContainers t of
        Ok ( cs, t_ ) ->
            ( cs, t_ )

        Err err ->
            crash (Debug.toString err)


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


blockHtmlTags : Set String
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


parseBullet : Parser ListType
parseBullet =
    -- do
    --   c <- satisfy (\c -> c == '+' || c == '*' || c == '-')
    --   unless (c == '+')
    --     $ nfb $ (count 2 $ scanSpaces >> skip (== c)) >>
    --           skipWhile (\x -> x == ' ' || x == c) >> endOfInput -- hrule
    --   return $ Bullet c
    Debug.todo "parseBullet"



-- Parse a list number marker and return list type.


parseListNumber : Parser ListType
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



-- ...


stripPrefix : String -> String -> Maybe String
stripPrefix p t =
    if String.startsWith p t then
        Just (String.dropLeft (String.length p) t)

    else
        Nothing


stringBreak : (Char -> Bool) -> String -> ( String, String )
stringBreak p t =
    Debug.todo "stringBreak"
