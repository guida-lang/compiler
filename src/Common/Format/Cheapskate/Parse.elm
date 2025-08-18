module Common.Format.Cheapskate.Parse exposing (markdown)

import Common.Format.Cheapskate.Inlines exposing (pHtmlTag, pLinkLabel, pReference, parseInlines)
import Common.Format.Cheapskate.ParserCombinators exposing (Parser, Position(..), apply, bind, char, count, endOfInput, fmap, getPosition, guard, lookAhead, many, notFollowedBy, oneOf, option, parse, pure, return, satisfy, setPosition, showParseError, skip, skipWhile, string, takeText, takeWhile, takeWhile1, unless)
import Common.Format.Cheapskate.Types exposing (Block(..), Blocks, CodeAttr(..), Doc(..), HtmlTagType(..), ListType(..), NumWrapper(..), Options, ReferenceMap)
import Common.Format.Cheapskate.Util exposing (Scanner, isWhitespace, joinLines, nfb, normalizeReference, scanBlankline, scanChar, scanIndentSpace, scanNonindentSpace, scanSpaces, scanSpacesToColumn, tabFilter, upToCountChars)
import Common.Format.RWS as RWS exposing (RWS)
import Data.Map as Dict
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
    scanNonindentSpace
        |> bind
            (\_ ->
                oneOf parseCodeFence
                    (oneOf
                        (guard (not lastLineIsText)
                            |> bind
                                (\_ ->
                                    nfb scanBlankline
                                        |> bind (\_ -> char ' ')
                                        |> fmap (\_ -> IndentedCode)
                                )
                        )
                        (oneOf (guard (not lastLineIsText) |> bind (\_ -> fmap (\_ -> RawHtmlBlock) parseHtmlBlockStart))
                            (guard (not lastLineIsText) |> bind (\_ -> fmap (\_ -> Reference) scanReference))
                        )
                    )
            )



-- Leaves of the container structure (they don't take children).


type Leaf
    = TextLine String
    | BlankLine String
    | ATXHeader Int String
    | SetextHeader Int String
    | Rule


type alias ContainerM a =
    RWS () ContainerStack a



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
                case top of
                    Container Reference cs__ ->
                        case parse pReference (String.trim <| joinLines <| List.map extractText cs__) of
                            Ok ( lab, lnk, tit ) ->
                                RWS.tell (Dict.singleton identity (normalizeReference lab) ( lnk, tit ))
                                    |> RWS.bind
                                        (\_ ->
                                            case rest of
                                                (Container ct_ cs_) :: rs ->
                                                    RWS.put (ContainerStack (Container ct_ (cs_ ++ [ C top ])) rs)

                                                [] ->
                                                    RWS.return ()
                                        )

                            Err _ ->
                                -- pass over in silence if ref doesn't parse?
                                case rest of
                                    c :: cs ->
                                        RWS.put (ContainerStack c cs)

                                    [] ->
                                        RWS.return ()

                    Container ((ListItem _) as li) cs__ ->
                        case rest of
                            -- move final BlankLine outside of list item
                            (Container ct_ cs_) :: rs ->
                                case List.reverse cs__ of
                                    ((L _ (BlankLine _)) as b) :: zs ->
                                        RWS.put
                                            (ContainerStack
                                                (if List.isEmpty zs then
                                                    Container ct_ (cs_ ++ [ C (Container li zs) ])

                                                 else
                                                    Container ct_ (cs_ ++ [ C (Container li zs), b ])
                                                )
                                                rs
                                            )

                                    _ ->
                                        RWS.put (ContainerStack (Container ct_ (cs_ ++ [ C top ])) rs)

                            [] ->
                                RWS.return ()

                    _ ->
                        case rest of
                            (Container ct_ cs_) :: rs ->
                                RWS.put (ContainerStack (Container ct_ (cs_ ++ [ C top ])) rs)

                            [] ->
                                RWS.return ()
            )



-- Add a leaf to the top container.


addLeaf : LineNumber -> Leaf -> ContainerM ()
addLeaf lineNum lf =
    RWS.get
        |> RWS.bind
            (\(ContainerStack top rest) ->
                case ( top, lf ) of
                    ( Container ((ListItem _) as ct) cs, BlankLine _ ) ->
                        case List.reverse cs of
                            (L _ (BlankLine _)) :: _ ->
                                -- two blanks break out of list item:
                                closeContainer
                                    |> RWS.bind (\_ -> addLeaf lineNum lf)

                            _ ->
                                RWS.put (ContainerStack (Container ct (L lineNum lf :: cs)) rest)

                    ( Container ct cs, _ ) ->
                        RWS.put (ContainerStack (Container ct (L lineNum lf :: cs)) rest)
            )



-- Add a container to the container stack.


addContainer : ContainerType -> ContainerM ()
addContainer ct =
    RWS.modify
        (\(ContainerStack top rest) ->
            ContainerStack (Container ct []) (top :: rest)
        )



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
                                    List.span isDocLine rest

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
                            (ElmDocs <| List.filter ((/=) []) <| List.map (List.filter ((/=) "") << List.map String.trim << String.split ",") docs)
                                :: processElts refmap rest_

                        Nothing ->
                            -- Gobble text lines and make them into a Para:
                            let
                                txt : String
                                txt =
                                    String.trimRight <|
                                        joinLines <|
                                            List.map String.trimLeft
                                                (t :: List.map extractText textlines)

                                ( textlines, rest_ ) =
                                    List.span isTextLine rest

                                isTextLine : Elt -> Bool
                                isTextLine elt =
                                    case elt of
                                        L _ (TextLine s) ->
                                            not (String.startsWith "@docs" s)

                                        _ ->
                                            False
                            in
                            Para (parseInlines refmap txt)
                                :: processElts refmap rest_

                -- Blanks at outer level are ignored:
                BlankLine _ ->
                    processElts refmap rest

                -- Headers:
                ATXHeader lvl t ->
                    (Header lvl <| parseInlines refmap t)
                        :: processElts refmap rest

                SetextHeader lvl t ->
                    (Header lvl <| parseInlines refmap t)
                        :: processElts refmap rest

                -- Horizontal rule:
                Rule ->
                    HRule :: processElts refmap rest

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
                    (Blockquote <| processElts refmap cs)
                        :: processElts refmap rest

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
                        takeListItems : List Elt -> List Elt
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

                        listTypesMatch : ListType -> ListType -> Bool
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

                        items_ : List Blocks
                        items_ =
                            List.map (processElts refmap) items

                        isTight : Bool
                        isTight =
                            tightListItem xs && List.all tightListItem items
                    in
                    List isTight listType items_ :: processElts refmap rest_

                FencedCode { info } ->
                    let
                        txt : String
                        txt =
                            joinLines <| List.map extractText cs

                        attr : CodeAttr
                        attr =
                            CodeAttr { codeLang = x, codeInfo = String.trim y }

                        ( x, y ) =
                            stringBreak ((==) ' ') info
                    in
                    CodeBlock attr txt
                        :: processElts refmap rest

                IndentedCode ->
                    let
                        txt : String
                        txt =
                            joinLines <|
                                stripTrailingEmpties <|
                                    List.concatMap extractCode cbs

                        stripTrailingEmpties : List String -> List String
                        stripTrailingEmpties =
                            List.reverse
                                << List.dropWhile (String.all ((==) ' '))
                                << List.reverse

                        -- explanation for next line:  when we parsed
                        -- the blank line, we dropped 0-3 spaces.
                        -- but for this, code block context, we want
                        -- to have dropped 4 spaces. we simply drop
                        -- one more:
                        extractCode : Elt -> List String
                        extractCode elt =
                            case elt of
                                L _ (BlankLine t) ->
                                    [ String.dropLeft 1 t ]

                                C (Container IndentedCode cs_) ->
                                    List.map extractText cs_

                                _ ->
                                    []

                        ( cbs, rest_ ) =
                            List.span isIndentedCodeOrBlank
                                (C (Container ct cs) :: rest)

                        isIndentedCodeOrBlank : Elt -> Bool
                        isIndentedCodeOrBlank elt =
                            case elt of
                                L _ (BlankLine _) ->
                                    True

                                C (Container IndentedCode _) ->
                                    True

                                _ ->
                                    False
                    in
                    CodeBlock (CodeAttr { codeLang = "", codeInfo = "" }) txt
                        :: processElts refmap rest_

                RawHtmlBlock ->
                    let
                        txt : String
                        txt =
                            joinLines (List.map extractText cs)
                    in
                    HtmlBlock txt :: processElts refmap rest

                -- References have already been taken into account in the reference map,
                -- so we just skip.
                Reference ->
                    let
                        refs : List Elt -> List ( String, String, String )
                        refs cs_ =
                            List.map (extractRef << extractText) cs_

                        extractRef : String -> ( String, String, String )
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
                                    (ReferencesBlock <| List.concat <| List.reverse acc)
                                        :: processElts refmap pass
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
        lns : List ( LineNumber, String )
        lns =
            List.indexedMap (\i ln -> ( i + 1, ln )) (List.map tabFilter (String.lines t))

        startState : ContainerStack
        startState =
            ContainerStack (Container Document []) []
    in
    RWS.evalRWS (RWS.mapM_ processLine lns |> RWS.bind (\_ -> closeStack)) () startState



-- The main block-parsing function.
-- We analyze a line of text and modify the container stack accordingly,
-- adding a new leaf, or closing or opening containers.


processLine : ( LineNumber, String ) -> ContainerM ()
processLine ( lineNumber, txt ) =
    RWS.get
        |> RWS.bind
            (\(ContainerStack ((Container ct cs) as top) rest) ->
                -- Apply the line-start scanners appropriate for each nested container.
                -- Return the remainder of the string, and the number of unmatched
                -- containers.
                let
                    ( t_, numUnmatched ) =
                        tryOpenContainers (List.reverse (top :: rest)) txt

                    -- Some new containers can be started only after a blank.
                    lastLineIsText : Bool
                    lastLineIsText =
                        (numUnmatched == 0)
                            && (case List.reverse cs of
                                    (L _ (TextLine _)) :: _ ->
                                        True

                                    _ ->
                                        False
                               )

                    addNew : ( List ContainerType, Leaf ) -> () -> ContainerStack -> ( (), ContainerStack, Dict.Dict String String ( String, String ) )
                    addNew ( ns, lf ) =
                        RWS.mapM_ addContainer ns
                            |> RWS.bind
                                (\_ ->
                                    case ( List.reverse ns, lf ) of
                                        -- don't add extra blank at beginning of fenced code block
                                        ( (FencedCode _) :: _, BlankLine _ ) ->
                                            RWS.return ()

                                        _ ->
                                            addLeaf lineNumber lf
                                )
                in
                -- Process the rest of the line in a way that makes sense given
                -- the container type at the top of the stack (ct):
                case ( ct, numUnmatched == 0 ) of
                    -- If it's a verbatim line container, add the line.
                    ( RawHtmlBlock, True ) ->
                        addLeaf lineNumber (TextLine t_)

                    ( IndentedCode, True ) ->
                        addLeaf lineNumber (TextLine t_)

                    ( FencedCode { fence }, _ ) ->
                        -- here we don't check numUnmatched because we allow laziness
                        if
                            String.startsWith fence t_
                            -- closing code fence
                        then
                            closeContainer

                        else
                            addLeaf lineNumber (TextLine t_)

                    ( Reference, _ ) ->
                        let
                            ( ns, lf ) =
                                tryNewContainers lastLineIsText (String.length txt - String.length t_) t_
                        in
                        closeContainer
                            |> RWS.bind (\_ -> addNew ( ns, lf ))

                    -- otherwise, parse the remainder to see if we have new container starts:
                    _ ->
                        case tryNewContainers lastLineIsText (String.length txt - String.length t_) t_ of
                            -- lazy continuation: text line, last line was text, no new containers,
                            -- some unmatched containers:
                            ( [] as ns, (TextLine t) as lf ) ->
                                if
                                    numUnmatched
                                        > 0
                                        && (case List.reverse cs of
                                                (L _ (TextLine _)) :: _ ->
                                                    True

                                                _ ->
                                                    False
                                           )
                                        && ct
                                        /= IndentedCode
                                then
                                    addLeaf lineNumber (TextLine t)

                                else
                                    -- close unmatched containers, add new ones
                                    RWS.replicateM numUnmatched closeContainer
                                        |> RWS.bind (\_ -> addNew ( ns, lf ))

                            -- if it's a setext header line and the top container has a textline
                            -- as last child, add a setext header:
                            ( [] as ns, (SetextHeader lev _) as lf ) ->
                                if numUnmatched == 0 then
                                    case List.reverse cs of
                                        (L _ (TextLine t)) :: cs_ ->
                                            -- replace last text line with setext header
                                            RWS.put
                                                (ContainerStack
                                                    (Container ct
                                                        (List.reverse (L lineNumber (SetextHeader lev t) :: cs_))
                                                    )
                                                    rest
                                                )

                                        -- Note: the following case should not occur, since
                                        -- we don't add a SetextHeader leaf unless lastLineIsText.
                                        _ ->
                                            RWS.error "setext header line without preceding text line"

                                else
                                    -- close unmatched containers, add new ones
                                    RWS.replicateM numUnmatched closeContainer
                                        |> RWS.bind (\_ -> addNew ( ns, lf ))

                            -- otherwise, close all the unmatched containers, add the new
                            -- containers, and finally add the new leaf:
                            ( ns, lf ) ->
                                -- close unmatched containers, add new ones
                                RWS.replicateM numUnmatched closeContainer
                                    |> RWS.bind (\_ -> addNew ( ns, lf ))
            )



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
                    ++ showParseError e



-- Try to match parsers for new containers.  Return list of new
-- container types, and the leaf to add inside the new containers.


tryNewContainers : Bool -> Int -> String -> ( List ContainerType, Leaf )
tryNewContainers lastLineIsText offset t =
    let
        newContainers : Parser ( List ContainerType, Leaf )
        newContainers =
            getPosition
                |> bind
                    (\(Position ln _) ->
                        setPosition (Position ln (offset + 1))
                            |> bind
                                (\_ ->
                                    many (containerStart lastLineIsText)
                                        |> bind
                                            (\regContainers ->
                                                option [] (count 1 (verbatimContainerStart lastLineIsText))
                                                    |> bind
                                                        (\verbatimContainers ->
                                                            if List.isEmpty verbatimContainers then
                                                                fmap (Tuple.pair regContainers) (leaf lastLineIsText)

                                                            else
                                                                fmap (Tuple.pair (regContainers ++ verbatimContainers)) textLineOrBlank
                                                        )
                                            )
                                )
                    )
    in
    case parse newContainers t of
        Ok ( cs, t_ ) ->
            ( cs, t_ )

        Err err ->
            crash (showParseError err)


textLineOrBlank : Parser Leaf
textLineOrBlank =
    let
        consolidate : String -> Leaf
        consolidate ts =
            if String.all isWhitespace ts then
                BlankLine ts

            else
                TextLine ts
    in
    fmap consolidate takeText



-- Parse a leaf node.


leaf : Bool -> Parser Leaf
leaf lastLineIsText =
    scanNonindentSpace
        |> bind
            (\_ ->
                let
                    removeATXSuffix : String -> String
                    removeATXSuffix t =
                        case String.uncons (String.reverse (stringDropWhileEnd (\c -> String.contains (String.fromChar c) " #") t)) of
                            Nothing ->
                                ""

                            Just ( '\\', t_ ) ->
                                String.reverse t_ ++ "\\#"

                            Just ( c, t_ ) ->
                                String.reverse (String.cons c t_)
                in
                oneOf
                    (pure ATXHeader
                        |> apply parseAtxHeaderStart
                        |> apply (fmap (String.trim << removeATXSuffix) takeText)
                    )
                    (oneOf
                        (guard lastLineIsText
                            |> bind
                                (\_ ->
                                    pure SetextHeader
                                        |> apply parseSetextHeaderLine
                                        |> apply (pure "")
                                )
                        )
                        (oneOf (fmap (\_ -> Rule) scanHRuleLine)
                            textLineOrBlank
                        )
                    )
            )



-- Scanners


scanReference : Scanner
scanReference =
    fmap (\_ -> ()) (lookAhead (pLinkLabel |> bind (\_ -> scanChar ':')))



-- Scan the beginning of a blockquote:  up to three
-- spaces indent, the `>` character, and an optional space.


scanBlockquoteStart : Scanner
scanBlockquoteStart =
    scanChar '>'
        |> bind (\_ -> option () (scanChar ' '))



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
    char '#'
        |> bind (\_ -> upToCountChars 5 ((==) '#'))
        |> bind
            (\hashes ->
                -- hashes must be followed by space unless empty header:
                notFollowedBy (skip ((/=) ' '))
                    |> fmap (\_ -> String.length hashes + 1)
            )


parseSetextHeaderLine : Parser Int
parseSetextHeaderLine =
    satisfy (\c -> c == '-' || c == '=')
        |> bind
            (\d ->
                let
                    lev : Int
                    lev =
                        if d == '=' then
                            1

                        else
                            2
                in
                skipWhile ((==) d)
                    |> bind (\_ -> scanBlankline)
                    |> fmap (\_ -> lev)
            )



-- Scan a horizontal rule line: "...three or more hyphens, asterisks,
-- or underscores on a line by themselves. If you wish, you may use
-- spaces between the hyphens or asterisks."


scanHRuleLine : Scanner
scanHRuleLine =
    satisfy (\c -> c == '*' || c == '_' || c == '-')
        |> bind
            (\c ->
                count 2 scanSpaces
                    |> bind (\_ -> skip ((==) c))
                    |> bind (\_ -> skipWhile (\x -> x == ' ' || x == c))
                    |> bind (\_ -> endOfInput)
            )



-- Parse an initial code fence line, returning
-- the fence part and the rest (after any spaces).


parseCodeFence : Parser ContainerType
parseCodeFence =
    getPosition
        |> bind
            (\(Position _ col) ->
                oneOf (takeWhile1 ((==) '`')) (takeWhile1 ((==) '~'))
                    |> bind
                        (\cs ->
                            guard (String.length cs >= 3)
                                |> bind (\_ -> scanSpaces)
                                |> bind (\_ -> takeWhile (\c -> c /= '`' && c /= '~'))
                                |> bind
                                    (\rawattr ->
                                        endOfInput
                                            |> fmap
                                                (\_ ->
                                                    FencedCode
                                                        { startColumn = col
                                                        , fence = cs
                                                        , info = rawattr
                                                        }
                                                )
                                    )
                        )
            )



-- Parse the start of an HTML block:  either an HTML tag or an
-- HTML comment, with no indentation.


parseHtmlBlockStart : Parser ()
parseHtmlBlockStart =
    let
        f : HtmlTagType -> Bool
        f htmlTagType =
            case htmlTagType of
                Opening name ->
                    Set.member name blockHtmlTags

                SelfClosing name ->
                    Set.member name blockHtmlTags

                Closing name ->
                    Set.member name blockHtmlTags
    in
    -- () <$
    lookAhead
        (oneOf
            (pHtmlTag
                |> bind
                    (\t ->
                        guard (f (Tuple.first t))
                            |> fmap (\_ -> Tuple.second t)
                    )
            )
            (oneOf (string "<!--") (string "-->"))
        )
        |> fmap (\_ -> ())



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
    getPosition
        |> bind
            (\(Position _ col) ->
                oneOf parseBullet parseListNumber
                    |> bind
                        (\ty ->
                            -- padding is 1 if list marker followed by a blank line
                            -- or indented code.  otherwise it's the length of the
                            -- whitespace between the list marker and the following text:
                            oneOf (fmap (\_ -> 1) scanBlankline)
                                (oneOf (fmap (\_ -> 1) (skip ((==) ' ') |> bind (\_ -> lookAhead (count 4 (char ' ')))))
                                    (fmap String.length (takeWhile ((==) ' ')))
                                )
                                |> bind
                                    (\padding_ ->
                                        -- text can't immediately follow the list marker:
                                        guard (padding_ > 0)
                                            |> bind
                                                (\() ->
                                                    return
                                                        (ListItem
                                                            { listType = ty
                                                            , markerColumn = col
                                                            , padding = padding_ + listMarkerWidth ty
                                                            }
                                                        )
                                                )
                                    )
                        )
            )


listMarkerWidth : ListType -> Int
listMarkerWidth listType =
    case listType of
        Bullet _ ->
            1

        Numbered _ n ->
            if n < 10 then
                2

            else if n < 100 then
                3

            else if n < 1000 then
                4

            else
                5



-- Parse a bullet and return list type.


parseBullet : Parser ListType
parseBullet =
    satisfy (\c -> c == '+' || c == '*' || c == '-')
        |> bind
            (\c ->
                unless (c == '+') (nfb (count 2 scanSpaces |> bind (\_ -> skip ((==) c))))
                    |> bind
                        (\_ ->
                            -- hrule
                            skipWhile (\x -> x == ' ' || x == c) |> bind (\_ -> endOfInput)
                        )
                    |> bind (\_ -> return (Bullet c))
            )



-- Parse a list number marker and return list type.


parseListNumber : Parser ListType
parseListNumber =
    takeWhile1 Char.isDigit
        |> bind
            (\numStr ->
                case String.toInt numStr of
                    Just num ->
                        oneOf (fmap (\_ -> PeriodFollowing) (skip ((==) '.'))) (fmap (\_ -> ParenFollowing) (skip ((==) ')')))
                            |> bind (\wrap -> return (Numbered wrap num))

                    Nothing ->
                        crash "Exception: Prelude.read: no parse"
            )



-- ...


stripPrefix : String -> String -> Maybe String
stripPrefix p t =
    if String.startsWith p t then
        Just (String.dropLeft (String.length p) t)

    else
        Nothing


stringBreak : (Char -> Bool) -> String -> ( String, String )
stringBreak p t =
    List.splitWhen p (String.toList t)
        |> Maybe.map (Tuple.mapBoth String.fromList String.fromList)
        |> Maybe.withDefault ( t, "" )


stringDropWhileEnd : (Char -> Bool) -> String -> String
stringDropWhileEnd f =
    String.reverse
        >> stringDropWhile f
        >> String.reverse


stringDropWhile : (Char -> Bool) -> String -> String
stringDropWhile f str =
    case String.uncons str of
        Just ( first, rest ) ->
            if f first then
                stringDropWhile f rest

            else
                str

        Nothing ->
            ""
