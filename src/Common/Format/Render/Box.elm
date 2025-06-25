module Common.Format.Render.Box exposing (..)

import Basics.Extra as Basics exposing (flip)
import Common.Format.Box as Box exposing (Box)
import Common.Format.ImportInfo as ImportInfo exposing (ImportInfo)
import Common.Format.KnownContents as KnownContents
import Common.Format.Render.ElmStructure as ElmStructure
import Common.Format.Render.Markdown as Markdown
import Compiler.AST.Source as Src
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Module as M
import Compiler.Parse.Space as Space
import Compiler.Reporting.Annotation as A
import Data.Map as Map exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Hex
import Language.GLSL.Syntax exposing (Statement(..))
import Maybe.Extra as Maybe
import Utils.Crash exposing (crash)


pleaseReport__ : String -> String -> String
pleaseReport__ what details =
    -- TODO: include version in the message
    "<elm-format: " ++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"


pleaseReport_ : String -> String -> Box.Line
pleaseReport_ what details =
    Box.keyword (pleaseReport__ what details)


pleaseReport : String -> String -> Box
pleaseReport what details =
    Box.line (pleaseReport_ what details)


surround : Char -> Char -> Box -> Box
surround left right b =
    let
        left_ : Box.Line
        left_ =
            Box.punc (String.fromChar left)

        right_ : Box.Line
        right_ =
            Box.punc (String.fromChar right)
    in
    case b of
        Box.SingleLine b_ ->
            Box.line (Box.row [ left_, b_, right_ ])

        _ ->
            Box.stack1
                [ Box.prefix left_ b
                , Box.line right_
                ]


parens : Box -> Box
parens =
    surround '(' ')'


formatBinary : Bool -> Box -> List ( ( Bool, Comments, Box ), Box ) -> Box
formatBinary multiline left ops =
    case ops of
        [] ->
            left

        ( ( isLeftPipe, comments, op ), next ) :: rest ->
            if isLeftPipe then
                ElmStructure.forceableSpaceSepOrIndented multiline
                    (ElmStructure.spaceSepOrStack left
                        (List.concat
                            [ Maybe.toList <| formatComments comments
                            , [ op ]
                            ]
                        )
                    )
                    [ formatBinary multiline next rest ]

            else
                formatBinary
                    multiline
                    (ElmStructure.forceableSpaceSepOrIndented multiline left [ formatCommentedApostrophe comments (ElmStructure.spaceSepOrPrefix op next) ])
                    rest


splitWhere : (a -> Bool) -> List a -> List (List a)
splitWhere predicate list =
    let
        merge : List a -> List (List a) -> List (List a)
        merge acc result =
            List.reverse acc :: result

        step : a -> ( List a, List (List a) ) -> ( List a, List (List a) )
        step next ( acc, result ) =
            if predicate next then
                ( [], merge (next :: acc) result )

            else
                ( next :: acc, result )
    in
    list
        |> List.foldl step ( [], [] )
        |> Basics.uncurry merge
        |> List.reverse
        |> List.filter List.isEmpty


type DeclarationType
    = DComment
    | DStarter
    | DCloser
    | DDefinition (Maybe (Ref ()))
    | DFixity
    | DDocComment


declarationType : TopLevelStructure BodyEntryType -> DeclarationType
declarationType decl =
    case decl of
        Entry entry ->
            case entry of
                BodyNamed name ->
                    DDefinition (Just name)

                BodyUnnamed ->
                    DDefinition Nothing

                BodyFixity ->
                    DFixity

        DocComment _ ->
            DDocComment

        BodyComment Space.CommentTrickOpener ->
            DStarter

        BodyComment Space.CommentTrickCloser ->
            DCloser

        BodyComment _ ->
            DComment


removeDuplicates : List (List (C2 Src.Exposed)) -> List (List (C2 Src.Exposed))
removeDuplicates input =
    let
        step :
            List (C2 Src.Exposed)
            -> ( List (List (C2 Src.Exposed)), EverySet String (C2 Src.Exposed) )
            -> ( List (List (C2 Src.Exposed)), EverySet String (C2 Src.Exposed) )
        step next ( acc, seen ) =
            case List.foldl stepChildren ( [], seen ) next |> (\( a, b ) -> ( List.reverse a, b )) of
                ( [], seen_ ) ->
                    ( acc, seen_ )

                ( children_, seen_ ) ->
                    ( children_ :: acc, seen_ )

        stepChildren :
            C2 Src.Exposed
            -> ( List (C2 Src.Exposed), EverySet String (C2 Src.Exposed) )
            -> ( List (C2 Src.Exposed), EverySet String (C2 Src.Exposed) )
        stepChildren next ( acc, seen ) =
            if EverySet.member (\( _, v, _ ) -> Debug.todo "v") next seen then
                ( acc, seen )

            else
                ( next :: acc, EverySet.insert (\( _, v, _ ) -> Debug.todo "v") next seen )
    in
    List.foldl step ( [], EverySet.empty ) input
        |> Tuple.first
        |> List.reverse


sortVars : Bool -> EverySet String (C2 Src.Exposed) -> List (List String) -> ( List (List (C2 Src.Exposed)), Comments )
sortVars forceMultiline fromExposing fromDocs =
    let
        varOrder : C2 Src.Exposed -> ( Int, String )
        varOrder ( _, exposed, _ ) =
            case exposed of
                Src.Operator _ name ->
                    ( 1, name )

                Src.Upper (A.At _ name) _ ->
                    ( 2, name )

                Src.Lower (A.At _ name) ->
                    ( 3, name )

        listedInDocs : List (List (C2 Src.Exposed))
        listedInDocs =
            fromDocs
                |> List.map (List.filterMap (\v -> Map.get identity v allowedInDocs))
                |> List.filter (not << List.isEmpty)
                |> List.map (List.map (\v -> ( [], v, [] )))
                |> removeDuplicates

        listedInExposing =
            fromExposing
                |> EverySet.toList (\a b -> compare (varName a) (varName b))
                |> List.sortBy varOrder

        varName : C2 Src.Exposed -> String
        varName ( _, exposed, _ ) =
            case exposed of
                Src.Lower (A.At _ name) ->
                    name

                Src.Upper (A.At _ name) _ ->
                    name

                Src.Operator _ name ->
                    name

        varSetToMap : EverySet String (C2 Src.Exposed) -> Dict String String Src.Exposed
        varSetToMap set =
            EverySet.toList (\a b -> compare (varName a) (varName b)) set
                |> List.map (\(( _, exposed, _ ) as var) -> ( varName var, exposed ))
                |> Map.fromList identity

        allowedInDocs : Dict String String Src.Exposed
        allowedInDocs =
            varSetToMap fromExposing

        allFromDocs : EverySet String String
        allFromDocs =
            EverySet.fromList identity (List.map varName (List.concat listedInDocs))

        inDocs : ( List Space.Comment, Src.Exposed, List Space.Comment ) -> Bool
        inDocs x =
            EverySet.member identity (varName x) allFromDocs

        remainingFromExposing : List ( List Space.Comment, Src.Exposed, List Space.Comment )
        remainingFromExposing =
            listedInExposing
                |> List.filter (not << inDocs)

        commentsFromReorderedVars : List Space.Comment
        commentsFromReorderedVars =
            listedInExposing
                |> List.filter inDocs
                |> List.map (\( pre, _, post ) -> pre ++ post)
                |> List.concat
    in
    if List.isEmpty listedInDocs && forceMultiline then
        ( List.map List.singleton remainingFromExposing, commentsFromReorderedVars )

    else
        ( listedInDocs
            ++ (if List.isEmpty remainingFromExposing then
                    []

                else
                    [ remainingFromExposing ]
               )
        , commentsFromReorderedVars
        )


formatModuleHeader : Bool -> M.Module -> List Box
formatModuleHeader addDefaultHeader modu =
    let
        maybeHeader : Maybe M.Header
        maybeHeader =
            if addDefaultHeader then
                Just (Maybe.withDefault M.defaultHeader modu.header)

            else
                modu.header

        refName ref =
            case ref of
                VarRef _ name ->
                    name

                TagRef _ name ->
                    name

                OpRef name ->
                    name

        documentedVars : List (List String)
        documentedVars =
            -- TODO
            -- modu.header
            --     |> Maybe.andThen (.docs >> Result.toMaybe)
            --     |> Maybe.toList
            --     |> List.concatMap extractDocs
            []

        documentedVarsSet : EverySet String String
        documentedVarsSet =
            EverySet.fromList identity (List.concat documentedVars)

        extractDocs : Markdown.Block -> List (List String)
        extractDocs block =
            case block of
                Markdown.ElmDocs vars ->
                    List.map (List.map (refName << textToRef)) vars

                _ ->
                    []

        textToRef : String -> Ref (List String)
        textToRef text =
            case String.toList text of
                c :: [] ->
                    if Char.isUpper c then
                        TagRef [] text

                    else
                        VarRef [] text

                [ '(', a, ')' ] ->
                    OpRef (String.fromChar a)

                [ '(', a, b, ')' ] ->
                    OpRef (String.fromList [ a, b ])

                _ ->
                    VarRef [] text

        definedVars : EverySet String ( List Space.Comment, Src.Exposed, List Space.Comment )
        definedVars =
            modu.decls
                |> List.concatMap extractVarName
                |> List.map (\varName -> ( [], varName, [] ))
                |> EverySet.fromList
                    (\( _, exposed, _ ) ->
                        case exposed of
                            Src.Lower (A.At _ name) ->
                                name

                            Src.Operator _ name ->
                                name

                            Src.Upper (A.At _ name) _ ->
                                name
                    )

        exportsList =
            Maybe.withDefault M.defaultHeader maybeHeader
                |> .exports
                |> (\( _, v, _ ) -> v)

        detailedListingToSet : Src.Exposing -> EverySet String ( List Space.Comment, Src.Exposed, List Space.Comment )
        detailedListingToSet listing =
            case listing of
                Src.Open ->
                    EverySet.empty

                Src.Explicit exposedList ->
                    exposedList
                        |> List.map (\exposed -> ( [], exposed, [] ))
                        |> EverySet.fromList
                            (\( _, exposed, _ ) ->
                                case exposed of
                                    Src.Lower (A.At _ name) ->
                                        name

                                    Src.Operator _ name ->
                                        name

                                    Src.Upper (A.At _ name) _ ->
                                        name
                            )

        detailedListingIsMultiline : A.Located Src.Exposing -> Bool
        detailedListingIsMultiline listing =
            case listing of
                A.At region (Src.Explicit _) ->
                    A.isMultiline region

                A.At _ Src.Open ->
                    False

        varsToExpose : EverySet String ( List Space.Comment, Src.Exposed, List Space.Comment )
        varsToExpose =
            case Maybe.map .exports maybeHeader of
                Nothing ->
                    if List.all List.isEmpty documentedVars then
                        definedVars

                    else
                        EverySet.filter
                            (\( _, v, _ ) ->
                                EverySet.member identity
                                    (case v of
                                        Src.Lower (A.At _ name) ->
                                            name

                                        Src.Operator _ name ->
                                            name

                                        Src.Upper (A.At _ name) _ ->
                                            name
                                    )
                                    documentedVarsSet
                            )
                            definedVars

                Just ( _, A.At _ e, _ ) ->
                    detailedListingToSet e

        sortedExports : ( List (List ( List Space.Comment, Src.Exposed, List Space.Comment )), List Space.Comment )
        sortedExports =
            sortVars
                (detailedListingIsMultiline exportsList)
                varsToExpose
                documentedVars

        extractVarName : Decl.Decl -> List Src.Exposed
        extractVarName decl =
            case decl of
                Decl.Value _ _ (A.At _ (Src.Value name _ _ _)) ->
                    [ Src.Lower name ]

                Decl.Union _ (A.At _ (Src.Union name _ _)) ->
                    [ Src.Upper name (Src.Public A.zero) ]

                Decl.Alias _ (A.At _ (Src.Alias name _ _)) ->
                    [ Src.Upper name Src.Private ]

                Decl.Port _ (Src.Port name _) ->
                    [ Src.Lower name ]

        formatModuleLine_ : M.Header -> Box
        formatModuleLine_ header =
            let
                ( preExposing, _, postExposing ) =
                    header.exports
            in
            formatModuleLine sortedExports header.effects header.name preExposing postExposing

        -- docs =
        --     fmap (formatDocComment (ImportInfo.fromModule mempty modu)) <| AST.Module.docs modu
        -- docs =
        --     modu.header
        --         |> Maybe.andThen (.docs >> Result.toMaybe)
        --         |> Maybe.map (formatDocComment (ImportInfo.fromModule KnownContents.mempty modu))
        imports =
            formatImports modu
    in
    List.intersperse Box.blankLine
        (List.concat
            [ Maybe.toList (Maybe.map formatModuleLine_ maybeHeader)

            -- , Maybe.toList docs
            , imports
            ]
        )


formatImports : M.Module -> List Box
formatImports modu =
    let
        comments : Comments
        comments =
            -- TODO
            []

        imports : List Src.Import
        imports =
            modu.imports
    in
    [ formatComments comments
        |> Maybe.toList
    , List.map formatImport imports
    ]
        |> List.filter (not << List.isEmpty)
        |> List.intersperse [ Box.blankLine ]
        |> List.concat


formatModuleLine :
    ( List (List (C2 Src.Exposed)), List Space.Comment )
    -> M.Effects
    -> ( List Space.Comment, A.Located Name.Name, List Space.Comment )
    -> List Space.Comment
    -> List Space.Comment
    -> Box
formatModuleLine ( varsToExpose, extraComments ) srcTag ( preName, A.At _ name, postName ) preExposing postExposing =
    let
        tag =
            case srcTag of
                M.NoEffects _ ->
                    Box.line (Box.keyword "module")

                M.Ports _ ->
                    let
                        comments =
                            -- TODO
                            []
                    in
                    ElmStructure.spaceSepOrIndented
                        (formatTailCommented ( comments, Box.line (Box.keyword "port") ))
                        [ Box.line (Box.keyword "module") ]

                M.Manager _ _ ->
                    let
                        comments =
                            -- TODO
                            []
                    in
                    ElmStructure.spaceSepOrIndented
                        (formatTailCommented ( comments, Box.line (Box.keyword "effect") ))
                        [ Box.line (Box.keyword "module") ]

        exports =
            case varsToExpose of
                [] ->
                    Box.line (Box.keyword "(..)")

                [ oneGroup ] ->
                    oneGroup
                        |> List.map (formatCommented << c2map formatVarValue)
                        |> ElmStructure.group_ False "(" "," (Maybe.toList (formatComments extraComments)) ")" False

                _ ->
                    varsToExpose
                        |> List.map (formatCommented << c2map (ElmStructure.group False "" "," "" False << List.map formatVarValue) << sequenceAC2)
                        |> ElmStructure.group_ False "(" "," (Maybe.toList (formatComments extraComments)) ")" True

        formatSetting : ( ( List Space.Comment, String, List Space.Comment ), ( List Space.Comment, String, List Space.Comment ) ) -> Box
        formatSetting ( k, v ) =
            formatRecordPair "=" (Box.line << formatUppercaseIdentifier) ( k, v, False )

        formatSettings : List ( ( List Space.Comment, String, List Space.Comment ), ( List Space.Comment, String, List Space.Comment ) ) -> Box
        formatSettings settings =
            List.map formatSetting settings
                |> ElmStructure.group True "{" "," "}" False

        whereClause : List Box
        whereClause =
            case srcTag of
                M.NoEffects _ ->
                    []

                M.Ports _ ->
                    []

                M.Manager _ manager ->
                    let
                        settings : List ( ( List Space.Comment, String, List Space.Comment ), ( List Space.Comment, Name.Name, List Space.Comment ) )
                        settings =
                            case manager of
                                Src.Cmd (A.At _ cmdType) ->
                                    [ ( ( [], "command", [] ), ( [], cmdType, [] ) ) ]

                                Src.Sub (A.At _ subType) ->
                                    [ ( ( [], "subscription", [] ), ( [], subType, [] ) ) ]

                                Src.Fx (A.At _ cmdType) (A.At _ subType) ->
                                    [ ( ( [], "command", [] ), ( [], cmdType, [] ) )
                                    , ( ( [], "subscription", [] ), ( [], subType, [] ) )
                                    ]
                    in
                    -- TODO add comments around manager/settings (`where` part)
                    [ formatKeywordCommented "where" ( [], formatSettings settings, [] ) ]

        nameClause =
            case
                ( tag
                , formatCommented ( preName, Box.line (formatQualifiedUppercaseIdentifier (String.split "." name)), postName )
                )
            of
                ( Box.SingleLine tag_, Box.SingleLine name_ ) ->
                    Box.line
                        (Box.row
                            [ tag_
                            , Box.space
                            , name_
                            ]
                        )

                ( tag_, name_ ) ->
                    Box.stack1
                        [ tag_
                        , Box.indent name_
                        ]
    in
    ElmStructure.spaceSepOrIndented
        (ElmStructure.spaceSepOrIndented
            nameClause
            (whereClause ++ [ formatCommented ( preExposing, Box.line (Box.keyword "exposing"), postExposing ) ])
        )
        [ exports ]


formatModule : Bool -> Int -> M.Module -> Box
formatModule addDefaultHeader spacing modu =
    let
        initialComments_ =
            case modu.initialComments of
                [] ->
                    []

                comments ->
                    List.map formatComment comments
                        ++ [ Box.blankLine, Box.blankLine ]

        spaceBeforeBody : Int
        spaceBeforeBody =
            case ( modu.decls, Maybe.andThen (.docs >> Result.toMaybe) modu.header ) of
                ( [], _ ) ->
                    0

                ( _, Just _ ) ->
                    spacing + 1

                _ ->
                    spacing

        decls =
            modu.decls
                -- TODO review
                |> List.map Entry
    in
    Box.stack1
        (List.concat
            [ initialComments_
            , formatModuleHeader addDefaultHeader modu
            , List.repeat spaceBeforeBody Box.blankLine
            , Maybe.toList (formatModuleBody spacing (ImportInfo.fromModule KnownContents.mempty modu) decls)
            ]
        )


formatModuleBody : Int -> ImportInfo -> List (TopLevelStructure Decl.Decl) -> Maybe Box
formatModuleBody linesBetween importInfo body =
    let
        entryType : Decl.Decl -> BodyEntryType
        entryType adecl =
            case adecl of
                -- CommonDeclaration def ->
                --     case extract (I.unFix def) of
                --         Definition pat _ _ _ ->
                --             case extract (I.unFix pat) of
                --                 VarPattern name ->
                --                     BodyNamed (VarRef () name)
                --                 OpPattern name ->
                --                     BodyNamed (OpRef name)
                --                 _ ->
                --                     BodyUnnamed
                --         TypeAnnotation (C _ name) _ ->
                --             BodyNamed name
                -- Datatype (C _ (NameWithArgs name _)) _ ->
                --     BodyNamed (TagRef () name)
                -- TypeAlias _ (C _ (NameWithArgs name _)) _ ->
                --     BodyNamed (TagRef () name)
                -- PortAnnotation (C _ name) _ _ ->
                --     BodyNamed (VarRef () name)
                -- Fixity _ _ _ _ ->
                --     BodyFixity
                Decl.Value _ _ (A.At _ (Src.Value (A.At _ name) _ _ _)) ->
                    BodyNamed (VarRef () name)

                Decl.Union _ (A.At _ (Src.Union (A.At _ name) _ _)) ->
                    BodyNamed (TagRef () name)

                Decl.Alias _ (A.At _ (Src.Alias (A.At _ name) _ _)) ->
                    BodyNamed (TagRef () name)

                Decl.Port _ (Src.Port (A.At _ name) _) ->
                    BodyNamed (VarRef () name)
    in
    formatTopLevelBody linesBetween importInfo <|
        List.map (topLevelStructureMap (\b -> ( entryType b, formatDeclaration importInfo b ))) body


type BodyEntryType
    = BodyNamed (Ref ())
    | BodyUnnamed
    | BodyFixity


type Ref ns
    = VarRef ns String
    | TagRef ns String
    | OpRef String


refMap : (a -> b) -> Ref a -> Ref b
refMap f ref =
    case ref of
        VarRef namespace name ->
            VarRef (f namespace) name

        TagRef namespace name ->
            TagRef (f namespace) name

        OpRef name ->
            OpRef name


type TopLevelStructure a
    = DocComment Markdown.Blocks
    | BodyComment Space.Comment
    | Entry a


topLevelStructureMap : (a -> b) -> TopLevelStructure a -> TopLevelStructure b
topLevelStructureMap f topLevelStructure =
    case topLevelStructure of
        DocComment blocks ->
            DocComment blocks

        BodyComment comment ->
            BodyComment comment

        Entry a ->
            Entry (f a)


formatTopLevelBody :
    Int
    -> ImportInfo
    -> List (TopLevelStructure ( BodyEntryType, Box ))
    -> Maybe Box
formatTopLevelBody linesBetween importInfo body =
    let
        extraLines : Int -> List Box
        extraLines n =
            List.repeat n Box.blankLine

        spacer : TopLevelStructure ( BodyEntryType, Box ) -> TopLevelStructure ( BodyEntryType, Box ) -> Int
        spacer a b =
            case ( declarationType (topLevelStructureMap Tuple.first a), declarationType (topLevelStructureMap Tuple.first b) ) of
                ( DStarter, _ ) ->
                    0

                ( _, DCloser ) ->
                    0

                ( DComment, DComment ) ->
                    0

                ( _, DComment ) ->
                    if linesBetween == 1 then
                        1

                    else
                        linesBetween + 1

                ( DComment, DDefinition _ ) ->
                    if linesBetween == 1 then
                        0

                    else
                        linesBetween

                ( DComment, _ ) ->
                    linesBetween

                ( DDocComment, DDefinition _ ) ->
                    0

                ( DDefinition Nothing, DDefinition (Just _) ) ->
                    linesBetween

                ( DDefinition _, DStarter ) ->
                    linesBetween

                ( DDefinition Nothing, DDefinition Nothing ) ->
                    linesBetween

                ( DDefinition a_, DDefinition b_ ) ->
                    if a_ == b_ then
                        0

                    else
                        linesBetween

                ( DCloser, _ ) ->
                    linesBetween

                ( _, DDocComment ) ->
                    linesBetween

                ( DDocComment, DStarter ) ->
                    0

                ( DFixity, DFixity ) ->
                    0

                ( DFixity, _ ) ->
                    linesBetween

                ( _, DFixity ) ->
                    linesBetween

        boxes : List Box
        boxes =
            intersperseMap (\a b -> extraLines (spacer a b))
                (formatTopLevelStructure importInfo << topLevelStructureMap Tuple.second)
                body
    in
    case boxes of
        [] ->
            Nothing

        _ ->
            Just (Box.stack1 boxes)


pairs : List a -> List ( a, a )
pairs input =
    let
        step next ( prev, acc ) =
            case prev of
                Nothing ->
                    ( Just next, acc )

                Just prev_ ->
                    ( Just next, ( next, prev_ ) :: acc )
    in
    List.foldr step ( Nothing, [] ) input
        |> Tuple.second


intersperseMap : (a -> a -> List b) -> (a -> b) -> List a -> List b
intersperseMap spacer fn list =
    case list of
        [] ->
            []

        first :: _ ->
            fn first
                :: (pairs list
                        |> List.concatMap (\( a, b ) -> spacer a b ++ [ fn b ])
                   )



-- type ElmCodeBlock annf ns
--     = DeclarationsCode (List (TopLevelStructure (ASTNS annf ns TopLevelDeclarationNK)))
--     | ExpressionsCode (List (TopLevelStructure (C0Eol (ASTNS annf ns ExpressionNK))))
--     | ModuleCode (AST.Module.Module ns (ASTNS annf ns TopLevelNK))
-- convertElmCodeBlock : (ann -> ann_) -> ElmCodeBlock ann ns -> ElmCodeBlock ann_ ns
-- convertElmCodeBlock f elmCodeBlock =
--     case elmCodeBlock of
--         DeclarationsCode decls ->
--             DeclarationsCode (fmap (fmap (I.convert f)) decls)
--         ExpressionsCode exprs ->
--             ExpressionsCode (fmap (fmap (fmap (I.convert f))) exprs)
--         ModuleCode mod ->
--             ModuleCode (fmap (I.convert f) mod)
-- -- TODO: there must be an existing haskell function that does this, right?
-- firstOf : List (a -> Maybe b) -> a -> Maybe b
-- firstOf options value =
--     case options of
--         [] ->
--             Nothing
--         next :: rest ->
--             case next value of
--                 Just result ->
--                     Just result
--                 Nothing ->
--                     firstOf rest value


formatDocComment : ImportInfo -> Markdown.Blocks -> Box
formatDocComment importInfo blocks =
    -- let
    --     parse : String -> Maybe (ElmCodeBlock Identity (List UppercaseIdentifier))
    --     parse source =
    --         source
    --             |> firstOf
    --                 [ fmap DeclarationsCode << Result.toMaybe << Parse.parseDeclarations
    --                 , fmap ExpressionsCode << Result.toMaybe << Parse.parseExpressions
    --                 , fmap ModuleCode << Result.toMaybe << Parse.parseModule
    --                 ]
    --             |> fmap (convertElmCodeBlock (pure << extract))
    --     format : ElmCodeBlock annf (List UppercaseIdentifier) -> String
    --     format result =
    --         case result of
    --             ModuleCode modu ->
    --                 formatModule False 1 modu
    --                     |> (Text.unpack << Box.render)
    --             DeclarationsCode declarations ->
    --                 formatModuleBody 1 importInfo declarations
    --                     |> fmap (Text.unpack << Box.render)
    --                     |> fromMaybe ""
    --             ExpressionsCode expressions ->
    --                 expressions
    --                     |> fmap (fmap (fmap (I.convert (Identity << extract))))
    --                     |> fmap (fmap (formatEolCommented << fmap (syntaxParens SyntaxSeparated << formatExpression importInfo)))
    --                     |> fmap (fmap (Tuple.pair BodyUnnamed))
    --                     |> formatTopLevelBody 1 importInfo
    --                     |> fmap (Text.unpack << Box.render)
    --                     |> fromMaybe ""
    --     content : String
    --     content =
    --         ElmFormat.Render.Markdown.formatMarkdown (fmap format << parse) (fmap cleanBlock blocks)
    --     cleanBlock : Markdown.Block -> Markdown.Block
    --     cleanBlock block =
    --         case block of
    --             Markdown.ElmDocs docs ->
    --                 Markdown.ElmDocs
    --                     ((fmap << fmap)
    --                         (Text.replace (Text.pack "(..)") (Text.pack ""))
    --                         docs
    --                     )
    --             _ ->
    --                 block
    -- in
    -- formatDocCommentString content
    Debug.todo "formatDocComment"



-- formatDocCommentString : String -> Box
-- formatDocCommentString docs =
--     case lines docs of
--         [] ->
--             line (row [ punc "{-|", space, punc "-}" ])
--         [ first ] ->
--             stack1
--                 [ line (row [ punc "{-|", space, literal first ])
--                 , line (punc "-}")
--                 ]
--         first :: rest ->
--             line (row [ punc "{-|", space, literal first ])
--                 |> andThen (map (line << literal) rest)
--                 |> andThen [ line <| punc "-}" ]


formatImport : Src.Import -> Box
formatImport (Src.Import (A.At _ importName) maybeAlias exposing_) =
    let
        maybeRequestedAs =
            maybeAlias
                |> Maybe.andThen
                    (\aliasName ->
                        if aliasName == importName then
                            Nothing

                        else
                            Just aliasName
                    )

        asVar =
            maybeRequestedAs
                |> Maybe.map
                    (\requestedAs ->
                        formatImportClause
                            (Just << Box.line << formatUppercaseIdentifier)
                            "as"
                            ( [], requestedAs, [] )
                    )
                |> Maybe.join

        exposingVar =
            formatImportClause
                formatExposing
                "exposing"
                ( [], exposing_, [] )

        formatImportClause : (a -> Maybe Box) -> String -> C2 a -> Maybe Box
        formatImportClause format keyw input =
            case c2map format input of
                ( [], Nothing, [] ) ->
                    Nothing

                ( preKeyword, Just listing_, postKeyword ) ->
                    case
                        ( formatPreCommented ( preKeyword, Box.line (Box.keyword keyw) )
                        , formatPreCommented ( postKeyword, listing_ )
                        )
                    of
                        ( Box.SingleLine keyword_, Box.SingleLine listing__ ) ->
                            Just
                                (Box.line
                                    (Box.row
                                        [ keyword_
                                        , Box.space
                                        , listing__
                                        ]
                                    )
                                )

                        ( keyword_, listing__ ) ->
                            Just
                                (Box.stack1
                                    [ keyword_
                                    , Box.indent listing__
                                    ]
                                )

                _ ->
                    Just (pleaseReport "UNEXPECTED IMPORT" "import clause comments with no clause")
    in
    case
        ( formatPreCommented (c1map (Box.line << formatQualifiedUppercaseIdentifier) ( [], String.split "." importName ))
        , asVar
        , exposingVar
        )
    of
        ( Box.SingleLine name_, Just (Box.SingleLine as_), Just (Box.SingleLine exposing__) ) ->
            Box.line <|
                Box.row
                    [ Box.keyword "import"
                    , Box.space
                    , name_
                    , Box.space
                    , as_
                    , Box.space
                    , exposing__
                    ]

        ( Box.SingleLine name_, Just (Box.SingleLine as_), Nothing ) ->
            Box.line <|
                Box.row
                    [ Box.keyword "import"
                    , Box.space
                    , name_
                    , Box.space
                    , as_
                    ]

        ( Box.SingleLine name_, Nothing, Just (Box.SingleLine exposing__) ) ->
            Box.line <|
                Box.row
                    [ Box.keyword "import"
                    , Box.space
                    , name_
                    , Box.space
                    , exposing__
                    ]

        ( Box.SingleLine name_, Nothing, Nothing ) ->
            Box.line <|
                Box.row
                    [ Box.keyword "import"
                    , Box.space
                    , name_
                    ]

        ( Box.SingleLine name_, Just (Box.SingleLine as_), Just exposing__ ) ->
            Box.stack1
                [ Box.line <|
                    Box.row
                        [ Box.keyword "import"
                        , Box.space
                        , name_
                        , Box.space
                        , as_
                        ]
                , Box.indent exposing__
                ]

        ( Box.SingleLine name_, Just as_, Just exposing__ ) ->
            Box.stack1
                [ Box.line <|
                    Box.row
                        [ Box.keyword "import"
                        , Box.space
                        , name_
                        ]
                , Box.indent as_
                , Box.indent exposing__
                ]

        ( Box.SingleLine name_, Nothing, Just exposing__ ) ->
            Box.stack1
                [ Box.line <|
                    Box.row
                        [ Box.keyword "import"
                        , Box.space
                        , name_
                        ]
                , Box.indent exposing__
                ]

        ( name_, Just as_, Just exposing__ ) ->
            Box.stack1
                [ Box.line <| Box.keyword "import"
                , Box.indent name_
                , Box.indent <| Box.indent as_
                , Box.indent <| Box.indent exposing__
                ]

        ( name_, Nothing, Just exposing__ ) ->
            Box.stack1
                [ Box.line <| Box.keyword "import"
                , Box.indent name_
                , Box.indent <| Box.indent exposing__
                ]

        ( name_, Just as_, Nothing ) ->
            Box.stack1
                [ Box.line <| Box.keyword "import"
                , Box.indent name_
                , Box.indent <| Box.indent as_
                ]

        ( name_, Nothing, Nothing ) ->
            Box.stack1
                [ Box.line <| Box.keyword "import"
                , Box.indent name_
                ]


formatListing : Src.Privacy -> Maybe Box
formatListing listing =
    case listing of
        Src.Private ->
            Nothing

        Src.Public _ ->
            -- TODO comments
            Just (parens (formatCommented ( [], Box.line (Box.keyword ".."), [] )))


formatExposing : Src.Exposing -> Maybe Box
formatExposing listing =
    case listing of
        Src.Open ->
            -- TODO comments
            Just (parens (formatCommented ( [], Box.line (Box.keyword ".."), [] )))

        Src.Explicit _ ->
            Nothing


type Listing a
    = ExplicitListing a Bool
    | OpenListing (C2 ())
    | ClosedListing


type alias DetailedListing =
    { values : Dict String String (C2 ())
    , operators : Dict String String (C2 ())
    , types : Dict String String (C1 (Listing (Dict String String (C2 ()))))
    }



-- formatDetailedListing : DetailedListing -> List Box
-- formatDetailedListing listing =
--     List.concat
--         [ formatCommentedMap (\_ _ -> EQ)
--             (\name () -> Src.Operator A.zero name)
--             formatVarValue
--             listing.operators
--         , formatCommentedMap (\_ _ -> EQ)
--             (\name _ -> Src.Upper (A.At A.zero name) Src.Private)
--             formatVarValue
--             listing.types
--         , formatCommentedMap (\_ _ -> EQ)
--             (\name () -> Src.Lower (A.At A.zero name))
--             formatVarValue
--             listing.values
--         ]


formatCommentedMap : (k -> k -> Order) -> (k -> v -> a) -> (a -> Box) -> Dict comparable k (C2 v) -> List Box
formatCommentedMap keyComparison construct format values =
    let
        format_ ( k, ( beforeComments, v, afterComments ) ) =
            formatCommented ( beforeComments, format (construct k v), afterComments )
    in
    values
        |> Map.toList keyComparison
        |> List.map format_


formatVarValue : Src.Exposed -> Box
formatVarValue aval =
    case aval of
        Src.Lower (A.At _ val) ->
            Box.line (formatLowercaseIdentifier [] val)

        Src.Operator _ name ->
            Box.line (Box.identifier ("(" ++ name ++ ")"))

        Src.Upper (A.At _ name) privacy ->
            case
                ( formatListing privacy
                  -- TODO post-comments on `name`
                , formatTailCommented ( [], Box.line (formatUppercaseIdentifier name) )
                )
            of
                ( Just _, _ ) ->
                    formatTailCommented <|
                        -- TODO post-comments on `name`
                        ( [], Box.line (Box.row [ formatUppercaseIdentifier name, Box.keyword "(..)" ]) )

                ( Nothing, name_ ) ->
                    name_


formatTopLevelStructure : ImportInfo -> TopLevelStructure Box -> Box
formatTopLevelStructure importInfo topLevelStructure =
    case topLevelStructure of
        DocComment docs ->
            formatDocComment importInfo docs

        BodyComment c ->
            formatComment c

        Entry entry ->
            entry


formatCommonDeclaration : ImportInfo -> A.Located Src.Value -> Box
formatCommonDeclaration importInfo (A.At _ (Src.Value (A.At nameRegion name) args expr _)) =
    -- case decl of
    --     Definition name args comments expr ->
    --         formatDefinition importInfo name args comments expr
    --     TypeAnnotation name typ ->
    --         formatTypeAnnotation name typ
    formatDefinition importInfo (A.At nameRegion (Src.PVar name)) args [] expr


formatDeclaration : ImportInfo -> Decl.Decl -> Box
formatDeclaration importInfo decl =
    case decl of
        -- CommonDeclaration def ->
        --     formatCommonDeclaration importInfo def
        -- Datatype nameWithArgs tags ->
        --     let
        --         ctor (NameWithArgs tag args_) =
        --             case allSingles <| map (formatPreCommented .fmap (typeParens ForCtor << formatType)) args_ of
        --                 Ok args__ ->
        --                     Box.line <| Box.row <| List.intersperse space <| formatUppercaseIdentifier tag :: args__
        --                 Err [] ->
        --                     Box.line <| formatUppercaseIdentifier tag
        --                 Err args__ ->
        --                     Box.stack1
        --                         [ Box.line <| formatUppercaseIdentifier tag
        --                         , Box.stack1 args__
        --                             |> indent
        --                         ]
        --     in
        --     case
        --         formatOpenCommentedList <| fmap ctor tags
        --     of
        --         [] ->
        --             error "List can't be empty"
        --         first :: rest ->
        --             case formatCommented <| fmap formatNameWithArgs nameWithArgs of
        --                 SingleLine nameWithArgs_ ->
        --                     Box.stack1
        --                         [ Box.line <|
        --                             Box.row
        --                                 [ Box.keyword "type"
        --                                 , space
        --                                 , nameWithArgs_
        --                                 ]
        --                         , first
        --                             |> prefix (Box.row [ punc "=", space ])
        --                             |> andThen (map (prefix (Box.row [ punc "|", space ])) rest)
        --                             |> indent
        --                         ]
        --                 nameWithArgs_ ->
        --                     Box.stack1
        --                         [ Box.line <| Box.keyword "type"
        --                         , Box.indent nameWithArgs_
        --                         , first
        --                             |> Box.prefix (Box.row [ Box.punc "=", Box.space ])
        --                             |> andThen (map (prefix (Box.row [ Box.punc "|", Box.space ])) rest)
        --                             |> Box.indent
        --                         ]
        -- TypeAlias preAlias nameWithArgs typ ->
        --     ElmStructure.definition "="
        --         True
        --         (Box.line (Box.keyword "type"))
        --         [ formatPreCommented (C preAlias (Box.line (Box.keyword "alias")))
        --         , formatCommented <| fmap formatNameWithArgs nameWithArgs
        --         ]
        --         (formatPreCommentedStack <| fmap (typeParens NotRequired << formatType) typ)
        -- Fixity assoc precedence name value ->
        --     let
        --         formatAssoc a =
        --             case a of
        --                 L ->
        --                     Box.keyword "left "
        --                 R ->
        --                     Box.keyword "right"
        --                 N ->
        --                     Box.keyword "non  "
        --     in
        --     ElmStructure.spaceSepOrIndented
        --         (Box.line (Box.keyword "infix"))
        --         [ formatPreCommented (fmap (Box.line << formatAssoc) assoc)
        --         , formatPreCommented (fmap (Box.line << Box.literal << show) precedence)
        --         , formatCommented (fmap (Box.line << formatSymbolIdentifierInParens) name)
        --         , Box.line (Box.keyword "=")
        --         , formatPreCommented (fmap (Box.line << Box.identifier << formatVarName) value)
        --         ]
        Decl.Value _ _ value ->
            formatCommonDeclaration importInfo value

        Decl.Union _ (A.At _ (Src.Union name args tags)) ->
            let
                tags_ : OpenCommentedList ( A.Located Name, List Src.Type )
                tags_ =
                    case tags of
                        firstTag :: restTags ->
                            OpenCommentedList (List.map (\tag -> ( ( [], [], Nothing ), tag )) restTags)
                                ( [], Nothing, firstTag )

                        _ ->
                            Debug.todo "tags"

                ctor : ( A.Located Name, List Src.Type ) -> Box
                ctor ( A.At _ tag, args_ ) =
                    case Box.allSingles (List.map (\arg -> formatPreCommented ( [], typeParens ForCtor (formatType arg) )) args_) of
                        Ok args__ ->
                            Box.line <| Box.row <| List.intersperse Box.space <| formatUppercaseIdentifier tag :: args__

                        Err [] ->
                            Box.line (formatUppercaseIdentifier tag)

                        Err args__ ->
                            Box.stack1
                                [ Box.line (formatUppercaseIdentifier tag)
                                , Box.stack1 args__
                                    |> Box.indent
                                ]
            in
            case formatOpenCommentedList (openCommentedListMap ctor tags_) of
                [] ->
                    crash "List can't be empty"

                first :: rest ->
                    -- TODO add comments surrounding name+args
                    case formatCommented ( [], formatNameWithArgs (A.toValue name) (List.map (\(A.At _ arg) -> ( [], arg )) args), [] ) of
                        Box.SingleLine nameWithArgs_ ->
                            Box.stack1
                                [ Box.line <|
                                    Box.row
                                        [ Box.keyword "type"
                                        , Box.space
                                        , nameWithArgs_
                                        ]
                                , first
                                    |> Box.prefix (Box.row [ Box.punc "=", Box.space ])
                                    |> Box.andThen (List.map (Box.prefix (Box.row [ Box.punc "|", Box.space ])) rest)
                                    |> Box.indent
                                ]

                        nameWithArgs_ ->
                            Box.stack1
                                [ Box.line <| Box.keyword "type"
                                , Box.indent nameWithArgs_
                                , first
                                    |> Box.prefix (Box.row [ Box.punc "=", Box.space ])
                                    |> Box.andThen (List.map (Box.prefix (Box.row [ Box.punc "|", Box.space ])) rest)
                                    |> Box.indent
                                ]

        Decl.Alias _ (A.At _ (Src.Alias (A.At _ name) args typ)) ->
            ElmStructure.definition "="
                True
                (Box.line (Box.keyword "type"))
                [ formatPreCommented ( [], Box.line (Box.keyword "alias") )
                , formatCommented ( [], formatNameWithArgs name (List.map (\(A.At _ arg) -> ( [], arg )) args), [] )
                ]
                (formatPreCommentedStack <| c1map (typeParens NotRequired << formatType) ( [], typ ))

        Decl.Port _ (Src.Port (A.At _ name) typ) ->
            let
                typeComments =
                    -- TODO
                    []
            in
            ElmStructure.definition ":"
                False
                (Box.line (Box.keyword "port"))
                [ formatCommented (c2map (Box.line << formatLowercaseIdentifier []) ( [], name, [] )) ]
                (formatCommentedApostrophe typeComments (typeParens NotRequired (formatType typ)))


formatNameWithArgs : Name -> List (C1 Name) -> Box
formatNameWithArgs name args =
    case Box.allSingles <| List.map (formatPreCommented << c1map (Box.line << formatLowercaseIdentifier [])) args of
        Ok args_ ->
            Box.line (Box.row (List.intersperse Box.space (formatUppercaseIdentifier name :: args_)))

        Err args_ ->
            Box.stack1
                ((Box.line <| formatUppercaseIdentifier name)
                    :: List.map Box.indent args_
                )


formatDefinition : ImportInfo -> Src.Pattern -> List Src.Pattern -> Comments -> Src.Expr -> Box
formatDefinition importInfo (A.At _ name) args comments expr =
    let
        body =
            Box.stack1
                (List.concat
                    [ List.map formatComment comments
                    , [ syntaxParens SyntaxSeparated (formatExpression importInfo expr) ]
                    ]
                )
    in
    ElmStructure.definition "="
        True
        (syntaxParens SpaceSeparated (formatPattern name))
        (List.map (\(A.At _ y) -> formatCommentedApostrophe [] (syntaxParens SpaceSeparated (formatPattern y))) args)
        body


formatTypeAnnotation : C1 (Ref ()) -> C1 Src.Type -> Box
formatTypeAnnotation name typ =
    ElmStructure.definition ":"
        False
        (formatTailCommented (c1map (Box.line << formatVar << refMap (\() -> [])) name))
        []
        (formatPreCommented (c1map (typeParens NotRequired << formatType) typ))


formatPattern : Src.Pattern_ -> ( SyntaxContext, Box )
formatPattern apattern =
    case apattern of
        --     Anything ->
        --         Tuple.pair SyntaxSeparated (line (keyword "_"))
        --     UnitPattern comments ->
        --         Tuple.pair SyntaxSeparated (formatUnit '(' ')' comments)
        --     LiteralPattern lit ->
        --         Tuple.pair SyntaxSeparated (formatLiteral lit)
        --     VarPattern var ->
        --         Tuple.pair SyntaxSeparated (line (formatLowercaseIdentifier [] var))
        --     OpPattern (SymbolIdentifier name) ->
        --         Tuple.pair SyntaxSeparated (line (identifier ("(" ++ name ++ ")")))
        --     ConsPattern first rest ->
        --         let
        --             formatRight (C ( preOp, postOp, eol ) term) =
        --                 ( False
        --                 , preOp
        --                 , line (punc "::")
        --                 , formatC2Eol
        --                     ((fmap <| syntaxParens SpaceSeparated << formatPattern )
        --                         (C ( postOp, [], eol ) term)
        --                     )
        --                 )
        --         in
        --         Tuple.pair SpaceSeparated
        --             (formatBinary False
        --                 (formatEolCommented <| fmap (syntaxParens SpaceSeparated << formatPattern ) first)
        --                 (formatRight (toCommentedList rest))
        --             )
        --     DataPattern ( ns, tag ) [] ->
        --         let
        --             ctor =
        --                 ns ++ [ tag ]
        --         in
        --         line (formatQualifiedUppercaseIdentifier ctor)
        --             |> Tuple.pair SyntaxSeparated
        --     DataPattern ( ns, tag ) patterns ->
        --         let
        --             ctor =
        --                 ns ++ [ tag ]
        --         in
        --         Tuple.pair SpaceSeparated
        --             (ElmStructure.application
        --                 (FAJoinFirst JoinAll)
        --                 (line (formatQualifiedUppercaseIdentifier  ctor))
        --                 (fmap (formatPreCommented << fmap (syntaxParens SpaceSeparated << formatPattern )) patterns)
        --             )
        --     PatternParens pattern ->
        --         formatCommented (fmap (syntaxParens SyntaxSeparated << formatPattern ) pattern)
        --             |> parens
        --             |> Tuple.pair SyntaxSeparated
        --     TuplePattern patterns ->
        --         Tuple.pair SyntaxSeparated <|
        --             ElmStructure.group True "(" "," ")" False <|
        --                 fmap (formatCommented << fmap (syntaxParens SyntaxSeparated << formatPattern )) patterns
        --     EmptyListPattern comments ->
        --         Tuple.pair SyntaxSeparated <|
        --             formatUnit '[' ']' comments
        --     ListPattern patterns ->
        --         Tuple.pair SyntaxSeparated <|
        --             ElmStructure.group True "[" "," "]" False <|
        --                 fmap (formatCommented << fmap (syntaxParens SyntaxSeparated << formatPattern )) patterns
        --     EmptyRecordPattern comments ->
        --         Tuple.pair SyntaxSeparated <|
        --             formatUnit '{' '}' comments
        --     RecordPattern fields ->
        --         Tuple.pair SyntaxSeparated <|
        --             ElmStructure.group True "{" "," "}" False <|
        --                 map (formatCommented << fmap (line << formatLowercaseIdentifier  [])) fields
        --     Alias pattern name ->
        --         Tuple.pair SpaceSeparated <|
        --             case
        --                 ( formatTailCommented <| fmap (syntaxParens SpaceSeparated << formatPattern ) pattern
        --                 , formatPreCommented <| fmap (line << formatLowercaseIdentifier  []) name
        --                 )
        --             of
        --                 ( SingleLine pattern_, SingleLine name_ ) ->
        --                     line <|
        --                         row
        --                             [ pattern_
        --                             , space
        --                             , keyword "as"
        --                             , space
        --                             , name_
        --                             ]
        --                 ( pattern_, name_ ) ->
        --                     stack1
        --                         [ pattern_
        --                         , line <| keyword "as"
        --                         , indent name_
        --                         ]
        Src.PAnything name ->
            ( SyntaxSeparated, Box.line (Box.identifier ("_" ++ name)) )

        Src.PVar name ->
            ( SyntaxSeparated, Box.line (formatLowercaseIdentifier [] name) )

        Src.PRecord fields ->
            Debug.todo "formatPattern.PRecord"

        Src.PAlias aliasPattern name ->
            Debug.todo "formatPattern.PAlias"

        Src.PUnit ->
            Debug.todo "formatPattern.PUnit"

        Src.PTuple a b cs ->
            Debug.todo "formatPattern.PTuple"

        Src.PCtor nameRegion name patterns ->
            Debug.todo "formatPattern.PCtor"

        Src.PCtorQual nameRegion home name patterns ->
            Debug.todo "formatPattern.PCtorQual"

        Src.PList patterns ->
            Debug.todo "formatPattern.PList"

        Src.PCons hd tl ->
            Debug.todo "formatPattern.PCons"

        Src.PChr chr ->
            Debug.todo "formatPattern.PChr"

        Src.PStr str ->
            Debug.todo "formatPattern.PStr"

        Src.PInt int ->
            Debug.todo "formatPattern.PInt"


formatRecordPair : String -> (v -> Box) -> ( ( List Space.Comment, String, List Space.Comment ), ( List Space.Comment, v, List Space.Comment ), Bool ) -> Box
formatRecordPair delim formatValue ( ( pre, k, postK ), ( preV, v, postV ), forceMultiline ) =
    formatPreCommented
        ( pre
        , ElmStructure.equalsPair delim
            forceMultiline
            (formatCommented ( [], Box.line (formatLowercaseIdentifier [] k), postK ))
            (formatCommented ( preV, formatValue v, postV ))
        )


formatPair : String -> Pair Box.Line Box -> Box
formatPair delim (Pair a b (ForceMultiline forceMultiline)) =
    ElmStructure.equalsPair delim
        forceMultiline
        (formatTailCommented <| c1map Box.line a)
        (formatPreCommented b)



-- negativeCasePatternWorkaround : ASTNS annf (List UppercaseIdentifier) PatternNK -> Box -> Box
-- negativeCasePatternWorkaround pattern =
--     case extract <| I.unFix pattern of
--         LiteralPattern (IntNum i _) ->
--             if i < 0 then
--                 parens
--             else
--                 id
--         LiteralPattern (FloatNum f _) ->
--             if f < 0 then
--                 parens
--             else
--                 id
--         _ ->
--             id


type SyntaxContext
    = SyntaxSeparated
    | InfixSeparated
    | SpaceSeparated
    | AmbiguousEnd


syntaxParens : SyntaxContext -> ( SyntaxContext, Box ) -> Box
syntaxParens outer ( inner, box ) =
    let
        parensIf bool =
            if bool then
                parens

            else
                identity
    in
    parensIf (needsParensInContext inner outer) box


needsParensInContext : SyntaxContext -> SyntaxContext -> Bool
needsParensInContext inner outer =
    case ( inner, outer ) of
        ( SpaceSeparated, SpaceSeparated ) ->
            True

        ( InfixSeparated, SpaceSeparated ) ->
            True

        ( InfixSeparated, InfixSeparated ) ->
            True

        ( AmbiguousEnd, SpaceSeparated ) ->
            True

        ( AmbiguousEnd, InfixSeparated ) ->
            True

        ( InfixSeparated, AmbiguousEnd ) ->
            True

        _ ->
            False


formatExpression : ImportInfo -> Src.Expr -> ( SyntaxContext, Box )
formatExpression importInfo (A.At _ aexpr) =
    case aexpr of
        --     Literal lit ->
        --         Tuple.pair SyntaxSeparated <| formatLiteral lit
        --     VarExpr v ->
        --         Tuple.pair SyntaxSeparated <| line <| formatVar v
        --     Range left right multiline ->
        --         formatRange_0_18 importInfo left right
        --     ExplicitList exprs trailing multiline ->
        --         Tuple.pair SyntaxSeparated <|
        --             formatSequence '['
        --                 ','
        --                 (Just ']')
        --                 multiline
        --                 trailing
        --                 (syntaxParens SyntaxSeparated << formatExpression importInfo exprs)
        --     Binops left ops multiline ->
        --         Tuple.pair InfixSeparated <|
        --             formatBinops importInfo left ops multiline
        --     Lambda patterns bodyComments expr multiline ->
        --         Tuple.pair AmbiguousEnd <|
        --             case
        --                 ( multiline
        --                 , allSingles <| fmap (formatPreCommented << fmap (syntaxParens SpaceSeparated << formatPattern)) patterns
        --                 , bodyComments == []
        --                 , syntaxParens SyntaxSeparated <| formatExpression importInfo expr
        --                 )
        --             of
        --                 ( False, Right patterns_, True, SingleLine expr_ ) ->
        --                     line <|
        --                         row
        --                             [ punc "\\"
        --                             , row <| List.intersperse space patterns_
        --                             , space
        --                             , punc "->"
        --                             , space
        --                             , expr_
        --                             ]
        --                 ( _, Right patterns_, _, expr_ ) ->
        --                     stack1
        --                         [ line <|
        --                             row
        --                                 [ punc "\\"
        --                                 , row (List.intersperse space patterns_)
        --                                 , space
        --                                 , punc "->"
        --                                 ]
        --                         , indent <|
        --                             stack1 <|
        --                                 fmap formatComment bodyComments
        --                                     ++ [ expr_ ]
        --                         ]
        --                 ( _, Left [], _, _ ) ->
        --                     pleaseReport "UNEXPECTED LAMBDA" "no patterns"
        --                 ( _, Left patterns_, _, expr_ ) ->
        --                     stack1
        --                         [ prefix (punc "\\") <| stack1 patterns_
        --                         , line <| punc "->"
        --                         , indent <|
        --                             stack1 <|
        --                                 fmap formatComment bodyComments
        --                                     ++ [ expr_ ]
        --                         ]
        --     Unary Negative e ->
        --         Tuple.pair SyntaxSeparated <|
        --             prefix (punc "-") <|
        --                 syntaxParens SpaceSeparated <|
        --                     formatExpression importInfo e
        --     If if_ elseifs (C elsComments els) ->
        --         let
        --             opening key cond =
        --                 case ( key, cond ) of
        --                     ( SingleLine key_, SingleLine cond_ ) ->
        --                         line <|
        --                             row
        --                                 [ key_
        --                                 , space
        --                                 , cond_
        --                                 , space
        --                                 , keyword "then"
        --                                 ]
        --                     _ ->
        --                         stack1
        --                             [ key
        --                             , cond |> indent
        --                             , line <| keyword "then"
        --                             ]
        --             formatIf (IfClause cond body) =
        --                 stack1
        --                     [ opening (line <| keyword "if") <| formatCommentedExpression importInfo cond
        --                     , indent <| formatCommented_ True <| fmap (syntaxParens SyntaxSeparated << formatExpression importInfo) body
        --                     ]
        --             formatElseIf (C ifComments (IfClause cond body)) =
        --                 let
        --                     key =
        --                         case formatPreCommented (C ifComments <| line <| keyword "if") of
        --                             SingleLine key_ ->
        --                                 line <| row [ keyword "else", space, key_ ]
        --                             key_ ->
        --                                 stack1
        --                                     [ line <| keyword "else"
        --                                     , key_
        --                                     ]
        --                 in
        --                 stack1
        --                     [ blankLine
        --                     , opening key <| formatCommentedExpression importInfo cond
        --                     , indent <| formatCommented_ True <| fmap (syntaxParens SyntaxSeparated << formatExpression importInfo) body
        --                     ]
        --         in
        --         Tuple.pair AmbiguousEnd <|
        --             formatIf if_
        --                 |> andThen (fmap formatElseIf elseifs)
        --                 |> andThen
        --                     [ blankLine
        --                     , line <| keyword "else"
        --                     , indent <| formatCommented_ True <| fmap (syntaxParens SyntaxSeparated << formatExpression importInfo) (C ( elsComments, [] ) els)
        --                     ]
        --     Let defs bodyComments expr ->
        --         let
        --             spacer : AST typeRef ctorRef varRef (I.Fix Identity (AST typeRef ctorRef varRef)) LetDeclarationNK -> AST typeRef ctorRef varRef getType LetDeclarationNK -> List Box
        --             spacer first _ =
        --                 case first of
        --                     LetCommonDeclaration (I.Fix (Identity (Definition _ _ _ _))) ->
        --                         [ blankLine ]
        --                     _ ->
        --                         []
        --             formatDefinition_ def =
        --                 case def of
        --                     LetCommonDeclaration (I.Fix (Identity (Definition name args comments expr_))) ->
        --                         formatDefinition importInfo name args comments expr_
        --                     LetCommonDeclaration (I.Fix (Identity (TypeAnnotation name typ))) ->
        --                         formatTypeAnnotation name typ
        --                     LetComment comment ->
        --                         formatComment comment
        --         in
        --         Tuple.pair AmbiguousEnd <|
        --             -- TODO: not tested
        --             line (keyword "let")
        --                 |> andThen
        --                     (defs
        --                         |> fmap (extract << I.unFix)
        --                         |> intersperseMap spacer formatDefinition_
        --                         |> map indent
        --                     )
        --                 |> andThen
        --                     [ line <| keyword "in"
        --                     , stack1 <|
        --                         fmap formatComment bodyComments
        --                             ++ [ syntaxParens SyntaxSeparated <| formatExpression importInfo expr ]
        --                     ]
        --     Case ( subject, multiline ) clauses ->
        --         let
        --             opening =
        --                 case
        --                     ( multiline
        --                     , formatCommentedExpression importInfo subject
        --                     )
        --                 of
        --                     ( False, SingleLine subject_ ) ->
        --                         line <|
        --                             row
        --                                 [ keyword "case"
        --                                 , space
        --                                 , subject_
        --                                 , space
        --                                 , keyword "of"
        --                                 ]
        --                     ( _, subject_ ) ->
        --                         stack1
        --                             [ line <| keyword "case"
        --                             , indent subject_
        --                             , line <| keyword "of"
        --                             ]
        --             clause (CaseBranch prePat postPat preExpr pat expr) =
        --                 case
        --                     ( postPat
        --                     , formatPattern pat
        --                         |> syntaxParens SyntaxSeparated
        --                         |> negativeCasePatternWorkaround pat
        --                     , formatCommentedStack (fmap (syntaxParens SyntaxSeparated << formatPattern) (C ( prePat, postPat ) pat))
        --                         |> negativeCasePatternWorkaround pat
        --                     , formatPreCommentedStack <| fmap (syntaxParens SyntaxSeparated << formatExpression importInfo) (C preExpr expr)
        --                     )
        --                 of
        --                     ( _, _, SingleLine pat_, body_ ) ->
        --                         stack1
        --                             [ line (row [ pat_, space, keyword "->" ])
        --                             , indent body_
        --                             ]
        --                     ( [], SingleLine pat_, _, body_ ) ->
        --                         stack1
        --                             (fmap formatComment prePat
        --                                 ++ [ line (row [ pat_, space, keyword "->" ])
        --                                    , indent body_
        --                                    ]
        --                             )
        --                     ( _, _, pat_, body_ ) ->
        --                         stack1
        --                             [ pat_
        --                             , line (keyword "->")
        --                             , indent body_
        --                             ]
        --         in
        --         Tuple.pair AmbiguousEnd <|
        --             -- TODO: not tested
        --             opening
        --                 |> andThen
        --                     (clauses
        --                         |> fmap (clause << extract << I.unFix)
        --                         |> List.intersperse blankLine
        --                         |> map indent
        --                     )
        --     Tuple exprs multiline ->
        --         Tuple.pair SyntaxSeparated <|
        --             ElmStructure.group True "(" "," ")" multiline <|
        --                 map (formatCommentedExpression importInfo) exprs
        --     TupleFunction n ->
        --         Tuple.pair SyntaxSeparated <|
        --             line <|
        --                 keyword <|
        --                     "("
        --                         ++ List.replicate (n - 1) ','
        --                         ++ ")"
        --     Access expr field ->
        --         Tuple.pair SyntaxSeparated <|
        --             formatExpression importInfo expr
        --                 |> syntaxParens SpaceSeparated
        --                 -- TODO: does this need a different context than SpaceSeparated?
        --                 |> addSuffix (row <| [ punc ".", formatLowercaseIdentifier [] field ])
        --     AccessFunction (LowercaseIdentifier field) ->
        --         Tuple.pair SyntaxSeparated <|
        --             line <|
        --                 identifier <|
        --                     "."
        --                         ++ formatVarName_ field
        --     Record base fields trailing multiline ->
        --         Tuple.pair SyntaxSeparated <|
        --             formatRecordLike
        --                 (fmap (line << formatLowercaseIdentifier []) base)
        --                 (fmap (formatPair "=" << mapPair (formatLowercaseIdentifier []) (syntaxParens SyntaxSeparated << formatExpression importInfo)) fields)
        --                 trailing
        --                 multiline
        --     Parens expr ->
        --         case expr of
        --             C ( [], [] ) expr_ ->
        --                 formatExpression importInfo expr_
        --             _ ->
        --                 Tuple.pair SyntaxSeparated <|
        --                     formatCommentedExpression importInfo expr
        --                         |> parens
        --     Unit comments ->
        --         Tuple.pair SyntaxSeparated <|
        --             formatUnit '(' ')' comments
        --     GLShader src ->
        --         Tuple.pair SyntaxSeparated <|
        --             line <|
        --                 row
        --                     [ punc "[glsl|"
        --                     , literal src
        --                     , punc "|]"
        --                     ]
        Src.Chr char ->
            ( SyntaxSeparated, formatLiteral (Chr char) )

        Src.Str string ->
            -- TODO SingleQuotedString
            ( SyntaxSeparated, formatLiteral (Str string SingleQuotedString) )

        Src.Int int ->
            -- TODO HexadecimalInt
            ( SyntaxSeparated, formatLiteral (IntNum int DecimalInt) )

        Src.Float float ->
            Debug.todo "formatExpression.Float"

        Src.Var Src.LowVar name ->
            ( SyntaxSeparated, Box.line (formatVar (VarRef [] name)) )

        Src.Var Src.CapVar name ->
            ( SyntaxSeparated, Box.line (formatVar (TagRef [] name)) )

        Src.VarQual Src.LowVar prefix name ->
            ( SyntaxSeparated, Box.line (formatVar (VarRef (String.split "." prefix) name)) )

        Src.VarQual Src.CapVar prefix name ->
            ( SyntaxSeparated, Box.line (formatVar (TagRef (String.split "." prefix) name)) )

        Src.List list ->
            let
                multiline : ForceMultiline
                multiline =
                    -- TODO
                    ForceMultiline False

                trailing : Comments
                trailing =
                    []

                exprs =
                    List.map (\expr -> ( ( [], [], Nothing ), expr )) list
            in
            ( SyntaxSeparated
            , formatSequence '['
                ','
                (Just ']')
                multiline
                trailing
                (List.map (c2EolMap (syntaxParens SyntaxSeparated << formatExpression importInfo)) exprs)
            )

        Src.Op op ->
            Debug.todo "formatExpression.Op"

        Src.Negate expr ->
            Debug.todo "formatExpression.Negate"

        Src.Binops ops final ->
            -- Binops left ops multiline ->
            let
                ( left, clauses ) =
                    List.foldr
                        (\( currExpr, A.At _ currOp ) ( leftAcc, clausesAcc ) ->
                            ( currExpr, BinopsClause [] (OpRef currOp) [] leftAcc :: clausesAcc )
                        )
                        ( final, [] )
                        ops

                multiline =
                    -- TODO
                    False
            in
            ( InfixSeparated
            , formatBinops importInfo left clauses multiline
            )

        Src.Lambda srcArgs expr ->
            -- Lambda patterns bodyComments expr multiline ->
            let
                patterns =
                    List.map (\srcArg -> ( [], srcArg )) srcArgs

                bodyComments =
                    -- TODO
                    []

                multiline =
                    -- TODO
                    False
            in
            ( AmbiguousEnd
            , case
                ( ( multiline
                  , Box.allSingles <| List.map (formatPreCommented << c1map (syntaxParens SpaceSeparated << formatPattern << A.toValue)) patterns
                  )
                , ( bodyComments == []
                  , syntaxParens SyntaxSeparated <| formatExpression importInfo expr
                  )
                )
              of
                ( ( False, Ok patterns_ ), ( True, Box.SingleLine expr_ ) ) ->
                    Box.line <|
                        Box.row
                            [ Box.punc "\\"
                            , Box.row <| List.intersperse Box.space patterns_
                            , Box.space
                            , Box.punc "->"
                            , Box.space
                            , expr_
                            ]

                ( ( _, Ok patterns_ ), ( _, expr_ ) ) ->
                    Box.stack1
                        [ Box.line <|
                            Box.row
                                [ Box.punc "\\"
                                , Box.row (List.intersperse Box.space patterns_)
                                , Box.space
                                , Box.punc "->"
                                ]
                        , Box.indent <|
                            Box.stack1 <|
                                List.map formatComment bodyComments
                                    ++ [ expr_ ]
                        ]

                ( ( _, Err [] ), ( _, _ ) ) ->
                    pleaseReport "UNEXPECTED LAMBDA" "no patterns"

                ( ( _, Err patterns_ ), ( _, expr_ ) ) ->
                    Box.stack1
                        [ Box.prefix (Box.punc "\\") <| Box.stack1 patterns_
                        , Box.line <| Box.punc "->"
                        , Box.indent <|
                            Box.stack1 <|
                                List.map formatComment bodyComments
                                    ++ [ expr_ ]
                        ]
            )

        Src.Call func [] ->
            -- App left [] _ ->
            let
                left =
                    func
            in
            formatExpression importInfo left

        Src.Call func args_ ->
            -- TODO: This might need something stronger than SpaceSeparated?
            -- App left args multiline ->
            let
                left =
                    func

                args =
                    List.map (\arg -> ( [], arg )) args_

                multiline =
                    -- TODO
                    ElmStructure.FASplitFirst
            in
            ( SpaceSeparated
            , ElmStructure.application
                multiline
                (syntaxParens InfixSeparated <| formatExpression importInfo left)
                (List.map (formatPreCommentedExpression importInfo SpaceSeparated) args)
            )

        Src.If branches finally ->
            Debug.todo "formatExpression.If"

        Src.Let defs expr ->
            --     Let defs bodyComments expr ->
            let
                bodyComments : Comments
                bodyComments =
                    -- TODO
                    []

                spacer : A.Located Src.Def -> A.Located Src.Def -> List Box
                spacer first _ =
                    -- case first of
                    --     LetCommonDeclaration (I.Fix (Identity (Definition _ _ _ _))) ->
                    --         [ blankLine ]
                    --     _ ->
                    --         []
                    Debug.todo "spacer"

                formatDefinition_ : A.Located Src.Def -> Box
                formatDefinition_ (A.At _ def) =
                    case def of
                        --     LetCommonDeclaration (I.Fix (Identity (Definition name args comments expr_))) ->
                        --         formatDefinition importInfo name args comments expr_
                        --     LetCommonDeclaration (I.Fix (Identity (TypeAnnotation name typ))) ->
                        --         formatTypeAnnotation name typ
                        --     LetComment comment ->
                        --         formatComment comment
                        Src.Define (A.At nameRegion name) srcArgs body maybeType ->
                            let
                                comments =
                                    -- TODO
                                    []
                            in
                            formatDefinition importInfo (A.At nameRegion (Src.PVar name)) srcArgs comments body

                        Src.Destruct pattern body ->
                            let
                                comments =
                                    -- TODO
                                    []
                            in
                            formatDefinition importInfo pattern [] comments body
            in
            ( AmbiguousEnd
            , -- TODO: not tested
              Box.line (Box.keyword "let")
                |> Box.andThen
                    (defs
                        |> intersperseMap spacer formatDefinition_
                        |> List.map Box.indent
                    )
                |> Box.andThen
                    [ Box.line (Box.keyword "in")
                    , Box.stack1 <|
                        List.map formatComment bodyComments
                            ++ [ syntaxParens SyntaxSeparated <| formatExpression importInfo expr ]
                    ]
            )

        Src.Case expr branches ->
            Debug.todo "formatExpression.Case"

        Src.Accessor field ->
            Debug.todo "formatExpression.Accessor"

        Src.Access record field ->
            Debug.todo "formatExpression.Access"

        Src.Update name fields ->
            Debug.todo "formatExpression.Update"

        Src.Record fields ->
            Debug.todo "formatExpression.Record"

        Src.Unit ->
            let
                -- TODO
                comments =
                    []
            in
            ( SyntaxSeparated
            , formatUnit '(' ')' comments
            )

        Src.Tuple a b cs ->
            let
                multiline =
                    -- TODO
                    False

                exprs =
                    ( [], a, [] ) :: ( [], b, [] ) :: List.map (\c -> ( [], c, [] )) cs
            in
            Tuple.pair SyntaxSeparated <|
                ElmStructure.group True "(" "," ")" multiline <|
                    List.map (formatCommentedExpression importInfo) exprs

        Src.Shader src tipe ->
            Debug.todo "formatExpression.Shader"


formatCommentedExpression : ImportInfo -> C2 Src.Expr -> Box
formatCommentedExpression importInfo ( pre, e, post ) =
    let
        commented_ =
            -- TODO
            -- case e of
            --     Src.Parens (C ( pre__, post__ ) e__) ->
            --         ( pre ++ pre__, e__, post__ ++ post )
            --     _ ->
            ( pre, e, post )
    in
    formatCommented <| c2map (syntaxParens SyntaxSeparated << formatExpression importInfo) commented_


formatPreCommentedExpression : ImportInfo -> SyntaxContext -> C1 Src.Expr -> Box
formatPreCommentedExpression importInfo context ( pre, e ) =
    let
        ( pre_, e_ ) =
            -- TODO
            -- case e of
            --     Parens (C ( pre__, [] ) e__) ->
            --         ( pre ++ pre__, e__ )
            --     _ ->
            ( pre, e )
    in
    formatCommentedApostrophe pre_ (syntaxParens context <| formatExpression importInfo e_)


formatRecordLike : Maybe (C2 Box) -> List (C2Eol Box) -> Comments -> ForceMultiline -> Box
formatRecordLike base_ fields trailing multiline =
    case ( base_, fields ) of
        ( Just base, pairs_ ) ->
            ElmStructure.extensionGroup_
                ((\(ForceMultiline b) -> b) multiline)
                (formatCommented base)
                (formatSequence '|'
                    ','
                    Nothing
                    multiline
                    trailing
                    pairs_
                )

        ( Nothing, pairs_ ) ->
            formatSequence '{'
                ','
                (Just '}')
                multiline
                trailing
                pairs_


formatSequence : Char -> Char -> Maybe Char -> ForceMultiline -> Comments -> List (C2Eol Box) -> Box
formatSequence left delim maybeRight (ForceMultiline multiline) trailing list =
    case ( left, maybeRight, list ) of
        ( _, _, first :: rest ) ->
            let
                formatItem : Char -> C2Eol Box -> Box
                formatItem delim_ ( ( pre, post, eol ), item ) =
                    Maybe.unwrap identity (Box.stack_ << Box.stack_ Box.blankLine) (formatComments pre) <|
                        Box.prefix (Box.row [ Box.punc (String.fromChar delim_), Box.space ]) <|
                            formatC2Eol <|
                                ( ( post, [], eol ), item )
            in
            ElmStructure.forceableSpaceSepOrStack multiline
                (ElmStructure.forceableRowOrStack multiline
                    (formatItem left first)
                    (List.map (formatItem delim) rest)
                )
                (Maybe.unwrap [] (flip (::) [] << Box.stack_ Box.blankLine) (formatComments trailing) ++ Maybe.toList (Maybe.map (Box.line << Box.punc << String.fromChar) maybeRight))

        ( _, Just right, [] ) ->
            formatUnit left right trailing

        ( _, Nothing, [] ) ->
            formatUnit left ' ' trailing


mapIsLast : (Bool -> a -> b) -> List a -> List b
mapIsLast f l =
    case l of
        [] ->
            []

        [ last_ ] ->
            [ f True last_ ]

        next :: rest ->
            f False next :: mapIsLast f rest


type BinopsClause varRef expr
    = BinopsClause Comments varRef Comments expr


formatBinops : ImportInfo -> Src.Expr -> List (BinopsClause (Ref (List String)) Src.Expr) -> Bool -> Box
formatBinops importInfo left ops multiline =
    let
        formatPair_ : Bool -> BinopsClause (Ref (List String)) Src.Expr -> ( ( Bool, Comments, Box ), Box )
        formatPair_ isLast (BinopsClause po o pe e) =
            let
                isLeftPipe =
                    o == OpRef "<|"

                formatContext =
                    if isLeftPipe && isLast then
                        AmbiguousEnd

                    else
                        InfixSeparated
            in
            ( ( isLeftPipe
              , po
              , (Box.line << formatInfixVar) o
              )
            , formatCommentedApostrophe pe <| syntaxParens formatContext <| formatExpression importInfo e
            )
    in
    formatBinary
        multiline
        (syntaxParens InfixSeparated <| formatExpression importInfo left)
        (mapIsLast formatPair_ ops)



-- nowhere : A.Position
-- nowhere =
--     A.Position 0 0
-- noRegion : a -> A.Located a
-- noRegion =
--     A.at nowhere nowhere
-- formatRange_0_18 :
--     ImportInfo (List UppercaseIdentifier)
--     -> C2 before after (ASTNS annf (List UppercaseIdentifier) ExpressionNK)
--     -> C2 before after (ASTNS annf (List UppercaseIdentifier) ExpressionNK)
--     -> ( SyntaxContext, Box )
-- formatRange_0_18 importInfo left right =
--     case ( left, right ) of
--         ( C ( preLeft, [] ) left_, C ( preRight, [] ) right_ ) ->
--             App
--                 (I.Fix <| Identity <| VarExpr <| VarRef [ UppercaseIdentifier "List" ] <| LowercaseIdentifier "range")
--                 [ C preLeft <| I.convert (pure << extract) left_
--                 , C preRight <| I.convert (pure << extract) right_
--                 ]
--                 (FAJoinFirst JoinAll)
--                 |> (I.Fix << pure)
--                 |> formatExpression importInfo
--         _ ->
--             App
--                 (I.Fix <| Identity <| VarExpr <| VarRef [ UppercaseIdentifier "List" ] <| LowercaseIdentifier "range")
--                 [ C [] <| I.Fix <| pure <| Parens <| fmap (I.convert (pure << extract)) left
--                 , C [] <| I.Fix <| pure <| Parens <| fmap (I.convert (pure << extract)) right
--                 ]
--                 (FAJoinFirst JoinAll)
--                 |> (I.Fix << pure)
--                 |> formatExpression importInfo


formatUnit : Char -> Char -> Comments -> Box
formatUnit left right comments =
    case ( left, comments ) of
        ( _, [] ) ->
            Box.line <| Box.punc (String.fromList [ left, right ])

        ( '{', (Space.LineComment _) :: _ ) ->
            surround left right <| Box.prefix Box.space <| Box.stack1 <| List.map formatComment comments

        _ ->
            surround left right <|
                case Box.allSingles <| List.map formatComment comments of
                    Ok comments_ ->
                        Box.line <| Box.row <| List.intersperse Box.space comments_

                    Err comments_ ->
                        Box.stack1 comments_


formatComments : Comments -> Maybe Box
formatComments comments =
    case List.map formatComment comments of
        [] ->
            Nothing

        first :: rest ->
            Just (ElmStructure.spaceSepOrStack first rest)


formatCommented_ : Bool -> C2 Box -> Box
formatCommented_ forceMultiline ( pre, inner, post ) =
    ElmStructure.forceableSpaceSepOrStack1 forceMultiline <|
        List.concat
            [ Maybe.toList (formatComments pre)
            , [ inner ]
            , Maybe.toList (formatComments post)
            ]


formatCommented : C2 Box -> Box
formatCommented =
    formatCommented_ False


formatPreCommented : C1 Box -> Box
formatPreCommented ( pre, inner ) =
    formatCommentedApostrophe pre inner


formatCommentedApostrophe : Comments -> Box -> Box
formatCommentedApostrophe pre inner =
    formatCommented ( pre, inner, [] )


formatTailCommented : C1 Box -> Box
formatTailCommented ( post, inner ) =
    formatCommented ( [], inner, post )


formatC2Eol : C2Eol Box -> Box
formatC2Eol ( ( pre, post, eol ), a ) =
    formatCommented ( pre, formatEolCommented ( eol, a ), post )


formatEolCommented : ( Maybe String, Box ) -> Box
formatEolCommented ( post, inner ) =
    case ( post, inner ) of
        ( Nothing, box ) ->
            box

        ( Just eol, Box.SingleLine result ) ->
            Box.mustBreak <| Box.row [ result, Box.space, Box.punc "--", Box.literal eol ]

        ( Just eol, box ) ->
            Box.stack1 [ box, formatComment <| Space.LineComment eol ]


formatCommentedStack : C2 Box -> Box
formatCommentedStack ( pre, inner, post ) =
    Box.stack1 <|
        List.map formatComment pre
            ++ inner
            :: List.map formatComment post


formatPreCommentedStack : C1 Box -> Box
formatPreCommentedStack ( pre, inner ) =
    formatCommentedStack ( pre, inner, [] )


formatKeywordCommented : String -> ( List Space.Comment, Box, List Space.Comment ) -> Box
formatKeywordCommented word ( pre, value, post ) =
    ElmStructure.spaceSepOrIndented
        (formatCommented ( pre, Box.line (Box.keyword word), post ))
        [ value ]


formatOpenCommentedList : OpenCommentedList Box -> List Box
formatOpenCommentedList (OpenCommentedList rest ( preLst, eol, lst )) =
    List.map formatC2Eol rest
        ++ [ formatC2Eol ( ( preLst, [], eol ), lst ) ]


formatComment : Space.Comment -> Box
formatComment comment =
    case comment of
        Space.BlockComment c ->
            case c of
                [] ->
                    Box.line <| Box.punc "{- -}"

                [ l ] ->
                    Box.line <|
                        Box.row
                            [ Box.punc "{-"
                            , Box.space
                            , Box.literal l
                            , Box.space
                            , Box.punc "-}"
                            ]

                ls ->
                    Box.stack1
                        [ Box.prefix
                            (Box.row [ Box.punc "{-", Box.space ])
                            (Box.stack1 <| List.map (Box.line << Box.literal) ls)
                        , Box.line <| Box.punc "-}"
                        ]

        Space.LineComment c ->
            Box.mustBreak <| Box.row [ Box.punc "--", Box.literal c ]

        Space.CommentTrickOpener ->
            Box.mustBreak <| Box.punc "{--}"

        Space.CommentTrickCloser ->
            Box.mustBreak <| Box.punc "--}"

        Space.CommentTrickBlock c ->
            Box.mustBreak <| Box.row [ Box.punc "{--", Box.literal c, Box.punc "-}" ]


type IntRepresentation
    = DecimalInt
    | HexadecimalInt


type FloatRepresentation
    = DecimalFloat
    | ExponentFloat


type StringRepresentation
    = SingleQuotedString
    | TripleQuotedString


type LiteralValue
    = IntNum Int IntRepresentation
    | FloatNum Float FloatRepresentation
    | Chr String
    | Str String StringRepresentation
    | Boolean Bool


formatLiteral : LiteralValue -> Box
formatLiteral lit =
    case lit of
        IntNum i DecimalInt ->
            Box.line <| Box.literal <| String.fromInt i

        IntNum i HexadecimalInt ->
            Box.line <|
                Box.literal <|
                    Hex.toString i

        FloatNum f DecimalFloat ->
            Box.line <| Box.literal <| String.fromFloat f

        FloatNum f ExponentFloat ->
            -- Box.line <| Box.literal <| printf "%e" f
            Debug.todo "FloatNum f ExponentFloat"

        Chr c ->
            formatString SChar c

        Str s multi ->
            formatString (SString multi) s

        Boolean True ->
            Box.line <| Box.literal "True"

        Boolean False ->
            Box.line <| Box.literal "False"


type StringStyle
    = SChar
    | SString StringRepresentation


charIsPrint : Char -> Bool
charIsPrint c =
    -- TODO
    False


charIsSpace : Char -> Bool
charIsSpace c =
    let
        uc =
            Char.toCode c
    in
    if uc <= 0x0377 then
        uc == 32 || uc - 0x09 <= 4 || uc == 0xA0

    else
        c == ' '


formatString : StringStyle -> String -> Box
formatString style s =
    let
        stringBox quotes escaper =
            Box.line <|
                Box.row
                    [ Box.punc quotes
                    , Box.literal <| escaper <| String.concat <| List.map fix <| String.toList s
                    , Box.punc quotes
                    ]

        fix c =
            if (style == SString TripleQuotedString) && c == '\n' then
                String.fromChar c

            else if c == '\n' then
                "\\n"

            else if c == '\t' then
                "\\t"

            else if c == '\\' then
                "\\\\"

            else if (style == SString SingleQuotedString) && c == '"' then
                "\\\""

            else if (style == SChar) && c == '\'' then
                "\\'"

            else if not <| charIsPrint c then
                hex c

            else if c == ' ' then
                String.fromChar c

            else if charIsSpace c then
                hex c

            else
                String.fromChar c

        hex char =
            -- "\\u{" ++ (printf "%04X" <| Char.toCode char) ++ "}"
            Debug.todo "hex char"

        escapeMultiQuote =
            let
                step : String -> Int -> String -> String
                step okay quotes remaining =
                    case String.toList remaining of
                        [] ->
                            String.reverse (String.concat (List.repeat quotes "\"\\") ++ okay)

                        next :: rest ->
                            if next == '"' then
                                step okay (quotes + 1) (String.fromList rest)

                            else if quotes >= 3 then
                                step (String.cons next (String.concat <| List.repeat quotes "\"\\") ++ okay) 0 (String.fromList rest)

                            else if quotes > 0 then
                                step (String.cons next (String.fromList (List.repeat quotes '"') ++ okay)) 0 (String.fromList rest)

                            else
                                step (String.cons next okay) 0 (String.fromList rest)
            in
            step "" 0
    in
    case style of
        SChar ->
            stringBox "'" identity

        SString SingleQuotedString ->
            stringBox "\"" identity

        SString TripleQuotedString ->
            stringBox "\"\"\"" escapeMultiQuote


type TypeParensRequired
    = {- 0 -} NotRequired
    | {- 1 -} ForLambda
    | {- 2 -} ForCtor


type TypeParensInner
    = NotNeeded
    | ForFunctionType
    | ForTypeConstruction


typeParens : TypeParensRequired -> ( TypeParensInner, Box ) -> Box
typeParens outer ( inner, box ) =
    if typeParensNeeded outer inner then
        parens box

    else
        box


typeParensNeeded : TypeParensRequired -> TypeParensInner -> Bool
typeParensNeeded outer typeParensInner =
    case typeParensInner of
        NotNeeded ->
            False

        ForTypeConstruction ->
            -- outer >= ForCtor
            outer == ForCtor

        ForFunctionType ->
            -- outer >= ForLambda
            outer == ForLambda || outer == ForCtor



-- commaSpace : Line
-- commaSpace =
--     row
--         [ punc ","
--         , space
--         ]
-- formatTypeConstructor : TypeConstructor ( List UppercaseIdentifier, UppercaseIdentifier ) -> Box
-- formatTypeConstructor ctor =
--     case ctor of
--         NamedConstructor ( namespace, name ) ->
--             line <| formatQualifiedUppercaseIdentifier (namespace ++ [ name ])
--         TupleConstructor n ->
--             line <| keyword <| "(" ++ List.replicate (n - 1) ',' ++ ")"


formatType : Src.Type -> ( TypeParensInner, Box )
formatType (A.At _ atype) =
    case atype of
        -- FunctionType first rest (ForceMultiline forceMultiline) ->
        --     let
        --         formatRight (C ( preOp, postOp, eol ) term) =
        --             ElmStructure.forceableSpaceSepOrStack1
        --                 False
        --             <|
        --                 concat
        --                     [ Maybe.maybeToList <| formatComments preOp
        --                     , [ ElmStructure.prefixOrIndented
        --                             (line <| punc "->")
        --                             (formatC2Eol <|
        --                                 (fmap <| typeParens ForLambda << formatType)
        --                                     (C ( postOp, [], eol ) term)
        --                             )
        --                       ]
        --                     ]
        --     in
        --     Tuple.pair ForFunctionType <|
        --         ElmStructure.forceableSpaceSepOrStack
        --             forceMultiline
        --             (formatEolCommented (typeParens ForLambda << formatType first))
        --             (formatRight (toCommentedList rest))
        -- TypeVariable var ->
        --     Tuple.pair NotNeeded <|
        --         line <|
        --             identifier <|
        --                 formatVarName var
        -- TypeConstruction ctor args forceMultiline ->
        --     let
        --         join =
        --             case forceMultiline of
        --                 ForceMultiline True ->
        --                     FASplitFirst
        --                 ForceMultiline False ->
        --                     FAJoinFirst JoinAll
        --     in
        --     Tuple.pair
        --         (if null args then
        --             NotNeeded
        --          else
        --             ForTypeConstruction
        --         )
        --     <|
        --         ElmStructure.application
        --             join
        --             (formatTypeConstructor ctor)
        --             (fmap (formatPreCommented << fmap (typeParens ForCtor << formatType)) args)
        -- TypeParens type_ ->
        --     Tuple.pair NotNeeded <|
        --         parens <|
        --             formatCommented <|
        --                 fmap (typeParens NotRequired << formatType) type_
        Src.TRecord fields ext ->
            let
                base =
                    Maybe.map (\(A.At _ extName) -> ( [], extName, [] )) ext

                fields_ : List (C2Eol (Pair Name Src.Type))
                fields_ =
                    List.map
                        (\( A.At _ name, typ ) ->
                            ( ( [], [], Nothing )
                            , Pair ( [], name ) ( [], typ ) (ForceMultiline False)
                            )
                        )
                        fields

                trailing =
                    -- TODO
                    []

                multiline =
                    -- TODO
                    ForceMultiline False
            in
            ( NotNeeded
            , formatRecordLike
                (Maybe.map (c2map (Box.line << formatLowercaseIdentifier [])) base)
                (List.map (c2EolMap (formatPair ":" << mapPair (formatLowercaseIdentifier []) (typeParens NotRequired << formatType))) fields_)
                trailing
                multiline
            )

        Src.TUnit ->
            let
                comments =
                    -- TODO
                    []
            in
            ( NotNeeded
            , formatUnit '(' ')' comments
            )

        Src.TTuple a b cs ->
            let
                types =
                    List.map (Tuple.pair ( [], [], Nothing )) (a :: b :: cs)

                forceMultiline =
                    -- TODO
                    False
            in
            ( NotNeeded
            , ElmStructure.group True "(" "," ")" forceMultiline (List.map (formatC2Eol << c2EolMap (typeParens NotRequired << formatType)) types)
            )

        _ ->
            Debug.todo ("formatType: " ++ Debug.toString atype)


formatVar : Ref (List String) -> Box.Line
formatVar var =
    case var of
        VarRef namespace name ->
            formatLowercaseIdentifier namespace name

        TagRef namespace name ->
            case namespace of
                [] ->
                    Box.identifier (formatVarName name)

                _ ->
                    Box.row
                        [ formatQualifiedUppercaseIdentifier namespace
                        , Box.punc "."
                        , Box.identifier (formatVarName name)
                        ]

        OpRef name ->
            formatSymbolIdentifierInParens name


formatSymbolIdentifierInParens : String -> Box.Line
formatSymbolIdentifierInParens name =
    Box.identifier <| "(" ++ name ++ ")"


formatInfixVar : Ref (List String) -> Box.Line
formatInfixVar var =
    case var of
        VarRef _ _ ->
            Box.row
                [ Box.punc "`"
                , formatVar var
                , Box.punc "`"
                ]

        TagRef _ _ ->
            Box.row
                [ Box.punc "`"
                , formatVar var
                , Box.punc "`"
                ]

        OpRef name ->
            Box.identifier name


formatLowercaseIdentifier : List String -> String -> Box.Line
formatLowercaseIdentifier namespace name =
    case ( namespace, name ) of
        ( [], _ ) ->
            Box.identifier (formatVarName name)

        _ ->
            Box.row
                [ formatQualifiedUppercaseIdentifier namespace
                , Box.punc "."
                , Box.identifier (formatVarName name)
                ]


formatUppercaseIdentifier : String -> Box.Line
formatUppercaseIdentifier name =
    Box.identifier (formatVarName name)


formatQualifiedUppercaseIdentifier : List String -> Box.Line
formatQualifiedUppercaseIdentifier names =
    Box.identifier <|
        String.join "." <|
            List.map formatVarName names


formatVarName : String -> String
formatVarName name =
    String.map
        (\x ->
            if x == '\'' then
                '_'

            else
                x
        )
        name



-- AST


type ForceMultiline
    = ForceMultiline Bool


type alias Comments =
    List Space.Comment


type alias C1 a =
    ( Comments, a )


c1map : (a -> b) -> C1 a -> C1 b
c1map f ( comments, a ) =
    ( comments, f a )


type alias C2 a =
    ( Comments, a, Comments )


c2map : (a -> b) -> C2 a -> C2 b
c2map f ( before, a, after ) =
    ( before, f a, after )


sequenceAC2 : List (C2 a) -> C2 (List a)
sequenceAC2 =
    List.foldr
        (\( before, a, after ) ( beforeAcc, acc, afterAcc ) ->
            ( before ++ beforeAcc, a :: acc, after ++ afterAcc )
        )
        ( [], [], [] )


type alias C3 a =
    ( ( Comments, Comments, Comments ), a )


type alias C0Eol a =
    ( Maybe String, a )


type alias C1Eol a =
    ( Comments, Maybe String, a )


type alias C2Eol a =
    ( ( Comments, Comments, Maybe String ), a )


c2EolMap : (a -> b) -> C2Eol a -> C2Eol b
c2EolMap f ( ( before, after, eol ), a ) =
    ( ( before, after, eol ), f a )


{-| This represents a list of things that have a clear start delimiter but no
clear end delimiter.
There must be at least one item.
Comments can appear before the last item, or around any other item.
An end-of-line comment can also appear after the last item.

For example:
= a
= a, b, c

TODO: this should be replaced with (Sequence a)

-}
type OpenCommentedList a
    = OpenCommentedList (List (C2Eol a)) (C1Eol a)


openCommentedListMap : (a -> b) -> OpenCommentedList a -> OpenCommentedList b
openCommentedListMap f (OpenCommentedList rest ( preLst, eolLst, lst )) =
    OpenCommentedList
        (List.map (\( ( pre, post, eol ), a ) -> ( ( pre, post, eol ), f a )) rest)
        ( preLst, eolLst, f lst )


{-| Represents a delimiter-separated pair.

Comments can appear after the key or before the value.

For example:

key = value
key : value

-}
type Pair key value
    = Pair (C1 key) (C1 value) ForceMultiline


mapPair : (a1 -> a2) -> (b1 -> b2) -> Pair a1 b1 -> Pair a2 b2
mapPair fa fb (Pair k v fm) =
    Pair (c1map fa k) (c1map fb v) fm
