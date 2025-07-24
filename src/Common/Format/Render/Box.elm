module Common.Format.Render.Box exposing (..)

import Basics.Extra as Basics exposing (flip)
import Common.Format.Box as Box exposing (Box)
import Common.Format.Cheapskate.Parse as Parse
import Common.Format.Cheapskate.Types exposing (..)
import Common.Format.ImportInfo as ImportInfo exposing (ImportInfo)
import Common.Format.KnownContents as KnownContents
import Common.Format.Render.ElmStructure as ElmStructure
import Common.Format.Render.Markdown as Markdown
import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Module as M
import Compiler.Parse.Primitives as P
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


formatBinary : Bool -> Box -> List ( ( Bool, Src.FComments, Box ), Box ) -> Box
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

        BodyComment Src.CommentTrickOpener ->
            DStarter

        BodyComment Src.CommentTrickCloser ->
            DCloser

        BodyComment _ ->
            DComment


removeDuplicates : List (List (Src.C2 Src.Exposed)) -> List (List (Src.C2 Src.Exposed))
removeDuplicates input =
    let
        step :
            List (Src.C2 Src.Exposed)
            -> ( List (List (Src.C2 Src.Exposed)), EverySet String (Src.C2 Src.Exposed) )
            -> ( List (List (Src.C2 Src.Exposed)), EverySet String (Src.C2 Src.Exposed) )
        step next ( acc, seen ) =
            case List.foldl stepChildren ( [], seen ) next |> (\( a, b ) -> ( List.reverse a, b )) of
                ( [], seen_ ) ->
                    ( acc, seen_ )

                ( children_, seen_ ) ->
                    ( children_ :: acc, seen_ )

        stepChildren :
            Src.C2 Src.Exposed
            -> ( List (Src.C2 Src.Exposed), EverySet String (Src.C2 Src.Exposed) )
            -> ( List (Src.C2 Src.Exposed), EverySet String (Src.C2 Src.Exposed) )
        stepChildren next ( acc, seen ) =
            if EverySet.member (\( _, v, _ ) -> Debug.todo "v") next seen then
                ( acc, seen )

            else
                ( next :: acc, EverySet.insert (\( _, v, _ ) -> Debug.todo "v") next seen )
    in
    List.foldl step ( [], EverySet.empty ) input
        |> Tuple.first
        |> List.reverse


sortVars : Bool -> EverySet String (Src.C2 Src.Exposed) -> List (List String) -> ( List (List (Src.C2 Src.Exposed)), Src.FComments )
sortVars forceMultiline fromExposing fromDocs =
    let
        varOrder : Src.C2 Src.Exposed -> ( Int, String )
        varOrder ( _, _, exposed ) =
            case exposed of
                Src.Operator _ name ->
                    ( 1, name )

                Src.Upper (A.At _ name) _ ->
                    ( 2, name )

                Src.Lower (A.At _ name) ->
                    ( 3, name )

        listedInDocs : List (List (Src.C2 Src.Exposed))
        listedInDocs =
            fromDocs
                |> List.map (List.filterMap (\v -> Map.get identity v allowedInDocs))
                |> List.filter (not << List.isEmpty)
                |> List.map (List.map (\v -> ( [], [], v )))
                |> removeDuplicates

        listedInExposing =
            fromExposing
                |> EverySet.toList (\a b -> compare (varName a) (varName b))
                |> List.sortBy varOrder

        varName : Src.C2 Src.Exposed -> String
        varName ( _, _, exposed ) =
            case exposed of
                Src.Lower (A.At _ name) ->
                    name

                Src.Upper (A.At _ name) _ ->
                    name

                Src.Operator _ name ->
                    name

        varSetToMap : EverySet String (Src.C2 Src.Exposed) -> Dict String String Src.Exposed
        varSetToMap set =
            EverySet.toList (\a b -> compare (varName a) (varName b)) set
                |> List.map (\(( _, _, exposed ) as var) -> ( varName var, exposed ))
                |> Map.fromList identity

        allowedInDocs : Dict String String Src.Exposed
        allowedInDocs =
            varSetToMap fromExposing

        allFromDocs : EverySet String String
        allFromDocs =
            EverySet.fromList identity (List.map varName (List.concat listedInDocs))

        inDocs : Src.C2 Src.Exposed -> Bool
        inDocs x =
            EverySet.member identity (varName x) allFromDocs

        remainingFromExposing : List (Src.C2 Src.Exposed)
        remainingFromExposing =
            listedInExposing
                |> List.filter (not << inDocs)

        commentsFromReorderedVars : Src.FComments
        commentsFromReorderedVars =
            listedInExposing
                |> List.filter inDocs
                |> List.map (\( pre, post, _ ) -> pre ++ post)
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


formatModuleHeader : Bool -> M.Module -> Src.C1 (List Box)
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
            modu.header
                |> Maybe.andThen (.docs >> Result.toMaybe)
                |> Maybe.map
                    (\(Src.Comment (P.Snippet { fptr, offset, length })) ->
                        String.slice offset (offset + length) fptr
                            |> String.trim
                            |> Parse.markdown
                                (Options
                                    { sanitize = True
                                    , allowRawHtml = True
                                    , preserveHardBreaks = True
                                    , debug = False
                                    }
                                )
                            |> (\(Doc _ blocks) -> blocks)
                    )
                |> Maybe.withDefault []
                |> List.concatMap extractDocs

        documentedVarsSet : EverySet String String
        documentedVarsSet =
            EverySet.fromList identity (List.concat documentedVars)

        extractDocs : Block -> List (List String)
        extractDocs block =
            case block of
                ElmDocs vars ->
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

        definedVars : EverySet String (Src.C2 Src.Exposed)
        definedVars =
            modu.decls
                |> List.map Src.c2Value
                |> List.concatMap extractVarName
                |> List.map (\varName -> ( [], [], varName ))
                |> EverySet.fromList
                    (\( _, _, exposed ) ->
                        case exposed of
                            Src.Lower (A.At _ name) ->
                                name

                            Src.Operator _ name ->
                                name

                            Src.Upper (A.At _ name) _ ->
                                name
                    )

        exportsList : A.Located Src.Exposing
        exportsList =
            Maybe.withDefault M.defaultHeader maybeHeader
                |> .exports
                |> (\( _, _, v ) -> v)

        detailedListingToSet : Src.Exposing -> EverySet String (Src.C2 Src.Exposed)
        detailedListingToSet listing =
            case listing of
                Src.Open _ _ ->
                    EverySet.empty

                Src.Explicit (A.At _ exposedList) ->
                    exposedList
                        |> EverySet.fromList
                            (\( _, _, exposed ) ->
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

                A.At _ (Src.Open _ _) ->
                    False

        varsToExpose : EverySet String (Src.C2 Src.Exposed)
        varsToExpose =
            case Maybe.map .exports maybeHeader of
                Nothing ->
                    if List.all List.isEmpty documentedVars then
                        definedVars

                    else
                        EverySet.filter
                            (\( _, _, v ) ->
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

                Just ( _, _, A.At _ e ) ->
                    detailedListingToSet e

        sortedExports : ( List (List (Src.C2 Src.Exposed)), Src.FComments )
        sortedExports =
            sortVars
                (detailedListingIsMultiline exportsList)
                varsToExpose
                documentedVars

        extractVarName : Decl.Decl -> List Src.Exposed
        extractVarName decl =
            case decl of
                Decl.Value _ (A.At _ (Src.Value _ ( _, name ) _ _ _)) ->
                    [ Src.Lower name ]

                Decl.Union _ (A.At _ (Src.Union name _ _)) ->
                    [ Src.Upper name (Src.Public A.zero) ]

                Decl.Alias _ (A.At _ (Src.Alias _ ( _, _, name ) _ _)) ->
                    [ Src.Upper name Src.Private ]

                Decl.Port _ (Src.Port _ ( _, _, name ) _) ->
                    [ Src.Lower name ]

        formatModuleLine_ : M.Header -> Box
        formatModuleLine_ header =
            let
                ( preExposing, postExposing, _ ) =
                    header.exports
            in
            formatModuleLine sortedExports header.effects header.name preExposing postExposing

        docs =
            modu.header
                |> Maybe.andThen (.docs >> Result.toMaybe)
                |> Maybe.map
                    (\(Src.Comment (P.Snippet { fptr, offset, length })) ->
                        String.slice offset (offset + length) fptr
                            |> String.trim
                            |> Parse.markdown
                                (Options
                                    { sanitize = True
                                    , allowRawHtml = True
                                    , preserveHardBreaks = True
                                    , debug = False
                                    }
                                )
                            |> (\(Doc _ blocks) -> formatDocComment (ImportInfo.fromModule KnownContents.mempty modu) blocks)
                    )
    in
    Src.c1map
        (\imports ->
            List.concat
                (List.intersperse [ Box.blankLine ]
                    (List.concat
                        [ Maybe.toList (Maybe.map (List.singleton << formatModuleLine_) maybeHeader)
                        , Maybe.toList (Maybe.map List.singleton docs)
                        , if List.isEmpty imports then
                            []

                          else
                            [ imports ]
                        ]
                    )
                )
        )
        (formatImports modu)


formatImports : M.Module -> Src.C1 (List Box)
formatImports modu =
    let
        ( comments, imports ) =
            modu.imports

        ( postImportComments, restImports ) =
            case List.reverse imports of
                [] ->
                    ( [], [] )

                ( lastComments, _ ) :: firstImports ->
                    ( lastComments, List.reverse firstImports )
    in
    ( postImportComments
    , [ List.foldl (\( importComments, _ ) acc -> acc ++ importComments) comments restImports
            |> formatComments
            |> Maybe.toList
      , imports
            |> List.map Src.c1Value
            |> List.sortBy (\(Src.Import ( _, A.At _ importName ) _ _) -> importName)
            |> List.map formatImport
      ]
        |> List.filter (not << List.isEmpty)
        |> List.intersperse [ Box.blankLine ]
        |> List.concat
    )


formatModuleLine :
    ( List (List (Src.C2 Src.Exposed)), Src.FComments )
    -> M.Effects
    -> Src.C2 (A.Located Name.Name)
    -> Src.FComments
    -> Src.FComments
    -> Box
formatModuleLine ( varsToExpose, extraComments ) srcTag ( preName, postName, A.At _ name ) preExposing postExposing =
    let
        tag =
            case srcTag of
                M.NoEffects _ ->
                    Box.line (Box.keyword "module")

                M.Ports _ comments ->
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
                        |> List.map (formatCommented << Src.c2map formatVarValue)
                        |> ElmStructure.group_ False "(" "," (Maybe.toList (formatComments extraComments)) ")" False

                _ ->
                    varsToExpose
                        |> List.map (formatCommented << Src.c2map (ElmStructure.group False "" "," "" False << List.map formatVarValue) << Src.sequenceAC2)
                        |> ElmStructure.group_ False "(" "," (Maybe.toList (formatComments extraComments)) ")" True

        formatSetting : ( Src.C2 String, Src.C2 String ) -> Box
        formatSetting ( k, v ) =
            formatRecordPair "=" (Box.line << formatUppercaseIdentifier) ( k, v, False )

        formatSettings : List ( Src.C2 String, Src.C2 String ) -> Box
        formatSettings settings =
            List.map formatSetting settings
                |> ElmStructure.group True "{" "," "}" False

        whereClause : List Box
        whereClause =
            case srcTag of
                M.NoEffects _ ->
                    []

                M.Ports _ _ ->
                    []

                M.Manager _ manager ->
                    let
                        settings : List ( Src.C2 String, Src.C2 Name.Name )
                        settings =
                            case manager of
                                Src.Cmd (A.At _ cmdType) ->
                                    [ ( ( [], [], "command" ), ( [], [], cmdType ) ) ]

                                Src.Sub (A.At _ subType) ->
                                    [ ( ( [], [], "subscription" ), ( [], [], subType ) ) ]

                                Src.Fx (A.At _ cmdType) (A.At _ subType) ->
                                    [ ( ( [], [], "command" ), ( [], [], cmdType ) )
                                    , ( ( [], [], "subscription" ), ( [], [], subType ) )
                                    ]
                    in
                    -- TODO add comments around manager/settings (`where` part)
                    [ formatKeywordCommented "where" ( [], [], formatSettings settings ) ]

        nameClause =
            case
                ( tag
                , formatCommented ( preName, postName, Box.line (formatQualifiedUppercaseIdentifier (String.split "." name)) )
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
            (whereClause ++ [ formatCommented ( preExposing, postExposing, Box.line (Box.keyword "exposing") ) ])
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

        declarations =
            List.concatMap
                (\( preDeclComments, postDeclComments, decl ) ->
                    let
                        _ =
                            Debug.log "declaration!123" ( preDeclComments, postDeclComments, decl )
                    in
                    List.map BodyComment preDeclComments
                        ++ (case decl of
                                Decl.Value maybeDocs (A.At _ (Src.Value preValueComments _ _ _ _)) ->
                                    (maybeDocs
                                        |> Maybe.map
                                            (\(Src.Comment (P.Snippet { fptr, offset, length })) ->
                                                [ DocComment
                                                    (String.slice offset (offset + length) fptr
                                                        |> String.trim
                                                        |> Parse.markdown
                                                            (Options
                                                                { sanitize = True
                                                                , allowRawHtml = True
                                                                , preserveHardBreaks = True
                                                                , debug = False
                                                                }
                                                            )
                                                        |> (\(Doc _ blocks) -> blocks)
                                                    )
                                                ]
                                            )
                                        |> Maybe.withDefault []
                                    )
                                        ++ List.map BodyComment preValueComments

                                -- Decl.Union (Just comment) _ ->
                                --     [ BodyComment comment ]
                                -- Decl.Alias (Just comment) _ ->
                                --     [ BodyComment comment ]
                                Decl.Port (Just (Src.Comment (P.Snippet { fptr, offset, length }))) _ ->
                                    [ DocComment
                                        (String.slice offset (offset + length) fptr
                                            |> String.trim
                                            |> Parse.markdown
                                                (Options
                                                    { sanitize = True
                                                    , allowRawHtml = True
                                                    , preserveHardBreaks = True
                                                    , debug = False
                                                    }
                                                )
                                            |> (\(Doc _ blocks) -> blocks)
                                        )
                                    ]

                                _ ->
                                    []
                           )
                        ++ Entry (CommonDeclaration decl)
                        :: List.map BodyComment postDeclComments
                )
                modu.decls

        ( moduleHeaderComments, moduleHeader ) =
            formatModuleHeader addDefaultHeader modu

        body : List (TopLevelStructure Declaration)
        body =
            List.map BodyComment moduleHeaderComments
                ++ List.concatMap
                    (\( commments, infix_ ) ->
                        Entry (InfixDeclaration infix_)
                            :: List.map BodyComment commments
                    )
                    (List.reverse modu.infixes)
                ++ declarations

        spaceBeforeBody : Int
        spaceBeforeBody =
            case body of
                [] ->
                    0

                (BodyComment _) :: _ ->
                    spacing + 1

                _ ->
                    spacing
    in
    Box.stack1
        (List.concat
            [ initialComments_
            , moduleHeader
            , List.repeat spaceBeforeBody Box.blankLine
            , Maybe.toList (formatModuleBody spacing (ImportInfo.fromModule KnownContents.mempty modu) body)
            ]
        )


type Declaration
    = CommonDeclaration Decl.Decl
    | InfixDeclaration (A.Located Src.Infix)


formatModuleBody : Int -> ImportInfo -> List (TopLevelStructure Declaration) -> Maybe Box
formatModuleBody linesBetween importInfo body =
    let
        entryType : Declaration -> BodyEntryType
        entryType adecl =
            case adecl of
                CommonDeclaration (Decl.Value _ (A.At _ (Src.Value _ ( _, A.At _ name ) _ _ _))) ->
                    BodyNamed (VarRef () name)

                CommonDeclaration (Decl.Union _ (A.At _ (Src.Union (A.At _ name) _ _))) ->
                    BodyNamed (TagRef () name)

                CommonDeclaration (Decl.Alias _ (A.At _ (Src.Alias _ ( _, _, A.At _ name ) _ _))) ->
                    BodyNamed (TagRef () name)

                CommonDeclaration (Decl.Port _ (Src.Port _ ( _, _, A.At _ name ) _)) ->
                    BodyNamed (VarRef () name)

                InfixDeclaration (A.At _ (Src.Infix _ _ _ _)) ->
                    BodyFixity
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
    = DocComment Blocks
    | BodyComment Src.FComment
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


type ElmCodeBlock
    = DeclarationsCode (List (TopLevelStructure Declaration))
    | ExpressionsCode (List (TopLevelStructure (Src.C0Eol Src.Expr)))
    | ModuleCode M.Module



-- convertElmCodeBlock : (ann -> ann_) -> ElmCodeBlock ann ns -> ElmCodeBlock ann_ ns
-- convertElmCodeBlock f elmCodeBlock =
--     case elmCodeBlock of
--         DeclarationsCode decls ->
--             DeclarationsCode (fmap (fmap (I.convert f)) decls)
--         ExpressionsCode exprs ->
--             ExpressionsCode (fmap (fmap (fmap (I.convert f))) exprs)
--         ModuleCode mod ->
--             ModuleCode (fmap (I.convert f) mod)


{-| TODO: there must be an existing haskell function that does this, right?
-}
firstOf : List (a -> Maybe b) -> a -> Maybe b
firstOf options value =
    case options of
        [] ->
            Nothing

        next :: rest ->
            case next value of
                Just result ->
                    Just result

                Nothing ->
                    firstOf rest value


formatDocComment : ImportInfo -> Blocks -> Box
formatDocComment importInfo blocks =
    let
        parse : String -> Maybe ElmCodeBlock
        parse source =
            -- source
            --     |> Debug.log "formatDocComment.source"
            --     |> firstOf
            --         [ Maybe.map DeclarationsCode << Result.toMaybe << Parse.parseDeclarations
            --         , Maybe.map ExpressionsCode << Result.toMaybe << Parse.parseExpressions
            --         , Maybe.map ModuleCode << Result.toMaybe << Parse.parseModule
            --         ]
            -- TODO!!!!
            Nothing

        format : ElmCodeBlock -> String
        format result =
            case result of
                ModuleCode modu ->
                    formatModule False 1 modu
                        |> Box.render

                DeclarationsCode declarations ->
                    formatModuleBody 1 importInfo declarations
                        |> Maybe.map Box.render
                        |> Maybe.withDefault ""

                ExpressionsCode expressions ->
                    expressions
                        |> List.map (topLevelStructureMap (formatEolCommented << Src.c0EolMap (syntaxParens SyntaxSeparated << formatExpression importInfo)))
                        |> List.map (topLevelStructureMap (Tuple.pair BodyUnnamed))
                        |> formatTopLevelBody 1 importInfo
                        |> Maybe.map Box.render
                        |> Maybe.withDefault ""

        content : String
        content =
            Markdown.formatMarkdown (Maybe.map format << parse) (List.map cleanBlock blocks)

        cleanBlock : Block -> Block
        cleanBlock block =
            case block of
                ElmDocs docs ->
                    ElmDocs ((List.map << List.map) (String.replace "(..)" "") docs)

                _ ->
                    block
    in
    formatDocCommentString content


formatDocCommentString : String -> Box
formatDocCommentString docs =
    case lines docs of
        [] ->
            Box.line (Box.row [ Box.punc "{-|", Box.space, Box.punc "-}" ])

        [ first ] ->
            Box.stack1
                [ Box.line (Box.row [ Box.punc "{-|", Box.space, Box.literal first ])
                , Box.line (Box.punc "-}")
                ]

        first :: rest ->
            Box.line (Box.row [ Box.punc "{-|", Box.space, Box.literal first ])
                |> Box.andThen (List.map (Box.line << Box.literal) rest)
                |> Box.andThen [ Box.line <| Box.punc "-}" ]


lines : String -> List String
lines str =
    case List.reverse (String.lines str) of
        "" :: rest ->
            List.reverse rest

        result ->
            List.reverse result


formatImport : Src.Import -> Box
formatImport ((Src.Import ( preImportNameComments, A.At _ importName ) maybeAlias exposing_) as import__) =
    let
        maybeRequestedAs =
            maybeAlias
                |> Maybe.andThen
                    (\aliasName ->
                        if Src.c2Value aliasName == importName then
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
                            requestedAs
                    )
                |> Maybe.join

        exposingVar =
            formatImportClause
                (formatExposing formatDetailedListing)
                "exposing"
                exposing_

        formatImportClause : (a -> Maybe Box) -> String -> Src.C2 a -> Maybe Box
        formatImportClause format keyw input =
            case Src.c2map format input of
                ( [], [], Nothing ) ->
                    Nothing

                ( preKeyword, postKeyword, Just listing_ ) ->
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
        ( formatPreCommented (Src.c1map (Box.line << formatQualifiedUppercaseIdentifier) ( preImportNameComments, String.split "." importName ))
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
            Just (parens (formatCommented ( [], [], Box.line (Box.keyword "..") )))


formatExposing : (List (Src.C2 Src.Exposed) -> List Box) -> Src.Exposing -> Maybe Box
formatExposing format listing =
    case listing of
        Src.Open preComments postComments ->
            Just (parens (formatCommented ( preComments, postComments, Box.line (Box.keyword "..") )))

        Src.Explicit (A.At _ []) ->
            Nothing

        Src.Explicit (A.At region exposedList) ->
            case format exposedList of
                [] ->
                    Nothing

                vars_ ->
                    let
                        multiline =
                            A.isMultiline region
                    in
                    Just <| ElmStructure.group False "(" "," ")" multiline vars_


formatDetailedListing : List (Src.C2 Src.Exposed) -> List Box
formatDetailedListing exposedList =
    -- List.concat
    --     [ formatCommentedMap (\_ _ -> EQ)
    --         (\name () -> Src.Operator A.zero name)
    --         formatVarValue
    --         listing.operators
    --     , formatCommentedMap (\_ _ -> EQ)
    --         (\name _ -> Src.Upper (A.At A.zero name) Src.Private)
    --         formatVarValue
    --         listing.types
    --     , formatCommentedMap (\_ _ -> EQ)
    --         (\name () -> Src.Lower (A.At A.zero name))
    --         formatVarValue
    --         listing.values
    --     ]
    exposedList
        |> List.sortBy
            (\( _, _, exposed ) ->
                case exposed of
                    Src.Lower (A.At _ name) ->
                        ( 3, name )

                    Src.Upper (A.At _ name) _ ->
                        ( 2, name )

                    Src.Operator _ name ->
                        ( 1, name )
            )
        |> List.map (\exposed -> formatCommented (Src.c2map formatVarValue exposed))



-- formatCommentedMap : (k -> k -> Order) -> (k -> v -> a) -> (a -> Box) -> Dict comparable k (C2 v) -> List Box
-- formatCommentedMap keyComparison construct format values =
--     let
--         format_ ( k, ( beforeComments, v, afterComments ) ) =
--             formatCommented ( beforeComments, format (construct k v), afterComments )
--     in
--     values
--         |> Map.toList keyComparison
--         |> List.map format_


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
formatCommonDeclaration importInfo (A.At _ (Src.Value _ ( postNameComments, A.At nameRegion name ) args ( comments, expr ) maybeType)) =
    let
        formattedDefinition =
            formatDefinition importInfo (A.At nameRegion (Src.PVar name)) args comments expr
    in
    case maybeType of
        Just typ ->
            Box.stack1
                [ formatTypeAnnotation ( postNameComments, VarRef () name ) typ
                , formattedDefinition
                ]

        Nothing ->
            formattedDefinition


formatDeclaration : ImportInfo -> Declaration -> Box
formatDeclaration importInfo decl =
    case decl of
        CommonDeclaration (Decl.Value _ value) ->
            formatCommonDeclaration importInfo (Debug.log "VALUE!" value)

        CommonDeclaration (Decl.Union _ (A.At _ (Src.Union name args tags))) ->
            let
                tags_ : Src.OpenCommentedList ( A.Located Name, List Src.Type )
                tags_ =
                    case tags of
                        firstTag :: restTags ->
                            Src.OpenCommentedList (List.map (\tag -> ( ( [], [], Nothing ), tag )) restTags)
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
            case formatOpenCommentedList (Src.openCommentedListMap ctor tags_) of
                [] ->
                    crash "List can't be empty"

                first :: rest ->
                    -- TODO add comments surrounding name+args
                    case formatCommented ( [], [], formatNameWithArgs (A.toValue name) (List.map (\(A.At _ arg) -> ( [], arg )) args) ) of
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

        CommonDeclaration (Decl.Alias _ (A.At _ (Src.Alias preAlias ( preNameComments, postArgsComments, A.At _ name ) args typ))) ->
            ElmStructure.definition "="
                True
                (Box.line (Box.keyword "type"))
                [ formatPreCommented ( preAlias, Box.line (Box.keyword "alias") )
                , formatCommented ( preNameComments, postArgsComments, formatNameWithArgs name (List.map (Src.c1map A.toValue) args) )
                ]
                (formatPreCommentedStack <| Src.c1map (typeParens NotRequired << formatType) typ)

        CommonDeclaration (Decl.Port _ (Src.Port typeComments ( preNameComments, postNameComments, A.At _ name ) typ)) ->
            ElmStructure.definition ":"
                False
                (Box.line (Box.keyword "port"))
                [ formatCommented (Src.c2map (Box.line << formatLowercaseIdentifier []) ( preNameComments, postNameComments, name )) ]
                (formatCommentedApostrophe typeComments (typeParens NotRequired (formatType typ)))

        InfixDeclaration (A.At _ (Src.Infix op associativity precedence name)) ->
            let
                formatAssoc a =
                    case a of
                        Binop.Left ->
                            Box.keyword "left "

                        Binop.Right ->
                            Box.keyword "right"

                        Binop.Non ->
                            Box.keyword "non  "
            in
            ElmStructure.spaceSepOrIndented
                (Box.line (Box.keyword "infix"))
                [ formatPreCommented (Src.c1map (Box.line << formatAssoc) associativity)
                , formatPreCommented (Src.c1map (Box.line << Box.literal << String.fromInt) precedence)
                , formatCommented (Src.c2map (Box.line << formatSymbolIdentifierInParens) op)
                , Box.line (Box.keyword "=")
                , formatPreCommented (Src.c1map (Box.line << Box.identifier << formatVarName) name)
                ]


formatNameWithArgs : Name -> List (Src.C1 Name) -> Box
formatNameWithArgs name args =
    case Box.allSingles <| List.map (formatPreCommented << Src.c1map (Box.line << formatLowercaseIdentifier [])) args of
        Ok args_ ->
            Box.line (Box.row (List.intersperse Box.space (formatUppercaseIdentifier name :: args_)))

        Err args_ ->
            Box.stack1
                ((Box.line <| formatUppercaseIdentifier name)
                    :: List.map Box.indent args_
                )


formatDefinition : ImportInfo -> Src.Pattern -> List (Src.C1 Src.Pattern) -> Src.FComments -> Src.Expr -> Box
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
        (List.map (\( x, A.At _ y ) -> formatCommentedApostrophe x (syntaxParens SpaceSeparated (formatPattern y))) args)
        body


formatTypeAnnotation : Src.C1 (Ref ()) -> Src.C1 Src.Type -> Box
formatTypeAnnotation name typ =
    ElmStructure.definition ":"
        False
        (formatTailCommented (Src.c1map (Box.line << formatVar << refMap (\() -> [])) name))
        []
        (formatPreCommented (Src.c1map (typeParens NotRequired << formatType) typ))


formatPattern : Src.Pattern_ -> ( SyntaxContext, Box )
formatPattern apattern =
    case apattern of
        Src.PAnything name ->
            ( SyntaxSeparated, Box.line (Box.identifier ("_" ++ name)) )

        Src.PVar name ->
            ( SyntaxSeparated, Box.line (formatLowercaseIdentifier [] name) )

        Src.PRecord ( comments, [] ) ->
            ( SyntaxSeparated
            , formatUnit '{' '}' comments
            )

        Src.PRecord ( _, fields ) ->
            ( SyntaxSeparated
            , ElmStructure.group True "{" "," "}" False (List.map (formatCommented << Src.c2map (Box.line << formatLowercaseIdentifier [] << A.toValue)) fields)
            )

        Src.PAlias aliasPattern name ->
            ( SpaceSeparated
            , case
                ( formatTailCommented (Src.c1map (syntaxParens SpaceSeparated << formatPattern << A.toValue) aliasPattern)
                , formatPreCommented (Src.c1map (Box.line << formatLowercaseIdentifier [] << A.toValue) name)
                )
              of
                ( Box.SingleLine pattern_, Box.SingleLine name_ ) ->
                    Box.line
                        (Box.row
                            [ pattern_
                            , Box.space
                            , Box.keyword "as"
                            , Box.space
                            , name_
                            ]
                        )

                ( pattern_, name_ ) ->
                    Box.stack1
                        [ pattern_
                        , Box.line (Box.keyword "as")
                        , Box.indent name_
                        ]
            )

        Src.PUnit comments ->
            ( SyntaxSeparated, formatUnit '(' ')' comments )

        Src.PTuple a b cs ->
            let
                patterns : List (Src.C2 Src.Pattern)
                patterns =
                    a :: b :: cs
            in
            ( SyntaxSeparated
            , ElmStructure.group True "(" "," ")" False (List.map (formatCommented << Src.c2map (syntaxParens SyntaxSeparated << formatPattern << A.toValue)) patterns)
            )

        Src.PCtor _ name [] ->
            let
                ctor =
                    [ name ]
            in
            ( SyntaxSeparated
            , Box.line (formatQualifiedUppercaseIdentifier ctor)
            )

        Src.PCtor _ name patterns ->
            let
                ctor =
                    [ name ]
            in
            ( SpaceSeparated
            , ElmStructure.application
                (ElmStructure.FAJoinFirst ElmStructure.JoinAll)
                (Box.line (formatQualifiedUppercaseIdentifier ctor))
                (List.map (formatPreCommented << Src.c1map (syntaxParens SpaceSeparated << formatPattern << A.toValue)) patterns)
            )

        Src.PCtorQual _ home name [] ->
            let
                ctor =
                    String.split "." home ++ [ name ]
            in
            ( SyntaxSeparated
            , Box.line (formatQualifiedUppercaseIdentifier ctor)
            )

        Src.PCtorQual _ home name patterns ->
            let
                ctor =
                    String.split "." home ++ [ name ]
            in
            ( SpaceSeparated
            , ElmStructure.application
                (ElmStructure.FAJoinFirst ElmStructure.JoinAll)
                (Box.line (formatQualifiedUppercaseIdentifier ctor))
                (List.map (formatPreCommented << Src.c1map (syntaxParens SpaceSeparated << formatPattern << A.toValue)) patterns)
            )

        Src.PList ( comments, [] ) ->
            ( SyntaxSeparated
            , formatUnit '[' ']' comments
            )

        Src.PList ( _, patterns ) ->
            ( SyntaxSeparated
            , ElmStructure.group True "[" "," "]" False (List.map (formatCommented << Src.c2map (syntaxParens SyntaxSeparated << formatPattern << A.toValue)) patterns)
            )

        Src.PCons hd tl ->
            let
                formatRight : Src.C2Eol Src.Pattern -> ( ( Bool, Src.FComments, Box ), Box )
                formatRight ( ( preOp, postOp, eol ), term ) =
                    ( ( False
                      , preOp
                      , Box.line (Box.punc "::")
                      )
                    , formatC2Eol
                        (Src.c2EolMap (syntaxParens SpaceSeparated << formatPattern << A.toValue)
                            ( ( postOp, [], eol ), term )
                        )
                    )
            in
            ( SpaceSeparated
            , formatBinary False
                (formatEolCommented (Src.c0EolMap (syntaxParens SpaceSeparated << formatPattern << A.toValue) hd))
                [ formatRight tl ]
            )

        Src.PChr chr ->
            ( SyntaxSeparated, formatString SChar chr )

        Src.PStr str False ->
            ( SyntaxSeparated, formatString (SString SingleQuotedString) str )

        Src.PStr str True ->
            ( SyntaxSeparated, formatString (SString TripleQuotedString) str )

        Src.PInt _ src ->
            ( SyntaxSeparated
            , formatLiteral (IntNum src)
            )


formatRecordPair : String -> (v -> Box) -> ( Src.C2 String, Src.C2 v, Bool ) -> Box
formatRecordPair delim formatValue ( ( pre, postK, k ), ( preV, postV, v ), forceMultiline ) =
    formatPreCommented
        ( pre
        , ElmStructure.equalsPair delim
            forceMultiline
            (formatCommented ( [], postK, Box.line (formatLowercaseIdentifier [] k) ))
            (formatCommented ( preV, postV, formatValue v ))
        )


formatPair : String -> Src.Pair Box.Line Box -> Box
formatPair delim ((Src.Pair a b (Src.ForceMultiline forceMultiline)) as pair) =
    ElmStructure.equalsPair delim
        forceMultiline
        (formatTailCommented <| Src.c1map Box.line a)
        (formatPreCommented b)


negativeCasePatternWorkaround : Src.Pattern_ -> Box -> Box
negativeCasePatternWorkaround pattern =
    case pattern of
        Src.PInt i _ ->
            if i < 0 then
                parens

            else
                identity

        _ ->
            identity


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
formatExpression importInfo (A.At region aexpr) =
    case aexpr of
        Src.Chr char ->
            ( SyntaxSeparated, formatString SChar char )

        Src.Str string False ->
            ( SyntaxSeparated, formatString (SString SingleQuotedString) string )

        Src.Str string True ->
            ( SyntaxSeparated, formatString (SString TripleQuotedString) string )

        Src.Int _ src ->
            ( SyntaxSeparated, formatLiteral (IntNum src) )

        Src.Float _ src ->
            ( SyntaxSeparated, formatLiteral (FloatNum src) )

        Src.Var Src.LowVar name ->
            ( SyntaxSeparated, Box.line (formatVar (VarRef [] name)) )

        Src.Var Src.CapVar name ->
            ( SyntaxSeparated, Box.line (formatVar (TagRef [] name)) )

        Src.VarQual Src.LowVar prefix name ->
            ( SyntaxSeparated, Box.line (formatVar (VarRef (String.split "." prefix) name)) )

        Src.VarQual Src.CapVar prefix name ->
            ( SyntaxSeparated, Box.line (formatVar (TagRef (String.split "." prefix) name)) )

        Src.List list trailing ->
            let
                multiline : Src.ForceMultiline
                multiline =
                    Src.ForceMultiline (A.isMultiline region)
            in
            ( SyntaxSeparated
            , formatSequence '['
                ','
                (Just ']')
                multiline
                trailing
                (List.map (Src.c2EolMap (syntaxParens SyntaxSeparated << formatExpression importInfo)) list)
            )

        Src.Op op ->
            ( SyntaxSeparated
            , Box.line (formatSymbolIdentifierInParens op)
            )

        Src.Negate expr ->
            ( SyntaxSeparated
              -- TODO: This might need something stronger than SpaceSeparated?
            , Box.prefix (Box.punc "-") (syntaxParens SpaceSeparated (formatExpression importInfo expr))
            )

        Src.Binops ops final ->
            let
                ( left, clauses ) =
                    List.foldr
                        (\( currExpr, A.At _ currOp ) ( leftAcc, clausesAcc ) ->
                            ( currExpr, BinopsClause [] (OpRef currOp) [] leftAcc :: clausesAcc )
                        )
                        ( final, [] )
                        ops

                multiline : Bool
                multiline =
                    A.isMultiline region
            in
            ( InfixSeparated
            , formatBinops importInfo left clauses multiline
            )

        Src.Lambda ( trailingComments, srcArgs ) ( bodyComments, expr ) ->
            -- TODO trailingComments (should go before `->`)
            let
                multiline : Bool
                multiline =
                    A.isMultiline region
            in
            ( AmbiguousEnd
            , case
                ( ( multiline
                  , Box.allSingles (List.map (formatPreCommented << Src.c1map (syntaxParens SpaceSeparated << formatPattern << A.toValue)) srcArgs)
                  )
                , ( bodyComments == []
                  , syntaxParens SyntaxSeparated (formatExpression importInfo expr)
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
            formatExpression importInfo func

        Src.Call func ((( _, A.At (A.Region _ (A.Position firstArgEndRow _)) _ ) :: _) as args) ->
            let
                (A.Region (A.Position aexprStartRow _) _) =
                    region

                multiline =
                    if firstArgEndRow > aexprStartRow then
                        ElmStructure.FASplitFirst

                    else
                        ElmStructure.FAJoinFirst
                            (if A.isMultiline region then
                                ElmStructure.SplitAll

                             else
                                ElmStructure.JoinAll
                            )
            in
            ( SpaceSeparated
            , ElmStructure.application
                multiline
                (syntaxParens InfixSeparated <| formatExpression importInfo func)
                (List.map (formatPreCommentedExpression importInfo SpaceSeparated) args)
            )

        Src.If [] _ ->
            -- TODO
            Debug.todo "needs review, this should not happen"

        Src.If (( _, if_ ) :: elseifs) ( elsComments, els ) ->
            let
                opening : Box -> Box -> Box
                opening key cond =
                    case ( key, cond ) of
                        ( Box.SingleLine key_, Box.SingleLine cond_ ) ->
                            Box.line <|
                                Box.row
                                    [ key_
                                    , Box.space
                                    , cond_
                                    , Box.space
                                    , Box.keyword "then"
                                    ]

                        _ ->
                            Box.stack1
                                [ key
                                , Box.indent cond
                                , Box.line (Box.keyword "then")
                                ]

                formatIf : ( Src.C2 Src.Expr, Src.C2 Src.Expr ) -> Box
                formatIf ( cond, body ) =
                    Box.stack1
                        [ opening (Box.line (Box.keyword "if")) (formatCommentedExpression importInfo cond)
                        , Box.indent <| formatCommented_ True <| Src.c2map (syntaxParens SyntaxSeparated << formatExpression importInfo) body
                        ]

                formatElseIf : Src.C1 ( Src.C2 Src.Expr, Src.C2 Src.Expr ) -> Box
                formatElseIf ( ifComments, ( cond, body ) ) =
                    let
                        key =
                            case formatPreCommented ( ifComments, Box.line (Box.keyword "if") ) of
                                Box.SingleLine key_ ->
                                    Box.line <| Box.row [ Box.keyword "else", Box.space, key_ ]

                                key_ ->
                                    Box.stack1
                                        [ Box.line (Box.keyword "else")
                                        , key_
                                        ]
                    in
                    Box.stack1
                        [ Box.blankLine
                        , opening key <| formatCommentedExpression importInfo cond
                        , Box.indent <| formatCommented_ True <| Src.c2map (syntaxParens SyntaxSeparated << formatExpression importInfo) body
                        ]
            in
            ( AmbiguousEnd
            , formatIf if_
                |> Box.andThen (List.map formatElseIf elseifs)
                |> Box.andThen
                    [ Box.blankLine
                    , Box.line (Box.keyword "else")
                    , Box.indent <| formatCommented_ True <| Src.c2map (syntaxParens SyntaxSeparated << formatExpression importInfo) ( elsComments, [], els )
                    ]
            )

        Src.Let defs bodyComments expr ->
            let
                letDeclarations : Src.C2 (A.Located Src.Def) -> List LetDeclaration
                letDeclarations ( preDefComments, postDefComments, def ) =
                    List.map LetComment preDefComments
                        ++ LetCommonDeclaration def
                        :: List.map LetComment postDefComments

                spacer : LetDeclaration -> LetDeclaration -> List Box
                spacer first _ =
                    case first of
                        LetCommonDeclaration _ ->
                            [ Box.blankLine ]

                        _ ->
                            []

                formatDefinition_ : LetDeclaration -> Box
                formatDefinition_ def =
                    case def of
                        LetCommonDeclaration (A.At _ (Src.Define (A.At nameRegion name) srcArgs body maybeType)) ->
                            let
                                comments =
                                    -- TODO
                                    []
                            in
                            formatDefinition importInfo (A.At nameRegion (Src.PVar name)) srcArgs comments body

                        LetCommonDeclaration (A.At _ (Src.Destruct pattern body)) ->
                            let
                                comments =
                                    -- TODO
                                    []
                            in
                            formatDefinition importInfo pattern [] comments body

                        LetComment comment ->
                            formatComment comment
            in
            ( AmbiguousEnd
            , -- TODO: not tested
              Box.line (Box.keyword "let")
                |> Box.andThen
                    (defs
                        |> List.concatMap letDeclarations
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

        Src.Case (( _, _, A.At subjectRegion _ ) as subject) clauses ->
            let
                opening =
                    case
                        ( A.isMultiline subjectRegion
                        , formatCommentedExpression importInfo subject
                        )
                    of
                        ( False, Box.SingleLine subject_ ) ->
                            Box.line <|
                                Box.row
                                    [ Box.keyword "case"
                                    , Box.space
                                    , subject_
                                    , Box.space
                                    , Box.keyword "of"
                                    ]

                        ( _, subject_ ) ->
                            Box.stack1
                                [ Box.line <| Box.keyword "case"
                                , Box.indent subject_
                                , Box.line <| Box.keyword "of"
                                ]

                clause : ( Src.C2 Src.Pattern, Src.C1 Src.Expr ) -> Box
                clause ( ( prePat, postPat, A.At _ pat ), ( preExpr, expr ) ) =
                    case
                        ( ( postPat
                          , formatPattern pat
                                |> syntaxParens SyntaxSeparated
                                |> negativeCasePatternWorkaround pat
                          )
                        , ( formatCommentedStack (Src.c2map (syntaxParens SyntaxSeparated << formatPattern) ( prePat, postPat, pat ))
                                |> negativeCasePatternWorkaround pat
                          , formatPreCommentedStack <| Src.c1map (syntaxParens SyntaxSeparated << formatExpression importInfo) ( preExpr, expr )
                          )
                        )
                    of
                        ( ( _, _ ), ( Box.SingleLine pat_, body_ ) ) ->
                            Box.stack1
                                [ Box.line (Box.row [ pat_, Box.space, Box.keyword "->" ])
                                , Box.indent body_
                                ]

                        ( ( [], Box.SingleLine pat_ ), ( _, body_ ) ) ->
                            Box.stack1
                                (List.map formatComment prePat
                                    ++ [ Box.line (Box.row [ pat_, Box.space, Box.keyword "->" ])
                                       , Box.indent body_
                                       ]
                                )

                        ( ( _, _ ), ( pat_, body_ ) ) ->
                            Box.stack1
                                [ pat_
                                , Box.line (Box.keyword "->")
                                , Box.indent body_
                                ]
            in
            ( AmbiguousEnd
            , -- TODO: not tested
              opening
                |> Box.andThen
                    (clauses
                        |> List.map clause
                        |> List.intersperse Box.blankLine
                        |> List.map Box.indent
                    )
            )

        Src.Accessor field ->
            ( SyntaxSeparated
            , Box.line (Box.identifier ("." ++ formatVarName field))
            )

        Src.Access record (A.At _ field) ->
            ( SyntaxSeparated
            , formatExpression importInfo record
                |> syntaxParens SpaceSeparated
                -- TODO: does this need a different context than SpaceSeparated?
                |> Box.addSuffix (Box.row [ Box.punc ".", formatLowercaseIdentifier [] field ])
            )

        Src.Update name fields ->
            let
                trailing =
                    -- TODO
                    []

                multiline =
                    Src.ForceMultiline (A.isMultiline region)

                fields_ =
                    List.map (\( A.At _ name_, expr ) -> Src.Pair ( [], name_ ) ( [], expr ) multiline) fields
            in
            ( SyntaxSeparated
            , formatRecordLike
                (Just (Src.c2map (syntaxParens SyntaxSeparated << formatExpression importInfo) name))
                (List.map (Tuple.pair ( [], [], Nothing ) << formatPair "=" << Src.mapPair (formatLowercaseIdentifier []) (syntaxParens SyntaxSeparated << formatExpression importInfo)) fields_)
                trailing
                multiline
            )

        Src.Record fields ->
            let
                trailing =
                    -- TODO
                    []

                multiline =
                    Src.ForceMultiline (A.isMultiline region)

                fields_ =
                    List.map (\( A.At _ name_, expr ) -> Src.Pair ( [], name_ ) ( [], expr ) multiline) fields
            in
            ( SyntaxSeparated
            , formatRecordLike Nothing
                (List.map (Tuple.pair ( [], [], Nothing ) << formatPair "=" << Src.mapPair (formatLowercaseIdentifier []) (syntaxParens SyntaxSeparated << formatExpression importInfo)) fields_)
                trailing
                multiline
            )

        Src.Unit ->
            ( SyntaxSeparated
            , formatUnit '(' ')' []
            )

        Src.Tuple a b cs ->
            let
                multiline =
                    A.isMultiline region

                exprs =
                    a :: b :: cs
            in
            Tuple.pair SyntaxSeparated <|
                ElmStructure.group True "(" "," ")" multiline <|
                    List.map (formatCommentedExpression importInfo) exprs

        Src.Shader (Shader.Source src) _ ->
            -- TODO/FIXME
            ( SyntaxSeparated
            , Box.line <|
                Box.row
                    [ Box.punc "[glsl|"
                    , Box.literal src
                    , Box.punc "|]"
                    ]
            )


type LetDeclaration
    = LetCommonDeclaration (A.Located Src.Def)
    | LetComment Src.FComment


formatCommentedExpression : ImportInfo -> Src.C2 Src.Expr -> Box
formatCommentedExpression importInfo ( pre, post, e ) =
    let
        commented_ =
            -- TODO
            -- case e of
            --     Src.Parens (C ( pre__, post__ ) e__) ->
            --         ( pre ++ pre__, e__, post__ ++ post )
            --     _ ->
            ( pre, post, e )
    in
    formatCommented <| Src.c2map (syntaxParens SyntaxSeparated << formatExpression importInfo) commented_


formatPreCommentedExpression : ImportInfo -> SyntaxContext -> Src.C1 Src.Expr -> Box
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


formatRecordLike : Maybe (Src.C2 Box) -> List (Src.C2Eol Box) -> Src.FComments -> Src.ForceMultiline -> Box
formatRecordLike base_ fields trailing multiline =
    case ( base_, fields ) of
        ( Just base, pairs_ ) ->
            ElmStructure.extensionGroup_
                ((\(Src.ForceMultiline b) -> b) multiline)
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


formatSequence : Char -> Char -> Maybe Char -> Src.ForceMultiline -> Src.FComments -> List (Src.C2Eol Box) -> Box
formatSequence left delim maybeRight (Src.ForceMultiline multiline) trailing list =
    case ( maybeRight, list ) of
        ( _, first :: rest ) ->
            let
                formatItem : Char -> Src.C2Eol Box -> Box
                formatItem delim_ ( ( pre, post, eol ), item ) =
                    Maybe.unwrap identity (Box.stack_ << Box.stack_ Box.blankLine) (formatComments pre) <|
                        Box.prefix (Box.row [ Box.punc (String.fromChar delim_), Box.space ]) <|
                            formatC2Eol ( ( post, [], eol ), item )
            in
            ElmStructure.forceableSpaceSepOrStack multiline
                (ElmStructure.forceableRowOrStack multiline
                    (formatItem left first)
                    (List.map (formatItem delim) rest)
                )
                (Maybe.unwrap [] (flip (::) [] << Box.stack_ Box.blankLine) (formatComments trailing) ++ Maybe.toList (Maybe.map (Box.line << Box.punc << String.fromChar) maybeRight))

        ( Just right, [] ) ->
            formatUnit left right trailing

        ( Nothing, [] ) ->
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
    = BinopsClause Src.FComments varRef Src.FComments expr


formatBinops : ImportInfo -> Src.Expr -> List (BinopsClause (Ref (List String)) Src.Expr) -> Bool -> Box
formatBinops importInfo left ops multiline =
    let
        formatPair_ : Bool -> BinopsClause (Ref (List String)) Src.Expr -> ( ( Bool, Src.FComments, Box ), Box )
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


formatUnit : Char -> Char -> Src.FComments -> Box
formatUnit left right comments =
    case ( left, comments ) of
        ( _, [] ) ->
            Box.line <| Box.punc (String.fromList [ left, right ])

        ( '{', (Src.LineComment _) :: _ ) ->
            surround left right <| Box.prefix Box.space <| Box.stack1 <| List.map formatComment comments

        _ ->
            surround left right <|
                case Box.allSingles <| List.map formatComment comments of
                    Ok comments_ ->
                        Box.line <| Box.row <| List.intersperse Box.space comments_

                    Err comments_ ->
                        Box.stack1 comments_


formatComments : Src.FComments -> Maybe Box
formatComments comments =
    case List.map formatComment comments of
        [] ->
            Nothing

        first :: rest ->
            Just (ElmStructure.spaceSepOrStack first rest)


formatCommented_ : Bool -> Src.C2 Box -> Box
formatCommented_ forceMultiline ( pre, post, inner ) =
    ElmStructure.forceableSpaceSepOrStack1 forceMultiline <|
        List.concat
            [ Maybe.toList (formatComments pre)
            , [ inner ]
            , Maybe.toList (formatComments post)
            ]


formatCommented : Src.C2 Box -> Box
formatCommented =
    formatCommented_ False


formatPreCommented : Src.C1 Box -> Box
formatPreCommented ( pre, inner ) =
    formatCommentedApostrophe pre inner


formatCommentedApostrophe : Src.FComments -> Box -> Box
formatCommentedApostrophe pre inner =
    formatCommented ( pre, [], inner )


formatTailCommented : Src.C1 Box -> Box
formatTailCommented ( post, inner ) =
    formatCommented ( [], post, inner )


formatC2Eol : Src.C2Eol Box -> Box
formatC2Eol ( ( pre, post, eol ), a ) =
    formatCommented ( pre, post, formatEolCommented ( eol, a ) )


formatEolCommented : ( Maybe String, Box ) -> Box
formatEolCommented ( post, inner ) =
    case ( post, inner ) of
        ( Nothing, box ) ->
            box

        ( Just eol, Box.SingleLine result ) ->
            Box.mustBreak <| Box.row [ result, Box.space, Box.punc "--", Box.literal eol ]

        ( Just eol, box ) ->
            Box.stack1 [ box, formatComment <| Src.LineComment eol ]


formatCommentedStack : Src.C2 Box -> Box
formatCommentedStack ( pre, post, inner ) =
    Box.stack1 <|
        List.map formatComment pre
            ++ inner
            :: List.map formatComment post


formatPreCommentedStack : Src.C1 Box -> Box
formatPreCommentedStack ( pre, inner ) =
    formatCommentedStack ( pre, [], inner )


formatKeywordCommented : String -> Src.C2 Box -> Box
formatKeywordCommented word ( pre, post, value ) =
    ElmStructure.spaceSepOrIndented
        (formatCommented ( pre, post, Box.line (Box.keyword word) ))
        [ value ]


formatOpenCommentedList : Src.OpenCommentedList Box -> List Box
formatOpenCommentedList (Src.OpenCommentedList rest ( preLst, eol, lst )) =
    List.map formatC2Eol rest
        ++ [ formatC2Eol ( ( preLst, [], eol ), lst ) ]


formatComment : Src.FComment -> Box
formatComment comment =
    case comment of
        Src.BlockComment c ->
            case c of
                [] ->
                    Box.line <| Box.punc "{- -}"

                [ l ] ->
                    Box.line <|
                        Box.row
                            [ Box.punc "{-"
                            , Box.space
                            , Box.literal (String.trim l)
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

        Src.LineComment c ->
            Box.mustBreak <| Box.row [ Box.punc "--", Box.literal c ]

        Src.CommentTrickOpener ->
            Box.mustBreak <| Box.punc "{--}"

        Src.CommentTrickCloser ->
            Box.mustBreak <| Box.punc "--}"

        Src.CommentTrickBlock c ->
            Box.mustBreak <| Box.row [ Box.punc "{--", Box.literal c, Box.punc "-}" ]


type FloatRepresentation
    = DecimalFloat
    | ExponentFloat


type StringRepresentation
    = SingleQuotedString
    | TripleQuotedString


type LiteralValue
    = IntNum String
    | FloatNum String
    | Boolean Bool


formatLiteral : LiteralValue -> Box
formatLiteral lit =
    case lit of
        IntNum i ->
            let
                number =
                    if String.startsWith "0x" i then
                        "0x" ++ String.toUpper (String.dropLeft 2 i)

                    else
                        i
            in
            Box.line (Box.literal number)

        FloatNum f ->
            Box.line (Box.literal f)

        Boolean True ->
            Box.line <| Box.literal "True"

        Boolean False ->
            Box.line <| Box.literal "False"


type StringStyle
    = SChar
    | SString StringRepresentation


charIsPrint : Char -> Bool
charIsPrint c =
    case c of
        '\u{2028}' ->
            False

        '\u{2029}' ->
            False

        _ ->
            True


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
        stringBox : String -> (String -> String) -> Box
        stringBox quotes escaper =
            Box.line <|
                Box.row
                    [ Box.punc quotes
                    , Box.literal <| escaper <| String.concat <| List.map fix <| String.toList <| String.replace "\\n" "\n" s
                    , Box.punc quotes
                    ]

        fix : Char -> String
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

            else if not (charIsPrint c) then
                hex c

            else if c == ' ' then
                String.fromChar c

            else if charIsSpace c then
                hex c

            else
                String.fromChar c

        hex char =
            "\\u{" ++ String.padLeft 4 '0' (Hex.toString (Char.toCode char)) ++ "}"

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


formatTypeConstructor : String -> Box
formatTypeConstructor name =
    Box.line <| formatQualifiedUppercaseIdentifier (String.split "." name)


formatType : Src.Type -> ( TypeParensInner, Box )
formatType (A.At region atype) =
    case Debug.log "atype" atype of
        Src.TLambda arg result ->
            let
                first : Src.C0Eol Src.Type
                first =
                    Debug.log "first" arg

                rest : List (Src.C2Eol Src.Type)
                rest =
                    [ result ]

                forceMultiline =
                    -- TODO
                    False

                formatRight : Src.C2Eol Src.Type -> Box
                formatRight ( ( preOp, postOp, eol ), term ) =
                    let
                        _ =
                            Debug.log "formatRight" ( preOp, postOp, eol )
                    in
                    ElmStructure.forceableSpaceSepOrStack1 False <|
                        List.concat
                            [ Maybe.toList <| formatComments preOp
                            , [ ElmStructure.prefixOrIndented
                                    (Box.line <| Box.punc "->")
                                    (formatC2Eol <|
                                        (Src.c2EolMap <| typeParens ForLambda << formatType)
                                            ( ( postOp, [], eol ), term )
                                    )
                              ]
                            ]
            in
            Tuple.pair ForFunctionType <|
                ElmStructure.forceableSpaceSepOrStack
                    forceMultiline
                    (formatEolCommented (Src.c0EolMap (typeParens ForLambda << formatType) first))
                    (List.map formatRight rest)

        Src.TVar name ->
            ( NotNeeded
            , Box.line <|
                Box.identifier <|
                    formatVarName name
            )

        Src.TType _ ctor args ->
            let
                forceMultiline =
                    -- TODO
                    Src.ForceMultiline False

                join =
                    case forceMultiline of
                        Src.ForceMultiline True ->
                            ElmStructure.FASplitFirst

                        Src.ForceMultiline False ->
                            ElmStructure.FAJoinFirst ElmStructure.JoinAll
            in
            ( if List.isEmpty args then
                NotNeeded

              else
                ForTypeConstruction
            , ElmStructure.application
                join
                (formatTypeConstructor ctor)
                (List.map (formatPreCommented << Src.c1map (typeParens ForCtor << formatType)) args)
            )

        Src.TTypeQual _ home name args ->
            let
                forceMultiline =
                    -- TODO
                    Src.ForceMultiline False

                join =
                    case forceMultiline of
                        Src.ForceMultiline True ->
                            ElmStructure.FASplitFirst

                        Src.ForceMultiline False ->
                            ElmStructure.FAJoinFirst ElmStructure.JoinAll
            in
            ( if List.isEmpty args then
                NotNeeded

              else
                ForTypeConstruction
            , ElmStructure.application
                join
                (formatTypeConstructor (home ++ "." ++ name))
                (List.map (formatPreCommented << Src.c1map (typeParens ForCtor << formatType)) args)
            )

        Src.TRecord fields ext trailing ->
            let
                base =
                    Maybe.map (Src.c2map A.toValue) ext

                fields_ : List (Src.C2Eol (Src.Pair Name Src.Type))
                fields_ =
                    List.map
                        (\( preFieldComments, postFieldComments, ( ( postNameComments, A.At _ name ), ( preTypeComments, typ ) ) ) ->
                            ( ( preFieldComments, postFieldComments, Nothing )
                            , Src.Pair ( postNameComments, name ) ( preTypeComments, typ ) (Src.ForceMultiline False)
                            )
                        )
                        fields

                multiline =
                    Src.ForceMultiline (A.isMultiline region)
            in
            ( NotNeeded
            , formatRecordLike
                (Maybe.map (Src.c2map (Box.line << formatLowercaseIdentifier [])) base)
                (List.map (Src.c2EolMap (formatPair ":" << Src.mapPair (formatLowercaseIdentifier []) (typeParens NotRequired << formatType))) fields_)
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
            , ElmStructure.group True "(" "," ")" forceMultiline (List.map (formatC2Eol << Src.c2EolMap (typeParens NotRequired << formatType)) types)
            )


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
