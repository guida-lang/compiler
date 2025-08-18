module Common.Format.Render.Box exposing (formatModule)

import Basics.Extra exposing (flip)
import Common.Format.Box as Box exposing (Box)
import Common.Format.Cheapskate.Parse as Parse
import Common.Format.Cheapskate.Types exposing (Block(..), Blocks, Doc(..), LinkTarget(..), Options(..))
import Common.Format.ImportInfo as ImportInfo exposing (ImportInfo)
import Common.Format.KnownContents as KnownContents
import Common.Format.Render.ElmStructure as ElmStructure
import Common.Format.Render.Markdown as Markdown
import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Name exposing (Name)
import Compiler.Parse.Declaration as Decl
import Compiler.Parse.Expression as Expr
import Compiler.Parse.Module as M
import Compiler.Parse.Primitives as P
import Compiler.Parse.SyntaxVersion as SV
import Compiler.Reporting.Annotation as A
import Data.Map as Map exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Hex
import Maybe.Extra as Maybe
import Utils.Crash exposing (crash)


type alias Module =
    { importInfo : ImportInfo
    , initialComments : Src.FComments
    , header : Maybe Header
    , docs : A.Located (Maybe Blocks)
    , imports : Src.C1 (Dict (List String) (List Name) (Src.C1 ImportMethod))
    , body : List (TopLevelStructure Declaration)
    }


type Header
    = Header SourceTag (Src.C2 (List Name)) (Maybe (Src.C2 SourceSettings)) (Maybe (Src.C2 (Listing DetailedListing)))


defaultHeader : Header
defaultHeader =
    Header Normal ( ( [], [] ), [ "Main" ] ) Nothing Nothing


type alias SourceSettings =
    List ( Src.C2 Name, Src.C2 Name )


type SourceTag
    = Normal
    | Effect Src.FComments
    | Port Src.FComments


type alias UserImport =
    ( Src.C1 (List Name), ImportMethod )


type ImportMethod
    = ImportMethod (Maybe (Src.C2 Name)) (Src.C2 (Listing DetailedListing))


{-| A listing of values. Something like (a,b,c) or (..) or (a,b,..)
-}
type Listing a
    = ExplicitListing a Bool
    | OpenListing (Src.C2 ())
    | ClosedListing


type alias DetailedListing =
    { values : CommentedMap Name ()
    , operators : CommentedMap Name ()
    , types : CommentedMap Name (Src.C1 (Listing (CommentedMap Name ())))
    }


type alias CommentedMap k v =
    Dict String k (Src.C2 v)


{-| A value that can be imported or exported
-}
type Value
    = Value Name
    | OpValue Name
    | Union (Src.C1 Name) (Listing (CommentedMap Name ()))


type Declaration
    = CommonDeclaration CommonDeclaration
    | Datatype (Src.C2 (NameWithArgs Name Name)) (Src.OpenCommentedList (NameWithArgs Name Src.Type))
    | TypeAlias Src.FComments (Src.C2 (NameWithArgs Name Name)) (Src.C1 Src.Type)
    | PortAnnotation (Src.C2 Name) Src.FComments Src.Type
    | Fixity (Src.C1 Binop.Associativity) (Src.C1 Binop.Precedence) (Src.C2 Name) (Src.C1 Name)


type CommonDeclaration
    = Definition Src.Pattern (List (Src.C1 Src.Pattern)) Src.FComments Src.Expr
    | TypeAnnotation (Src.C1 (Ref ())) (Src.C1 Src.Type)


type NameWithArgs name arg
    = NameWithArgs name (List (Src.C1 arg))


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


pairs : List a -> List ( a, a )
pairs input =
    let
        step : a -> ( Maybe b, List ( a, b ) ) -> ( Maybe a, List ( a, b ) )
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


pleaseReport__ : String -> String -> String
pleaseReport__ what details =
    -- TODO: include version in the message
    "<elm-format: " ++ what ++ ": " ++ details ++ " -- please report this at https://github.com/guida-lang/compiler/issues>"


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


removeDuplicates : List (List (Src.C2 Value)) -> List (List (Src.C2 Value))
removeDuplicates input =
    let
        step :
            List (Src.C2 Value)
            -> ( List (List (Src.C2 Value)), EverySet String (Src.C2 Value) )
            -> ( List (List (Src.C2 Value)), EverySet String (Src.C2 Value) )
        step next ( acc, seen ) =
            case List.foldl stepChildren ( [], seen ) next |> (\( a, b ) -> ( List.reverse a, b )) of
                ( [], seen_ ) ->
                    ( acc, seen_ )

                ( children_, seen_ ) ->
                    ( children_ :: acc, seen_ )

        varName : Src.C2 Value -> Name
        varName var =
            case var of
                ( _, Value name ) ->
                    name

                ( _, OpValue name ) ->
                    name

                ( _, Union ( _, name ) _ ) ->
                    name

        stepChildren :
            Src.C2 Value
            -> ( List (Src.C2 Value), EverySet String (Src.C2 Value) )
            -> ( List (Src.C2 Value), EverySet String (Src.C2 Value) )
        stepChildren next ( acc, seen ) =
            if EverySet.member varName next seen then
                ( acc, seen )

            else
                ( next :: acc, EverySet.insert varName next seen )
    in
    List.foldl step ( [], EverySet.empty ) input
        |> Tuple.first
        |> List.reverse


sortVars : Bool -> EverySet String (Src.C2 Value) -> List (List String) -> ( List (List (Src.C2 Value)), Src.FComments )
sortVars forceMultiline fromExposing fromDocs =
    let
        varOrder : Src.C2 Value -> ( Int, String )
        varOrder ( _, value ) =
            case value of
                OpValue name ->
                    ( 1, name )

                Union ( _, name ) _ ->
                    ( 2, name )

                Value name ->
                    ( 3, name )

        listedInDocs : List (List (Src.C2 Value))
        listedInDocs =
            fromDocs
                |> List.map (List.filterMap (\v -> Map.get identity v allowedInDocs))
                |> List.filter (not << List.isEmpty)
                |> List.map (List.map (Tuple.pair ( [], [] )))
                |> removeDuplicates

        listedInExposing : List (Src.C2 Value)
        listedInExposing =
            fromExposing
                |> EverySet.toList (\a b -> compare (varName a) (varName b))
                |> List.sortBy varOrder

        varName : Src.C2 Value -> String
        varName ( _, value ) =
            case value of
                Value name ->
                    name

                OpValue name ->
                    name

                Union ( _, name ) _ ->
                    name

        varSetToMap : EverySet String (Src.C2 Value) -> Dict String String Value
        varSetToMap set =
            EverySet.toList (\a b -> compare (varName a) (varName b)) set
                |> List.map (\( c, var ) -> ( varName ( c, var ), var ))
                |> Map.fromList identity

        allowedInDocs : Dict String String Value
        allowedInDocs =
            varSetToMap fromExposing

        allFromDocs : EverySet String String
        allFromDocs =
            EverySet.fromList identity <| List.map varName <| List.concat listedInDocs

        inDocs : Src.C2 Value -> Bool
        inDocs x =
            EverySet.member identity (varName x) allFromDocs

        remainingFromExposing : List (Src.C2 Value)
        remainingFromExposing =
            listedInExposing
                |> List.filter (not << inDocs)

        commentsFromReorderedVars : Src.FComments
        commentsFromReorderedVars =
            listedInExposing
                |> List.filter inDocs
                |> List.concatMap (\( ( pre, post ), _ ) -> pre ++ post)
    in
    if List.isEmpty listedInDocs && forceMultiline then
        ( List.map (\x -> [ x ]) remainingFromExposing, commentsFromReorderedVars )

    else
        ( listedInDocs
            ++ (if List.isEmpty remainingFromExposing then
                    []

                else
                    [ remainingFromExposing ]
               )
        , commentsFromReorderedVars
        )


formatModuleHeader : Bool -> Module -> List Box
formatModuleHeader addDefaultHeader modu =
    let
        maybeHeader : Maybe Header
        maybeHeader =
            if addDefaultHeader then
                Just (Maybe.withDefault defaultHeader modu.header)

            else
                modu.header

        refName : Ref (List String) -> String
        refName ref =
            case ref of
                VarRef _ name ->
                    name

                TagRef _ name ->
                    name

                OpRef name ->
                    name

        varName : Src.C2 Value -> Name
        varName var =
            case var of
                ( _, Value name ) ->
                    name

                ( _, OpValue name ) ->
                    name

                ( _, Union ( _, name ) _ ) ->
                    name

        documentedVars : List (List String)
        documentedVars =
            modu.docs
                |> A.toValue
                |> Maybe.withDefault []
                |> List.concatMap extractDocs

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
                (c :: _) as s ->
                    if Char.isUpper c then
                        TagRef [] text

                    else if Char.isLower c then
                        VarRef [] text

                    else
                        case s of
                            [ '(', a, ')' ] ->
                                OpRef (String.fromChar a)

                            [ '(', a, b, ')' ] ->
                                OpRef (String.fromList [ a, b ])

                            _ ->
                                VarRef [] text

                _ ->
                    VarRef [] text

        exportsList : Listing DetailedListing
        exportsList =
            case Maybe.withDefault defaultHeader maybeHeader of
                Header _ _ _ (Just ( _, e )) ->
                    e

                Header _ _ _ Nothing ->
                    ClosedListing

        detailedListingToSet : Listing DetailedListing -> EverySet String (Src.C2 Value)
        detailedListingToSet value =
            case value of
                OpenListing _ ->
                    EverySet.empty

                ClosedListing ->
                    EverySet.empty

                ExplicitListing { values, operators, types } _ ->
                    let
                        toComparable : Src.C2 Value -> Name
                        toComparable ( _, v ) =
                            case v of
                                OpValue name ->
                                    name

                                Union ( _, name ) _ ->
                                    name

                                Value name ->
                                    name
                    in
                    List.foldl EverySet.union EverySet.empty <|
                        [ Map.toList compare values |> List.map (\( name, ( c, () ) ) -> ( c, Value name )) |> EverySet.fromList toComparable
                        , Map.toList compare operators |> List.map (\( name, ( c, () ) ) -> ( c, OpValue name )) |> EverySet.fromList toComparable
                        , Map.toList compare types |> List.map (\( name, ( c, ( preListing, listing ) ) ) -> ( c, Union ( preListing, name ) listing )) |> EverySet.fromList toComparable
                        ]

        detailedListingIsMultiline : Listing a -> Bool
        detailedListingIsMultiline listing =
            case listing of
                ExplicitListing _ isMultiline ->
                    isMultiline

                _ ->
                    False

        varsToExpose : EverySet String (Src.C2 Value)
        varsToExpose =
            case Maybe.andThen (\(Header _ _ _ exports) -> exports) maybeHeader of
                Nothing ->
                    let
                        definedVars : EverySet String (Src.C2 Value)
                        definedVars =
                            modu.body
                                |> List.concatMap extractVarName
                                |> List.map (Tuple.pair ( [], [] ))
                                |> EverySet.fromList
                                    (\( _, value ) ->
                                        case value of
                                            Value name ->
                                                name

                                            OpValue name ->
                                                name

                                            Union ( _, name ) _ ->
                                                name
                                    )
                    in
                    if List.all List.isEmpty documentedVars then
                        definedVars

                    else
                        let
                            documentedVarsSet : EverySet String String
                            documentedVarsSet =
                                EverySet.fromList identity (List.concat documentedVars)
                        in
                        definedVars |> EverySet.filter (\v -> EverySet.member identity (varName v) documentedVarsSet)

                Just ( _, e ) ->
                    detailedListingToSet e

        sortedExports : ( List (List (Src.C2 Value)), Src.FComments )
        sortedExports =
            sortVars
                (detailedListingIsMultiline exportsList)
                varsToExpose
                documentedVars

        extractVarName : TopLevelStructure Declaration -> List Value
        extractVarName decl =
            case decl of
                DocComment _ ->
                    []

                BodyComment _ ->
                    []

                Entry (PortAnnotation ( _, name ) _ _) ->
                    [ Value name ]

                Entry (CommonDeclaration def) ->
                    case def of
                        Definition (A.At _ pat) _ _ _ ->
                            case pat of
                                Src.PVar name ->
                                    [ Value name ]

                                Src.PRecord ( _, fields ) ->
                                    List.map (\( _, A.At _ field ) -> Value field) fields

                                _ ->
                                    []

                        _ ->
                            []

                Entry (Datatype ( _, NameWithArgs name _ ) _) ->
                    [ Union ( [], name ) (OpenListing ( ( [], [] ), () )) ]

                Entry (TypeAlias _ ( _, NameWithArgs name _ ) _) ->
                    [ Union ( [], name ) ClosedListing ]

                Entry _ ->
                    []

        formatModuleLine_ : Header -> Box
        formatModuleLine_ (Header srcTag name moduleSettings exports) =
            let
                ( preExposing, postExposing ) =
                    case exports of
                        Nothing ->
                            ( [], [] )

                        Just ( ( pre, post ), _ ) ->
                            ( pre, post )
            in
            formatModuleLine sortedExports srcTag name moduleSettings preExposing postExposing

        docs : Maybe Box
        docs =
            Maybe.map (formatDocComment modu.importInfo) <| A.toValue <| modu.docs

        imports : List Box
        imports =
            formatImports modu
    in
    List.concat
        (List.intersperse [ Box.blankLine ] <|
            List.concat
                [ Maybe.toList (Maybe.map (List.singleton << formatModuleLine_) maybeHeader)
                , Maybe.toList (Maybe.map List.singleton docs)
                , if List.isEmpty imports then
                    []

                  else
                    [ imports ]
                ]
        )


formatImports : Module -> List Box
formatImports modu =
    let
        ( comments, imports ) =
            modu.imports
    in
    [ formatComments comments
        |> Maybe.toList
    , imports
        |> Map.toList compare
        |> List.map (\( name, ( pre, method ) ) -> formatImport ( ( pre, name ), method ))
    ]
        |> List.filter (not << List.isEmpty)
        |> List.intersperse [ Box.blankLine ]
        |> List.concat


formatModuleLine :
    ( List (List (Src.C2 Value)), Src.FComments )
    -> SourceTag
    -> Src.C2 (List Name)
    -> Maybe (Src.C2 SourceSettings)
    -> Src.FComments
    -> Src.FComments
    -> Box
formatModuleLine ( varsToExpose, extraComments ) srcTag name moduleSettings preExposing postExposing =
    let
        tag : Box
        tag =
            case srcTag of
                Normal ->
                    Box.line (Box.keyword "module")

                Port comments ->
                    ElmStructure.spaceSepOrIndented
                        (formatTailCommented ( comments, Box.line <| Box.keyword "port" ))
                        [ Box.line (Box.keyword "module") ]

                Effect comments ->
                    ElmStructure.spaceSepOrIndented
                        (formatTailCommented ( comments, Box.line <| Box.keyword "effect" ))
                        [ Box.line (Box.keyword "module") ]

        exports : Box
        exports =
            case varsToExpose of
                [] ->
                    Box.line <| Box.keyword "(..)"

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
            moduleSettings
                |> Maybe.map (formatKeywordCommented "where" << Src.c2map formatSettings)
                |> Maybe.map (\x -> [ x ])
                |> Maybe.withDefault []

        nameClause : Box
        nameClause =
            case
                ( tag
                , formatCommented <| Src.c2map (Box.line << formatQualifiedUppercaseIdentifier) name
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
            (whereClause ++ [ formatCommented ( ( preExposing, postExposing ), Box.line <| Box.keyword "exposing" ) ])
        )
        [ exports ]


formatModule : Bool -> Int -> M.Module -> Box
formatModule addDefaultHeader spacing modu =
    formatModule_ addDefaultHeader spacing (formatModu modu)


formatModu : M.Module -> Module
formatModu modu =
    let
        declarations : List (TopLevelStructure Declaration)
        declarations =
            List.concatMap declToDeclarations modu.decls

        ( moduleHeaderComments, imports ) =
            List.foldl
                (\( comments, Src.Import ( importNameComments, A.At _ importName ) maybeAlias exposing_ ) ( importCommentsAcc, ( importComments, importsAcc ) ) ->
                    let
                        exposedVars : Src.C2 (Listing DetailedListing)
                        exposedVars =
                            Src.c2map
                                (\exposing__ ->
                                    case exposing__ of
                                        Src.Open preComments postComments ->
                                            OpenListing ( ( preComments, postComments ), () )

                                        Src.Explicit (A.At _ []) ->
                                            ClosedListing

                                        Src.Explicit (A.At region exposed) ->
                                            ExplicitListing
                                                (List.foldl
                                                    (\( entryComments, entry ) acc ->
                                                        case entry of
                                                            Src.Lower (A.At _ name) ->
                                                                { acc | values = Map.insert identity name ( entryComments, () ) acc.values }

                                                            Src.Upper (A.At _ name) ( privacyComments, Src.Public _ ) ->
                                                                { acc | types = Map.insert identity name ( entryComments, ( privacyComments, OpenListing ( ( [], [] ), () ) ) ) acc.types }

                                                            Src.Upper (A.At _ name) ( privacyComments, Src.Private ) ->
                                                                { acc | types = Map.insert identity name ( entryComments, ( privacyComments, ClosedListing ) ) acc.types }

                                                            Src.Operator _ name ->
                                                                { acc | operators = Map.insert identity name ( entryComments, () ) acc.operators }
                                                    )
                                                    { values = Map.empty
                                                    , operators = Map.empty
                                                    , types = Map.empty
                                                    }
                                                    exposed
                                                )
                                                (A.isMultiline region)
                                )
                                exposing_
                    in
                    ( comments
                    , ( importComments ++ importCommentsAcc
                      , Map.insert identity (String.split "." importName) ( importNameComments, ImportMethod maybeAlias exposedVars ) importsAcc
                      )
                    )
                )
                ( [], Src.c1map (\_ -> Map.empty) modu.imports )
                (Src.c1Value modu.imports)

        body : List (TopLevelStructure Declaration)
        body =
            List.map BodyComment moduleHeaderComments
                ++ List.concatMap
                    (\( commments, A.At _ (Src.Infix op associativity precedence name) ) ->
                        Entry (Fixity associativity precedence op name) :: List.map BodyComment commments
                    )
                    (List.reverse modu.infixes)
                ++ declarations
    in
    { importInfo = ImportInfo.fromModule KnownContents.mempty modu
    , initialComments = modu.initialComments
    , header =
        Maybe.map
            (\header ->
                let
                    ( sourceTag, sourceSettings ) =
                        case header.effects of
                            M.NoEffects _ ->
                                ( Normal, Nothing )

                            M.Ports _ comments ->
                                ( Port comments, Nothing )

                            M.Manager _ comments ( postWhereComments, manager ) ->
                                ( Effect comments
                                , Just
                                    ( ( [], postWhereComments )
                                    , case manager of
                                        Src.Cmd ( ( preCmdComments, postCmdComments ), ( ( preEqualComments, postEqualComments ), A.At _ cmdType ) ) ->
                                            [ ( ( ( preCmdComments, preEqualComments ), "command" ), ( ( postEqualComments, postCmdComments ), cmdType ) )
                                            ]

                                        Src.Sub ( ( preSubComments, postSubComments ), ( ( preEqualComments, postEqualComments ), A.At _ subType ) ) ->
                                            [ ( ( ( preSubComments, preEqualComments ), "subscription" ), ( ( postEqualComments, postSubComments ), subType ) )
                                            ]

                                        Src.Fx ( ( preCmdComments, postCmdComments ), ( ( preEqualCmdComments, postEqualCmdComments ), A.At (A.Region (A.Position cmdTypeStart cmdTypeEnd) _) cmdType ) ) ( ( preSubComments, postSubComments ), ( ( preEqualSubComments, postEqualSubComments ), A.At (A.Region (A.Position subTypeStart subTypeEnd) _) subType ) ) ->
                                            [ ( ( cmdTypeStart, cmdTypeEnd ), ( ( ( preCmdComments, preEqualCmdComments ), "command" ), ( ( postEqualCmdComments, postCmdComments ), cmdType ) ) )
                                            , ( ( subTypeStart, subTypeEnd ), ( ( ( preSubComments, preEqualSubComments ), "subscription" ), ( ( postEqualSubComments, postSubComments ), subType ) ) )
                                            ]
                                                |> List.sortBy Tuple.first
                                                |> List.map Tuple.second
                                    )
                                )

                    exportsListing : Src.C2 (Listing DetailedListing)
                    exportsListing =
                        Src.c2map
                            (\(A.At _ exposing_) ->
                                case exposing_ of
                                    Src.Open preComments postComments ->
                                        OpenListing ( ( preComments, postComments ), () )

                                    Src.Explicit (A.At region exposed) ->
                                        ExplicitListing
                                            (List.foldl
                                                (\( entryComments, entry ) acc ->
                                                    case entry of
                                                        Src.Lower (A.At _ name) ->
                                                            { acc | values = Map.insert identity name ( entryComments, () ) acc.values }

                                                        Src.Upper (A.At _ name) ( privacyComments, Src.Public _ ) ->
                                                            { acc | types = Map.insert identity name ( entryComments, ( privacyComments, OpenListing ( ( [], [] ), () ) ) ) acc.types }

                                                        Src.Upper (A.At _ name) ( privacyComments, Src.Private ) ->
                                                            { acc | types = Map.insert identity name ( entryComments, ( privacyComments, ClosedListing ) ) acc.types }

                                                        Src.Operator _ name ->
                                                            { acc | operators = Map.insert identity name ( entryComments, () ) acc.operators }
                                                )
                                                { values = Map.empty
                                                , operators = Map.empty
                                                , types = Map.empty
                                                }
                                                exposed
                                            )
                                            (A.isMultiline region)
                            )
                            header.exports
                in
                Header sourceTag (Src.c2map (String.split "." << A.toValue) header.name) sourceSettings (Just exportsListing)
            )
            modu.header
    , docs =
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
            |> A.At A.zero
    , imports = imports
    , body = body
    }


declToDeclarations : Src.C2 Decl.Decl -> List (TopLevelStructure Declaration)
declToDeclarations ( ( preDeclComments, postDeclComments ), decl ) =
    List.map BodyComment preDeclComments
        ++ (case decl of
                Decl.Value maybeDocs (A.At _ (Src.Value preValueComments ( postNameComments, A.At nameRegion name ) srcArgs ( valueBodyComments, valueBody ) maybeType)) ->
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
                        ++ (maybeType
                                |> Maybe.map
                                    (\( postComments, ( ( preTypComments, postTypeComments ), typ ) ) ->
                                        Entry (CommonDeclaration (TypeAnnotation ( preTypComments, VarRef () name ) ( postTypeComments, typ )))
                                            :: List.map BodyComment postComments
                                    )
                                |> Maybe.withDefault []
                           )
                        ++ [ Entry (CommonDeclaration (Definition (A.At nameRegion (Src.PVar name)) srcArgs (postNameComments ++ valueBodyComments) valueBody)) ]

                Decl.Union maybeDocs (A.At _ (Src.Union ( nameComments, A.At _ name ) args constructors)) ->
                    let
                        ( postTagsComments, tags ) =
                            case List.reverse constructors of
                                ( ( preLastConstructorComments, postLastConstructorComments, _ ), ( A.At _ lastName, lastArgs ) ) :: restConstructors ->
                                    ( postLastConstructorComments
                                    , Src.OpenCommentedList
                                        (List.map
                                            (\( comments, ( A.At _ constructorName, constructorArgs ) ) ->
                                                ( comments, NameWithArgs constructorName constructorArgs )
                                            )
                                            (List.reverse restConstructors)
                                        )
                                        ( preLastConstructorComments, Nothing, NameWithArgs lastName lastArgs )
                                    )

                                -- Note: the following case should not occur, since
                                -- it is invalid to have a union type without constructors.
                                _ ->
                                    crash "union type without constructors"
                    in
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
                        ++ Entry (Datatype ( nameComments, NameWithArgs name (List.map (Src.c1map A.toValue) args) ) tags)
                        :: List.map BodyComment postTagsComments

                Decl.Alias maybeDocs (A.At _ (Src.Alias comments name args tipe)) ->
                    let
                        nameWithArgs : Src.C2 (NameWithArgs Name Name)
                        nameWithArgs =
                            Src.c2map
                                (\(A.At _ n) ->
                                    NameWithArgs n (List.map (Src.c1map A.toValue) args)
                                )
                                name
                    in
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
                        ++ [ Entry (TypeAlias comments nameWithArgs tipe) ]

                Decl.Port maybeDocs (Src.Port comments name tipe) ->
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
                        ++ [ Entry (PortAnnotation (Src.c2map A.toValue name) comments tipe) ]
           )
        ++ List.map BodyComment postDeclComments


formatModule_ : Bool -> Int -> Module -> Box
formatModule_ addDefaultHeader spacing modu =
    let
        initialComments_ : List Box
        initialComments_ =
            case modu.initialComments of
                [] ->
                    []

                comments ->
                    List.map formatComment comments
                        ++ [ Box.blankLine, Box.blankLine ]

        spaceBeforeBody : Int
        spaceBeforeBody =
            case modu.body of
                [] ->
                    0

                (BodyComment _) :: _ ->
                    spacing + 1

                _ ->
                    spacing

        decls : List (TopLevelStructure Declaration)
        decls =
            modu.body
    in
    Box.stack1 <|
        List.concat
            [ initialComments_
            , formatModuleHeader addDefaultHeader modu
            , List.repeat spaceBeforeBody Box.blankLine
            , Maybe.toList <| formatModuleBody spacing modu.importInfo decls
            ]


formatModuleBody : Int -> ImportInfo -> List (TopLevelStructure Declaration) -> Maybe Box
formatModuleBody linesBetween importInfo body =
    let
        entryType : Declaration -> BodyEntryType
        entryType adecl =
            case adecl of
                CommonDeclaration def ->
                    case def of
                        Definition (A.At _ pat) _ _ _ ->
                            case pat of
                                Src.PVar name ->
                                    BodyNamed <| VarRef () name

                                _ ->
                                    BodyUnnamed

                        TypeAnnotation ( _, name ) _ ->
                            BodyNamed name

                Datatype ( _, NameWithArgs name _ ) _ ->
                    BodyNamed <| TagRef () name

                TypeAlias _ ( _, NameWithArgs name _ ) _ ->
                    BodyNamed <| TagRef () name

                PortAnnotation ( _, name ) _ _ ->
                    BodyNamed <| VarRef () name

                Fixity _ _ _ _ ->
                    BodyFixity
    in
    formatTopLevelBody linesBetween importInfo <|
        List.map (topLevelStructureMap (\b -> ( entryType b, formatDeclaration importInfo b ))) body


type BodyEntryType
    = BodyNamed (Ref ())
    | BodyUnnamed
    | BodyFixity


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
            intersperseMap (\a b -> extraLines <| spacer a b)
                (formatTopLevelStructure importInfo << topLevelStructureMap Tuple.second)
                body
    in
    case boxes of
        [] ->
            Nothing

        _ ->
            Just <| Box.stack1 boxes


type ElmCodeBlock
    = DeclarationsCode (List (TopLevelStructure Declaration))
    | ExpressionsCode (List (TopLevelStructure (Src.C0Eol Src.Expr)))
    | ModuleCode Module


formatDocComment : ImportInfo -> Blocks -> Box
formatDocComment importInfo blocks =
    let
        parse : String -> Maybe ElmCodeBlock
        parse source =
            source
                |> Maybe.oneOf
                    [ Maybe.map DeclarationsCode << Result.toMaybe << parseDeclarations
                    , Maybe.map ExpressionsCode << Result.toMaybe << parseExpressions
                    , Maybe.map ModuleCode << Result.toMaybe << parseModule
                    ]

        format : ElmCodeBlock -> String
        format result =
            case result of
                ModuleCode modu ->
                    formatModule_ False 1 modu
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
                    ElmDocs <|
                        (List.map << List.map)
                            (String.replace "(..)" "")
                            docs

                _ ->
                    block
    in
    formatDocCommentString content


parseDeclarations : String -> Result () (List (TopLevelStructure Declaration))
parseDeclarations source =
    -- TODO/FIXME SyntaxVersion
    P.fromByteString (P.specialize (\_ -> Tuple.pair) (Decl.declaration SV.Guida)) Tuple.pair source
        |> Result.mapError (\_ -> ())
        |> Result.map (\( decl, _ ) -> declToDeclarations decl)


parseExpressions : String -> Result () (List (TopLevelStructure (Src.C0Eol Src.Expr)))
parseExpressions source =
    -- TODO/FIXME SyntaxVersion
    P.fromByteString (P.specialize (\_ -> Tuple.pair) (Expr.expression SV.Guida)) Tuple.pair source
        |> Result.mapError (\_ -> ())
        |> Result.map (\( ( _, expr ), _ ) -> [ Entry ( Nothing, expr ) ])


parseModule : String -> Result () Module
parseModule source =
    -- TODO/FIXME SyntaxVersion
    P.fromByteString (P.specialize (\_ -> Tuple.pair) (M.chompModule SV.Guida M.Application)) Tuple.pair source
        |> Result.mapError (\_ -> ())
        |> Result.map formatModu


formatDocCommentString : String -> Box
formatDocCommentString docs =
    case lines docs of
        [] ->
            Box.line <| Box.row [ Box.punc "{-|", Box.space, Box.punc "-}" ]

        [ first ] ->
            Box.stack1
                [ Box.line <| Box.row [ Box.punc "{-|", Box.space, Box.literal first ]
                , Box.line <| Box.punc "-}"
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


formatImport : UserImport -> Box
formatImport ( ( _, rawName ) as name, (ImportMethod _ exposedVars) as method ) =
    let
        requestedAs : Maybe (Src.C2 Name)
        requestedAs =
            case method of
                ImportMethod ((Just ( _, aliasName )) as other) _ ->
                    if [ aliasName ] == rawName then
                        Nothing

                    else
                        other

                ImportMethod other _ ->
                    other

        asVar : Maybe Box
        asVar =
            requestedAs
                |> Maybe.map
                    (formatImportClause
                        (Just << Box.line << formatUppercaseIdentifier)
                        "as"
                    )
                |> Maybe.join

        exposingVar : Maybe Box
        exposingVar =
            formatImportClause
                (formatListing formatDetailedListing)
                "exposing"
                exposedVars

        formatImportClause : (a -> Maybe Box) -> String -> Src.C2 a -> Maybe Box
        formatImportClause format keyw input =
            case Src.c2map format input of
                ( ( [], [] ), Nothing ) ->
                    Nothing

                ( ( preKeyword, postKeyword ), Just listing_ ) ->
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
        ( formatPreCommented <| Src.c1map (Box.line << formatQualifiedUppercaseIdentifier) name
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


formatListing : (a -> List Box) -> Listing a -> Maybe Box
formatListing format listing =
    case listing of
        ClosedListing ->
            Nothing

        OpenListing ( comments, () ) ->
            Just <| parens <| formatCommented <| ( comments, Box.line <| Box.keyword ".." )

        ExplicitListing vars multiline ->
            case format vars of
                [] ->
                    Nothing

                vars_ ->
                    Just <| ElmStructure.group False "(" "," ")" multiline vars_



-- formatExposing : (List (Src.C2 Src.Exposed) -> List Box) -> Src.Exposing -> Maybe Box
-- formatExposing format listing =
--     case listing of
--         Src.Open preComments postComments ->
--             Just (parens (formatCommented ( preComments, postComments, Box.line (Box.keyword "..") )))
--         Src.Explicit (A.At _ []) ->
--             Nothing
--         Src.Explicit (A.At region exposedList) ->
--             case format exposedList of
--                 [] ->
--                     Nothing
--                 vars_ ->
--                     let
--                         multiline =
--                             A.isMultiline region
--                     in
--                     Just <| ElmStructure.group False "(" "," ")" multiline vars_


formatDetailedListing : DetailedListing -> List Box
formatDetailedListing listing =
    List.concat
        [ formatCommentedMap compare
            (\name () -> OpValue name)
            formatVarValue
            listing.operators
        , formatCommentedMap compare
            (\name ( inner, listing_ ) -> Union ( inner, name ) listing_)
            formatVarValue
            listing.types
        , formatCommentedMap compare
            (\name () -> Value name)
            formatVarValue
            listing.values
        ]


formatCommentedMap : (k -> k -> Order) -> (k -> v -> a) -> (a -> Box) -> CommentedMap k v -> List Box
formatCommentedMap keyComparison construct format values =
    let
        format_ : ( k, Src.C2 v ) -> Box
        format_ ( k, ( c, v ) ) =
            formatCommented ( c, format (construct k v) )
    in
    values
        |> Map.toList keyComparison
        |> List.map format_


formatVarValue : Value -> Box
formatVarValue aval =
    case aval of
        Value val ->
            Box.line <| formatLowercaseIdentifier [] val

        OpValue name ->
            Box.line <| Box.identifier <| "(" ++ name ++ ")"

        Union name listing ->
            case
                ( formatListing
                    (formatCommentedMap compare
                        (\name_ () -> name_)
                        (Box.line << formatUppercaseIdentifier)
                    )
                    listing
                , formatTailCommented <| Src.c1map (Box.line << formatUppercaseIdentifier) name
                , (\( c, _ ) -> c) name
                )
            of
                ( Just _, _, _ ) ->
                    formatTailCommented <|
                        Src.c1map (\n -> Box.line <| Box.row [ formatUppercaseIdentifier n, Box.keyword "(..)" ])
                            name

                ( Nothing, name_, _ ) ->
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


formatCommonDeclaration : ImportInfo -> CommonDeclaration -> Box
formatCommonDeclaration importInfo decl =
    case decl of
        Definition name args comments expr ->
            formatDefinition importInfo name args comments expr

        TypeAnnotation name typ ->
            formatTypeAnnotation name typ


formatDeclaration : ImportInfo -> Declaration -> Box
formatDeclaration importInfo decl =
    case decl of
        CommonDeclaration def ->
            formatCommonDeclaration importInfo def

        Datatype nameWithArgs tags ->
            let
                ctor : NameWithArgs Name Src.Type -> Box
                ctor (NameWithArgs tag args_) =
                    case Box.allSingles <| List.map (formatPreCommented << Src.c1map (typeParens ForCtor << formatType)) args_ of
                        Ok args__ ->
                            Box.line <| Box.row <| List.intersperse Box.space <| formatUppercaseIdentifier tag :: args__

                        Err [] ->
                            Box.line <| formatUppercaseIdentifier tag

                        Err args__ ->
                            Box.stack1
                                [ Box.line <| formatUppercaseIdentifier tag
                                , Box.stack1 args__
                                    |> Box.indent
                                ]
            in
            case
                formatOpenCommentedList <| Src.openCommentedListMap ctor tags
            of
                [] ->
                    crash "List can't be empty"

                first :: rest ->
                    case formatCommented <| Src.c2map formatNameWithArgs nameWithArgs of
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

        TypeAlias preAlias nameWithArgs typ ->
            ElmStructure.definition "="
                True
                (Box.line <| Box.keyword "type")
                [ formatPreCommented ( preAlias, Box.line <| Box.keyword "alias" )
                , formatCommented <| Src.c2map formatNameWithArgs nameWithArgs
                ]
                (formatPreCommentedStack <| Src.c1map (typeParens NotRequired << formatType) typ)

        PortAnnotation name typeComments typ ->
            ElmStructure.definition ":"
                False
                (Box.line <| Box.keyword "port")
                [ formatCommented <| Src.c2map (Box.line << formatLowercaseIdentifier []) name ]
                (formatCommentedApostrophe typeComments <| typeParens NotRequired <| formatType typ)

        Fixity assoc precedence name value ->
            let
                formatAssoc : Binop.Associativity -> Box.Line
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
                (Box.line <| Box.keyword "infix")
                [ formatPreCommented <| Src.c1map (Box.line << formatAssoc) assoc
                , formatPreCommented <| Src.c1map (Box.line << Box.literal << String.fromInt) precedence
                , formatCommented <| Src.c2map (Box.line << formatSymbolIdentifierInParens) name
                , Box.line <| Box.keyword "="
                , formatPreCommented <| Src.c1map (Box.line << Box.identifier << formatVarName) value
                ]


formatNameWithArgs : NameWithArgs Name Name -> Box
formatNameWithArgs (NameWithArgs name args) =
    case Box.allSingles <| List.map (formatPreCommented << Src.c1map (Box.line << formatLowercaseIdentifier [])) args of
        Ok args_ ->
            Box.line <| Box.row <| List.intersperse Box.space (formatUppercaseIdentifier name :: args_)

        Err args_ ->
            Box.stack1 <|
                (Box.line <| formatUppercaseIdentifier name)
                    :: List.map Box.indent args_


formatDefinition : ImportInfo -> Src.Pattern -> List (Src.C1 Src.Pattern) -> Src.FComments -> Src.Expr -> Box
formatDefinition importInfo (A.At _ name) args comments expr =
    let
        body : Box
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
            , ElmStructure.group True "{" "," "}" False (List.map (formatCommented << Src.c2map (Box.line << formatLowercaseIdentifier [] << A.toValue)) (List.reverse fields))
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
                ctor : List Name
                ctor =
                    [ name ]
            in
            ( SyntaxSeparated
            , Box.line (formatQualifiedUppercaseIdentifier ctor)
            )

        Src.PCtor _ name patterns ->
            let
                ctor : List Name
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
                ctor : List String
                ctor =
                    String.split "." home ++ [ name ]
            in
            ( SyntaxSeparated
            , Box.line (formatQualifiedUppercaseIdentifier ctor)
            )

        Src.PCtorQual _ home name patterns ->
            let
                ctor : List String
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
                go : List (Src.C2Eol Src.Pattern) -> Src.C2Eol Src.Pattern -> List (Src.C2Eol Src.Pattern)
                go acc p =
                    case p of
                        ( comments, A.At _ (Src.PCons ( _, hd_ ) tl_) ) ->
                            go (( comments, hd_ ) :: acc) tl_

                        _ ->
                            List.reverse (p :: acc)

                rest : List (Src.C2Eol Src.Pattern)
                rest =
                    go [] tl

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
                (List.map formatRight rest)
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

        Src.PParens ( ( [], [] ), A.At _ pattern ) ->
            formatPattern pattern

        Src.PParens pattern ->
            ( SyntaxSeparated
            , parens (formatCommented (Src.c2map (syntaxParens SyntaxSeparated << formatPattern << A.toValue) pattern))
            )


formatRecordPair : String -> (v -> Box) -> ( Src.C2 String, Src.C2 v, Bool ) -> Box
formatRecordPair delim formatValue ( ( ( pre, postK ), k ), v, forceMultiline ) =
    ElmStructure.equalsPair delim
        forceMultiline
        (formatCommented <| Src.c2map (Box.line << formatLowercaseIdentifier []) ( ( [], postK ), k ))
        (formatCommented <| Src.c2map formatValue v)
        |> Tuple.pair pre
        |> formatPreCommented


formatPair : String -> Src.Pair Box.Line Box -> Box
formatPair delim (Src.Pair a b (Src.ForceMultiline forceMultiline)) =
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
        parensIf : Bool -> Box -> Box
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
                        (\( currExpr, ( ( preOpComments, postOpComments ), A.At _ currOp ) ) ( leftAcc, clausesAcc ) ->
                            ( currExpr, BinopsClause preOpComments (OpRef currOp) postOpComments leftAcc :: clausesAcc )
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

        Src.Lambda ( trailingComments, srcArgs ) ( exprComments, expr ) ->
            let
                multiline : Bool
                multiline =
                    A.isMultiline region

                bodyComments : Src.FComments
                bodyComments =
                    trailingComments ++ exprComments
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

                ( ( _, Err [] ), _ ) ->
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

                multiline : ElmStructure.FunctionApplicationMultiline
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

        Src.If ( _, if_ ) elseifs ( elsComments, els ) ->
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
                        key : Box
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
                    , Box.indent <| formatCommented_ True <| Src.c2map (syntaxParens SyntaxSeparated << formatExpression importInfo) ( ( elsComments, [] ), els )
                    ]
            )

        Src.Let defs bodyComments expr ->
            let
                letDeclarations : Src.C2 (A.Located Src.Def) -> List LetDeclaration
                letDeclarations ( ( preDefComments, postDefComments ), A.At _ def ) =
                    let
                        ( typeAnnotation, commonDeclaration ) =
                            case def of
                                Src.Define (A.At nameRegion name) srcArgs ( comments, body ) maybeType ->
                                    ( maybeType
                                        |> Maybe.map
                                            (\( postComments, ( ( preTypComments, postTypeComments ), typ ) ) ->
                                                LetCommonDeclaration (TypeAnnotation ( preTypComments, VarRef () name ) ( postTypeComments, typ ))
                                                    :: List.map LetComment postComments
                                            )
                                        |> Maybe.withDefault []
                                    , Definition (A.At nameRegion (Src.PVar name)) srcArgs comments body
                                    )

                                Src.Destruct pattern ( comments, body ) ->
                                    ( [], Definition pattern [] comments body )
                    in
                    List.map LetComment preDefComments
                        ++ typeAnnotation
                        ++ LetCommonDeclaration commonDeclaration
                        :: List.map LetComment postDefComments

                spacer : LetDeclaration -> LetDeclaration -> List Box
                spacer first _ =
                    case first of
                        LetCommonDeclaration (Definition _ _ _ _) ->
                            [ Box.blankLine ]

                        _ ->
                            []

                formatDefinition_ : LetDeclaration -> Box
                formatDefinition_ def =
                    case def of
                        LetCommonDeclaration (Definition name args comments expr_) ->
                            formatDefinition importInfo name args comments expr_

                        LetCommonDeclaration (TypeAnnotation name typ) ->
                            formatTypeAnnotation name typ

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

        Src.Case (( _, A.At subjectRegion _ ) as subject) clauses ->
            let
                opening : Box
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
                clause ( ( ( prePat, postPat ), A.At _ pat ), ( preExpr, expr ) ) =
                    case
                        ( ( postPat
                          , formatPattern pat
                                |> syntaxParens SyntaxSeparated
                                |> negativeCasePatternWorkaround pat
                          )
                        , ( formatCommentedStack (Src.c2map (syntaxParens SyntaxSeparated << formatPattern) ( ( prePat, postPat ), pat ))
                                |> negativeCasePatternWorkaround pat
                          , formatPreCommentedStack <| Src.c1map (syntaxParens SyntaxSeparated << formatExpression importInfo) ( preExpr, expr )
                          )
                        )
                    of
                        ( _, ( Box.SingleLine pat_, body_ ) ) ->
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

                        ( _, ( pat_, body_ ) ) ->
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

        Src.Update name ( trailing, fields ) ->
            let
                multiline : Src.ForceMultiline
                multiline =
                    Src.ForceMultiline (A.isMultiline region)

                fields_ : List (Src.C2Eol (Src.Pair Name Src.Expr))
                fields_ =
                    List.map (Src.c2EolMap (\( ( nameComments, A.At _ name_ ), expr ) -> Src.Pair ( nameComments, name_ ) expr multiline)) fields
            in
            ( SyntaxSeparated
            , formatRecordLike
                (Just (Src.c2map (syntaxParens SyntaxSeparated << formatExpression importInfo) name))
                (List.map (Src.c2EolMap (formatPair "=" << Src.mapPair (formatLowercaseIdentifier []) (syntaxParens SyntaxSeparated << formatExpression importInfo))) fields_)
                trailing
                multiline
            )

        Src.Record ( trailing, fields ) ->
            let
                multiline : Src.ForceMultiline
                multiline =
                    Src.ForceMultiline (A.isMultiline region)

                fields_ : List (Src.C2Eol (Src.Pair Name Src.Expr))
                fields_ =
                    List.map
                        (Src.c2EolMap
                            (\( ( nameComments, A.At nameRegion name_ ), ( _, A.At exprRegion _ ) as expr ) ->
                                Src.Pair ( nameComments, name_ ) expr (Src.ForceMultiline (A.isMultiline (A.mergeRegions nameRegion exprRegion)))
                            )
                        )
                        fields
            in
            ( SyntaxSeparated
            , formatRecordLike Nothing
                (List.map (Src.c2EolMap (formatPair "=" << Src.mapPair (formatLowercaseIdentifier []) (syntaxParens SyntaxSeparated << formatExpression importInfo))) fields_)
                trailing
                multiline
            )

        Src.Unit ->
            ( SyntaxSeparated
            , formatUnit '(' ')' []
            )

        Src.Tuple a b cs ->
            let
                multiline : Bool
                multiline =
                    A.isMultiline region

                exprs : List (Src.C2 Src.Expr)
                exprs =
                    a :: b :: cs
            in
            ( SyntaxSeparated
            , ElmStructure.group True "(" "," ")" multiline <|
                List.map (formatCommentedExpression importInfo) exprs
            )

        Src.Shader (Shader.Source src) _ ->
            ( SyntaxSeparated
            , Box.line <|
                Box.row
                    [ Box.punc "[glsl|"
                    , Box.literal (Shader.unescape src)
                    , Box.punc "|]"
                    ]
            )

        Src.Parens expr ->
            case expr of
                ( ( [], [] ), expr_ ) ->
                    formatExpression importInfo expr_

                _ ->
                    ( SyntaxSeparated
                    , formatCommentedExpression importInfo expr
                        |> parens
                    )


type LetDeclaration
    = LetCommonDeclaration CommonDeclaration
    | LetComment Src.FComment


formatCommentedExpression : ImportInfo -> Src.C2 Src.Expr -> Box
formatCommentedExpression importInfo ( ( pre, post ), e ) =
    let
        commented_ : Src.C2 Src.Expr
        commented_ =
            -- TODO
            -- case e of
            --     Src.Parens (C ( pre__, post__ ) e__) ->
            --         ( pre ++ pre__, e__, post__ ++ post )
            --     _ ->
            ( ( pre, post ), e )
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
                isLeftPipe : Bool
                isLeftPipe =
                    o == OpRef "<|"

                formatContext : SyntaxContext
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
formatCommented_ forceMultiline ( ( pre, post ), inner ) =
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
    formatCommented ( ( pre, [] ), inner )


formatTailCommented : Src.C1 Box -> Box
formatTailCommented ( post, inner ) =
    formatCommented ( ( [], post ), inner )


formatC2Eol : Src.C2Eol Box -> Box
formatC2Eol ( ( pre, post, eol ), a ) =
    formatCommented ( ( pre, post ), formatEolCommented ( eol, a ) )


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
formatCommentedStack ( ( pre, post ), inner ) =
    Box.stack1 <|
        List.map formatComment pre
            ++ inner
            :: List.map formatComment post


formatPreCommentedStack : Src.C1 Box -> Box
formatPreCommentedStack ( pre, inner ) =
    formatCommentedStack ( ( pre, [] ), inner )


formatKeywordCommented : String -> Src.C2 Box -> Box
formatKeywordCommented word ( ( pre, post ), value ) =
    ElmStructure.spaceSepOrIndented
        (formatCommented ( ( pre, post ), Box.line (Box.keyword word) ))
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
                number : String
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


formatString : StringStyle -> String -> Box
formatString style s =
    let
        stringBox : String -> (String -> String) -> Box
        stringBox quotes escaper =
            Box.line <|
                Box.row
                    [ Box.punc quotes
                    , Box.literal <| escaper fixedString
                    , Box.punc quotes
                    ]

        styleBasedFix : String -> String
        styleBasedFix =
            case style of
                SChar ->
                    String.replace "\\\"" "\""
                        >> String.replace "\t" "\\t"

                SString TripleQuotedString ->
                    String.replace "\\n" "\n"
                        >> String.replace "\\\"" "\""
                        >> String.replace "\\'" "'"
                        >> String.replace "\t" "\\t"

                SString SingleQuotedString ->
                    String.replace "\\'" "'"

        fixedString : String
        fixedString =
            s
                |> styleBasedFix
                |> String.replace "\t" "\\t"
                |> String.toList
                |> List.map
                    (\c ->
                        if not (charIsPrint c) then
                            hex c

                        else
                            String.fromChar c
                    )
                |> String.concat

        hex : Char -> String
        hex char =
            "\\u{" ++ String.padLeft 4 '0' (Hex.toString (Char.toCode char)) ++ "}"
    in
    case style of
        SChar ->
            stringBox "'" identity

        SString SingleQuotedString ->
            stringBox "\"" identity

        SString TripleQuotedString ->
            let
                escapeMultiQuote : String -> String
                escapeMultiQuote =
                    let
                        step : String -> Int -> String -> String
                        step okay quotes remaining =
                            case String.toList remaining of
                                [] ->
                                    String.reverse (String.repeat quotes "\"\\" ++ okay)

                                next :: rest ->
                                    if next == '"' then
                                        step okay (quotes + 1) (String.fromList rest)

                                    else if quotes >= 3 then
                                        step (String.cons next (String.repeat quotes "\"\\") ++ okay) 0 (String.fromList rest)

                                    else if quotes > 0 then
                                        step (String.cons next (String.fromList (List.repeat quotes '"') ++ okay)) 0 (String.fromList rest)

                                    else
                                        step (String.cons next okay) 0 (String.fromList rest)
                    in
                    step "" 0
            in
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


formatTypeConstructor : String -> Box
formatTypeConstructor name =
    Box.line <| formatQualifiedUppercaseIdentifier (String.split "." name)


formatType : Src.Type -> ( TypeParensInner, Box )
formatType (A.At region atype) =
    case atype of
        Src.TLambda first result ->
            let
                rest : List (Src.C2Eol Src.Type)
                rest =
                    let
                        go : Src.C2Eol Src.Type -> List (Src.C2Eol Src.Type) -> List (Src.C2Eol Src.Type)
                        go ( comments, type_ ) acc =
                            case type_ of
                                A.At _ (Src.TLambda ( _, subFirst ) subRest) ->
                                    go subRest (acc ++ [ ( comments, subFirst ) ])

                                _ ->
                                    acc ++ [ ( comments, type_ ) ]
                    in
                    go result []

                forceMultiline : Bool
                forceMultiline =
                    A.isMultiline region

                formatRight : Src.C2Eol Src.Type -> Box
                formatRight ( ( preOp, postOp, eol ), term ) =
                    ElmStructure.forceableSpaceSepOrStack1 False <|
                        ((Maybe.toList <| formatComments preOp)
                            ++ [ ElmStructure.prefixOrIndented
                                    (Box.line <| Box.punc "->")
                                    (formatC2Eol <|
                                        (Src.c2EolMap <| typeParens ForLambda << formatType)
                                            ( ( postOp, [], eol ), term )
                                    )
                               ]
                        )
            in
            ( ForFunctionType
            , ElmStructure.forceableSpaceSepOrStack
                forceMultiline
                (formatEolCommented (Src.c0EolMap (typeParens ForLambda << formatType) first))
                (List.map formatRight rest)
            )

        Src.TVar name ->
            ( NotNeeded
            , Box.line <|
                Box.identifier <|
                    formatVarName name
            )

        Src.TType _ ctor args ->
            let
                forceMultiline : Src.ForceMultiline
                forceMultiline =
                    Src.ForceMultiline (A.isMultiline region)

                join : ElmStructure.FunctionApplicationMultiline
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
                forceMultiline : Src.ForceMultiline
                forceMultiline =
                    Src.ForceMultiline (A.isMultiline region)

                join : ElmStructure.FunctionApplicationMultiline
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
                base : Maybe (Src.C2 Name)
                base =
                    Maybe.map (Src.c2map A.toValue) ext

                fields_ : List (Src.C2Eol (Src.Pair Name Src.Type))
                fields_ =
                    List.map
                        (\( ( preFieldComments, postFieldComments ), ( ( postNameComments, A.At _ name ), ( preTypeComments, typ ) ) ) ->
                            ( ( preFieldComments, postFieldComments, Nothing )
                            , Src.Pair ( postNameComments, name ) ( preTypeComments, typ ) (Src.ForceMultiline False)
                            )
                        )
                        fields

                multiline : Src.ForceMultiline
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
            ( NotNeeded
            , formatUnit '(' ')' []
            )

        Src.TTuple a b cs ->
            let
                types : List (Src.C2Eol Src.Type)
                types =
                    a :: b :: cs

                forceMultiline : Bool
                forceMultiline =
                    A.isMultiline region
            in
            ( NotNeeded
            , ElmStructure.group True "(" "," ")" forceMultiline (List.map (formatC2Eol << Src.c2EolMap (typeParens NotRequired << formatType)) types)
            )

        Src.TParens type_ ->
            ( NotNeeded
            , parens <| formatCommented <| Src.c2map (typeParens NotRequired << formatType) type_
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
