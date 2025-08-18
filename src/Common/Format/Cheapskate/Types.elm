module Common.Format.Cheapskate.Types exposing
    ( Block(..)
    , Blocks
    , CodeAttr(..)
    , Doc(..)
    , HtmlTagType(..)
    , Inline(..)
    , Inlines
    , LinkTarget(..)
    , ListType(..)
    , NumWrapper(..)
    , Options(..)
    , ReferenceMap
    )

import Data.Map exposing (Dict)



-- TYPES


{-| Structured representation of a document. The 'Options' affect
how the document is rendered by `toHtml`.
-}
type Doc
    = Doc Options Blocks


{-| Block-level elements.
-}
type Block
    = Para Inlines
    | Header Int Inlines
    | Blockquote Blocks
    | List Bool ListType (List Blocks)
    | CodeBlock CodeAttr String
    | HtmlBlock String
    | HRule
    | ReferencesBlock (List ( String, String, String ))
    | ElmDocs (List (List String))


{-| Attributes for fenced code blocks. 'codeLang' is the
first word of the attribute line, 'codeInfo' is the rest.
-}
type CodeAttr
    = CodeAttr
        { codeLang : String
        , codeInfo : String
        }


type ListType
    = Bullet Char
    | Numbered NumWrapper Int


type NumWrapper
    = PeriodFollowing
    | ParenFollowing


{-| Simple representation of HTML tag.
-}
type HtmlTagType
    = Opening String
    | Closing String
    | SelfClosing String


{-| We operate with sequences instead of lists, because
they allow more efficient appending on to the end.
-}
type alias Blocks =
    List Block


{-| Inline elements.
-}
type Inline
    = Str String
    | Space
    | SoftBreak
    | LineBreak
    | Emph Inlines
    | Strong Inlines
    | Code String
    | Link Inlines LinkTarget {- URL -} String {- title -}
    | Image Inlines String {- URL -} String {- title -}
    | Entity String
    | RawHtml String


type LinkTarget
    = Url String
    | Ref String


type alias Inlines =
    List Inline


type alias ReferenceMap =
    Dict String String ( String, String )


{-| Rendering and parsing options.
-}
type Options
    = Options
        { sanitize : Bool -- ^ Sanitize raw HTML, link/image attributes
        , allowRawHtml : Bool -- ^ Allow raw HTML (if false it gets escaped)
        , preserveHardBreaks : Bool -- ^ Preserve hard line breaks in the source
        , debug : Bool -- ^ Print container structure for debugging
        }
