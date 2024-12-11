module Compiler.AST.Source exposing
    ( Alias(..)
    , Comment(..)
    , Def(..)
    , Docs(..)
    , Effects(..)
    , Exposed(..)
    , Exposing(..)
    , Expr
    , Expr_(..)
    , Import(..)
    , Infix(..)
    , Manager(..)
    , Module(..)
    , Pattern
    , Pattern_(..)
    , Port(..)
    , Privacy(..)
    , Type
    , Type_(..)
    , Union(..)
    , Value(..)
    , VarType(..)
    , getImportName
    , getName
    , moduleCodec
    , typeCodec
    )

import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Parse.Primitives as P
import Compiler.Reporting.Annotation as A
import Serialize exposing (Codec)



-- EXPRESSIONS


type alias Expr =
    A.Located Expr_


type Expr_
    = Chr String
    | Str String
    | Int Int
    | Float Float
    | Var VarType Name
    | VarQual VarType Name Name
    | List (List Expr)
    | Op Name
    | Negate Expr
    | Binops (List ( Expr, A.Located Name )) Expr
    | Lambda (List Pattern) Expr
    | Call Expr (List Expr)
    | If (List ( Expr, Expr )) Expr
    | Let (List (A.Located Def)) Expr
    | Case Expr (List ( Pattern, Expr ))
    | Accessor Name
    | Access Expr (A.Located Name)
    | Update (A.Located Name) (List ( A.Located Name, Expr ))
    | Record (List ( A.Located Name, Expr ))
    | Unit
    | Tuple Expr Expr (List Expr)
    | Shader Shader.Source Shader.Types


type VarType
    = LowVar
    | CapVar



-- DEFINITIONS


type Def
    = Define (A.Located Name) (List Pattern) Expr (Maybe Type)
    | Destruct Pattern Expr



-- PATTERN


type alias Pattern =
    A.Located Pattern_


type Pattern_
    = PAnything
    | PVar Name
    | PRecord (List (A.Located Name))
    | PAlias Pattern (A.Located Name)
    | PUnit
    | PTuple Pattern Pattern (List Pattern)
    | PCtor A.Region Name (List Pattern)
    | PCtorQual A.Region Name Name (List Pattern)
    | PList (List Pattern)
    | PCons Pattern Pattern
    | PChr String
    | PStr String
    | PInt Int



-- TYPE


type alias Type =
    A.Located Type_


type Type_
    = TLambda Type Type
    | TVar Name
    | TType A.Region Name (List Type)
    | TTypeQual A.Region Name Name (List Type)
    | TRecord (List ( A.Located Name, Type )) (Maybe (A.Located Name))
    | TUnit
    | TTuple Type Type (List Type)



-- MODULE


type Module
    = Module (Maybe (A.Located Name)) (A.Located Exposing) Docs (List Import) (List (A.Located Value)) (List (A.Located Union)) (List (A.Located Alias)) (List (A.Located Infix)) Effects


getName : Module -> Name
getName (Module maybeName _ _ _ _ _ _ _ _) =
    case maybeName of
        Just (A.At _ name) ->
            name

        Nothing ->
            Name.mainModule


getImportName : Import -> Name
getImportName (Import (A.At _ name) _ _) =
    name


type Import
    = Import (A.Located Name) (Maybe Name.Name) Exposing


type Value
    = Value (A.Located Name) (List Pattern) Expr (Maybe Type)


type Union
    = Union (A.Located Name) (List (A.Located Name)) (List ( A.Located Name, List Type ))


type Alias
    = Alias (A.Located Name) (List (A.Located Name)) Type


type Infix
    = Infix Name Binop.Associativity Binop.Precedence Name


type Port
    = Port (A.Located Name) Type


type Effects
    = NoEffects
    | Ports (List Port)
    | Manager A.Region Manager


type Manager
    = Cmd (A.Located Name)
    | Sub (A.Located Name)
    | Fx (A.Located Name) (A.Located Name)


type Docs
    = NoDocs A.Region
    | YesDocs Comment (List ( Name, Comment ))


type Comment
    = Comment P.Snippet



-- EXPOSING


type Exposing
    = Open
    | Explicit (List Exposed)


type Exposed
    = Lower (A.Located Name)
    | Upper (A.Located Name) Privacy
    | Operator A.Region Name


type Privacy
    = Public A.Region
    | Private



-- ENCODERS and DECODERS


typeCodec : Codec e Type
typeCodec =
    A.locatedCodec type_Codec


type_Codec : Codec e Type_
type_Codec =
    Serialize.customType
        (\tLambdaEncoder tVarEncoder tTypeEncoder tTypeQualEncoder tRecordEncoder tUnitEncoder tTupleEncoder value ->
            case value of
                TLambda arg result ->
                    tLambdaEncoder arg result

                TVar name ->
                    tVarEncoder name

                TType region name args ->
                    tTypeEncoder region name args

                TTypeQual region home name args ->
                    tTypeQualEncoder region home name args

                TRecord fields ext ->
                    tRecordEncoder fields ext

                TUnit ->
                    tUnitEncoder

                TTuple a b cs ->
                    tTupleEncoder a b cs
        )
        |> Serialize.variant2
            TLambda
            (A.locatedCodec (Serialize.lazy (\() -> type_Codec)))
            (A.locatedCodec (Serialize.lazy (\() -> type_Codec)))
        |> Serialize.variant1 TVar Serialize.string
        |> Serialize.variant3
            TType
            A.regionCodec
            Serialize.string
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> type_Codec))))
        |> Serialize.variant4
            TTypeQual
            A.regionCodec
            Serialize.string
            Serialize.string
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> type_Codec))))
        |> Serialize.variant2
            TRecord
            (Serialize.list
                (Serialize.tuple (A.locatedCodec Serialize.string) (A.locatedCodec (Serialize.lazy (\() -> type_Codec))))
            )
            (Serialize.maybe (A.locatedCodec Serialize.string))
        |> Serialize.variant0 TUnit
        |> Serialize.variant3
            TTuple
            (A.locatedCodec (Serialize.lazy (\() -> type_Codec)))
            (A.locatedCodec (Serialize.lazy (\() -> type_Codec)))
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> type_Codec))))
        |> Serialize.finishCustomType


moduleCodec : Codec e Module
moduleCodec =
    Serialize.customType
        (\moduleCodecEncoder (Module maybeName exports docs imports values unions aliases binops effects) ->
            moduleCodecEncoder maybeName exports docs imports values unions aliases binops effects
        )
        |> Serialize.variant9
            Module
            (Serialize.maybe (A.locatedCodec Serialize.string))
            (A.locatedCodec exposingCodec)
            docsCodec
            (Serialize.list importCodec)
            (Serialize.list (A.locatedCodec valueCodec))
            (Serialize.list (A.locatedCodec unionCodec))
            (Serialize.list (A.locatedCodec aliasCodec))
            (Serialize.list (A.locatedCodec infixCodec))
            effectsCodec
        |> Serialize.finishCustomType


exposingCodec : Codec e Exposing
exposingCodec =
    Serialize.customType
        (\openEncoder explicitEncoder value ->
            case value of
                Open ->
                    openEncoder

                Explicit exposedList ->
                    explicitEncoder exposedList
        )
        |> Serialize.variant0 Open
        |> Serialize.variant1 Explicit (Serialize.list exposedCodec)
        |> Serialize.finishCustomType


docsCodec : Codec e Docs
docsCodec =
    Serialize.customType
        (\noDocsEncoder yesDocsEncoder value ->
            case value of
                NoDocs region ->
                    noDocsEncoder region

                YesDocs overview comments ->
                    yesDocsEncoder overview comments
        )
        |> Serialize.variant1 NoDocs A.regionCodec
        |> Serialize.variant2 YesDocs commentCodec (Serialize.list (Serialize.tuple Serialize.string commentCodec))
        |> Serialize.finishCustomType


importCodec : Codec e Import
importCodec =
    Serialize.customType
        (\importCodecEncoder (Import importName maybeAlias exposing_) ->
            importCodecEncoder importName maybeAlias exposing_
        )
        |> Serialize.variant3 Import (A.locatedCodec Serialize.string) (Serialize.maybe Serialize.string) exposingCodec
        |> Serialize.finishCustomType


valueCodec : Codec e Value
valueCodec =
    Serialize.customType
        (\valueCodecEncoder (Value name srcArgs body maybeType) ->
            valueCodecEncoder name srcArgs body maybeType
        )
        |> Serialize.variant4
            Value
            (A.locatedCodec Serialize.string)
            (Serialize.list (A.locatedCodec pattern_Codec))
            (A.locatedCodec expr_Codec)
            (Serialize.maybe (A.locatedCodec type_Codec))
        |> Serialize.finishCustomType


unionCodec : Codec e Union
unionCodec =
    Serialize.customType
        (\unionCodecEncoder (Union name args constructors) ->
            unionCodecEncoder name args constructors
        )
        |> Serialize.variant3
            Union
            (A.locatedCodec Serialize.string)
            (Serialize.list (A.locatedCodec Serialize.string))
            (Serialize.list
                (Serialize.tuple (A.locatedCodec Serialize.string) (Serialize.list (A.locatedCodec type_Codec)))
            )
        |> Serialize.finishCustomType


aliasCodec : Codec e Alias
aliasCodec =
    Serialize.customType
        (\aliasCodecEncoder (Alias name args tipe) ->
            aliasCodecEncoder name args tipe
        )
        |> Serialize.variant3
            Alias
            (A.locatedCodec Serialize.string)
            (Serialize.list (A.locatedCodec Serialize.string))
            (A.locatedCodec type_Codec)
        |> Serialize.finishCustomType


infixCodec : Codec e Infix
infixCodec =
    Serialize.customType
        (\infixCodecEncoder (Infix op associativity precedence name) ->
            infixCodecEncoder op associativity precedence name
        )
        |> Serialize.variant4 Infix Serialize.string Binop.associativityCodec Binop.precedenceCodec Serialize.string
        |> Serialize.finishCustomType


effectsCodec : Codec e Effects
effectsCodec =
    Serialize.customType
        (\noEffectsEncoder portsEncoder managerCodecEncoder value ->
            case value of
                NoEffects ->
                    noEffectsEncoder

                Ports ports ->
                    portsEncoder ports

                Manager region manager ->
                    managerCodecEncoder region manager
        )
        |> Serialize.variant0 NoEffects
        |> Serialize.variant1 Ports (Serialize.list portCodec)
        |> Serialize.variant2 Manager A.regionCodec managerCodec
        |> Serialize.finishCustomType


commentCodec : Codec e Comment
commentCodec =
    Serialize.customType
        (\commentCodecEncoder (Comment snippet) ->
            commentCodecEncoder snippet
        )
        |> Serialize.variant1 Comment P.snippetCodec
        |> Serialize.finishCustomType


portCodec : Codec e Port
portCodec =
    Serialize.customType
        (\portCodecEncoder (Port name tipe) ->
            portCodecEncoder name tipe
        )
        |> Serialize.variant2 Port (A.locatedCodec Serialize.string) (A.locatedCodec type_Codec)
        |> Serialize.finishCustomType


managerCodec : Codec e Manager
managerCodec =
    Serialize.customType
        (\cmdEncoder subEncoder fxEncoder value ->
            case value of
                Cmd cmdType ->
                    cmdEncoder cmdType

                Sub subType ->
                    subEncoder subType

                Fx cmdType subType ->
                    fxEncoder cmdType subType
        )
        |> Serialize.variant1 Cmd (A.locatedCodec Serialize.string)
        |> Serialize.variant1 Sub (A.locatedCodec Serialize.string)
        |> Serialize.variant2 Fx (A.locatedCodec Serialize.string) (A.locatedCodec Serialize.string)
        |> Serialize.finishCustomType


exposedCodec : Codec e Exposed
exposedCodec =
    Serialize.customType
        (\lowerEncoder upperEncoder operatorEncoder value ->
            case value of
                Lower name ->
                    lowerEncoder name

                Upper name dotDotRegion ->
                    upperEncoder name dotDotRegion

                Operator region name ->
                    operatorEncoder region name
        )
        |> Serialize.variant1 Lower (A.locatedCodec Serialize.string)
        |> Serialize.variant2 Upper (A.locatedCodec Serialize.string) privacyCodec
        |> Serialize.variant2 Operator A.regionCodec Serialize.string
        |> Serialize.finishCustomType


privacyCodec : Codec e Privacy
privacyCodec =
    Serialize.customType
        (\publicEncoder privateEncoder value ->
            case value of
                Public region ->
                    publicEncoder region

                Private ->
                    privateEncoder
        )
        |> Serialize.variant1 Public A.regionCodec
        |> Serialize.variant0 Private
        |> Serialize.finishCustomType


patternCodec : Codec e Pattern
patternCodec =
    A.locatedCodec pattern_Codec


pattern_Codec : Codec e Pattern_
pattern_Codec =
    Serialize.customType
        (\pAnythingEncoder pVarEncoder pRecordEncoder pAliasEncoder pUnitEncoder pTupleEncoder pCtorEncoder pCtorQualEncoder pListEncoder pConsEncoder pChrEncoder pStrEncoder pIntEncoder value ->
            case value of
                PAnything ->
                    pAnythingEncoder

                PVar name ->
                    pVarEncoder name

                PRecord fields ->
                    pRecordEncoder fields

                PAlias aliasPattern name ->
                    pAliasEncoder aliasPattern name

                PUnit ->
                    pUnitEncoder

                PTuple a b cs ->
                    pTupleEncoder a b cs

                PCtor nameRegion name patterns ->
                    pCtorEncoder nameRegion name patterns

                PCtorQual nameRegion home name patterns ->
                    pCtorQualEncoder nameRegion home name patterns

                PList patterns ->
                    pListEncoder patterns

                PCons hd tl ->
                    pConsEncoder hd tl

                PChr chr ->
                    pChrEncoder chr

                PStr str ->
                    pStrEncoder str

                PInt int ->
                    pIntEncoder int
        )
        |> Serialize.variant0 PAnything
        |> Serialize.variant1 PVar Serialize.string
        |> Serialize.variant1 PRecord (Serialize.list (A.locatedCodec Serialize.string))
        |> Serialize.variant2
            PAlias
            (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec)))
            (A.locatedCodec Serialize.string)
        |> Serialize.variant0 PUnit
        |> Serialize.variant3
            PTuple
            (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec)))
            (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec)))
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec))))
        |> Serialize.variant3
            PCtor
            A.regionCodec
            Serialize.string
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec))))
        |> Serialize.variant4
            PCtorQual
            A.regionCodec
            Serialize.string
            Serialize.string
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec))))
        |> Serialize.variant1 PList (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec))))
        |> Serialize.variant2
            PCons
            (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec)))
            (A.locatedCodec (Serialize.lazy (\() -> pattern_Codec)))
        |> Serialize.variant1 PChr Serialize.string
        |> Serialize.variant1 PStr Serialize.string
        |> Serialize.variant1 PInt Serialize.int
        |> Serialize.finishCustomType


exprCodec : Codec e Expr
exprCodec =
    A.locatedCodec (Serialize.lazy (\() -> expr_Codec))


expr_Codec : Codec e Expr_
expr_Codec =
    Serialize.customType
        (\chrEncoder strEncoder intEncoder floatEncoder varEncoder varQualEncoder listEncoder opEncoder negateEncoder binopsEncoder lambdaEncoder callEncoder ifEncoder letEncoder caseEncoder accessorEncoder accessEncoder updateEncoder recordEncoder unitEncoder tupleEncoder shaderEncoder value ->
            case value of
                Chr char ->
                    chrEncoder char

                Str string ->
                    strEncoder string

                Int int ->
                    intEncoder int

                Float float ->
                    floatEncoder float

                Var varType name ->
                    varEncoder varType name

                VarQual varType prefix name ->
                    varQualEncoder varType prefix name

                List list ->
                    listEncoder list

                Op op ->
                    opEncoder op

                Negate expr ->
                    negateEncoder expr

                Binops ops final ->
                    binopsEncoder ops final

                Lambda srcArgs body ->
                    lambdaEncoder srcArgs body

                Call func args ->
                    callEncoder func args

                If branches finally ->
                    ifEncoder branches finally

                Let defs expr ->
                    letEncoder defs expr

                Case expr branches ->
                    caseEncoder expr branches

                Accessor field ->
                    accessorEncoder field

                Access record field ->
                    accessEncoder record field

                Update name fields ->
                    updateEncoder name fields

                Record fields ->
                    recordEncoder fields

                Unit ->
                    unitEncoder

                Tuple a b cs ->
                    tupleEncoder a b cs

                Shader src tipe ->
                    shaderEncoder src tipe
        )
        |> Serialize.variant1 Chr Serialize.string
        |> Serialize.variant1 Str Serialize.string
        |> Serialize.variant1 Int Serialize.int
        |> Serialize.variant1 Float Serialize.float
        |> Serialize.variant2 Var varTypeCodec Serialize.string
        |> Serialize.variant3 VarQual varTypeCodec Serialize.string Serialize.string
        |> Serialize.variant1 List (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
        |> Serialize.variant1 Op Serialize.string
        |> Serialize.variant1 Negate (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2
            Binops
            (Serialize.list
                (Serialize.tuple (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))) (A.locatedCodec Serialize.string))
            )
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2
            Lambda
            (Serialize.list (A.locatedCodec pattern_Codec))
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2
            Call
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
        |> Serialize.variant2
            If
            (Serialize.list
                (Serialize.tuple
                    (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
                    (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
                )
            )
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2
            Let
            (Serialize.list (A.locatedCodec defCodec))
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
        |> Serialize.variant2
            Case
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (Serialize.list
                (Serialize.tuple (A.locatedCodec pattern_Codec) (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
            )
        |> Serialize.variant1 Accessor Serialize.string
        |> Serialize.variant2
            Access
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (A.locatedCodec Serialize.string)
        |> Serialize.variant2
            Update
            (A.locatedCodec Serialize.string)
            (Serialize.list
                (Serialize.tuple (A.locatedCodec Serialize.string) (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
            )
        |> Serialize.variant1
            Record
            (Serialize.list
                (Serialize.tuple (A.locatedCodec Serialize.string) (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
            )
        |> Serialize.variant0 Unit
        |> Serialize.variant3
            Tuple
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (A.locatedCodec (Serialize.lazy (\() -> expr_Codec)))
            (Serialize.list (A.locatedCodec (Serialize.lazy (\() -> expr_Codec))))
        |> Serialize.variant2 Shader Shader.sourceCodec Shader.typesCodec
        |> Serialize.finishCustomType


varTypeCodec : Codec e VarType
varTypeCodec =
    Serialize.customType
        (\lowVarEncoder capVarEncoder value ->
            case value of
                LowVar ->
                    lowVarEncoder

                CapVar ->
                    capVarEncoder
        )
        |> Serialize.variant0 LowVar
        |> Serialize.variant0 CapVar
        |> Serialize.finishCustomType


defCodec : Codec e Def
defCodec =
    Serialize.customType
        (\defineEncoder destructEncoder value ->
            case value of
                Define name srcArgs body maybeType ->
                    defineEncoder name srcArgs body maybeType

                Destruct pattern body ->
                    destructEncoder pattern body
        )
        |> Serialize.variant4 Define (A.locatedCodec Serialize.string) (Serialize.list patternCodec) exprCodec (Serialize.maybe typeCodec)
        |> Serialize.variant2 Destruct patternCodec exprCodec
        |> Serialize.finishCustomType
