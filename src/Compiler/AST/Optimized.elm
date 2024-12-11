module Compiler.AST.Optimized exposing
    ( Choice(..)
    , Decider(..)
    , Def(..)
    , Destructor(..)
    , EffectsType(..)
    , Expr(..)
    , Global(..)
    , GlobalGraph(..)
    , LocalGraph(..)
    , Main(..)
    , Node(..)
    , Path(..)
    , addGlobalGraph
    , addKernel
    , addLocalGraph
    , compareGlobal
    , empty
    , globalGraphCodec
    , localGraphCodec
    , toKernelGlobal
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.Kernel as K
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Optimize.DecisionTree as DT
import Compiler.Reporting.Annotation as A
import Compiler.Serialize as S
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Serialize exposing (Codec)
import System.TypeCheck.IO as IO



-- EXPRESSIONS


type Expr
    = Bool Bool
    | Chr String
    | Str String
    | Int Int
    | Float Float
    | VarLocal Name
    | VarGlobal Global
    | VarEnum Global Index.ZeroBased
    | VarBox Global
    | VarCycle IO.Canonical Name
    | VarDebug Name IO.Canonical A.Region (Maybe Name)
    | VarKernel Name Name
    | List (List Expr)
    | Function (List Name) Expr
    | Call Expr (List Expr)
    | TailCall Name (List ( Name, Expr ))
    | If (List ( Expr, Expr )) Expr
    | Let Def Expr
    | Destruct Destructor Expr
    | Case Name Name (Decider Choice) (List ( Int, Expr ))
    | Accessor Name
    | Access Expr Name
    | Update Expr (Dict Name Expr)
    | Record (Dict Name Expr)
    | Unit
    | Tuple Expr Expr (Maybe Expr)
    | Shader Shader.Source (EverySet Name) (EverySet Name)


type Global
    = Global IO.Canonical Name


compareGlobal : Global -> Global -> Order
compareGlobal (Global home1 name1) (Global home2 name2) =
    case compare name1 name2 of
        LT ->
            LT

        EQ ->
            ModuleName.compareCanonical home1 home2

        GT ->
            GT



-- DEFINITIONS


type Def
    = Def Name Expr
    | TailDef Name (List Name) Expr


type Destructor
    = Destructor Name Path


type Path
    = Index Index.ZeroBased Path
    | Field Name Path
    | Unbox Path
    | Root Name



-- BRANCHING


type Decider a
    = Leaf a
    | Chain (List ( DT.Path, DT.Test )) (Decider a) (Decider a)
    | FanOut DT.Path (List ( DT.Test, Decider a )) (Decider a)


type Choice
    = Inline Expr
    | Jump Int



-- OBJECT GRAPH


type GlobalGraph
    = GlobalGraph (Dict Global Node) (Dict Name Int)


type LocalGraph
    = LocalGraph
        (Maybe Main)
        -- PERF profile switching Global to Name
        (Dict Global Node)
        (Dict Name Int)


type Main
    = Static
    | Dynamic Can.Type Expr


type Node
    = Define Expr (EverySet Global)
    | DefineTailFunc (List Name) Expr (EverySet Global)
    | Ctor Index.ZeroBased Int
    | Enum Index.ZeroBased
    | Box
    | Link Global
    | Cycle (List Name) (List ( Name, Expr )) (List Def) (EverySet Global)
    | Manager EffectsType
    | Kernel (List K.Chunk) (EverySet Global)
    | PortIncoming Expr (EverySet Global)
    | PortOutgoing Expr (EverySet Global)


type EffectsType
    = Cmd
    | Sub
    | Fx



-- GRAPHS


empty : GlobalGraph
empty =
    GlobalGraph Dict.empty Dict.empty


addGlobalGraph : GlobalGraph -> GlobalGraph -> GlobalGraph
addGlobalGraph (GlobalGraph nodes1 fields1) (GlobalGraph nodes2 fields2) =
    GlobalGraph
        (Dict.union compareGlobal nodes1 nodes2)
        (Dict.union compare fields1 fields2)


addLocalGraph : LocalGraph -> GlobalGraph -> GlobalGraph
addLocalGraph (LocalGraph _ nodes1 fields1) (GlobalGraph nodes2 fields2) =
    GlobalGraph
        (Dict.union compareGlobal nodes1 nodes2)
        (Dict.union compare fields1 fields2)


addKernel : Name -> List K.Chunk -> GlobalGraph -> GlobalGraph
addKernel shortName chunks (GlobalGraph nodes fields) =
    let
        global : Global
        global =
            toKernelGlobal shortName

        node : Node
        node =
            Kernel chunks (List.foldr addKernelDep EverySet.empty chunks)
    in
    GlobalGraph
        (Dict.insert compareGlobal global node nodes)
        (Dict.union compare (K.countFields chunks) fields)


addKernelDep : K.Chunk -> EverySet Global -> EverySet Global
addKernelDep chunk deps =
    case chunk of
        K.JS _ ->
            deps

        K.ElmVar home name ->
            EverySet.insert compareGlobal (Global home name) deps

        K.JsVar shortName _ ->
            EverySet.insert compareGlobal (toKernelGlobal shortName) deps

        K.ElmField _ ->
            deps

        K.JsField _ ->
            deps

        K.JsEnum _ ->
            deps

        K.Debug ->
            deps

        K.Prod ->
            deps


toKernelGlobal : Name.Name -> Global
toKernelGlobal shortName =
    Global (IO.Canonical Pkg.kernel shortName) Name.dollar



-- ENCODERS and DECODERS


globalGraphCodec : Codec e GlobalGraph
globalGraphCodec =
    Serialize.customType
        (\globalGraphCodecEncoder (GlobalGraph nodes fields) ->
            globalGraphCodecEncoder nodes fields
        )
        |> Serialize.variant2 GlobalGraph (S.assocListDict compareGlobal globalCodec nodeCodec) (S.assocListDict compare Serialize.string Serialize.int)
        |> Serialize.finishCustomType


localGraphCodec : Codec e LocalGraph
localGraphCodec =
    Serialize.customType
        (\localGraphCodecEncoder (LocalGraph main nodes fields) ->
            localGraphCodecEncoder main nodes fields
        )
        |> Serialize.variant3 LocalGraph
            (Serialize.maybe mainCodec)
            (S.assocListDict compareGlobal globalCodec nodeCodec)
            (S.assocListDict compare Serialize.string Serialize.int)
        |> Serialize.finishCustomType


mainCodec : Codec c Main
mainCodec =
    Serialize.customType
        (\staticEncoder dynamicEncoder value ->
            case value of
                Static ->
                    staticEncoder

                Dynamic msgType decoder ->
                    dynamicEncoder msgType decoder
        )
        |> Serialize.variant0 Static
        |> Serialize.variant2 Dynamic Can.typeCodec exprCodec
        |> Serialize.finishCustomType


globalCodec : Codec e Global
globalCodec =
    Serialize.customType
        (\globalCodecEncoder (Global home name) ->
            globalCodecEncoder home name
        )
        |> Serialize.variant2 Global ModuleName.canonicalCodec Serialize.string
        |> Serialize.finishCustomType


nodeCodec : Codec e Node
nodeCodec =
    Serialize.customType
        (\defineEncoder defineTailFuncEncoder ctorEncoder enumEncoder boxEncoder linkEncoder cycleEncoder managerEncoder kernelEncoder portIncomingEncoder portOutgoingEncoder node ->
            case node of
                Define expr deps ->
                    defineEncoder expr deps

                DefineTailFunc argNames body deps ->
                    defineTailFuncEncoder argNames body deps

                Ctor index arity ->
                    ctorEncoder index arity

                Enum index ->
                    enumEncoder index

                Box ->
                    boxEncoder

                Link linkedGlobal ->
                    linkEncoder linkedGlobal

                Cycle names values functions deps ->
                    cycleEncoder names values functions deps

                Manager effectsType ->
                    managerEncoder effectsType

                Kernel chunks deps ->
                    kernelEncoder chunks deps

                PortIncoming decoder deps ->
                    portIncomingEncoder decoder deps

                PortOutgoing encoder deps ->
                    portOutgoingEncoder encoder deps
        )
        |> Serialize.variant2 Define exprCodec (S.everySet compareGlobal globalCodec)
        |> Serialize.variant3 DefineTailFunc (Serialize.list Serialize.string) exprCodec (S.everySet compareGlobal globalCodec)
        |> Serialize.variant2 Ctor Index.zeroBasedCodec Serialize.int
        |> Serialize.variant1 Enum Index.zeroBasedCodec
        |> Serialize.variant0 Box
        |> Serialize.variant1 Link globalCodec
        |> Serialize.variant4 Cycle (Serialize.list Serialize.string) (Serialize.list (Serialize.tuple Serialize.string exprCodec)) (Serialize.list defCodec) (S.everySet compareGlobal globalCodec)
        |> Serialize.variant1 Manager effectsTypeCodec
        |> Serialize.variant2 Kernel (Serialize.list K.chunkCodec) (S.everySet compareGlobal globalCodec)
        |> Serialize.variant2 PortIncoming exprCodec (S.everySet compareGlobal globalCodec)
        |> Serialize.variant2 PortOutgoing exprCodec (S.everySet compareGlobal globalCodec)
        |> Serialize.finishCustomType


exprCodec : Codec e Expr
exprCodec =
    Serialize.customType
        (\boolEncoder chrEncoder strEncoder intEncoder floatEncoder varLocalEncoder varGlobalEncoder varEnumEncoder varBoxEncoder varCycleEncoder varDebugEncoder varKernelEncoder listEncoder functionEncoder callEncoder tailCallEncoder ifEncoder letEncoder destructEncoder caseEncoder accessorEncoder accessEncoder updateEncoder recordEncoder unitEncoder tupleEncoder shaderEncoder expr ->
            case expr of
                Bool value ->
                    boolEncoder value

                Chr value ->
                    chrEncoder value

                Str value ->
                    strEncoder value

                Int value ->
                    intEncoder value

                Float value ->
                    floatEncoder value

                VarLocal value ->
                    varLocalEncoder value

                VarGlobal value ->
                    varGlobalEncoder value

                VarEnum global index ->
                    varEnumEncoder global index

                VarBox value ->
                    varBoxEncoder value

                VarCycle home name ->
                    varCycleEncoder home name

                VarDebug name home region unhandledValueName ->
                    varDebugEncoder name home region unhandledValueName

                VarKernel home name ->
                    varKernelEncoder home name

                List value ->
                    listEncoder value

                Function args body ->
                    functionEncoder args body

                Call func args ->
                    callEncoder func args

                TailCall name args ->
                    tailCallEncoder name args

                If branches final ->
                    ifEncoder branches final

                Let def body ->
                    letEncoder def body

                Destruct destructor body ->
                    destructEncoder destructor body

                Case label root decider jumps ->
                    caseEncoder label root decider jumps

                Accessor field ->
                    accessorEncoder field

                Access record field ->
                    accessEncoder record field

                Update record fields ->
                    updateEncoder record fields

                Record value ->
                    recordEncoder value

                Unit ->
                    unitEncoder

                Tuple a b maybeC ->
                    tupleEncoder a b maybeC

                Shader src attributes uniforms ->
                    shaderEncoder src attributes uniforms
        )
        |> Serialize.variant1 Bool Serialize.bool
        |> Serialize.variant1 Chr Serialize.string
        |> Serialize.variant1 Str Serialize.string
        |> Serialize.variant1 Int Serialize.int
        |> Serialize.variant1 Float Serialize.float
        |> Serialize.variant1 VarLocal Serialize.string
        |> Serialize.variant1 VarGlobal globalCodec
        |> Serialize.variant2 VarEnum globalCodec Index.zeroBasedCodec
        |> Serialize.variant1 VarBox globalCodec
        |> Serialize.variant2 VarCycle ModuleName.canonicalCodec Serialize.string
        |> Serialize.variant4 VarDebug Serialize.string ModuleName.canonicalCodec A.regionCodec (Serialize.maybe Serialize.string)
        |> Serialize.variant2 VarKernel Serialize.string Serialize.string
        |> Serialize.variant1 List (Serialize.list (Serialize.lazy (\() -> exprCodec)))
        |> Serialize.variant2 Function (Serialize.list Serialize.string) (Serialize.lazy (\() -> exprCodec))
        |> Serialize.variant2 Call (Serialize.lazy (\() -> exprCodec)) (Serialize.list (Serialize.lazy (\() -> exprCodec)))
        |> Serialize.variant2 TailCall Serialize.string (Serialize.list (Serialize.tuple Serialize.string (Serialize.lazy (\() -> exprCodec))))
        |> Serialize.variant2 If (Serialize.list (Serialize.tuple (Serialize.lazy (\() -> exprCodec)) (Serialize.lazy (\() -> exprCodec)))) (Serialize.lazy (\() -> exprCodec))
        |> Serialize.variant2 Let defCodec (Serialize.lazy (\() -> exprCodec))
        |> Serialize.variant2 Destruct destructorCodec (Serialize.lazy (\() -> exprCodec))
        |> Serialize.variant4 Case Serialize.string Serialize.string (deciderCodec choiceCodec) (Serialize.list (Serialize.tuple Serialize.int (Serialize.lazy (\() -> exprCodec))))
        |> Serialize.variant1 Accessor Serialize.string
        |> Serialize.variant2 Access (Serialize.lazy (\() -> exprCodec)) Serialize.string
        |> Serialize.variant2 Update (Serialize.lazy (\() -> exprCodec)) (S.assocListDict compare Serialize.string (Serialize.lazy (\() -> exprCodec)))
        |> Serialize.variant1 Record (S.assocListDict compare Serialize.string (Serialize.lazy (\() -> exprCodec)))
        |> Serialize.variant0 Unit
        |> Serialize.variant3 Tuple (Serialize.lazy (\() -> exprCodec)) (Serialize.lazy (\() -> exprCodec)) (Serialize.maybe (Serialize.lazy (\() -> exprCodec)))
        |> Serialize.variant3 Shader Shader.sourceCodec (S.everySet compare Serialize.string) (S.everySet compare Serialize.string)
        |> Serialize.finishCustomType


defCodec : Codec e Def
defCodec =
    Serialize.customType
        (\defCodecEncoder tailDefEncoder value ->
            case value of
                Def name expr ->
                    defCodecEncoder name expr

                TailDef name args expr ->
                    tailDefEncoder name args expr
        )
        |> Serialize.variant2 Def Serialize.string (Serialize.lazy (\() -> exprCodec))
        |> Serialize.variant3 TailDef Serialize.string (Serialize.list Serialize.string) (Serialize.lazy (\() -> exprCodec))
        |> Serialize.finishCustomType


destructorCodec : Codec e Destructor
destructorCodec =
    Serialize.customType
        (\destructorCodecEncoder (Destructor name path) ->
            destructorCodecEncoder name path
        )
        |> Serialize.variant2 Destructor Serialize.string pathCodec
        |> Serialize.finishCustomType


deciderCodec : Codec e a -> Codec e (Decider a)
deciderCodec codec =
    Serialize.customType
        (\leafEncoder chainEncoder fanOutEncoder decider ->
            case decider of
                Leaf value ->
                    leafEncoder value

                Chain testChain success failure ->
                    chainEncoder testChain success failure

                FanOut path edges fallback ->
                    fanOutEncoder path edges fallback
        )
        |> Serialize.variant1 Leaf codec
        |> Serialize.variant3 Chain (Serialize.list (Serialize.tuple DT.pathCodec DT.testCodec)) (Serialize.lazy (\() -> deciderCodec codec)) (Serialize.lazy (\() -> deciderCodec codec))
        |> Serialize.variant3 FanOut DT.pathCodec (Serialize.list (Serialize.tuple DT.testCodec (Serialize.lazy (\() -> deciderCodec codec)))) (Serialize.lazy (\() -> deciderCodec codec))
        |> Serialize.finishCustomType


choiceCodec : Codec e Choice
choiceCodec =
    Serialize.customType
        (\inlineEncoder jumpEncoder choice ->
            case choice of
                Inline value ->
                    inlineEncoder value

                Jump value ->
                    jumpEncoder value
        )
        |> Serialize.variant1 Inline (Serialize.lazy (\() -> exprCodec))
        |> Serialize.variant1 Jump Serialize.int
        |> Serialize.finishCustomType


pathCodec : Codec e Path
pathCodec =
    Serialize.customType
        (\indexEncoder fieldEncoder unboxEncoder rootEncoder path ->
            case path of
                Index index subPath ->
                    indexEncoder index subPath

                Field field subPath ->
                    fieldEncoder field subPath

                Unbox subPath ->
                    unboxEncoder subPath

                Root name ->
                    rootEncoder name
        )
        |> Serialize.variant2 Index Index.zeroBasedCodec (Serialize.lazy (\() -> pathCodec))
        |> Serialize.variant2 Field Serialize.string (Serialize.lazy (\() -> pathCodec))
        |> Serialize.variant1 Unbox (Serialize.lazy (\() -> pathCodec))
        |> Serialize.variant1 Root Serialize.string
        |> Serialize.finishCustomType


effectsTypeCodec : Codec e EffectsType
effectsTypeCodec =
    Serialize.customType
        (\cmdEncoder subEncoder fxEncoder effectsType ->
            case effectsType of
                Cmd ->
                    cmdEncoder

                Sub ->
                    subEncoder

                Fx ->
                    fxEncoder
        )
        |> Serialize.variant0 Cmd
        |> Serialize.variant0 Sub
        |> Serialize.variant0 Fx
        |> Serialize.finishCustomType
