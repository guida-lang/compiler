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
    , globalGraphDecoder
    , globalGraphEncoder
    , localGraphDecoder
    , localGraphEncoder
    , toComparableGlobal
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
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import System.TypeCheck.IO as IO
import Utils.Bytes.Decode as BD
import Utils.Bytes.Encode as BE



-- EXPRESSIONS


type Expr
    = Bool A.Region Bool
    | Chr A.Region String
    | Str A.Region String
    | Int A.Region Int
    | Float A.Region Float
    | VarLocal Name
    | TrackedVarLocal A.Region Name
    | VarGlobal A.Region Global
    | VarEnum A.Region Global Index.ZeroBased
    | VarBox A.Region Global
    | VarCycle A.Region IO.Canonical Name
    | VarDebug A.Region Name IO.Canonical (Maybe Name)
    | VarKernel A.Region Name Name
    | List A.Region (List Expr)
    | Function (List Name) Expr
    | TrackedFunction (List (A.Located Name)) Expr
    | Call A.Region Expr (List Expr)
    | TailCall Name (List ( Name, Expr ))
    | If (List ( Expr, Expr )) Expr
    | Let Def Expr
    | Destruct Destructor Expr
    | Case Name Name (Decider Choice) (List ( Int, Expr ))
    | Accessor A.Region Name
    | Access Expr A.Region Name
    | Update A.Region Expr (Dict String (A.Located Name) Expr)
    | Record (Dict String Name Expr)
    | TrackedRecord A.Region (Dict String (A.Located Name) Expr)
    | Unit
    | Tuple A.Region Expr Expr (List Expr)
    | Shader Shader.Source (EverySet String Name) (EverySet String Name)


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


toComparableGlobal : Global -> List String
toComparableGlobal (Global home name) =
    ModuleName.toComparableCanonical home ++ [ name ]



-- DEFINITIONS


type Def
    = Def A.Region Name Expr
    | TailDef A.Region Name (List (A.Located Name)) Expr


type Destructor
    = Destructor Name Path


type Path
    = Index Index.ZeroBased Path
    | ArrayIndex Int Path
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
    = GlobalGraph (Dict (List String) Global Node) (Dict String Name Int)


type LocalGraph
    = LocalGraph
        (Maybe Main)
        -- PERF profile switching Global to Name
        (Dict (List String) Global Node)
        (Dict String Name Int)


type Main
    = Static
    | Dynamic Can.Type Expr


type Node
    = Define Expr (EverySet (List String) Global)
    | TrackedDefine A.Region Expr (EverySet (List String) Global)
    | DefineTailFunc A.Region (List (A.Located Name)) Expr (EverySet (List String) Global)
    | Ctor Index.ZeroBased Int
    | Enum Index.ZeroBased
    | Box
    | Link Global
    | Cycle (List Name) (List ( Name, Expr )) (List Def) (EverySet (List String) Global)
    | Manager EffectsType
    | Kernel (List K.Chunk) (EverySet (List String) Global)
    | PortIncoming Expr (EverySet (List String) Global)
    | PortOutgoing Expr (EverySet (List String) Global)


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
        (Dict.union nodes1 nodes2)
        (Dict.union fields1 fields2)


addLocalGraph : LocalGraph -> GlobalGraph -> GlobalGraph
addLocalGraph (LocalGraph _ nodes1 fields1) (GlobalGraph nodes2 fields2) =
    GlobalGraph
        (Dict.union nodes1 nodes2)
        (Dict.union fields1 fields2)


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
        (Dict.insert toComparableGlobal global node nodes)
        (Dict.union (K.countFields chunks) fields)


addKernelDep : K.Chunk -> EverySet (List String) Global -> EverySet (List String) Global
addKernelDep chunk deps =
    case chunk of
        K.JS _ ->
            deps

        K.ElmVar home name ->
            EverySet.insert toComparableGlobal (Global home name) deps

        K.JsVar shortName _ ->
            EverySet.insert toComparableGlobal (toKernelGlobal shortName) deps

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


globalGraphEncoder : GlobalGraph -> BE.Encoder
globalGraphEncoder (GlobalGraph nodes fields) =
    BE.sequence
        [ BE.assocListDict compareGlobal globalEncoder nodeEncoder nodes
        , BE.assocListDict compare BE.string BE.int fields
        ]


globalGraphDecoder : BD.Decoder GlobalGraph
globalGraphDecoder =
    BD.map2 GlobalGraph
        (BD.assocListDict toComparableGlobal globalDecoder nodeDecoder)
        (BD.assocListDict identity BD.string BD.int)


localGraphEncoder : LocalGraph -> BE.Encoder
localGraphEncoder (LocalGraph main nodes fields) =
    BE.sequence
        [ BE.maybe mainEncoder main
        , BE.assocListDict compareGlobal globalEncoder nodeEncoder nodes
        , BE.assocListDict compare BE.string BE.int fields
        ]


localGraphDecoder : BD.Decoder LocalGraph
localGraphDecoder =
    BD.map3 LocalGraph
        (BD.maybe mainDecoder)
        (BD.assocListDict toComparableGlobal globalDecoder nodeDecoder)
        (BD.assocListDict identity BD.string BD.int)


mainEncoder : Main -> BE.Encoder
mainEncoder main_ =
    case main_ of
        Static ->
            BE.unsignedInt8 0

        Dynamic msgType decoder ->
            BE.sequence
                [ BE.unsignedInt8 1
                , Can.typeEncoder msgType
                , exprEncoder decoder
                ]


mainDecoder : BD.Decoder Main
mainDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Static

                    1 ->
                        BD.map2 Dynamic
                            Can.typeDecoder
                            exprDecoder

                    _ ->
                        BD.fail
            )


globalEncoder : Global -> BE.Encoder
globalEncoder (Global home name) =
    BE.sequence
        [ ModuleName.canonicalEncoder home
        , BE.string name
        ]


globalDecoder : BD.Decoder Global
globalDecoder =
    BD.map2 Global
        ModuleName.canonicalDecoder
        BD.string


nodeEncoder : Node -> BE.Encoder
nodeEncoder node =
    case node of
        Define expr deps ->
            BE.sequence
                [ BE.unsignedInt8 0
                , exprEncoder expr
                , BE.everySet compareGlobal globalEncoder deps
                ]

        TrackedDefine region expr deps ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder region
                , exprEncoder expr
                , BE.everySet compareGlobal globalEncoder deps
                ]

        DefineTailFunc region argNames body deps ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.regionEncoder region
                , BE.list (A.locatedEncoder BE.string) argNames
                , exprEncoder body
                , BE.everySet compareGlobal globalEncoder deps
                ]

        Ctor index arity ->
            BE.sequence
                [ BE.unsignedInt8 3
                , Index.zeroBasedEncoder index
                , BE.int arity
                ]

        Enum index ->
            BE.sequence
                [ BE.unsignedInt8 4
                , Index.zeroBasedEncoder index
                ]

        Box ->
            BE.unsignedInt8 5

        Link linkedGlobal ->
            BE.sequence
                [ BE.unsignedInt8 6
                , globalEncoder linkedGlobal
                ]

        Cycle names values functions deps ->
            BE.sequence
                [ BE.unsignedInt8 7
                , BE.list BE.string names
                , BE.list (BE.jsonPair BE.string exprEncoder) values
                , BE.list defEncoder functions
                , BE.everySet compareGlobal globalEncoder deps
                ]

        Manager effectsType ->
            BE.sequence
                [ BE.unsignedInt8 8
                , effectsTypeEncoder effectsType
                ]

        Kernel chunks deps ->
            BE.sequence
                [ BE.unsignedInt8 9
                , BE.list K.chunkEncoder chunks
                , BE.everySet compareGlobal globalEncoder deps
                ]

        PortIncoming decoder deps ->
            BE.sequence
                [ BE.unsignedInt8 10
                , exprEncoder decoder
                , BE.everySet compareGlobal globalEncoder deps
                ]

        PortOutgoing encoder deps ->
            BE.sequence
                [ BE.unsignedInt8 11
                , exprEncoder encoder
                , BE.everySet compareGlobal globalEncoder deps
                ]


nodeDecoder : BD.Decoder Node
nodeDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 Define
                            exprDecoder
                            (BD.everySet toComparableGlobal globalDecoder)

                    1 ->
                        BD.map3 TrackedDefine
                            A.regionDecoder
                            exprDecoder
                            (BD.everySet toComparableGlobal globalDecoder)

                    2 ->
                        BD.map4 DefineTailFunc
                            A.regionDecoder
                            (BD.list (A.locatedDecoder BD.string))
                            exprDecoder
                            (BD.everySet toComparableGlobal globalDecoder)

                    3 ->
                        BD.map2 Ctor
                            Index.zeroBasedDecoder
                            BD.int

                    4 ->
                        BD.map Enum
                            Index.zeroBasedDecoder

                    5 ->
                        BD.succeed Box

                    6 ->
                        BD.map Link globalDecoder

                    7 ->
                        BD.map4 Cycle
                            (BD.list BD.string)
                            (BD.list (BD.jsonPair BD.string exprDecoder))
                            (BD.list defDecoder)
                            (BD.everySet toComparableGlobal globalDecoder)

                    8 ->
                        BD.map Manager effectsTypeDecoder

                    9 ->
                        BD.map2 Kernel
                            (BD.list K.chunkDecoder)
                            (BD.everySet toComparableGlobal globalDecoder)

                    10 ->
                        BD.map2 PortIncoming
                            exprDecoder
                            (BD.everySet toComparableGlobal globalDecoder)

                    11 ->
                        BD.map2 PortOutgoing
                            exprDecoder
                            (BD.everySet toComparableGlobal globalDecoder)

                    _ ->
                        BD.fail
            )


exprEncoder : Expr -> BE.Encoder
exprEncoder expr =
    case expr of
        Bool region value ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                , BE.bool value
                ]

        Chr region value ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder region
                , BE.string value
                ]

        Str region value ->
            BE.sequence
                [ BE.unsignedInt8 2
                , A.regionEncoder region
                , BE.string value
                ]

        Int region value ->
            BE.sequence
                [ BE.unsignedInt8 3
                , A.regionEncoder region
                , BE.int value
                ]

        Float region value ->
            BE.sequence
                [ BE.unsignedInt8 4
                , A.regionEncoder region
                , BE.float value
                ]

        VarLocal value ->
            BE.sequence
                [ BE.unsignedInt8 5
                , BE.string value
                ]

        TrackedVarLocal region value ->
            BE.sequence
                [ BE.unsignedInt8 6
                , A.regionEncoder region
                , BE.string value
                ]

        VarGlobal region value ->
            BE.sequence
                [ BE.unsignedInt8 7
                , A.regionEncoder region
                , globalEncoder value
                ]

        VarEnum region global index ->
            BE.sequence
                [ BE.unsignedInt8 8
                , A.regionEncoder region
                , globalEncoder global
                , Index.zeroBasedEncoder index
                ]

        VarBox region value ->
            BE.sequence
                [ BE.unsignedInt8 9
                , A.regionEncoder region
                , globalEncoder value
                ]

        VarCycle region home name ->
            BE.sequence
                [ BE.unsignedInt8 10
                , A.regionEncoder region
                , ModuleName.canonicalEncoder home
                , BE.string name
                ]

        VarDebug region name home unhandledValueName ->
            BE.sequence
                [ BE.unsignedInt8 11
                , A.regionEncoder region
                , BE.string name
                , ModuleName.canonicalEncoder home
                , BE.maybe BE.string unhandledValueName
                ]

        VarKernel region home name ->
            BE.sequence
                [ BE.unsignedInt8 12
                , A.regionEncoder region
                , BE.string home
                , BE.string name
                ]

        List region value ->
            BE.sequence
                [ BE.unsignedInt8 13
                , A.regionEncoder region
                , BE.list exprEncoder value
                ]

        Function args body ->
            BE.sequence
                [ BE.unsignedInt8 14
                , BE.list BE.string args
                , exprEncoder body
                ]

        TrackedFunction args body ->
            BE.sequence
                [ BE.unsignedInt8 15
                , BE.list (A.locatedEncoder BE.string) args
                , exprEncoder body
                ]

        Call region func args ->
            BE.sequence
                [ BE.unsignedInt8 16
                , A.regionEncoder region
                , exprEncoder func
                , BE.list exprEncoder args
                ]

        TailCall name args ->
            BE.sequence
                [ BE.unsignedInt8 17
                , BE.string name
                , BE.list (BE.jsonPair BE.string exprEncoder) args
                ]

        If branches final ->
            BE.sequence
                [ BE.unsignedInt8 18
                , BE.list (BE.jsonPair exprEncoder exprEncoder) branches
                , exprEncoder final
                ]

        Let def body ->
            BE.sequence
                [ BE.unsignedInt8 19
                , defEncoder def
                , exprEncoder body
                ]

        Destruct destructor body ->
            BE.sequence
                [ BE.unsignedInt8 20
                , destructorEncoder destructor
                , exprEncoder body
                ]

        Case label root decider jumps ->
            BE.sequence
                [ BE.unsignedInt8 21
                , BE.string label
                , BE.string root
                , deciderEncoder choiceEncoder decider
                , BE.list (BE.jsonPair BE.int exprEncoder) jumps
                ]

        Accessor region field ->
            BE.sequence
                [ BE.unsignedInt8 22
                , A.regionEncoder region
                , BE.string field
                ]

        Access record region field ->
            BE.sequence
                [ BE.unsignedInt8 23
                , exprEncoder record
                , A.regionEncoder region
                , BE.string field
                ]

        Update region record fields ->
            BE.sequence
                [ BE.unsignedInt8 24
                , A.regionEncoder region
                , exprEncoder record
                , BE.assocListDict A.compareLocated (A.locatedEncoder BE.string) exprEncoder fields
                ]

        Record value ->
            BE.sequence
                [ BE.unsignedInt8 25
                , BE.assocListDict compare BE.string exprEncoder value
                ]

        TrackedRecord region value ->
            BE.sequence
                [ BE.unsignedInt8 26
                , A.regionEncoder region
                , BE.assocListDict A.compareLocated (A.locatedEncoder BE.string) exprEncoder value
                ]

        Unit ->
            BE.unsignedInt8 27

        Tuple region a b cs ->
            BE.sequence
                [ BE.unsignedInt8 28
                , A.regionEncoder region
                , exprEncoder a
                , exprEncoder b
                , BE.list exprEncoder cs
                ]

        Shader src attributes uniforms ->
            BE.sequence
                [ BE.unsignedInt8 29
                , Shader.sourceEncoder src
                , BE.everySet compare BE.string attributes
                , BE.everySet compare BE.string uniforms
                ]


exprDecoder : BD.Decoder Expr
exprDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 Bool
                            A.regionDecoder
                            BD.bool

                    1 ->
                        BD.map2 Chr
                            A.regionDecoder
                            BD.string

                    2 ->
                        BD.map2 Str
                            A.regionDecoder
                            BD.string

                    3 ->
                        BD.map2 Int
                            A.regionDecoder
                            BD.int

                    4 ->
                        BD.map2 Float
                            A.regionDecoder
                            BD.float

                    5 ->
                        BD.map VarLocal BD.string

                    6 ->
                        BD.map2 TrackedVarLocal
                            A.regionDecoder
                            BD.string

                    7 ->
                        BD.map2 VarGlobal
                            A.regionDecoder
                            globalDecoder

                    8 ->
                        BD.map3 VarEnum
                            A.regionDecoder
                            globalDecoder
                            Index.zeroBasedDecoder

                    9 ->
                        BD.map2 VarBox
                            A.regionDecoder
                            globalDecoder

                    10 ->
                        BD.map3 VarCycle
                            A.regionDecoder
                            ModuleName.canonicalDecoder
                            BD.string

                    11 ->
                        BD.map4 VarDebug
                            A.regionDecoder
                            BD.string
                            ModuleName.canonicalDecoder
                            (BD.maybe BD.string)

                    12 ->
                        BD.map3 VarKernel
                            A.regionDecoder
                            BD.string
                            BD.string

                    13 ->
                        BD.map2 List
                            A.regionDecoder
                            (BD.list exprDecoder)

                    14 ->
                        BD.map2 Function
                            (BD.list BD.string)
                            exprDecoder

                    15 ->
                        BD.map2 TrackedFunction
                            (BD.list (A.locatedDecoder BD.string))
                            exprDecoder

                    16 ->
                        BD.map3 Call
                            A.regionDecoder
                            exprDecoder
                            (BD.list exprDecoder)

                    17 ->
                        BD.map2 TailCall
                            BD.string
                            (BD.list (BD.jsonPair BD.string exprDecoder))

                    18 ->
                        BD.map2 If
                            (BD.list (BD.jsonPair exprDecoder exprDecoder))
                            exprDecoder

                    19 ->
                        BD.map2 Let
                            defDecoder
                            exprDecoder

                    20 ->
                        BD.map2 Destruct
                            destructorDecoder
                            exprDecoder

                    21 ->
                        BD.map4 Case
                            BD.string
                            BD.string
                            (deciderDecoder choiceDecoder)
                            (BD.list (BD.jsonPair BD.int exprDecoder))

                    22 ->
                        BD.map2 Accessor
                            A.regionDecoder
                            BD.string

                    23 ->
                        BD.map3 Access
                            exprDecoder
                            A.regionDecoder
                            BD.string

                    24 ->
                        BD.map3 Update
                            A.regionDecoder
                            exprDecoder
                            (BD.assocListDict A.toValue (A.locatedDecoder BD.string) exprDecoder)

                    25 ->
                        BD.map Record
                            (BD.assocListDict identity BD.string exprDecoder)

                    26 ->
                        BD.map2 TrackedRecord
                            A.regionDecoder
                            (BD.assocListDict A.toValue (A.locatedDecoder BD.string) exprDecoder)

                    27 ->
                        BD.succeed Unit

                    28 ->
                        BD.map4 Tuple
                            A.regionDecoder
                            exprDecoder
                            exprDecoder
                            (BD.list exprDecoder)

                    29 ->
                        BD.map3 Shader
                            Shader.sourceDecoder
                            (BD.everySet identity BD.string)
                            (BD.everySet identity BD.string)

                    _ ->
                        BD.fail
            )


defEncoder : Def -> BE.Encoder
defEncoder def =
    case def of
        Def region name expr ->
            BE.sequence
                [ BE.unsignedInt8 0
                , A.regionEncoder region
                , BE.string name
                , exprEncoder expr
                ]

        TailDef region name args expr ->
            BE.sequence
                [ BE.unsignedInt8 1
                , A.regionEncoder region
                , BE.string name
                , BE.list (A.locatedEncoder BE.string) args
                , exprEncoder expr
                ]


defDecoder : BD.Decoder Def
defDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map3 Def
                            A.regionDecoder
                            BD.string
                            exprDecoder

                    1 ->
                        BD.map4 TailDef
                            A.regionDecoder
                            BD.string
                            (BD.list (A.locatedDecoder BD.string))
                            exprDecoder

                    _ ->
                        BD.fail
            )


destructorEncoder : Destructor -> BE.Encoder
destructorEncoder (Destructor name path) =
    BE.sequence
        [ BE.string name
        , pathEncoder path
        ]


destructorDecoder : BD.Decoder Destructor
destructorDecoder =
    BD.map2 Destructor
        BD.string
        pathDecoder


deciderEncoder : (a -> BE.Encoder) -> Decider a -> BE.Encoder
deciderEncoder encoder decider =
    case decider of
        Leaf value ->
            BE.sequence
                [ BE.unsignedInt8 0
                , encoder value
                ]

        Chain testChain success failure ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.list (BE.jsonPair DT.pathEncoder DT.testEncoder) testChain
                , deciderEncoder encoder success
                , deciderEncoder encoder failure
                ]

        FanOut path edges fallback ->
            BE.sequence
                [ BE.unsignedInt8 2
                , DT.pathEncoder path
                , BE.list (BE.jsonPair DT.testEncoder (deciderEncoder encoder)) edges
                , deciderEncoder encoder fallback
                ]


deciderDecoder : BD.Decoder a -> BD.Decoder (Decider a)
deciderDecoder decoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Leaf decoder

                    1 ->
                        BD.map3 Chain
                            (BD.list (BD.jsonPair DT.pathDecoder DT.testDecoder))
                            (deciderDecoder decoder)
                            (deciderDecoder decoder)

                    2 ->
                        BD.map3 FanOut
                            DT.pathDecoder
                            (BD.list (BD.jsonPair DT.testDecoder (deciderDecoder decoder)))
                            (deciderDecoder decoder)

                    _ ->
                        BD.fail
            )


choiceEncoder : Choice -> BE.Encoder
choiceEncoder choice =
    case choice of
        Inline value ->
            BE.sequence
                [ BE.unsignedInt8 0
                , exprEncoder value
                ]

        Jump value ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.int value
                ]


choiceDecoder : BD.Decoder Choice
choiceDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map Inline exprDecoder

                    1 ->
                        BD.map Jump BD.int

                    _ ->
                        BD.fail
            )


pathEncoder : Path -> BE.Encoder
pathEncoder path =
    case path of
        Index index subPath ->
            BE.sequence
                [ BE.unsignedInt8 0
                , Index.zeroBasedEncoder index
                , pathEncoder subPath
                ]

        ArrayIndex index subPath ->
            BE.sequence
                [ BE.unsignedInt8 1
                , BE.int index
                , pathEncoder subPath
                ]

        Field field subPath ->
            BE.sequence
                [ BE.unsignedInt8 2
                , BE.string field
                , pathEncoder subPath
                ]

        Unbox subPath ->
            BE.sequence
                [ BE.unsignedInt8 3
                , pathEncoder subPath
                ]

        Root name ->
            BE.sequence
                [ BE.unsignedInt8 4
                , BE.string name
                ]


pathDecoder : BD.Decoder Path
pathDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.map2 Index
                            Index.zeroBasedDecoder
                            pathDecoder

                    1 ->
                        BD.map2 ArrayIndex
                            BD.int
                            pathDecoder

                    2 ->
                        BD.map2 Field
                            BD.string
                            pathDecoder

                    3 ->
                        BD.map Unbox pathDecoder

                    4 ->
                        BD.map Root BD.string

                    _ ->
                        BD.fail
            )


effectsTypeEncoder : EffectsType -> BE.Encoder
effectsTypeEncoder effectsType =
    BE.unsignedInt8
        (case effectsType of
            Cmd ->
                0

            Sub ->
                1

            Fx ->
                2
        )


effectsTypeDecoder : BD.Decoder EffectsType
effectsTypeDecoder =
    BD.unsignedInt8
        |> BD.andThen
            (\idx ->
                case idx of
                    0 ->
                        BD.succeed Cmd

                    1 ->
                        BD.succeed Sub

                    2 ->
                        BD.succeed Fx

                    _ ->
                        BD.fail
            )
