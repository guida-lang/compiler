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
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Optimize.DecisionTree as DT
import Compiler.Reporting.Annotation as A
import Data.Map as Dict exposing (Dict)
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import System.TypeCheck.IO as IO



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
    | Tuple A.Region Expr Expr (Maybe Expr)
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


globalGraphEncoder : GlobalGraph -> Encode.Value
globalGraphEncoder (GlobalGraph nodes fields) =
    Encode.object
        [ ( "type", Encode.string "GlobalGraph" )
        , ( "nodes", E.assocListDict compareGlobal globalEncoder nodeEncoder nodes )
        , ( "fields", E.assocListDict compare Encode.string Encode.int fields )
        ]


globalGraphDecoder : Decode.Decoder GlobalGraph
globalGraphDecoder =
    Decode.map2 GlobalGraph
        (Decode.field "nodes" (D.assocListDict toComparableGlobal globalDecoder nodeDecoder))
        (Decode.field "fields" (D.assocListDict identity Decode.string Decode.int))


localGraphEncoder : LocalGraph -> Encode.Value
localGraphEncoder (LocalGraph main nodes fields) =
    Encode.object
        [ ( "type", Encode.string "LocalGraph" )
        , ( "main", E.maybe mainEncoder main )
        , ( "nodes", E.assocListDict compareGlobal globalEncoder nodeEncoder nodes )
        , ( "fields", E.assocListDict compare Encode.string Encode.int fields )
        ]


localGraphDecoder : Decode.Decoder LocalGraph
localGraphDecoder =
    Decode.map3 LocalGraph
        (Decode.field "main" (Decode.maybe mainDecoder))
        (Decode.field "nodes" (D.assocListDict toComparableGlobal globalDecoder nodeDecoder))
        (Decode.field "fields" (D.assocListDict identity Decode.string Decode.int))


mainEncoder : Main -> Encode.Value
mainEncoder main_ =
    case main_ of
        Static ->
            Encode.object
                [ ( "type", Encode.string "Static" )
                ]

        Dynamic msgType decoder ->
            Encode.object
                [ ( "type", Encode.string "Dynamic" )
                , ( "msgType", Can.typeEncoder msgType )
                , ( "decoder", exprEncoder decoder )
                ]


mainDecoder : Decode.Decoder Main
mainDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Static" ->
                        Decode.succeed Static

                    "Dynamic" ->
                        Decode.map2 Dynamic
                            (Decode.field "msgType" Can.typeDecoder)
                            (Decode.field "decoder" exprDecoder)

                    _ ->
                        Decode.fail ("Unknown Main's type: " ++ type_)
            )


globalEncoder : Global -> Encode.Value
globalEncoder (Global home name) =
    Encode.object
        [ ( "type", Encode.string "Global" )
        , ( "home", ModuleName.canonicalEncoder home )
        , ( "name", Encode.string name )
        ]


globalDecoder : Decode.Decoder Global
globalDecoder =
    Decode.map2 Global
        (Decode.field "home" ModuleName.canonicalDecoder)
        (Decode.field "name" Decode.string)


nodeEncoder : Node -> Encode.Value
nodeEncoder node =
    case node of
        Define expr deps ->
            Encode.object
                [ ( "type", Encode.string "Define" )
                , ( "expr", exprEncoder expr )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        TrackedDefine region expr deps ->
            Encode.object
                [ ( "type", Encode.string "TrackedDefine" )
                , ( "region", A.regionEncoder region )
                , ( "expr", exprEncoder expr )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        DefineTailFunc region argNames body deps ->
            Encode.object
                [ ( "type", Encode.string "DefineTailFunc" )
                , ( "region", A.regionEncoder region )
                , ( "argNames", Encode.list (A.locatedEncoder Encode.string) argNames )
                , ( "body", exprEncoder body )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        Ctor index arity ->
            Encode.object
                [ ( "type", Encode.string "Ctor" )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "arity", Encode.int arity )
                ]

        Enum index ->
            Encode.object
                [ ( "type", Encode.string "Enum" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        Box ->
            Encode.object
                [ ( "type", Encode.string "Box" )
                ]

        Link linkedGlobal ->
            Encode.object
                [ ( "type", Encode.string "Link" )
                , ( "linkedGlobal", globalEncoder linkedGlobal )
                ]

        Cycle names values functions deps ->
            Encode.object
                [ ( "type", Encode.string "Cycle" )
                , ( "names", Encode.list Encode.string names )
                , ( "values", Encode.list (E.jsonPair Encode.string exprEncoder) values )
                , ( "functions", Encode.list defEncoder functions )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        Manager effectsType ->
            Encode.object
                [ ( "type", Encode.string "Manager" )
                , ( "effectsType", effectsTypeEncoder effectsType )
                ]

        Kernel chunks deps ->
            Encode.object
                [ ( "type", Encode.string "Kernel" )
                , ( "chunks", Encode.list K.chunkEncoder chunks )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        PortIncoming decoder deps ->
            Encode.object
                [ ( "type", Encode.string "PortIncoming" )
                , ( "decoder", exprEncoder decoder )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        PortOutgoing encoder deps ->
            Encode.object
                [ ( "type", Encode.string "PortOutgoing" )
                , ( "encoder", exprEncoder encoder )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Define" ->
                        Decode.map2 Define
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "TrackedDefine" ->
                        Decode.map3 TrackedDefine
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "DefineTailFunc" ->
                        Decode.map4 DefineTailFunc
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "argNames" (Decode.list (A.locatedDecoder Decode.string)))
                            (Decode.field "body" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "Ctor" ->
                        Decode.map2 Ctor
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "arity" Decode.int)

                    "Enum" ->
                        Decode.map Enum
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "Box" ->
                        Decode.succeed Box

                    "Link" ->
                        Decode.map Link (Decode.field "linkedGlobal" globalDecoder)

                    "Cycle" ->
                        Decode.map4 Cycle
                            (Decode.field "names" (Decode.list Decode.string))
                            (Decode.field "values" (Decode.list (D.jsonPair Decode.string exprDecoder)))
                            (Decode.field "functions" (Decode.list defDecoder))
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "Manager" ->
                        Decode.map Manager (Decode.field "effectsType" effectsTypeDecoder)

                    "Kernel" ->
                        Decode.map2 Kernel
                            (Decode.field "chunks" (Decode.list K.chunkDecoder))
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "PortIncoming" ->
                        Decode.map2 PortIncoming
                            (Decode.field "decoder" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "PortOutgoing" ->
                        Decode.map2 PortOutgoing
                            (Decode.field "encoder" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    _ ->
                        Decode.fail ("Unknown Node's type: " ++ type_)
            )


exprEncoder : Expr -> Encode.Value
exprEncoder expr =
    case expr of
        Bool region value ->
            Encode.object
                [ ( "type", Encode.string "Bool" )
                , ( "region", A.regionEncoder region )
                , ( "value", Encode.bool value )
                ]

        Chr region value ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "region", A.regionEncoder region )
                , ( "value", Encode.string value )
                ]

        Str region value ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "region", A.regionEncoder region )
                , ( "value", Encode.string value )
                ]

        Int region value ->
            Encode.object
                [ ( "type", Encode.string "Int" )
                , ( "region", A.regionEncoder region )
                , ( "value", Encode.int value )
                ]

        Float region value ->
            Encode.object
                [ ( "type", Encode.string "Float" )
                , ( "region", A.regionEncoder region )
                , ( "value", Encode.float value )
                ]

        VarLocal value ->
            Encode.object
                [ ( "type", Encode.string "VarLocal" )
                , ( "value", Encode.string value )
                ]

        TrackedVarLocal region value ->
            Encode.object
                [ ( "type", Encode.string "TrackedVarLocal" )
                , ( "region", A.regionEncoder region )
                , ( "value", Encode.string value )
                ]

        VarGlobal region value ->
            Encode.object
                [ ( "type", Encode.string "VarGlobal" )
                , ( "region", A.regionEncoder region )
                , ( "value", globalEncoder value )
                ]

        VarEnum region global index ->
            Encode.object
                [ ( "type", Encode.string "VarEnum" )
                , ( "region", A.regionEncoder region )
                , ( "global", globalEncoder global )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        VarBox region value ->
            Encode.object
                [ ( "type", Encode.string "VarBox" )
                , ( "region", A.regionEncoder region )
                , ( "value", globalEncoder value )
                ]

        VarCycle region home name ->
            Encode.object
                [ ( "type", Encode.string "VarCycle" )
                , ( "region", A.regionEncoder region )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                ]

        VarDebug region name home unhandledValueName ->
            Encode.object
                [ ( "type", Encode.string "VarDebug" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "unhandledValueName", E.maybe Encode.string unhandledValueName )
                ]

        VarKernel region home name ->
            Encode.object
                [ ( "type", Encode.string "VarKernel" )
                , ( "region", A.regionEncoder region )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                ]

        List region value ->
            Encode.object
                [ ( "type", Encode.string "List" )
                , ( "region", A.regionEncoder region )
                , ( "value", Encode.list exprEncoder value )
                ]

        Function args body ->
            Encode.object
                [ ( "type", Encode.string "Function" )
                , ( "args", Encode.list Encode.string args )
                , ( "body", exprEncoder body )
                ]

        TrackedFunction args body ->
            Encode.object
                [ ( "type", Encode.string "TrackedFunction" )
                , ( "args", Encode.list (A.locatedEncoder Encode.string) args )
                , ( "body", exprEncoder body )
                ]

        Call region func args ->
            Encode.object
                [ ( "type", Encode.string "Call" )
                , ( "region", A.regionEncoder region )
                , ( "func", exprEncoder func )
                , ( "args", Encode.list exprEncoder args )
                ]

        TailCall name args ->
            Encode.object
                [ ( "type", Encode.string "TailCall" )
                , ( "name", Encode.string name )
                , ( "args", Encode.list (E.jsonPair Encode.string exprEncoder) args )
                ]

        If branches final ->
            Encode.object
                [ ( "type", Encode.string "If" )
                , ( "branches", Encode.list (E.jsonPair exprEncoder exprEncoder) branches )
                , ( "final", exprEncoder final )
                ]

        Let def body ->
            Encode.object
                [ ( "type", Encode.string "Let" )
                , ( "def", defEncoder def )
                , ( "body", exprEncoder body )
                ]

        Destruct destructor body ->
            Encode.object
                [ ( "type", Encode.string "Destruct" )
                , ( "destructor", destructorEncoder destructor )
                , ( "body", exprEncoder body )
                ]

        Case label root decider jumps ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                , ( "label", Encode.string label )
                , ( "root", Encode.string root )
                , ( "decider", deciderEncoder choiceEncoder decider )
                , ( "jumps", Encode.list (E.jsonPair Encode.int exprEncoder) jumps )
                ]

        Accessor region field ->
            Encode.object
                [ ( "type", Encode.string "Accessor" )
                , ( "region", A.regionEncoder region )
                , ( "field", Encode.string field )
                ]

        Access record region field ->
            Encode.object
                [ ( "type", Encode.string "Access" )
                , ( "record", exprEncoder record )
                , ( "region", A.regionEncoder region )
                , ( "field", Encode.string field )
                ]

        Update region record fields ->
            Encode.object
                [ ( "type", Encode.string "Update" )
                , ( "region", A.regionEncoder region )
                , ( "record", exprEncoder record )
                , ( "fields", E.assocListDict (\a b -> compare (A.toValue a) (A.toValue b)) (A.locatedEncoder Encode.string) exprEncoder fields )
                ]

        Record value ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "value", E.assocListDict compare Encode.string exprEncoder value )
                ]

        TrackedRecord region value ->
            Encode.object
                [ ( "type", Encode.string "TrackedRecord" )
                , ( "region", A.regionEncoder region )
                , ( "value", E.assocListDict (\a b -> compare (A.toValue a) (A.toValue b)) (A.locatedEncoder Encode.string) exprEncoder value )
                ]

        Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        Tuple region a b maybeC ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "region", A.regionEncoder region )
                , ( "a", exprEncoder a )
                , ( "b", exprEncoder b )
                , ( "maybeC", E.maybe exprEncoder maybeC )
                ]

        Shader src attributes uniforms ->
            Encode.object
                [ ( "type", Encode.string "Shader" )
                , ( "src", Shader.sourceEncoder src )
                , ( "attributes", E.everySet compare Encode.string attributes )
                , ( "uniforms", E.everySet compare Encode.string uniforms )
                ]


exprDecoder : Decode.Decoder Expr
exprDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Bool" ->
                        Decode.map2 Bool
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" Decode.bool)

                    "Chr" ->
                        Decode.map2 Chr
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" Decode.string)

                    "Str" ->
                        Decode.map2 Str
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" Decode.string)

                    "Int" ->
                        Decode.map2 Int
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" Decode.int)

                    "Float" ->
                        Decode.map2 Float
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" Decode.float)

                    "VarLocal" ->
                        Decode.map VarLocal
                            (Decode.field "value" Decode.string)

                    "TrackedVarLocal" ->
                        Decode.map2 TrackedVarLocal
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" Decode.string)

                    "VarGlobal" ->
                        Decode.map2 VarGlobal
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" globalDecoder)

                    "VarEnum" ->
                        Decode.map3 VarEnum
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "global" globalDecoder)
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "VarBox" ->
                        Decode.map2 VarBox
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" globalDecoder)

                    "VarCycle" ->
                        Decode.map3 VarCycle
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)

                    "VarDebug" ->
                        Decode.map4 VarDebug
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "unhandledValueName" (Decode.maybe Decode.string))

                    "VarKernel" ->
                        Decode.map3 VarKernel
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)

                    "List" ->
                        Decode.map2 List
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" (Decode.list exprDecoder))

                    "Function" ->
                        Decode.map2 Function
                            (Decode.field "args" (Decode.list Decode.string))
                            (Decode.field "body" exprDecoder)

                    "TrackedFunction" ->
                        Decode.map2 TrackedFunction
                            (Decode.field "args" (Decode.list (A.locatedDecoder Decode.string)))
                            (Decode.field "body" exprDecoder)

                    "Call" ->
                        Decode.map3 Call
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "func" exprDecoder)
                            (Decode.field "args" (Decode.list exprDecoder))

                    "TailCall" ->
                        Decode.map2 TailCall
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list (D.jsonPair Decode.string exprDecoder)))

                    "If" ->
                        Decode.map2 If
                            (Decode.field "branches" (Decode.list (D.jsonPair exprDecoder exprDecoder)))
                            (Decode.field "final" exprDecoder)

                    "Let" ->
                        Decode.map2 Let
                            (Decode.field "def" defDecoder)
                            (Decode.field "body" exprDecoder)

                    "Destruct" ->
                        Decode.map2 Destruct
                            (Decode.field "destructor" destructorDecoder)
                            (Decode.field "body" exprDecoder)

                    "Case" ->
                        Decode.map4 Case
                            (Decode.field "label" Decode.string)
                            (Decode.field "root" Decode.string)
                            (Decode.field "decider" (deciderDecoder choiceDecoder))
                            (Decode.field "jumps" (Decode.list (D.jsonPair Decode.int exprDecoder)))

                    "Accessor" ->
                        Decode.map2 Accessor
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "field" Decode.string)

                    "Access" ->
                        Decode.map3 Access
                            (Decode.field "record" exprDecoder)
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "field" Decode.string)

                    "Update" ->
                        Decode.map3 Update
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "record" exprDecoder)
                            (Decode.field "fields" (D.assocListDict A.toValue (A.locatedDecoder Decode.string) exprDecoder))

                    "Record" ->
                        Decode.map Record
                            (Decode.field "value" (D.assocListDict identity Decode.string exprDecoder))

                    "TrackedRecord" ->
                        Decode.map2 TrackedRecord
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "value" (D.assocListDict A.toValue (A.locatedDecoder Decode.string) exprDecoder))

                    "Unit" ->
                        Decode.succeed Unit

                    "Tuple" ->
                        Decode.map4 Tuple
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "a" exprDecoder)
                            (Decode.field "b" exprDecoder)
                            (Decode.field "maybeC" (Decode.maybe exprDecoder))

                    "Shader" ->
                        Decode.map3 Shader
                            (Decode.field "src" Shader.sourceDecoder)
                            (Decode.field "attributes" (D.everySet identity Decode.string))
                            (Decode.field "uniforms" (D.everySet identity Decode.string))

                    _ ->
                        Decode.fail ("Unknown Expr's type: " ++ type_)
            )


defEncoder : Def -> Encode.Value
defEncoder def =
    case def of
        Def region name expr ->
            Encode.object
                [ ( "type", Encode.string "Def" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "expr", exprEncoder expr )
                ]

        TailDef region name args expr ->
            Encode.object
                [ ( "type", Encode.string "TailDef" )
                , ( "region", A.regionEncoder region )
                , ( "name", Encode.string name )
                , ( "args", Encode.list (A.locatedEncoder Encode.string) args )
                , ( "expr", exprEncoder expr )
                ]


defDecoder : Decode.Decoder Def
defDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Def" ->
                        Decode.map3 Def
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "expr" exprDecoder)

                    "TailDef" ->
                        Decode.map4 TailDef
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list (A.locatedDecoder Decode.string)))
                            (Decode.field "expr" exprDecoder)

                    _ ->
                        Decode.fail ("Unknown Def's type: " ++ type_)
            )


destructorEncoder : Destructor -> Encode.Value
destructorEncoder (Destructor name path) =
    Encode.object
        [ ( "type", Encode.string "Destructor" )
        , ( "name", Encode.string name )
        , ( "path", pathEncoder path )
        ]


destructorDecoder : Decode.Decoder Destructor
destructorDecoder =
    Decode.map2 Destructor
        (Decode.field "name" Decode.string)
        (Decode.field "path" pathDecoder)


deciderEncoder : (a -> Encode.Value) -> Decider a -> Encode.Value
deciderEncoder encoder decider =
    case decider of
        Leaf value ->
            Encode.object
                [ ( "type", Encode.string "Leaf" )
                , ( "value", encoder value )
                ]

        Chain testChain success failure ->
            Encode.object
                [ ( "type", Encode.string "Chain" )
                , ( "testChain", Encode.list (E.jsonPair DT.pathEncoder DT.testEncoder) testChain )
                , ( "success", deciderEncoder encoder success )
                , ( "failure", deciderEncoder encoder failure )
                ]

        FanOut path edges fallback ->
            Encode.object
                [ ( "type", Encode.string "FanOut" )
                , ( "path", DT.pathEncoder path )
                , ( "edges", Encode.list (E.jsonPair DT.testEncoder (deciderEncoder encoder)) edges )
                , ( "fallback", deciderEncoder encoder fallback )
                ]


deciderDecoder : Decode.Decoder a -> Decode.Decoder (Decider a)
deciderDecoder decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Leaf" ->
                        Decode.map Leaf (Decode.field "value" decoder)

                    "Chain" ->
                        Decode.map3 Chain
                            (Decode.field "testChain" (Decode.list (D.jsonPair DT.pathDecoder DT.testDecoder)))
                            (Decode.field "success" (deciderDecoder decoder))
                            (Decode.field "failure" (deciderDecoder decoder))

                    "FanOut" ->
                        Decode.map3 FanOut
                            (Decode.field "path" DT.pathDecoder)
                            (Decode.field "edges" (Decode.list (D.jsonPair DT.testDecoder (deciderDecoder decoder))))
                            (Decode.field "fallback" (deciderDecoder decoder))

                    _ ->
                        Decode.fail ("Unknown Decider's type: " ++ type_)
            )


choiceEncoder : Choice -> Encode.Value
choiceEncoder choice =
    case choice of
        Inline value ->
            Encode.object
                [ ( "type", Encode.string "Inline" )
                , ( "value", exprEncoder value )
                ]

        Jump value ->
            Encode.object
                [ ( "type", Encode.string "Jump" )
                , ( "value", Encode.int value )
                ]


choiceDecoder : Decode.Decoder Choice
choiceDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Inline" ->
                        Decode.map Inline (Decode.field "value" exprDecoder)

                    "Jump" ->
                        Decode.map Jump (Decode.field "value" Decode.int)

                    _ ->
                        Decode.fail ("Unknown Choice's type: " ++ type_)
            )


pathEncoder : Path -> Encode.Value
pathEncoder path =
    case path of
        Index index subPath ->
            Encode.object
                [ ( "type", Encode.string "Index" )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "subPath", pathEncoder subPath )
                ]

        Field field subPath ->
            Encode.object
                [ ( "type", Encode.string "Field" )
                , ( "field", Encode.string field )
                , ( "subPath", pathEncoder subPath )
                ]

        Unbox subPath ->
            Encode.object
                [ ( "type", Encode.string "Unbox" )
                , ( "subPath", pathEncoder subPath )
                ]

        Root name ->
            Encode.object
                [ ( "type", Encode.string "Root" )
                , ( "name", Encode.string name )
                ]


pathDecoder : Decode.Decoder Path
pathDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Index" ->
                        Decode.map2 Index
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "subPath" pathDecoder)

                    "Field" ->
                        Decode.map2 Field
                            (Decode.field "field" Decode.string)
                            (Decode.field "subPath" pathDecoder)

                    "Unbox" ->
                        Decode.map Unbox (Decode.field "subPath" pathDecoder)

                    "Root" ->
                        Decode.map Root (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail ("Unknown Path's type: " ++ type_)
            )


effectsTypeEncoder : EffectsType -> Encode.Value
effectsTypeEncoder effectsType =
    case effectsType of
        Cmd ->
            Encode.string "Cmd"

        Sub ->
            Encode.string "Sub"

        Fx ->
            Encode.string "Fx"


effectsTypeDecoder : Decode.Decoder EffectsType
effectsTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Cmd" ->
                        Decode.succeed Cmd

                    "Sub" ->
                        Decode.succeed Sub

                    "Fx" ->
                        Decode.succeed Fx

                    _ ->
                        Decode.fail ("Unknown EffectsType: " ++ str)
            )
