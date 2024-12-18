module Compiler.AST.Optimized exposing
    ( addGlobalGraph
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
import Compiler.Data.Name as Name
import Compiler.Elm.Kernel as K
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Optimize.DecisionTree as DT
import Compiler.Reporting.Annotation as A
import Data.Map as Dict
import Data.Set as EverySet exposing (EverySet)
import Json.Decode as Decode
import Json.Encode as Encode
import Types as T



-- EXPRESSIONS


compareGlobal : T.CASTO_Global -> T.CASTO_Global -> Order
compareGlobal (T.CASTO_Global home1 name1) (T.CASTO_Global home2 name2) =
    case compare name1 name2 of
        LT ->
            LT

        EQ ->
            ModuleName.compareCanonical home1 home2

        GT ->
            GT


toComparableGlobal : T.CASTO_Global -> List String
toComparableGlobal (T.CASTO_Global home name) =
    ModuleName.toComparableCanonical home ++ [ name ]



-- GRAPHS


empty : T.CASTO_GlobalGraph
empty =
    T.CASTO_GlobalGraph Dict.empty Dict.empty


addGlobalGraph : T.CASTO_GlobalGraph -> T.CASTO_GlobalGraph -> T.CASTO_GlobalGraph
addGlobalGraph (T.CASTO_GlobalGraph nodes1 fields1) (T.CASTO_GlobalGraph nodes2 fields2) =
    T.CASTO_GlobalGraph
        (Dict.union nodes1 nodes2)
        (Dict.union fields1 fields2)


addLocalGraph : T.CASTO_LocalGraph -> T.CASTO_GlobalGraph -> T.CASTO_GlobalGraph
addLocalGraph (T.CASTO_LocalGraph _ nodes1 fields1) (T.CASTO_GlobalGraph nodes2 fields2) =
    T.CASTO_GlobalGraph
        (Dict.union nodes1 nodes2)
        (Dict.union fields1 fields2)


addKernel : T.CDN_Name -> List T.CEK_Chunk -> T.CASTO_GlobalGraph -> T.CASTO_GlobalGraph
addKernel shortName chunks (T.CASTO_GlobalGraph nodes fields) =
    let
        global : T.CASTO_Global
        global =
            toKernelGlobal shortName

        node : T.CASTO_Node
        node =
            T.CASTO_Kernel chunks (List.foldr addKernelDep EverySet.empty chunks)
    in
    T.CASTO_GlobalGraph
        (Dict.insert toComparableGlobal global node nodes)
        (Dict.union (K.countFields chunks) fields)


addKernelDep : T.CEK_Chunk -> EverySet (List String) T.CASTO_Global -> EverySet (List String) T.CASTO_Global
addKernelDep chunk deps =
    case chunk of
        T.CEK_JS _ ->
            deps

        T.CEK_ElmVar home name ->
            EverySet.insert toComparableGlobal (T.CASTO_Global home name) deps

        T.CEK_JsVar shortName _ ->
            EverySet.insert toComparableGlobal (toKernelGlobal shortName) deps

        T.CEK_ElmField _ ->
            deps

        T.CEK_JsField _ ->
            deps

        T.CEK_JsEnum _ ->
            deps

        T.CEK_Debug ->
            deps

        T.CEK_Prod ->
            deps


toKernelGlobal : T.CDN_Name -> T.CASTO_Global
toKernelGlobal shortName =
    T.CASTO_Global (T.CEMN_Canonical Pkg.kernel shortName) Name.dollar



-- ENCODERS and DECODERS


globalGraphEncoder : T.CASTO_GlobalGraph -> Encode.Value
globalGraphEncoder (T.CASTO_GlobalGraph nodes fields) =
    Encode.object
        [ ( "type", Encode.string "GlobalGraph" )
        , ( "nodes", E.assocListDict compareGlobal globalEncoder nodeEncoder nodes )
        , ( "fields", E.assocListDict compare Encode.string Encode.int fields )
        ]


globalGraphDecoder : Decode.Decoder T.CASTO_GlobalGraph
globalGraphDecoder =
    Decode.map2 T.CASTO_GlobalGraph
        (Decode.field "nodes" (D.assocListDict toComparableGlobal globalDecoder nodeDecoder))
        (Decode.field "fields" (D.assocListDict identity Decode.string Decode.int))


localGraphEncoder : T.CASTO_LocalGraph -> Encode.Value
localGraphEncoder (T.CASTO_LocalGraph main nodes fields) =
    Encode.object
        [ ( "type", Encode.string "LocalGraph" )
        , ( "main", E.maybe mainEncoder main )
        , ( "nodes", E.assocListDict compareGlobal globalEncoder nodeEncoder nodes )
        , ( "fields", E.assocListDict compare Encode.string Encode.int fields )
        ]


localGraphDecoder : Decode.Decoder T.CASTO_LocalGraph
localGraphDecoder =
    Decode.map3 T.CASTO_LocalGraph
        (Decode.field "main" (Decode.maybe mainDecoder))
        (Decode.field "nodes" (D.assocListDict toComparableGlobal globalDecoder nodeDecoder))
        (Decode.field "fields" (D.assocListDict identity Decode.string Decode.int))


mainEncoder : T.CASTO_Main -> Encode.Value
mainEncoder main_ =
    case main_ of
        T.CASTO_Static ->
            Encode.object
                [ ( "type", Encode.string "Static" )
                ]

        T.CASTO_Dynamic msgType decoder ->
            Encode.object
                [ ( "type", Encode.string "Dynamic" )
                , ( "msgType", Can.typeEncoder msgType )
                , ( "decoder", exprEncoder decoder )
                ]


mainDecoder : Decode.Decoder T.CASTO_Main
mainDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Static" ->
                        Decode.succeed T.CASTO_Static

                    "Dynamic" ->
                        Decode.map2 T.CASTO_Dynamic
                            (Decode.field "msgType" Can.typeDecoder)
                            (Decode.field "decoder" exprDecoder)

                    _ ->
                        Decode.fail ("Unknown Main's type: " ++ type_)
            )


globalEncoder : T.CASTO_Global -> Encode.Value
globalEncoder (T.CASTO_Global home name) =
    Encode.object
        [ ( "type", Encode.string "Global" )
        , ( "home", ModuleName.canonicalEncoder home )
        , ( "name", Encode.string name )
        ]


globalDecoder : Decode.Decoder T.CASTO_Global
globalDecoder =
    Decode.map2 T.CASTO_Global
        (Decode.field "home" ModuleName.canonicalDecoder)
        (Decode.field "name" Decode.string)


nodeEncoder : T.CASTO_Node -> Encode.Value
nodeEncoder node =
    case node of
        T.CASTO_Define expr deps ->
            Encode.object
                [ ( "type", Encode.string "Define" )
                , ( "expr", exprEncoder expr )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        T.CASTO_DefineTailFunc argNames body deps ->
            Encode.object
                [ ( "type", Encode.string "DefineTailFunc" )
                , ( "argNames", Encode.list Encode.string argNames )
                , ( "body", exprEncoder body )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        T.CASTO_Ctor index arity ->
            Encode.object
                [ ( "type", Encode.string "Ctor" )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "arity", Encode.int arity )
                ]

        T.CASTO_Enum index ->
            Encode.object
                [ ( "type", Encode.string "Enum" )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CASTO_Box ->
            Encode.object
                [ ( "type", Encode.string "Box" )
                ]

        T.CASTO_Link linkedGlobal ->
            Encode.object
                [ ( "type", Encode.string "Link" )
                , ( "linkedGlobal", globalEncoder linkedGlobal )
                ]

        T.CASTO_Cycle names values functions deps ->
            Encode.object
                [ ( "type", Encode.string "Cycle" )
                , ( "names", Encode.list Encode.string names )
                , ( "values", Encode.list (E.jsonPair Encode.string exprEncoder) values )
                , ( "functions", Encode.list defEncoder functions )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        T.CASTO_Manager effectsType ->
            Encode.object
                [ ( "type", Encode.string "Manager" )
                , ( "effectsType", effectsTypeEncoder effectsType )
                ]

        T.CASTO_Kernel chunks deps ->
            Encode.object
                [ ( "type", Encode.string "Kernel" )
                , ( "chunks", Encode.list K.chunkEncoder chunks )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        T.CASTO_PortIncoming decoder deps ->
            Encode.object
                [ ( "type", Encode.string "PortIncoming" )
                , ( "decoder", exprEncoder decoder )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]

        T.CASTO_PortOutgoing encoder deps ->
            Encode.object
                [ ( "type", Encode.string "PortOutgoing" )
                , ( "encoder", exprEncoder encoder )
                , ( "deps", E.everySet compareGlobal globalEncoder deps )
                ]


nodeDecoder : Decode.Decoder T.CASTO_Node
nodeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Define" ->
                        Decode.map2 T.CASTO_Define
                            (Decode.field "expr" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "DefineTailFunc" ->
                        Decode.map3 T.CASTO_DefineTailFunc
                            (Decode.field "argNames" (Decode.list Decode.string))
                            (Decode.field "body" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "Ctor" ->
                        Decode.map2 T.CASTO_Ctor
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "arity" Decode.int)

                    "Enum" ->
                        Decode.map T.CASTO_Enum
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "Box" ->
                        Decode.succeed T.CASTO_Box

                    "Link" ->
                        Decode.map T.CASTO_Link (Decode.field "linkedGlobal" globalDecoder)

                    "Cycle" ->
                        Decode.map4 T.CASTO_Cycle
                            (Decode.field "names" (Decode.list Decode.string))
                            (Decode.field "values" (Decode.list (D.jsonPair Decode.string exprDecoder)))
                            (Decode.field "functions" (Decode.list defDecoder))
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "Manager" ->
                        Decode.map T.CASTO_Manager (Decode.field "effectsType" effectsTypeDecoder)

                    "Kernel" ->
                        Decode.map2 T.CASTO_Kernel
                            (Decode.field "chunks" (Decode.list K.chunkDecoder))
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "PortIncoming" ->
                        Decode.map2 T.CASTO_PortIncoming
                            (Decode.field "decoder" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    "PortOutgoing" ->
                        Decode.map2 T.CASTO_PortOutgoing
                            (Decode.field "encoder" exprDecoder)
                            (Decode.field "deps" (D.everySet toComparableGlobal globalDecoder))

                    _ ->
                        Decode.fail ("Unknown Node's type: " ++ type_)
            )


exprEncoder : T.CASTO_Expr -> Encode.Value
exprEncoder expr =
    case expr of
        T.CASTO_Bool value ->
            Encode.object
                [ ( "type", Encode.string "Bool" )
                , ( "value", Encode.bool value )
                ]

        T.CASTO_Chr value ->
            Encode.object
                [ ( "type", Encode.string "Chr" )
                , ( "value", Encode.string value )
                ]

        T.CASTO_Str value ->
            Encode.object
                [ ( "type", Encode.string "Str" )
                , ( "value", Encode.string value )
                ]

        T.CASTO_Int value ->
            Encode.object
                [ ( "type", Encode.string "Int" )
                , ( "value", Encode.int value )
                ]

        T.CASTO_Float value ->
            Encode.object
                [ ( "type", Encode.string "Float" )
                , ( "value", Encode.float value )
                ]

        T.CASTO_VarLocal value ->
            Encode.object
                [ ( "type", Encode.string "VarLocal" )
                , ( "value", Encode.string value )
                ]

        T.CASTO_VarGlobal value ->
            Encode.object
                [ ( "type", Encode.string "VarGlobal" )
                , ( "value", globalEncoder value )
                ]

        T.CASTO_VarEnum global index ->
            Encode.object
                [ ( "type", Encode.string "VarEnum" )
                , ( "global", globalEncoder global )
                , ( "index", Index.zeroBasedEncoder index )
                ]

        T.CASTO_VarBox value ->
            Encode.object
                [ ( "type", Encode.string "VarBox" )
                , ( "value", globalEncoder value )
                ]

        T.CASTO_VarCycle home name ->
            Encode.object
                [ ( "type", Encode.string "VarCycle" )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "name", Encode.string name )
                ]

        T.CASTO_VarDebug name home region unhandledValueName ->
            Encode.object
                [ ( "type", Encode.string "VarDebug" )
                , ( "name", Encode.string name )
                , ( "home", ModuleName.canonicalEncoder home )
                , ( "region", A.regionEncoder region )
                , ( "unhandledValueName", E.maybe Encode.string unhandledValueName )
                ]

        T.CASTO_VarKernel home name ->
            Encode.object
                [ ( "type", Encode.string "VarKernel" )
                , ( "home", Encode.string home )
                , ( "name", Encode.string name )
                ]

        T.CASTO_List value ->
            Encode.object
                [ ( "type", Encode.string "List" )
                , ( "value", Encode.list exprEncoder value )
                ]

        T.CASTO_Function args body ->
            Encode.object
                [ ( "type", Encode.string "Function" )
                , ( "args", Encode.list Encode.string args )
                , ( "body", exprEncoder body )
                ]

        T.CASTO_Call func args ->
            Encode.object
                [ ( "type", Encode.string "Call" )
                , ( "func", exprEncoder func )
                , ( "args", Encode.list exprEncoder args )
                ]

        T.CASTO_TailCall name args ->
            Encode.object
                [ ( "type", Encode.string "TailCall" )
                , ( "name", Encode.string name )
                , ( "args", Encode.list (E.jsonPair Encode.string exprEncoder) args )
                ]

        T.CASTO_If branches final ->
            Encode.object
                [ ( "type", Encode.string "If" )
                , ( "branches", Encode.list (E.jsonPair exprEncoder exprEncoder) branches )
                , ( "final", exprEncoder final )
                ]

        T.CASTO_Let def body ->
            Encode.object
                [ ( "type", Encode.string "Let" )
                , ( "def", defEncoder def )
                , ( "body", exprEncoder body )
                ]

        T.CASTO_Destruct destructor body ->
            Encode.object
                [ ( "type", Encode.string "Destruct" )
                , ( "destructor", destructorEncoder destructor )
                , ( "body", exprEncoder body )
                ]

        T.CASTO_Case label root decider jumps ->
            Encode.object
                [ ( "type", Encode.string "Case" )
                , ( "label", Encode.string label )
                , ( "root", Encode.string root )
                , ( "decider", deciderEncoder choiceEncoder decider )
                , ( "jumps", Encode.list (E.jsonPair Encode.int exprEncoder) jumps )
                ]

        T.CASTO_Accessor field ->
            Encode.object
                [ ( "type", Encode.string "Accessor" )
                , ( "field", Encode.string field )
                ]

        T.CASTO_Access record field ->
            Encode.object
                [ ( "type", Encode.string "Access" )
                , ( "record", exprEncoder record )
                , ( "field", Encode.string field )
                ]

        T.CASTO_Update record fields ->
            Encode.object
                [ ( "type", Encode.string "Update" )
                , ( "record", exprEncoder record )
                , ( "fields", E.assocListDict compare Encode.string exprEncoder fields )
                ]

        T.CASTO_Record value ->
            Encode.object
                [ ( "type", Encode.string "Record" )
                , ( "value", E.assocListDict compare Encode.string exprEncoder value )
                ]

        T.CASTO_Unit ->
            Encode.object
                [ ( "type", Encode.string "Unit" )
                ]

        T.CASTO_Tuple a b maybeC ->
            Encode.object
                [ ( "type", Encode.string "Tuple" )
                , ( "a", exprEncoder a )
                , ( "b", exprEncoder b )
                , ( "maybeC", E.maybe exprEncoder maybeC )
                ]

        T.CASTO_Shader src attributes uniforms ->
            Encode.object
                [ ( "type", Encode.string "Shader" )
                , ( "src", Shader.sourceEncoder src )
                , ( "attributes", E.everySet compare Encode.string attributes )
                , ( "uniforms", E.everySet compare Encode.string uniforms )
                ]


exprDecoder : Decode.Decoder T.CASTO_Expr
exprDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Bool" ->
                        Decode.map T.CASTO_Bool (Decode.field "value" Decode.bool)

                    "Chr" ->
                        Decode.map T.CASTO_Chr (Decode.field "value" Decode.string)

                    "Str" ->
                        Decode.map T.CASTO_Str (Decode.field "value" Decode.string)

                    "Int" ->
                        Decode.map T.CASTO_Int (Decode.field "value" Decode.int)

                    "Float" ->
                        Decode.map T.CASTO_Float (Decode.field "value" Decode.float)

                    "VarLocal" ->
                        Decode.map T.CASTO_VarLocal (Decode.field "value" Decode.string)

                    "VarGlobal" ->
                        Decode.map T.CASTO_VarGlobal (Decode.field "value" globalDecoder)

                    "VarEnum" ->
                        Decode.map2 T.CASTO_VarEnum
                            (Decode.field "global" globalDecoder)
                            (Decode.field "index" Index.zeroBasedDecoder)

                    "VarBox" ->
                        Decode.map T.CASTO_VarBox (Decode.field "value" globalDecoder)

                    "VarCycle" ->
                        Decode.map2 T.CASTO_VarCycle
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)

                    "VarDebug" ->
                        Decode.map4 T.CASTO_VarDebug
                            (Decode.field "name" Decode.string)
                            (Decode.field "home" ModuleName.canonicalDecoder)
                            (Decode.field "region" A.regionDecoder)
                            (Decode.field "unhandledValueName" (Decode.maybe Decode.string))

                    "VarKernel" ->
                        Decode.map2 T.CASTO_VarKernel
                            (Decode.field "home" Decode.string)
                            (Decode.field "name" Decode.string)

                    "List" ->
                        Decode.map T.CASTO_List (Decode.field "value" (Decode.list exprDecoder))

                    "Function" ->
                        Decode.map2 T.CASTO_Function
                            (Decode.field "args" (Decode.list Decode.string))
                            (Decode.field "body" exprDecoder)

                    "Call" ->
                        Decode.map2 T.CASTO_Call
                            (Decode.field "func" exprDecoder)
                            (Decode.field "args" (Decode.list exprDecoder))

                    "TailCall" ->
                        Decode.map2 T.CASTO_TailCall
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list (D.jsonPair Decode.string exprDecoder)))

                    "If" ->
                        Decode.map2 T.CASTO_If
                            (Decode.field "branches" (Decode.list (D.jsonPair exprDecoder exprDecoder)))
                            (Decode.field "final" exprDecoder)

                    "Let" ->
                        Decode.map2 T.CASTO_Let
                            (Decode.field "def" defDecoder)
                            (Decode.field "body" exprDecoder)

                    "Destruct" ->
                        Decode.map2 T.CASTO_Destruct
                            (Decode.field "destructor" destructorDecoder)
                            (Decode.field "body" exprDecoder)

                    "Case" ->
                        Decode.map4 T.CASTO_Case
                            (Decode.field "label" Decode.string)
                            (Decode.field "root" Decode.string)
                            (Decode.field "decider" (deciderDecoder choiceDecoder))
                            (Decode.field "jumps" (Decode.list (D.jsonPair Decode.int exprDecoder)))

                    "Accessor" ->
                        Decode.map T.CASTO_Accessor (Decode.field "field" Decode.string)

                    "Access" ->
                        Decode.map2 T.CASTO_Access
                            (Decode.field "record" exprDecoder)
                            (Decode.field "field" Decode.string)

                    "Update" ->
                        Decode.map2 T.CASTO_Update
                            (Decode.field "record" exprDecoder)
                            (Decode.field "fields" (D.assocListDict identity Decode.string exprDecoder))

                    "Record" ->
                        Decode.map T.CASTO_Record (Decode.field "value" (D.assocListDict identity Decode.string exprDecoder))

                    "Unit" ->
                        Decode.succeed T.CASTO_Unit

                    "Tuple" ->
                        Decode.map3 T.CASTO_Tuple
                            (Decode.field "a" exprDecoder)
                            (Decode.field "b" exprDecoder)
                            (Decode.field "maybeC" (Decode.maybe exprDecoder))

                    "Shader" ->
                        Decode.map3 T.CASTO_Shader
                            (Decode.field "src" Shader.sourceDecoder)
                            (Decode.field "attributes" (D.everySet identity Decode.string))
                            (Decode.field "uniforms" (D.everySet identity Decode.string))

                    _ ->
                        Decode.fail ("Unknown Expr's type: " ++ type_)
            )


defEncoder : T.CASTO_Def -> Encode.Value
defEncoder def =
    case def of
        T.CASTO_Def name expr ->
            Encode.object
                [ ( "type", Encode.string "Def" )
                , ( "name", Encode.string name )
                , ( "expr", exprEncoder expr )
                ]

        T.CASTO_TailDef name args expr ->
            Encode.object
                [ ( "type", Encode.string "TailDef" )
                , ( "name", Encode.string name )
                , ( "args", Encode.list Encode.string args )
                , ( "expr", exprEncoder expr )
                ]


defDecoder : Decode.Decoder T.CASTO_Def
defDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Def" ->
                        Decode.map2 T.CASTO_Def
                            (Decode.field "name" Decode.string)
                            (Decode.field "expr" exprDecoder)

                    "TailDef" ->
                        Decode.map3 T.CASTO_TailDef
                            (Decode.field "name" Decode.string)
                            (Decode.field "args" (Decode.list Decode.string))
                            (Decode.field "expr" exprDecoder)

                    _ ->
                        Decode.fail ("Unknown Def's type: " ++ type_)
            )


destructorEncoder : T.CASTO_Destructor -> Encode.Value
destructorEncoder (T.CASTO_Destructor name path) =
    Encode.object
        [ ( "type", Encode.string "Destructor" )
        , ( "name", Encode.string name )
        , ( "path", pathEncoder path )
        ]


destructorDecoder : Decode.Decoder T.CASTO_Destructor
destructorDecoder =
    Decode.map2 T.CASTO_Destructor
        (Decode.field "name" Decode.string)
        (Decode.field "path" pathDecoder)


deciderEncoder : (a -> Encode.Value) -> T.CASTO_Decider a -> Encode.Value
deciderEncoder encoder decider =
    case decider of
        T.CASTO_Leaf value ->
            Encode.object
                [ ( "type", Encode.string "Leaf" )
                , ( "value", encoder value )
                ]

        T.CASTO_Chain testChain success failure ->
            Encode.object
                [ ( "type", Encode.string "Chain" )
                , ( "testChain", Encode.list (E.jsonPair DT.pathEncoder DT.testEncoder) testChain )
                , ( "success", deciderEncoder encoder success )
                , ( "failure", deciderEncoder encoder failure )
                ]

        T.CASTO_FanOut path edges fallback ->
            Encode.object
                [ ( "type", Encode.string "FanOut" )
                , ( "path", DT.pathEncoder path )
                , ( "edges", Encode.list (E.jsonPair DT.testEncoder (deciderEncoder encoder)) edges )
                , ( "fallback", deciderEncoder encoder fallback )
                ]


deciderDecoder : Decode.Decoder a -> Decode.Decoder (T.CASTO_Decider a)
deciderDecoder decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Leaf" ->
                        Decode.map T.CASTO_Leaf (Decode.field "value" decoder)

                    "Chain" ->
                        Decode.map3 T.CASTO_Chain
                            (Decode.field "testChain" (Decode.list (D.jsonPair DT.pathDecoder DT.testDecoder)))
                            (Decode.field "success" (deciderDecoder decoder))
                            (Decode.field "failure" (deciderDecoder decoder))

                    "FanOut" ->
                        Decode.map3 T.CASTO_FanOut
                            (Decode.field "path" DT.pathDecoder)
                            (Decode.field "edges" (Decode.list (D.jsonPair DT.testDecoder (deciderDecoder decoder))))
                            (Decode.field "fallback" (deciderDecoder decoder))

                    _ ->
                        Decode.fail ("Unknown Decider's type: " ++ type_)
            )


choiceEncoder : T.CASTO_Choice -> Encode.Value
choiceEncoder choice =
    case choice of
        T.CASTO_Inline value ->
            Encode.object
                [ ( "type", Encode.string "Inline" )
                , ( "value", exprEncoder value )
                ]

        T.CASTO_Jump value ->
            Encode.object
                [ ( "type", Encode.string "Jump" )
                , ( "value", Encode.int value )
                ]


choiceDecoder : Decode.Decoder T.CASTO_Choice
choiceDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Inline" ->
                        Decode.map T.CASTO_Inline (Decode.field "value" exprDecoder)

                    "Jump" ->
                        Decode.map T.CASTO_Jump (Decode.field "value" Decode.int)

                    _ ->
                        Decode.fail ("Unknown Choice's type: " ++ type_)
            )


pathEncoder : T.CASTO_Path -> Encode.Value
pathEncoder path =
    case path of
        T.CASTO_Index index subPath ->
            Encode.object
                [ ( "type", Encode.string "Index" )
                , ( "index", Index.zeroBasedEncoder index )
                , ( "subPath", pathEncoder subPath )
                ]

        T.CASTO_Field field subPath ->
            Encode.object
                [ ( "type", Encode.string "Field" )
                , ( "field", Encode.string field )
                , ( "subPath", pathEncoder subPath )
                ]

        T.CASTO_Unbox subPath ->
            Encode.object
                [ ( "type", Encode.string "Unbox" )
                , ( "subPath", pathEncoder subPath )
                ]

        T.CASTO_Root name ->
            Encode.object
                [ ( "type", Encode.string "Root" )
                , ( "name", Encode.string name )
                ]


pathDecoder : Decode.Decoder T.CASTO_Path
pathDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Index" ->
                        Decode.map2 T.CASTO_Index
                            (Decode.field "index" Index.zeroBasedDecoder)
                            (Decode.field "subPath" pathDecoder)

                    "Field" ->
                        Decode.map2 T.CASTO_Field
                            (Decode.field "field" Decode.string)
                            (Decode.field "subPath" pathDecoder)

                    "Unbox" ->
                        Decode.map T.CASTO_Unbox (Decode.field "subPath" pathDecoder)

                    "Root" ->
                        Decode.map T.CASTO_Root (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail ("Unknown Path's type: " ++ type_)
            )


effectsTypeEncoder : T.CASTO_EffectsType -> Encode.Value
effectsTypeEncoder effectsType =
    case effectsType of
        T.CASTO_Cmd ->
            Encode.string "Cmd"

        T.CASTO_Sub ->
            Encode.string "Sub"

        T.CASTO_Fx ->
            Encode.string "Fx"


effectsTypeDecoder : Decode.Decoder T.CASTO_EffectsType
effectsTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Cmd" ->
                        Decode.succeed T.CASTO_Cmd

                    "Sub" ->
                        Decode.succeed T.CASTO_Sub

                    "Fx" ->
                        Decode.succeed T.CASTO_Fx

                    _ ->
                        Decode.fail ("Unknown EffectsType: " ++ str)
            )
