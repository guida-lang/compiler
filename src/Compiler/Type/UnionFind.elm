module Compiler.Type.UnionFind exposing
    ( Content(..)
    , Descriptor(..)
    , FlatType(..)
    , Mark(..)
    , Point
    , SuperType(..)
    , Variable
    , equivalent
    , fresh
    , get
    , modify
    , redundant
    , set
    , union
    , variableCodec
    , variableDecoder
    , variableEncoder
    )

{- This is based on the following implementations:

     - https://hackage.haskell.org/package/union-find-0.2/docs/src/Data-UnionFind-IO.html
     - http://yann.regis-gianas.org/public/mini/code_UnionFind.html

   It seems like the OCaml one came first, but I am not sure.

   Compared to the Haskell implementation, the major changes here include:

     1. No more reallocating PointInfo when changing the weight
     2. Using the strict modifyIORef

-}

import Compiler.Data.Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Serialize as S
import Data.IO as IO exposing (IO, IORef)
import Data.Map exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Serialize exposing (Codec)
import Utils.Crash exposing (crash)



-- FROM TYPE


type Descriptor
    = Descriptor Content Int Mark (Maybe Variable)


descriptorCodec : Codec e Descriptor
descriptorCodec =
    Serialize.customType
        (\descriptorCodecEncoder (Descriptor content rank mark copy) ->
            descriptorCodecEncoder content rank mark copy
        )
        |> Serialize.variant4 Descriptor contentCodec Serialize.int markCodec (Serialize.maybe variableCodec)
        |> Serialize.finishCustomType


type Content
    = FlexVar (Maybe Name)
    | FlexSuper SuperType (Maybe Name)
    | RigidVar Name
    | RigidSuper SuperType Name
    | Structure FlatType
    | Alias ModuleName.Canonical Name (List ( Name, Variable )) Variable
    | Error


contentCodec : Codec e Content
contentCodec =
    Serialize.customType
        (\flexVarEncoder flexSuperEncoder rigidVarEncoder rigidSuperEncoder structureEncoder aliasEncoder errorEncoder content ->
            case content of
                FlexVar maybeName ->
                    flexVarEncoder maybeName

                FlexSuper superType maybeName ->
                    flexSuperEncoder superType maybeName

                RigidVar name ->
                    rigidVarEncoder name

                RigidSuper superType name ->
                    rigidSuperEncoder superType name

                Structure flatType ->
                    structureEncoder flatType

                Alias canonical name variableList variable ->
                    aliasEncoder canonical name variableList variable

                Error ->
                    errorEncoder
        )
        |> Serialize.variant1 FlexVar (Serialize.maybe Serialize.string)
        |> Serialize.variant2 FlexSuper superTypeCodec (Serialize.maybe Serialize.string)
        |> Serialize.variant1 RigidVar Serialize.string
        |> Serialize.variant2 RigidSuper superTypeCodec Serialize.string
        |> Serialize.variant1 Structure flatTypeCodec
        |> Serialize.variant4 Alias
            ModuleName.canonicalCodec
            Serialize.string
            (Serialize.list (Serialize.tuple Serialize.string variableCodec))
            variableCodec
        |> Serialize.variant0 Error
        |> Serialize.finishCustomType


type SuperType
    = Number
    | Comparable
    | Appendable
    | CompAppend


superTypeCodec : Codec e SuperType
superTypeCodec =
    Serialize.customType
        (\numberEncoder comparableEncoder appendableEncoder compAppendEncoder superType ->
            case superType of
                Number ->
                    numberEncoder

                Comparable ->
                    comparableEncoder

                Appendable ->
                    appendableEncoder

                CompAppend ->
                    compAppendEncoder
        )
        |> Serialize.variant0 Number
        |> Serialize.variant0 Comparable
        |> Serialize.variant0 Appendable
        |> Serialize.variant0 CompAppend
        |> Serialize.finishCustomType


type FlatType
    = App1 ModuleName.Canonical Name (List Variable)
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Dict Name Variable) Variable
    | Unit1
    | Tuple1 Variable Variable (Maybe Variable)


flatTypeCodec : Codec e FlatType
flatTypeCodec =
    Serialize.customType
        (\app1Encoder fun1Encoder emptyRecord1Encoder record1Encoder unit1Encoder tuple1Encoder flatType ->
            case flatType of
                App1 canonical name variableList ->
                    app1Encoder canonical name variableList

                Fun1 var1 var2 ->
                    fun1Encoder var1 var2

                EmptyRecord1 ->
                    emptyRecord1Encoder

                Record1 variableDict variable ->
                    record1Encoder variableDict variable

                Unit1 ->
                    unit1Encoder

                Tuple1 var1 var2 maybeVariable ->
                    tuple1Encoder var1 var2 maybeVariable
        )
        |> Serialize.variant3 App1 ModuleName.canonicalCodec Serialize.string (Serialize.list variableCodec)
        |> Serialize.variant2 Fun1 variableCodec variableCodec
        |> Serialize.variant0 EmptyRecord1
        |> Serialize.variant2 Record1 (S.assocListDict compare Serialize.string variableCodec) variableCodec
        |> Serialize.variant0 Unit1
        |> Serialize.variant3 Tuple1 variableCodec variableCodec (Serialize.maybe variableCodec)
        |> Serialize.finishCustomType


type Mark
    = Mark Int


markCodec : Codec e Mark
markCodec =
    Serialize.int |> Serialize.map Mark (\(Mark value) -> value)


type alias Variable =
    Point


variableEncoder : Variable -> Encode.Value
variableEncoder =
    pointEncoder


variableDecoder : Decode.Decoder Variable
variableDecoder =
    pointDecoder


variableCodec : Codec e Variable
variableCodec =
    pointCodec



-- POINT


type Point
    = Pt (IORef PointInfo)


pointEncoder : Point -> Encode.Value
pointEncoder (Pt ioRef) =
    IO.ioRefEncoder ioRef


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.map Pt IO.ioRefDecoder


pointCodec : Codec e Point
pointCodec =
    IO.ioRefCodec |> Serialize.map Pt (\(Pt ioRef) -> ioRef)


type PointInfo
    = Info (IORef Int) (IORef Descriptor)
    | Link Point


pointInfoCodec : Codec e PointInfo
pointInfoCodec =
    Serialize.customType
        (\infoEncoder linkEncoder value ->
            case value of
                Info weight desc ->
                    infoEncoder weight desc

                Link point ->
                    linkEncoder point
        )
        |> Serialize.variant2 Info IO.ioRefCodec IO.ioRefCodec
        |> Serialize.variant1 Link pointCodec
        |> Serialize.finishCustomType



-- HELPERS


fresh : Descriptor -> IO Variable
fresh value =
    IO.newIORef Serialize.int 1
        |> IO.bind
            (\weight ->
                IO.newIORef descriptorCodec value
                    |> IO.bind (\desc -> IO.newIORef pointInfoCodec (Info weight desc))
                    |> IO.fmap (\link -> Pt link)
            )


repr : Point -> IO Point
repr ((Pt ref) as point) =
    IO.readIORef pointInfoCodec ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ _ ->
                        IO.pure point

                    Link ((Pt ref1) as point1) ->
                        repr point1
                            |> IO.bind
                                (\point2 ->
                                    if point2 /= point1 then
                                        IO.readIORef pointInfoCodec ref1
                                            |> IO.bind
                                                (\pInfo1 ->
                                                    IO.writeIORef pointInfoCodec ref pInfo1
                                                        |> IO.fmap (\_ -> point2)
                                                )

                                    else
                                        IO.pure point2
                                )
            )


get : Point -> IO Descriptor
get ((Pt ref) as point) =
    IO.readIORef pointInfoCodec ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IO.readIORef descriptorCodec descRef

                    Link (Pt ref1) ->
                        IO.readIORef pointInfoCodec ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IO.readIORef descriptorCodec descRef

                                        Link _ ->
                                            IO.bind get (repr point)
                                )
            )


set : Point -> Descriptor -> IO ()
set ((Pt ref) as point) newDesc =
    IO.readIORef pointInfoCodec ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IO.writeIORef descriptorCodec descRef newDesc

                    Link (Pt ref1) ->
                        IO.readIORef pointInfoCodec ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IO.writeIORef descriptorCodec descRef newDesc

                                        Link _ ->
                                            repr point
                                                |> IO.bind
                                                    (\newPoint ->
                                                        set newPoint newDesc
                                                    )
                                )
            )


modify : Point -> (Descriptor -> Descriptor) -> IO ()
modify ((Pt ref) as point) func =
    IO.readIORef pointInfoCodec ref
        |> IO.bind
            (\pInfo ->
                case pInfo of
                    Info _ descRef ->
                        IO.modifyIORef descriptorCodec descRef func

                    Link (Pt ref1) ->
                        IO.readIORef pointInfoCodec ref1
                            |> IO.bind
                                (\link_ ->
                                    case link_ of
                                        Info _ descRef ->
                                            IO.modifyIORef descriptorCodec descRef func

                                        Link _ ->
                                            repr point
                                                |> IO.bind (\newPoint -> modify newPoint func)
                                )
            )


union : Point -> Point -> Descriptor -> IO ()
union p1 p2 newDesc =
    repr p1
        |> IO.bind
            (\((Pt ref1) as point1) ->
                repr p2
                    |> IO.bind
                        (\((Pt ref2) as point2) ->
                            IO.readIORef pointInfoCodec ref1
                                |> IO.bind
                                    (\pointInfo1 ->
                                        IO.readIORef pointInfoCodec ref2
                                            |> IO.bind
                                                (\pointInfo2 ->
                                                    case ( pointInfo1, pointInfo2 ) of
                                                        ( Info w1 d1, Info w2 d2 ) ->
                                                            if point1 == point2 then
                                                                IO.writeIORef descriptorCodec d1 newDesc

                                                            else
                                                                IO.readIORef Serialize.int w1
                                                                    |> IO.bind
                                                                        (\weight1 ->
                                                                            IO.readIORef Serialize.int w2
                                                                                |> IO.bind
                                                                                    (\weight2 ->
                                                                                        let
                                                                                            newWeight : Int
                                                                                            newWeight =
                                                                                                weight1 + weight2
                                                                                        in
                                                                                        if weight1 >= weight2 then
                                                                                            IO.writeIORef pointInfoCodec ref2 (Link point1)
                                                                                                |> IO.bind (\_ -> IO.writeIORef Serialize.int w1 newWeight)
                                                                                                |> IO.bind (\_ -> IO.writeIORef descriptorCodec d1 newDesc)

                                                                                        else
                                                                                            IO.writeIORef pointInfoCodec ref1 (Link point2)
                                                                                                |> IO.bind (\_ -> IO.writeIORef Serialize.int w2 newWeight)
                                                                                                |> IO.bind (\_ -> IO.writeIORef descriptorCodec d2 newDesc)
                                                                                    )
                                                                        )

                                                        _ ->
                                                            crash "Unexpected pattern"
                                                )
                                    )
                        )
            )


equivalent : Point -> Point -> IO Bool
equivalent p1 p2 =
    repr p1
        |> IO.bind
            (\v1 ->
                repr p2
                    |> IO.fmap (\v2 -> v1 == v2)
            )


redundant : Point -> IO Bool
redundant (Pt ref) =
    IO.readIORef pointInfoCodec ref
        |> IO.fmap
            (\pInfo ->
                case pInfo of
                    Info _ _ ->
                        False

                    Link _ ->
                        True
            )
