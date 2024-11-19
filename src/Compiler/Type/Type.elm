module Compiler.Type.Type exposing
    ( Constraint(..)
    , Content(..)
    , Descriptor(..)
    , FlatType(..)
    , Mark
    , SuperType(..)
    , Type(..)
    , Variable
    , bool
    , char
    , descriptorDecoder
    , descriptorEncoder
    , exists
    , float
    , funType
    , int
    , mat4
    , mkFlexNumber
    , mkFlexVar
    , nameToFlex
    , nameToRigid
    , never
    , nextMark
    , noMark
    , noRank
    , outermostRank
    , string
    , texture
    , toAnnotation
    , toErrorType
    , unnamedFlexSuper
    , unnamedFlexVar
    , variableDecoder
    , variableEncoder
    , vec2
    , vec3
    , vec4
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Json.Decode as D
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Error as ET
import Compiler.Type.UnionFind as UF
import Data.IO as IO exposing (IO)
import Data.Map as Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Utils.Crash exposing (crash)
import Utils.Main as Utils



-- CONSTRAINTS


type Constraint
    = CTrue
    | CSaveTheEnvironment
    | CEqual A.Region E.Category Type (E.Expected Type)
    | CLocal A.Region Name (E.Expected Type)
    | CForeign A.Region Name Can.Annotation (E.Expected Type)
    | CPattern A.Region E.PCategory Type (E.PExpected Type)
    | CAnd (List Constraint)
    | CLet (List Variable) (List Variable) (Dict Name (A.Located Type)) Constraint Constraint


exists : List Variable -> Constraint -> Constraint
exists flexVars constraint =
    CLet [] flexVars Dict.empty constraint CTrue



-- TYPE PRIMITIVES


type alias Variable =
    UF.Point Descriptor


variableEncoder : Variable -> Encode.Value
variableEncoder =
    UF.pointEncoder


variableDecoder : Decode.Decoder Variable
variableDecoder =
    UF.pointDecoder


type FlatType
    = App1 ModuleName.Canonical Name (List Variable)
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Dict Name Variable) Variable
    | Unit1
    | Tuple1 Variable Variable (Maybe Variable)


flatTypeEncoder : FlatType -> Encode.Value
flatTypeEncoder flatType =
    case flatType of
        App1 canonical name variableList ->
            Encode.object
                [ ( "type", Encode.string "App1" )
                , ( "canonical", ModuleName.canonicalEncoder canonical )
                , ( "name", Encode.string name )
                , ( "variableList", Encode.list variableEncoder variableList )
                ]

        Fun1 var1 var2 ->
            Encode.object
                [ ( "type", Encode.string "Fun1" )
                , ( "var1", variableEncoder var1 )
                , ( "var2", variableEncoder var2 )
                ]

        EmptyRecord1 ->
            Encode.object
                [ ( "type", Encode.string "EmptyRecord1" )
                ]

        Record1 variableDict variable ->
            Encode.object
                [ ( "type", Encode.string "Record1" )
                , ( "variableDict"
                  , Dict.toList variableDict
                        |> Encode.list
                            (\( name, var ) ->
                                Encode.object
                                    [ ( "a", Encode.string name )
                                    , ( "b", variableEncoder var )
                                    ]
                            )
                  )
                , ( "variable", variableEncoder variable )
                ]

        Unit1 ->
            Encode.object
                [ ( "type", Encode.string "Unit1" )
                ]

        Tuple1 var1 var2 maybeVariable ->
            Encode.object
                [ ( "type", Encode.string "Tuple1" )
                , ( "var1", variableEncoder var1 )
                , ( "var2", variableEncoder var2 )
                , ( "maybeVariable"
                  , maybeVariable
                        |> Maybe.map variableEncoder
                        |> Maybe.withDefault Encode.null
                  )
                ]


flatTypeDecoder : Decode.Decoder FlatType
flatTypeDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "App1" ->
                        Decode.map3 App1
                            (Decode.field "canonical" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "variableList" (Decode.list variableDecoder))

                    "Fun1" ->
                        Decode.map2 Fun1
                            (Decode.field "var1" variableDecoder)
                            (Decode.field "var2" variableDecoder)

                    "EmptyRecord1" ->
                        Decode.succeed EmptyRecord1

                    "Record1" ->
                        Decode.map2 Record1
                            (Decode.field "variableDict" (D.assocListDict compare Decode.string variableDecoder))
                            (Decode.field "variable" variableDecoder)

                    "Unit1" ->
                        Decode.succeed Unit1

                    "Tuple1" ->
                        Decode.map3 Tuple1
                            (Decode.field "var1" variableDecoder)
                            (Decode.field "var2" variableDecoder)
                            (Decode.field "maybeVariable" (Decode.maybe variableDecoder))

                    _ ->
                        Decode.fail ("Unknown FlatType's type: " ++ type_)
            )


type Type
    = PlaceHolder Name
    | AliasN ModuleName.Canonical Name (List ( Name, Type )) Type
    | VarN Variable
    | AppN ModuleName.Canonical Name (List Type)
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Dict Name Type) Type
    | UnitN
    | TupleN Type Type (Maybe Type)



-- DESCRIPTORS


type Descriptor
    = Descriptor Content Int Mark (Maybe Variable)


descriptorEncoder : Descriptor -> Encode.Value
descriptorEncoder (Descriptor content rank mark copy) =
    Encode.object
        [ ( "type", Encode.string "Descriptor" )
        , ( "content", contentEncoder content )
        , ( "rank", Encode.int rank )
        , ( "mark", markEncoder mark )
        , ( "copy"
          , copy
                |> Maybe.map variableEncoder
                |> Maybe.withDefault Encode.null
          )
        ]


descriptorDecoder : Decode.Decoder Descriptor
descriptorDecoder =
    Decode.map4 Descriptor
        (Decode.field "content" contentDecoder)
        (Decode.field "rank" Decode.int)
        (Decode.field "mark" markDecoder)
        (Decode.field "copy" (Decode.maybe variableDecoder))


type Content
    = FlexVar (Maybe Name)
    | FlexSuper SuperType (Maybe Name)
    | RigidVar Name
    | RigidSuper SuperType Name
    | Structure FlatType
    | Alias ModuleName.Canonical Name (List ( Name, Variable )) Variable
    | Error


contentEncoder : Content -> Encode.Value
contentEncoder content =
    case content of
        FlexVar maybeName ->
            Encode.object
                [ ( "type", Encode.string "FlexVar" )
                , ( "name"
                  , maybeName
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                  )
                ]

        FlexSuper superType maybeName ->
            Encode.object
                [ ( "type", Encode.string "FlexSuper" )
                , ( "superType", superTypeEncoder superType )
                , ( "name"
                  , maybeName
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                  )
                ]

        RigidVar name ->
            Encode.object
                [ ( "type", Encode.string "RigidVar" )
                , ( "name", Encode.string name )
                ]

        RigidSuper superType name ->
            Encode.object
                [ ( "type", Encode.string "RigidSuper" )
                , ( "superType", superTypeEncoder superType )
                , ( "name", Encode.string name )
                ]

        Structure flatType ->
            Encode.object
                [ ( "type", Encode.string "Structure" )
                , ( "flatType", flatTypeEncoder flatType )
                ]

        Alias canonical name variableList variable ->
            Encode.object
                [ ( "type", Encode.string "Alias" )
                , ( "canonical", ModuleName.canonicalEncoder canonical )
                , ( "name", Encode.string name )
                , ( "variableList", Encode.object (List.map (Tuple.mapSecond variableEncoder) variableList) )
                , ( "variable", variableEncoder variable )
                ]

        Error ->
            Encode.object
                [ ( "type", Encode.string "Error" )
                ]


contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "FlexVar" ->
                        Decode.map FlexVar
                            (Decode.field "name" (Decode.maybe Decode.string))

                    "FlexSuper" ->
                        Decode.map2 FlexSuper
                            (Decode.field "superType" superTypeDecoder)
                            (Decode.field "name" (Decode.maybe Decode.string))

                    "RigidVar" ->
                        Decode.map RigidVar
                            (Decode.field "name" Decode.string)

                    "RigidSuper" ->
                        Decode.map2 RigidSuper
                            (Decode.field "superType" superTypeDecoder)
                            (Decode.field "name" Decode.string)

                    "Structure" ->
                        Decode.map Structure
                            (Decode.field "flatType" flatTypeDecoder)

                    "Alias" ->
                        Decode.map4 Alias
                            (Decode.field "canonical" ModuleName.canonicalDecoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "variableList" (Decode.keyValuePairs variableDecoder))
                            (Decode.field "variable" variableDecoder)

                    "Error" ->
                        Decode.succeed Error

                    _ ->
                        Decode.fail ("Unknown Content's type: " ++ type_)
            )


type SuperType
    = Number
    | Comparable
    | Appendable
    | CompAppend


superTypeEncoder : SuperType -> Encode.Value
superTypeEncoder superType =
    case superType of
        Number ->
            Encode.string "Number"

        Comparable ->
            Encode.string "Comparable"

        Appendable ->
            Encode.string "Appendable"

        CompAppend ->
            Encode.string "CompAppend"


superTypeDecoder : Decode.Decoder SuperType
superTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Number" ->
                        Decode.succeed Number

                    "Comparable" ->
                        Decode.succeed Comparable

                    "Appendable" ->
                        Decode.succeed Appendable

                    "CompAppend" ->
                        Decode.succeed CompAppend

                    _ ->
                        Decode.fail ("Failed to decode SuperType: " ++ str)
            )


makeDescriptor : Content -> Descriptor
makeDescriptor content =
    Descriptor content noRank noMark Nothing



-- RANKS


noRank : Int
noRank =
    0


outermostRank : Int
outermostRank =
    1



-- MARKS


type Mark
    = Mark Int


markEncoder : Mark -> Encode.Value
markEncoder (Mark value) =
    Encode.int value


markDecoder : Decode.Decoder Mark
markDecoder =
    Decode.map Mark Decode.int


noMark : Mark
noMark =
    Mark 2


occursMark : Mark
occursMark =
    Mark 1


getVarNamesMark : Mark
getVarNamesMark =
    Mark 0


nextMark : Mark -> Mark
nextMark (Mark mark) =
    Mark (mark + 1)



-- FUNCTION TYPES


funType : Type -> Type -> Type
funType =
    FunN



-- PRIMITIVE TYPES


int : Type
int =
    AppN ModuleName.basics "Int" []


float : Type
float =
    AppN ModuleName.basics "Float" []


char : Type
char =
    AppN ModuleName.char "Char" []


string : Type
string =
    AppN ModuleName.string "String" []


bool : Type
bool =
    AppN ModuleName.basics "Bool" []


never : Type
never =
    AppN ModuleName.basics "Never" []



-- WEBGL TYPES


vec2 : Type
vec2 =
    AppN ModuleName.vector2 "Vec2" []


vec3 : Type
vec3 =
    AppN ModuleName.vector3 "Vec3" []


vec4 : Type
vec4 =
    AppN ModuleName.vector4 "Vec4" []


mat4 : Type
mat4 =
    AppN ModuleName.matrix4 "Mat4" []


texture : Type
texture =
    AppN ModuleName.texture "Texture" []



-- MAKE FLEX VARIABLES


mkFlexVar : IO Variable
mkFlexVar =
    UF.fresh descriptorEncoder flexVarDescriptor


flexVarDescriptor : Descriptor
flexVarDescriptor =
    makeDescriptor unnamedFlexVar


unnamedFlexVar : Content
unnamedFlexVar =
    FlexVar Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber : IO Variable
mkFlexNumber =
    UF.fresh descriptorEncoder flexNumberDescriptor


flexNumberDescriptor : Descriptor
flexNumberDescriptor =
    makeDescriptor (unnamedFlexSuper Number)


unnamedFlexSuper : SuperType -> Content
unnamedFlexSuper super =
    FlexSuper super Nothing



-- MAKE NAMED VARIABLES


nameToFlex : Name -> IO Variable
nameToFlex name =
    UF.fresh descriptorEncoder <|
        makeDescriptor <|
            Maybe.unwrap FlexVar FlexSuper (toSuper name) (Just name)


nameToRigid : Name -> IO Variable
nameToRigid name =
    UF.fresh descriptorEncoder <|
        makeDescriptor <|
            Maybe.unwrap RigidVar RigidSuper (toSuper name) name


toSuper : Name -> Maybe SuperType
toSuper name =
    if Name.isNumberType name then
        Just Number

    else if Name.isComparableType name then
        Just Comparable

    else if Name.isAppendableType name then
        Just Appendable

    else if Name.isCompappendType name then
        Just CompAppend

    else
        Nothing



-- TO TYPE ANNOTATION


toAnnotation : Variable -> IO Can.Annotation
toAnnotation variable =
    getVarNames variable Dict.empty
        |> IO.bind
            (\userNames ->
                IO.runStateT (variableToCanType variable) (makeNameState userNames)
                    |> IO.fmap
                        (\( tipe, NameState freeVars _ _ _ _ _ ) ->
                            Can.Forall freeVars tipe
                        )
            )


variableToCanType : Variable -> IO.StateT NameState Can.Type
variableToCanType variable =
    IO.liftIO (UF.get descriptorDecoder variable)
        |> IO.bindStateT
            (\(Descriptor content _ _ _) ->
                case content of
                    Structure term ->
                        termToCanType term

                    FlexVar maybeName ->
                        case maybeName of
                            Just name ->
                                IO.pureStateT (Can.TVar name)

                            Nothing ->
                                getFreshVarName
                                    |> IO.bindStateT
                                        (\name ->
                                            IO.liftIO
                                                (UF.modify descriptorDecoder
                                                    descriptorEncoder
                                                    variable
                                                    (\(Descriptor _ rank mark copy) ->
                                                        Descriptor (FlexVar (Just name)) rank mark copy
                                                    )
                                                )
                                                |> IO.fmapStateT (\_ -> Can.TVar name)
                                        )

                    FlexSuper super maybeName ->
                        case maybeName of
                            Just name ->
                                IO.pureStateT (Can.TVar name)

                            Nothing ->
                                getFreshSuperName super
                                    |> IO.bindStateT
                                        (\name ->
                                            IO.liftIO
                                                (UF.modify descriptorDecoder
                                                    descriptorEncoder
                                                    variable
                                                    (\(Descriptor _ rank mark copy) ->
                                                        Descriptor (FlexSuper super (Just name)) rank mark copy
                                                    )
                                                )
                                                |> IO.fmapStateT (\_ -> Can.TVar name)
                                        )

                    RigidVar name ->
                        IO.pureStateT (Can.TVar name)

                    RigidSuper _ name ->
                        IO.pureStateT (Can.TVar name)

                    Alias home name args realVariable ->
                        Utils.listTraverseStateT (Utils.tupleTraverseStateT variableToCanType) args
                            |> IO.bindStateT
                                (\canArgs ->
                                    variableToCanType realVariable
                                        |> IO.fmapStateT
                                            (\canType ->
                                                Can.TAlias home name canArgs (Can.Filled canType)
                                            )
                                )

                    Error ->
                        crash "cannot handle Error types in variableToCanType"
            )


termToCanType : FlatType -> IO.StateT NameState Can.Type
termToCanType term =
    case term of
        App1 home name args ->
            Utils.listTraverseStateT variableToCanType args
                |> IO.fmapStateT (Can.TType home name)

        Fun1 a b ->
            IO.pureStateT Can.TLambda
                |> IO.applyStateT (variableToCanType a)
                |> IO.applyStateT (variableToCanType b)

        EmptyRecord1 ->
            IO.pureStateT (Can.TRecord Dict.empty Nothing)

        Record1 fields extension ->
            Utils.mapTraverseStateT compare fieldToCanType fields
                |> IO.bindStateT
                    (\canFields ->
                        variableToCanType extension
                            |> IO.fmapStateT Type.iteratedDealias
                            |> IO.fmapStateT
                                (\canExt ->
                                    case canExt of
                                        Can.TRecord subFields subExt ->
                                            Can.TRecord (Dict.union compare subFields canFields) subExt

                                        Can.TVar name ->
                                            Can.TRecord canFields (Just name)

                                        _ ->
                                            crash "Used toAnnotation on a type that is not well-formed"
                                )
                    )

        Unit1 ->
            IO.pureStateT Can.TUnit

        Tuple1 a b maybeC ->
            IO.pureStateT Can.TTuple
                |> IO.applyStateT (variableToCanType a)
                |> IO.applyStateT (variableToCanType b)
                |> IO.applyStateT (Utils.maybeTraverseStateT variableToCanType maybeC)


fieldToCanType : Variable -> IO.StateT NameState Can.FieldType
fieldToCanType variable =
    variableToCanType variable
        |> IO.fmapStateT (\tipe -> Can.FieldType 0 tipe)



-- TO ERROR TYPE


toErrorType : Variable -> IO ET.Type
toErrorType variable =
    getVarNames variable Dict.empty
        |> IO.bind
            (\userNames ->
                IO.evalStateT (variableToErrorType variable) (makeNameState userNames)
            )


variableToErrorType : Variable -> IO.StateT NameState ET.Type
variableToErrorType variable =
    IO.liftIO (UF.get descriptorDecoder variable)
        |> IO.bindStateT
            (\(Descriptor content _ mark _) ->
                if mark == occursMark then
                    IO.pureStateT ET.Infinite

                else
                    IO.liftIO (UF.modify descriptorDecoder descriptorEncoder variable (\(Descriptor content_ rank_ _ copy_) -> Descriptor content_ rank_ occursMark copy_))
                        |> IO.bindStateT
                            (\_ ->
                                contentToErrorType variable content
                                    |> IO.bindStateT
                                        (\errType ->
                                            IO.liftIO (UF.modify descriptorDecoder descriptorEncoder variable (\(Descriptor content_ rank_ _ copy_) -> Descriptor content_ rank_ mark copy_))
                                                |> IO.fmapStateT (\_ -> errType)
                                        )
                            )
            )


contentToErrorType : Variable -> Content -> IO.StateT NameState ET.Type
contentToErrorType variable content =
    case content of
        Structure term ->
            termToErrorType term

        FlexVar maybeName ->
            case maybeName of
                Just name ->
                    IO.pureStateT (ET.FlexVar name)

                Nothing ->
                    getFreshVarName
                        |> IO.bindStateT
                            (\name ->
                                IO.liftIO
                                    (UF.modify descriptorDecoder
                                        descriptorEncoder
                                        variable
                                        (\(Descriptor _ rank mark copy) ->
                                            Descriptor (FlexVar (Just name)) rank mark copy
                                        )
                                    )
                                    |> IO.fmapStateT (\_ -> ET.FlexVar name)
                            )

        FlexSuper super maybeName ->
            case maybeName of
                Just name ->
                    IO.pureStateT (ET.FlexSuper (superToSuper super) name)

                Nothing ->
                    getFreshSuperName super
                        |> IO.bindStateT
                            (\name ->
                                IO.liftIO
                                    (UF.modify descriptorDecoder
                                        descriptorEncoder
                                        variable
                                        (\(Descriptor _ rank mark copy) ->
                                            Descriptor (FlexSuper super (Just name)) rank mark copy
                                        )
                                    )
                                    |> IO.fmapStateT (\_ -> ET.FlexSuper (superToSuper super) name)
                            )

        RigidVar name ->
            IO.pureStateT (ET.RigidVar name)

        RigidSuper super name ->
            IO.pureStateT (ET.RigidSuper (superToSuper super) name)

        Alias home name args realVariable ->
            Utils.listTraverseStateT (Utils.tupleTraverseStateT variableToErrorType) args
                |> IO.bindStateT
                    (\errArgs ->
                        variableToErrorType realVariable
                            |> IO.fmapStateT
                                (\errType ->
                                    ET.Alias home name errArgs errType
                                )
                    )

        Error ->
            IO.pureStateT ET.Error


superToSuper : SuperType -> ET.Super
superToSuper super =
    case super of
        Number ->
            ET.Number

        Comparable ->
            ET.Comparable

        Appendable ->
            ET.Appendable

        CompAppend ->
            ET.CompAppend


termToErrorType : FlatType -> IO.StateT NameState ET.Type
termToErrorType term =
    case term of
        App1 home name args ->
            Utils.listTraverseStateT variableToErrorType args
                |> IO.fmapStateT (ET.Type home name)

        Fun1 a b ->
            variableToErrorType a
                |> IO.bindStateT
                    (\arg ->
                        variableToErrorType b
                            |> IO.fmapStateT
                                (\result ->
                                    case result of
                                        ET.Lambda arg1 arg2 others ->
                                            ET.Lambda arg arg1 (arg2 :: others)

                                        _ ->
                                            ET.Lambda arg result []
                                )
                    )

        EmptyRecord1 ->
            IO.pureStateT (ET.Record Dict.empty ET.Closed)

        Record1 fields extension ->
            Utils.mapTraverseStateT compare variableToErrorType fields
                |> IO.bindStateT
                    (\errFields ->
                        variableToErrorType extension
                            |> IO.fmapStateT ET.iteratedDealias
                            |> IO.fmapStateT
                                (\errExt ->
                                    case errExt of
                                        ET.Record subFields subExt ->
                                            ET.Record (Dict.union compare subFields errFields) subExt

                                        ET.FlexVar ext ->
                                            ET.Record errFields (ET.FlexOpen ext)

                                        ET.RigidVar ext ->
                                            ET.Record errFields (ET.RigidOpen ext)

                                        _ ->
                                            crash "Used toErrorType on a type that is not well-formed"
                                )
                    )

        Unit1 ->
            IO.pureStateT ET.Unit

        Tuple1 a b maybeC ->
            IO.pureStateT ET.Tuple
                |> IO.applyStateT (variableToErrorType a)
                |> IO.applyStateT (variableToErrorType b)
                |> IO.applyStateT (Utils.maybeTraverseStateT variableToErrorType maybeC)



-- MANAGE FRESH VARIABLE NAMES


type NameState
    = NameState (Dict Name ()) Int Int Int Int Int


makeNameState : Dict Name Variable -> NameState
makeNameState taken =
    NameState (Dict.map (\_ _ -> ()) taken) 0 0 0 0 0



-- FRESH VAR NAMES


getFreshVarName : IO.StateT NameState Name
getFreshVarName =
    IO.gets (\(NameState _ normals _ _ _ _) -> normals)
        |> IO.bindStateT
            (\index ->
                IO.gets (\(NameState taken _ _ _ _ _) -> taken)
                    |> IO.bindStateT
                        (\taken ->
                            let
                                ( name, newIndex, newTaken ) =
                                    getFreshVarNameHelp index taken
                            in
                            IO.modify
                                (\(NameState _ _ numbers comparables appendables compAppends) ->
                                    NameState newTaken newIndex numbers comparables appendables compAppends
                                )
                                |> IO.fmapStateT (\_ -> name)
                        )
            )


getFreshVarNameHelp : Int -> Dict Name () -> ( Name, Int, Dict Name () )
getFreshVarNameHelp index taken =
    let
        name : Name
        name =
            Name.fromTypeVariableScheme index
    in
    if Dict.member name taken then
        getFreshVarNameHelp (index + 1) taken

    else
        ( name, index + 1, Dict.insert compare name () taken )



-- FRESH SUPER NAMES


getFreshSuperName : SuperType -> IO.StateT NameState Name
getFreshSuperName super =
    case super of
        Number ->
            getFreshSuper "number"
                (\(NameState _ _ numbers _ _ _) -> numbers)
                (\index (NameState taken normals _ comparables appendables compAppends) ->
                    NameState taken normals index comparables appendables compAppends
                )

        Comparable ->
            getFreshSuper "comparable"
                (\(NameState _ _ _ comparables _ _) -> comparables)
                (\index (NameState taken normals numbers _ appendables compAppends) ->
                    NameState taken normals numbers index appendables compAppends
                )

        Appendable ->
            getFreshSuper "appendable"
                (\(NameState _ _ _ _ appendables _) -> appendables)
                (\index (NameState taken normals numbers comparables _ compAppends) ->
                    NameState taken normals numbers comparables index compAppends
                )

        CompAppend ->
            getFreshSuper "compappend"
                (\(NameState _ _ _ _ _ compAppends) -> compAppends)
                (\index (NameState taken normals numbers comparables appendables _) ->
                    NameState taken normals numbers comparables appendables index
                )


getFreshSuper : Name -> (NameState -> Int) -> (Int -> NameState -> NameState) -> IO.StateT NameState Name
getFreshSuper prefix getter setter =
    IO.gets getter
        |> IO.bindStateT
            (\index ->
                IO.gets (\(NameState taken _ _ _ _ _) -> taken)
                    |> IO.bindStateT
                        (\taken ->
                            let
                                ( name, newIndex, newTaken ) =
                                    getFreshSuperHelp prefix index taken
                            in
                            IO.modify
                                (\(NameState _ normals numbers comparables appendables compAppends) ->
                                    setter newIndex (NameState newTaken normals numbers comparables appendables compAppends)
                                )
                                |> IO.fmapStateT (\_ -> name)
                        )
            )


getFreshSuperHelp : Name -> Int -> Dict Name () -> ( Name, Int, Dict Name () )
getFreshSuperHelp prefix index taken =
    let
        name : Name
        name =
            Name.fromTypeVariable prefix index
    in
    if Dict.member name taken then
        getFreshSuperHelp prefix (index + 1) taken

    else
        ( name, index + 1, Dict.insert compare name () taken )



-- GET ALL VARIABLE NAMES


getVarNames : Variable -> Dict Name Variable -> IO (Dict Name Variable)
getVarNames var takenNames =
    UF.get descriptorDecoder var
        |> IO.bind
            (\(Descriptor content rank mark copy) ->
                if mark == getVarNamesMark then
                    IO.pure takenNames

                else
                    UF.set descriptorEncoder var (Descriptor content rank getVarNamesMark copy)
                        |> IO.bind
                            (\_ ->
                                case content of
                                    Error ->
                                        IO.pure takenNames

                                    FlexVar maybeName ->
                                        case maybeName of
                                            Nothing ->
                                                IO.pure takenNames

                                            Just name ->
                                                addName 0 name var (FlexVar << Just) takenNames

                                    FlexSuper super maybeName ->
                                        case maybeName of
                                            Nothing ->
                                                IO.pure takenNames

                                            Just name ->
                                                addName 0 name var (FlexSuper super << Just) takenNames

                                    RigidVar name ->
                                        addName 0 name var RigidVar takenNames

                                    RigidSuper super name ->
                                        addName 0 name var (RigidSuper super) takenNames

                                    Alias _ _ args _ ->
                                        Utils.ioFoldrM getVarNames takenNames (List.map Tuple.second args)

                                    Structure flatType ->
                                        case flatType of
                                            App1 _ _ args ->
                                                Utils.ioFoldrM getVarNames takenNames args

                                            Fun1 arg body ->
                                                IO.bind (getVarNames arg) (getVarNames body takenNames)

                                            EmptyRecord1 ->
                                                IO.pure takenNames

                                            Record1 fields extension ->
                                                IO.bind (getVarNames extension)
                                                    (Utils.ioFoldrM getVarNames takenNames (Dict.values fields))

                                            Unit1 ->
                                                IO.pure takenNames

                                            Tuple1 a b Nothing ->
                                                IO.bind (getVarNames a) (getVarNames b takenNames)

                                            Tuple1 a b (Just c) ->
                                                getVarNames c takenNames
                                                    |> IO.bind (getVarNames b)
                                                    |> IO.bind (getVarNames a)
                            )
            )



-- REGISTER NAME / RENAME DUPLICATES


addName : Int -> Name -> Variable -> (Name -> Content) -> Dict Name Variable -> IO (Dict Name Variable)
addName index givenName var makeContent takenNames =
    let
        indexedName : Name
        indexedName =
            Name.fromTypeVariable givenName index
    in
    case Dict.get indexedName takenNames of
        Nothing ->
            (if indexedName == givenName then
                IO.pure ()

             else
                UF.modify descriptorDecoder
                    descriptorEncoder
                    var
                    (\(Descriptor _ rank mark copy) ->
                        Descriptor (makeContent indexedName) rank mark copy
                    )
            )
                |> IO.fmap (\_ -> Dict.insert compare indexedName var takenNames)

        Just otherVar ->
            UF.equivalent var otherVar
                |> IO.bind
                    (\same ->
                        if same then
                            IO.pure takenNames

                        else
                            addName (index + 1) givenName var makeContent takenNames
                    )
