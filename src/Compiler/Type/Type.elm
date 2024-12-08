module Compiler.Type.Type exposing
    ( Constraint(..)
    , Type(..)
    , bool
    , char
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
    , vec2
    , vec3
    , vec4
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Error as ET
import Compiler.Type.UnionFind as UF
import Control.Monad.State.TypeCheck.Strict as State exposing (StateT, liftIO)
import Data.Map as Dict exposing (Dict)
import Maybe.Extra as Maybe
import System.TypeCheck.IO as IO exposing (Content(..), Descriptor(..), FlatType(..), IO, Mark(..), SuperType(..), Variable)
import Utils.Crash exposing (crash)



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


type Type
    = PlaceHolder Name
    | AliasN IO.Canonical Name (List ( Name, Type )) Type
    | VarN Variable
    | AppN IO.Canonical Name (List Type)
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Dict Name Type) Type
    | UnitN
    | TupleN Type Type (Maybe Type)



-- DESCRIPTORS


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
    UF.fresh flexVarDescriptor


flexVarDescriptor : Descriptor
flexVarDescriptor =
    makeDescriptor unnamedFlexVar


unnamedFlexVar : Content
unnamedFlexVar =
    FlexVar Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber : IO Variable
mkFlexNumber =
    UF.fresh flexNumberDescriptor


flexNumberDescriptor : Descriptor
flexNumberDescriptor =
    makeDescriptor (unnamedFlexSuper Number)


unnamedFlexSuper : SuperType -> Content
unnamedFlexSuper super =
    FlexSuper super Nothing



-- MAKE NAMED VARIABLES


nameToFlex : Name -> IO Variable
nameToFlex name =
    UF.fresh <|
        makeDescriptor <|
            Maybe.unwrap FlexVar FlexSuper (toSuper name) (Just name)


nameToRigid : Name -> IO Variable
nameToRigid name =
    UF.fresh <|
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
                State.runStateT (variableToCanType variable) (makeNameState userNames)
                    |> IO.fmap
                        (\( tipe, NameState freeVars _ _ _ _ _ ) ->
                            Can.Forall freeVars tipe
                        )
            )


variableToCanType : Variable -> State.StateT NameState Can.Type
variableToCanType variable =
    liftIO (UF.get variable)
        |> State.bind
            (\(Descriptor content _ _ _) ->
                case content of
                    Structure term ->
                        termToCanType term

                    FlexVar maybeName ->
                        case maybeName of
                            Just name ->
                                State.pure (Can.TVar name)

                            Nothing ->
                                getFreshVarName
                                    |> State.bind
                                        (\name ->
                                            liftIO
                                                (UF.modify variable
                                                    (\(Descriptor _ rank mark copy) ->
                                                        Descriptor (FlexVar (Just name)) rank mark copy
                                                    )
                                                )
                                                |> State.fmap (\_ -> Can.TVar name)
                                        )

                    FlexSuper super maybeName ->
                        case maybeName of
                            Just name ->
                                State.pure (Can.TVar name)

                            Nothing ->
                                getFreshSuperName super
                                    |> State.bind
                                        (\name ->
                                            liftIO
                                                (UF.modify variable
                                                    (\(Descriptor _ rank mark copy) ->
                                                        Descriptor (FlexSuper super (Just name)) rank mark copy
                                                    )
                                                )
                                                |> State.fmap (\_ -> Can.TVar name)
                                        )

                    RigidVar name ->
                        State.pure (Can.TVar name)

                    RigidSuper _ name ->
                        State.pure (Can.TVar name)

                    Alias home name args realVariable ->
                        State.traverseList (State.traverseTuple variableToCanType) args
                            |> State.bind
                                (\canArgs ->
                                    variableToCanType realVariable
                                        |> State.fmap
                                            (\canType ->
                                                Can.TAlias home name canArgs (Can.Filled canType)
                                            )
                                )

                    Error ->
                        crash "cannot handle Error types in variableToCanType"
            )


termToCanType : FlatType -> StateT NameState Can.Type
termToCanType term =
    case term of
        App1 home name args ->
            State.traverseList variableToCanType args
                |> State.fmap (Can.TType home name)

        Fun1 a b ->
            State.pure Can.TLambda
                |> State.apply (variableToCanType a)
                |> State.apply (variableToCanType b)

        EmptyRecord1 ->
            State.pure (Can.TRecord Dict.empty Nothing)

        Record1 fields extension ->
            State.traverseMap compare fieldToCanType fields
                |> State.bind
                    (\canFields ->
                        variableToCanType extension
                            |> State.fmap Type.iteratedDealias
                            |> State.fmap
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
            State.pure Can.TUnit

        Tuple1 a b maybeC ->
            State.pure Can.TTuple
                |> State.apply (variableToCanType a)
                |> State.apply (variableToCanType b)
                |> State.apply (State.traverseMaybe variableToCanType maybeC)


fieldToCanType : Variable -> StateT NameState Can.FieldType
fieldToCanType variable =
    variableToCanType variable
        |> State.fmap (\tipe -> Can.FieldType 0 tipe)



-- TO ERROR TYPE


toErrorType : Variable -> IO ET.Type
toErrorType variable =
    getVarNames variable Dict.empty
        |> IO.bind
            (\userNames ->
                State.evalStateT (variableToErrorType variable) (makeNameState userNames)
            )


variableToErrorType : Variable -> StateT NameState ET.Type
variableToErrorType variable =
    liftIO (UF.get variable)
        |> State.bind
            (\(Descriptor content _ mark _) ->
                if mark == occursMark then
                    State.pure ET.Infinite

                else
                    liftIO (UF.modify variable (\(Descriptor content_ rank_ _ copy_) -> Descriptor content_ rank_ occursMark copy_))
                        |> State.bind
                            (\_ ->
                                contentToErrorType variable content
                                    |> State.bind
                                        (\errType ->
                                            liftIO (UF.modify variable (\(Descriptor content_ rank_ _ copy_) -> Descriptor content_ rank_ mark copy_))
                                                |> State.fmap (\_ -> errType)
                                        )
                            )
            )


contentToErrorType : Variable -> Content -> StateT NameState ET.Type
contentToErrorType variable content =
    case content of
        Structure term ->
            termToErrorType term

        FlexVar maybeName ->
            case maybeName of
                Just name ->
                    State.pure (ET.FlexVar name)

                Nothing ->
                    getFreshVarName
                        |> State.bind
                            (\name ->
                                liftIO
                                    (UF.modify variable
                                        (\(Descriptor _ rank mark copy) ->
                                            Descriptor (FlexVar (Just name)) rank mark copy
                                        )
                                    )
                                    |> State.fmap (\_ -> ET.FlexVar name)
                            )

        FlexSuper super maybeName ->
            case maybeName of
                Just name ->
                    State.pure (ET.FlexSuper (superToSuper super) name)

                Nothing ->
                    getFreshSuperName super
                        |> State.bind
                            (\name ->
                                liftIO
                                    (UF.modify variable
                                        (\(Descriptor _ rank mark copy) ->
                                            Descriptor (FlexSuper super (Just name)) rank mark copy
                                        )
                                    )
                                    |> State.fmap (\_ -> ET.FlexSuper (superToSuper super) name)
                            )

        RigidVar name ->
            State.pure (ET.RigidVar name)

        RigidSuper super name ->
            State.pure (ET.RigidSuper (superToSuper super) name)

        Alias home name args realVariable ->
            State.traverseList (State.traverseTuple variableToErrorType) args
                |> State.bind
                    (\errArgs ->
                        variableToErrorType realVariable
                            |> State.fmap
                                (\errType ->
                                    ET.Alias home name errArgs errType
                                )
                    )

        Error ->
            State.pure ET.Error


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


termToErrorType : FlatType -> StateT NameState ET.Type
termToErrorType term =
    case term of
        App1 home name args ->
            State.traverseList variableToErrorType args
                |> State.fmap (ET.Type home name)

        Fun1 a b ->
            variableToErrorType a
                |> State.bind
                    (\arg ->
                        variableToErrorType b
                            |> State.fmap
                                (\result ->
                                    case result of
                                        ET.Lambda arg1 arg2 others ->
                                            ET.Lambda arg arg1 (arg2 :: others)

                                        _ ->
                                            ET.Lambda arg result []
                                )
                    )

        EmptyRecord1 ->
            State.pure (ET.Record Dict.empty ET.Closed)

        Record1 fields extension ->
            State.traverseMap compare variableToErrorType fields
                |> State.bind
                    (\errFields ->
                        variableToErrorType extension
                            |> State.fmap ET.iteratedDealias
                            |> State.fmap
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
            State.pure ET.Unit

        Tuple1 a b maybeC ->
            State.pure ET.Tuple
                |> State.apply (variableToErrorType a)
                |> State.apply (variableToErrorType b)
                |> State.apply (State.traverseMaybe variableToErrorType maybeC)



-- MANAGE FRESH VARIABLE NAMES


type NameState
    = NameState (Dict Name ()) Int Int Int Int Int


makeNameState : Dict Name Variable -> NameState
makeNameState taken =
    NameState (Dict.map (\_ _ -> ()) taken) 0 0 0 0 0



-- FRESH VAR NAMES


getFreshVarName : StateT NameState Name
getFreshVarName =
    State.gets (\(NameState _ normals _ _ _ _) -> normals)
        |> State.bind
            (\index ->
                State.gets (\(NameState taken _ _ _ _ _) -> taken)
                    |> State.bind
                        (\taken ->
                            let
                                ( name, newIndex, newTaken ) =
                                    getFreshVarNameHelp index taken
                            in
                            State.modify
                                (\(NameState _ _ numbers comparables appendables compAppends) ->
                                    NameState newTaken newIndex numbers comparables appendables compAppends
                                )
                                |> State.fmap (\_ -> name)
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


getFreshSuperName : SuperType -> StateT NameState Name
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


getFreshSuper : Name -> (NameState -> Int) -> (Int -> NameState -> NameState) -> StateT NameState Name
getFreshSuper prefix getter setter =
    State.gets getter
        |> State.bind
            (\index ->
                State.gets (\(NameState taken _ _ _ _ _) -> taken)
                    |> State.bind
                        (\taken ->
                            let
                                ( name, newIndex, newTaken ) =
                                    getFreshSuperHelp prefix index taken
                            in
                            State.modify
                                (\(NameState _ normals numbers comparables appendables compAppends) ->
                                    setter newIndex (NameState newTaken normals numbers comparables appendables compAppends)
                                )
                                |> State.fmap (\_ -> name)
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
    UF.get var
        |> IO.bind
            (\(Descriptor content rank mark copy) ->
                if mark == getVarNamesMark then
                    IO.pure takenNames

                else
                    UF.set var (Descriptor content rank getVarNamesMark copy)
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
                                        IO.foldrM getVarNames takenNames (List.map Tuple.second args)

                                    Structure flatType ->
                                        case flatType of
                                            App1 _ _ args ->
                                                IO.foldrM getVarNames takenNames args

                                            Fun1 arg body ->
                                                IO.bind (getVarNames arg) (getVarNames body takenNames)

                                            EmptyRecord1 ->
                                                IO.pure takenNames

                                            Record1 fields extension ->
                                                IO.bind (getVarNames extension)
                                                    (IO.foldrM getVarNames takenNames (Dict.values fields))

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
                UF.modify var
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
