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

{-| Core compiler type definitions and utility constructors.

This module defines the type language used by the solver, helper constructors
for built-in and primitive types, and conversion helpers used by reporting and
annotation generation.

-}

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
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


{-| Type constraints produced by the compiler during inference.

Constraints are solved by the type solver and include equality checks,
pattern expectations, and let-generalization scopes.

-}
type Constraint
    = CTrue
    | CSaveTheEnvironment
    | CEqual A.Region E.Category Type (E.Expected Type)
    | CLocal A.Region Name (E.Expected Type)
    | CForeign A.Region Name Can.Annotation (E.Expected Type)
    | CPattern A.Region E.PCategory Type (E.PExpected Type)
    | CAnd (List Constraint)
    | CLet (List Variable) (List Variable) (Dict String Name (A.Located Type)) Constraint Constraint


{-| Introduce a scope with existentially quantified flexible variables.

This helper is used to model local inference scopes where the variables in
`flexVars` are treated as existentials for `constraint`.

-}
exists : List Variable -> Constraint -> Constraint
exists flexVars constraint =
    CLet [] flexVars Dict.empty constraint CTrue



-- TYPE PRIMITIVES


{-| The internal type representation used by the compiler's solver.

This type includes placeholders, aliases, flexible and rigid variables, type
applications, function arrows, records, tuples, and unit.

-}
type Type
    = PlaceHolder Name
    | AliasN IO.Canonical Name (List ( Name, Type )) Type
    | VarN Variable
    | AppN IO.Canonical Name (List Type)
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Dict String Name Type) Type
    | UnitN
    | TupleN Type Type (List Type)



-- DESCRIPTORS


{-| Create a new descriptor for a solver variable.

Descriptors track the variable's current content, rank, occurrence mark,
and copy state during inference.

-}
makeDescriptor : Content -> Descriptor
makeDescriptor content =
    Descriptor content noRank noMark Nothing



-- RANKS


{-| A rank reserved for un-generalized solver variables.
-}
noRank : Int
noRank =
    0


{-| The outermost solver rank used for top-level inference.
-}
outermostRank : Int
outermostRank =
    1



-- MARKS


{-| The default mark for newly created variables before any occurrence or
name-tracking pass.
-}
noMark : Mark
noMark =
    Mark 2


{-| A mark used when a variable is currently being visited during occurs-checks.
-}
occursMark : Mark
occursMark =
    Mark 1


{-| A mark used when collecting all named solver variables from a term.
-}
getVarNamesMark : Mark
getVarNamesMark =
    Mark 0


{-| Allocate a fresh mark for tracking solver variable visits.
-}
nextMark : Mark -> Mark
nextMark (Mark mark) =
    Mark (mark + 1)



-- FUNCTION TYPES


{-| Construct a function arrow type from an argument to a result.
-}
funType : Type -> Type -> Type
funType =
    FunN



-- PRIMITIVE TYPES


{-| Primitive built-in type constructors for the current compilation target.
-}
int : Target -> Type
int target =
    AppN (ModuleName.basics target) "Int" []


{-| The built-in `Float` type for the current compilation target.
-}
float : Target -> Type
float target =
    AppN (ModuleName.basics target) "Float" []


{-| The built-in `Char` type for the current compilation target.
-}
char : Target -> Type
char target =
    AppN (ModuleName.char target) "Char" []


{-| The built-in `String` type for the current compilation target.
-}
string : Target -> Type
string target =
    AppN (ModuleName.string target) "String" []


{-| The built-in `Bool` type for the current compilation target.
-}
bool : Target -> Type
bool target =
    AppN (ModuleName.basics target) "Bool" []


{-| The built-in `Never` type for the current compilation target.
-}
never : Target -> Type
never target =
    AppN (ModuleName.basics target) "Never" []



-- WEBGL TYPES


{-| 2D vector type for WebGL targets.
-}
vec2 : Target -> Type
vec2 target =
    AppN (ModuleName.vector2 target) "Vec2" []


{-| 3D vector type for WebGL targets.
-}
vec3 : Target -> Type
vec3 target =
    AppN (ModuleName.vector3 target) "Vec3" []


{-| 4D vector type for WebGL targets.
-}
vec4 : Target -> Type
vec4 target =
    AppN (ModuleName.vector4 target) "Vec4" []


{-| WebGL matrix type for WebGL targets.
-}
mat4 : Target -> Type
mat4 target =
    AppN (ModuleName.matrix4 target) "Mat4" []


{-| GPU texture type for WebGL targets.
-}
texture : Target -> Type
texture target =
    AppN (ModuleName.texture target) "Texture" []



-- MAKE FLEX VARIABLES


{-| Create a fresh unnamed flexible type variable.
-}
mkFlexVar : IO Variable
mkFlexVar =
    UF.fresh flexVarDescriptor


{-| Descriptor for a fresh unnamed flexible solver variable.

This descriptor is used when creating a newly allocated flexible type variable
in the solver.

-}
flexVarDescriptor : Descriptor
flexVarDescriptor =
    makeDescriptor unnamedFlexVar


{-| A fresh unnamed flexible variable content placeholder.
-}
unnamedFlexVar : Content
unnamedFlexVar =
    FlexVar Nothing



-- MAKE FLEX NUMBERS


{-| Create a fresh unnamed flexible numeric variable.

This variable can later unify with any numeric supertype during inference.

-}
mkFlexNumber : IO Variable
mkFlexNumber =
    UF.fresh flexNumberDescriptor


{-| Descriptor for a fresh unnamed flexible numeric solver variable.

This descriptor is used when allocating a fresh flexible variable that can
later unify with any numeric supertype.

-}
flexNumberDescriptor : Descriptor
flexNumberDescriptor =
    makeDescriptor (unnamedFlexSuper Number)


{-| An unnamed flexible supertype placeholder.

This is used to create placeholder content for fresh solver variables that
represent a specific supertype category such as `Number` or `Comparable`.

-}
unnamedFlexSuper : SuperType -> Content
unnamedFlexSuper super =
    FlexSuper super Nothing



-- MAKE NAMED VARIABLES


{-| Create a flexible solver variable with a user-provided name.

Named flexible variables are useful for preserving user-facing names in the
solver state while still allowing unification.

-}
nameToFlex : Name -> IO Variable
nameToFlex name =
    UF.fresh <|
        makeDescriptor <|
            Maybe.unwrap FlexVar FlexSuper (toSuper name) (Just name)


{-| Create a rigid solver variable with a user-provided name.

Rigid variables do not unify with other types during inference and are used to
represent fixed type names.

-}
nameToRigid : Name -> IO Variable
nameToRigid name =
    UF.fresh <|
        makeDescriptor <|
            Maybe.unwrap RigidVar RigidSuper (toSuper name) name


{-| Determine whether a user-provided name belongs to a known supertype.

Returns the corresponding `SuperType` for known numeric, comparable, appendable,
or compappend names.

-}
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


{-| Convert a solver variable into a canonical type annotation.

This is used by reporting and by the compiler front-end when producing
`Can.Annotation` values from solver state.

-}
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


{-| Convert a solver variable into a canonical AST type.

This is used when producing `Can.Type` values for annotation generation and
reporting.

-}
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


{-| Convert a flat solver type term into a canonical AST type.
-}
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
            State.traverseMap compare identity fieldToCanType fields
                |> State.bind
                    (\canFields ->
                        variableToCanType extension
                            |> State.fmap Type.iteratedDealias
                            |> State.fmap
                                (\canExt ->
                                    case canExt of
                                        Can.TRecord subFields subExt ->
                                            Can.TRecord (Dict.union subFields canFields) subExt

                                        Can.TVar name ->
                                            Can.TRecord canFields (Just name)

                                        _ ->
                                            crash "Used toAnnotation on a type that is not well-formed"
                                )
                    )

        Unit1 ->
            State.pure Can.TUnit

        Tuple1 a b cs ->
            State.pure Can.TTuple
                |> State.apply (variableToCanType a)
                |> State.apply (variableToCanType b)
                |> State.apply (State.traverseList variableToCanType cs)


{-| Convert a record field solver variable into a canonical field type.

This wraps a field variable in `Can.FieldType` so record construction and
annotation generation can use a consistent field representation.

-}
fieldToCanType : Variable -> StateT NameState Can.FieldType
fieldToCanType variable =
    variableToCanType variable
        |> State.fmap (\tipe -> Can.FieldType 0 tipe)



-- TO ERROR TYPE


{-| Convert a solver variable into an error-reporting type.

This is used to render type errors from the solver's internal state.

-}
toErrorType : Variable -> IO ET.Type
toErrorType variable =
    getVarNames variable Dict.empty
        |> IO.bind
            (\userNames ->
                State.evalStateT (variableToErrorType variable) (makeNameState userNames)
            )


{-| Convert a solver variable into an error-reporting type.

This wraps variable-to-type conversion with occurs checks and name tracking
for error messages.

-}
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


{-| Convert solver variable content into an error-reporting type.
-}
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


{-| Convert a solver supertype into the error-reporting supertype representation.
-}
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


{-| Convert a flat solver type term into an error-reporting type.
-}
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
            State.traverseMap compare identity variableToErrorType fields
                |> State.bind
                    (\errFields ->
                        variableToErrorType extension
                            |> State.fmap ET.iteratedDealias
                            |> State.fmap
                                (\errExt ->
                                    case errExt of
                                        ET.Record subFields subExt ->
                                            ET.Record (Dict.union subFields errFields) subExt

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

        Tuple1 a b cs ->
            State.pure ET.Tuple
                |> State.apply (variableToErrorType a)
                |> State.apply (variableToErrorType b)
                |> State.apply (State.traverseList variableToErrorType cs)



-- MANAGE FRESH VARIABLE NAMES


{-| Internal state used while generating fresh names for type variables.

The `NameState` contains the set of taken names and counters for normal,
comparable, appendable, and compappend name generation.

-}
type NameState
    = NameState (Dict String Name ()) Int Int Int Int Int


{-| Create initial name state from already taken solver variable names.
-}
makeNameState : Dict String Name Variable -> NameState
makeNameState taken =
    NameState (Dict.map (\_ _ -> ()) taken) 0 0 0 0 0



-- FRESH VAR NAMES


{-| Generate a fresh normal type variable name within the current name state.
-}
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


{-| Helper for `getFreshVarName` that avoids duplicate type variable names.
-}
getFreshVarNameHelp : Int -> Dict String Name () -> ( Name, Int, Dict String Name () )
getFreshVarNameHelp index taken =
    let
        name : Name
        name =
            Name.fromTypeVariableScheme index
    in
    if Dict.member identity name taken then
        getFreshVarNameHelp (index + 1) taken

    else
        ( name, index + 1, Dict.insert identity name () taken )



-- FRESH SUPER NAMES


{-| Generate a fresh named supertype variable within the current name state.
-}
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


{-| Generic helper for generating fresh supertype variable names.

The caller provides the supertype prefix and state accessors for the
appropriate name counter.

-}
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


{-| Helper for `getFreshSuper` that avoids duplicate supertype names.
-}
getFreshSuperHelp : Name -> Int -> Dict String Name () -> ( Name, Int, Dict String Name () )
getFreshSuperHelp prefix index taken =
    let
        name : Name
        name =
            Name.fromTypeVariable prefix index
    in
    if Dict.member identity name taken then
        getFreshSuperHelp prefix (index + 1) taken

    else
        ( name, index + 1, Dict.insert identity name () taken )



-- GET ALL VARIABLE NAMES


{-| Collect all named solver variables reachable from a variable.

This traverses the variable's structure and records any named flexible,
rigid, or alias variables for later annotation generation.

-}
getVarNames : Variable -> Dict String Name Variable -> IO (Dict String Name Variable)
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
                                                    (IO.foldrM getVarNames takenNames (Dict.values compare fields))

                                            Unit1 ->
                                                IO.pure takenNames

                                            Tuple1 a b cs ->
                                                IO.foldrM getVarNames takenNames (a :: b :: cs)
                            )
            )



-- REGISTER NAME / RENAME DUPLICATES


{-| Register a named solver variable and rename duplicates if necessary.

If the given name is already taken by another variable, this function will
try an indexed variant until it finds a unique name.

-}
addName : Int -> Name -> Variable -> (Name -> Content) -> Dict String Name Variable -> IO (Dict String Name Variable)
addName index givenName var makeContent takenNames =
    let
        indexedName : Name
        indexedName =
            Name.fromTypeVariable givenName index
    in
    case Dict.get identity indexedName takenNames of
        Nothing ->
            (if indexedName == givenName then
                IO.pure ()

             else
                UF.modify var
                    (\(Descriptor _ rank mark copy) ->
                        Descriptor (makeContent indexedName) rank mark copy
                    )
            )
                |> IO.fmap (\_ -> Dict.insert identity indexedName var takenNames)

        Just otherVar ->
            UF.equivalent var otherVar
                |> IO.bind
                    (\same ->
                        if same then
                            IO.pure takenNames

                        else
                            addName (index + 1) givenName var makeContent takenNames
                    )
