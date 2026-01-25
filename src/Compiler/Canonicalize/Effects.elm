module Compiler.Canonicalize.Effects exposing
    ( canonicalize
    , checkPayload
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.AST.Utils.Type as Type
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Name as Name
import Compiler.Generate.Target exposing (Target)
import Compiler.Guida.ModuleName as ModuleName
import Compiler.Parse.SyntaxVersion as SV exposing (SyntaxVersion)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import System.TypeCheck.IO as IO



-- RESULT


type alias EResult i w a =
    R.RResult i w Error.Error a



-- CANONICALIZE


canonicalize : Target -> SyntaxVersion -> Env.Env -> List (A.Located Src.Value) -> Dict String Name.Name union -> Src.Effects -> EResult i w Can.Effects
canonicalize target syntaxVersion env values unions effects =
    case effects of
        Src.NoEffects ->
            R.ok Can.NoEffects

        Src.Ports ports ->
            R.traverse (canonicalizePort target syntaxVersion env) ports
                |> R.fmap (Can.Ports << Dict.fromList identity)

        Src.Manager region manager ->
            let
                dict : Dict String Name.Name A.Region
                dict =
                    Dict.fromList identity (List.map toNameRegion values)
            in
            R.fmap Can.Manager (verifyManager region dict "init")
                |> R.apply (verifyManager region dict "onEffects")
                |> R.apply (verifyManager region dict "onSelfMsg")
                |> R.apply
                    (case manager of
                        Src.Cmd ( _, ( _, cmdType ) ) ->
                            R.fmap Can.Cmd (verifyEffectType cmdType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "cmdMap"
                                            |> R.fmap (\_ -> result)
                                    )

                        Src.Sub ( _, ( _, subType ) ) ->
                            R.fmap Can.Sub (verifyEffectType subType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "subMap"
                                            |> R.fmap (\_ -> result)
                                    )

                        Src.Fx ( _, ( _, cmdType ) ) ( _, ( _, subType ) ) ->
                            R.fmap Can.Fx (verifyEffectType cmdType unions)
                                |> R.apply (verifyEffectType subType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "cmdMap"
                                            |> R.fmap (\_ -> result)
                                    )
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "subMap"
                                            |> R.fmap (\_ -> result)
                                    )
                    )



-- CANONICALIZE PORT


canonicalizePort : Target -> SyntaxVersion -> Env.Env -> Src.Port -> EResult i w ( Name.Name, Can.Port )
canonicalizePort target syntaxVersion env (Src.Port _ ( _, A.At region portName ) tipe) =
    Type.toAnnotation target syntaxVersion env tipe
        |> R.bind
            (\(Can.Forall freeVars ctipe) ->
                case List.reverse (Type.delambda (Type.deepDealias ctipe)) of
                    (Can.TType home name [ msg ]) :: revArgs ->
                        if home == ModuleName.cmd target && name == Name.cmd then
                            case revArgs of
                                [] ->
                                    R.throw (Error.PortTypeInvalid target region portName Error.CmdNoArg)

                                [ outgoingType ] ->
                                    case msg of
                                        Can.TVar _ ->
                                            case checkPayload target syntaxVersion outgoingType of
                                                Ok () ->
                                                    R.ok
                                                        ( portName
                                                        , Can.Outgoing
                                                            { freeVars = freeVars
                                                            , payload = outgoingType
                                                            , func = ctipe
                                                            }
                                                        )

                                                Err ( badType, err ) ->
                                                    R.throw (Error.PortPayloadInvalid target region portName badType err)

                                        _ ->
                                            R.throw (Error.PortTypeInvalid target region portName Error.CmdBadMsg)

                                _ ->
                                    R.throw (Error.PortTypeInvalid target region portName (Error.CmdExtraArgs (List.length revArgs)))

                        else if home == ModuleName.sub target && name == Name.sub then
                            case revArgs of
                                [ Can.TLambda incomingType (Can.TVar msg1) ] ->
                                    case msg of
                                        Can.TVar msg2 ->
                                            if msg1 == msg2 then
                                                case checkPayload target syntaxVersion incomingType of
                                                    Ok () ->
                                                        R.ok
                                                            ( portName
                                                            , Can.Incoming
                                                                { freeVars = freeVars
                                                                , payload = incomingType
                                                                , func = ctipe
                                                                }
                                                            )

                                                    Err ( badType, err ) ->
                                                        R.throw (Error.PortPayloadInvalid target region portName badType err)

                                            else
                                                R.throw (Error.PortTypeInvalid target region portName Error.SubBad)

                                        _ ->
                                            R.throw (Error.PortTypeInvalid target region portName Error.SubBad)

                                _ ->
                                    R.throw (Error.PortTypeInvalid target region portName Error.SubBad)

                        else
                            R.throw (Error.PortTypeInvalid target region portName Error.NotCmdOrSub)

                    _ ->
                        R.throw (Error.PortTypeInvalid target region portName Error.NotCmdOrSub)
            )



-- VERIFY MANAGER


verifyEffectType : A.Located Name.Name -> Dict String Name.Name a -> EResult i w Name.Name
verifyEffectType (A.At region name) unions =
    if Dict.member identity name unions then
        R.ok name

    else
        R.throw (Error.EffectNotFound region name)


toNameRegion : A.Located Src.Value -> ( Name.Name, A.Region )
toNameRegion (A.At _ (Src.Value _ ( _, A.At region name ) _ _ _)) =
    ( name, region )


verifyManager : A.Region -> Dict String Name.Name A.Region -> Name.Name -> EResult i w A.Region
verifyManager tagRegion values name =
    case Dict.get identity name values of
        Just region ->
            R.ok region

        Nothing ->
            R.throw (Error.EffectFunctionNotFound tagRegion name)



-- CHECK PAYLOAD TYPES


checkPayload : Target -> SyntaxVersion -> Can.Type -> Result ( Can.Type, Error.InvalidPayload ) ()
checkPayload target syntaxVersion tipe =
    case tipe of
        Can.TAlias _ _ args aliasedType ->
            checkPayload target syntaxVersion (Type.dealias args aliasedType)

        Can.TType home name args ->
            case args of
                [] ->
                    if isJson target home name || isString target home name || isIntFloatBool target home name || allowsBytes target syntaxVersion home name then
                        Ok ()

                    else
                        Err ( tipe, Error.UnsupportedType syntaxVersion name )

                [ arg ] ->
                    if isList target home name || isMaybe target home name || isArray target home name then
                        checkPayload target syntaxVersion arg

                    else
                        Err ( tipe, Error.UnsupportedType syntaxVersion name )

                _ ->
                    Err ( tipe, Error.UnsupportedType syntaxVersion name )

        Can.TUnit ->
            Ok ()

        Can.TTuple a b cs ->
            checkPayload target syntaxVersion a
                |> Result.andThen (\_ -> checkPayload target syntaxVersion b)
                |> Result.andThen (\_ -> checkPayloadTupleCs target syntaxVersion cs)

        Can.TVar name ->
            Err ( tipe, Error.TypeVariable name )

        Can.TLambda _ _ ->
            Err ( tipe, Error.Function )

        Can.TRecord _ (Just _) ->
            Err ( tipe, Error.ExtendedRecord )

        Can.TRecord fields Nothing ->
            Dict.foldl compare
                (\_ field acc -> Result.andThen (\_ -> checkFieldPayload target syntaxVersion field) acc)
                (Ok ())
                fields


checkPayloadTupleCs : Target -> SyntaxVersion -> List Can.Type -> Result ( Can.Type, Error.InvalidPayload ) ()
checkPayloadTupleCs target syntaxVersion types =
    case types of
        [] ->
            Ok ()

        tipe :: rest ->
            checkPayload target syntaxVersion tipe
                |> Result.andThen (\_ -> checkPayloadTupleCs target syntaxVersion rest)


checkFieldPayload : Target -> SyntaxVersion -> Can.FieldType -> Result ( Can.Type, Error.InvalidPayload ) ()
checkFieldPayload target syntaxVersion (Can.FieldType _ tipe) =
    checkPayload target syntaxVersion tipe


isIntFloatBool : Target -> IO.Canonical -> Name.Name -> Bool
isIntFloatBool target home name =
    (home == ModuleName.basics target)
        && (name == Name.int || name == Name.float || name == Name.bool)


isString : Target -> IO.Canonical -> Name.Name -> Bool
isString target home name =
    (home == ModuleName.string target)
        && (name == Name.string)


isJson : Target -> IO.Canonical -> Name.Name -> Bool
isJson target home name =
    (home == ModuleName.jsonEncode target)
        && (name == Name.value)


isList : Target -> IO.Canonical -> Name.Name -> Bool
isList target home name =
    (home == ModuleName.list target)
        && (name == Name.list)


isMaybe : Target -> IO.Canonical -> Name.Name -> Bool
isMaybe target home name =
    home == ModuleName.maybe target && name == Name.maybe


isArray : Target -> IO.Canonical -> Name.Name -> Bool
isArray target home name =
    home == ModuleName.array target && name == Name.array


allowsBytes : Target -> SyntaxVersion -> IO.Canonical -> Name.Name -> Bool
allowsBytes target syntaxVersion home name =
    syntaxVersion == SV.Guida && isBytes target home name


isBytes : Target -> IO.Canonical -> Name.Name -> Bool
isBytes target home name =
    home == ModuleName.bytes target && name == Name.bytes
