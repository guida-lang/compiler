module Compiler.Canonicalize.Effects exposing
    ( canonicalize
    , checkPayload
    )

import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Type as Type
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Maybe exposing (Maybe(..))
import Types as T



-- RESULT


type alias EResult i w a =
    R.RResult i w Error.CREC_Error a



-- CANONICALIZE


canonicalize :
    Env.Env
    -> List (T.CRA_Located T.CASTS_Value)
    -> Dict String T.CDN_Name union
    -> T.CASTS_Effects
    -> EResult i w Can.Effects
canonicalize env values unions effects =
    case effects of
        T.CASTS_NoEffects ->
            R.ok Can.NoEffects

        T.CASTS_Ports ports ->
            let
                pairs : R.RResult i w Error.CREC_Error (List ( T.CDN_Name, Can.Port ))
                pairs =
                    R.traverse (canonicalizePort env) ports
            in
            R.fmap (Can.Ports << Dict.fromList identity) pairs

        T.CASTS_Manager region manager ->
            let
                dict : Dict String T.CDN_Name T.CRA_Region
                dict =
                    Dict.fromList identity (List.map toNameRegion values)
            in
            R.ok Can.Manager
                |> R.apply (verifyManager region dict "init")
                |> R.apply (verifyManager region dict "onEffects")
                |> R.apply (verifyManager region dict "onSelfMsg")
                |> R.apply
                    (case manager of
                        T.CASTS_Cmd cmdType ->
                            R.ok Can.Cmd
                                |> R.apply (verifyEffectType cmdType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "cmdMap"
                                            |> R.fmap (\_ -> result)
                                    )

                        T.CASTS_Sub subType ->
                            R.ok Can.Sub
                                |> R.apply (verifyEffectType subType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "subMap"
                                            |> R.fmap (\_ -> result)
                                    )

                        T.CASTS_Fx cmdType subType ->
                            R.ok Can.Fx
                                |> R.apply (verifyEffectType cmdType unions)
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


canonicalizePort : Env.Env -> T.CASTS_Port -> EResult i w ( T.CDN_Name, Can.Port )
canonicalizePort env (T.CASTS_Port (T.CRA_At region portName) tipe) =
    Type.toAnnotation env tipe
        |> R.bind
            (\(T.CASTC_Forall freeVars ctipe) ->
                case List.reverse (Type.delambda (Type.deepDealias ctipe)) of
                    (T.CASTC_TType home name [ msg ]) :: revArgs ->
                        if home == ModuleName.cmd && name == Name.cmd then
                            case revArgs of
                                [] ->
                                    R.throw (Error.CREC_PortTypeInvalid region portName Error.CREC_CmdNoArg)

                                [ outgoingType ] ->
                                    case msg of
                                        T.CASTC_TVar _ ->
                                            case checkPayload outgoingType of
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
                                                    R.throw (Error.CREC_PortPayloadInvalid region portName badType err)

                                        _ ->
                                            R.throw (Error.CREC_PortTypeInvalid region portName Error.CREC_CmdBadMsg)

                                _ ->
                                    R.throw (Error.CREC_PortTypeInvalid region portName (Error.CREC_CmdExtraArgs (List.length revArgs)))

                        else if home == ModuleName.sub && name == Name.sub then
                            case revArgs of
                                [ T.CASTC_TLambda incomingType (T.CASTC_TVar msg1) ] ->
                                    case msg of
                                        T.CASTC_TVar msg2 ->
                                            if msg1 == msg2 then
                                                case checkPayload incomingType of
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
                                                        R.throw (Error.CREC_PortPayloadInvalid region portName badType err)

                                            else
                                                R.throw (Error.CREC_PortTypeInvalid region portName Error.CREC_SubBad)

                                        _ ->
                                            R.throw (Error.CREC_PortTypeInvalid region portName Error.CREC_SubBad)

                                _ ->
                                    R.throw (Error.CREC_PortTypeInvalid region portName Error.CREC_SubBad)

                        else
                            R.throw (Error.CREC_PortTypeInvalid region portName Error.CREC_NotCmdOrSub)

                    _ ->
                        R.throw (Error.CREC_PortTypeInvalid region portName Error.CREC_NotCmdOrSub)
            )



-- VERIFY MANAGER


verifyEffectType : T.CRA_Located T.CDN_Name -> Dict String T.CDN_Name a -> EResult i w T.CDN_Name
verifyEffectType (T.CRA_At region name) unions =
    if Dict.member identity name unions then
        R.ok name

    else
        R.throw (Error.CREC_EffectNotFound region name)


toNameRegion : T.CRA_Located T.CASTS_Value -> ( T.CDN_Name, T.CRA_Region )
toNameRegion (T.CRA_At _ (T.CASTS_Value (T.CRA_At region name) _ _ _)) =
    ( name, region )


verifyManager : T.CRA_Region -> Dict String T.CDN_Name T.CRA_Region -> T.CDN_Name -> EResult i w T.CRA_Region
verifyManager tagRegion values name =
    case Dict.get identity name values of
        Just region ->
            R.ok region

        Nothing ->
            R.throw (Error.CREC_EffectFunctionNotFound tagRegion name)



-- CHECK PAYLOAD TYPES


checkPayload : T.CASTC_Type -> Result ( T.CASTC_Type, Error.CREC_InvalidPayload ) ()
checkPayload tipe =
    case tipe of
        T.CASTC_TAlias _ _ args aliasedType ->
            checkPayload (Type.dealias args aliasedType)

        T.CASTC_TType home name args ->
            case args of
                [] ->
                    if isJson home name || isString home name || isIntFloatBool home name then
                        Ok ()

                    else
                        Err ( tipe, Error.CREC_UnsupportedType name )

                [ arg ] ->
                    if isList home name || isMaybe home name || isArray home name then
                        checkPayload arg

                    else
                        Err ( tipe, Error.CREC_UnsupportedType name )

                _ ->
                    Err ( tipe, Error.CREC_UnsupportedType name )

        T.CASTC_TUnit ->
            Ok ()

        T.CASTC_TTuple a b maybeC ->
            checkPayload a
                |> Result.andThen (\_ -> checkPayload b)
                |> Result.andThen
                    (\_ ->
                        case maybeC of
                            Nothing ->
                                Ok ()

                            Just c ->
                                checkPayload c
                    )

        T.CASTC_TVar name ->
            Err ( tipe, Error.CREC_TypeVariable name )

        T.CASTC_TLambda _ _ ->
            Err ( tipe, Error.CREC_Function )

        T.CASTC_TRecord _ (Just _) ->
            Err ( tipe, Error.CREC_ExtendedRecord )

        T.CASTC_TRecord fields Nothing ->
            Dict.foldl compare
                (\_ field acc -> Result.andThen (\_ -> checkFieldPayload field) acc)
                (Ok ())
                fields


checkFieldPayload : T.CASTC_FieldType -> Result ( T.CASTC_Type, Error.CREC_InvalidPayload ) ()
checkFieldPayload (T.CASTC_FieldType _ tipe) =
    checkPayload tipe


isIntFloatBool : T.CEMN_Canonical -> T.CDN_Name -> Bool
isIntFloatBool home name =
    home
        == ModuleName.basics
        && (name == Name.int || name == Name.float || name == Name.bool)


isString : T.CEMN_Canonical -> T.CDN_Name -> Bool
isString home name =
    home
        == ModuleName.string
        && name
        == Name.string


isJson : T.CEMN_Canonical -> T.CDN_Name -> Bool
isJson home name =
    (home == ModuleName.jsonEncode)
        && (name == Name.value)


isList : T.CEMN_Canonical -> T.CDN_Name -> Bool
isList home name =
    home
        == ModuleName.list
        && name
        == Name.list


isMaybe : T.CEMN_Canonical -> T.CDN_Name -> Bool
isMaybe home name =
    home
        == ModuleName.maybe
        && name
        == Name.maybe


isArray : T.CEMN_Canonical -> T.CDN_Name -> Bool
isArray home name =
    home
        == ModuleName.array
        && name
        == Name.array
