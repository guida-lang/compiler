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
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as R
import Data.Map as Dict exposing (Dict)
import Maybe exposing (Maybe(..))
import System.TypeCheck.IO as IO



-- RESULT


type alias EResult i w a =
    R.RResult i w Error.Error a



-- CANONICALIZE


canonicalize :
    Env.Env
    -> List (A.CRA_Located Src.CASTS_Value)
    -> Dict String Name.CDN_Name union
    -> Src.CASTS_Effects
    -> EResult i w Can.Effects
canonicalize env values unions effects =
    case effects of
        Src.CASTS_NoEffects ->
            R.ok Can.NoEffects

        Src.CASTS_Ports ports ->
            let
                pairs : R.RResult i w Error.Error (List ( Name.CDN_Name, Can.Port ))
                pairs =
                    R.traverse (canonicalizePort env) ports
            in
            R.fmap (Can.Ports << Dict.fromList identity) pairs

        Src.CASTS_Manager region manager ->
            let
                dict : Dict String Name.CDN_Name A.CRA_Region
                dict =
                    Dict.fromList identity (List.map toNameRegion values)
            in
            R.ok Can.Manager
                |> R.apply (verifyManager region dict "init")
                |> R.apply (verifyManager region dict "onEffects")
                |> R.apply (verifyManager region dict "onSelfMsg")
                |> R.apply
                    (case manager of
                        Src.CASTS_Cmd cmdType ->
                            R.ok Can.Cmd
                                |> R.apply (verifyEffectType cmdType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "cmdMap"
                                            |> R.fmap (\_ -> result)
                                    )

                        Src.CASTS_Sub subType ->
                            R.ok Can.Sub
                                |> R.apply (verifyEffectType subType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "subMap"
                                            |> R.fmap (\_ -> result)
                                    )

                        Src.CASTS_Fx cmdType subType ->
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


canonicalizePort : Env.Env -> Src.CASTS_Port -> EResult i w ( Name.CDN_Name, Can.Port )
canonicalizePort env (Src.CASTS_Port (A.CRA_At region portName) tipe) =
    Type.toAnnotation env tipe
        |> R.bind
            (\(Can.CASTC_Forall freeVars ctipe) ->
                case List.reverse (Type.delambda (Type.deepDealias ctipe)) of
                    (Can.CASTC_TType home name [ msg ]) :: revArgs ->
                        if home == ModuleName.cmd && name == Name.cmd then
                            case revArgs of
                                [] ->
                                    R.throw (Error.PortTypeInvalid region portName Error.CmdNoArg)

                                [ outgoingType ] ->
                                    case msg of
                                        Can.CASTC_TVar _ ->
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
                                                    R.throw (Error.PortPayloadInvalid region portName badType err)

                                        _ ->
                                            R.throw (Error.PortTypeInvalid region portName Error.CmdBadMsg)

                                _ ->
                                    R.throw (Error.PortTypeInvalid region portName (Error.CmdExtraArgs (List.length revArgs)))

                        else if home == ModuleName.sub && name == Name.sub then
                            case revArgs of
                                [ Can.CASTC_TLambda incomingType (Can.CASTC_TVar msg1) ] ->
                                    case msg of
                                        Can.CASTC_TVar msg2 ->
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
                                                        R.throw (Error.PortPayloadInvalid region portName badType err)

                                            else
                                                R.throw (Error.PortTypeInvalid region portName Error.SubBad)

                                        _ ->
                                            R.throw (Error.PortTypeInvalid region portName Error.SubBad)

                                _ ->
                                    R.throw (Error.PortTypeInvalid region portName Error.SubBad)

                        else
                            R.throw (Error.PortTypeInvalid region portName Error.NotCmdOrSub)

                    _ ->
                        R.throw (Error.PortTypeInvalid region portName Error.NotCmdOrSub)
            )



-- VERIFY MANAGER


verifyEffectType : A.CRA_Located Name.CDN_Name -> Dict String Name.CDN_Name a -> EResult i w Name.CDN_Name
verifyEffectType (A.CRA_At region name) unions =
    if Dict.member identity name unions then
        R.ok name

    else
        R.throw (Error.EffectNotFound region name)


toNameRegion : A.CRA_Located Src.CASTS_Value -> ( Name.CDN_Name, A.CRA_Region )
toNameRegion (A.CRA_At _ (Src.CASTS_Value (A.CRA_At region name) _ _ _)) =
    ( name, region )


verifyManager : A.CRA_Region -> Dict String Name.CDN_Name A.CRA_Region -> Name.CDN_Name -> EResult i w A.CRA_Region
verifyManager tagRegion values name =
    case Dict.get identity name values of
        Just region ->
            R.ok region

        Nothing ->
            R.throw (Error.EffectFunctionNotFound tagRegion name)



-- CHECK PAYLOAD TYPES


checkPayload : Can.CASTC_Type -> Result ( Can.CASTC_Type, Error.InvalidPayload ) ()
checkPayload tipe =
    case tipe of
        Can.CASTC_TAlias _ _ args aliasedType ->
            checkPayload (Type.dealias args aliasedType)

        Can.CASTC_TType home name args ->
            case args of
                [] ->
                    if isJson home name || isString home name || isIntFloatBool home name then
                        Ok ()

                    else
                        Err ( tipe, Error.UnsupportedType name )

                [ arg ] ->
                    if isList home name || isMaybe home name || isArray home name then
                        checkPayload arg

                    else
                        Err ( tipe, Error.UnsupportedType name )

                _ ->
                    Err ( tipe, Error.UnsupportedType name )

        Can.CASTC_TUnit ->
            Ok ()

        Can.CASTC_TTuple a b maybeC ->
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

        Can.CASTC_TVar name ->
            Err ( tipe, Error.TypeVariable name )

        Can.CASTC_TLambda _ _ ->
            Err ( tipe, Error.Function )

        Can.CASTC_TRecord _ (Just _) ->
            Err ( tipe, Error.ExtendedRecord )

        Can.CASTC_TRecord fields Nothing ->
            Dict.foldl compare
                (\_ field acc -> Result.andThen (\_ -> checkFieldPayload field) acc)
                (Ok ())
                fields


checkFieldPayload : Can.CASTC_FieldType -> Result ( Can.CASTC_Type, Error.InvalidPayload ) ()
checkFieldPayload (Can.CASTC_FieldType _ tipe) =
    checkPayload tipe


isIntFloatBool : IO.CEMN_Canonical -> Name.CDN_Name -> Bool
isIntFloatBool home name =
    home
        == ModuleName.basics
        && (name == Name.int || name == Name.float || name == Name.bool)


isString : IO.CEMN_Canonical -> Name.CDN_Name -> Bool
isString home name =
    home
        == ModuleName.string
        && name
        == Name.string


isJson : IO.CEMN_Canonical -> Name.CDN_Name -> Bool
isJson home name =
    (home == ModuleName.jsonEncode)
        && (name == Name.value)


isList : IO.CEMN_Canonical -> Name.CDN_Name -> Bool
isList home name =
    home
        == ModuleName.list
        && name
        == Name.list


isMaybe : IO.CEMN_Canonical -> Name.CDN_Name -> Bool
isMaybe home name =
    home
        == ModuleName.maybe
        && name
        == Name.maybe


isArray : IO.CEMN_Canonical -> Name.CDN_Name -> Bool
isArray home name =
    home
        == ModuleName.array
        && name
        == Name.array
