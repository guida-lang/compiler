module Canonicalize.Effects exposing
    ( canonicalize
    , checkPayload
    )

import AST.Canonical as Can
import AST.Source as Src
import AST.Utils.Type as Type
import Canonicalize.Environment as Env
import Canonicalize.Type as Type
import Data.Map as Dict exposing (Dict)
import Data.Name as Name
import Elm.ModuleName as ModuleName
import Maybe exposing (Maybe(..))
import Reporting.Annotation as A
import Reporting.Error.Canonicalize as Error
import Reporting.Result as R



-- RESULT


type alias EResult i w a =
    R.RResult i w Error.Error a



-- CANONICALIZE


canonicalize :
    Env.Env
    -> List (A.Located Src.Value)
    -> Dict Name.Name union
    -> Src.Effects
    -> EResult i w Can.Effects
canonicalize env values unions effects =
    case effects of
        Src.NoEffects ->
            R.ok Can.NoEffects

        Src.Ports ports ->
            let
                pairs =
                    R.traverse (canonicalizePort env) ports
            in
            R.fmap (Can.Ports << Dict.fromList compare) pairs

        Src.Manager region manager ->
            let
                dict =
                    Dict.fromList compare (List.map toNameRegion values)
            in
            R.ok Can.Manager
                |> R.apply (verifyManager region dict "init")
                |> R.apply (verifyManager region dict "onEffects")
                |> R.apply (verifyManager region dict "onSelfMsg")
                |> R.apply
                    (case manager of
                        Src.Cmd cmdType ->
                            R.ok Can.Cmd
                                |> R.apply (verifyEffectType cmdType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "cmdMap"
                                            |> R.fmap (\_ -> result)
                                    )

                        Src.Sub subType ->
                            R.ok Can.Sub
                                |> R.apply (verifyEffectType subType unions)
                                |> R.bind
                                    (\result ->
                                        verifyManager region dict "subMap"
                                            |> R.fmap (\_ -> result)
                                    )

                        Src.Fx cmdType subType ->
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


canonicalizePort : Env.Env -> Src.Port -> EResult i w ( Name.Name, Can.Port )
canonicalizePort env (Src.Port (A.At region portName) tipe) =
    Type.toAnnotation env tipe
        |> R.bind
            (\(Can.Forall freeVars ctipe) ->
                case List.reverse (Type.delambda (Type.deepDealias ctipe)) of
                    (Can.TType home name [ msg ]) :: revArgs ->
                        if home == ModuleName.cmd && name == Name.cmd then
                            case revArgs of
                                [] ->
                                    R.throw (Error.PortTypeInvalid region portName Error.CmdNoArg)

                                [ outgoingType ] ->
                                    case msg of
                                        Can.TVar _ ->
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
                                [ Can.TLambda incomingType (Can.TVar msg1) ] ->
                                    case msg of
                                        Can.TVar msg2 ->
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


verifyEffectType : A.Located Name.Name -> Dict Name.Name a -> EResult i w Name.Name
verifyEffectType (A.At region name) unions =
    if Dict.member name unions then
        R.ok name

    else
        R.throw (Error.EffectNotFound region name)


toNameRegion : A.Located Src.Value -> ( Name.Name, A.Region )
toNameRegion (A.At _ (Src.Value (A.At region name) _ _ _)) =
    ( name, region )


verifyManager : A.Region -> Dict Name.Name A.Region -> Name.Name -> EResult i w A.Region
verifyManager tagRegion values name =
    case Dict.get name values of
        Just region ->
            R.ok region

        Nothing ->
            R.throw (Error.EffectFunctionNotFound tagRegion name)



-- CHECK PAYLOAD TYPES


checkPayload : Can.Type -> Result ( Can.Type, Error.InvalidPayload ) ()
checkPayload tipe =
    case tipe of
        Can.TAlias _ _ args aliasedType ->
            checkPayload (Type.dealias args aliasedType)

        Can.TType home name args ->
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

        Can.TUnit ->
            Ok ()

        Can.TTuple a b maybeC ->
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

        Can.TVar name ->
            Err ( tipe, Error.TypeVariable name )

        Can.TLambda _ _ ->
            Err ( tipe, Error.Function )

        Can.TRecord _ (Just _) ->
            Err ( tipe, Error.ExtendedRecord )

        Can.TRecord fields Nothing ->
            Dict.foldl (\_ field acc -> Result.andThen (\_ -> checkFieldPayload field) acc)
                (Ok ())
                fields


checkFieldPayload : Can.FieldType -> Result ( Can.Type, Error.InvalidPayload ) ()
checkFieldPayload (Can.FieldType _ tipe) =
    checkPayload tipe


isIntFloatBool : ModuleName.Canonical -> Name.Name -> Bool
isIntFloatBool home name =
    home
        == ModuleName.basics
        && (name == Name.int || name == Name.float || name == Name.bool)


isString : ModuleName.Canonical -> Name.Name -> Bool
isString home name =
    home
        == ModuleName.string
        && name
        == Name.string


isJson : ModuleName.Canonical -> Name.Name -> Bool
isJson home name =
    (home == ModuleName.jsonEncode)
        && (name == Name.value)


isList : ModuleName.Canonical -> Name.Name -> Bool
isList home name =
    home
        == ModuleName.list
        && name
        == Name.list


isMaybe : ModuleName.Canonical -> Name.Name -> Bool
isMaybe home name =
    home
        == ModuleName.maybe
        && name
        == Name.maybe


isArray : ModuleName.Canonical -> Name.Name -> Bool
isArray home name =
    home
        == ModuleName.array
        && name
        == Name.array
