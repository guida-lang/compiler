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
import Compiler.Parse.SyntaxVersion exposing (SyntaxVersion)
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
    SyntaxVersion
    -> Env.Env
    -> List (A.Located Src.Value)
    -> Dict String Name.Name union
    -> Src.Effects
    -> EResult i w Can.Effects
canonicalize syntaxVersion env values unions effects =
    case effects of
        Src.NoEffects ->
            R.ok Can.NoEffects

        Src.Ports ports ->
            R.traverse (canonicalizePort syntaxVersion env) ports
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


canonicalizePort : SyntaxVersion -> Env.Env -> Src.Port -> EResult i w ( Name.Name, Can.Port )
canonicalizePort syntaxVersion env (Src.Port _ ( _, A.At region portName ) tipe) =
    Type.toAnnotation syntaxVersion env tipe
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

        Can.TTuple a b cs ->
            checkPayload a
                |> Result.andThen (\_ -> checkPayload b)
                |> Result.andThen (\_ -> checkPayloadTupleCs cs)

        Can.TVar name ->
            Err ( tipe, Error.TypeVariable name )

        Can.TLambda _ _ ->
            Err ( tipe, Error.Function )

        Can.TRecord _ (Just _) ->
            Err ( tipe, Error.ExtendedRecord )

        Can.TRecord fields Nothing ->
            Dict.foldl compare
                (\_ field acc -> Result.andThen (\_ -> checkFieldPayload field) acc)
                (Ok ())
                fields


checkPayloadTupleCs : List Can.Type -> Result ( Can.Type, Error.InvalidPayload ) ()
checkPayloadTupleCs types =
    case types of
        [] ->
            Ok ()

        tipe :: rest ->
            checkPayload tipe
                |> Result.andThen (\_ -> checkPayloadTupleCs rest)


checkFieldPayload : Can.FieldType -> Result ( Can.Type, Error.InvalidPayload ) ()
checkFieldPayload (Can.FieldType _ tipe) =
    checkPayload tipe


isIntFloatBool : IO.Canonical -> Name.Name -> Bool
isIntFloatBool home name =
    home
        == ModuleName.basics
        && (name == Name.int || name == Name.float || name == Name.bool)


isString : IO.Canonical -> Name.Name -> Bool
isString home name =
    home
        == ModuleName.string
        && name
        == Name.string


isJson : IO.Canonical -> Name.Name -> Bool
isJson home name =
    (home == ModuleName.jsonEncode)
        && (name == Name.value)


isList : IO.Canonical -> Name.Name -> Bool
isList home name =
    home
        == ModuleName.list
        && name
        == Name.list


isMaybe : IO.Canonical -> Name.Name -> Bool
isMaybe home name =
    home
        == ModuleName.maybe
        && name
        == Name.maybe


isArray : IO.Canonical -> Name.Name -> Bool
isArray home name =
    home
        == ModuleName.array
        && name
        == Name.array
