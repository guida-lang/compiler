module Compiler.Type.Occurs exposing (occurs)

import Compiler.Type.Type as Type
import Compiler.Type.UnionFind as UF
import Data.IO as IO exposing (IO)
import Data.Map as Dict



-- OCCURS


occurs : Type.Variable -> IO Bool
occurs var =
    occursHelp [] var False


occursHelp : List Type.Variable -> Type.Variable -> Bool -> IO Bool
occursHelp seen var foundCycle =
    if List.member var seen then
        IO.pure True

    else
        UF.get Type.descriptorDecoder var
            |> IO.bind
                (\(Type.Descriptor content _ _ _) ->
                    case content of
                        Type.FlexVar _ ->
                            IO.pure foundCycle

                        Type.FlexSuper _ _ ->
                            IO.pure foundCycle

                        Type.RigidVar _ ->
                            IO.pure foundCycle

                        Type.RigidSuper _ _ ->
                            IO.pure foundCycle

                        Type.Structure term ->
                            let
                                newSeen : List Type.Variable
                                newSeen =
                                    var :: seen
                            in
                            case term of
                                Type.App1 _ _ args ->
                                    IO.foldrM (occursHelp newSeen) foundCycle args

                                Type.Fun1 a b ->
                                    IO.bind (occursHelp newSeen a)
                                        (occursHelp newSeen b foundCycle)

                                Type.EmptyRecord1 ->
                                    IO.pure foundCycle

                                Type.Record1 fields ext ->
                                    IO.bind (occursHelp newSeen ext) <|
                                        IO.foldrM (occursHelp newSeen) foundCycle (Dict.values fields)

                                Type.Unit1 ->
                                    IO.pure foundCycle

                                Type.Tuple1 a b maybeC ->
                                    case maybeC of
                                        Nothing ->
                                            IO.bind (occursHelp newSeen a)
                                                (occursHelp newSeen b foundCycle)

                                        Just c ->
                                            IO.bind (occursHelp newSeen a)
                                                (IO.bind (occursHelp newSeen b)
                                                    (occursHelp newSeen c foundCycle)
                                                )

                        Type.Alias _ _ args _ ->
                            IO.foldrM (occursHelp (var :: seen)) foundCycle (List.map Tuple.second args)

                        Type.Error ->
                            IO.pure foundCycle
                )
