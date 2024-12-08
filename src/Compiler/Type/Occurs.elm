module Compiler.Type.Occurs exposing (occurs)

import Compiler.Type.UnionFind as UF
import Data.Map as Dict
import System.TypeCheck.IO as IO exposing (IO)



-- OCCURS


occurs : IO.Variable -> IO Bool
occurs var =
    occursHelp [] var False


occursHelp : List IO.Variable -> IO.Variable -> Bool -> IO Bool
occursHelp seen var foundCycle =
    if List.member var seen then
        IO.pure True

    else
        UF.get var
            |> IO.bind
                (\(IO.Descriptor content _ _ _) ->
                    case content of
                        IO.FlexVar _ ->
                            IO.pure foundCycle

                        IO.FlexSuper _ _ ->
                            IO.pure foundCycle

                        IO.RigidVar _ ->
                            IO.pure foundCycle

                        IO.RigidSuper _ _ ->
                            IO.pure foundCycle

                        IO.Structure term ->
                            let
                                newSeen : List IO.Variable
                                newSeen =
                                    var :: seen
                            in
                            case term of
                                IO.App1 _ _ args ->
                                    IO.foldrM (occursHelp newSeen) foundCycle args

                                IO.Fun1 a b ->
                                    IO.bind (occursHelp newSeen a)
                                        (occursHelp newSeen b foundCycle)

                                IO.EmptyRecord1 ->
                                    IO.pure foundCycle

                                IO.Record1 fields ext ->
                                    IO.bind (occursHelp newSeen ext) <|
                                        IO.foldrM (occursHelp newSeen) foundCycle (Dict.values fields)

                                IO.Unit1 ->
                                    IO.pure foundCycle

                                IO.Tuple1 a b maybeC ->
                                    case maybeC of
                                        Nothing ->
                                            IO.bind (occursHelp newSeen a)
                                                (occursHelp newSeen b foundCycle)

                                        Just c ->
                                            IO.bind (occursHelp newSeen a)
                                                (IO.bind (occursHelp newSeen b)
                                                    (occursHelp newSeen c foundCycle)
                                                )

                        IO.Alias _ _ args _ ->
                            IO.foldrM (occursHelp (var :: seen)) foundCycle (List.map Tuple.second args)

                        IO.Error ->
                            IO.pure foundCycle
                )
