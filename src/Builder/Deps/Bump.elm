module Builder.Deps.Bump exposing (getPossibilities)

import Compiler.Elm.Magnitude as M
import Compiler.Elm.Version as V
import List.Extra
import Types as T
import Utils.Main as Utils



-- GET POSSIBILITIES


getPossibilities : T.BDR_KnownVersions -> List ( T.CEV_Version, T.CEV_Version, M.Magnitude )
getPossibilities (T.BDR_KnownVersions latest previous) =
    let
        allVersions : List T.CEV_Version
        allVersions =
            List.reverse (latest :: previous)

        minorPoints : List T.CEV_Version
        minorPoints =
            List.filterMap List.Extra.last (Utils.listGroupBy sameMajor allVersions)

        patchPoints : List T.CEV_Version
        patchPoints =
            List.filterMap List.Extra.last (Utils.listGroupBy sameMinor allVersions)
    in
    ( latest, V.bumpMajor latest, M.MAJOR )
        :: List.map (\v -> ( v, V.bumpMinor v, M.MINOR )) minorPoints
        ++ List.map (\v -> ( v, V.bumpPatch v, M.PATCH )) patchPoints


sameMajor : T.CEV_Version -> T.CEV_Version -> Bool
sameMajor (T.CEV_Version major1 _ _) (T.CEV_Version major2 _ _) =
    major1 == major2


sameMinor : T.CEV_Version -> T.CEV_Version -> Bool
sameMinor (T.CEV_Version major1 minor1 _) (T.CEV_Version major2 minor2 _) =
    major1 == major2 && minor1 == minor2
