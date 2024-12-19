module Compiler.Parse.Shader exposing (shader)

import Compiler.AST.Utils.Shader as Shader
import Compiler.Parse.Primitives as P exposing (Parser)
import Compiler.Reporting.Annotation as A
import Data.Map as Dict
import Language.GLSL.Parser as GLP
import Language.GLSL.Syntax as GLS
import Types as T
import Utils.Crash as Crash



-- SHADER


shader : T.CRA_Position -> Parser T.CRES_Expr T.CASTS_Expr
shader ((T.CRA_Position row col) as start) =
    parseBlock
        |> P.bind
            (\block ->
                parseGlsl row col block
                    |> P.bind
                        (\shdr ->
                            P.getPosition
                                |> P.fmap
                                    (\end ->
                                        A.at start end (T.CASTS_Shader (Shader.fromString block) shdr)
                                    )
                        )
            )



-- BLOCK


parseBlock : Parser T.CRES_Expr String
parseBlock =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            let
                pos6 : Int
                pos6 =
                    pos + 6
            in
            if
                (pos6 <= end)
                    && (P.unsafeIndex src pos == '[')
                    && (P.unsafeIndex src (pos + 1) == 'g')
                    && (P.unsafeIndex src (pos + 2) == 'l')
                    && (P.unsafeIndex src (pos + 3) == 's')
                    && (P.unsafeIndex src (pos + 4) == 'l')
                    && (P.unsafeIndex src (pos + 5) == '|')
            then
                let
                    ( ( status, newPos ), ( newRow, newCol ) ) =
                        eatShader src pos6 end row (col + 6)
                in
                case status of
                    Good ->
                        let
                            off : Int
                            off =
                                pos6

                            len : Int
                            len =
                                newPos - pos6

                            block : String
                            block =
                                String.left len (String.dropLeft off src)

                            newState : P.State
                            newState =
                                P.State src (newPos + 2) end indent newRow (newCol + 2)
                        in
                        Ok (P.POk P.Consumed block newState)

                    Unending ->
                        Err (P.PErr P.Consumed row col T.CRES_EndlessShader)

            else
                Err (P.PErr P.Empty row col T.CRES_Start)


type Status
    = Good
    | Unending


eatShader : String -> Int -> Int -> T.CPP_Row -> T.CPP_Col -> ( ( Status, Int ), ( T.CPP_Row, T.CPP_Col ) )
eatShader src pos end row col =
    if pos >= end then
        ( ( Unending, pos ), ( row, col ) )

    else
        let
            word : Char
            word =
                P.unsafeIndex src pos
        in
        if word == '|' && P.isWord src (pos + 1) end ']' then
            ( ( Good, pos ), ( row, col ) )

        else if word == '\n' then
            eatShader src (pos + 1) end (row + 1) 1

        else
            let
                newPos : Int
                newPos =
                    pos + P.getCharWidth word
            in
            eatShader src newPos end row (col + 1)



-- GLSL


parseGlsl : T.CPP_Row -> T.CPP_Col -> String -> Parser T.CRES_Expr T.CASTUS_Types
parseGlsl startRow startCol src =
    case GLP.parse src of
        Ok (GLS.TranslationUnit decls) ->
            P.pure (List.foldr addInput emptyTypes (List.concatMap extractInputs decls))

        Err { position, messages } ->
            -- FIXME this should be moved into guida-lang/glsl
            let
                lines : List String
                lines =
                    String.left position src
                        |> String.lines

                row : Int
                row =
                    List.length lines

                col : Int
                col =
                    case List.reverse lines of
                        lastLine :: _ ->
                            String.length lastLine

                        _ ->
                            0

                msg : String
                msg =
                    showErrorMessages messages
            in
            if row == 1 then
                failure startRow (startCol + 6 + col) msg

            else
                failure (startRow + row - 1) col msg


showErrorMessages : List String -> String
showErrorMessages msgs =
    if List.isEmpty msgs then
        "unknown parse error"

    else
        String.join "\n" msgs


failure : T.CPP_Row -> T.CPP_Col -> String -> Parser T.CRES_Expr a
failure row col msg =
    P.Parser <|
        \_ ->
            Err (P.PErr P.Consumed row col (T.CRES_ShaderProblem msg))



-- INPUTS


emptyTypes : T.CASTUS_Types
emptyTypes =
    T.CASTUS_Types Dict.empty Dict.empty Dict.empty


addInput : ( GLS.StorageQualifier, T.CASTUS_Type, String ) -> T.CASTUS_Types -> T.CASTUS_Types
addInput ( qual, tipe, name ) (T.CASTUS_Types attribute uniform varying) =
    case qual of
        GLS.Attribute ->
            T.CASTUS_Types (Dict.insert identity name tipe attribute) uniform varying

        GLS.Uniform ->
            T.CASTUS_Types attribute (Dict.insert identity name tipe uniform) varying

        GLS.Varying ->
            T.CASTUS_Types attribute uniform (Dict.insert identity name tipe varying)

        _ ->
            Crash.crash "Should never happen due to `extractInputs` function"


extractInputs : GLS.ExternalDeclaration -> List ( GLS.StorageQualifier, T.CASTUS_Type, String )
extractInputs decl =
    case decl of
        GLS.Declaration (GLS.InitDeclaration (GLS.TypeDeclarator (GLS.FullType (Just (GLS.TypeQualSto qual)) (GLS.TypeSpec _ (GLS.TypeSpecNoPrecision tipe _)))) [ GLS.InitDecl name _ _ ]) ->
            if List.member qual [ GLS.Attribute, GLS.Varying, GLS.Uniform ] then
                case tipe of
                    GLS.Vec2 ->
                        [ ( qual, T.CASTUS_V2, name ) ]

                    GLS.Vec3 ->
                        [ ( qual, T.CASTUS_V3, name ) ]

                    GLS.Vec4 ->
                        [ ( qual, T.CASTUS_V4, name ) ]

                    GLS.Mat4 ->
                        [ ( qual, T.CASTUS_M4, name ) ]

                    GLS.Int ->
                        [ ( qual, T.CASTUS_Int, name ) ]

                    GLS.Float ->
                        [ ( qual, T.CASTUS_Float, name ) ]

                    GLS.Sampler2D ->
                        [ ( qual, T.CASTUS_Texture, name ) ]

                    _ ->
                        []

            else
                []

        _ ->
            []
