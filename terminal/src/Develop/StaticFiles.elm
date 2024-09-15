module Develop.StaticFiles exposing
    ( cssPath
    , elmPath
    , lookup
    , waitingPath
    )

import Data.Map as Dict exposing (Dict)
import Develop.StaticFiles.Build as Build



-- FILE LOOKUP


type alias MimeType =
    String


lookup : String -> Maybe ( String, MimeType )
lookup path =
    Dict.get path dict


dict : Dict String ( String, MimeType )
dict =
    Dict.fromList
        [ ( faviconPath, ( favicon, "image/x-icon" ) )
        , ( elmPath, ( elm, "application/javascript" ) )
        , ( cssPath, ( css, "text/css" ) )
        , ( codeFontPath, ( codeFont, "font/ttf" ) )
        , ( sansFontPath, ( sansFont, "font/ttf" ) )
        ]



-- PATHS


faviconPath : String
faviconPath =
    "favicon.ico"


waitingPath : String
waitingPath =
    "_elm/waiting.gif"


elmPath : String
elmPath =
    "_elm/elm.js"


cssPath : String
cssPath =
    "_elm/styles.css"


codeFontPath : String
codeFontPath =
    "_elm/source-code-pro.ttf"


sansFontPath : String
sansFontPath =
    "_elm/source-sans-pro.ttf"



---- ELM
--
--
--elm : String
--elm =
--    bsToExp =<< runIO Build.buildReactorFrontEnd
--
--
--
---- CSS
--
--
--css : String
--css =
--    bsToExp =<< runIO (Build.readAsset "styles.css")
--
--
--
---- FONTS
--
--
--codeFont : String
--codeFont =
--    bsToExp =<< runIO (Build.readAsset "source-code-pro.ttf")
--
--
--sansFont : String
--sansFont =
--    bsToExp =<< runIO (Build.readAsset "source-sans-pro.ttf")
--
--
--
---- IMAGES
--
--
--favicon : String
--favicon =
--    bsToExp =<< runIO (Build.readAsset "favicon.ico")
