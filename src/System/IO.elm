port module System.IO exposing
    ( Program, run
    , IO(..), ION(..), pure, apply, fmap, bind, foldrM
    , Handle
    , stdout, stderr
    , hFlush
    , hIsTerminalDevice
    , hPutStr, hPutStrLn
    , putStr, getLine
    )

{-| Ref.: <https://hackage.haskell.org/package/base-4.20.0.1/docs/System-IO.html>

@docs Program, run


# The IO monad

@docs IO, ION, pure, apply, fmap, bind, foldrM


# Files and handles

@docs Handle


# Standard handles

@docs stdout, stderr


# Buffering operations

@docs hFlush


# Terminal operations (not portable: GHC only)

@docs hIsTerminalDevice


# Text output

@docs hPutStr, hPutStrLn


# Special cases for standard input and output

@docs putStr, getLine

-}

import Array exposing (Array)
import Codec.Archive.Zip exposing (ZipArchive)
import Json.Encode as Encode


type alias Program =
    Platform.Program () Model Msg


run : IO () -> Program
run app =
    Platform.worker
        { init =
            \() ->
                update (PureMsg app)
                    { realWorld = { ioRefs = Array.empty }
                    , next = Nothing
                    }
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ recvGetLine GetLineMsg
                    , recvHPutStr (\() -> HPutLineMsg)
                    , recvWriteString (\() -> WriteStringMsg)
                    ]
        }


type alias Model =
    { realWorld : RealWorld
    , next : Maybe Next
    }


type Next
    = GetLineNext (String -> IO ())
    | HPutLineNext (() -> IO ())
    | WriteStringNext (() -> IO ())
    | ReadNext (String -> IO ())
    | HttpFetchNext (String -> IO ())
    | GetArchiveNext (( String, ZipArchive ) -> IO ())
    | HttpUploadNext (() -> IO ())
    | HFlushNext (() -> IO ())


type Msg
    = PureMsg (IO ())
    | GetLineMsg String
    | HPutLineMsg
    | WriteStringMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.next ) of
        ( PureMsg (IO fn), _ ) ->
            case fn model.realWorld of
                ( newRealWorld, Pure () ) ->
                    ( { model | realWorld = newRealWorld }, Cmd.none )

                ( newRealWorld, GetLine next ) ->
                    ( { model | realWorld = newRealWorld, next = Just (GetLineNext next) }, sendGetLine () )

                ( newRealWorld, HPutStr next (Handle fd) content ) ->
                    ( { model | realWorld = newRealWorld, next = Just (HPutLineNext next) }, sendHPutStr { fd = fd, content = content } )

                ( newRealWorld, WriteString next path content ) ->
                    ( { model | realWorld = newRealWorld, next = Just (WriteStringNext next) }, sendWriteString { path = path, content = content } )

                ( newRealWorld, Read next path ) ->
                    ( { model | realWorld = newRealWorld, next = Just (ReadNext next) }, sendRead path )

                ( newRealWorld, HttpFetch next method url headers ) ->
                    ( { model | realWorld = newRealWorld, next = Just (HttpFetchNext next) }, sendHttpFetch { method = method, url = url, headers = headers } )

                ( newRealWorld, GetArchive next method url ) ->
                    ( { model | realWorld = newRealWorld, next = Just (GetArchiveNext next) }, sendGetArchive { method = method, url = url } )

                ( newRealWorld, HttpUpload next url headers parts ) ->
                    ( { model | realWorld = newRealWorld, next = Just (HttpUploadNext next) }, sendHttpUpload { url = url, headers = headers, parts = parts } )

                ( newRealWorld, HFlush next (Handle fd) ) ->
                    ( { model | realWorld = newRealWorld, next = Just (HFlushNext next) }, sendHFlush fd )

        ( GetLineMsg input, Just (GetLineNext fn) ) ->
            update (PureMsg (fn input)) model

        ( HPutLineMsg, Just (HPutLineNext fn) ) ->
            update (PureMsg (fn ())) model

        ( WriteStringMsg, Just (WriteStringNext fn) ) ->
            update (PureMsg (fn ())) model

        _ ->
            ( model, Cmd.none )


port sendGetLine : () -> Cmd msg


port recvGetLine : (String -> msg) -> Sub msg


port sendHPutStr : { fd : Int, content : String } -> Cmd msg


port recvHPutStr : (() -> msg) -> Sub msg


port sendWriteString : { path : FilePath, content : String } -> Cmd msg


port recvWriteString : (() -> msg) -> Sub msg


port sendRead : String -> Cmd msg


port recvRead : (String -> msg) -> Sub msg


port sendHttpFetch : { method : String, url : String, headers : List ( String, String ) } -> Cmd msg


port recvHttpFetch : (String -> msg) -> Sub msg


port sendGetArchive : { method : String, url : String } -> Cmd msg


port recvGetArchive : (( String, ZipArchive ) -> msg) -> Sub msg


port sendHttpUpload : { url : String, headers : List ( String, String ), parts : List Encode.Value } -> Cmd msg


port recvHttpUpload : (() -> msg) -> Sub msg


port sendHFlush : Int -> Cmd msg


port recvHFlush : (() -> msg) -> Sub msg



-- The IO monad


type IO a
    = IO (RealWorld -> ( RealWorld, ION a ))


type ION a
    = Pure a
    | HPutStr (() -> IO a) Handle String
    | GetLine (String -> IO a)
    | WriteString (() -> IO a) FilePath String
    | Read (String -> IO a) FilePath
    | HttpFetch (String -> IO a) String String (List ( String, String ))
    | GetArchive (( String, ZipArchive ) -> IO a) String String
    | HttpUpload (() -> IO a) String (List ( String, String )) (List Encode.Value)
    | HFlush (() -> IO a) Handle


type alias RealWorld =
    { ioRefs : Array Encode.Value
    }


pure : a -> IO a
pure x =
    IO (\s -> ( s, Pure x ))


apply : IO a -> IO (a -> b) -> IO b
apply ma mf =
    bind (\f -> bind (\a -> pure (f a)) ma) mf


fmap : (a -> b) -> IO a -> IO b
fmap fn ma =
    bind (\a -> pure (fn a)) ma


bind : (a -> IO b) -> IO a -> IO b
bind f (IO ma) =
    IO
        (\s0 ->
            case ma s0 of
                ( s1, Pure a ) ->
                    unIO (f a) s1

                ( s1, GetLine next ) ->
                    ( s1, GetLine (\input -> bind f (next input)) )

                ( s1, HPutStr next handle content ) ->
                    ( s1, HPutStr (\() -> bind f (next ())) handle content )

                ( s1, WriteString next path content ) ->
                    ( s1, WriteString (\() -> bind f (next ())) path content )

                ( s1, Read next path ) ->
                    ( s1, Read (\input -> bind f (next input)) path )

                ( s1, HttpFetch next method url headers ) ->
                    ( s1, HttpFetch (\body -> bind f (next body)) method url headers )

                ( s1, GetArchive next method url ) ->
                    ( s1, GetArchive (\body -> bind f (next body)) method url )

                ( s1, HttpUpload next url headers parts ) ->
                    ( s1, HttpUpload (\() -> bind f (next ())) url headers parts )

                ( s1, HFlush next handle ) ->
                    ( s1, HFlush (\() -> bind f (next ())) handle )
        )


foldrM : (a -> b -> IO b) -> b -> List a -> IO b
foldrM f z0 xs =
    let
        c : a -> (b -> IO c) -> b -> IO c
        c x k z =
            bind k (f x z)
    in
    List.foldl c pure xs z0


unIO : IO a -> (RealWorld -> ( RealWorld, ION a ))
unIO (IO a) =
    a



-- Files and handles


type alias FilePath =
    String


type Handle
    = Handle Int



-- Standard handles


stdout : Handle
stdout =
    Handle 1


stderr : Handle
stderr =
    Handle 2



-- Buffering operations


hFlush : Handle -> IO ()
hFlush handle =
    IO (\s -> ( s, HFlush pure handle ))



-- Terminal operations (not portable: GHC only)


hIsTerminalDevice : Handle -> IO Bool
hIsTerminalDevice _ =
    pure True



-- Text output


hPutStr : Handle -> String -> IO ()
hPutStr handle content =
    IO (\s -> ( s, HPutStr pure handle content ))


hPutStrLn : Handle -> String -> IO ()
hPutStrLn handle content =
    hPutStr handle (content ++ "\n")



-- Special cases for standard input and output


putStr : String -> IO ()
putStr =
    hPutStr stdout


getLine : IO String
getLine =
    IO (\s -> ( s, GetLine pure ))
