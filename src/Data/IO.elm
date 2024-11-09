module Data.IO exposing
    ( CmdSpec(..)
    , CreateProcess
    , Effect(..)
    , ExitCode(..)
    , Handle(..)
    , IO(..)
    , IOMode(..)
    , IORef(..)
    , Process(..)
    , ProcessHandle
    , StateT(..)
    , StdStream(..)
    , apply
    , applyStateT
    , bind
    , bindStateT
    , evalStateT
    , exitFailure
    , exitWith
    , fmap
    , fmapStateT
    , foldrM
    , getLine
    , gets
    , hClose
    , hFileSize
    , hFlush
    , hIsTerminalDevice
    , hPutStr
    , hPutStrLn
    , ioRefCodec
    , ioRefDecoder
    , ioRefEncoder
    , liftIO
    , mVectorGrow
    , mVectorLength
    , mVectorModify
    , mVectorRead
    , mVectorReplicate
    , mVectorWrite
    , make
    , modify
    , modifyIORef
    , newIORef
    , procProc
    , procWaitForProcess
    , procWithCreateProcess
    , pure
    , pureStateT
    , putStr
    , readIORef
    , runStateT
    , stderr
    , stdout
    , vectorForM_
    , vectorImapM_
    , vectorUnsafeFreeze
    , vectorUnsafeInit
    , vectorUnsafeLast
    , withFile
    , writeIORef
    )

import Array exposing (Array)
import Array.Extra as Array
import Json.Decode as Decode
import Json.Encode as Encode
import Serialize exposing (Codec)


make : Decode.Decoder a -> Effect -> IO a
make resultDecoder effect =
    IO
        (\next ->
            Decode.succeed
                ( Process (Decode.lazy (\_ -> Decode.andThen next resultDecoder))
                , effect
                , Nothing
                )
        )



-- EFFECT


type Effect
    = Exit String Int
    | NewIORef Encode.Value
    | ReadIORef Int
    | VectorUnsafeLast Encode.Value
    | MVectorRead Int Encode.Value
    | WriteIORef Int Encode.Value
    | GetLine
    | HPutStr Handle String
    | Write String Encode.Value
    | WriteString String String
    | PutStrLn String
    | DirDoesFileExist String
    | DirFindExecutable String
    | DirCreateDirectoryIfMissing Bool String
    | DirRemoveFile String
    | DirRemoveDirectoryRecursive String
    | DirDoesDirectoryExist String
    | EnvLookupEnv String
    | EnvGetProgName
    | EnvGetArgs
    | BinaryDecodeFileOrFail String
    | Read String
    | HttpFetch String String (List ( String, String ))
    | HttpUpload String (List ( String, String )) Encode.Value
    | DirGetAppUserDataDirectory String
    | DirGetCurrentDirectory
    | DirGetModificationTime String
    | DirCanonicalizePath String
    | DirWithCurrentDirectory String
    | GetArchive String String
    | LockFile String
    | UnlockFile String
    | NewEmptyMVar
    | ReadMVar Int
    | TakeMVar Int
    | PutMVar Int Encode.Value
    | ReplGetInputLine String
    | ReplGetInputLineWithInitial String ( String, String )
    | HClose Handle
    | HFileSize Handle
    | HFlush Handle
    | WithFile String IOMode
    | StatePut Encode.Value
    | StateGet
    | ProcWithCreateProcess CreateProcess
    | ProcWaitForProcess Int
    | NoOp



-- System.Process


type CmdSpec
    = RawCommand String (List String)


type alias CreateProcess =
    { cmdspec : CmdSpec
    , std_in : StdStream
    , std_out : StdStream
    , std_err : StdStream
    }


type StdStream
    = Inherit
    | UseHandle Handle
    | CreatePipe
    | NoStream


type ProcessHandle
    = ProcessHandle Int


procProc : String -> List String -> CreateProcess
procProc cmd args =
    { cmdspec = RawCommand cmd args
    , std_in = Inherit
    , std_out = Inherit
    , std_err = Inherit
    }


procWithCreateProcess : CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO ExitCode) -> IO ExitCode
procWithCreateProcess createProcess f =
    make
        (Decode.map2 Tuple.pair
            (Decode.maybe (Decode.field "stdin" Decode.int))
            (Decode.field "ph" Decode.int)
        )
        (ProcWithCreateProcess createProcess)
        |> bind
            (\( stdinHandle, ph ) ->
                f (Maybe.map Handle stdinHandle) Nothing Nothing (ProcessHandle ph)
            )


procWaitForProcess : ProcessHandle -> IO ExitCode
procWaitForProcess (ProcessHandle ph) =
    make Decode.int (ProcWaitForProcess ph)
        |> fmap
            (\exitCode ->
                case exitCode of
                    0 ->
                        ExitSuccess

                    int ->
                        ExitFailure int
            )



-- IO


type IO a
    = IO ((a -> Decode.Decoder ( Process, Effect, Maybe (IO ()) )) -> Decode.Decoder ( Process, Effect, Maybe (IO ()) ))


type Process
    = Process (Decode.Decoder ( Process, Effect, Maybe (IO ()) ))


type IORef a
    = IORef Int


ioRefEncoder : IORef a -> Encode.Value
ioRefEncoder (IORef value) =
    Encode.int value


ioRefDecoder : Decode.Decoder (IORef a)
ioRefDecoder =
    Decode.map IORef Decode.int


ioRefCodec : Codec e (IORef a)
ioRefCodec =
    Serialize.int |> Serialize.map IORef (\(IORef value) -> value)


newIORef : Codec e a -> a -> IO (IORef a)
newIORef codec value =
    make (Decode.map IORef Decode.int) (NewIORef (Serialize.encodeToJson codec value))


readIORef : Codec e a -> IORef a -> IO a
readIORef codec (IORef ref) =
    make (Serialize.getJsonDecoder (\_ -> "failure on readIORef...") codec) (ReadIORef ref)


writeIORef : Codec e b -> IORef a -> b -> IO ()
writeIORef codec (IORef ref) value =
    make (Decode.succeed ()) (WriteIORef ref (Serialize.encodeToJson codec value))


modifyIORef : Codec e a -> IORef a -> (a -> a) -> IO ()
modifyIORef codec ioRef func =
    readIORef codec ioRef
        |> bind (\value -> writeIORef codec ioRef (func value))


pure : a -> IO a
pure a =
    make (Decode.succeed a) NoOp


apply : IO a -> IO (a -> b) -> IO b
apply (IO ioArg) (IO ioFunc) =
    IO
        (\next ->
            ioArg
                (\a ->
                    ioFunc
                        (\aToB ->
                            next (aToB a)
                        )
                )
        )


fmap : (a -> b) -> IO a -> IO b
fmap fn (IO a) =
    IO (\k -> a (k << fn))


bind : (a -> IO b) -> IO a -> IO b
bind cont (IO fn) =
    IO
        (\next ->
            fn
                (\a ->
                    let
                        (IO cont2) =
                            cont a
                    in
                    cont2 next
                )
        )


foldrM : (a -> b -> IO b) -> b -> List a -> IO b
foldrM f z0 xs =
    let
        c : a -> (b -> IO c) -> b -> IO c
        c x k z =
            bind k (f x z)
    in
    List.foldl c pure xs z0


mVectorReplicate : Codec e a -> Int -> a -> IO (IORef (Array (Maybe a)))
mVectorReplicate codec n e =
    newIORef (Serialize.array (Serialize.maybe codec)) (Array.repeat n (Just e))


mVectorLength : Codec e a -> IORef (Array (Maybe a)) -> IO Int
mVectorLength codec =
    readIORef (Serialize.array (Serialize.maybe codec))
        >> fmap Array.length


mVectorGrow : Codec e a -> IORef (Array (Maybe a)) -> Int -> IO (IORef (Array (Maybe a)))
mVectorGrow codec ioRef length =
    readIORef (Serialize.array (Serialize.maybe codec)) ioRef
        |> bind
            (\value ->
                writeIORef (Serialize.array (Serialize.maybe codec))
                    ioRef
                    (Array.append value (Array.repeat length Nothing))
            )
        |> fmap (\_ -> ioRef)


mVectorWrite : Codec e a -> IORef (Array (Maybe a)) -> Int -> a -> IO ()
mVectorWrite codec ioRef i x =
    modifyIORef (Serialize.array (Serialize.maybe codec))
        ioRef
        (Array.set i (Just x))


mVectorRead : Codec e a -> IORef (Array (Maybe a)) -> Int -> IO a
mVectorRead codec ioRef i =
    let
        arrayCodec =
            Serialize.array (Serialize.maybe codec)
    in
    readIORef arrayCodec ioRef
        |> bind
            (\vector ->
                make (Serialize.getJsonDecoder (\_ -> "failure on mVectorRead") codec)
                    (MVectorRead i (Serialize.encodeToJson arrayCodec vector))
            )


vectorImapM_ : Codec e a -> (Int -> a -> IO b) -> IORef (Array (Maybe a)) -> IO ()
vectorImapM_ codec action ioRef =
    readIORef (Serialize.array (Serialize.maybe codec)) ioRef
        |> bind
            (\value ->
                Array.foldl
                    (\( i, maybeX ) ioAcc ->
                        case maybeX of
                            Just x ->
                                bind
                                    (\acc ->
                                        fmap (\newX -> Array.push (Just newX) acc)
                                            (action i x)
                                    )
                                    ioAcc

                            Nothing ->
                                ioAcc
                    )
                    (pure Array.empty)
                    (Array.indexedMap Tuple.pair value)
                    |> fmap (\_ -> ())
            )


vectorMapM_ : Codec e a -> (a -> IO b) -> IORef (Array (Maybe a)) -> IO ()
vectorMapM_ codec action ioRef =
    vectorImapM_ codec (\_ -> action) ioRef


vectorForM_ : Codec e a -> IORef (Array (Maybe a)) -> (a -> IO b) -> IO ()
vectorForM_ codec ioRef action =
    vectorMapM_ codec action ioRef


vectorUnsafeInit : IORef (Array (Maybe a)) -> IORef (Array (Maybe a))
vectorUnsafeInit =
    identity


mVectorModify : Codec e a -> IORef (Array (Maybe a)) -> (a -> a) -> Int -> IO ()
mVectorModify codec ioRef func index =
    modifyIORef (Serialize.array (Serialize.maybe codec)) ioRef (Array.update index (Maybe.map func))


vectorUnsafeLast : Codec e a -> IORef (Array (Maybe a)) -> IO a
vectorUnsafeLast codec ioRef =
    let
        arrayCodec =
            Serialize.array (Serialize.maybe codec)
    in
    readIORef arrayCodec ioRef
        |> bind
            (\value ->
                make (Serialize.getJsonDecoder (\_ -> "failure on vectorUnsafeLast") codec)
                    (VectorUnsafeLast (Serialize.encodeToJson arrayCodec value))
            )


vectorUnsafeFreeze : IORef (Array (Maybe a)) -> IO (IORef (Array (Maybe a)))
vectorUnsafeFreeze =
    pure



-- StateT


{-| newtype StateT s m a

A state transformer monad parameterized by:

s - The state.
m - The inner monad. (== IO)

The return function leaves the state unchanged, while >>= uses the final state of the first computation as the initial state of the second.

Ref: <https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Monad-Trans-State-Lazy.html#t:StateT>

-}
type StateT s a
    = StateT (s -> IO ( a, s ))


runStateT : StateT s a -> s -> IO ( a, s )
runStateT (StateT f) =
    f


evalStateT : StateT s a -> s -> IO a
evalStateT (StateT f) =
    f >> fmap Tuple.first


liftIO : IO a -> StateT s a
liftIO io =
    StateT (\s -> fmap (\a -> ( a, s )) io)


applyStateT : StateT s a -> StateT s (a -> b) -> StateT s b
applyStateT (StateT arg) (StateT func) =
    StateT
        (\s ->
            arg s
                |> bind
                    (\( a, sa ) ->
                        func sa
                            |> fmap (\( fb, sb ) -> ( fb a, sb ))
                    )
        )


fmapStateT : (a -> b) -> StateT s a -> StateT s b
fmapStateT func argStateT =
    applyStateT argStateT (pureStateT func)


bindStateT : (a -> StateT s b) -> StateT s a -> StateT s b
bindStateT func (StateT arg) =
    StateT
        (\s ->
            arg s
                |> bind
                    (\( a, sa ) ->
                        case func a of
                            StateT fb ->
                                fb sa
                    )
        )


pureStateT : a -> StateT s a
pureStateT value =
    StateT (\s -> pure ( value, s ))


gets : (s -> a) -> StateT s a
gets f =
    StateT (\s -> pure ( f s, s ))


modify : (s -> s) -> StateT s ()
modify f =
    StateT (\s -> pure ( (), f s ))



-- Handles


type Handle
    = Handle Int


stdout : Handle
stdout =
    Handle 1


stderr : Handle
stderr =
    Handle 2


hFlush : Handle -> IO ()
hFlush handle =
    make (Decode.succeed ()) (HFlush handle)


hFileSize : Handle -> IO Int
hFileSize handle =
    make Decode.int (HFileSize handle)


hClose : Handle -> IO ()
hClose handle =
    make (Decode.succeed ()) (HClose handle)


getLine : IO String
getLine =
    make Decode.string GetLine


putStr : String -> IO ()
putStr =
    hPutStr stdout


hPutStr : Handle -> String -> IO ()
hPutStr handle str =
    make (Decode.succeed ()) (HPutStr handle str)


hPutStrLn : Handle -> String -> IO ()
hPutStrLn handle str =
    hPutStr handle (str ++ "\n")


hIsTerminalDevice : Handle -> IO Bool
hIsTerminalDevice _ =
    pure True



-- IOMode


type IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode



-- ExitCode


type ExitCode
    = ExitSuccess
    | ExitFailure Int


exitWith : ExitCode -> IO a
exitWith exitCode =
    IO
        (\_ ->
            let
                code : Int
                code =
                    case exitCode of
                        ExitSuccess ->
                            0

                        ExitFailure int ->
                            int
            in
            Decode.fail (Encode.encode 0 (Encode.int code))
        )


exitFailure : IO a
exitFailure =
    exitWith (ExitFailure 1)


withFile : String -> IOMode -> (Handle -> IO a) -> IO a
withFile path mode callback =
    make (Decode.map Handle Decode.int) (WithFile path mode)
        |> bind callback
