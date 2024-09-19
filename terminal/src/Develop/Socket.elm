module Develop.Socket exposing (watchFile)

import Control.Concurrent exposing (forkIO, threadDelay)
import Control.Exception exposing (SomeException, catch)
import Data.ByteString.Char8 as BS
import Data.IO exposing (IO)
import Network.WebSockets as WS
import System.FSNotify as Notify
import System.FSNotify.Devel as Notify


a =
    0



--watchFile : FilePath -> WS.PendingConnection -> IO ()
--watchFile watchedFile pendingConnection =
--  do  connection <- WS.acceptRequest pendingConnection
--
--      Notify.withManager <| \mgmt ->
--        do  stop <- Notify.treeExtAny mgmt "." ".elm" print
--            tend connection
--            stop
--
--
--tend : WS.Connection -> IO ()
--tend connection =
--  let
--    pinger : Integer -> IO a
--    pinger n =
--      do  threadDelay (5 * 1000 * 1000)
--          WS.sendPing connection (BS.pack (show n))
--          pinger (n + 1)
--
--    receiver : IO ()
--    receiver =
--      do  _ <- WS.receiveDataMessage connection
--          receiver
--
--    shutdown : SomeException -> IO ()
--    shutdown _ =
--      return ()
--  in
--    do  _pid <- forkIO (receiver `catch` shutdown)
--        pinger 1 `catch` shutdown
