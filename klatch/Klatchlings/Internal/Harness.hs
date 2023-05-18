module Internal.Harness
  ( tcpHarness
  ) where

-- A module that will provide a more coherent interface for the external
-- communication

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)

localHost = "127.0.0.1"
erlPort = "3637"

tcpHarness :: Chan String -> IO ()
tcpHarness ch = runTCPClient localHost erlPort (harnessLoop ch)
  where harnessLoop ch s = do
          gameRequest <- readChan ch
          sendAll s $ C.pack gameRequest
          srvrResponse <- recv s 1024
          writeChan ch $ C.unpack srvrResponse
          harnessLoop ch s

-- taking from Network.Socket example
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
    where
      resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
      open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

-- Keep track of which game we want to respond/interact with
newtype GameID = GameID
  { gameID :: Int
  }
