module Internal.Harness
  ( tcpHarness
  ) where

-- A module that will provide a more coherent interface for the external
-- communication

import Internal.Comms
import Internal.Manager
  ( GameTree, GameID(..)
  , allocate, strip, dress
  )
import Internal.Start (invokeGame)

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map (empty, lookup, insert)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)

localHost = "127.0.0.1"
erlPort = "3637"

tcpHarness :: IO ()
tcpHarness = runTCPClient localHost erlPort (harnessLoop Map.empty)
  where
    harnessLoop :: GameTree -> Socket -> IO ()
    harnessLoop gameTree s = do
      srvrRequest <- C.unpack <$> recv s 1024
      case strip srvrRequest of
        Nothing -> do
          sendAll s $ C.pack "bad header"
          harnessLoop gameTree s
        (Just (gID, req)) -> handleRequest gameTree s gID req

    handleRequest :: GameTree -> Socket -> GameID -> String -> IO ()
    handleRequest gameTree s (GameID 0) req = do
      gameCh <- newChan
      mgrCh <- newChan
      let ch = Conn (gameCh, mgrCh)
      forkIO (invokeGame ch)
      let gID = allocate gameTree
          gameTree' = Map.insert gID ch gameTree
      handleRequest gameTree' s gID req
    handleRequest gameTree s gID req
      = case Map.lookup gID gameTree of
          Nothing -> do
            sendAll s $ C.pack "bad game id"
            harnessLoop gameTree s
          (Just ch) -> do
            writeChan (managerWrite ch) req
            nxtGameReq <- dress gID <$> readChan (managerRead ch)
            sendAll s $ C.pack nxtGameReq
            harnessLoop gameTree s


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
