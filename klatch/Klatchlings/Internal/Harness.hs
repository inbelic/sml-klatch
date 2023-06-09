module Internal.Harness
  ( tcpHarness
  ) where

-- A module that will provide a more coherent interface for the external
-- communication

import Internal.Comms
import Internal.Manager
  ( GameTree, GameID(..)
  , strip, dress
  )
import Internal.External (portLog)
import Internal.Start (invokeGame)

import qualified Control.Exception as E

import qualified Data.ByteString.Char8 as C (unpack, pack)
import qualified Data.Map.Strict as Map (empty, lookup, insert)

import Network.Socket
  ( Socket, AddrInfo, HostName, ServiceName, SocketType(..)
  , defaultHints, addrSocketType, getAddrInfo, addrAddress
  , openSocket, connect, close
  , withSocketsDo
  )
import Network.Socket.ByteString (recv, sendAll)

import Control.Concurrent.Chan (newChan, readChan, writeChan)
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
          portLog "log: bad header"
          harnessLoop gameTree s
        (Just (gID, req)) -> handleRequest gameTree s gID req

    handleRequest :: GameTree -> Socket -> GameID -> String -> IO ()
    handleRequest gameTree s gID req
      = case Map.lookup gID gameTree of
          Nothing -> do
            gameCh <- newChan
            mgrCh <- newChan
            let ch = Conn (gameCh, mgrCh)
                gameTree' = Map.insert gID ch gameTree
            forkIO (invokeGame ch)
            portLog "new game"
            handleRequest gameTree' s gID req
          (Just ch) -> do
            writeChan (managerWrite ch) req
            nxtGameReq <- dress gID <$> readChan (managerRead ch)
            sendAll s $ C.pack nxtGameReq
            harnessLoop gameTree s

-- taking from Network.Socket example
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) onClose client
    where
      resolve :: IO AddrInfo
      resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)

      open :: AddrInfo -> IO Socket
      open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

      onClose :: Socket -> IO ()
      onClose sock = portLog "exit: closed" >> close sock
