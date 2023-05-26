module Internal.External
  ( portLog
  ) where

-- Module deals with some of the external requirements, such as saving the
-- GameTree state to disk during a gracefull close (TODO), or communicating
-- to supervising erlang process

import qualified Data.ByteString.Char8 as C
import Data.Char (chr)
import System.IO (hFlush, hPutStr, stdout)

import Control.Concurrent.Chan

-- The portlog allows us to send info message to the supervising erlang port
-- through the stdout using the {packet, 1} option in erlang. This means that
-- every message we send needs to be less than 128 bytes. Should we need to
-- extend this, then we need to update the erlang side accordingly to
-- {packet, N} as desired. However, for the standard logging that is expected,
-- a 128 bytes limit should be okay.
-- WARNING: If the limit is exceeded then we will
-- send consecutive messages in chunks of 128 bytes and the erlang side will
-- process them as each their own message

portLog :: String -> IO ()
portLog = portLog' . C.pack

portLog' :: C.ByteString -> IO ()
portLog' msg
  | size < 128 = send size msg
  | otherwise = send 127 msg' >> portLog' rest
  where
    size = C.length msg
    (msg', rest) = C.splitAt 127 msg

    send :: Int -> C.ByteString -> IO ()
    send size msg = do
        let size_byte = C.singleton $ chr size
        C.hPutStr stdout size_byte
        C.hPutStr stdout msg
        hFlush stdout
