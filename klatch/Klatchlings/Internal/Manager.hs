module Internal.Manager where

import Control.Concurrent.Chan
import Internal.Comms (Conn)
import qualified Data.Map.Strict as Map (Map, keys)

import Text.Read (readMaybe)

import qualified Data.ByteString as B (ByteString, null, append)
import qualified Data.ByteString.Char8 as C (pack, uncons, cons)

type GameTree = Map.Map GameID Conn

-- Keep track of which game we want to respond/interact with
newtype GameID = GameID
  { gameID :: Integer
  }
  deriving (Eq, Ord)

strip :: B.ByteString -> Maybe (GameID, B.ByteString)
strip = strip' []
  where
    strip' :: String -> B.ByteString -> Maybe (GameID, B.ByteString)
    strip' acc bytes
      | B.null bytes = Nothing
      | otherwise = do
        (char, rest) <- C.uncons bytes
        case char of
          ':' -> fmap ((, rest) . GameID) . readMaybe . reverse $ acc
          digit -> strip' (digit : acc) rest

dress :: GameID -> B.ByteString -> B.ByteString
dress (GameID gID) = B.append gIDBytes . C.cons ':'
  where
    gIDBytes = C.pack $ show gID
