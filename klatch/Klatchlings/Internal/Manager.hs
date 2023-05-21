module Internal.Manager where

import Control.Concurrent.Chan
import Internal.Comms (Conn)
import qualified Data.Map.Strict as Map (Map, keys)

import Text.Read (readMaybe)

type GameTree = Map.Map GameID Conn

-- Keep track of which game we want to respond/interact with
newtype GameID = GameID
  { gameID :: Integer
  }
  deriving (Eq, Ord)

instance Show GameID where
  show (GameID gID) = show gID

strip :: String -> Maybe (GameID, String)
strip = strip' []
  where
    strip' :: String -> String -> Maybe (GameID, String)
    strip' acc [] = Nothing
    strip' acc (':': str)
      = fmap ((, str) . GameID)
      . readMaybe . reverse $ acc
    strip' acc (digit : str) = strip' (digit : acc) str

dress :: GameID -> String -> String
dress gID str = show gID ++ (':' : str)

allocate :: GameTree -> GameID
allocate = GameID .  (+ 1) . maximum' . map gameID .  Map.keys
  where
    maximum' :: [Integer] -> Integer
    maximum' [] = 1
    maximum' xs = maximum xs
