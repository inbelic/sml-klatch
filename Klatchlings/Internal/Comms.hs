module Internal.Comms
  ( Comm
  , displayState
  , requestOrder
  , requestTargets
  ) where

-- TODO: Dummy module for the moment

import Base.Display (displayStateCallback)
import Base.GameState (GameState(..))
import Internal.Load (LoadInfo)
import Internal.Types (Header)

import Control.Concurrent.Chan (Chan, readChan, writeChan)
import qualified Data.Map as Map (Map, foldrWithKey)
import Text.Read (readMaybe)

type Comm a = Chan String -> a -> IO a

displayState :: LoadInfo -> GameState -> Chan String -> IO ()
displayState loadInfo gameState ch = do
  writeChan ch $ displayStateCallback loadInfo gameState
  response <- readChan ch
  case readMaybe response of
    Nothing -> displayState loadInfo gameState ch
    (Just "ok") -> return ()
    (Just _) -> displayState loadInfo gameState ch

requestOrder :: Comm [Header]
requestOrder = undefined

requestTargets :: Comm Header
requestTargets = undefined
