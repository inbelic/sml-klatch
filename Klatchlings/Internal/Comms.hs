module Internal.Comms
  ( Comm
  , displayState
  , requestOrder
  , requestTargets
  ) where

-- TODO: Dummy module for the moment

import Control.Concurrent.Chan (Chan)
import Internal.Types (CompiledMasks, Header, GameState)

type Comm a = Chan String -> a -> IO a

displayState :: CompiledMasks -> GameState -> Chan String -> IO ()
displayState = undefined

requestOrder :: Comm [Header]
requestOrder = undefined

requestTargets :: Comm Header
requestTargets = undefined
