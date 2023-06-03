module Internal.Start
  ( invokeGame
  ) where

import Base.History
import Base.Rules

import Internal.Engine (resolveStack)
import Internal.External (portLog)
import Internal.Types
import Internal.Load (LoadInfo(..), basicLoadInfo)
import Internal.Comms

import Control.Concurrent.Chan
import Control.Monad (void)
import qualified Data.Map as Map (fromList)

-- TODO
startGame :: String -> IO (LoadInfo, Game)
startGame _ = return (basicLoadInfo, Game [] begin cards)
  where
    cards = Map.fromList [(ruleCardID, baseRules)]

runGame :: LoadInfo -> Conn -> Game -> IO ()
runGame loadInfo conn game = void $ resolveStack loadInfo conn game

invokeGame :: Conn -> IO ()
invokeGame conn = do
  startInfo <- gameRead conn
  (loadInfo, game) <- startGame startInfo
  portLog "new game"
  runGame loadInfo conn game
