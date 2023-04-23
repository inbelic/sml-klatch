module Internal.Comms
  ( Comm
  , displayState
  , requestOrder
  , requestTargets
  ) where

import Base.Display (displayStateCallback)
import Base.GameState (GameState(..))
import Internal.Load (LoadInfo)
import Internal.Misc (reorder)
import Internal.Types
  ( Header(..)
  , CardID(..), AbilityID(..), TargetID(..)
  , Target(..), Create(..), Range
  )

import Control.Concurrent.Chan (Chan, readChan, writeChan)
import qualified Data.Map as Map (Map, foldrWithKey)
import Text.Read (readMaybe)

type Comm a = Chan String -> a -> IO a

-- Wrapper function for the displayStateCallback function that will ensure
-- that both players have received the state before proceeding
displayState :: LoadInfo -> GameState -> Chan String -> IO ()
displayState loadInfo gameState ch = do
  writeChan ch $ displayStateCallback loadInfo gameState
  response <- readChan ch
  case readMaybe response of
    Nothing -> displayState loadInfo gameState ch
    (Just "ok") -> return ()
    (Just _) -> displayState loadInfo gameState ch


-- Once we have collected the various triggers we need to request the order
-- of them from the players
requestOrder :: Comm [Header]
requestOrder _ [] = return []
requestOrder ch hdrs = do
  writeChan ch . ("[" ++) . tail . foldr formatOrderHeaders "]" $ hdrs
  response <- readChan ch
  case reorder hdrs =<< readMaybe response of
    Nothing -> requestOrder ch hdrs
    (Just hdrs') -> return hdrs'
  where
    formatOrderHeaders :: Header -> String -> String
    formatOrderHeaders (Unassigned (CardID cID) (AbilityID aID))
      = (++) ("," ++ show cID ++ ":" ++ show aID)
    formatOrderHeaders (Assigned (CardID cID) (AbilityID aID) _)
      = (++) ("," ++ show cID ++ ":" ++ show aID)

-- We need to get the targets of the various components of an ability
-- from the player that owns the card
requestTargets :: Comm Header
requestTargets ch (Assigned cID aID targets)
  = fmap (Targeted cID aID)
  . mapM (requestTarget ch)
  $ targets
  where
    requestTarget :: Chan String -> (TargetID, Target)
                  -> IO (TargetID, Create CardID)
    requestTarget ch (tID, target)
      = case target of
          Void -> return (tID, Create)
          (Given cID) -> return (tID, Existing cID)
          (Inquire range) -> doRequest "i:" tID range ch
          (Random range) -> doRequest "r:" tID range ch

    doRequest :: String -> TargetID -> Range -> Chan String
                  -> IO (TargetID, Create CardID)
    doRequest requestType tID range ch = do
      writeChan ch . (++) requestType . show . map cardID $ range
      response <- readChan ch
      case validTarget range =<< readMaybe response of
        Nothing -> doRequest requestType tID range ch
        (Just cID) -> return (tID, Existing cID)

    validTarget :: [CardID] -> Int -> Maybe CardID
    validTarget xs x = if elem x . map cardID $ xs
                          then Just $ CardID x
                          else Nothing
