module Internal.Comms
  ( Comm
  , Conn(..)
  , gameWrite, gameRead
  , managerWrite, managerRead
  , displayState
  , requestOrder
  , requestTargets
  ) where

import Base.Display (displayStateCallback)
import Base.Fields (Field(..), Owner(..))
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

newtype Conn = Conn
  { getChans :: (Chan String, Chan String)
  }

gameWrite :: Conn -> Chan String
gameWrite = fst . getChans

gameRead :: Conn -> Chan String
gameRead = snd . getChans

managerWrite :: Conn -> Chan String
managerWrite = snd . getChans

managerRead :: Conn -> Chan String
managerRead = fst . getChans

type Comm a = Conn -> a -> IO a

-- Wrapper function for the displayStateCallback function that will ensure
-- that both players have received the state before proceeding
displayState :: LoadInfo -> GameState -> Conn -> IO ()
displayState loadInfo gameState ch = do
  writeChan (gameWrite ch) (displayStateCallback loadInfo gameState ++ "{d}")
  response <- readChan (gameRead ch)
  case readMaybe response of
    Nothing -> displayState loadInfo gameState ch
    (Just 0) -> return ()
    (Just _) -> displayState loadInfo gameState ch


-- Once we have collected the various triggers we need to request the order
-- of them from the players
requestOrder :: Comm [Header]
requestOrder _ [] = return []
requestOrder ch hdrs = do
  let fmtHdrs = allMap closeBrackets
              . foldr formatOrderHeaders ("]", "]", "]")
              $ hdrs
  writeChan (gameWrite ch) . wrapMsgs $ fmtHdrs
  response <- readChan (gameRead ch)
  case reorder hdrs =<< readMaybe response of
    Nothing -> requestOrder ch hdrs
    (Just hdrs') -> return hdrs'
  where
    formatOrderHeaders :: Header -> (String, String, String)
                       -> (String, String, String)
    formatOrderHeaders (Unassigned owner cID aID)
      = ownerMap owner (appendCard cID aID)
    formatOrderHeaders (Assigned owner cID aID _)
      = ownerMap owner (appendCard cID aID)

    appendCard :: CardID -> AbilityID -> String -> String
    appendCard (CardID cID) (AbilityID aID)
      = (++) ("," ++ show cID ++ ":" ++ show aID)

-- We need to get the targets of the various components of an ability
-- from the player that owns the card
requestTargets :: Comm Header
requestTargets ch (Assigned owner cID aID targets)
  = fmap (Targeted owner cID aID)
  . mapM (requestTarget ch owner)
  $ targets
  where
    requestTarget :: Conn -> Owner -> (TargetID, Target)
                  -> IO (TargetID, Create CardID)
    requestTarget ch owner (tID, target)
      = case target of
          Void -> return (tID, Create)
          (Given cID) -> return (tID, Existing cID)
          (Inquire range) -> doRequest owner "i:" tID range ch
          (Random range) -> doRequest owner "r:" tID range ch

    doRequest :: Owner -> String -> TargetID -> Range -> Conn
                  -> IO (TargetID, Create CardID)
    doRequest owner requestType tID range ch = do
      let msg = (++) requestType . show . map cardID $ range
      writeChan (gameWrite ch) . wrapInOwner owner $ msg
      response <- readChan (gameRead ch)
      case validTarget range =<< readMaybe response of
        Nothing -> doRequest owner requestType tID range ch
        (Just cID) -> return (tID, Existing cID)

    validTarget :: [CardID] -> Int -> Maybe CardID
    validTarget xs x = if elem x . map cardID $ xs
                          then Just $ CardID x
                          else Nothing

-- Some helper functions for templating into the 'temporary' syntax that
-- the first {} denotes what the first player will receive, the second {} is
-- what the second player receives and the third {} is what is outputted to
-- the serverside game helper
wrapInOwner :: Owner -> String -> String
wrapInOwner P1 msg = "{" ++ msg ++ "}{}{}"
wrapInOwner P2 msg = "{}{" ++ msg ++ "}{}"
wrapInOwner System msg = "{}{}{" ++ msg ++ "}"

wrapMsgs :: (String, String, String) -> String
wrapMsgs (x, y, z) = "{" ++ x ++ "}{" ++ y ++ "}{" ++ z ++ "}"

ownerMap :: Owner -> (a -> a) -> (a, a, a) -> (a, a, a)
ownerMap owner f (x, y, z)
  | owner == P1 = (f x, y, z)
  | owner == P2 = (x, f y, z)
  | owner == System = (x, y, f z)

allMap :: (a -> a) -> (a, a, a) -> (a, a, a)
allMap f (x, y, z) = (f x, f y, f z)

closeBrackets :: String -> String
closeBrackets str = case tail str of
                   [] -> ""
                   str' -> '[' : str'
