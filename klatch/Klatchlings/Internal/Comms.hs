module Internal.Comms
  ( Comm
  , Conn(..)
  , gameWrite, gameRead
  , displayState
  , requestOrder
  , requestTargets
  ) where

import Base.Display (displayStateCallback)
import Base.Fields (Field(..), Owner(..))
import Base.GameState (GameState(..))
import Internal.Cmds
import Internal.External (portLog)
import Internal.Load (LoadInfo)
import Internal.Misc (reorder)
import Internal.Types
  ( Header(..)
  , CardID(..), AbilityID(..), TargetID(..)
  , Target(..), Create(..), Range
  )

import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.ByteString as B (ByteString, cons)
import qualified Data.Map as Map (Map, foldrWithKey)
import Data.Word (Word8)
import Text.Read (readMaybe)

newtype Conn = Conn
  { getChans :: (Chan B.ByteString, Chan B.ByteString)
  }

putOutput :: Chan B.ByteString -> Cmd -> String -> IO ()
putOutput ch (Cmd cmd) = writeChan ch . B.cons cmd . C.pack

getInput :: Chan B.ByteString -> IO String
getInput ch = C.unpack <$> readChan ch

gameWrite :: Conn -> Cmd -> String -> IO ()
gameWrite conn = putOutput (fst $ getChans conn)

gameRead :: Conn -> IO String
gameRead conn = getInput (snd $ getChans conn)

type Comm a = Conn -> a -> IO a

-- Wrapper function for the displayStateCallback function that will ensure
-- that both players have received the state before proceeding
displayState :: LoadInfo -> GameState -> Conn -> IO ()
displayState loadInfo gameState conn = do
  gameWrite conn display $ displayStateCallback loadInfo gameState ++ "{}"
  response <- gameRead conn
  when (response /= "{}{}{}") $ portLog ("not empty: " ++ response)
  when (response /= "{}{}{}") $ displayState loadInfo gameState conn

-- Once we have collected the various triggers we need to request the order
-- of them from the players
requestOrder :: Bool -> Comm [Header]
requestOrder _ _ [] = return []
requestOrder p1First conn hdrs = do
  let groupedHdrs = foldr groupHeaders ([], [], []) hdrs
      fmtHdrs = allMap closeBrackets
              . allMap (foldr appendCard "]")
              $ groupedHdrs
  gameWrite conn order . wrapMsgs $ fmtHdrs
  response <- gameRead conn
  case (=<<) (extractMap readMaybe)
       . extractMap unwrapMsg
       . splitMsg $ response of
    Nothing -> do
      portLog ("bad format: " ++ response)
      requestOrder p1First conn hdrs
    (Just orders) ->
      case orderAll p1First groupedHdrs orders of
        Nothing -> do
          portLog ("bad orders: " ++ response)
          requestOrder p1First conn hdrs
        (Just hdrs') -> return hdrs'
  where
    -- Group headers into their respective owners
    groupHeaders :: Header -> ([Header], [Header], [Header])
                    -> ([Header], [Header], [Header])
    groupHeaders hdr@(Unassigned owner _ _) = ownerMap owner (hdr :)
    groupHeaders hdr@(Assigned owner _ _ _) = ownerMap owner (hdr :)

    appendCard :: Header -> String -> String
    appendCard (Unassigned _ (CardID cID) (AbilityID aID))
      = (++) ("," ++ show cID ++ ":" ++ show aID)
    appendCard (Assigned _ (CardID cID) (AbilityID aID) _)
      = (++) ("," ++ show cID ++ ":" ++ show aID)

    orderAll :: Bool -> ([Header], [Header], [Header]) -> ([Int], [Int], [Int])
             -> Maybe [Header]
    orderAll p1First (p1Hdrs, p2Hdrs, sysHdrs) (p1Order, p2Order, sysOrder)
      | p1First = maybeJoin sysHdrs' . maybeJoin p1Hdrs' $ p2Hdrs'
      | not p1First = maybeJoin sysHdrs' . maybeJoin p2Hdrs' $ p1Hdrs'
      where
        p1Hdrs' = reorder p1Hdrs p1Order
        p2Hdrs' = reorder p2Hdrs p2Order
        sysHdrs' = reorder sysHdrs sysOrder

        maybeJoin :: Maybe [a] -> Maybe [a] -> Maybe [a]
        maybeJoin (Just x) (Just y) = Just $ x ++ y
        maybeJoin _ _ = Nothing


-- We need to get the targets of the various components of an ability
-- from the player that owns the card
requestTargets :: Comm Header
requestTargets conn (Assigned owner cID aID targets)
  = fmap (Targeted owner cID aID)
  . mapM (requestTarget conn owner)
  $ targets
  where
    requestTarget :: Conn -> Owner -> (TargetID, Target)
                  -> IO (TargetID, Create CardID)
    requestTarget conn owner (tID, target)
      = case target of
          Void -> return (tID, Create)
          (Given cID) -> return (tID, Existing cID)
          (Inquire range) -> doRequest owner tID range conn
          (Random range) -> doRequest System tID range conn

    doRequest :: Owner -> TargetID -> Range -> Conn
                  -> IO (TargetID, Create CardID)
    doRequest owner tID range conn = do
      let msg = wrapInOwner owner . show . map cardID $ range
      gameWrite conn target msg
      response <- gameRead conn
      case (=<<) readTarget
           . (=<<) stripEmpty
           . extractMap unwrapMsg
           . splitMsg $ response of
        Nothing -> do
          portLog ("bad format: " ++ response)
          doRequest owner tID range conn
        (Just (targeter, cID)) ->
          case ensureValid owner targeter range cID of
            True -> return (tID, Existing $ CardID cID)
            False -> do
              portLog ("bad trgt: " ++ response)
              doRequest owner tID range conn

    ensureValid :: Owner -> Owner -> Range -> Int -> Bool
    ensureValid owner targeter rng cID
      = owner == targeter && validTarget rng cID

    validTarget :: [CardID] -> Int -> Bool
    validTarget xs x = elem x $ map cardID xs

    readTarget :: (Owner, String) -> Maybe (Owner, Int)
    readTarget (o, str) = case readMaybe str of
                            Nothing -> Nothing
                            (Just x) -> Just (o, x)

-- Some helper functions for templating into the 'temporary' syntax that
-- the first {} denotes what the first player will receive, the second {} is
-- what the second player receives and the third {} is what is outputted to
-- the serverside game helper
wrapInOwner :: Owner -> String -> String
wrapInOwner P1 msg = "{" ++ msg ++ "}{}{}"
wrapInOwner P2 msg = "{}{" ++ msg ++ "}{}"
wrapInOwner System msg = "{}{}{" ++ msg ++ "}"

wrapMsg :: String -> String
wrapMsg str = "{" ++ str ++ "}"

wrapMsgs :: (String, String, String) -> String
wrapMsgs = collapse . allMap wrapMsg

unwrapMsg :: String -> Maybe String
unwrapMsg ['{'] = Nothing
unwrapMsg ('{' : rest)
  = if last == '}'
       then Just $ reverse str
       else Nothing
  where
    (last : str) = reverse rest
unwrapMsg _ = Nothing

splitMsg :: String -> (String, String, String)
splitMsg = undefined

stripEmpty :: (String, String, String) -> Maybe (Owner, String)
stripEmpty = undefined

ownerMap :: Owner -> (a -> a) -> (a, a, a) -> (a, a, a)
ownerMap owner f (x, y, z)
  | owner == P1 = (f x, y, z)
  | owner == P2 = (x, f y, z)
  | owner == System = (x, y, f z)

allMap :: (a -> b) -> (a, a, a) -> (b, b, b)
allMap f (x, y, z) = (f x, f y, f z)

closeBrackets :: String -> String
closeBrackets str = case tail str of
                   [] -> ""
                   str' -> '[' : str'

collapse :: ([a], [a], [a]) -> [a]
collapse (x, y, z) = x ++ y ++ z

extractJust :: (Maybe a, Maybe b, Maybe c) -> Maybe (a, b, c)
extractJust (Just x, Just y, Just z) = Just (x, y, z)
extractJust _ = Nothing

extractMap :: (a -> Maybe b) -> (a, a, a) -> Maybe (b, b, b)
extractMap f = extractJust . allMap f
