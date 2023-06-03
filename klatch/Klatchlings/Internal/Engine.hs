module Internal.Engine where

import Base.Card (collectHeaders, lookupAbility, create, mint)
import Base.CardState (check)
import Base.Fields (Field(..), Owner(..))
import Base.GameState (Game(..), GameState(..), peek)
import Base.History (History, current, write, record)
import Internal.Comms (Comm, displayState, requestOrder, requestTargets)
import Internal.Misc (getNextKey)
import Internal.Types
import Internal.Load (LoadInfo, compiledWindows)

import Control.Monad ((<=<))
import qualified Data.Map as Map (Map, lookup, insert)

-- The Grand Engine:
--
-- As enjoyable as it would be to just leave this hunk of compact,
-- condense, robust and beatiful Haskell code undocumented and then imply
-- that it is the readers lack of intelligence if they don't understand...
-- I will add some artwork and hopefully helpful description of how this
-- is working. Let us visualize how the functions relate to each other
--
--
--  Overall function flow:
--  --------------------------      ----- Entry-Point
-- |                         |     |
-- |                        \/    \/
-- |                   ----------------   if history wasn't empty
-- |        --------- | ResolveStack  | <--------------------------
-- |       |          ----------------                           /
-- |      \/                                                    /
-- |   -----------------        if the stack is empty          /
-- |  | ResolveTrigger | -------------------------------------/
-- |  -----------------                                      /
-- |        /   /\ target Hdr and put front of stack        -------------> done
-- |  Get  /    |--------------------------------             otherwise
-- |  Hdr /                                    /
-- |     /         Hdr is Unassigned   ---------------------
-- |    ----------------------------> / ResolveUnassigned /
-- |             /                   ---------------------
-- |            / Hdr is Targeted     -------------------
-- |           /-------------------> / ResolveTargeted /
-- |                                 ------------------
-- |                                       /
-- ---------------------------------------/

resolveStack :: LoadInfo -> Comm Game
resolveStack loadInfo ch game@(Game stck hist crds) = do
  -- Evaluate our GameState and collect the trigger headers
  let gameState = peek game . compiledWindows $ loadInfo
      hdrs = collectHeaders gameState crds
      p1First = determineFirstPlayer gameState
  -- Output the current gamestate to viewers if triggers need to be resolved
  if null hdrs
     then return ()
     else displayState loadInfo gameState ch
  -- Then request the order and targets of applicable headers
  hdrs' <- mapM (requestTargets ch)
        <=< requestOrder p1First ch $ hdrs
  let hist' = write hist      -- Write the current history to not trigger twice
      stck' = hdrs' ++ stck   -- add the new headers to the stack
      curEmpty = null . current $ hist
  resolveTrigger loadInfo curEmpty gameState ch $ Game stck' hist' crds

  where
    -- TODO: can extend to have logic if desired
    determineFirstPlayer :: GameState -> Bool
    determineFirstPlayer _ = True

resolveTrigger :: LoadInfo -> Bool -> GameState -> Comm Game
resolveTrigger loadInfo curEmpty _ ch game@(Game [] _ _)
  | curEmpty = return game -- recursive base case
  | otherwise = resolveStack loadInfo ch game
    -- We may have a trigger on empty stack so need to check again
resolveTrigger loadInfo _ gameState ch (Game (hdr : stck') hist crds)
  = let game' = Game stck' hist crds
     in case hdr of   -- Dispatch to the correct handling
          (Targeted _ cID aID targets) ->
            resolveTargeted loadInfo gameState cID aID targets ch game'
          (Unassigned _ cID aID) ->
            resolveUnassigned loadInfo gameState cID aID ch game'

resolveTargeted :: LoadInfo -> GameState -> CardID -> AbilityID
                      -> TargetedMap -> Comm Game
resolveTargeted loadInfo gameState cID aID targets ch (Game stck hist crds)
  = case lookupAbility cID aID crds of
      Nothing -> undefined    -- Should crash, it is a bug
      (Just (Ability _ _ guard resolves _)) -> do
        let (hist', crds')
              = foldr (resolveResolve gameState cID) (hist, crds)
              . filter (validGuard gameState guard cID)
              . map (retrieveResolve resolves)
              . snd
              . foldr fillCreated (nxtCID, [])
              $ targets
            nxtCID = getNextKey cardID crds -- get current highest cID
            game' = Game stck hist' crds'
        resolveStack loadInfo ch game'
  where
    -- Here we allocate a new CardID for all the new cards being created
    -- and provide those as the target cID
    --
    -- We will increment for each to retain their uniqueness
    fillCreated :: (TargetID, Create CardID) -> (Int, [(TargetID, CardID)])
                      -> (Int, [(TargetID, CardID)])
    fillCreated (tID, Create) (cID, acc) = (cID + 1, (tID, CardID cID) : acc)
    fillCreated (tID, Existing cID) (idx, acc) = (idx, (tID, cID) : acc)

    -- Here we lookup and transform our TargetID to a Resolve to apply
    retrieveResolve :: Resolves -> (TargetID, CardID) -> (Resolve, CardID)
    retrieveResolve resolves (tID, cID) = (resolve, cID)
      where
        resolve = case Map.lookup tID resolves of
                    Nothing -> undefined    -- Should crash, it is a bug
                    (Just resolve) -> resolve

    -- wrapper function to filter out the resolves that not violate their
    -- guard
    validGuard :: GameState -> Guard -> CardID -> (Resolve, CardID) -> Bool
    validGuard gameState guard cID (_, tcID)
        = checkValid guard cID tcID gameState

    -- here we apply and record the alterations from the resolve
    resolveResolve :: GameState -> CardID -> (Resolve, CardID)
                        -> (History, Cards) -> (History, Cards)
    resolveResolve gameState cID (Resolve resolve, tcID) (hist, crds)
      = (hist', crds')
        where
          -- get or create our card to apply the changes
          (created, crd) = case Map.lookup tcID crds of
                             Nothing -> ([Created], create)
                             (Just crd) -> ([], crd)

          -- apply the changes
          (alterations, crd')
            = fmap (mintCreated created)  -- mint if card was created
            . changes (resolve cID tcID gameState)
            $ crd

          -- record all the alterations from the changes into history
          hist' = foldr (record . Event cID tcID) hist
                $ created ++ alterations

          -- update the card into our cards
          crds' = Map.insert tcID crd' crds

          -- If we have created a new card then we will mint it after we
          -- have applied the altertions to make the card
          mintCreated :: [Alteration] -> Card -> Card
          mintCreated [Created] = mint
          mintCreated _ = id

resolveUnassigned :: LoadInfo -> GameState -> CardID -> AbilityID -> Comm Game
resolveUnassigned loadInfo gameState cID aID ch (Game stck hist crds)
  = case lookupAbility cID aID crds of
      Nothing -> undefined    -- Should crash, it is a bug
      (Just ablty) -> do
        let owner = toEnum
                  . check cID Owner (fromEnum System)
                  . getCS $ gameState
        hdr <- requestTargets ch
             . Assigned owner cID aID
             $ getTargets (getTargeting ablty) cID gameState
        let game' = Game (hdr : stck) hist crds
        resolveTrigger loadInfo undefined gameState ch game'
