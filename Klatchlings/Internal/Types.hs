module Internal.Types where

import Base.Fields
import qualified Data.Map as Map (Map)

-- A Card ID is a unique integer that refers to an instance of a gard in
-- a game
newtype CardID = CardID
  { cardID :: Int
  }
  deriving (Eq, Ord)

-- Allows us to query a cards ability without needing to define an
-- instance of Eq Ability
newtype AbilityID = AbilityID 
  { abilityID :: Int
  }
  deriving (Eq, Ord)

-- Allows us to query an ability's target without needing to define an
-- instance of Eq Target
newtype TargetID = TID
  { targetID :: Int
  }
  deriving (Eq, Ord)

-- An Eval is how we will evaluate a fields value when we want to create
-- the given state of the Cards
newtype Eval = Eval
  { doEval :: CardState -> Int -> Int
  }
-- Note: We can wrap evals to allow for monoid notation when updating or
-- altering

instance Semigroup Eval where
  (<>) (Eval e1) (Eval e2)
    = Eval $ \cs -> e1 cs . e2 cs

instance Monoid Eval where
  mempty = Eval $ const id

wrap :: (Int -> Int) -> Eval
wrap = Eval . const

-- Cards store how their fields should be evaluated and their abilities,
-- as well as, their original card form.
--
-- FIXME: this implementation of original is temporary until we can directly
-- load a complied card
data Card
  = Card
  { fields    :: Map.Map Field Eval
  , abilities :: AbilityMap
  , original  :: Maybe Card
  }
type Cards = Map.Map CardID Card
type CardState = Map.Map CardID FieldMap

-- We can then define our game structure that will keep track of everything
data Game = Game Stack History Cards
data GameState = GameState
  { getStack    :: Stack
  , getHistory  :: History
  , getCS       :: CardState
  }

-- To allow for cards to trigger or react from particular game actions we need
-- to be able to enumerate over them to the best of our ability. Hence, we
-- have the possible alterations of a card listed:
data Alteration
  = Shift Field Int     -- We shifted the Field value by Int
  | Set Field Int       -- We set an Attr to a value Int
  | Alter Field         -- We either modified or replaced how to eval the Field
  | Equip               -- We gave the card an ability
  | Created             -- This card was created as a token

-- A change can consist of numerous alterations
-- and we can wrap it in a newtype to allow for monoid notation to combine
-- numerous changes into a single one
newtype Change = Change
  { changes :: Card -> ([Alteration], Card)
  }

instance Semigroup Change where
  (<>) (Change c1) (Change c2)
    = Change $ \crd ->
      let (alts1, crd') = c2 crd
          (alts2, crd'') = c1 crd
       in (alts2 ++ alts1, crd'')

instance Monoid Change where
  mempty = Change ([], )

-- Ability related things
data Ability = Ability
  { getTiming     :: Timing
  , getTrigger    :: Trigger
  , getGuard      :: Guard
  , getResolves   :: Resolves
  , getTargeting  :: Targeting
  }
type AbilityMap = Map.Map AbilityID Ability

-- A timing denotes when we should request our targets, either we can get
-- the targets when the trigger occurs or when we are about to resolve the
-- ability
--
-- The was a consequence of trying to 'program' drawing a random card in
-- previous prototypes. We can't assign the CardID to draw at trigger
-- since that card may move before it resolves. Hence, we need a way to target
-- right before resolution to ensure things
data Timing
  = OnResolve
  | OnTrigger
  deriving Eq

-- Each ability will have a trigger that we can evaluate to determine if we
-- should put it on the stack. The CardID denotes the owner of the ability,
-- this is commonly required for abilities that are triggered when a Field
-- of the owning card is shifted/set
newtype Trigger = Trigger
  { checkTrigger :: CardID -> GameState -> Bool
  }

-- Since the gamestate can change from when the ability triggers and to when
-- it resolves, each ability has a guard to determine if we can or should still
-- resolve the ability in the new gamestate
--
-- NOTE: We could instead have a Resolve have a Maybe computation to let it
-- fail, however, it "feels better" to have an explicit guard and a resolve
-- that will always compute, however, to have a guard and then resolve
-- may be more computational expensive
newtype Guard = Guard
  { checkValid :: CardID -> CardID -> GameState -> Bool
  }

-- A resolve is best explained through its parameters:
--  CardID -> Denoting which card triggered and ability it is from
--  CardID -> The card that is being targeted
--  GameState -> The current gamestate
--  Change -> The change that will occur
newtype Resolve = Resolve
  { resolve :: CardID -> CardID -> GameState -> Change
  }
type Resolves = Map.Map TargetID Resolve

-- A targeting allows us a way to map the possible targets for the ability to
-- their respective resolves, so when we are creating an ability the resolve
-- will denote what happens to the Target of the matching TargetID
newtype Targeting = Targeting
  { getTargets :: CardID -> GameState -> TargetMap
  }
type TargetMap = [(TargetID, Target)]
type TargetedMap = [(TargetID, Create CardID)]

-- A header denotes the various states of a trigger from the time it triggers
-- to resolution. First we collect all the triggered abilities from the Cards
-- and we can store which abilities as:
--
--    Unassigned: If Timing = OnTrigger then we will pass the targets as
--  unassigned until we resolve the Ability, so we just need to keep track
--  of the CardID and AbilityID to retreive later
--
--    Assigned: If Timing = OnResolve then we compute the targets directly when
--  the ability is triggered, so we store the possible targets to pass up for
--  targeting
--
--  After we collect the abilities then the abilities will be reordered
--  accordingly, after which we will target all the assigned headers.
--
--  So then all Assigned headers are transformed into Targeted headers
--  and we append this collection of Targeted or Unassigned headers onto
--  the stack.
--
--  Then we go to resolve the ability, where all the unassigned headers
--  are transformed to an Assigned header by retrieving and computing
--  the Targets from the current GameState. Then we follow the same process
--  to transform the Assigned header to a Targeted header. So then we
--  go to resolve the ability and we will only have a collection of
--  Targeted headers in which we can do the ability
data Header
  = Unassigned CardID AbilityID
  | Assigned CardID AbilityID TargetMap
  | Targeted CardID AbilityID TargetedMap

-- Create datatype allows us to denote if we should Create a new card for the
-- ability to be applied to or to apply it to an existing card
data Create a = Create | Existing a

-- The Stack will store the current abilities that have triggered but have
-- not yet resolved
type Stack = [Header]

-- We may have many possible targets and denote this in a range of targets
type Range = [CardID]
data Target
  = Given CardID        -- We already know which card to target
  | Inquire Range       -- We will need to request a target from the range
  | Random Range        -- We will request a random target from the range
  | Void                -- We will need to create a new card for the ability
  deriving (Eq, Ord)

-- History related things
-- A history records 'Events' of what has occured throughout the game.
-- The first element of the tuple dentoes the Events that have just occured
-- to allow for cards to trigger on, the second element denotes all the events
-- that have occured, the front element is the most recent event.
-- Each event will spend exactly one 'frame' in the current history before
-- being moved to the past history. This allows an easier way for triggers
-- to react to an event that just occured
newtype History = History
  { history :: ([Event], [Event])
  }

-- An event records an Alteration that the first CardID did to the second CardID
data Event = Event CardID CardID Alteration
