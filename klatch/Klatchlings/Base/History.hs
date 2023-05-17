module Base.History
  ( History
  , Event(..)
  , Alteration(..)
  -- External Interface
  , begin
  , current
  , past
  , record
  , write
  ) where

import Internal.Types(History(..), Event(..), Alteration(..))

-- Begin with an empty history
begin :: History
begin = History ([], [])

-- Look at the current events only
current :: History -> [Event]
current (History (cur, _)) = cur

-- Look at the past events only
past :: History -> [Event]
past (History (_, past)) = past

-- Record an event to the current history
record :: Event -> History -> History
record event (History (cur, past)) = History (event : cur, past)

-- Write and merge the current history to the past history
write :: History -> History
write (History (cur, past)) = History ([], cur ++ past)
