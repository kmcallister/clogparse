-- | Represents events in an IRC channel.
-- These do not correspond precisely to messages of the IRC
-- protocol.  They provide a somewhat higher level view.

module Data.IRC.Event
  ( Nick   (..)
  , Event  (..)
  , EventAt(..)
  ) where

import qualified Data.Time as Time
import qualified Data.Text as T

-- | Event with timestamp.
data EventAt
  = EventAt  Time.UTCTime Event  -- ^ Event with timestamp.
  | BadParse T.Text              -- ^ Unparsable line.
  deriving (Show, Eq, Ord)

-- | IRC nicks.
newtype Nick = Nick T.Text
  deriving (Show, Eq, Ord)

-- | Events in an IRC channel.
data Event
  = Join   Nick      T.Text  -- ^ User joined.
  | Part   Nick      T.Text  -- ^ User left the channel.
  | Quit   Nick      T.Text  -- ^ User quit the server.
  | ReNick Nick Nick         -- ^ User changed from one to another nick.
  | Talk   Nick      T.Text  -- ^ User spoke (@PRIVMSG@).
  | Notice Nick      T.Text  -- ^ User spoke (@NOTICE@).
  | Act    Nick      T.Text  -- ^ User acted (@CTCP ACTION@).
  | Kick   Nick Nick T.Text  -- ^ User was kicked by user.
  | Mode   Nick      T.Text  -- ^ User set mode on the channel.
  | Log              T.Text  -- ^ Logging started or stopped.
  | Topic            T.Text  -- ^ Topic listing or change.
  | Names            T.Text  -- ^ Users list.
  deriving (Show, Eq, Ord)
