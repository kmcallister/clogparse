{-# LANGUAGE
    DeriveDataTypeable #-}

-- | Represents events in an IRC channel.
-- These do not correspond precisely to messages of the IRC
-- protocol.  They provide a somewhat higher-level view.

module Data.IRC.Event
  ( -- * Events
    Nick   (..)
  , Event  (..)
  , EventAt(..)

    -- * Generic events
  , GenericEvent(..)
  , decompose
  ) where

import qualified Data.Time as Time
import qualified Data.Text as T

import Data.Typeable ( Typeable )
import Data.Data     ( Data, Constr, toConstr )

-- | Event with timestamp.
data EventAt
  = EventAt Time.UTCTime Event  -- ^ Event with timestamp.
  | NoParse T.Text              -- ^ Unparsable line.
  deriving (Show, Eq, Ord, Typeable)
  -- UTCTime lacks Data, so we can't derive it.
  -- We don't want to force that orphan instance on users.

-- | IRC nicks.
newtype Nick = Nick T.Text
  deriving (Show, Eq, Ord, Typeable, Data)

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
  deriving (Show, Eq, Ord, Typeable, Data)

-- | For working with @'Event'@s generically.
--
-- Indicates the \"subject\" of an event, if any, followed
-- by other text.
--
-- The subject of a @'ReNick'@ event is the old nick.
data GenericEvent
  = GenericEvent Constr (Maybe Nick) [T.Text]
  deriving (Show, Eq, Typeable)

-- | Decompose an @'Event'@ into a @'GenericEvent'@.
decompose :: Event -> GenericEvent
decompose x = go (GenericEvent $ toConstr x) x where
  go c (Join   n t) = c (Just n) [t]
  go c (Part   n t) = c (Just n) [t]
  go c (Quit   n t) = c (Just n) [t]
  go c (Talk   n t) = c (Just n) [t]
  go c (Notice n t) = c (Just n) [t]
  go c (Act    n t) = c (Just n) [t]
  go c (Mode   n t) = c (Just n) [t]

  go c (Log   t) = c Nothing [t]
  go c (Topic t) = c Nothing [t]
  go c (Names t) = c Nothing [t]

  go c (ReNick n (Nick t)) = c (Just n) [t]
  go c (Kick a (Nick b) t) = c (Just a) [b,t]
