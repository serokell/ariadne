-- APIs for communication between threads.
module Ariadne.Face
  (
    AuxxFace(..),
    AuxxEvent(..),
    UiFace(..),
    CommandId(..)
  ) where

import Prelude
import qualified Lang as Auxx
import Data.Unique

-- A unique identifier assigned to each command, needed to associate it with
-- the result of its execution.
newtype CommandId = CommandId Unique

-- An event triggered by the auxx backend.
data AuxxEvent
  = CardanoNodeEvent -- update slot id, stuff like that
  | AuxxResultEvent CommandId Auxx.Value -- a command has finished execution

-- API for the auxx backend.
data AuxxFace =
  AuxxFace
    {
      -- Execute an auxx expression asynchronously. Does not block unless the
      -- queue of commands is full (should not normally happen) -- the result of
      -- execution will be returned later as an application event.
      putAuxxCommand :: CommandId -> Auxx.Expr Auxx.Name -> IO ()
    }

-- API for the UI.
data UiFace =
  UiFace
    {
      -- Update the user interface with an event. Does not block unless the
      -- queue of events is full (should not normally happen).
      putUiEvent :: AuxxEvent -> IO ()
    }
