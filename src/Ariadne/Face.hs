-- APIs for communication between threads.
module Ariadne.Face
  (
    AuxxFace(..),
    AuxxEvent(..),
    UiFace(..),
    CommandId(..),
    CommandResult(..)
  ) where

import Prelude
import Data.Unique
import Data.List.NonEmpty
import Control.Exception (SomeException)

import qualified Lang as Auxx

-- A unique identifier assigned to each command, needed to associate it with
-- the result of its execution.
newtype CommandId = CommandId Unique

-- The result of executing an auxx command.
data CommandResult
  = CommandSuccess Auxx.Value
  | CommandEvalError Auxx.EvalError
  | CommandProcError (NonEmpty Auxx.Name)
  | CommandException SomeException

data CardanoNodeEvent
  = CardanoNodeEvent -- update slot id, stuff like that

-- An event triggered by the auxx backend.
data AuxxEvent
  = AuxxCardanoEvent CardanoNodeEvent -- the node state has changed
  | AuxxResultEvent CommandId CommandResult -- a command has finished execution

-- API for the auxx backend.
data AuxxFace =
  AuxxFace
    {
      -- Execute an auxx expression asynchronously. Does not block unless the
      -- queue of commands is full (should not normally happen) -- the result of
      -- execution will be returned later as an application event.
      putAuxxCommand :: Auxx.Expr Auxx.Name -> IO CommandId
    }

-- API for the UI.
data UiFace =
  UiFace
    {
      -- Update the user interface with an event. Does not block unless the
      -- queue of events is full (should not normally happen).
      putUiEvent :: AuxxEvent -> IO ()
    }
