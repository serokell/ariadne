-- APIs for communication between threads.
module Ariadne.Face
  ( KnitFace(..),
    CardanoNodeEvent(..),
    KnitEvent(..),
    UiEvent(..),
    UiFace(..),
    CommandId(..),
    CommandResult(..),
    DefaultKnitComponents
  ) where

import Prelude
import Data.Unique
import Data.List.NonEmpty
import Control.Exception (SomeException)

import qualified Knit
import qualified Ariadne.Knit.Cardano as Knit

-- A unique identifier assigned to each command, needed to associate it with
-- the result of its execution.
newtype CommandId = CommandId Unique
  deriving (Eq)

-- The result of executing an auxx command.
data CommandResult components
  = CommandSuccess (Knit.Value components)
  | CommandEvalError (Knit.EvalError components)
  | CommandProcError (NonEmpty Knit.Name)
  | CommandException SomeException

data CardanoNodeEvent
  = CardanoNodeEvent -- update slot id, stuff like that

-- An event triggered by the knit backend.
data KnitEvent components
  -- a command has finished execution
  = KnitResultEvent CommandId (CommandResult components)

data UiEvent components
  = UiCardanoEvent CardanoNodeEvent -- the node state has changed
  | UiKnitEvent (KnitEvent components)

data KnitFace components =
  KnitFace
    {
      -- Execute a knit expression asynchronously. Does not block unless the
      -- queue of commands is full (should not normally happen) -- the result of
      -- execution will be returned later as an application event.
      putKnitCommand :: Knit.Expr Knit.Name components -> IO CommandId
    }

-- API for the UI.
data UiFace components =
  UiFace
    {
      -- Update the user interface with an event. Does not block unless the
      -- queue of events is full (should not normally happen).
      putUiEvent :: UiEvent components -> IO ()
    }

-- TODO: move to 'main'
type DefaultKnitComponents = '[Knit.Core, Knit.Cardano]
