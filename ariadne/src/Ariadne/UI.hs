module Ariadne.UI (createAriadneUI) where

import Prelude
import Control.Monad (void)

import qualified Brick as B
import Brick.BChan
import qualified Graphics.Vty as V

import Ariadne.Face
import Ariadne.UI.App (initialAppState, app, Components)

type UiAction components = KnitFace components-> IO ()

-- Initialize the UI, returning two components:
--
-- * a record of methods for interacting with the UI from other threads
-- * the IO action to run in the UI thread
createAriadneUI
  :: forall components.
     Components components
  => IO (UiFace components, UiAction components)
createAriadneUI = do
  eventChan <- mkEventChan
  return (mkUiFace eventChan, runUI eventChan)

-- Run the Ariadne UI. This action should be run in its own thread to provide a
-- responsive interface, and the application should exit when this action
-- completes.
runUI
  :: Components components
  => BChan (UiEvent components)
  -> KnitFace components
  -> IO ()
runUI eventChan auxxFace = do
  vtyConfig <- mkVtyConfig

  -- Run the Brick event loop:
  --   1. Draw the widgets.
  --   2. Wait for user input.
  --   3. Update the app state (handle the event).
  --   4. Goto 1.
  --
  -- Note that we don't want to do too much work while updating the application
  -- state, as it will make the UI unresponsive. Slow or blocking actions must
  -- be delegated to other threads.
  void $ B.customMain

    -- The first argument to 'customMain' is an action that creates an object
    -- that manages terminal interactions in block mode. This object can be
    -- destroyed when/if we switch to character mode (Brick does it when it
    -- suspends the event loop), and then created again (when we resume the
    -- interface), so we don't want to remake the terminal configuration here.
    (V.mkVty vtyConfig)

    -- The second argument to 'customMain' is a channel for custom events.
    -- They will be handled in the same event loop as keyboard input and
    -- mouse clicks.
    (Just eventChan)

    -- The third argument to 'customMain' is a record that contains the view
    -- and the controller.
    (app auxxFace)

    -- The fourth argument to 'customMain' is the initial application state.
    initialAppState

-- Build terminal configuration. This is where we can configure technical
-- details like mouse support, input/output file descriptors, terminal name
-- (reading from $TERM by default), etc.
mkVtyConfig :: IO V.Config
mkVtyConfig = do
  stdConfig <- V.standardIOConfig
  return stdConfig
    { V.mouseMode = Just True
    , V.inputMap =
      [
        -- Terminals map ^H to an escape sequence, we
        -- unmap it back.
        (Nothing, "\b", V.EvKey (V.KChar 'h') [V.MCtrl])
      ]
    }

-- Create a channel for application events that aren't user input. This channel
-- is bounded to avoid infinite accumulation of events, but the bound is
-- somewhat arbitrary.
mkEventChan :: IO (BChan (UiEvent components))
mkEventChan = newBChan 100

-- Create the API for interacting with the UI thread.
mkUiFace :: BChan (UiEvent components) -> UiFace components
mkUiFace eventChan =
  UiFace
    {
      putUiEvent = writeBChan eventChan
    }
