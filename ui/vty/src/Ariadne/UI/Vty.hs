module Ariadne.UI.Vty
  ( UiFace(..)
  , createAriadneUI
  ) where

import Universum

import qualified Brick as B
import Brick.BChan
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.App
import Ariadne.UI.Vty.Face

type UiAction = UiLangFace -> IO ()

-- Initialize the UI, returning two components:
--
-- * a record of methods for interacting with the UI from other threads
-- * the IO action to run in the UI thread
createAriadneUI :: UiHistoryFace -> IO (UiFace, UiAction)
createAriadneUI historyFace = do
  eventChan <- mkEventChan
  let uiFace = mkUiFace eventChan

  return (uiFace, runUI eventChan uiFace historyFace)

-- Run the Ariadne UI. This action should be run in its own thread to provide a
-- responsive interface, and the application should exit when this action
-- completes.
runUI
  :: BChan UiEvent
  -> UiFace
  -> UiHistoryFace
  -> UiLangFace
  -> IO ()
runUI eventChan uiFace historyFace langFace = do
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
    app

    -- The fourth argument to 'customMain' is the initial application state.
    (initApp uiFace langFace historyFace)

-- Build terminal configuration. This is where we can configure technical
-- details like mouse support, input/output file descriptors, terminal name
-- (reading from $TERM by default), etc.
mkVtyConfig :: IO V.Config
mkVtyConfig = do
  stdConfig <- V.standardIOConfig
  return stdConfig
    { V.mouseMode = Just True
    , V.bracketedPasteMode = Just True
    }

-- Create a channel for application events that aren't user input. This channel
-- is bounded to avoid infinite accumulation of events, but the bound is
-- somewhat arbitrary.
mkEventChan :: IO (BChan UiEvent)
mkEventChan = newBChan 100

-- Create the API for interacting with the UI thread.
mkUiFace :: BChan UiEvent -> UiFace
mkUiFace eventChan =
  UiFace
    {
      putUiEvent = writeBChan eventChan
    }
