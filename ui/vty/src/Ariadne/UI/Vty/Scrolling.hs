module Ariadne.UI.Vty.Scrolling
     ( ScrollingAction(..)
     , keyToScrollingAction
     , handleScrollingEvent
     , scrollToEnd
     , fixedViewport
     ) where

import Universum

import Ariadne.UI.Vty.Keyboard

import qualified Brick as B
import qualified Graphics.Vty as V

data ScrollingAction
  = ScrollingLineUp
  | ScrollingLineDown
  | ScrollingPgUp
  | ScrollingPgDown
  | ScrollingHome
  | ScrollingEnd
  | ScrollingLeft
  | ScrollingRight
  deriving (Eq)

keyToScrollingAction :: KeyboardEvent -> Maybe ScrollingAction
keyToScrollingAction = \case
  KeyUp       -> Just ScrollingLineUp
  KeyDown     -> Just ScrollingLineDown
  KeyCtrlUp   -> Just ScrollingLineUp
  KeyCtrlDown -> Just ScrollingLineDown
  KeyPageUp   -> Just ScrollingPgUp
  KeyPageDown -> Just ScrollingPgDown
  KeyHome     -> Just ScrollingHome
  KeyEnd      -> Just ScrollingEnd
  KeyLeft     -> Just ScrollingLeft
  KeyRight    -> Just ScrollingRight
  KeyChar 'h' -> Just ScrollingLeft
  KeyChar 'j' -> Just ScrollingLineDown
  KeyChar 'k' -> Just ScrollingLineUp
  KeyChar 'l' -> Just ScrollingRight
  _ -> Nothing

handleScrollingEvent :: n -> ScrollingAction -> B.EventM n ()
handleScrollingEvent name = \case
    ScrollingLineUp -> B.vScrollBy vps -1
    ScrollingLineDown -> B.vScrollBy vps 1
    ScrollingPgUp -> B.vScrollPage vps B.Up
    ScrollingPgDown -> B.vScrollPage vps B.Down
    ScrollingHome -> B.vScrollToBeginning vps
    ScrollingEnd -> B.vScrollToEnd vps
    ScrollingLeft -> B.hScrollBy vps -1
    ScrollingRight -> B.hScrollBy vps 1
  where vps = B.viewportScroll name

scrollToEnd :: n -> B.EventM n ()
scrollToEnd = B.vScrollToEnd . B.viewportScroll

-- Unidirectional Brick viewport doesn't defer its sizing policy
-- to underlying widget. Brick author considers it okay.
-- See https://github.com/jtdaugherty/brick/issues/174
-- This function works around this by getting the size of widget first
-- and limiting the size of resulting viewport.
fixedViewport
  :: (Ord n, Show n)
  => n
  -> B.ViewportType
  -> B.Widget n
  -> B.Widget n
fixedViewport name vpType p = case vpType of
  B.Both -> B.Widget B.Greedy B.Greedy render
  B.Vertical -> B.Widget (B.hSize p) B.Greedy render
  B.Horizontal -> B.Widget B.Greedy (B.vSize p) render
  where
    render = do
      result <- B.render p
      B.render $
        limit result .
        B.viewport name vpType $
        B.Widget (B.hSize p) (B.vSize p) (return result)
    limit :: B.Result n -> (B.Widget n -> B.Widget n)
    limit result = case vpType of
      B.Vertical -> B.hLimit (result ^. B.imageL & V.imageWidth)
      B.Horizontal -> B.vLimit (result ^. B.imageL & V.imageHeight)
      B.Both -> identity
