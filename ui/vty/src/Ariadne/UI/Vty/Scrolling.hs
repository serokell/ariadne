module Ariadne.UI.Vty.Scrolling
     ( ScrollingAction(..)
     , keyToScrollingAction
     , handleScrollingEvent
     , scrollToEnd
     , keepScrollingToEnd
     ) where

import Universum

import Ariadne.UI.Vty.Keyboard

import qualified Brick as B

data ScrollingAction
  = ScrollingLineUp
  | ScrollingLineDown
  | ScrollingPgUp
  | ScrollingPgDown
  | ScrollingHome
  | ScrollingEnd
  | ScrollingLeft
  | ScrollingRight

keyToScrollingAction :: KeyboardEvent -> Maybe ScrollingAction
keyToScrollingAction = \case
  KeyUp       -> Just ScrollingLineUp
  KeyDown     -> Just ScrollingLineDown
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

keepScrollingToEnd :: Ord n => n -> Int -> B.EventM n ()
keepScrollingToEnd name lineCount =
  whenJustM (B.lookupViewport name) $ \vp ->
    when (vp ^. B.vpTop + vp ^. B.vpSize ^. _2 >= lineCount) $
      B.vScrollToEnd $ B.viewportScroll name
