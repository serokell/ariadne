module Ariadne.UI.Vty.Scrolling
     ( ScrollingAction(..)
     , eventToScrollingAction
     , handleScrollingEvent
     , scrollToEnd
     , keepScrollingToEnd
     ) where

import Universum

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

eventToScrollingAction :: V.Event -> Maybe ScrollingAction
eventToScrollingAction = \case
  V.EvKey V.KUp         [] -> Just ScrollingLineUp
  V.EvKey (V.KChar 'k') [] -> Just ScrollingLineUp
  V.EvKey V.KDown       [] -> Just ScrollingLineDown
  V.EvKey (V.KChar 'j') [] -> Just ScrollingLineDown
  V.EvKey V.KPageUp     [] -> Just ScrollingPgUp
  V.EvKey V.KPageDown   [] -> Just ScrollingPgDown
  V.EvKey V.KHome       [] -> Just ScrollingHome
  V.EvKey V.KEnd        [] -> Just ScrollingEnd
  V.EvKey V.KLeft       [] -> Just ScrollingLeft
  V.EvKey (V.KChar 'h') [] -> Just ScrollingLeft
  V.EvKey V.KRight      [] -> Just ScrollingRight
  V.EvKey (V.KChar 'l') [] -> Just ScrollingRight
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