module Ariadne.UI.Vty.Scrolling
     ( ScrollingAction(..)
     , keyToScrollingAction
     , handleScrollingEvent
     , scrollToEnd
     , fixedViewport
     , viewportWithScrollBar
     ) where

import Universum

import Ariadne.UI.Vty.Keyboard

import qualified Brick as B
import qualified Brick.Widgets.Border as BR
import qualified Graphics.Vty as V
import qualified Control.Monad.Trans.Reader as R

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
  KeyModUp    -> Just ScrollingLineUp
  KeyModDown  -> Just ScrollingLineDown
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

-- | Creates a viewport with a scrollbar on the right, bottom or both sides.
-- Note: when there is no need the scrollbar does not get displayed, but
-- it will still give one column/row less to the sub-widget to use.
viewportWithScrollBar
    :: (Ord n, Show n)
    => n                -- ^ The name of the viewport (must be unique)
    -> B.ViewportType   -- ^ The type of the viewport (scrolling direction)
    -> B.Widget n       -- ^ The widget to be rendered in the viewport
    -> B.Widget n
viewportWithScrollBar name vpType p = B.Widget B.Greedy B.Greedy $ do
    -- first get the current context
    c <- B.getContext
    -- adjust it for the inner widget
    let unrestricted = 100000
        releasing = case vpType of
            B.Vertical   -> set B.availHeightL unrestricted
                              . over B.availWidthL (subtract 1)
            B.Horizontal -> set B.availWidthL unrestricted
                              . over B.availHeightL (subtract 1)
            B.Both       -> over B.availHeightL (subtract 1)
                              . over B.availWidthL (subtract 1)
    -- render the inner widget
    result <- R.withReaderT releasing $ B.render p
    -- add the scrollbar to the result
    mvp <- B.unsafeLookupViewport name
    let vpWidget = B.viewport name vpType $
            B.Widget (B.hSize p) (B.vSize p) (return result)

        resultImg = B.image result

        enoughHeight = V.imageHeight resultImg <= B.availHeight c
        availHeight = fromIntegral $ B.availHeight c
        resultHeight = fromIntegral $ V.imageHeight resultImg
        vpPadTop = fromIntegral $ maybe 0 (view B.vpTop) mvp
        vRatio = availHeight / resultHeight :: Double
        vPad = round $ vRatio * vpPadTop 
        vSize = max 1 . round $ vRatio * availHeight
        vBar = B.vLimit (vPad + vSize) $ B.padTop (B.Pad vPad) BR.vBorder
        addVBar widg = if enoughHeight then widg else B.hBox [widg, vBar]

        enoughWidth = V.imageWidth resultImg <= B.availWidth c
        availWidth = fromIntegral $ B.availWidth c
        resultWidth = fromIntegral $ V.imageWidth resultImg
        vpPadLeft = fromIntegral $ maybe 0 (view B.vpLeft) mvp
        hRatio = availWidth / resultWidth :: Double
        hPad = round $ hRatio * vpPadLeft
        hSize = max 1 . round $ hRatio * availWidth
        hBar = B.hLimit (hPad + hSize) $ B.padLeft (B.Pad hPad) BR.hBorder
        addHBar widg = if enoughWidth then widg else B.vBox [widg, hBar]

    B.render $ case vpType of
        B.Vertical   -> addVBar vpWidget
        B.Horizontal -> addHBar vpWidget
        B.Both       -> addHBar $ addVBar vpWidget
