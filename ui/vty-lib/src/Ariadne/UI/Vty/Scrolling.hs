module Ariadne.UI.Vty.Scrolling
     ( ScrollingAction(..)
     , keyToScrollingAction
     , handleScrollingEvent
     , scrollToEnd
     , fixedViewport
     , scrollingViewport
     , fixedScrollingViewport
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
scrollingViewport
    :: (Ord n, Show n)
    => n                -- ^ The name of the viewport (must be unique)
    -> B.ViewportType   -- ^ The type of the viewport (scrolling direction)
    -> B.Widget n       -- ^ The widget to be rendered in the viewport
    -> B.Widget n
scrollingViewport = withScrollBar B.viewport

-- | Adds a scrollbar just like scrollingViewport, but uses a fixedViewport
-- instead of a Brick.Widgets.Core viewport
fixedScrollingViewport
    :: (Ord n, Show n)
    => n                -- ^ The name of the viewport (must be unique)
    -> B.ViewportType   -- ^ The type of the viewport (scrolling direction)
    -> B.Widget n       -- ^ The widget to be rendered in the viewport
    -> B.Widget n
fixedScrollingViewport = withScrollBar fixedViewport

withScrollBar
    :: (Ord n, Show n)
    => (n -> B.ViewportType -> B.Widget n -> B.Widget n)
    -> n
    -> B.ViewportType
    -> B.Widget n
    -> B.Widget n
withScrollBar vpFunc name vpType p = case vpType of
    B.Both       -> B.Widget B.Greedy B.Greedy render
    B.Vertical   -> B.Widget (B.hSize p) B.Greedy render
    B.Horizontal -> B.Widget B.Greedy (B.vSize p) render
  where
    render = do
        -- first get the current context
        c <- B.getContext
        -- adjust it for the inner widget and render it
        result <- renderReleased vpType p
        -- add the scrollbar to the result
        mvp <- B.unsafeLookupViewport name
        let vpWidget = vpFunc name vpType $
                B.Widget (B.hSize p) (B.vSize p) (return result)

            resultImg = B.image result
            (imageH, imageW) = (V.imageHeight resultImg, V.imageWidth resultImg)
            (availH, availW) = case vpType of
                B.Both -> (B.availHeight c - 1, B.availWidth c - 1)
                _ -> (B.availHeight c, B.availWidth c)

            barDims :: Int -> Int -> Maybe Int -> (Int, Int)
            barDims availDim imgDim vpPad = (barSize, barPad)
              where
                availableDim = fromIntegral availDim
                imageDim = fromIntegral imgDim
                vpPadding = fromIntegral $ fromMaybe 0 vpPad
                ratio = availableDim / imageDim :: Double
                barSize = max 1 . round $ ratio * availableDim
                tmpPad = round $ ratio * vpPadding
                barPad = if tmpPad + barSize > availDim then availDim - barSize
                    else tmpPad

            (vSize, vPad) = barDims availH imageH $ view B.vpTop <$> mvp
            vBar = B.vLimit (vPad + vSize) $ B.padTop (B.Pad vPad) BR.vBorder
            addVBar wdg = if imageH <= availH then wdg else B.hBox [wdg, vBar]

            (hSize, hPad) = barDims availW imageW $ view B.vpLeft <$> mvp
            hBar = B.hLimit (hPad + hSize) $ B.padLeft (B.Pad hPad) BR.hBorder
            addHBar wdg = if imageW <= availW then wdg else B.vBox [wdg, hBar]

        B.render $ case vpType of
            B.Vertical   -> addVBar vpWidget
            B.Horizontal -> addHBar vpWidget
            B.Both       -> addHBar $ addVBar vpWidget

renderReleased :: B.ViewportType -> B.Widget n -> B.RenderM n (B.Result n)
renderReleased vpType p = R.withReaderT releasing $ B.render p
  where
    unrestricted = 100000
    releasing = case vpType of
        B.Vertical   -> set B.availHeightL unrestricted
                          . over B.availWidthL (subtract 1)
        B.Horizontal -> set B.availWidthL unrestricted
                          . over B.availHeightL (subtract 1)
        B.Both       -> over B.availHeightL (subtract 1)
                          . over B.availWidthL (subtract 1)