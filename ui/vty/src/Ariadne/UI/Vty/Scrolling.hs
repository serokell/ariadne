module Ariadne.UI.Vty.Scrolling
     ( ScrollingOffset,
       ScrollingAction(..),
       defaultScrollingOffset,
       handleScrollingEvent,
       cropScrolling) where

import Universum

import qualified Graphics.Vty as V

data ScrollingOffsetData
    = OffsetFollowing
    | OffsetFixed Int

type ScrollingOffset = Int -> Int -> ScrollingOffsetData
-- ^ viewport height -> full image height -> offset from top
-- TODO (thatguy): use `named`

data ScrollingAction
  = ScrollingLineUp
  | ScrollingLineDown
  | ScrollingPgUp
  | ScrollingPgDown

data ScrollingDistance = OneLine | Page

defaultScrollingOffset :: ScrollingOffset
defaultScrollingOffset _ _ = OffsetFollowing

cropScrolling :: Int -> ScrollingOffset -> V.Image -> V.Image
cropScrolling viewportHeight mkPos image =
  let imageHeight = V.imageHeight image in
  case mkPos viewportHeight imageHeight of
    OffsetFollowing ->
      V.cropTop viewportHeight image
    OffsetFixed pos ->
      V.cropBottom viewportHeight $ V.cropTop (imageHeight - pos) image

handleScrollingEvent :: ScrollingAction -> ScrollingOffset -> ScrollingOffset
handleScrollingEvent = \case
  ScrollingLineUp -> goUp OneLine
  ScrollingLineDown -> goDown OneLine
  ScrollingPgUp -> goUp Page
  ScrollingPgDown -> goDown Page

goUp :: ScrollingDistance -> ScrollingOffset -> ScrollingOffset
goUp distance mkPos viewportHeight imageHeight =
  let numLines = toNumLines viewportHeight distance
      prev = mkPos viewportHeight imageHeight
      pos = unwrapOffset (imageHeight - viewportHeight) prev
  in OffsetFixed $ max 0 (pos - numLines)

goDown :: ScrollingDistance -> ScrollingOffset -> ScrollingOffset
goDown distance mkPos viewportHeight imageHeight =
  let numLines = toNumLines viewportHeight distance in
  case mkPos viewportHeight imageHeight of
    OffsetFollowing -> OffsetFollowing
    OffsetFixed pos ->
      if pos >= imageHeight - viewportHeight - numLines then OffsetFollowing
      else OffsetFixed $ pos + numLines

toNumLines :: Int -> ScrollingDistance -> Int
toNumLines viewportHeight = \case
  OneLine -> 1
  Page -> viewportHeight

unwrapOffset :: Int -> ScrollingOffsetData -> Int
unwrapOffset def = \case
  OffsetFollowing -> def
  OffsetFixed pos -> pos
