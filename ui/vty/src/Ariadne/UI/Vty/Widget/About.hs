module Ariadne.UI.Vty.Widget.About
       ( AboutWidgetState
       , initAboutWidget
       , drawAboutWidget

       , AboutWidgetEvent(..)
       , handleAboutWidgetEvent
       ) where

import Universum

import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.UI

import qualified Brick as B
import qualified Brick.Widgets.Center as B
import qualified Graphics.Vty as V

data AboutWidgetState = AboutWidgetState ()

widgetName :: BrickName
widgetName = BrickAbout

initAboutWidget :: AboutWidgetState
initAboutWidget = AboutWidgetState ()

drawAboutWidget :: AboutWidgetState -> B.Widget BrickName
drawAboutWidget _aboutWidgetState =
  B.viewport widgetName B.Vertical $
    B.hCenter $ B.cached widgetName $ B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        selAttr = attr <> B.attrMapLookup "focused" (rdrCtx ^. B.ctxAttrMapL)
        img = aboutBanner attr selAttr
      return $
        B.emptyResult
          & B.imageL .~ img

data AboutCompleted = AboutCompleted | AboutInProgress

data AboutWidgetEvent
  = AboutScrollingEvent ScrollingAction

handleAboutWidgetEvent
  :: AboutWidgetEvent
  -> StateT AboutWidgetState (B.EventM BrickName) ()
handleAboutWidgetEvent ev = do
  case ev of
    AboutScrollingEvent action ->
      lift $ handleScrollingEvent widgetName action

aboutBanner :: V.Attr -> V.Attr -> V.Image
aboutBanner defAttr selAttr = V.vertCat
  [ V.text' defAttr ""
  , V.horizCat [ V.text' defAttr "                            ", V.text' selAttr "ARIADNE"]
  , V.text' defAttr "A cryptocurrency wallet developed by Serokell."
  , V.text' defAttr ""
  , V.text' defAttr "Ariadne is distributed under the terms of the MPL 2.0 license."
  , V.horizCat
      [ V.text' defAttr "For details, please, see: "
      , V.text' selAttr "https://serokell.io/ariadne/license"
      ]
  , V.text' defAttr ""
  , V.horizCat
      [ V.text' defAttr "The source code is available at "
      , V.text' selAttr "https://github.com/serokell/ariadne"
      ]
  ]
