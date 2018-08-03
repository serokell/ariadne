module Ariadne.UI.Vty.Widget.Help
       ( initHelpWidget
       ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget

data HelpWidgetState =
  HelpWidgetState
    { helpWidgetData :: ![PP.Doc]
    }

makeLensesWith postfixLFields ''HelpWidgetState

initHelpWidget :: UiLangFace -> Widget p
initHelpWidget UiLangFace{..} =
  initWidget $ do
    setWidgetDraw drawHelpWidget
    setWidgetScrollable
    setWidgetState HelpWidgetState
      { helpWidgetData = langGetHelp
      }

drawHelpWidget :: HelpWidgetState -> WidgetDrawM HelpWidgetState p (B.Widget WidgetName)
drawHelpWidget HelpWidgetState{..} = do
  widgetName <- getWidgetName
  return $
    B.viewport widgetName B.Vertical $
    B.cached widgetName $
    B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        width = rdrCtx ^. B.availWidthL
        img =
          V.vertCat $
          fmap drawDoc helpWidgetData
        drawDoc = pprDoc attr width
      return $
        B.emptyResult
          & B.imageL .~ img