module Ariadne.UI.Vty.Widget.Status where

import Control.Lens
import Control.Monad.Trans.State as State
import Data.Text (Text)
import Prelude

import qualified Brick as B
import qualified Graphics.Vty as V

import IiExtras

data StatusWidgetState =
  StatusWidgetState
    {
      statusWidgetContent :: Text
    }

makeLensesWith postfixLFields ''StatusWidgetState

initStatusWidget :: StatusWidgetState
initStatusWidget = StatusWidgetState
    {
      statusWidgetContent = "Here be status"
    }

drawStatusWidget
  :: StatusWidgetState
  -> B.Widget name
drawStatusWidget statusWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        (leftPad, rightPad) =
          integralDistribExcess
            (rdrCtx ^. B.availWidthL)
            (V.imageWidth img)

        backStatusAttr =
          V.defAttr
            `V.withForeColor` V.black
            `V.withBackColor` V.white

        fill n = V.charFill @Int backStatusAttr ' ' n 1

        img = V.text' backStatusAttr $ statusWidgetContent statusWidgetState

        img' =
          V.horizCat [fill leftPad, img, fill rightPad]

      return $
        B.emptyResult
          & B.imageL .~ img'

data StatusWidgetEvent
  = StatusUpdateEvent Text

handleStatusWidgetEvent
  :: StatusWidgetEvent
  -> StateT StatusWidgetState IO ()
handleStatusWidgetEvent = \case
  StatusUpdateEvent text -> zoom statusWidgetContentL $ State.modify $ const text
