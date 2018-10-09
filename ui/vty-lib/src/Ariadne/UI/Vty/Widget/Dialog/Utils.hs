module Ariadne.UI.Vty.Widget.Dialog.Utils
       ( DialogState
       , newDialogState
       , addDialogButton
       , drawInsideDialog
       ) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as B
import Control.Lens (makeLensesWith, (%=))

import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.Util

data DialogState = DialogState
    { dialogLabel   :: !Text
    , dialogButtons :: ![WidgetNamePart]
    }

makeLensesWith postfixLFields ''DialogState

newDialogState :: Text -> DialogState
newDialogState dialogLabel = DialogState {..}
  where
    dialogButtons = []

addDialogButton :: Lens' s DialogState -> WidgetNamePart -> Text -> WidgetEventM s p () -> WidgetInitM s p
addDialogButton dialogStateL name buttonLabel action = do
    addWidgetChild name $ initButtonWidget buttonLabel
    addWidgetEventHandler name $ \case
        WidgetEventButtonPressed -> action
        _ -> pass
    widgetParentLens (dialogStateL . dialogButtonsL) %= (name :)

drawInsideDialog :: DialogState -> WidgetName -> B.Widget WidgetName -> WidgetDrawM s p WidgetDrawing
drawInsideDialog DialogState{..} focus widgetDrawing = do
    widget <- ask
    let drawChild = last . drawWidgetChild focus widget
    return $
        singleDrawing .
        B.centerLayer .
        B.withBorderStyle B.unicodeBold .
        withBorder .
        B.hLimit 70 .
        B.vLimit 20 $
        B.vBox
            [ B.withAttr "default" $ B.padAll 1 . B.padTopBottom 1 $ widgetDrawing
            , B.padLeftRight 1 . B.hBox . (B.vLimit 1 (B.fill ' ') :) $
              map drawChild dialogButtons
            ]
  where
    withBorder = if null dialogLabel
        then B.border
        else B.borderWithLabel (B.txt $ " " <> dialogLabel <> " ")
