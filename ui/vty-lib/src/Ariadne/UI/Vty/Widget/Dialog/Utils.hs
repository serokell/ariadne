module Ariadne.UI.Vty.Widget.Dialog.Utils
       ( drawInsideDialog
       , addDialogButton
       ) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as B

import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button

drawInsideDialog :: Text -> WidgetName -> [WidgetNamePart] -> B.Widget WidgetName -> WidgetDrawM s p WidgetDrawing
drawInsideDialog label focus childNames widgetDrawing = do
    widget <- ask
    let drawChild = last . drawWidgetChild focus widget
    return $
        singleDrawing .
        B.centerLayer .
        B.withBorderStyle B.unicodeBold .
        B.borderWithLabel (B.txt $ " " <> label <> " ") .
        B.hLimit 70 .
        B.vLimit 20 $
        B.vBox
            [ B.padLeftRight 2 . B.padTopBottom 1 $ widgetDrawing
            , B.padLeftRight 1 . B.hBox . (B.vLimit 1 (B.fill ' ') :) $ map drawChild childNames
            ]

addDialogButton :: WidgetNamePart -> Text -> WidgetEventM s p () -> WidgetInitM s p
addDialogButton name title action = do
    addWidgetChild name $ initButtonWidget title
    addWidgetEventHandler name $ \case
        WidgetEventButtonPressed -> action
        _ -> pass
