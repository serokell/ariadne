module Ariadne.UI.Qt.Widgets.Dialogs.Validation
  ( Validations
  , createValidations
  , showErrors
  , showErrorsV
  )
  where

import Data.Bifoldable (bitraverse_)
import qualified Data.Map.Strict as Map
import Data.Validation (Validation)

import Graphics.UI.Qtah.Core.HPoint (HPoint(..))
import Graphics.UI.Qtah.Core.HSize (HSize(..))
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util

data ValidationDisplay =
  ValidationDisplay
    { display :: QWidget.QWidget
    , label :: QLabel.QLabel
    , widget :: QWidget.QWidget
    , hasError :: IORef Bool
    , hasFocus :: IORef Bool
    }

createValidationErrorDisplay :: QWidget.QWidgetPtr wgt => wgt -> Text -> IO ValidationDisplay
createValidationErrorDisplay (QWidget.cast -> widget) message = do
  display <- QWidget.new
  layout <- QVBoxLayout.new
  QWidget.setLayout display layout
  label <- QLabel.newWithText $ toString message
  QBoxLayout.addWidget layout label

  QLabel.setWordWrap label True
  QWidget.setSizePolicyRaw label Minimum Minimum
  QLayout.setSizeConstraint layout QLayout.SetMinimumSize

  setProperty display ("styleRole" :: Text) ("validationErrorDisplay" :: Text)

  hasError <- newIORef False
  hasFocus <- newIORef False

  return ValidationDisplay{..}

attachValidationDisplay ::
  (QWidget.QWidgetPtr wnd) =>
  wnd -> ValidationDisplay -> IO ()
attachValidationDisplay window vd@ValidationDisplay{..} = do
  QWidget.setParent display window

  connect_ window QObject.destroyedSignal $ \_ -> QObject.deleteLater display

  QWidget.adjustSize display
  QWidget.raise display

  onEventType window $ \case
    QEvent.Move -> moveDisplay vd
    QEvent.Resize -> moveDisplay vd
    QEvent.Hide -> QWidget.hide display
    QEvent.Show -> do
      hadError <- readIORef hasError
      hadFocus <- readIORef hasFocus

      QWidget.setVisible display $ hadError && not hadFocus
    _ -> pass
  onEventType widget $ \case
    QEvent.FocusIn -> toggleFocus True
    QEvent.FocusOut -> toggleFocus False
    _ -> pass

  where
    toggleFocus setHasFocus = do
      vHasError <- readIORef hasError
      when vHasError $ QWidget.setVisible display $ not setHasFocus
      writeIORef hasFocus setHasFocus

moveDisplay :: ValidationDisplay -> IO ()
moveDisplay ValidationDisplay{..} = do
  HSize{width = widgetWidth, height = widgetHeight} <- QWidget.size widget
  HSize{height = displayHeight} <- QWidget.size display

  QWidget.resize display $ HSize{width = 2 * widgetWidth, height = displayHeight}
  QWidget.adjustSize label
  QWidget.adjustSize display

  HSize{width = displayWidth'} <- QWidget.size display
  HPoint{x = widgetX, y = widgetY} <- QWidget.pos widget

  let displayPos = HPoint{x = widgetX + widgetWidth - displayWidth', y = widgetY + widgetHeight + 4}
  QWidget.move display displayPos

setMessage :: ValidationDisplay -> Text -> IO ()
setMessage vd@ValidationDisplay{..} message = do
  QLabel.setText label $ toString message
  moveDisplay vd

showError :: ValidationDisplay -> Bool -> IO ()
showError ValidationDisplay{..} newHasError = do
  hadFocus <- readIORef hasFocus
  writeIORef hasError newHasError
  unless hadFocus $ do
    QWidget.setVisible display newHasError
    QWidget.raise display

newtype Validations = Validations (Map.Map QWidget.QWidget ValidationDisplay)

newValidations :: Validations
newValidations = Validations Map.empty

addValidation :: QWidget.QWidgetPtr wnd => Validations -> Text -> wnd -> QWidget.QWidget -> IO Validations
addValidation (Validations vals) message window widget = do
  validation <- createValidationErrorDisplay widget message
  attachValidationDisplay window validation

  return $ Validations $ Map.insert widget validation vals

createValidations :: QWidget.QWidgetPtr wnd => wnd -> [(Text, QWidget.QWidget)] -> IO Validations
createValidations window =
  foldlM (\vals (message, widget) -> addValidation vals message window widget) newValidations

showErrors :: Validations -> [(Maybe Text, QWidget.QWidget)] -> IO ()
showErrors (Validations vals) errors = do
  forM_ (toList vals) $ flip showError False
  forM_ errors $ \(mtext, err) -> do
    let val = vals Map.! err
    showError val True
    whenJust mtext $ setMessage val

showErrorsV :: Validations -> Validation [e] a -> (e -> Maybe (Maybe Text, QWidget.QWidget)) -> IO ()
showErrorsV vals validation renderError =
  bitraverse_
    (showErrors vals . mapMaybe renderError)
    (const $ showErrors vals [])
    validation
