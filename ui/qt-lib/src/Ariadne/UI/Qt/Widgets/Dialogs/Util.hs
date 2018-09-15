module Ariadne.UI.Qt.Widgets.Dialogs.Util where

import Data.Bits

import Graphics.UI.Qtah.Core.Types (alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import qualified Graphics.UI.Qtah.Gui.QMouseEvent as QMouseEvent
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QFrame as QFrame
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI

data CheckboxPosition = CheckboxOnLeft | CheckboxOnRight

createLayout :: (QWidget.QWidgetPtr wgt) => wgt -> IO QVBoxLayout.QVBoxLayout
createLayout widget = do
  layout <- QVBoxLayout.new
  QWidget.setLayout widget layout
  QBoxLayout.setSpacing layout 18
  QLayout.setContentsMarginsRaw layout 24 24 24 24

  return layout

addHeader :: (QWidget.QWidgetPtr hdr)
          => QVBoxLayout.QVBoxLayout -> hdr -> IO ()
addHeader layout header = do
  QBoxLayout.addWidget layout header
  void $ QLayout.setWidgetAlignment layout header $ alignHCenter .|. alignVCenter
  QBoxLayout.addSpacing layout 24

  setProperty header ("styleRole" :: Text) ("dialogHeader" :: Text)

createRowLayout :: IO QHBoxLayout.QHBoxLayout
createRowLayout = do
  rowLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw rowLayout 18 0 18 0

  return rowLayout

internalAddRow :: (QWidget.QWidgetPtr lbl)
               => QVBoxLayout.QVBoxLayout -> lbl -> (QHBoxLayout.QHBoxLayout -> IO ()) -> IO ()
internalAddRow layout label widgetAdder = do
  rowLayout <- createRowLayout
  QBoxLayout.addWidget rowLayout label
  widgetAdder rowLayout
  QBoxLayout.setStretch rowLayout 0 2
  QBoxLayout.setStretch rowLayout 1 1

  QBoxLayout.addLayout layout rowLayout

addRow :: (QWidget.QWidgetPtr lbl, QWidget.QWidgetPtr wgt)
       => QVBoxLayout.QVBoxLayout -> lbl -> wgt -> IO ()
addRow layout label widget =
  internalAddRow layout label $ flip QBoxLayout.addWidget widget

addRowLayout :: (QWidget.QWidgetPtr lbl, QLayout.QLayoutPtr lay)
       => QVBoxLayout.QVBoxLayout -> lbl -> lay -> IO ()
addRowLayout layout label widgetLayout =
  internalAddRow layout label $ flip QBoxLayout.addLayout widgetLayout

addSeparator :: QVBoxLayout.QVBoxLayout -> IO ()
addSeparator layout = do
  separator <- QFrame.new
  QFrame.setFrameShape separator QFrame.HLine
  setProperty separator ("styleRole" :: Text) ("separator" :: Text)
  setProperty separator ("orientation" :: Text) ("horizontal" :: Text)

  QBoxLayout.addWidget layout separator

createSubWidget :: IO (QWidget.QWidget, QVBoxLayout.QVBoxLayout)
createSubWidget = do
  widget <- QWidget.new
  layout <- QVBoxLayout.new
  QWidget.setLayout widget layout
  QLayout.setContentsMarginsRaw layout 0 0 0 0
  QBoxLayout.setSpacing layout 18
  QWidget.setSizePolicyRaw widget Preferred Minimum
  QLayout.setSizeConstraint layout QLayout.SetMinimumSize

  return (widget, layout)

createCheckBoxWithLabel :: Text -> IO (QCheckBox.QCheckBox, QLabel.QLabel)
createCheckBoxWithLabel labelText = do
  checkBox <- QCheckBox.new
  QWidget.setSizePolicyRaw checkBox Maximum Maximum
  checkBoxLabel <- QLabel.newWithText $ toString labelText
  QLabel.setWordWrap checkBoxLabel True

  void $ Event.onEvent checkBoxLabel $
    \(ev :: QMouseEvent.QMouseEvent) -> do
      eventType <- QEvent.eventType ev
      if eventType == QEvent.MouseButtonRelease
        then QAbstractButton.toggle checkBox $> True
        else return False

  return (checkBox, checkBoxLabel)

createCheckBox
  :: QVBoxLayout.QVBoxLayout
  -> CheckboxPosition
  -> Text
  -> IO QCheckBox.QCheckBox
createCheckBox layout checkBoxPos labelText = do
  (checkBox, checkBoxLabel) <- createCheckBoxWithLabel labelText
  QWidget.setMinimumSizeRaw checkBoxLabel 600 30

  checkBoxLayout <- QHBoxLayout.new
  case checkBoxPos of
    CheckboxOnLeft -> do
      QBoxLayout.addWidget checkBoxLayout checkBox
      QBoxLayout.addWidget checkBoxLayout checkBoxLabel
    CheckboxOnRight -> do
      QBoxLayout.addWidget checkBoxLayout checkBoxLabel
      QBoxLayout.addWidget checkBoxLayout checkBox

  QBoxLayout.addLayout layout checkBoxLayout

  return checkBox

createPasswordField :: Text -> IO (QHBoxLayout.QHBoxLayout, QLineEdit.QLineEdit)
createPasswordField placeholder = do
    layout <- QHBoxLayout.new
    field <- QLineEdit.new
    QLineEdit.setEchoMode field QLineEdit.Password
    QLineEdit.setPlaceholderText field $ toString placeholder

    visibleButton <- QPushButton.new
    setProperty visibleButton ("styleRole" :: Text) ("passwordVisibilityToggle" :: Text)
    QAbstractButton.setIcon visibleButton =<< QIcon.newWithFile (":/images/hide-pass-ic.png" :: String)
    QAbstractButton.setCheckable visibleButton True
    QWidget.setSizePolicyRaw visibleButton Maximum Maximum

    QBoxLayout.addWidget layout field
    QBoxLayout.addWidget layout visibleButton
    QLayout.setSpacing layout 12
    QBoxLayout.setStretch layout 0 1
    QBoxLayout.setStretch layout 1 0

    let
      togglePasswordVisibility checked = QLineEdit.setEchoMode field $
          if checked then QLineEdit.Normal else QLineEdit.Password

    connect_ visibleButton QAbstractButton.toggledSignal togglePasswordVisibility

    return (layout, field)
