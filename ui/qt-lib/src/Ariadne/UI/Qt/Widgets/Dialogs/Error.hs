module Ariadne.UI.Qt.Widgets.Dialogs.Error
  ( runErrorDialog
  ) where

import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UI.Qt.UI

initErrorDialog :: (QWidget.QWidgetPtr parent) => parent -> String -> IO QDialog.QDialog
initErrorDialog parent message = do
  errorDialog <- QDialog.newWithParent parent
  layout <- createLayout errorDialog
  QWidget.resizeRaw errorDialog 556 72
  QWidget.setWindowTitle errorDialog ("Error" :: String)

  headerLabel <- QLabel.newWithText ("ERROR" :: String)
  addHeader layout headerLabel

  infoLabel <- QLabel.newWithText message
  QLabel.setWordWrap infoLabel True
  QWidget.setMinimumWidth infoLabel 500
  QBoxLayout.addWidget layout infoLabel

  buttonLayout <- QHBoxLayout.new
  closeButton <- QPushButton.newWithText ("CLOSE" :: String)
  setProperty closeButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty closeButton ("styleRole" :: Text) ("dangerButton" :: Text)
  QBoxLayout.addStretch buttonLayout
  QBoxLayout.addWidget buttonLayout closeButton
  QBoxLayout.addStretch buttonLayout

  QBoxLayout.addLayout layout buttonLayout

  connect_ closeButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept errorDialog
  QWidget.adjustSize errorDialog
  return errorDialog

runErrorDialog :: (QWidget.QWidgetPtr parent) => parent -> String -> IO ()
runErrorDialog parent message = do
  errorDialog <- initErrorDialog parent message
  void $ QDialog.exec errorDialog
