module Ariadne.UI.Qt.MainWindow.Slots
    ( replReturnPressed
    ) where

import Universum

import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.MainWindow.Events
import Ariadne.UI.Qt.MainWindow.Ui

replReturnPressed :: UiLangFace -> UI ()
replReturnPressed UiLangFace{..} = do
    cmdLine <- view $ replLayoutL . cmdLineL
    cmd <- liftIO $ QLineEdit.text cmdLine
    case langParse $ toText cmd of
      Left err -> displayCommandInfo " " $ langPpParseError err
      Right expr -> do
          UiCommandId{..} <- liftIO $ langPutCommand expr
          displayCommandInfo "&gt; " $ langPpExpr expr
          liftIO $ QLineEdit.clear cmdLine
