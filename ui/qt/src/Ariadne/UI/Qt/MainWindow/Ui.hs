module Ariadne.UI.Qt.MainWindow.Ui where

import Universum

import Control.Lens
import IiExtras

import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit

data ReplLayout =
    ReplLayout
    { cmdLine :: QLineEdit.QLineEdit
    , cmdHistory :: QTextEdit.QTextEdit
    }

data MainWindow =
    MainWindow
    { mwMainWindow :: QMainWindow.QMainWindow
    , blockchainInfo :: QLabel.QLabel
    , replLayout :: ReplLayout
    }

makeLensesWith postfixLFields ''ReplLayout
makeLensesWith postfixLFields ''MainWindow

type UI a = ReaderT MainWindow IO a

runUI :: UI a -> MainWindow -> IO a
runUI = runReaderT
