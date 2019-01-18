module Ariadne.UI.Qt.Widgets.Help
       ( Help
       , initHelp
       , showHelpWindow
       ) where

import qualified Data.Text as T

import Control.Lens (makeLensesWith)

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.AnsiToHTML
import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.Util

data Help =
  Help
    { dialog :: QDialog.QDialog
    , helpEdit :: QTextEdit.QTextEdit
    }

makeLensesWith postfixLFields ''Help

initHelp :: UiLangFace Qt -> IO (QDialog.QDialog, Help)
initHelp UiLangFace{..} = do
  dialog <- QDialog.new
  QWidget.setWindowTitle dialog ("Help" :: String)
  QWidget.resizeRaw dialog 800 600

  layout <- QVBoxLayout.newWithParent dialog

  helpEdit <- QTextEdit.new
  QTextEdit.setReadOnly helpEdit True
  QTextEdit.setHtml helpEdit $ toString $ T.intercalate "<br>" $
    (simpleDocToHTML . PP.renderPretty 0.985 200) <$> langGetHelp

  QLayout.addWidget layout helpEdit

  return (dialog, Help{..})

showHelpWindow :: UI Help ()
showHelpWindow = do
  dialog <- view dialogL

  liftIO $ QWidget.show dialog
