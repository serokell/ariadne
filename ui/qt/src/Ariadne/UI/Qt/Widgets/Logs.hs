module Ariadne.UI.Qt.Widgets.Logs
    ( Logs
    , initLogs
    , displayLogMessage
    , showLogsWindow
    ) where

import Universum

import qualified Data.Text as T

import Formatting

import Control.Lens (makeLensesWith)
import IiExtras (postfixLFields)

import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI

data Logs =
  Logs
    { dialog :: QDialog.QDialog
    , logsEdit :: QTextEdit.QTextEdit
    }

makeLensesWith postfixLFields ''Logs

initLogs :: IO (QDialog.QDialog, Logs)
initLogs = do
  dialog <- QDialog.new
  QWidget.setWindowTitle dialog ("Logs" :: String)
  QWidget.resizeRaw dialog 1024 720

  layout <- QVBoxLayout.newWithParent dialog

  logsEdit <- QTextEdit.new
  QTextEdit.setReadOnly logsEdit True

  QLayout.addWidget layout logsEdit

  return (dialog, Logs{..})

showLogsWindow :: UI Logs ()
showLogsWindow = do
  dialog <- view dialogL

  liftIO $ QWidget.show dialog

displayLogMessage :: Text -> UI Logs ()
displayLogMessage message = do
    logsEdit <- view logsEditL
    liftIO $ QTextEdit.insertHtml logsEdit $ toString $
        format (spanFormat % "<br>") $ csiToHTML message
    liftIO $ QTextEdit.ensureCursorVisible logsEdit

csiToHTML :: Text -> Text
csiToHTML line = T.concat split
  where
    split = readString "" $ toString line

    readString :: String -> String -> [Text]
    readString acc ('\x1b':'[':xs) = (fromString $ reverse acc) : readAttr "" xs
    readString acc (x:xs) = readString (x:acc) xs
    readString acc [] = [fromString $ reverse acc]

    readAttr :: String -> String -> [Text]
    readAttr acc (x:xs)
      | x `elem` ['\x40'..'\x7e'] = (makeSpan $ csiToCSS x $ fromString $ reverse acc) : readString "" xs
      | otherwise = readAttr (x:acc) xs
      -- Just skip unsupported attributes, we don't want any garbage in HTML
    readAttr _ [] = []

    makeSpan :: Text -> Text
    makeSpan = toText . format ("</span><span style=\"" % stext % "\">")

    csiToCSS :: Char -> Text -> Text
    csiToCSS 'm' params = T.intercalate ";" $ sgnToColor <$> T.splitOn ";" params
    csiToCSS _ _ = ""

    sgnToColor = \case
      "" -> "color: "
      "0" -> "color: black"

      "30" -> "color: black"
      "31" -> "color: red"
      "32" -> "color: green"
      "33" -> "color: yellow"
      "34" -> "color: blue"
      "35" -> "color: magenta"
      "36" -> "color: cyan"
      "37" -> "color: white"

      -- gruvbox theme: https://github.com/morhetz/gruvbox
      "90" -> "color: #928374" -- brightBlack
      "91" -> "color: #9d0006" -- brightRed
      "92" -> "color: #79740e" -- brightGreen
      "93" -> "color: #b57614" -- brightYellow
      "94" -> "color: #076678" -- brightBlue
      "95" -> "color: #8f3f71" -- brightMagenta
      "96" -> "color: #417b58" -- brightCyan
      "97" -> "color: #3c3836" -- brightWhite

      _ -> ""
