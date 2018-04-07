module Ariadne.UI.Qt.MainWindow.Events
    ( uiEventHandler
    , displayNewLine
    , displayCommandInfo
    ) where

import Prelude (showChar, showString)
import Universum

import Formatting

import qualified System.Console.ANSI.Types as AT
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit

import Ariadne.UI.Qt.Face
  (UiCardanoEvent(..), UiCommandEvent(..), UiCommandId(..), UiEvent(..))

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.MainWindow.Ui

uiEventHandler :: UiEvent -> UI ()
uiEventHandler = \case
    UiCardanoEvent (UiCardanoStatusUpdateEvent update) -> displayBlockchainInfo update
    UiCommandEvent UiCommandId{..} (UiCommandSuccess doc) -> displayCommandInfo "" doc
    UiCommandEvent UiCommandId{..} (UiCommandFailure doc) -> displayCommandInfo "" doc
    _ -> return ()

displayBlockchainInfo :: UiCardanoStatusUpdate -> UI ()
displayBlockchainInfo (UiCardanoStatusUpdate {..}) = do
    blockchainInfo <- view blockchainInfoL
    liftIO $ QLabel.setText blockchainInfo $ toString $
        format ("Tip Hash: " % stext % " | Tip slot: " % stext % " | Current slot: " % stext)
            tipHeaderHash tipSlot currentSlot

displayNewLine :: String -> UI ()
displayNewLine str = do
    cmdHistory <- view $ replLayoutL . cmdHistoryL
    liftIO $ QTextEdit.insertHtml cmdHistory str

displayCommandInfo :: Text -> PP.Doc -> UI ()
displayCommandInfo cmdIdRendered doc = displayNewLine $ toString $
    format (stext % stext % "<br>")
        cmdIdRendered
        (simpleDocToHTML $ PP.renderPretty 0.985 120 doc)

getColor :: [AT.SGR] -> String
getColor xs = case colors of
  []    -> "black"
  (c:_) -> toColor c
  where
    toColor (AT.SetColor _ _ c) = case c of
      AT.Black   -> "white"
      AT.Red     -> "red"
      AT.Green   -> "green"
      AT.Yellow  -> "yellow"
      AT.Blue    -> "blue"
      AT.Magenta -> "magenta"
      AT.Cyan    -> "cyan"
      AT.White   -> "black"
    toColor _ = "black"

    colors = filter isColor xs

    isColor = \case
      AT.SetColor {} -> True
      _ -> False

simpleDocToHTML :: PP.SimpleDoc -> Text
simpleDocToHTML sdoc = toText $
    format ("<span style=\"white-space: pre-wrap;\"><span>" % string % "</span></span>") $ go sdoc ""
    where
        indentation i = if i <= 0 then "" else replicate i ' '
        go = \case
            PP.SFail -> error "simpleDocToHTML: impossible"
            PP.SEmpty -> identity
            PP.SChar c x -> showChar c . go x
            PP.SText _ s x -> showString s . go x
            PP.SLine i x -> showString "<br>" . showString (indentation i) . go x
            PP.SSGR s x -> showString ("</span><span style=\"color: " ++ getColor s ++ ";\">") . go x
