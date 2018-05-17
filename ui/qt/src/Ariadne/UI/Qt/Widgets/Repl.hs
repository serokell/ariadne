module Ariadne.UI.Qt.Widgets.Repl
       ( Repl
       , initRepl
       , handleReplEvent
       ) where

import Universum

import Control.Lens (makeLensesWith)
import Formatting
import IiExtras

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Graphics.UI.Qtah.Core.Types
  (QtCursorShape(IBeamCursor), alignTop, textSelectableByMouse)
import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Core.QPalette as QPalette
import qualified Graphics.UI.Qtah.Widgets.QAbstractScrollArea as QAbstractScrollArea
import qualified Graphics.UI.Qtah.Widgets.QAbstractSlider as QAbstractSlider
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QGridLayout as QGridLayout
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QScrollArea as QScrollArea
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.AnsiToHTML
import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

data Repl =
  Repl
    { layout :: QVBoxLayout.QVBoxLayout
    , cmdLine :: QLineEdit.QLineEdit
    , historyWidget :: QScrollArea.QScrollArea
    , historyLayout :: QVBoxLayout.QVBoxLayout

    , commandOutputs :: IORef [CommandOutput]
    }

data CommandOutput =
  CommandOutput
    { coLayout :: QGridLayout.QGridLayout
    , commandId :: UiCommandId
    , promptLabel :: QLabel.QLabel
    , statusLabel :: QLabel.QLabel
    , commandLabel :: QLabel.QLabel
    , outputLabel :: QLabel.QLabel
    }

makeLensesWith postfixLFields ''Repl
makeLensesWith postfixLFields ''CommandOutput

initRepl :: UiLangFace -> IO (QVBoxLayout.QVBoxLayout, Repl)
initRepl langFace = do
  cmdLine <- QLineEdit.new

  (historyWidget, historyLayout) <- initHistory
  layout <- QVBoxLayout.new
  QLayout.addWidget layout historyWidget

  cmdLineLayout <- QHBoxLayout.new
  knitPrompt <- QLabel.newWithText ("knit>" :: String)
  QLayout.addWidget cmdLineLayout knitPrompt
  QLayout.addWidget cmdLineLayout cmdLine

  QBoxLayout.addLayout layout cmdLineLayout

  commandOutputs <- newIORef []

  let repl = Repl{..}

  connectSignal repl cmdLine QLineEdit.returnPressedSignal $
    returnPressed langFace

  scrollBar <- QAbstractScrollArea.verticalScrollBar historyWidget
  liftIO $ connect_ scrollBar QAbstractSlider.rangeChangedSignal $
    \minValue maxValue -> runUI (scrollDown minValue maxValue) repl

  QWidget.setFocus cmdLine

  return (layout, repl)

initHistory :: IO (QScrollArea.QScrollArea, QVBoxLayout.QVBoxLayout)
initHistory = do
  scrollArea <- QScrollArea.new
  QWidget.setBackgroundRole scrollArea QPalette.Base

  historyWidget <- QWidget.new
  QWidget.setContentsMarginsRaw historyWidget 0 0 0 0

  layout <- QVBoxLayout.new
  QWidget.setLayout historyWidget layout
  QLayout.setSizeConstraint layout QLayout.SetMinAndMaxSize

  QScrollArea.setWidget scrollArea historyWidget

  return (scrollArea, layout)

createCommandOutput :: UiCommandId -> String -> IO CommandOutput
createCommandOutput commandId command = do
  let UiCommandId{..} = commandId

  coLayout <- QGridLayout.new
  QGridLayout.setColumnStretch coLayout 1 1
  QGridLayout.setRowStretch coLayout 1 1

  promptLabel <- QLabel.newWithText (">" :: String)
  statusLabel <- QLabel.newWithText $ toString $ format ("\x29D6\n" % stext) $ fromMaybe "" cmdIdRendered
  commandLabel <- QLabel.newWithText command
  outputLabel <- QLabel.new

  QLabel.setTextInteractionFlags commandLabel textSelectableByMouse
  QLabel.setTextInteractionFlags outputLabel textSelectableByMouse

  QWidget.setCursor commandLabel IBeamCursor
  QWidget.setCursor outputLabel IBeamCursor

  QGridLayout.addWidgetWithSpanAndAlignment coLayout promptLabel 0 0 1 1 alignTop
  QGridLayout.addWidgetWithSpanAndAlignment coLayout statusLabel 1 0 1 1 alignTop
  QGridLayout.addWidgetWithSpanAndAlignment coLayout commandLabel 0 1 1 1 alignTop
  QGridLayout.addWidgetWithSpanAndAlignment coLayout outputLabel 1 1 1 1 alignTop

  return CommandOutput{..}

addNewCommand :: UiCommandId -> String -> UI Repl ()
addNewCommand commandId command = do
  Repl{..} <- ask
  liftIO $ do
    commandOutput <- createCommandOutput commandId command

    modifyIORef' commandOutputs (commandOutput:)
    QBoxLayout.addLayout historyLayout $ view coLayoutL commandOutput

returnPressed :: UiLangFace -> UI Repl ()
returnPressed UiLangFace{..} = do
  cmdLine <- view cmdLineL
  cmd <- liftIO $ QLineEdit.text cmdLine
  case langParse $ toText cmd of
    Left err -> displayParseFailure $ renderToHTML $ langPpParseError err
    Right expr -> do
      commandId <- liftIO $ langPutCommand expr
      void $ addNewCommand commandId $ renderToHTML $ langPpExpr expr

      liftIO $ QLineEdit.clear cmdLine

scrollDown :: Int -> Int -> UI Repl ()
scrollDown _ maxValue = do
  scrollBar <- view historyWidgetL >>= liftIO . QAbstractScrollArea.verticalScrollBar
  liftIO $ QAbstractSlider.setValue scrollBar maxValue

handleReplEvent :: UiCommandId -> UiCommandEvent -> UI Repl ()
handleReplEvent commandId event = do
  commandOutputs <- view commandOutputsL >>= liftIO . readIORef
  whenJust (find isThisCommand commandOutputs) $ liftIO . updateCommandResult event

  where
    isThisCommand CommandOutput{commandId = commandId'} = commandId == commandId'

updateCommandResult :: UiCommandEvent -> CommandOutput -> IO ()
updateCommandResult event CommandOutput{..} = do
  doc <- case event of
    UiCommandSuccess doc -> do
      QLabel.setText statusLabel ("\x2713" :: String)
      return doc
    UiCommandOutput doc -> return doc
    UiCommandFailure doc -> do
      QLabel.setText statusLabel ("\x274c" :: String)
      return doc

  oldText <- QLabel.text outputLabel
  QLabel.setText outputLabel $ appendLine oldText doc

appendLine :: String -> PP.Doc -> String
appendLine "" doc = renderToHTML doc
appendLine old doc
  | new == "" = old
  | otherwise = toString $ format (string % "<br>" % string) old new
  where
    new = renderToHTML doc

renderToHTML :: PP.Doc -> String
renderToHTML = toString . simpleDocToHTML . PP.renderPretty 0.985 120

displayParseFailure :: String -> UI Repl ()
displayParseFailure str = do
  historyLayout <- view historyLayoutL
  liftIO $ do
    newLine <- QLabel.newWithText str
    QLayout.addWidget historyLayout newLine
