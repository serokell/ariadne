module Ariadne.UI.Qt.Widgets.Repl
       ( Repl
       , initRepl
       , handleReplEvent
       , toggleRepl
       ) where

import Universum

import Control.Lens (makeLensesWith)
import Formatting
import IiExtras

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Graphics.UI.Qtah.Core.Types
  (QtCursorShape(IBeamCursor), QtKey(..), alignTop, textSelectableByMouse)
import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Gui.QCursor as QCursor
import qualified Graphics.UI.Qtah.Gui.QKeyEvent as QKeyEvent
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
    { replWidget :: QWidget.QWidget
    , layout :: QVBoxLayout.QVBoxLayout
    , cmdLine :: QLineEdit.QLineEdit
    , historyWidget :: QScrollArea.QScrollArea
    , historyLayout :: QVBoxLayout.QVBoxLayout

    , commandOutputs :: IORef [CommandOutput]

    , beamCursor :: QCursor.QCursor
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

initRepl :: UiLangFace -> UiHistoryFace -> IO (QWidget.QWidget, Repl)
initRepl langFace historyFace = do
  cmdLine <- QLineEdit.new
  QObject.setObjectName cmdLine ("cmdLine" :: String)

  (historyWidget, historyLayout) <- initHistory
  layout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw layout 0 8 0 0
  QLayout.setSpacing layout 8
  QLayout.addWidget layout historyWidget

  cmdLineWidget <- QWidget.new
  QObject.setObjectName cmdLineWidget ("cmdLineWidget" :: String)
  cmdLineLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw cmdLineLayout 16 8 16 8
  QLayout.setSpacing cmdLineLayout 10
  QWidget.setLayout cmdLineWidget cmdLineLayout

  knitPrompt <- QLabel.newWithText ("knit>" :: String)
  QObject.setObjectName knitPrompt ("knitPrompt" :: String)

  QLayout.addWidget cmdLineLayout knitPrompt
  QLayout.addWidget cmdLineLayout cmdLine

  QBoxLayout.addWidget layout cmdLineWidget

  commandOutputs <- newIORef []

  beamCursor <- QCursor.newWithCursorShape IBeamCursor

  replWidget <- QWidget.new
  QObject.setObjectName replWidget ("repl" :: String)
  QWidget.setLayout replWidget layout
  QWidget.hide replWidget

  let repl = Repl{..}

  connectSignal repl cmdLine QLineEdit.returnPressedSignal $
    returnPressed langFace historyFace

  scrollBar <- QAbstractScrollArea.verticalScrollBar historyWidget
  connect_ scrollBar QAbstractSlider.rangeChangedSignal $
    \minValue maxValue -> runUI (scrollDown minValue maxValue) repl

  connect_ cmdLine QLineEdit.textEditedSignal $ historySetPrefix historyFace . fromString

  void $ Event.onEvent cmdLine $ handleKeyEvent historyFace cmdLine

  return (replWidget, repl)

initHistory :: IO (QScrollArea.QScrollArea, QVBoxLayout.QVBoxLayout)
initHistory = do
  scrollArea <- QScrollArea.new
  QObject.setObjectName scrollArea ("historyArea" :: String)

  historyWidget <- QWidget.new
  QObject.setObjectName historyWidget ("historyWidget" :: String)

  layout <- QVBoxLayout.new
  QWidget.setLayout historyWidget layout
  QLayout.setContentsMarginsRaw layout 0 0 0 0
  QLayout.setSizeConstraint layout QLayout.SetMinAndMaxSize

  QScrollArea.setWidget scrollArea historyWidget

  return (scrollArea, layout)

toggleRepl :: UI Repl ()
toggleRepl = do
  replWidget <- view replWidgetL
  cmdLine <- view cmdLineL
  liftIO $ do
    wasVisible <- QWidget.isVisible replWidget
    QWidget.setVisible replWidget $ not wasVisible

    unless wasVisible $ QWidget.setFocus cmdLine

createCommandOutput :: QCursor.QCursor -> UiCommandId -> String -> IO CommandOutput
createCommandOutput beamCursor commandId command = do
  let UiCommandId{..} = commandId

  coLayout <- QGridLayout.new
  QGridLayout.setColumnStretch coLayout 1 1
  QGridLayout.setRowStretch coLayout 1 1

  promptLabel <- QLabel.newWithText (">" :: String)
  statusLabel <- QLabel.newWithText $ toString $ format ("\x29D6\n" % stext) $ fromMaybe "" cmdTaskIdRendered
  commandLabel <- QLabel.newWithText command
  outputLabel <- QLabel.new

  QLabel.setTextInteractionFlags commandLabel textSelectableByMouse
  QLabel.setTextInteractionFlags outputLabel textSelectableByMouse

  QWidget.setCursor commandLabel beamCursor
  QWidget.setCursor outputLabel beamCursor

  QGridLayout.addWidgetWithSpanAndAlignment coLayout promptLabel 0 0 1 1 alignTop
  QGridLayout.addWidgetWithSpanAndAlignment coLayout statusLabel 1 0 1 1 alignTop
  QGridLayout.addWidgetWithSpanAndAlignment coLayout commandLabel 0 1 1 1 alignTop
  QGridLayout.addWidgetWithSpanAndAlignment coLayout outputLabel 1 1 1 1 alignTop

  return CommandOutput{..}

addNewCommand :: UiCommandId -> String -> UI Repl ()
addNewCommand commandId command = do
  Repl{..} <- ask
  liftIO $ do
    commandOutput <- createCommandOutput beamCursor commandId command

    modifyIORef' commandOutputs (commandOutput:)
    QBoxLayout.addLayout historyLayout $ view coLayoutL commandOutput

returnPressed :: UiLangFace -> UiHistoryFace -> UI Repl ()
returnPressed UiLangFace{..} historyFace = do
  cmdLine <- view cmdLineL
  cmd <- liftIO $ QLineEdit.text cmdLine
  case langParse $ toText cmd of
    Left err -> displayParseFailure $ renderToHTML $ langPpParseError err
    Right expr -> do
      commandId <- liftIO $ langPutCommand expr
      void $ addNewCommand commandId $ renderToHTML $ langPpExpr expr

      liftIO $ historyAddCommand historyFace $ fromString cmd

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

handleKeyEvent :: UiHistoryFace -> QLineEdit.QLineEdit -> QKeyEvent.QKeyEvent -> IO Bool
handleKeyEvent historyFace cmdLine event = do
  eventType <- QEvent.eventType event
  if eventType == QEvent.KeyRelease
    then QKeyEvent.key event >>= handleKey historyFace cmdLine . toEnum
    else return False

handleKey :: UiHistoryFace -> QLineEdit.QLineEdit -> QtKey -> IO Bool
handleKey historyFace cmdLine key = do
  case keyToAction key of
    Just action -> do
      historyItem <- fromMaybe "" <$> action historyFace
      QLineEdit.setText cmdLine $ toString historyItem
      return True
    Nothing -> return False

  where
    keyToAction = \case
      KeyDown -> Just historyNextCommand
      KeyUp -> Just historyPrevCommand
      _ -> Nothing
