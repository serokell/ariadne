module Ariadne.UI.Qt.Widgets.Dialogs.Delete
       ( DeletionResult(..)
       , runDelete
       ) where

import qualified Data.Text as T
import Formatting

import Graphics.UI.Qtah.Signal (connect_)

import Graphics.UI.Qtah.Core.Types (QtArrowType(..))
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QScrollArea as QScrollArea
import qualified Graphics.UI.Qtah.Widgets.QSizePolicy as QSizePolicy
import qualified Graphics.UI.Qtah.Widgets.QToolButton as QToolButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UIConfig

data DeletionResult = DoDelete | Cancel deriving Eq

data Delete =
  Delete
    { delete :: QDialog.QDialog
    , isSure :: QCheckBox.QCheckBox
    , retypeWidget :: QWidget.QWidget
    , retypeName :: QLineEdit.QLineEdit
    , deleteButton :: QPushButton.QPushButton
    , deleteUnknownObjects :: Maybe (QToolButton.QToolButton,QScrollArea.QScrollArea)
    , itemType :: UiDeletingItem
    }

data MessagesOnDelWidget =
  MessagesOnDelWidget
    { header          :: !String
    , intro           :: !String
    , isSureMessage   :: !Text
    , reTypeMessage   :: !String
    , expandingMessge :: !Text
    , confirmMsg      :: !Text
    }

makeMessages :: UiDeletingItem -> MessagesOnDelWidget
makeMessages (UiDelUnknownKeys _) =
  let header  = toString rmUnkownKeysHeaderMessage
      intro = toString $ rmUnknownKeysIntroMessage
      expandingMessge = expandingMessageRemovingKeys
      isSureMessage = rmUnknownKeysSureMessage
      reTypeMessage = toString $
        rmUnknownKeysRetypeMkMessage ("<b>" <> rmUnknownKeysRetypeConfirm <> "</b>")
      confirmMsg = rmUnknownKeysRetypeConfirm
  in MessagesOnDelWidget{..}
makeMessages (UiDelBrokenWallets _) =
  let header  = toString rmBrokenWalletsHeaderMessage
      intro = toString $ rmBrokenWalletsIntroMkMessage
      expandingMessge = expandingMessageRemovingWallets
      isSureMessage = rmBrokenWalletDelSureMessage
      reTypeMessage = toString $
        rmBrokenWltRetypeMkMessage ("<b>" <> rmBrokenRetypeConfirm <> "</b>")
      confirmMsg = rmBrokenRetypeConfirm
  in MessagesOnDelWidget{..}
makeMessages itemType =
  let header = toString . T.toUpper $ deleteHeaderMkMessage itemTypeFormat itemType
      itemName = fromMaybe "this" $ itemTypeName itemType
      intro = toString $
        deleteIntroMkMessage itemTypeFormat ("<b>" <> itemName <> "</b>") itemType
      expandingMessge = ""
      isSureMessage = deleteSureMkMessage itemTypeFormat itemType
      reTypeMessage = toString $ deleteRetypeMkMessage itemTypeFormat itemType
      confirmMsg = itemName
  in MessagesOnDelWidget{..}

initDelete :: UiDeletingItem -> IO Delete
initDelete itemType = do
  delete <- QDialog.new
  layout <- createLayout delete
  let MessagesOnDelWidget{..} = makeMessages itemType

  QWidget.setWindowTitle delete header

  headerWidget <- QLabel.newWithText header
  addHeader layout headerWidget

  warningLabel <- QLabel.newWithText intro
  QBoxLayout.addWidget layout warningLabel

  deleteUnknownObjects <- case itemType of
    (UiDelUnknownKeys rootKeysText) ->
      showDeletingObjects rootKeysText expandingMessge layout
    (UiDelBrokenWallets brokenWalletNames) ->
      showDeletingObjects brokenWalletNames expandingMessge layout
    _ -> return Nothing
  isSure <- createCheckBox layout CheckboxOnLeft $ isSureMessage

  (retypeWidget, retypeLayout) <- createSubWidget
  addSeparator retypeLayout

  retypeLabel <- QLabel.newWithText reTypeMessage
  retypeName <- QLineEdit.new
  addRow retypeLayout retypeLabel retypeName

  QBoxLayout.addWidget layout retypeWidget
  QWidget.hide retypeWidget

  buttonsLayout <- QHBoxLayout.new
  cancelButton <- QPushButton.newWithText ("CANCEL" :: String)
  deleteButton <- QPushButton.newWithText ("DELETE" :: String)
  QBoxLayout.addWidget buttonsLayout cancelButton
  QBoxLayout.addWidget buttonsLayout deleteButton
  QBoxLayout.addLayout layout buttonsLayout

  setProperty cancelButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty deleteButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty cancelButton ("styleRole" :: Text) ("secondaryButton" :: Text)
  setProperty deleteButton ("styleRole" :: Text) ("dangerButton" :: Text)

  let del = Delete{..}

  case deleteUnknownObjects of
    (Just (unknownObjectsExpand,_)) -> connect_ unknownObjectsExpand QAbstractButton.toggledSignal $
                                   unknownObjectsToggled del
    _ -> pass
  connect_ isSure QAbstractButton.toggledSignal $ isSureToggled del
  connect_ retypeName QLineEdit.textChangedSignal $ \_ -> revalidate del
  connect_ cancelButton QAbstractButton.clickedSignal $ \_ -> QDialog.reject delete
  connect_ deleteButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept delete

  revalidate del

  return del

runDelete :: UiDeletingItem -> IO DeletionResult
runDelete itemType = do
  del@Delete{delete = delete} <- initDelete itemType
  result <- toEnum <$> QDialog.exec delete
  valid <- isValid del

  return $ case result of
    QDialog.Accepted -> if valid then DoDelete else Cancel
    QDialog.Rejected -> Cancel

isValid :: Delete -> IO Bool
isValid Delete{..} = do
  delIsSure <- QAbstractButton.isChecked isSure
  delItemName <- T.strip . fromString <$> QLineEdit.text retypeName

  -- We do not ask to retype name for accounts
  -- (or anything that does not have a name)
  let isNameChecked = case itemType of
        UiDelAccount _ -> True
        UiDelWallet maybeName -> case maybeName of
          Nothing -> True
          Just itemName -> delItemName == itemName
        UiDelUnknownKeys _ -> (T.toTitle delItemName) == "Yes"
        UiDelBrokenWallets _ -> (T.toTitle delItemName) == "Yes"
  return $ delIsSure && isNameChecked

shouldCheck :: UiDeletingItem -> Bool
shouldCheck = \case
  UiDelWallet _ -> True
  UiDelUnknownKeys _ -> True
  UiDelBrokenWallets _  -> True
  _ -> False

revalidate :: Delete -> IO ()
revalidate del@Delete{..} = isValid del >>= QWidget.setEnabled deleteButton

itemTypeFormat :: Format r (UiDeletingItem -> r)
itemTypeFormat = later $ \case
  UiDelWallet _ -> "wallet"
  UiDelAccount _ -> "account"
  UiDelUnknownKeys _ -> "keys"
  UiDelBrokenWallets _ -> "wallets"

itemTypeName :: UiDeletingItem -> Maybe Text
itemTypeName = \case
  UiDelWallet name -> name
  UiDelAccount name -> name
  UiDelUnknownKeys _ -> Just "DelUnknownKeys"
  UiDelBrokenWallets _ -> Just "DelBrokenWallets"

isSureToggled :: Delete -> Bool -> IO ()
isSureToggled del@Delete{..} checked = do
  revalidate del
  when (shouldCheck itemType && isJust (itemTypeName itemType)) $ do
    QWidget.setVisible retypeWidget checked
    QWidget.adjustSize delete

unknownObjectsToggled :: Delete -> Bool -> IO ()
unknownObjectsToggled Delete{..} expanded = case deleteUnknownObjects of
  Just (unknownObjectsExpand,unknownObjectsScroll) -> do
    QToolButton.setArrowType unknownObjectsExpand $ bool RightArrow DownArrow expanded
    QWidget.setVisible unknownObjectsScroll expanded
    QWidget.adjustSize delete
  _ -> pass

showDeletingObjects
  :: Text
  -> Text
  -> QVBoxLayout.QVBoxLayout
  -> IO (Maybe (QToolButton.QToolButton, QScrollArea.QScrollArea))
showDeletingObjects listOfDeletingObjects expandingLabel layout = do
  deletingObjectsExpand <- createToolButton layout expandingLabel
  QToolButton.setArrowType deletingObjectsExpand RightArrow
  unknownObjects <- QLabel.newWithText $ toString listOfDeletingObjects
  unknownObjectsScroll <- QScrollArea.new
  QScrollArea.setWidget unknownObjectsScroll unknownObjects
  QWidget.setMaximumSizeRaw unknownObjectsScroll 1200 75
  QWidget.setSizePolicyRaw unknownObjectsScroll QSizePolicy.Preferred QSizePolicy.Fixed
  QBoxLayout.addWidget layout unknownObjectsScroll
  QWidget.hide unknownObjectsScroll
  QAbstractButton.setCheckable deletingObjectsExpand True
  return $ Just (deletingObjectsExpand,unknownObjectsScroll)
