module Ariadne.UI.Qt.Widgets.Wallet
       ( Wallet
       , initWallet
       , displayBlockchainInfo
       , doOnReplButtonClick
       , WalletEvent(..)
       , handleWalletEvent
       ) where

import Universum

import Control.Lens (magnify, makeLensesWith)
import Data.Tree (Tree(..))
import Graphics.UI.Qtah.Signal (connect_)
import IiExtras (postfixLFields)
import Serokell.Util (enumerate)

import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Core.QModelIndex as QModelIndex
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Gui.QStandardItem as QStandardItem
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.WalletInfo
import Ariadne.UI.Qt.Widgets.WalletTree

data Wallet =
  Wallet
    { layout :: QHBoxLayout.QHBoxLayout
    , walletTree :: WalletTree
    , walletInfo :: WalletInfo
    , itemModel :: QStandardItemModel.QStandardItemModel
    , selectionModel :: QItemSelectionModel.QItemSelectionModel
    , syncLabel :: QLabel.QLabel
    , replBtn :: QPushButton.QPushButton
    }

makeLensesWith postfixLFields ''Wallet

initWallet :: UiLangFace -> IO (QHBoxLayout.QHBoxLayout, Wallet)
initWallet langFace = do
  itemModel <- initItemModel
  selectionModel <- QItemSelectionModel.newWithModel itemModel

  (qWalletTree, walletTree) <- initWalletTree langFace itemModel selectionModel
  (qWalletInfo, walletInfo) <- initWalletInfo langFace itemModel selectionModel

  QObject.setObjectName qWalletTree ("walletTreeLayout" :: String)

  rightPaneLayout <- QVBoxLayout.new
  (statusLayout, syncLabel, replBtn) <- initStatusLayout
  QBoxLayout.addWidget rightPaneLayout qWalletInfo
  QBoxLayout.addLayout rightPaneLayout statusLayout
  QBoxLayout.setStretch rightPaneLayout 0 1
  QBoxLayout.setStretch rightPaneLayout 1 0

  layout <- QHBoxLayout.new
  QObject.setObjectName layout ("walletLayout" :: String)
  QBoxLayout.addLayout layout qWalletTree
  QBoxLayout.addLayout layout rightPaneLayout
  QBoxLayout.setStretch layout 0 200
  QBoxLayout.setStretch layout 1 1080

  connect_ selectionModel QItemSelectionModel.currentChangedSignal $
    currentChanged langFace Wallet{..}

  return (layout, Wallet{..})

initStatusLayout :: IO (QHBoxLayout.QHBoxLayout, QLabel.QLabel, QPushButton.QPushButton)
initStatusLayout = do
  statusLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw statusLayout 6 6 6 6
  QLayout.setSpacing statusLayout 6

  reportBugBtn <- QPushButton.newWithText ("BUG" :: String)
  QObject.setObjectName reportBugBtn ("reportBugBtn" :: String)
  QBoxLayout.addWidget statusLayout reportBugBtn

  QBoxLayout.addStretchOf statusLayout 1

  replBtn <- QPushButton.newWithText (">_" :: String)
  QObject.setObjectName replBtn ("replBtn" :: String)
  QBoxLayout.addWidget statusLayout replBtn

  syncLabel <- QLabel.newWithText ("Sync in progress" :: String)
  QObject.setObjectName syncLabel ("syncLabel" :: String)
  QBoxLayout.addWidget statusLayout syncLabel

  return (statusLayout, syncLabel, replBtn)

displayBlockchainInfo :: UiCardanoStatusUpdate -> UI Wallet ()
displayBlockchainInfo UiCardanoStatusUpdate{..} = do
  syncLabel <- view syncLabelL
  liftIO $ QWidget.setToolTip syncLabel $ toString $
    "Local: " <> blockchainLocal <> "\n"
    <> "Network: " <> blockchainNetwork

  liftIO $ QLabel.setText syncLabel $ toString $
    case syncProgress of
      Just progress ->
        "Sync in progress (" <> progress <> ")"
      Nothing ->
        "Blockhain synced"

doOnReplButtonClick :: IO () -> UI Wallet ()
doOnReplButtonClick handler = do
  replBtn <- view replBtnL
  void $ liftIO $ connect_ replBtn QAbstractButton.clickedSignal $ const handler

initItemModel :: IO QStandardItemModel.QStandardItemModel
initItemModel = do
  model <- QStandardItemModel.new
  QStandardItemModel.setHorizontalHeaderLabels model ["Wallets" :: String]

  item <- QStandardItem.new
  QStandardItem.setText item ("Loading..." :: String)
  QStandardItem.setSelectable item False
  QStandardItem.setEditable item False

  root <- QStandardItemModel.invisibleRootItem model
  QStandardItem.appendRowItem root item

  return model

currentChanged :: UiLangFace -> Wallet -> QModelIndex.QModelIndex -> QModelIndex.QModelIndex -> IO ()
currentChanged UiLangFace{..} Wallet{..} selected deselected = do
  isValid <- QModelIndex.isValid selected
  when (isValid && selected /= deselected) $ do
    item <- QStandardItemModel.itemFromIndex itemModel selected
    path <- fromQVariant =<< QStandardItem.getData item
    unless (null path) $ void $ langPutUiCommand $ UiSelect path

data WalletEvent
  = WalletUpdateEvent [UiWalletTree] (Maybe UiWalletTreeSelection) (Maybe UiSelectionInfo)
  | WalletBalanceCommandResult UiCommandId UiBalanceCommandResult
  | WalletSendCommandResult UiCommandId UiSendCommandResult
  | WalletNewWalletCommandResult UiCommandId UiNewWalletCommandResult
  | WalletNewAccountCommandResult UiCommandId UiNewAccountCommandResult
  | WalletNewAddressCommandResult UiCommandId UiNewAddressCommandResult

handleWalletEvent
  :: UiLangFace
  -> WalletEvent
  -> UI Wallet ()
handleWalletEvent langFace ev = do
  Wallet{..} <- ask
  case ev of
    WalletUpdateEvent wallets selection selectionInfo -> do
      lift $ updateModel itemModel selectionModel wallets selection
      magnify walletInfoL $ handleWalletInfoEvent langFace $
        WalletInfoSelectionChange selectionInfo
    WalletBalanceCommandResult commandId result ->
      magnify walletInfoL $ handleWalletInfoEvent langFace $
        WalletInfoBalanceCommandResult commandId result
    WalletSendCommandResult commandId result ->
      magnify walletInfoL $ handleWalletInfoEvent langFace $
        WalletInfoSendCommandResult commandId result
    WalletNewWalletCommandResult commandId result ->
      magnify walletTreeL $ handleWalletTreeEvent langFace $
        WalletTreeNewWalletCommandResult commandId result
    WalletNewAccountCommandResult commandId result ->
      magnify walletTreeL $ handleWalletTreeEvent langFace $
        WalletTreeNewAccountCommandResult commandId result
    WalletNewAddressCommandResult commandId result ->
      magnify walletTreeL $ handleWalletTreeEvent langFace $
        WalletTreeNewAddressCommandResult commandId result

updateModel
  :: QStandardItemModel.QStandardItemModel
  -> QItemSelectionModel.QItemSelectionModel
  -> [UiWalletTree]
  -> Maybe UiWalletTreeSelection
  -> IO ()
updateModel model selectionModel wallets selection = do
  root <- QStandardItemModel.invisibleRootItem model
  rootRowCount <- QStandardItem.rowCount root
  mapM_ (\(idx, item) -> toModelItem root rootRowCount idx (idx, item)) $ enumerate wallets
  QStandardItem.removeRows root (length wallets) (rootRowCount - length wallets)
  where
    selPath = (\UiWalletTreeSelection{..} -> wtsWalletIdx:wtsPath) <$> selection
    toModelItem
      :: QStandardItem.QStandardItem
      -> Int
      -> Int
      -> (Int, UiWalletTree)
      -> IO ()
    toModelItem parent parentRowCount walletIdx (idx, Node UiWalletTreeItem{..} children) = do
      let path = (fromIntegral walletIdx):wtiPath

      item <- if idx < parentRowCount
        then QStandardItem.child parent $ fromIntegral idx
        else do
          newItem <- QStandardItem.new
          QStandardItem.appendRowItem parent newItem
          return newItem

      QStandardItem.setText item $ toString $ fromMaybe "" wtiLabel
      QStandardItem.setData item =<< toQVariant path
      QStandardItem.setSelectable item True
      QStandardItem.setEditable item False

      itemRowCount <- QStandardItem.rowCount item
      mapM_ (toModelItem item itemRowCount walletIdx) $ enumerate children
      QStandardItem.removeRows item (length children) (itemRowCount - length children)

      when (selPath == Just path) $ do
        modelIndex <- QStandardItem.index item
        QItemSelectionModel.selectIndex selectionModel modelIndex QItemSelectionModel.SelectCurrent
