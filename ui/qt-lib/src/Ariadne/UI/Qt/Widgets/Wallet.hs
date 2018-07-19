module Ariadne.UI.Qt.Widgets.Wallet
       ( Wallet
       , initWallet
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
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout

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
    }

makeLensesWith postfixLFields ''Wallet

initWallet :: UiLangFace -> IO (QHBoxLayout.QHBoxLayout, Wallet)
initWallet langFace = do
  itemModel <- initItemModel
  selectionModel <- QItemSelectionModel.newWithModel itemModel

  (qWalletTree, walletTree) <- initWalletTree langFace itemModel selectionModel
  (qWalletInfo, walletInfo) <- initWalletInfo langFace itemModel selectionModel

  layout <- QHBoxLayout.new
  QObject.setObjectName layout ("walletLayout" :: String)
  QBoxLayout.addWidget layout qWalletTree
  QBoxLayout.addWidget layout qWalletInfo
  QBoxLayout.setStretch layout 0 200
  QBoxLayout.setStretch layout 1 1080

  connect_ selectionModel QItemSelectionModel.currentChangedSignal $
    currentChanged langFace Wallet{..}

  return (layout, Wallet{..})

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
      whenJust selectionInfo $
        magnify walletInfoL . handleWalletInfoEvent langFace .
          WalletInfoSelectionChange
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
