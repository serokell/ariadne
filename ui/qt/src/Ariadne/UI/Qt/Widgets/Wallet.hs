module Ariadne.UI.Qt.Widgets.Wallet
       ( Wallet
       , initWallet
       , WalletEvent(..)
       , handleWalletEvent
       ) where

import Universum

import Control.Lens (makeLensesWith)
import Data.Tree (Tree(..))
import Graphics.UI.Qtah.Signal (connect_)
import IiExtras (postfixLFields)
import Serokell.Util (enumerate)

import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Core.QModelIndex as QModelIndex
import qualified Graphics.UI.Qtah.Core.QVariant as QVariant
import qualified Graphics.UI.Qtah.Gui.QStandardItem as QStandardItem
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QSplitter as QSplitter

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.WalletTree
import Ariadne.UI.Qt.Widgets.WalletInfo

data Wallet =
  Wallet
    { layout :: QSplitter.QSplitter
    , walletTree :: WalletTree
    , walletInfo :: WalletInfo
    , itemModel :: QStandardItemModel.QStandardItemModel
    , selectionModel :: QItemSelectionModel.QItemSelectionModel
    }

makeLensesWith postfixLFields ''Wallet

initWallet :: UiLangFace -> IO (QSplitter.QSplitter, Wallet)
initWallet langFace = do
  itemModel <- initItemModel
  selectionModel <- QItemSelectionModel.newWithModel itemModel

  (qWalletTree, walletTree) <- initWalletTree itemModel selectionModel
  (qWalletInfo, walletInfo) <- initWalletInfo itemModel selectionModel

  layout <- QSplitter.new
  QSplitter.addWidget layout qWalletTree
  QSplitter.addWidget layout qWalletInfo
  QSplitter.setStretchFactor layout 1 1
  QSplitter.setChildrenCollapsible layout False

  liftIO $ connect_ selectionModel QItemSelectionModel.currentChangedSignal $
    \selected deselected -> runUI (currentChanged langFace selected deselected) Wallet{..}

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

currentChanged :: UiLangFace -> QModelIndex.QModelIndex -> QModelIndex.QModelIndex -> UI Wallet ()
currentChanged UiLangFace{..} selected deselected = unless (selected == deselected) $ do
  Wallet{..} <- ask
  lift $ do
    item <- QStandardItemModel.itemFromIndex itemModel selected
    path <- mapM QVariant.toUInt =<< QVariant.toList =<< QStandardItem.getData item
    unless (null path) $ void $ liftIO $ langPutCommand $ langMkExpr $ UiSelect $ fmap fromIntegral path

data WalletEvent
  = WalletUpdateEvent [UiWalletTree] (Maybe UiWalletTreeSelection)

handleWalletEvent
  :: WalletEvent
  -> UI Wallet ()
handleWalletEvent ev = do
  Wallet{..} <- ask
  liftIO $ case ev of
    WalletUpdateEvent wallets selection -> do
      updateModel itemModel selectionModel wallets selection

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
      QStandardItem.setData item =<< QVariant.newWithList =<< mapM (QVariant.newWithUInt . fromIntegral) path
      QStandardItem.setSelectable item True
      QStandardItem.setEditable item False

      itemRowCount <- QStandardItem.rowCount item
      mapM_ (toModelItem item itemRowCount walletIdx) $ enumerate children
      QStandardItem.removeRows item (length children) (itemRowCount - length children)

      when (selPath == Just path) $ do
        modelIndex <- QStandardItem.index item
        QItemSelectionModel.selectIndex selectionModel modelIndex QItemSelectionModel.SelectCurrent