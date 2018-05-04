module Ariadne.UI.Qt.Widgets.Wallet
       ( Wallet
       , initWallet
       , WalletEvent(..)
       , handleWalletEvent
       ) where

import Universum

import Control.Lens (makeLensesWith)
import Data.Tree (Tree(..))
import IiExtras (postfixLFields)

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
    }

makeLensesWith postfixLFields ''Wallet

initWallet :: UiLangFace -> IO (QSplitter.QSplitter, Wallet)
initWallet _langFace = do
  itemModel <- initItemModel

  (qWalletTree, walletTree) <- initWalletTree itemModel
  (qWalletInfo, walletInfo) <- initWalletInfo itemModel

  layout <- QSplitter.new
  QSplitter.addWidget layout qWalletTree
  QSplitter.addWidget layout qWalletInfo
  QSplitter.setStretchFactor layout 1 1
  QSplitter.setChildrenCollapsible layout False

  return (layout, Wallet{..})

initItemModel :: IO QStandardItemModel.QStandardItemModel
initItemModel = do
  model <- QStandardItemModel.new
  QStandardItemModel.setHorizontalHeaderLabels model ["Wallets" :: String]

  item <- QStandardItem.new
  QStandardItem.setText item ("Loading..." :: String)
  QStandardItem.setSelectable item False

  root <- QStandardItemModel.invisibleRootItem model
  QStandardItem.appendRowItem root item

  return model

data WalletEvent
  = WalletUpdateEvent [UiWalletTree] (Maybe UiWalletTreeSelection)

handleWalletEvent
  :: WalletEvent
  -> UI Wallet ()
handleWalletEvent ev = do
  Wallet{..} <- ask
  liftIO $ case ev of
    WalletUpdateEvent wallets _selection -> do
      updateModel itemModel wallets

updateModel
  :: QStandardItemModel.QStandardItemModel
  -> [UiWalletTree]
  -> IO ()
updateModel model wallets = do
  -- TODO: don't rebuild model from scratch on every wallet update
  QStandardItemModel.clear model
  QStandardItemModel.setHorizontalHeaderLabels model ["Wallets" :: String]

  root <- QStandardItemModel.invisibleRootItem model
  forM_ wallets $ toModelItem root
  where
    toModelItem
      :: QStandardItem.QStandardItem
      -> UiWalletTree
      -> IO ()
    toModelItem parent (Node UiWalletTreeItem{..} children) = do
      item <- QStandardItem.new
      QStandardItem.setText item $ toString $ fromMaybe "" wtiLabel
      QStandardItem.appendRowItem parent item
      forM_ children $ toModelItem item
