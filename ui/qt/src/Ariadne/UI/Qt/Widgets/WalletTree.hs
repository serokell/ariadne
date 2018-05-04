module Ariadne.UI.Qt.Widgets.WalletTree
       ( WalletTree
       , initWalletTree
       ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras (postfixLFields)

import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QAbstractItemView as QAbstractItemView
import qualified Graphics.UI.Qtah.Widgets.QTreeView as QTreeView

data WalletTree =
  WalletTree
    { treeView :: QTreeView.QTreeView
    , itemModel :: QStandardItemModel.QStandardItemModel
    }

makeLensesWith postfixLFields ''WalletTree

initWalletTree
  :: QStandardItemModel.QStandardItemModel
  -> IO (QTreeView.QTreeView, WalletTree)
initWalletTree itemModel = do
  treeView <- QTreeView.new
  QAbstractItemView.setModel treeView itemModel

  return (treeView, WalletTree{..})
