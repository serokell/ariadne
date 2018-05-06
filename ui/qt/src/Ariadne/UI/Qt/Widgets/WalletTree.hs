module Ariadne.UI.Qt.Widgets.WalletTree
       ( WalletTree
       , initWalletTree
       ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras (postfixLFields)

import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QAbstractItemView as QAbstractItemView
import qualified Graphics.UI.Qtah.Widgets.QTreeView as QTreeView

data WalletTree =
  WalletTree
    { treeView :: QTreeView.QTreeView
    , itemModel :: QStandardItemModel.QStandardItemModel
    , selectionModel :: QItemSelectionModel.QItemSelectionModel
    }

makeLensesWith postfixLFields ''WalletTree

initWalletTree
  :: QStandardItemModel.QStandardItemModel
  -> QItemSelectionModel.QItemSelectionModel
  -> IO (QTreeView.QTreeView, WalletTree)
initWalletTree itemModel selectionModel = do
  treeView <- QTreeView.new
  QAbstractItemView.setModel treeView itemModel
  QAbstractItemView.setSelectionModel treeView selectionModel
  QAbstractItemView.setSelectionBehavior treeView QAbstractItemView.SelectRows

  return (treeView, WalletTree{..})
