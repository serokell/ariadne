module Ariadne.UI.Qt.Widgets.WalletTree
       ( WalletTree
       , initWalletTree
       ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras

import qualified Graphics.UI.Qtah.Widgets.QTreeWidget as QTreeWidget

data WalletTree =
  WalletTree
    { treeWidget :: QTreeWidget.QTreeWidget
    }

makeLensesWith postfixLFields ''WalletTree

initWalletTree :: IO (QTreeWidget.QTreeWidget, WalletTree)
initWalletTree = do
  treeWidget <- QTreeWidget.new

  return (treeWidget, WalletTree{..})
