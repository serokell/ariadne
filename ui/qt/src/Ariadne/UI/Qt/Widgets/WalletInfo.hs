module Ariadne.UI.Qt.Widgets.WalletInfo
       ( WalletInfo
       , initWalletInfo
       ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras

import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel

data WalletInfo =
  WalletInfo
    { label :: QLabel.QLabel
    , itemModel :: QStandardItemModel.QStandardItemModel
    }

makeLensesWith postfixLFields ''WalletInfo

initWalletInfo
  :: QStandardItemModel.QStandardItemModel
  -> IO (QLabel.QLabel, WalletInfo)
initWalletInfo itemModel = do
  label <- QLabel.new
  QLabel.setText label ("Wallet info" :: String)

  return (label, WalletInfo{..})
