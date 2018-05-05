module Ariadne.UI.Qt.Widgets.WalletInfo
       ( WalletInfo
       , initWalletInfo
       ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras

import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel

data WalletInfo =
  WalletInfo
    { label :: QLabel.QLabel
    }

makeLensesWith postfixLFields ''WalletInfo

initWalletInfo :: IO (QLabel.QLabel, WalletInfo)
initWalletInfo = do
  label <- QLabel.new
  QLabel.setText label ("Wallet info" :: String)

  return (label, WalletInfo{..})
