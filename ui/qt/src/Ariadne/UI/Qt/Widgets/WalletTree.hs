module Ariadne.UI.Qt.Widgets.WalletTree
       ( WalletTree
       , initWalletTree

       , WalletTreeEvent(..)
       , handleWalletTreeEvent
       ) where

import Universum hiding (intercalate)

import Control.Lens (makeLensesWith)
import Data.Text (intercalate)
import Graphics.UI.Qtah.Signal (connect_)
import IiExtras (postfixLFields)

import Data.Bits

import Graphics.UI.Qtah.Core.Types (alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QAbstractItemView as QAbstractItemView
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QInputDialog as QInputDialog
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QTreeView as QTreeView
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

data WalletTree =
  WalletTree
    { treeView :: QTreeView.QTreeView
    , itemModel :: QStandardItemModel.QStandardItemModel
    , selectionModel :: QItemSelectionModel.QItemSelectionModel
    }

makeLensesWith postfixLFields ''WalletTree

initWalletTree
  :: UiLangFace
  -> QStandardItemModel.QStandardItemModel
  -> QItemSelectionModel.QItemSelectionModel
  -> IO (QWidget.QWidget, WalletTree)
initWalletTree langFace itemModel selectionModel = do
  widget <- QWidget.new
  QObject.setObjectName widget ("walletTreePane" :: String)

  treeView <- QTreeView.new
  QObject.setObjectName treeView ("walletTree" :: String)
  QTreeView.setHeaderHidden treeView True

  QAbstractItemView.setModel treeView itemModel
  QAbstractItemView.setSelectionModel treeView selectionModel
  QAbstractItemView.setSelectionBehavior treeView QAbstractItemView.SelectRows

  layout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw layout 0 12 0 12
  QLayout.setSpacing layout 6
  QLayout.addWidget layout treeView

  newWalletBtn <- QPushButton.newWithText ("New wallet" :: String)
  QLayout.addWidget layout newWalletBtn
  QWidget.setSizePolicyRaw newWalletBtn Maximum Maximum
  void $ QLayout.setWidgetAlignment layout newWalletBtn $ alignHCenter .|. alignVCenter

  QBoxLayout.setStretch layout 0 2
  QBoxLayout.addStretchOf layout 1

  connect_ newWalletBtn QAbstractButton.clickedSignal $ addWalletClicked langFace WalletTree{..}

  QWidget.setLayout widget layout

  return (widget, WalletTree{..})

addWalletClicked :: UiLangFace -> WalletTree -> Bool -> IO ()
addWalletClicked UiLangFace{..} WalletTree{..} _checked = do
  name <- toText <$> QInputDialog.getText treeView ("New wallet" :: String) ("Wallet name" :: String)
  unless (null name) $ void $ langPutUiCommand $ UiNewWallet name

data WalletTreeEvent
  = WalletTreeNewWalletCommandResult UiCommandId UiNewWalletCommandResult

handleWalletTreeEvent
  :: UiLangFace
  -> WalletTreeEvent
  -> UI WalletTree ()
handleWalletTreeEvent UiLangFace{..} ev = do
  WalletTree{..} <- ask
  lift $ case ev of
    WalletTreeNewWalletCommandResult _commandId result -> case result of
      UiNewWalletCommandSuccess mnemonic -> do
        void $ QMessageBox.information treeView ("Success" :: String) $
          toString $ "This is your wallet mnemonic. Save it.\n\n" <> intercalate " " mnemonic
      UiNewWalletCommandFailure err -> do
        void $ QMessageBox.critical treeView ("Error" :: String) $ toString err
