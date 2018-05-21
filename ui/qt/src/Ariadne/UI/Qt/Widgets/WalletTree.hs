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

import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QAbstractItemView as QAbstractItemView
import qualified Graphics.UI.Qtah.Widgets.QAction as QAction
import qualified Graphics.UI.Qtah.Widgets.QInputDialog as QInputDialog
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QToolBar as QToolBar
import qualified Graphics.UI.Qtah.Widgets.QToolButton as QToolButton
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
  treeView <- QTreeView.new
  QAbstractItemView.setModel treeView itemModel
  QAbstractItemView.setSelectionModel treeView selectionModel
  QAbstractItemView.setSelectionBehavior treeView QAbstractItemView.SelectRows

  addMenu <- QMenu.new
  addWallet <- QMenu.addNewAction addMenu ("Wallet" :: String)
  addAccount <- QMenu.addNewAction addMenu ("Account" :: String)
  addAddress <- QMenu.addNewAction addMenu ("Address" :: String)

  addButton <- QToolButton.new
  QAbstractButton.setText addButton ("Add" :: String)
  QToolButton.setMenu addButton addMenu
  QToolButton.setPopupMode addButton QToolButton.InstantPopup

  toolBar <- QToolBar.new
  void $ QToolBar.addWidget toolBar addButton

  layout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw layout 0 0 0 0
  QLayout.addWidget layout toolBar
  QLayout.addWidget layout treeView

  widget <- QWidget.new
  QWidget.setLayout widget layout

  connect_ addWallet QAction.triggeredSignal $ addWalletClicked langFace WalletTree{..}
  connect_ addAccount QAction.triggeredSignal $ addAccountClicked langFace WalletTree{..}
  connect_ addAddress QAction.triggeredSignal $ addAddressClicked langFace WalletTree{..}

  return (widget, WalletTree{..})

addWalletClicked :: UiLangFace -> WalletTree -> Bool -> IO ()
addWalletClicked UiLangFace{..} WalletTree{..} _checked = do
  name <- toText <$> QInputDialog.getText treeView ("New wallet" :: String) ("Wallet name" :: String)
  unless (null name) $ void $ langPutUiCommand $ UiNewWallet name

addAccountClicked :: UiLangFace -> WalletTree -> Bool -> IO ()
addAccountClicked UiLangFace{..} WalletTree{..} _checked = do
  name <- toText <$> QInputDialog.getText treeView ("New account" :: String) ("Account name" :: String)
  unless (null name) $ void $ langPutUiCommand $ UiNewAccount name

addAddressClicked :: UiLangFace -> WalletTree -> Bool -> IO ()
addAddressClicked UiLangFace{..} WalletTree{..} _checked = do
  void $ langPutUiCommand $ UiNewAddress

data WalletTreeEvent
  = WalletTreeNewWalletCommandResult UiCommandId UiNewWalletCommandResult
  | WalletTreeNewAddressCommandResult UiCommandId UiNewAddressCommandResult
  | WalletTreeNewAccountCommandResult UiCommandId UiNewAccountCommandResult

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
    WalletTreeNewAccountCommandResult _commandId result -> case result of
      UiNewAccountCommandSuccess -> do
        void $ QMessageBox.information treeView ("Success" :: String) ("Account created" :: String)
      UiNewAccountCommandFailure err -> do
        void $ QMessageBox.critical treeView ("Error" :: String) $ toString err
    WalletTreeNewAddressCommandResult _commandId result -> case result of
      UiNewAddressCommandSuccess -> do
        void $ QMessageBox.information treeView ("Success" :: String) ("Address created" :: String)
      UiNewAddressCommandFailure err -> do
        void $ QMessageBox.critical treeView ("Error" :: String) $ toString err
