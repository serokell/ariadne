module Ariadne.UI.Qt.MainWindow
    ( mkMainWindow
    ) where

import Universum

import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QStatusBar as QStatusBar
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face (UiLangFace)

import Ariadne.UI.Qt.MainWindow.Slots
import Ariadne.UI.Qt.MainWindow.Ui

mkMainWindow :: UiLangFace -> IO MainWindow
mkMainWindow langFace = do
    mwMainWindow <- QMainWindow.new
    QWidget.setWindowTitle mwMainWindow ("Ariadne" :: String)
    centralWidget <- QWidget.newWithParent mwMainWindow
    QObject.setObjectName centralWidget ("centralWidget" :: String)

    mainLayout <- QVBoxLayout.newWithParent centralWidget
    QObject.setObjectName mainLayout ("mainLayout" :: String)

    blockchainInfo <- QLabel.new
    QObject.setObjectName blockchainInfo ("blockchainInfo" :: String)
    QLabel.setText blockchainInfo ("Current slot" :: String)

    statusBar <- QStatusBar.newWithParent mwMainWindow
    QObject.setObjectName statusBar ("statusBar" :: String)
    QMainWindow.setStatusBar mwMainWindow statusBar

    QStatusBar.addPermanentWidgetWithStretch statusBar blockchainInfo 1

    walletInfo <- QLabel.newWithParent centralWidget
    QObject.setObjectName walletInfo ("walletInfo" :: String)
    QLabel.setText walletInfo ("Wallet info pane" :: String)
    QLayout.addWidget mainLayout walletInfo

    (qReplLayout, replLayout) <- createReplLayout centralWidget
    QBoxLayout.addLayout mainLayout qReplLayout

    QBoxLayout.setStretch mainLayout 0 2
    QBoxLayout.setStretch mainLayout 1 1

    QMainWindow.setCentralWidget mwMainWindow centralWidget

    let mainWindow = MainWindow{..}

    QWidget.setFocus $ cmdLine replLayout

    runUI (connectSignals langFace) mainWindow

    return mainWindow

createReplLayout :: QWidget.QWidget -> IO (QVBoxLayout.QVBoxLayout, ReplLayout)
createReplLayout parent = do
    replLayout <- QVBoxLayout.new
    QObject.setObjectName replLayout ("replLayout" :: String)

    cmdHistory <- QTextEdit.newWithParent parent
    QObject.setObjectName cmdHistory ("cmdHistory" :: String)
    QTextEdit.setReadOnly cmdHistory True

    cmdLine <- QLineEdit.newWithParent parent
    QObject.setObjectName cmdHistory ("cmdLine" :: String)

    QLayout.addWidget replLayout cmdHistory
    QLayout.addWidget replLayout cmdLine

    return (replLayout, ReplLayout{..})

connectSignals :: UiLangFace -> UI ()
connectSignals langFace = do
    connect QLineEdit.returnPressedSignal (replReturnPressed langFace) =<< view (replLayoutL . cmdLineL)
    where
        connect signal handler widget = liftIO . connect_ widget signal . runUI handler =<< ask

