module UI.Wallet where

import Universum hiding (on)

import Lens.Micro.TH
import Lens.Micro

import Data.Text (unpack, pack)

import Brick.Widgets.Core
  ( vBox
  , hBox
  , padAll
  , padLeft
  , padRight
  , str
  , padTopBottom
  , padTop
  , (<+>)
  , hLimit
  , vLimit
  )
import Brick.Util (on, bg)
import qualified Brick.AttrMap        as Attr
import qualified Brick.Focus          as Focus
import qualified Brick.Main           as Main
import qualified Brick.Types          as Types
import qualified Brick.Themes         as Themes
import qualified Brick.Widgets.Border as WBorder
import qualified Brick.Widgets.Center as WCenter
import qualified Brick.Widgets.Dialog as WDialog
import qualified Brick.Widgets.Edit   as WEdit
import qualified Brick.Widgets.List   as WList
import qualified Graphics.Vty         as V
import qualified Data.Vector          as Vec

data Name = 
    MenuName
  | MiddleName
  | ListName
  | ReplName
  deriving (Show, Eq, Ord)

data Menu = 
    File
  | View
  | Help
  | Config
  | Exit
  deriving (Show, Eq, Ord)

data AppState = AppState 
  { _menu           :: WDialog.Dialog Menu
  , _middle            :: String
  , _focusRing         :: Focus.FocusRing Name
  , _repl              :: WEdit.Editor String Name
  , _wallets           :: [Wallet]
  , _clicked           :: [Types.Extent Name]
  , _lastReportedClick :: Maybe (Name, Types.Location)
  }

initialAppState :: AppState
initialAppState = AppState  
  (WDialog.dialog Nothing (Just (0, items)) maxBound)
  (" ")
  (Focus.focusRing [ReplName])
  (WEdit.editor ReplName (Just maxBound) " ")
  []
  []
  Nothing
  where
    items = [ ("File", File)
            , ("View", View)
            , ("Help", Help)
            , ("Config", Config)
            , ("Exit", Exit)
            ]

data Currency = ADA | SHN

data Wallet = Wallet 
  { _id       :: Int
  , _currency :: Currency
  , _balance  :: Int
  }

makeLenses ''AppState

drawUI :: AppState -> [Types.Widget Name]
drawUI st = [ vBox [ topUI
                   , hBox [walletList, WBorder.vBorder, walletInfo]
                   , WBorder.hBorder
                   , auxxReadEval
                   , WBorder.hBorder
                   , auxxPrint
                   ]
            ]
  where
    walletList = padLeft (Types.Pad 5) $ str "wallet list" -- walletList
    walletInfo = padRight Types.Max $ str "wallet info"
    topUI  = vBox [ WDialog.renderDialog (st^.menu) $
                    WCenter.hCenter $
                    padTopBottom 1 $
                    str "Menu Bar"
                  ]
    unlinesStr = unpack . unlines . fmap pack
    auxxReadEval = padTopBottom 5 $
                    ((WCenter.center $ str ">>>>>")
                        <+>
                        (hLimit 200 $ vLimit 32 e))
    e = Focus.withFocusRing
          (st^.focusRing)
          (WEdit.renderEditor (str . unlinesStr))
          (st^.repl)
    auxxPrint = str "auxx Print"

{-
walletList :: AppState -> [Widget ()]
walletList s = [wal] 
  where 
    label = str "Wallet" <+> 
-}


appEvent :: AppState
         -> Types.BrickEvent Name e
         -> Types.EventM Name (Types.Next AppState)
appEvent st (Types.VtyEvent ev) =
  case ev of
    V.EvResize {}             -> Main.continue st
    V.EvKey V.KEsc []         -> Main.halt st
    V.EvKey V.KEnter []       -> Main.halt st
    V.EvKey V.KLeft []        -> Main.continue =<< handleDialogEventLensed
    V.EvKey V.KRight []       -> Main.continue =<< handleDialogEventLensed
    V.EvKey (V.KChar '\t') [] -> Main.continue $ st & focusRing %~ Focus.focusNext
    _                         -> Main.continue =<<
                                  case Focus.focusGetCurrent (st ^. focusRing) of
                                    Just ReplName -> handleEditorEventLensed
                                    _ -> return st
    where
      handleDialogEventLensed =
        Types.handleEventLensed st menu WDialog.handleDialogEvent ev
      handleEditorEventLensed =
        Types.handleEventLensed st repl WEdit.handleEditorEvent ev
appEvent st _ = Main.continue st

appCursor :: AppState
          -> [Types.CursorLocation Name]
          -> Maybe (Types.CursorLocation Name)
appCursor = Focus.focusRingCursor (^.focusRing)

theMap :: Attr.AttrMap
theMap = Attr.attrMap V.defAttr
    [ (WDialog.dialogAttr, V.white `on` V.black)
    , (WDialog.buttonAttr, V.black `on` V.white)
    , (WDialog.buttonSelectedAttr, bg V.blue)
    , (WEdit.editAttr, V.white `on` V.black)
    , (WEdit.editFocusedAttr, V.white `on` V.black)
    ]

app :: Main.App AppState e Name
app =
    Main.App { Main.appDraw = drawUI
             , Main.appHandleEvent = appEvent
             , Main.appStartEvent = return
             , Main.appAttrMap = const theMap
             , Main.appChooseCursor = appCursor
             }

runBySt :: AppState -> IO AppState
runBySt st = Main.customMain buildVty Nothing app st
  where
    buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
