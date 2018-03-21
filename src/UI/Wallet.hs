module UI.Wallet where

import Universum hiding (on, (<>))

import Lens.Micro.TH
import Lens.Micro

import Data.Monoid((<>))
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
  , padBottom
  , (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , withAttr
  , emptyWidget
  )
import Brick.Util (on, bg, fg)
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
  | AuxxReadName
  | AuxxPrintName
  | WalletListName
  | WalletInfoName
  | WalID Int
  deriving (Show, Eq, Ord)

data Menu = 
    Help
  | Config
  | Exit
  deriving (Show, Eq, Ord)

data Currency = ADA | SHN deriving (Show, Eq, Ord)

data Wallet = Wallet 
  { _id       :: Int
  , _currency :: Currency
  , _balance  :: Int
  } 
  deriving (Show, Eq, Ord)

data AppState = AppState 
  { _menu              :: WDialog.Dialog Menu
  , _focusRing         :: Focus.FocusRing Name
  , _repl              :: WEdit.Editor String Name
  , _wallets           :: WList.List Name Wallet
  , _clicked           :: [Types.Extent Name]
  , _lastReportedClick :: Maybe (Name, Types.Location)
  -- , _log               :: Text
  }

initialAppState :: AppState
initialAppState = AppState  
  (WDialog.dialog Nothing (Just (0, items)) maxBound)
  (Focus.focusRing [AuxxReadName, MenuName, WalletListName, WalletInfoName])
  (WEdit.editor AuxxReadName (Just maxBound) " ")
  (WList.list WalletListName (Vec.fromList walletList) 1)
  []
  Nothing
  where
    items = [ ("Help", Help)
            , ("Config", Config)
            , ("Exit", Exit)
            ]
    walletList = [ Wallet 0 ADA 100
                 , Wallet 1 SHN 500
                 , Wallet 2 ADA 1000
                 , Wallet 3 SHN 50000
                 ]

makeLenses ''AppState

drawUI :: AppState -> [Types.Widget Name]
drawUI st = [ vBox [ topUI
                   , walletList <+> WBorder.vBorder <+> walletInfo
                   , WBorder.hBorder
                   ,  auxxPrint
                   , WBorder.hBorder
                   , auxxReadEval
                   ]
            ]
  where
    walletList = hLimit 30 $ padLeft (Types.Pad 4) $ renderWalletList st
    walletInfo = padRight Types.Max $ str "wallet info"
    topUI  = vLimit 4 $ WDialog.renderDialog (st^.menu) $
                    WCenter.hCenter $
                    padBottom (Types.Pad 1) $
                    emptyWidget
            
    unlinesStr = unpack . unlines . fmap pack
    auxxReadEval = str "auxx Read" <=> (str ">" <+> vLimit 1 e)
    e = Focus.withFocusRing
          (st^.focusRing)
          (WEdit.renderEditor (str . unlinesStr)) (st^.repl)
    auxxPrint = padTop (Types.Max) $ padBottom (Types.Max) $ str "auxx Print"

renderWalletList :: AppState -> Types.Widget Name
renderWalletList st = WList.renderList listDrawElement True (_wallets st)


listDrawElement :: Bool -> Wallet -> Types.Widget Name
listDrawElement sel w = 
  selStr $ walletStr
  where
   walletStr  = "Wallet" ++ idn ++ " " ++ cur ++ " " ++ bal
   idn  = show $ _id w
   cur  = show $ _currency w
   bal  = show $ _balance w
   selStr s =  if sel
               then withAttr customAttr (str $ "<" <> s <> ">")
               else str s
              
customAttr :: Attr.AttrName
customAttr = WList.listSelectedAttr <> "custom"


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
                                    Just AuxxReadName -> handleEditorEventLensed
                                    Just WalletListName -> handleListEventLensed
                                    _ -> return st
    where
      handleListEventLensed = 
        Types.handleEventLensed st wallets WList.handleListEvent ev
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
    , (WDialog.buttonSelectedAttr, V.withStyle (fg V.blue) V.standout)
    , (WEdit.editAttr, V.black `on` V.white)
    , (WEdit.editFocusedAttr, V.white `on` V.black)
    , (customAttr, V.white `on` V.black)
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
