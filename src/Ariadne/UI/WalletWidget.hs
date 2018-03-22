module Ariadne.UI.WalletWidget where

import Prelude
import Data.Text
import qualified Data.Vector as Vec
import Control.Lens
import Data.Monoid
import Control.Monad.Trans.Writer

import qualified Brick as B
import qualified Brick.AttrMap as B
import qualified Brick.Focus as B
import qualified Brick.Main as B
import qualified Brick.Types as B
import qualified Brick.Themes as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Dialog as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Widgets.List as B
import qualified Graphics.Vty as V

import Ariadne.UI.LayerName
import Ariadne.Util

data WalletWidgetSelector =
    MenuName
  | AuxxReadName
  | AuxxPrintName
  | WalletListName
  | WalletInfoName
  | WalletId Int
  deriving (Show, Eq, Ord)

data Menu =
    MenuHelp
  | MenuConfig
  | MenuExit
  deriving (Show, Eq, Ord)

data Currency = ADA | SHN
  deriving (Show, Eq, Ord)

data Wallet =
  Wallet
    { ident    :: Int
    , currency :: Currency
    , balance  :: Int
    } deriving (Show, Eq, Ord)

makeLensesWith postfixLFields ''Wallet

data WalletWidgetState name =
  WalletWidgetState
    { menu :: B.Dialog Menu
    , focusRing :: B.FocusRing name
    , repl :: B.Editor Text name
    , wallets :: B.List name Wallet
    , clicked :: [B.Extent name]
    , lastReportedClick :: Maybe (name, B.Location)
    }

makeLensesWith postfixLFields ''WalletWidgetState

initWalletWidget
  :: HasReview name WalletWidgetSelector
  => WalletWidgetState name
initWalletWidget = WalletWidgetState
  (B.dialog Nothing (Just (0, items)) maxBound)
  (B.focusRing [inj AuxxReadName, inj MenuName, inj WalletListName, inj WalletInfoName])
  (B.editor (inj AuxxReadName) (Just maxBound) " ")
  (B.list (inj WalletListName) (Vec.fromList walletList) 1)
  []
  Nothing
  where
    items =
      [ ("Help", MenuHelp)
      , ("Config", MenuConfig)
      , ("Exit", MenuExit)
      ]
    walletList =
      [ Wallet 0 ADA 100
      , Wallet 1 SHN 500
      , Wallet 2 ADA 1000
      , Wallet 3 SHN 50000
      ]

drawWalletWidget
  :: (Ord name, Show name)
  => WalletWidgetState name
  -> B.Widget name
drawWalletWidget st =
  B.vBox
    [ topUI
    , walletList B.<+> B.vBorder B.<+> walletInfo
    , B.hBorder
    , auxxPrint
    , B.hBorder
    , auxxReadEval
    ]
  where
    walletList = B.hLimit 30 $ B.padLeft (B.Pad 4) $ renderWalletList st
    walletInfo = B.padRight B.Max $ B.str "wallet info"
    topUI =
      B.vLimit 4 $
      B.renderDialog (st^.menuL) $
      B.hCenter $
      B.padBottom (B.Pad 1) $
      B.emptyWidget

    auxxReadEval = B.str "auxx Read" B.<=> (B.str ">" B.<+> B.vLimit 1 e)
    e = B.withFocusRing
          (st^.focusRingL)
          (B.renderEditor (B.txt . Data.Text.unlines)) (st^.replL)
    auxxPrint = B.padTop B.Max $ B.padBottom B.Max $ B.str "auxx Print"

renderWalletList :: (Ord name, Show name) => WalletWidgetState name -> B.Widget name
renderWalletList st = B.renderList listDrawElement True (wallets st)


listDrawElement :: Bool -> Wallet -> B.Widget name
listDrawElement sel w =
  selStr $ walletStr
  where
   walletStr  = "Wallet" ++ idn ++ " " ++ cur ++ " " ++ bal
   idn  = show $ id w
   cur  = show $ currency w
   bal  = show $ balance w
   selStr s =  if sel
               then B.withAttr customAttr (B.str $ "<" <> s <> ">")
               else B.str s

customAttr :: B.AttrName
customAttr = B.listSelectedAttr <> "custom"

walletWidgetAttrMap :: B.AttrMap
walletWidgetAttrMap = B.attrMap V.defAttr
    [ (B.dialogAttr, V.white `B.on` V.black)
    , (B.buttonAttr, V.black `B.on` V.white)
    , (B.buttonSelectedAttr, V.withStyle (B.fg V.blue) V.standout)
    , (B.editAttr, V.black `B.on` V.white)
    , (B.editFocusedAttr, V.white `B.on` V.black)
    , (customAttr, V.white `B.on` V.black)
    ]

walletWidgetCursor
  :: Eq name
  => WalletWidgetState name
  -> [B.CursorLocation name]
  -> Maybe (B.CursorLocation name)
walletWidgetCursor = B.focusRingCursor focusRing

data WalletCompleted = WalletCompleted | WalletInProgress | WalletToLayer LayerName

handleWalletWidgetEvent
  :: (Ord name, HasPrism name WalletWidgetSelector)
  => B.BrickEvent name ev
  -> WalletWidgetState name
  -> WriterT WalletCompleted (B.EventM name) (WalletWidgetState name)
handleWalletWidgetEvent ev walletWidgetState =
  WriterT $ case ev of
    B.VtyEvent (V.EvKey V.KEsc []) ->
      return (walletWidgetState, WalletCompleted)
    B.VtyEvent (V.EvKey V.KEnter []) -> do
      return $ (walletWidgetState,) $
        case B.dialogSelection (menu walletWidgetState) of
          Nothing -> WalletCompleted
          Just MenuExit -> WalletCompleted
          Just MenuConfig -> WalletToLayer LayerConfig
          Just MenuHelp -> WalletToLayer LayerHelp
    B.VtyEvent vtyEv@(V.EvKey V.KLeft []) -> (, WalletInProgress) <$>
      case B.focusGetCurrent (walletWidgetState ^. focusRingL) >>= proj of
        Just AuxxReadName -> handleEditorEventLensed vtyEv
        _ -> handleDialogEventLensed vtyEv
    B.VtyEvent vtyEv@(V.EvKey V.KRight []) -> (, WalletInProgress) <$>
      case B.focusGetCurrent (walletWidgetState ^. focusRingL) >>= proj of
        Just AuxxReadName -> handleEditorEventLensed vtyEv
        _ -> handleDialogEventLensed vtyEv
    B.VtyEvent (V.EvKey (V.KChar '\t') []) -> do
      let walletWidgetState' = walletWidgetState & focusRingL %~ B.focusNext
      return (walletWidgetState', WalletInProgress)
    B.VtyEvent vtyEv -> (, WalletInProgress) <$>
      case B.focusGetCurrent (walletWidgetState ^. focusRingL) >>= proj of
        Just AuxxReadName -> handleEditorEventLensed vtyEv
        Just WalletListName -> handleListEventLensed vtyEv
        _ -> return walletWidgetState
    _ -> return (walletWidgetState, WalletInProgress)
    where
      handleListEventLensed =
        B.handleEventLensed walletWidgetState walletsL B.handleListEvent
      handleDialogEventLensed =
        B.handleEventLensed walletWidgetState menuL B.handleDialogEvent
      handleEditorEventLensed =
        B.handleEventLensed walletWidgetState replL B.handleEditorEvent
