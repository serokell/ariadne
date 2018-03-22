module Ariadne.UI.WalletWidget where

import Prelude
import qualified Data.Vector as Vec
import Control.Lens
import Data.Monoid
import Control.Monad.Trans.State

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Dialog as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Widgets.List as B
import qualified Graphics.Vty as V

import Ariadne.UI.LayerName
import Ariadne.UI.ReplWidget
import Ariadne.Face
import Ariadne.Util

data WalletWidgetSelector =
    MenuName
  | ReplName
  | ReplSelector ReplWidgetSelector
  | WalletListName
  | WalletInfoName
  | WalletId Int
  deriving (Show, Eq, Ord)

makePrisms ''WalletWidgetSelector

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
    , focusRing :: B.FocusRing WalletWidgetSelector
    , repl :: ReplWidgetState name
    , wallets :: B.List name Wallet
    , clicked :: [B.Extent name]
    , lastReportedClick :: Maybe (name, B.Location)
    }

makeLensesWith postfixLFields ''WalletWidgetState

initWalletWidget
  :: (WalletWidgetSelector -> name)
  -> WalletWidgetState name
initWalletWidget injName = WalletWidgetState
  (B.dialog Nothing (Just (0, items)) maxBound)
  (B.focusRing
    [ ReplName
    , MenuName
    , WalletListName
    , WalletInfoName])
  (initReplWidget (injName . ReplSelector))
  (B.list (injName WalletListName) (Vec.fromList walletList) 1)
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
    , drawReplWidget (focus == ReplName) (st ^. replL)
    ]
  where
    focus = currentFocus (st ^. focusRingL)
    walletList = B.hLimit 30 $ B.padLeft (B.Pad 4) $ renderWalletList st
    walletInfo = B.padRight B.Max $ B.str "wallet info"
    topUI =
      B.vLimit 4 $
      B.renderDialog (st^.menuL) $
      B.hCenter $
      B.padBottom (B.Pad 1) $
      B.emptyWidget

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

data WalletCompleted
  = WalletCompleted
  | WalletInProgress
  | WalletToLayer LayerName

handleWalletWidgetEvent
  :: Ord name
  => AuxxFace
  -> B.BrickEvent name AuxxEvent
  -> StateT (WalletWidgetState name) (B.EventM name) WalletCompleted
handleWalletWidgetEvent auxxFace ev = do
  focus <- uses focusRingL currentFocus
  case ev of
    B.VtyEvent (V.EvKey V.KEsc []) ->
      return WalletCompleted
    B.VtyEvent (V.EvKey (V.KChar '\t') []) -> do
      zoom focusRingL $ modify B.focusNext
      return WalletInProgress
    B.VtyEvent vtyEv
      | Just replEv <- toReplEv vtyEv, ReplName <- focus -> do
        zoom replL $ handleReplWidgetEvent auxxFace replEv
        return WalletInProgress
    B.VtyEvent (V.EvKey V.KEnter []) -> do
      diaSel <- uses menuL B.dialogSelection
      return $
        case diaSel of
          Nothing -> WalletCompleted
          Just MenuExit -> WalletCompleted
          Just MenuConfig -> WalletToLayer LayerConfig
          Just MenuHelp -> WalletToLayer LayerHelp
    B.VtyEvent vtyEv@(V.EvKey V.KLeft []) -> do
      zoom menuL $ wrapBrickHandler B.handleDialogEvent vtyEv
      return WalletInProgress
    B.VtyEvent vtyEv@(V.EvKey V.KRight []) -> do
      zoom menuL $ wrapBrickHandler B.handleDialogEvent vtyEv
      return WalletInProgress
    B.VtyEvent vtyEv | WalletListName <- focus -> do
      zoom walletsL $ wrapBrickHandler B.handleListEvent vtyEv
      return WalletInProgress
    B.AppEvent (AuxxResultEvent commandId commandResult) -> do
      zoom replL $ handleReplWidgetEvent auxxFace $
        ReplCommandResultEvent commandId commandResult
      return WalletInProgress
    _ -> return WalletInProgress

toReplEv :: V.Event -> Maybe ReplWidgetEvent
toReplEv = \case
  V.EvKey V.KLeft [] ->
    Just $ ReplInputNavigationEvent NavArrowLeft
  V.EvKey V.KRight [] ->
    Just $ ReplInputNavigationEvent NavArrowRight
  V.EvKey V.KUp [] ->
    Just $ ReplInputNavigationEvent NavArrowUp
  V.EvKey V.KDown [] ->
    Just $ ReplInputNavigationEvent NavArrowDown
  V.EvKey V.KBS [] ->
    Just $ ReplInputModifyEvent DeleteBackwards
  V.EvKey V.KDel [] ->
    Just $ ReplInputModifyEvent DeleteForwards
  V.EvKey V.KEnter [] ->
    Just $ ReplSendEvent
  V.EvKey (V.KChar 'n') [V.MCtrl] ->
    Just $ ReplInputModifyEvent BreakLine
  V.EvKey (V.KChar c) _ ->
    Just $ ReplInputModifyEvent (InsertChar c)
  _ -> Nothing
