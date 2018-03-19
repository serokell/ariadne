module Ariadne.WalletLayout where

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
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import qualified Brick.Main as M
import qualified Brick.Focus as F
import qualified Graphics.Vty as V
import qualified Brick.Types as T

data Mode = MenuMode
          | MiddleMode
          | ReplMode
    deriving (Show, Eq, Ord)

data Menu = File
          | View
          | Help
          | Settings
          | Configurations
          | Exit
    deriving (Show, Eq, Ord)

data St =
  St { _menu :: D.Dialog Menu
     , _middle :: String
     , _focusRing :: F.FocusRing Mode
     , _repl :: E.Editor String Mode
     }

makeLenses ''St

drawUI :: St -> [T.Widget Mode]
drawUI st = [ vBox [ topUI
                   , mainUI
                   , B.hBorder
                   , auxxUI
                   ]
            ]
  where
    auxxUI = vBox [ padTopBottom 5 $
                    ((C.center $ str "")
                        <+>
                        (hLimit 200 $ vLimit 32 e))
                  , B.hBorder
                  ]
    mainUI = hBox [ padLeft (T.Pad 70) $ str "Cool left part"
                  , B.vBorder
                  , padRight T.Max $ str "Cool right part"
                  ]
    topUI  = vBox [ D.renderDialog (st^.menu) $
                    C.hCenter $
                    padTopBottom 1 $
                    str "Menu Bar"
                  ]
    e = F.withFocusRing
          (st^.focusRing)
          (E.renderEditor (str . unlinesStr))
          (st^.repl)
    unlinesStr = unpack . unlines . fmap pack

initialState :: St
initialState =
  St  (D.dialog Nothing (Just (0, items)) maxBound)
      (" ")
      (F.focusRing [ReplMode])
      (E.editor ReplMode (Just maxBound) " ")
  where
    items = [ ("File", File)
            , ("View", View)
            , ("Help", Help)
            , ("Settings", Settings)
            , ("Configurations", Configurations)
            , ("Exit", Exit)
            ]

appEvent :: St
         -> T.BrickEvent Mode e
         -> T.EventM Mode (T.Next St)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvResize {} -> M.continue st
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey V.KLeft [] -> M.continue =<< handleDialogEventLensed
    V.EvKey V.KRight [] -> M.continue =<< handleDialogEventLensed
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    _ -> M.continue =<<
                    case F.focusGetCurrent (st ^. focusRing) of
                      Just ReplMode -> handleEditorEventLensed
                      _ -> return st
    where
      handleDialogEventLensed =
        T.handleEventLensed st menu D.handleDialogEvent ev
      handleEditorEventLensed =
        T.handleEventLensed st repl E.handleEditorEvent ev
appEvent st _ = M.continue st

appCursor :: St
          -> [T.CursorLocation Mode]
          -> Maybe (T.CursorLocation Mode)
appCursor = F.focusRingCursor (^.focusRing)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.black)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.blue)
    , (E.editAttr, V.white `on` V.black)
    , (E.editFocusedAttr, V.white `on` V.black)
    ]

app :: M.App St e Mode
app =
    M.App { M.appDraw = drawUI
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appChooseCursor = appCursor
          }

runBySt :: St -> IO St
runBySt st = M.defaultMain app st
