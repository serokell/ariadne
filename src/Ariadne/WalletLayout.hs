{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ariadne.WalletLayout  where

import Universum hiding (on, unlines)

import Lens.Micro.TH

import Control.Monad (void)

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

data Edit = Edit
    deriving (Show, Eq, Ord)

data Menu = File
          | View
          | Help
          | Settings
          | Configurations
          | Repl
    deriving (Show, Eq, Ord)

data St =
  St { _menu :: D.Dialog Menu
     , _middle :: String
     , _focusRing :: F.FocusRing Edit
     , _repl :: E.Editor String Edit
     }

makeLenses ''St

drawUI :: St -> [T.Widget Edit]
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
          (E.renderEditor (str . unlines))
          (st^.repl)


initialState :: St
initialState =
  St  (D.dialog Nothing (Just (0, items)) maxBound)
      (" ")
      (F.focusRing [Edit])
      (E.editor Edit (Just maxBound) " ")
  where
    items = [ ("File", File)
            , ("View", View)
            , ("Help", Help)
            , ("Settings", Settings)
            , ("Configurations", Configurations)
            , ("Repl", Repl)
            ]

appEvent :: St
         -> T.BrickEvent Edit e
         -> T.EventM Edit (T.Next St)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey V.KLeft [] -> M.continue =<< handleDialogEventLensed
    V.EvKey V.KRight [] -> M.continue =<< handleDialogEventLensed
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    _ -> M.continue =<< case F.focusGetCurrent (st ^. focusRing) of
      Just Edit -> handleEditorEventLensed
      Nothing -> return st
    where
      handleDialogEventLensed =
        T.handleEventLensed st menu D.handleDialogEvent ev
      handleEditorEventLensed =
        T.handleEventLensed st repl E.handleEditorEvent ev
appEvent st _ = M.continue st

appCursor :: St
          -> [T.CursorLocation Edit]
          -> Maybe (T.CursorLocation Edit)
appCursor = F.focusRingCursor (^.focusRing)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.black)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.blue)
    , (E.editAttr, V.white `on` V.black)
    , (E.editFocusedAttr, V.white `on` V.black)
    ]

app :: M.App St e Edit
app =
    M.App { M.appDraw = drawUI
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appChooseCursor = appCursor
          }

main :: IO ()
main = void $ M.defaultMain app initialState
