{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ariadne.WalletLayout  where

import Lens.Micro
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
          | WIP
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
                    C.center $
                    (str "Auxx Repl" <+> (hLimit 5 $ vLimit 50 e))
                  , B.hBorder
                  ]
    mainUI = hBox [ padLeft (T.Pad 20) $ str "side"
                  , B.vBorder
                  , padRight T.Max $ str "main"
                  ]
    topUI  = vBox [ D.renderDialog (st^.menu) $
                    C.hCenter $
                    padTopBottom 1 $
                    str "Menu Bar"
                  ]
    e = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.repl)


initialState :: St
initialState =
  St  (D.dialog Nothing (Just (0, items)) maxBound)
      (" ")
      (F.focusRing [Edit])
      (E.editor Edit (Just 2) " ")
  where
    items = [ ("File", File)
            , ("View", View)
            , ("Help", Help)
            , ("Settings", Settings)
            , ("WIP", WIP)
            ]

appEvent :: St
         -> T.BrickEvent Edit e
         -> T.EventM Edit (T.Next St)
appEvent d (T.VtyEvent ev) = undefined

appCursor :: St -> [T.CursorLocation Edit] -> Maybe (T.CursorLocation Edit)
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
          , M.appHandleEvent = undefined
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appChooseCursor = appCursor
          }

main :: IO ()
main = void $ M.defaultMain app initialState
