{-# LANGUAGE OverloadedStrings #-}
module Ariadne.WalletLayout  where


import Control.Monad (void)

import qualified Brick.Main as M

import Brick.Widgets.Core
  ( vBox
  , hBox
  , padAll
  , padLeft
  , padRight
  , str
  , padTopBottom
  , padTop
  )
import Brick.Util (on, bg)


import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V
import qualified Brick.Types as T

data Menu = File | View | Help | WIP

drawUI :: D.Dialog Menu -> [T.Widget ()]
drawUI d = [
        vBox [ vBox [ D.renderDialog d $
                             C.hCenter $
                             padTopBottom 1 $
                             str " "
                    ]
       , hBox [ padLeft (T.Pad 70) $ str " "
              , B.vBorder
              , padRight T.Max $ str " "
              ]
       , B.hBorder
       , vBox [ padTopBottom 5 $ C.hCenter $ str "Auxx Repl"
              , B.hBorder
              ]
       ]
       ]

initialState :: D.Dialog Menu
initialState = D.dialog Nothing (Just (0, items)) maxBound
  where
    items = [ ("File", File)
              , ("View", View)
              , ("Help", Help)
              , ("Other itmes are in WIP, lad", WIP)
              ]

appEvent :: D.Dialog Menu -> T.BrickEvent () e -> T.EventM () (T.Next (D.Dialog Menu))
appEvent d (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt d
    V.EvKey V.KEnter [] -> M.halt d
    _ -> M.continue =<< D.handleDialogEvent ev d
appEvent d _ = M.continue d

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.black)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.blue)
    ]

app :: M.App (D.Dialog Menu) e ()
app =
    M.App { M.appDraw = drawUI
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        , M.appChooseCursor = M.showFirstCursor
        }

main :: IO ()
main = void $ M.defaultMain app initialState
