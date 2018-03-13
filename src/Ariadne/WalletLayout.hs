{-# LANGUAGE OverloadedStrings #-}
module Ariadne.WalletLayout  where

import Brick.Main (App(..), neverShowCursor, resizeOrQuit, defaultMain)
import Brick.Types
  ( Widget
  , Padding(..)
  )
import Brick.Widgets.Core
  ( vBox
  , hBox
  , str
  , padAll
  , padLeft
  , padRight
  , padTop
  , padBottom
  , padTopBottom
  , padLeftRight
  )
import Brick.Widgets.Border as B
import Brick.Widgets.Center as C
import Brick.AttrMap (attrMap)
import qualified Graphics.Vty as V

ui :: Widget ()
ui =
  vBox [ B.hBorder
         , hBox [ padTop Max $ hCenter $ str "Top-padded"
                , B.vBorder
                , padBottom Max $ hCenter $ str "Bottom-padded"
                ]
         , B.hBorder
         , hBox [ padLeftRight 2 $ str "Padded by 2 on left/right"
                , B.vBorder
                , vBox [ padTopBottom 1 $ str "Padded by 1 on top/bottom"
                       , B.hBorder
                       ]
                ]
         ]

app :: App () e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = defaultMain app ()
