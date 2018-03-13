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
  , padTopBottom
  )
import Brick.Widgets.Border as B
import Brick.Widgets.Center as C
import Brick.AttrMap (attrMap)
import qualified Graphics.Vty as V

ui :: Widget ()
ui =
  vBox [ B.hBorder
         , hBox [ vBox [ padTopBottom 2 $ str "some stuff"
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
