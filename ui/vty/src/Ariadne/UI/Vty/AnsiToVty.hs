module Ariadne.UI.Vty.AnsiToVty (ansiToVty) where

import Prelude

import Control.Monad.Trans.Writer

import qualified Graphics.Vty as V
import qualified System.Console.ANSI.Types as AT
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr.A

ansiToVty :: Ppr.A.SimpleDoc -> V.Image
ansiToVty = V.vertCat . execWriter . go V.defAttr id
  where
    go
      :: V.Attr
      -> (V.Image -> V.Image)
      -> Ppr.A.SimpleDoc
      -> Writer [V.Image] ()
    go attr img = \case
      Ppr.A.SFail -> error "ansiToVty: impossible"
      Ppr.A.SEmpty -> tell [img V.emptyImage]
      Ppr.A.SChar c x -> go attr (img . V.horizJoin (V.char attr c)) x
      Ppr.A.SText _ s x -> go attr (img . V.horizJoin (V.string attr s)) x
      Ppr.A.SLine i x -> do
        tell [img V.emptyImage]
        go attr (V.horizJoin (V.backgroundFill i 1)) x
      Ppr.A.SSGR s x ->
        go (V.withForeColor attr (getColor s)) img x

getColor :: [AT.SGR] -> V.Color
getColor xs = case colors of
  []    -> V.white
  (c:_) -> toColor c
  where
    toColor (AT.SetColor _ _ c) = case c of
      AT.Black   -> V.black
      AT.Red     -> V.red
      AT.Green   -> V.green
      AT.Yellow  -> V.yellow
      AT.Blue    -> V.blue
      AT.Magenta -> V.magenta
      AT.Cyan    -> V.cyan
      AT.White   -> V.white
    toColor _ = V.white

    colors = filter isColor xs

    isColor = \case
      AT.SetColor _ _ _ -> True
      _ -> False
