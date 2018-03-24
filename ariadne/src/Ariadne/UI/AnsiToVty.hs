module Ariadne.UI.AnsiToVty (ansiToVty) where

import Prelude
import Control.Monad.Trans.Writer

import qualified Text.PrettyPrint.ANSI.Leijen as Ppr.A
import qualified Graphics.Vty as V

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
      Ppr.A.SLine _i x -> do
        tell [img V.emptyImage]
        -- todo: indent
        go attr id x
      Ppr.A.SSGR _s x ->
        -- todo: attr
        go attr img x
