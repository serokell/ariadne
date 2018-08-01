module Ariadne.UI.Qt.UI where

import Universum

import Graphics.UI.Qtah.Signal (Signal, connect_)

import qualified Graphics.UI.Qtah.Core.QVariant as QVariant

type UI w r = ReaderT w IO r

runUI :: UI w r -> w -> IO r
runUI = runReaderT

connectSignal :: w -> o -> Signal o (IO r) -> UI w r -> IO ()
connectSignal widget object signal handler =
  liftIO $ connect_ object signal $ runUI handler widget

class QVariantCastable a where
  toQVariant :: a -> IO QVariant.QVariant
  fromQVariant :: (QVariant.QVariantValue v) => v -> IO a

instance QVariantCastable Int where
  toQVariant = QVariant.newWithInt . fromIntegral
  fromQVariant = return . fromIntegral <=< QVariant.toInt

instance QVariantCastable Word where
  toQVariant = QVariant.newWithUInt . fromIntegral
  fromQVariant = return . fromIntegral <=< QVariant.toUInt

instance QVariantCastable a => QVariantCastable [a] where
  toQVariant = QVariant.newWithList <=< mapM toQVariant
  fromQVariant = mapM fromQVariant <=< QVariant.toList
