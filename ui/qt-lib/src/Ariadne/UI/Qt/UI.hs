module Ariadne.UI.Qt.UI where

import Universum

import qualified Data.ByteString as BS

import Graphics.UI.Qtah.Signal (Signal, connect_)

import qualified Graphics.UI.Qtah.Core.QObject as QObject
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

-- This will be removed once qtah changes `setProperty` method to require something sane
-- instead of `CString`.
setProperty ::
  (QObject.QObjectPtr obj, ToString a, ToString b) =>
  obj -> a -> b -> IO Bool
setProperty object name value = do
  let packedName = encodeUtf8 $ toString name
  qValue <- QVariant.newWithString $ toString value
  BS.useAsCString packedName (\qName -> QObject.setProperty object qName qValue)
