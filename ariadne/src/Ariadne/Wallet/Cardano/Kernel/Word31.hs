module Ariadne.Wallet.Cardano.Kernel.Word31
    ( Word31
    , Word31Exception (..)
    , unsafeMkWord31
    , word31ToWord32
    ) where

import Universum

import Control.Monad.Except (MonadError(throwError))
import Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable (Buildable (..))
import Formatting (bprint, int)

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

newtype Word31 = Word31
    { getWord31 :: Word32
    } deriving (Show, Ord, Eq)

newtype Word31Exception = Word31Overflow Word32
    deriving (Show)

instance Exception Word31Exception where
    displayException (Word31Overflow n) =
        "Word31: " <> show n <> " is too large"

maxWord31 :: Word32
maxWord31 = 0x7FFFFFFF -- 2^31 - 1

instance Bounded Word31 where
    minBound = Word31 0
    maxBound = Word31 maxWord31

-- | Makes a 'Word31' but is _|_ if that number exceeds 2^31 - 1
unsafeMkWord31 :: Word32 -> Word31
unsafeMkWord31 n =
    either (error . toText . displayException) (const word31) (checkWord31 word31)
  where
    word31 = Word31 n
{-# INLINE unsafeMkWord31 #-}

checkWord31 :: MonadError Word31Exception m => Word31 -> m ()
checkWord31 (Word31 n)
    | n <= maxWord31 = pure ()
    | otherwise      = throwError $ Word31Overflow n

word31ToWord32 :: Word31 -> Word32
word31ToWord32 (Word31 n) = n

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Buildable Word31 where
    build (Word31 n) = bprint int n

deriveSafeCopySimple 1 'base ''Word31
