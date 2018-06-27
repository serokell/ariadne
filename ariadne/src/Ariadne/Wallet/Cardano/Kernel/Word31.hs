module Ariadne.Wallet.Cardano.Kernel.Word31
    ( Word31
    , Word31Exception (..)
    , unsafeMkWord31
    , word31ToWord32
    ) where

import Universum

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
    if n <= maxWord31 then
        Word31 n
    else
        bug $ Word31Overflow n
{-# INLINE unsafeMkWord31 #-}

word31ToWord32 :: Word31 -> Word32
word31ToWord32 (Word31 n) = n

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Buildable Word31 where
    build (Word31 n) = bprint int n

deriveSafeCopySimple 1 'base ''Word31
