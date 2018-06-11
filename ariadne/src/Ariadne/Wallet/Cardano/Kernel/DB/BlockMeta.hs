-- | Block metadata conform the wallet specification
module Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta (
    -- * Block metadata
    BlockMeta(..)
    -- ** Lenses
  , blockMetaSlotId
  ) where

import Universum

import Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as Map
import Data.SafeCopy (base, deriveSafeCopySimple)

import qualified Pos.Core as Core

import Ariadne.Wallet.Cardano.Kernel.DB.InDb

{-------------------------------------------------------------------------------
  Block metadata
-------------------------------------------------------------------------------}

-- | Block metadata
data BlockMeta = BlockMeta {
      -- | Slot each transaction got confirmed in
      _blockMetaSlotId :: InDb (Map Core.TxId Core.SlotId)
    }

makeLenses ''BlockMeta
deriveSafeCopySimple 1 'base ''BlockMeta

-- | Monoid instance to update 'BlockMeta' in 'applyBlock' (see wallet spec)
instance Monoid BlockMeta where
  mempty = BlockMeta {
        _blockMetaSlotId = InDb Map.empty
      }
  a `mappend` b = BlockMeta {
        _blockMetaSlotId = combineUsing (liftA2 Map.union) _blockMetaSlotId
      }
    where
      combineUsing :: (a -> a -> a) -> (BlockMeta -> a) -> a
      combineUsing op f = f a `op` f b
