-- | Resolved blocks and transactions
module Ariadne.Wallet.Cardano.Kernel.DB.Resolved
       ( -- * Resolved blocks and transactions
         ResolvedInput
       , ResolvedTx(..)
       , ResolvedBlock(..)
         -- ** Lenses
       , rtxInputs
       , rtxOutputs
       , rbTxs
       , rbSlot
       ) where

import Control.Lens.TH (makeLenses)
import qualified Data.Map as Map
import Data.SafeCopy (base, deriveSafeCopySimple)
import Formatting (bprint, (%))
import qualified Formatting.Buildable as Buildable

import Serokell.Util (listJson, mapJson)

import qualified Pos.Chain.Txp as Txp
import qualified Pos.Core as Core

import Ariadne.Wallet.Cardano.Kernel.DB.InDb

{-------------------------------------------------------------------------------
  Resolved blocks and transactions
-------------------------------------------------------------------------------}

-- | Resolved input
--
-- A transaction input @(h, i)@ points to the @i@th output of the transaction
-- with hash @h@, which is not particularly informative. The corresponding
-- 'ResolvedInput' is obtained by looking up what that output actually is.
type ResolvedInput = Txp.TxOutAux

-- | (Unsigned) transaction with inputs resolved
--
-- NOTE: We cannot recover the original transaction from a 'ResolvedTx'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedTx = ResolvedTx {
      -- | Transaction inputs
      _rtxInputs  :: InDb (NonEmpty (Txp.TxIn, ResolvedInput))

      -- | Transaction outputs
    , _rtxOutputs :: InDb Txp.Utxo
    }

-- | (Unsigned block) containing resolved transactions
--
-- NOTE: We cannot recover the original block from a 'ResolvedBlock'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedBlock = ResolvedBlock {
      -- | Transactions in the block
      _rbTxs  :: [ResolvedTx]

      -- | The `SlotId` of the slot this block appeared in
    , _rbSlot :: InDb Core.SlotId
    }

makeLenses ''ResolvedTx
makeLenses ''ResolvedBlock

deriveSafeCopySimple 1 'base ''ResolvedTx
deriveSafeCopySimple 1 'base ''ResolvedBlock

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable.Buildable ResolvedTx where
  build ResolvedTx{..} = bprint
    ( "ResolvedTx "
    % "{ inputs:  " % mapJson
    % ", outputs: " % mapJson
    % "}"
    )
    (Map.fromList (toList (_rtxInputs  ^. fromDb)))
    (_rtxOutputs ^. fromDb)

instance Buildable.Buildable ResolvedBlock where
  build ResolvedBlock{..} = bprint
    ( "ResolvedBlock "
    % "{ txs: " % listJson
    % "}"
    )
    _rbTxs
