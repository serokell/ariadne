{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeFamilies #-}

module Ariadne.Wallet.Cardano.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , prefilterBlock
       , ourUtxo
       ) where

import Universum

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import Formatting (bprint, (%))
import Serokell.Util (listJson, mapJson)

import Ariadne.Wallet.Cardano.Kernel.Decrypt
  (HDPassphrase, eskToHDPassphrase, selectOwnAddresses)
import Pos.Core (Address(..))
import Pos.Core.Txp (TxIn(..), TxOut(..), TxOutAux(..))
import Pos.Crypto (EncryptedSecretKey)
import Pos.Txp.Toil.Types (Utxo)

import Ariadne.Wallet.Cardano.Kernel.DB.InDb (fromDb)
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved
  (ResolvedBlock, ResolvedTx, rbTxs, rtxInputs, rtxOutputs)

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
+-------------------------------------------------------------------------------}

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbInputs  :: Set TxIn

      -- | Relevant outputs
    , pfbOutputs :: Utxo
    }

prefilterBlock :: EncryptedSecretKey
               -> ResolvedBlock
               -> PrefilteredBlock
prefilterBlock esk block = PrefilteredBlock {
      pfbInputs  = Set.fromList . map fst $ concat inpss
    , pfbOutputs = Map.unions outss
    }
  where
    inpss :: [[(TxIn, TxOutAux)]]
    outss :: [Utxo]
    (inpss, outss) = unzip $ map (prefilterTx hdPass) (block ^. rbTxs)

    hdPass :: HDPassphrase
    hdPass = eskToHDPassphrase esk

prefilterTx :: HDPassphrase
            -> ResolvedTx
            -> ([(TxIn, TxOutAux)], Utxo)
prefilterTx hdPass tx = (
      ourResolvedTxPairs hdPass (toList (tx ^. rtxInputs  . fromDb))
    , ourUtxo_           hdPass         (tx ^. rtxOutputs . fromDb)
    )

ourResolvedTxPairs :: HDPassphrase
                   -> [(TxIn, TxOutAux)]
                   -> [(TxIn, TxOutAux)]
ourResolvedTxPairs hdPass = ours hdPass (txOutAddress . toaOut . snd)

ourUtxo :: EncryptedSecretKey -> Utxo -> Utxo
ourUtxo esk = ourUtxo_ $ eskToHDPassphrase esk

ourUtxo_ :: HDPassphrase -> Utxo -> Utxo
ourUtxo_ hdPass utxo = Map.fromList $ ourResolvedTxPairs hdPass $ Map.toList utxo

ours :: HDPassphrase
     -> (a -> Address)
     -> [a]
     -> [a]
ours hdPass selectAddr rtxs = map fst $ selectOwnAddresses hdPass selectAddr rtxs

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable PrefilteredBlock where
  build PrefilteredBlock{..} = bprint
    ( "PrefilteredBlock "
    % "{ inputs:  " % listJson
    % ", outputs: " % mapJson
    % "}"
    )
    (Set.toList pfbInputs)
    pfbOutputs
