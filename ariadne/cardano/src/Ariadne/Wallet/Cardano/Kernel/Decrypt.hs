-- | Contains some utils to decrypt HD payload of addresses.
-- Also utils to create THEntryExtra which based on
-- decrypting of HDPayload.

module Ariadne.Wallet.Cardano.Kernel.Decrypt
       ( WAddressMeta (..)

       , THEntryExtra (..)
       , isTxEntryInteresting
       , buildTHEntryExtra

       , HDPassphrase
       , eskToHDPassphrase
       , selectOwnAddresses
       , decryptAddress
       ) where

import Universum

import qualified Data.List.NonEmpty as NE
import Pos.Client.Txp.History (TxHistoryEntry(..))
import Pos.Core
  (Address(..), ChainDifficulty, Timestamp, aaPkDerivationPath,
  addrAttributesUnwrapped)
import Pos.Core.Txp
  (Tx(..), TxIn(..), TxOut, TxOutAux(..), TxUndo, toaOut, txOutAddress)
import Pos.Crypto
  (EncryptedSecretKey, HDPassphrase, WithHash(..), deriveHDPassphrase,
  encToPublic, unpackHDAddressAttr)
import Serokell.Util (enumerate)

import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), decodeBip44DerivationPath)

data WAddressMeta = WAddressMeta
    { _wamDerivationPath :: Bip44DerivationPath
    , _wamAddress        :: Address
    } deriving (Eq, Ord, Show, Generic, Typeable)

type OwnTxInOuts = [((TxIn, TxOutAux), WAddressMeta)]

-- | Auxiliary datatype which holds TxIns and TxOuts
-- belonging to some wallet.
data THEntryExtra = THEntryExtra
    { theeInputs  :: OwnTxInOuts
    -- ^ Inputs and corresponding outputs of tx
    -- which belong to wallet
    , theeOutputs :: OwnTxInOuts
    -- ^ Outputs and corresponding inputs of tx
    -- which belong to wallet
    , theeTxEntry :: TxHistoryEntry
    -- ^ Tx entry for history, likely it's not our entry
    }

isTxEntryInteresting :: THEntryExtra -> Maybe TxHistoryEntry
isTxEntryInteresting THEntryExtra{..} =
    if (not $ null theeInputs) || (not $ null theeOutputs) then Just theeTxEntry
    else Nothing

buildTHEntryExtra
    :: HDPassphrase
    -> (WithHash Tx, TxUndo)
    -> (Maybe ChainDifficulty, Maybe Timestamp)
    -> THEntryExtra
buildTHEntryExtra hdPass (WithHash tx txId, NE.toList -> undoL) (mDiff, mTs) =
    let (UnsafeTx (NE.toList -> inps) (NE.toList -> outs) _) = tx
        toTxInOut (idx, out) = (TxInUtxo txId idx, TxOutAux out)

        resolvedInputs :: [(TxIn, TxOutAux)]
        resolvedInputs = catMaybes (zipWith (fmap . (,)) inps undoL)
        txOutgoings = map txOutAddress outs
        txInputs = map (toaOut . snd) resolvedInputs

        theeInputs :: [((TxIn, TxOutAux), WAddressMeta)]
        theeInputs = selectOwnAddresses hdPass (txOutAddress . toaOut . snd) resolvedInputs
        theeOutputsRaw :: [((Word32, TxOut), WAddressMeta)]
        theeOutputsRaw = selectOwnAddresses hdPass (txOutAddress . snd) (enumerate outs)
        theeOutputs = map (first toTxInOut) theeOutputsRaw
        theeTxEntry = THEntry txId tx mDiff txInputs txOutgoings mTs in
    THEntryExtra {..}

eskToHDPassphrase :: EncryptedSecretKey -> HDPassphrase
eskToHDPassphrase = deriveHDPassphrase . encToPublic

selectOwnAddresses
    :: HDPassphrase
    -> (a -> Address)
    -> [a]
    -> [(a, WAddressMeta)]
selectOwnAddresses hdPass getAddr =
    mapMaybe (\a -> (a,) <$> decryptAddress hdPass (getAddr a))

decryptAddress :: HDPassphrase -> Address -> Maybe WAddressMeta
decryptAddress hdPass addr = do
    hdPayload <- aaPkDerivationPath $ addrAttributesUnwrapped addr
    derPathList <- unpackHDAddressAttr hdPass hdPayload
    derPath <- decodeBip44DerivationPath derPathList
    pure $ WAddressMeta derPath addr
