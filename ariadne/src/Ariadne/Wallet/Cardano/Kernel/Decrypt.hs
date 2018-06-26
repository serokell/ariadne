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
  encToPublic, unpackHDAddressAttr, firstHardened, firstNonHardened)
import Serokell.Util (enumerate)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountIx(..), HdAddressChain(..), HdAddressIx(..))

data WAddressMeta = WAddressMeta
    { _wamAccountIndex :: HdAccountIx
    , _wamAddressChain :: HdAddressChain
    , _wamAddressIndex :: HdAddressIx
    , _wamAddress      :: Address
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
eskToHDPassphrase encSK = do
    let pubKey = encToPublic encSK
    let hdPass = deriveHDPassphrase pubKey
    hdPass

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
    derPath <- unpackHDAddressAttr hdPass hdPayload
    -- The bang is needed due to a GHC bug with ApplicativeDo
    -- (https://ghc.haskell.org/trac/ghc/ticket/14105, fixed in 8.4.1)
    ![purpose', coinType', accIdx', chainType, addrIdx] <- pure derPath

    purpose  <- fromHardened purpose'
    guard $ purpose  == bip44Purpose
    coinType <- fromHardened coinType'
    guard $ coinType == bip44CoinType

    hdAccountIx    <- HdAccountIx <$> fromHardened accIdx'
    hdAddressChain <- join $ mkAddressChain <$> fromNonHardened chainType
    hdAddressIx    <- HdAddressIx <$> fromNonHardened addrIdx

    -- In fact, this is almost HdAddressId, with the exception that
    -- we don't know the corresponding HdRootId at this point.
    pure $ WAddressMeta hdAccountIx hdAddressChain hdAddressIx addr
  where
    -- TODO (AD-219): use Word31 as output type
    fromNonHardened :: Word32 -> Maybe Word32
    fromNonHardened idx = do
        guard $ isNonHardened idx
        pure $ idx - firstNonHardened
      where
        isNonHardened :: Word32 -> Bool
        isNonHardened idx' = (firstNonHardened <= idx') && (idx' < firstHardened)

    fromHardened :: Word32 -> Maybe Word32
    fromHardened idx = do
        guard $ isHardened idx
        pure $ idx - firstHardened
      where
        isHardened :: Word32 -> Bool
        isHardened idx' = firstHardened <= idx'

    mkAddressChain :: Word32 -> Maybe HdAddressChain
    mkAddressChain 0 = Just HdChainExternal
    mkAddressChain 1 = Just HdChainInternal
    mkAddressChain _ = Nothing

    -- TODO (AD-219): refactor these constants together with similar ones in KeyStorage
    -- into a separate file
    bip44Purpose :: Word32
    bip44Purpose = 44

    bip44CoinType :: Word32
    bip44CoinType = 1815
