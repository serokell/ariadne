module Ariadne.Wallet.Backend.AddressDiscovery
       ( AddressWithPathToUtxoMap
       , discoverHDAddressWithUtxo
       , discoverHDAddressesWithUtxo
       ) where

import Universum

import Control.Lens (at, non, (?~))
import Data.Conduit (runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import UnliftIO (MonadUnliftIO)
import qualified Data.List.NonEmpty as NE

import Pos.Core (AddrAttributes(..), Address, addrAttributesUnwrapped)
import Pos.Core.Txp (TxOutAux(..), txOutAddress)
import Pos.Crypto.HD (HDAddressPayload, HDPassphrase, unpackHDAddressAttr)
import Pos.DB.Class (MonadDBRead)
import Pos.Txp.DB (utxoSource)
import Pos.Txp.Toil.Types (Utxo)

import Ariadne.Wallet.Cardano.Kernel.Bip32 (DerivationPath)

type AddressWithPathToUtxoMap = Map (DerivationPath, Address) Utxo

discoverHDAddressWithUtxo ::
       (MonadDBRead m, MonadUnliftIO m)
    => HDPassphrase
    -> m AddressWithPathToUtxoMap
discoverHDAddressWithUtxo walletPassphrase =
    NE.head <$> discoverHDAddressesWithUtxo (one walletPassphrase)

-- | This is heavily based on @Pos.Crypto.HDDiscovery.discoverHDAddresses@.
discoverHDAddressesWithUtxo
    :: (MonadDBRead m, MonadUnliftIO m)
    => NonEmpty HDPassphrase
    -> m (NonEmpty AddressWithPathToUtxoMap)
discoverHDAddressesWithUtxo walletPassphrases =
    runConduitRes $ utxoSource .| CL.fold step initWallets
  where
    initWallets = map (const Map.empty) walletPassphrases
    outAddr = txOutAddress . toaOut . snd

    hdPayload :: Address -> Maybe HDAddressPayload
    hdPayload (addrAttributesUnwrapped -> AddrAttributes {..}) =
        aaPkDerivationPath

    insertMaybe
        :: OneItem Utxo
        -> (Maybe DerivationPath, AddressWithPathToUtxoMap)
        -> AddressWithPathToUtxoMap
    insertMaybe _ (Nothing, m) = m
    insertMaybe utxoItem@(k, v) (Just derPath, m) =
        -- This comes straight from the @Control.Lens@ doc on @non@:
        -- https://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Iso.html#v:non
        -- This code is equivalent to the following:
        -- 1.  Check whether @(derPath, address)@ is a key in @m@. If it is not,
        --     insert @Map.empty@ at this key.
        -- 2.  Insert @utxoItem@ into the map at the key @(derPath, address)@.
        m & at (derPath, outAddr utxoItem) . non Map.empty . at k ?~ v

    step
        :: NonEmpty AddressWithPathToUtxoMap
        -> OneItem Utxo
        -> NonEmpty AddressWithPathToUtxoMap
    step res utxoItem =
        case hdPayload (outAddr utxoItem) of
            Just payload -> do
                let unpackResults :: NonEmpty (Maybe DerivationPath)
                    unpackResults = map (flip unpackHDAddressAttr payload) walletPassphrases
                map (insertMaybe utxoItem) $ NE.zip unpackResults res
            _ -> res
