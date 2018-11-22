module Ariadne.Wallet.Cardano.Kernel.Keystore.Util
       ( esksToMap
       , eskToPublicHash
       , walletIdToPublicHash
       ) where

import Pos.Core (AddressHash, addressHash)
import Pos.Crypto (EncryptedSecretKey(..), PublicKey, encToPublic)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (HdRootId(..))
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (fromDb)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

import qualified Data.Map as Map (fromList)

esksToMap :: [EncryptedSecretKey] -> Map (AddressHash PublicKey) EncryptedSecretKey
esksToMap = Map.fromList . map (\encKey -> (eskToPublicHash encKey, encKey))

eskToPublicHash :: EncryptedSecretKey -> AddressHash PublicKey
eskToPublicHash = addressHash . encToPublic

walletIdToPublicHash :: WalletId -> AddressHash PublicKey
walletIdToPublicHash (WalletIdHdSeq hdRootId) = view fromDb $ getHdRootId hdRootId
