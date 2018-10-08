module Ariadne.Wallet.Cardano.Kernel.Bip39
       ( entropyToMnemonic
       , mnemonicToSeedNoPassword
       ) where

import Loot.Crypto.Bip39 (entropyToMnemonic, mnemonicToSeed)

-- The empty string below is called a passphrase in BIP-39, but
-- it's essentially an extra mnemonic word.
mnemonicToSeedNoPassword :: Text -> ByteString
mnemonicToSeedNoPassword mnemonic = mnemonicToSeed mnemonic ""
