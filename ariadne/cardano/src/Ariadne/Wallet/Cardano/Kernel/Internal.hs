{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Ariadne.Wallet.Cardano.Kernel.Internal
       ( -- * Passive wallet
         PassiveWallet(..)
       , walletKeystore
       , wallets
       , walletLogMessage
       , walletProtocolMagic
       ) where

import Control.Lens.TH (makeLenses)
import Data.Acid (AcidState)
import System.Wlog (Severity(..))

import Pos.Crypto (ProtocolMagic)

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB)
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
data PassiveWallet = PassiveWallet {
      -- | Send log message
      _walletLogMessage :: Severity -> Text -> IO ()
      -- | An opaque handle to a place where we store the 'EncryptedSecretKey'.
    , _walletKeystore :: Keystore
      -- | Database handle
    , _wallets :: AcidState DB
      -- | The protocol magic used by an `ActiveWallet` to make transactions.
    , _walletProtocolMagic :: ProtocolMagic

    }

makeLenses ''PassiveWallet
