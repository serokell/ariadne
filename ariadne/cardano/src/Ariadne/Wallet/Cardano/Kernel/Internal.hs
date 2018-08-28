{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Ariadne.Wallet.Cardano.Kernel.Internal (
    -- * Passive wallet
    PassiveWallet(..)
  , ActiveWallet(..)
  , walletKeystore
  , wallets
  , walletLogMessage
  ) where

import Control.Lens.TH

import System.Wlog (Severity(..))

import Data.Acid (AcidState)

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
      -- ^ Logger
    , _walletKeystore   :: Keystore
      -- ^ An opaque handle to a place where we store the 'EncryptedSecretKey'.
    , _wallets          :: AcidState DB
    }

makeLenses ''PassiveWallet

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive       :: PassiveWallet
    }
