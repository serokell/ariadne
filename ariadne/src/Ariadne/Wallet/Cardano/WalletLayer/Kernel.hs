{-# LANGUAGE ScopedTypeVariables #-}

module Ariadne.Wallet.Cardano.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import Universum

import Data.Acid (AcidState)
import Data.Maybe (fromJust)
import System.Wlog (Severity(Debug))
import Control.Concurrent.STM.TVar (TVar)

import Pos.Block.Types (Blund, Undo(..))

import qualified Ariadne.Wallet.Cardano.Kernel as Kernel
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB)
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved (ResolvedBlock)
import Ariadne.Wallet.Cardano.Kernel.Diffusion (WalletDiffusion(..))
import Ariadne.Wallet.Cardano.Kernel.Types
  (RawResolvedBlock(..), fromRawResolvedBlock)
import Ariadne.Wallet.Cardano.WalletLayer.Types
  (ActiveWalletLayer(..), PassiveWalletLayer(..))

import Pos.Core.Chrono (OldestFirst(..))
import Pos.Util.UserSecret (UserSecret)

import qualified Ariadne.Wallet.Cardano.Kernel.Actions as Actions

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m)
    => (Severity -> Text -> IO ())
    -> TVar UserSecret
    -> AcidState DB
    -> (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet logFunction us db f =
    Kernel.bracketPassiveWallet logFunction us db $ \w -> do

      -- Create the wallet worker and its communication endpoint `invoke`.
      invoke <- Actions.forkWalletWorker $ Actions.WalletActionInterp
               { Actions.applyBlocks  =  \blunds ->
                   Kernel.applyBlocks w $
                       OldestFirst (mapMaybe blundToResolvedBlock (toList (getOldestFirst blunds)))
               , Actions.switchToFork = \_ _ -> logFunction Debug "<switchToFork>"
               , Actions.emit         = logFunction Debug
               }

      f (passiveWalletLayer w invoke)

  where
    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer _wallet invoke =
        PassiveWalletLayer
            { _pwlApplyBlocks    = invoke . Actions.ApplyBlocks
            , _pwlRollbackBlocks = invoke . Actions.RollbackBlocks
            }

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToJust b <&> \mainBlock ->
            fromRawResolvedBlock
            $ UnsafeRawResolvedBlock mainBlock spentOutputs'
        where
            spentOutputs' = map (map fromJust) $ undoTx u
            rightToJust   = either (const Nothing) Just

-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadIO m, MonadMask m)
    => PassiveWalletLayer n
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletPassiveLayer _walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())
