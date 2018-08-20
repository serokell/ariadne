{-# LANGUAGE ScopedTypeVariables #-}

module Ariadne.Wallet.Cardano.WalletLayer.Kernel
    ( passiveWalletLayerComponent
    , activeWalletLayerComponent
    ) where

import Universum

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Component (ComponentM)
import Data.Acid (AcidState)
import Data.Maybe (fromJust)
import System.Wlog (Severity(Debug))

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
passiveWalletLayerComponent
    :: forall n. (MonadIO n)
    => (Severity -> Text -> IO ())
    -> TVar UserSecret
    -> AcidState DB
    -> ComponentM (PassiveWalletLayer n)
passiveWalletLayerComponent logFunction us db = do
    w <- Kernel.passiveWalletComponent logFunction us db

    -- Create the wallet worker and its communication endpoint `invoke`.
    invoke <- Actions.forkWalletWorker $ Actions.WalletActionInterp
            { Actions.applyBlocks  =  \blunds ->
                Kernel.applyBlocks w $
                    OldestFirst (mapMaybe blundToResolvedBlock (toList (getOldestFirst blunds)))
            , Actions.switchToFork = \_ _ -> logFunction Debug "<switchToFork>"
            , Actions.emit         = logFunction Debug
            }

    return $ passiveWalletLayer w invoke
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
activeWalletLayerComponent ::
       forall n.
       PassiveWalletLayer n
    -> WalletDiffusion
    -> ComponentM (ActiveWalletLayer n)
activeWalletLayerComponent walletPassiveLayer _walletDiffusion =
    return ActiveWalletLayer{..}
