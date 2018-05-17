-- | UPDATE operations on HD wallets
module Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Update (
    updateHdRootAssurance
  , updateHdRootName
  , updateHdAccountName
  ) where

import Universum

import Control.Lens ((.=))

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update' HdWallets UnknownHdRoot ()
updateHdRootAssurance rootId assurance =
    zoomHdRootId identity rootId $
      hdRootAssurance .= assurance

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update' HdWallets UnknownHdRoot ()
updateHdRootName rootId name =
    zoomHdRootId identity rootId $
      hdRootName .= name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdWallets UnknownHdAccount ()
updateHdAccountName accId name =
    zoomHdAccountId identity accId $
      hdAccountName .= name
