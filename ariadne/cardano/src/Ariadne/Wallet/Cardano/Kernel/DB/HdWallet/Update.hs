-- | UPDATE operations on HD wallets
module Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Update (
    updateHdRoot
  , updateHdAccountName
  ) where

import Universum

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
import Ariadne.Wallet.Cardano.Kernel.Util (modifyAndGetNew)

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

-- | Updates in one gulp the Hd Wallet name and assurance level.
updateHdRoot :: HdRootId
             -> AssuranceLevel
             -> WalletName
             -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRoot rootId assurance name =
    zoomHdRootId identity rootId $ do
        modifyAndGetNew $ set hdRootAssurance assurance . set hdRootName name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdWallets UnknownHdAccount HdAccount
updateHdAccountName accId name = do
    zoomHdAccountId identity accId $ do
        oldAccount <- get
        let newAccount = oldAccount & hdAccountName .~ name
        put newAccount
        return newAccount
