-- | UPDATE operations on HD wallets
module Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Update (
    updateHdRootAssurance
  , updateHdRootName
  , updateHdAccountName
  ) where

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
import Ariadne.Wallet.Cardano.Kernel.Util (modifyAndGetNew)

{-------------------------------------------------------------------------------
  UPDATE
-------------------------------------------------------------------------------}

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRootAssurance rootId assurance =
    zoomHdRootId identity rootId $
      modifyAndGetNew $ set hdRootAssurance assurance

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update' HdWallets UnknownHdRoot HdRoot
updateHdRootName rootId name =
    zoomHdRootId identity rootId $
      modifyAndGetNew $ set hdRootName name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update' HdWallets UnknownHdAccount HdAccount
updateHdAccountName accId name = do
    zoomHdAccountId identity accId $ do
        oldAccount <- get
        let newAccount = oldAccount & hdAccountName .~ name
        put newAccount
        return newAccount
