module Ariadne.Wallet.Backend.Util
       ( allAccountIds
       , accountsToUse
       , AccountsToUseException(..)
       ) where

import Control.Lens (ix)
import Fmt (pretty)
import Formatting (bprint, build, int, (%))
import Formatting.Buildable (Buildable)
import qualified Formatting.Buildable

import Pos.Util (maybeThrow)

import Ariadne.Wallet.Backend.KeyStorage (resolveWalletRefThenRead)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccount, HdAccountId, HdRootId, hdAccountId, hdAccountIdParent)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read (readAccountsByRootId)
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet, (@+))
import Ariadne.Wallet.Cardano.WalletLayer (PassiveWalletLayer(..))
import Ariadne.Wallet.Face
  (LocalAccountReference(..), WalletReference, WalletSelection(WSAccount))


data AccountsToUseException = AccountsToUseIndexOutOfRange !HdRootId !Word
                       deriving (Show)

instance Buildable AccountsToUseException where
    build (AccountsToUseIndexOutOfRange rootId idx) =
        bprint ("Account #"%int%" doesn't exist in "%build) idx rootId

instance Exception AccountsToUseException where
    displayException = pretty

-- | Used for fee estimate
allAccountIds ::
       PassiveWalletLayer IO
    -> IORef (Maybe WalletSelection)
    -> WalletReference
    -> [LocalAccountReference]
    -> IO [HdAccountId]
allAccountIds pwl walletSelRef walletRef accRefs =
         fmap (^. hdAccountId) . toList . snd
     <$> accountsToUse pwl walletSelRef walletRef accRefs

accountsToUse ::
       PassiveWalletLayer IO
    -> IORef (Maybe WalletSelection)
    -> WalletReference
    -> [LocalAccountReference]
    -> IO (HdRootId, IxSet HdAccount)
accountsToUse pwl walletSelRef walletRef accRefs = do
    snapshot <- pwlGetDBSnapshot pwl
    (walletRootId, walletAccounts) <-
        resolveWalletRefThenRead walletSelRef walletRef
        snapshot readAccountsByRootId
    accountIds <- getSuitableAccounts walletRootId walletAccounts
    let filteredAccounts :: IxSet HdAccount
        filteredAccounts = maybe identity filterAccounts accountIds walletAccounts

        filterAccounts :: NonEmpty HdAccountId -> IxSet HdAccount -> IxSet HdAccount
        filterAccounts ids accounts = accounts @+ toList ids
    return (walletRootId, filteredAccounts)
  where

    -- Returns list of accounts which can be used.
    -- 'Nothing' means all accounts can be used.
    getSuitableAccounts ::
           HdRootId
        -> IxSet HdAccount
        -> IO (Maybe (NonEmpty HdAccountId))
    getSuitableAccounts rootId accountsSet =
        case nonEmpty accRefs of
            Nothing -> do
                readIORef walletSelRef >>= \case
                    Just (WSAccount selectedAccId)
                        | selectedAccId ^. hdAccountIdParent == rootId ->
                            return (Just $ one selectedAccId)
                    _ -> return Nothing
            Just accRefsNE -> Just <$> mapM refToId accRefsNE
      where
        -- Here we use knowledge of the order in UI, which is not very good.
        accountsList :: [HdAccount]
        accountsList = toList accountsSet

        refToId :: LocalAccountReference -> IO HdAccountId
        refToId (LocalAccountRefByIndex uiIdx) =
            maybeThrow (AccountsToUseIndexOutOfRange rootId uiIdx) $
            accountsList ^? ix (fromIntegral uiIdx) . hdAccountId
