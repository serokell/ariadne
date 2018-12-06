-- | Wallet restoration logic.

module Ariadne.Wallet.Backend.Restore
       ( restoreWallet
       , restoreFromKeyFile
       ) where

import qualified Universum.Unsafe as Unsafe (init)

import Control.Natural (type (~>))

import Pos.Util.BackupPhrase (BackupPhrase(..))
import Pos.Core.Configuration (HasConfiguration)
import Pos.Crypto (PassPhrase)

import Ariadne.Cardano.Face (CardanoMode)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (WalletName(..))
import Ariadne.Wallet.Cardano.Kernel.Restore (RestoreFrom(..), WrongMnemonic(..))
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))
import Ariadne.Wallet.Face (Mnemonic(..))

restoreWallet ::
       HasConfiguration
    => PassiveWalletLayer IO
    -> (CardanoMode ~> IO)
    -> IO PassPhrase
    -> Maybe WalletName
    -> Mnemonic
    -> IO ()
restoreWallet pwl runCardanoMode getPassTemp mbWalletName mnemonic = do
    pp <- getPassTemp
    rFrom <- getMnemonicRestore mnemonic pp
    pwlRestoreWallet pwl runCardanoMode rFrom (getWalletName mbWalletName)

restoreFromKeyFile ::
       HasConfiguration
    => PassiveWalletLayer IO
    -> (CardanoMode ~> IO)
    -> Maybe WalletName
    -> FilePath
    -> IO ()
restoreFromKeyFile pwl runCardanoMode mbWalletName path =
    let walletName = getWalletName mbWalletName
    in pwlRestoreWallet pwl runCardanoMode (RestoreFromKeyFile path) walletName

getMnemonicRestore ::
       MonadThrow m
    => Mnemonic
    -> PassPhrase
    -> m RestoreFrom
getMnemonicRestore (Mnemonic mnemonic) pp = do
    let mnemonicWords = words mnemonic

    let isAriadneMnemonic = fromMaybe False $ do
          lastWord <- last <$> nonEmpty mnemonicWords
          pure (lastWord == "ariadne-v0") -- TODO AD-124: version parsing?

    rFromEither <- if
        | isAriadneMnemonic ->
            pure . Left $ Mnemonic (unwords $ Unsafe.init mnemonicWords)
        | length mnemonicWords == 12 ->
            pure . Right $ BackupPhrase mnemonicWords
        | otherwise -> throwM $ WrongMnemonic "Unknown mnemonic type"

    pure $ RestoreFromMnemonic rFromEither pp

getWalletName :: Maybe WalletName -> WalletName
getWalletName =  fromMaybe (WalletName "Restored wallet")
