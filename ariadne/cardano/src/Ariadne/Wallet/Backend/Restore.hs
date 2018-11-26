-- | Wallet restoration logic.

module Ariadne.Wallet.Backend.Restore
       ( restoreWallet
       , restoreFromKeyFile
       ) where

import Control.Exception (Exception(displayException))
import Control.Natural (type (~>))

import Pos.Core.Configuration (HasConfiguration)
import Pos.Crypto (EncryptedSecretKey, PassPhrase)
import qualified Pos.Crypto as Crypto (checkPassMatches, emptyPassphrase)

import Ariadne.Cardano.Face (CardanoMode)
import Ariadne.Wallet.Backend.KeyStorage (addWallet)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (AssuranceLevel(..), WalletName(..))
import Ariadne.Wallet.Cardano.Kernel.Restore
  (getKeyFromMnemonic, getUtxoByAccount, readNonAriadneKeys)
import Ariadne.Wallet.Cardano.Kernel.Wallets
  (CreateWithAddress(..), HasNonemptyPassphrase(..), mkHasPP)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))
import Ariadne.Wallet.Face (Mnemonic(..), WalletFace(..), WalletRestoreType(..))

newtype WrongMnemonic = WrongMnemonic Text
 deriving (Eq, Show)

instance Exception WrongMnemonic where
  displayException (WrongMnemonic txt) =
    "Wrong mnemonic: " <> show txt

data SecretsDecodingError = SecretsDecodingError FilePath Text
  deriving (Eq, Show)

instance Exception SecretsDecodingError where
  displayException (SecretsDecodingError path txt) =
    "Failed to decode " <> path <> ": " <> show txt

restoreWallet ::
       HasConfiguration
    => PassiveWalletLayer IO
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> IO PassPhrase
    -> Maybe WalletName
    -> Mnemonic
    -> WalletRestoreType
    -> IO ()
restoreWallet pwl face runCardanoMode getPassTemp mbWalletName (Mnemonic mnemonic) rType = do
    pp <- getPassTemp
    let mnemonicWords = words mnemonic
        isAriadneMnemonic = fromMaybe False $ do
          lastWord <- last <$> nonEmpty mnemonicWords
          pure (lastWord == "ariadne-v0") -- TODO AD-124: version parsing?
    esk <- getKeyFromMnemonic isAriadneMnemonic mnemonicWords pp
    let hasPP = mkHasPP pp
    restoreFromSecretKey pwl face runCardanoMode mbWalletName esk rType hasPP assurance
  where
    -- TODO(AD-251): allow selecting assurance.
    assurance = AssuranceLevelNormal

restoreFromKeyFile ::
       HasConfiguration
    => PassiveWalletLayer IO
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> Maybe WalletName
    -> FilePath
    -> WalletRestoreType
    -> IO ()
restoreFromKeyFile pwl face runCardanoMode mbWalletName path rType = do
    esks <- readNonAriadneKeys path
    let templateName i (WalletName n) = WalletName $ n <> " " <> pretty i
    traverse_
        (\(i,esk) -> do
            let hasPP = HasNonemptyPassphrase $
                    isNothing $ Crypto.checkPassMatches Crypto.emptyPassphrase esk
            restoreFromSecretKey
                pwl
                face
                runCardanoMode
                (templateName i <$> mbWalletName)
                esk
                rType
                hasPP
                assurance)
        (zip [(0 :: Int)..] esks)
  where
    -- TODO(AD-251): allow selecting assurance.
    assurance = AssuranceLevelNormal

restoreFromSecretKey ::
       HasConfiguration
    => PassiveWalletLayer IO
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> Maybe WalletName
    -> EncryptedSecretKey
    -> WalletRestoreType
    -> HasNonemptyPassphrase
    -> AssuranceLevel
    -> IO ()
restoreFromSecretKey pwl face runCardanoMode mbWalletName esk rType hasPP assurance = do
    utxoByAccount <- getUtxoByAccount runCardanoMode esk rType
    addWallet pwl face esk mbWalletName utxoByAccount hasPP WithoutAddress assurance
