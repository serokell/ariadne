-- | Part of the backend which deals with transactions.

module Ariadne.Wallet.Backend.Tx
       ( sendTx
       ) where

import Universum hiding (list)

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable

import Control.Exception (Exception(displayException))
import Control.Lens (at)
import Data.Acid (AcidState, query)
import Data.Default (def)
import Data.Map (findWithDefault)
import Formatting (bprint, build, formatToString, (%))
import IiExtras ((:~>)(..))
import Pos.Client.KeyStorage (getSecretDefault)
import Pos.Client.Txp.Network (prepareMTx, submitTxRaw)
import Pos.Core.Txp (Tx(..), TxAux(..), TxOutAux(..))
import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, SafeSigner(..), checkPassMatches, hash)
import Pos.Crypto.HD (ShouldCheckPassphrase(..))
import Pos.Infra.Diffusion.Types (Diffusion)
import Pos.Launcher (HasConfigurations)
import Pos.Util (maybeThrow)
import Pos.Util.UserSecret (usWallets)
import Text.PrettyPrint.ANSI.Leijen (Doc, list, softline, string)

import Ariadne.Cardano.Face
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Mode ()
import Ariadne.Wallet.Cardano.Kernel.Bip32
import Ariadne.Wallet.Cardano.Kernel.Bip44
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet)
import Ariadne.Wallet.Face

data SendTxException
    = SendTxNoAddresses !HdRootId
    | SendTxIncorrectPassPhrase
    deriving (Show)

instance Buildable SendTxException where
    build = \case
        SendTxNoAddresses walletIdx ->
            bprint ("Wallet #"%build%" does not have any addresses") walletIdx
        SendTxIncorrectPassPhrase ->
            "Incorrect passphrase"

instance Exception SendTxException where
    displayException = toString . prettyL

-- | Send a transaction from selected to wallet to the list of
-- 'TxOut's.  If list of accounts is not empty, only those accounts
-- may be used as inputs.  If this list is empty and an account from
-- the input wallet it selected, this account will be used as input.
-- Otherwise inputs will be selected from all accounts in the wallet.
sendTx ::
       (HasConfigurations)
    => AcidState DB
    -> WalletFace
    -> CardanoFace
    -> IORef (Maybe WalletSelection)
    -> (Doc -> IO ())
    -> PassPhrase
    -> WalletReference
    -> [LocalAccountReference]
    -> NonEmpty TxOut
    -> IO TxId
sendTx acidDb WalletFace {..} CardanoFace {..} walletSelRef printAction pp walletRef _accRefs outs = do
    let Nat runCardanoMode = cardanoRunCardanoMode
    walletDb <- query acidDb Snapshot
    let wallets = walletDb ^. dbHdWallets
    walletRootId <- resolveWalletRef walletSelRef walletRef walletDb
    runCardanoMode $ sendTxDo wallets walletRootId =<< cardanoGetDiffusion
  where
    sendTxDo :: HdWallets -> HdRootId -> Diffusion CardanoMode -> CardanoMode TxId
    sendTxDo wallets walletRootId diffusion = do
        let
            walletAccounts :: IxSet HdAccount
            walletAccounts = fromRight
                -- Resolved walletRootId always exists.
                (error "SendTx: Unknown walletRootId")
                (readAccountsByRootId walletRootId wallets)

        us <- getSecretDefault
        let pubAddrHash = _fromDb (_unHdRootId walletRootId)
            -- Wallets creation and deletion organized in a such way that
            -- an absence of a key is not possible.
            esk = findWithDefault
                (error "Bug: _usWallets has no such key.")
                pubAddrHash
                (us ^. usWallets)

        maybeThrow SendTxIncorrectPassPhrase $
            checkPassMatches pp esk
        let signersMap :: HashMap Address SafeSigner
            -- safe due to passphrase check above
            signersMap = walletSigners esk wallets pp walletAccounts
        let getSigner :: Address -> Maybe SafeSigner
            getSigner addr = signersMap ^. at addr
        -- TODO [AD-234]: generate new change address
        ourAddresses <-
            maybeThrow
                (SendTxNoAddresses walletRootId)
                (nonEmpty $ HM.keys signersMap)
        let ourAddress = NE.head ourAddresses
        (txAux, _) <-
            prepareMTx
                getSigner
                mempty
                def
                ourAddresses
                (map TxOutAux outs)
                ourAddress
        let tx = taTx txAux
        let txId = hash tx
        liftIO $ printAction $ formatSubmitTxMsg tx
        txId <$ submitTxRaw diffusion txAux

    formatToDoc :: forall a. Buildable a => a -> Doc
    formatToDoc = string . formatToString build

    formatSubmitTxMsg :: Tx -> Doc
    formatSubmitTxMsg UnsafeTx {..} = mconcat
        [ "Submitting Tx with inputs: "
        , list . toList $ map formatToDoc _txInputs
        , ","
        , softline
        , "outputs: "
        , list . toList $ map formatToDoc _txOutputs
        , "…"
        ]

-- Assumes the passphrase is correct!
walletSigners :: EncryptedSecretKey -> HdWallets -> PassPhrase -> IxSet HdAccount -> HashMap Address SafeSigner
walletSigners rootSK wallets pp = foldMap accountSigners
  where
    -- See the contract of this function
    shouldn'tCheck = ShouldCheckPassphrase False
    invalidPassphrase :: a
    invalidPassphrase = error
        "walletSigners: passphrase is invalid and why was it checked at all??!!?"

    deriveAccountKey :: HdAccountIx -> EncryptedSecretKey
    deriveAccountKey accIdx =
        let derPath = encodeBip44DerivationPathToAccount accIdx
        in case deriveHDSecretKeyByPath shouldn'tCheck pp rootSK derPath of
               Nothing -> invalidPassphrase
               Just key -> key

    deriveAddressKey :: EncryptedSecretKey -> HdAddressChain -> HdAddressIx -> EncryptedSecretKey
    deriveAddressKey accKey addrChain addrIx =
        let derPath = encodeBip44DerivationPathFromAccount addrChain addrIx
        in case deriveHDSecretKeyByPath shouldn'tCheck pp accKey derPath of
               Nothing -> invalidPassphrase
               Just key -> key

    accountSigners :: HdAccount -> HashMap Address SafeSigner
    accountSigners hdAcc =
        let
            accId = hdAcc ^. hdAccountId
            accIx = accId ^. hdAccountIdIx

            accountKey :: EncryptedSecretKey
            accountKey = deriveAccountKey accIx

            toAddrTuple :: HdAddress -> (HdAddressChain, HdAddressIx, Address)
            toAddrTuple hdAddr =
                let addrId = hdAddr ^. hdAddressId
                    addrIx = addrId ^. hdAddressIdIx
                in ( undefined
                   , addrIx
                   , _fromDb (hdAddr ^. hdAddressAddress)
                   )

            accountAddresses :: [(HdAddressChain, HdAddressIx, Address)]
            accountAddresses = map toAddrTuple $ toList addressesSet

            addressesSet :: IxSet HdAddress
            addressesSet = fromRight
                (error "Bug: Unknown accountId")
                (readAddressesByAccountId accId wallets)

            step ::
                   HashMap Address SafeSigner
                -> (HdAddressChain, HdAddressIx, Address)
                -> HashMap Address SafeSigner
            step m (addrChain, addrIx, addr) =
                m &
                at addr .~
                Just (SafeSigner (deriveAddressKey accountKey addrChain addrIx) pp)
         in foldl' step mempty accountAddresses
