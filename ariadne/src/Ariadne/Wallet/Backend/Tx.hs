-- | Part of the backend which deals with transactions.

module Ariadne.Wallet.Backend.Tx
       ( sendTx
       ) where

import Universum hiding (list)

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
import Control.Exception (Exception(displayException))
import Control.Lens (at, ix)
import Data.Acid (AcidState, query, update)
import Data.Default (def)
import qualified Data.IxSet.Typed as IxSet
import Data.Map (findWithDefault)
import Formatting (bprint, build, formatToString, int, (%))
import IiExtras ((:~>)(..))
import Pos.Client.KeyStorage (getSecretDefault)
import Pos.Client.Txp.Network (prepareMTx, submitTxRaw)
import Pos.Core.Txp (Tx(..), TxAux(..), TxOutAux(..))
import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, SafeSigner(..), checkPassMatches, hash)
import Pos.Crypto.HD (ShouldCheckPassphrase(..), deriveHDSecretKey)
import Pos.Infra.Diffusion.Types (Diffusion)
import Pos.Launcher (HasConfigurations)
import Pos.Util (maybeThrow)
import Pos.Util.UserSecret (usWallets)
import Text.PrettyPrint.ANSI.Leijen (Doc, list, softline, string)

import Ariadne.Cardano.Face
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Mode ()
import Ariadne.Wallet.Face

data SendTxException
    = SendTxNoWallet !Word
    | SendTxNoAddresses !Word
    | SendTxIncorrectPassPhrase
    deriving (Show)

instance Buildable SendTxException where
    build = \case
        SendTxNoWallet walletIdx ->
            bprint ("Wallet #"%int%" does not exist") walletIdx
        SendTxNoAddresses walletIdx ->
            bprint ("Wallet #"%int%" does not have any addresses") walletIdx
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
            walletAccounts :: [HdAccountId]
            walletAccounts = map _hdAccountId $ map unwrapOrdByPrimKey
                (toAccountsList $ fromRight
                    -- Resolved walletRootId always exists.
                    (error "SendTx: Unknown walletRootId")
                    (readAccountsByRootId walletRootId wallets))

        -- TODO: get UserSecret
        let us = undefined
            pubAddrHash = _fromDb (_unHdRootId walletRootId)
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
        -- TODO: pass an index from the UI list representation.
        let walletIdx = undefined
        ourAddresses <-
            maybeThrow
                (SendTxNoAddresses walletIdx)
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
walletSigners :: EncryptedSecretKey -> HdWallets -> PassPhrase -> [HdAccountId] -> HashMap Address SafeSigner
walletSigners encSK wallets pp walletAccounts = foldMap accountSigners walletAccounts
  where
    deriveSecretKey :: EncryptedSecretKey -> Word32 -> EncryptedSecretKey
    deriveSecretKey encSK idx =
        case deriveHDSecretKey (ShouldCheckPassphrase False) pp encSK idx of
            Nothing ->
                error
                    "walletSigners: passphrase is invalid and why was it checked at all??!!?"
            Just key -> key
    accountSigners :: HdAccountId -> HashMap Address SafeSigner
    accountSigners accId =

        let accIx = accId ^. hdAccountIdIx

            unAccountIx :: HdAccountIx -> Word32
            unAccountIx (HdAccountIx x) = x

            accountKey :: EncryptedSecretKey
            accountKey = deriveSecretKey encSK (unAccountIx accIx)

            toAddrPair :: HdAddress -> (Word32, Address)
            toAddrPair hdAddr =
                ( _unHdAddressIx (hdAddr ^. hdAddressId . hdAddressIdIx)
                , _fromDb (hdAddr ^. hdAddressAddress))

            accountAddresses :: [(Word32, Address)]
            accountAddresses = map toAddrPair $ map unwrapOrdByPrimKey (toAddressList getAddresses)

            getAddresses :: IxSet HdAddress
            getAddresses = fromRight
                (error "Bug: Unknown accountId")
                (readAddressesByAccountId accId wallets)

            step ::
                   HashMap Address SafeSigner
                -> (Word32, Address)
                -> HashMap Address SafeSigner
            step m (addrIdx, addr) =
                m &
                at addr .~
                Just (SafeSigner (deriveSecretKey accountKey addrIdx) pp)
         in foldl' step mempty accountAddresses
