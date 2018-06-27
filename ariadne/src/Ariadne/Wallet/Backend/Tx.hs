-- | Part of the backend which deals with transactions.

module Ariadne.Wallet.Backend.Tx
       ( sendTx
       ) where

import Universum hiding (list)

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable

import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Control.Exception (Exception(displayException))
import Control.Lens (at, ix)
import Data.Default (def)
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
    walletDb <- query acidDB Snapshot
    walletRootId <- resolveWalletRef walletSelRef walletDb walletRef
    runCardanoMode $ sendTxDo walletRootId =<< cardanoGetDiffusion
  where
    sendTxDo :: HdRootId -> Diffusion CardanoMode -> CardanoMode TxId
    sendTxDo walletRootId diffusion = do
        let
            wallets = walletDb ^. hdWallets

            walletAccounts :: [HdAccountId]
            walletAccounts = map _hdAccountId $ IxSet.toList
              fromRight
              -- Resolved walletRootId always exists.
                (error "SendTx: Unknown walletRootId")
                (readAccountsByRootId walletRootId wallets)

        -- TODO: get UserSecret
        let us = undefined
            pubAddrHash = fromDb walletRootId
        -- Wallets creation and deletion organized in a such way that
        -- an absence of a key is not possible.
            esk = findWithDefault
                (error "Bug: _usWallets has no such key.")
                (us ^. usWallets) pubAddrHash

        maybeThrow SendTxIncorrectPassPhrase $
            checkPassMatches pp esk
        let signersMap :: HashMap Address SafeSigner
            -- safe due to passphrase check above
            signersMap = walletSigners wallets pp walletAccounts
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
walletSigners :: HdWallets -> PassPhrase -> [HdAccountId] -> HashMap Address SafeSigner
walletSigners wallets pp walletAccounts = foldMap accountSigners walletAccounts
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

        let walletKey :: EncryptedSecretKey
            walletKey = accId ^. hdAccountIdParent

            accIx = accId ^. hdAccountIdIx

            unAccountIx :: HdAccountIx -> Word32
            unAccountIx (HdAccountIx x) = x

            accountKey :: EncryptedSecretKey
            accountKey = deriveSecretKey walletKey (unAccountIx accIx)

            fromDb :: InDb Address -> Address
            fromDb (InDb x) = x

            toAddrPair :: HdAddress -> (Word32, Address)
            toAddrPair hdAddr =
                ( unHdAddressIdIx (hdAddr ^. hdAddressId . hdAddressIdIx)
                , fromDb (hdAddr ^. hdAddressAddress))

            accountAddress :: [(Word32, Address)]
            accountAddress = map toAddrPair $ IxSet.toList $ fromRight
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
