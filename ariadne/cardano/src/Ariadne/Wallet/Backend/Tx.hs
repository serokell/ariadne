-- | Part of the backend which deals with transactions.

module Ariadne.Wallet.Backend.Tx
       ( sendTx
       ) where

import Prelude hiding (list)

import qualified Data.Text.Buildable

import Control.Exception (Exception(displayException))
import Control.Lens (at, ix)
import Control.Natural ((:~>)(..))
import Formatting (bprint, build, formatToString, (%))
import Text.PrettyPrint.ANSI.Leijen (Doc, list, softline, string)

import Pos.Client.Txp.Network (prepareMTx, submitTxRaw)
import Pos.Core.Txp (Tx(..), TxAux(..), TxOutAux(..))
import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, SafeSigner(..), checkPassMatches, hash)
import Pos.Crypto.HD (ShouldCheckPassphrase(..))
import Pos.Infra.Diffusion.Types (Diffusion)
import Pos.Launcher (HasConfigurations)
import Pos.Util (maybeThrow)

import Ariadne.Cardano.Face
import Ariadne.Cardano.Knit (showCoin)
import Ariadne.Wallet.Backend.Mode ()
import Ariadne.Wallet.Backend.Util (accountsToUse)
import Ariadne.Wallet.Cardano.Kernel.Bip32
  (DerivationPath(..), deriveHDSecretKeyByPath)
import Ariadne.Wallet.Cardano.Kernel.Bip44
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet)
import Ariadne.Wallet.Cardano.WalletLayer
  (ActiveWalletLayer(..), PassiveWalletLayer(..))
import Ariadne.Wallet.Face

data SendTxException
    = SendTxNoAddresses !HdRootId
    | SendTxNoAccounts !HdRootId
    | SendTxIncorrectPassPhrase
    | SendTxNotConfirmed
    deriving (Show)

instance Buildable SendTxException where
    build = \case
        SendTxNoAccounts walletIdx ->
            bprint ("Wallet "%build%" does not have any accounts") walletIdx
        SendTxNoAddresses walletIdx ->
            bprint ("Wallet "%build%" does not have any addresses") walletIdx
        SendTxIncorrectPassPhrase ->
            "Incorrect passphrase"
        SendTxNotConfirmed ->
            "Not confirmed by User"

instance Exception SendTxException where
    toException e = case e of
        SendTxIncorrectPassPhrase -> walletPassExceptionToException e
        _ -> SomeException e
    fromException = walletPassExceptionFromException
    displayException = toString . prettyL

-- | Send a transaction from selected to wallet to the list of
-- 'TxOut's.  If list of accounts is not empty, only those accounts
-- may be used as inputs.  If this list is empty and an account from
-- the input wallet is selected, this account will be used as input.
-- Otherwise inputs will be selected from all accounts in the wallet.
sendTx
    :: HasConfigurations
    => ActiveWalletLayer IO
    -> WalletFace
    -> CardanoFace
    -> IORef (Maybe WalletSelection)
    -> (Doc -> IO ())
    -> (WalletReference -> IO PassPhrase)
    -> (WalletReference -> IO TxId -> IO TxId)
    -> (ConfirmationType -> IO Bool)
    -> Bool
    -> WalletReference
    -> [LocalAccountReference]
    -> InputSelectionPolicy
    -> NonEmpty TxOut
    -> IO TxId
sendTx
    awl
    WalletFace {..}
    CardanoFace {..}
    walletSelRef
    printAction
    getPassPhrase
    voidWrongPass
    waitUiConfirm
    noConfirm
    walletRef
    accRefs
    isp
    outs
  = do
    let NT runCardanoMode = cardanoRunCardanoMode
    walletDb <- pwlGetDBSnapshot pwl
    let wallets = walletDb ^. dbHdWallets
    (walletRootId, accountsToUse') <- accountsToUse pwl walletSelRef walletRef accRefs
    unless noConfirm $
        unlessM (waitUiConfirm . ConfirmSend . map txOutToInfo $ toList outs) $
            throwM SendTxNotConfirmed
    pp <- getPassPhrase $ WalletRefByHdRootId walletRootId
    voidWrongPass walletRef . runCardanoMode $
        sendTxDo wallets walletRootId pp accountsToUse' =<< cardanoGetDiffusion
  where
    pwl :: PassiveWalletLayer IO
    pwl = walletPassiveLayer awl

    sendTxDo ::
           HdWallets
        -> HdRootId
        -> PassPhrase
        -> IxSet HdAccount
        -> Diffusion CardanoMode
        -> CardanoMode TxId
    sendTxDo wallets walletRootId pp accountsToUse' diffusion = do
        -- Wallet creation and deletion is organized in such way that
        -- the absence of a key is not possible.
        esk <- liftIO $ fromMaybe
            (error "Bug: Keystore has no such key.")
            <$> pwlLookupKeystore pwl walletRootId

        maybeThrow SendTxIncorrectPassPhrase $
            checkPassMatches pp esk
        let signersMap :: HashMap Address SafeSigner
            -- safe due to passphrase check above
            signersMap = walletSigners esk wallets pp accountsToUse'
        let getSigner :: Address -> Maybe SafeSigner
            getSigner addr = signersMap ^. at addr
        ourAddresses <-
            maybeThrow
                (SendTxNoAddresses walletRootId)
                (nonEmpty $ keys signersMap)
        changeAccountId <- changeAccountIdFromAccounts
        let newChangeAddress = walletNewAddress
                (AccountRefByHdAccountId changeAccountId)
                HdChainInternal
        (txAux, _) <-
            prepareMTx
                cardanoProtocolMagic
                getSigner
                mempty
                isp
                ourAddresses
                (map TxOutAux outs)
                (const newChangeAddress)
        -- [AD-518] TODO: call newPending even when inputs are selected from multiple accounts.
        when (length accountsToUse' == 1) $
            liftIO $ whenLeftM (awlNewPending awl changeAccountId txAux) throwM
        let tx = taTx txAux
        let txId = hash tx
        liftIO $ printAction $ formatSubmitTxMsg tx
        txId <$ submitTxRaw diffusion txAux

      where
        -- | Used for sending tx with existing accountsToUse
        changeAccountIdFromAccounts :: MonadThrow m
                                 => m HdAccountId
        changeAccountIdFromAccounts = do
            -- We pick one of the accounts to generate change address in it.
            changeAccount <-
                maybeThrow
                    (SendTxNoAccounts walletRootId)
                    (toList accountsToUse'  ^? ix 0)
            return $ changeAccount ^. hdAccountId

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

    txOutToInfo :: TxOut -> ConfirmSendInfo
    txOutToInfo TxOut{..} = ConfirmSendInfo {..}
      where
        confirmSendAddress = show $ formatToDoc txOutAddress
        (confirmSendAmount, outCoin) = showCoin $ txOutValue
        confirmSendCoin = show outCoin

-- Assumes the passphrase is correct!
walletSigners :: EncryptedSecretKey -> HdWallets -> PassPhrase -> IxSet HdAccount -> HashMap Address SafeSigner
walletSigners rootSK wallets pp = foldMap accountSigners
  where
    -- See the contract of this function
    shouldNotCheck = ShouldCheckPassphrase False
    invalidPassphrase :: a
    invalidPassphrase = error
        "walletSigners: passphrase is invalid and why was it checked at all??!!?"

    deriveAccountKey :: HdAccountIx -> EncryptedSecretKey
    deriveAccountKey accIdx =
        let derPath = DerivationPath $ encodeBip44DerivationPathToAccount accIdx
        in case deriveHDSecretKeyByPath shouldNotCheck pp rootSK derPath of
            Nothing -> invalidPassphrase
            Just key -> key

    deriveAddressKey :: EncryptedSecretKey -> HdAddressChain -> HdAddressIx -> EncryptedSecretKey
    deriveAddressKey accKey addrChain addrIx =
        let derPath = DerivationPath $ encodeBip44DerivationPathFromAccount addrChain addrIx
        in case deriveHDSecretKeyByPath shouldNotCheck pp accKey derPath of
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
                    addrChain = addrId ^. hdAddressIdChain
                    addrIx = addrId ^. hdAddressIdIx
                in ( addrChain
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
