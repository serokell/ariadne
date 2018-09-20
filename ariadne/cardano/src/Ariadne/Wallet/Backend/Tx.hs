-- | Part of the backend which deals with transactions.

module Ariadne.Wallet.Backend.Tx
       ( sendTx
       , txFee
       ) where

import Prelude hiding (list)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable

import Control.Exception (Exception(displayException))
import Control.Lens (at, ix)
import Control.Natural ((:~>)(..))
import Formatting (bprint, build, formatToString, int, (%))
import Text.PrettyPrint.ANSI.Leijen (Doc, list, softline, string)

import Pos.Client.Txp (computeTxFee, getFakeChangeAddress, prepareMTx, runTxCreator, submitTxRaw)
import Pos.Core.Txp (Tx(..), TxAux(..), TxOutAux(..))
import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, SafeSigner(..), checkPassMatches, hash)
import Pos.Crypto.HD (ShouldCheckPassphrase(..))
import Pos.Infra.Diffusion.Types (Diffusion)
import Pos.Launcher (HasConfigurations)
import Pos.Txp (TxFee(..))
import Pos.Util (eitherToThrow, maybeThrow)

import Ariadne.Cardano.Face
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Mode ()
import Ariadne.Wallet.Cardano.Kernel.Bip32
import Ariadne.Wallet.Cardano.Kernel.Bip44
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet, (@+))
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))
import Ariadne.Wallet.Face

data SendTxException
    = SendTxNoAddresses !HdRootId
    | SendTxNoAccounts !HdRootId
    | SendTxIncorrectPassPhrase
    | SendTxAccountIndexOutOfRange !HdRootId !Word
    deriving (Show)

instance Buildable SendTxException where
    build = \case
        SendTxNoAddresses walletIdx ->
            bprint ("Wallet "%build%" does not have any addresses") walletIdx
        SendTxNoAccounts walletIdx ->
            bprint ("Wallet "%build%" does not have any accounts") walletIdx
        SendTxIncorrectPassPhrase ->
            "Incorrect passphrase"
        SendTxAccountIndexOutOfRange rootId idx ->
            bprint ("Account #"%int%" doesn't exist in "%build) idx rootId

instance Exception SendTxException where
    displayException = toString . prettyL

-- | Send a transaction from selected to wallet to the list of
-- 'TxOut's.  If list of accounts is not empty, only those accounts
-- may be used as inputs.  If this list is empty and an account from
-- the input wallet is selected, this account will be used as input.
-- Otherwise inputs will be selected from all accounts in the wallet.
sendTx ::
       (HasConfigurations)
    => PassiveWalletLayer IO
    -> WalletFace
    -> CardanoFace
    -> IORef (Maybe WalletSelection)
    -> (Doc -> IO ())
    -> PassPhrase
    -> WalletReference
    -> [LocalAccountReference]
    -> InputSelectionPolicy
    -> NonEmpty TxOut
    -> IO TxId
sendTx pwl WalletFace {..} CardanoFace {..} walletSelRef printAction pp walletRef accRefs isp outs = do
    -- TODO: call newPending here
    let NT runCardanoMode = cardanoRunCardanoMode
    walletDb <- pwlGetDBSnapshot pwl
    let wallets = walletDb ^. dbHdWallets
    (walletRootId, walletAccounts) <-
        resolveWalletRefThenRead walletSelRef walletRef
        walletDb readAccountsByRootId
    accountIds <- getSuitableAccounts walletRootId walletAccounts
    let filteredAccounts :: IxSet HdAccount
        filteredAccounts = maybe identity filterAccounts accountIds walletAccounts

        filterAccounts :: NonEmpty HdAccountId -> IxSet HdAccount -> IxSet HdAccount
        filterAccounts ids accounts = accounts @+ toList ids

    runCardanoMode $
        sendTxDo wallets walletRootId filteredAccounts =<< cardanoGetDiffusion
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
            maybeThrow (SendTxAccountIndexOutOfRange rootId uiIdx) $
            accountsList ^? ix (fromIntegral uiIdx) . hdAccountId

    sendTxDo ::
           HdWallets
        -> HdRootId
        -> IxSet HdAccount
        -> Diffusion CardanoMode
        -> CardanoMode TxId
    sendTxDo wallets walletRootId accountsToUse diffusion = do
        -- Wallet creation and deletion is organized in such way that
        -- the absence of a key is not possible.
        esk <- liftIO $ fromMaybe
            (error "Bug: Keystore has no such key.")
            <$> pwlLookupKeystore pwl walletRootId

        maybeThrow SendTxIncorrectPassPhrase $
            checkPassMatches pp esk
        let signersMap :: HashMap Address SafeSigner
            -- safe due to passphrase check above
            signersMap = walletSigners esk wallets pp accountsToUse
        let getSigner :: Address -> Maybe SafeSigner
            getSigner addr = signersMap ^. at addr
        ourAddresses <-
            maybeThrow
                (SendTxNoAddresses walletRootId)
                (nonEmpty $ HM.keys signersMap)
        -- We pick one of the accounts to generate change address in it.
        ourAccount <-
            maybeThrow
                (SendTxNoAccounts walletRootId)
                (toList accountsToUse ^? ix 0)
        let ourAccountId = ourAccount ^. hdAccountId
        let newChangeAddress = walletNewAddress
                (AccountRefByHdAccountId ourAccountId)
                HdChainInternal pp
        (txAux, _) <-
            prepareMTx
                cardanoProtocolMagic
                getSigner
                mempty
                isp
                ourAddresses
                (map TxOutAux outs)
                newChangeAddress
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

-- | Send a transaction from selected to wallet to the list of
-- 'TxOut's.  If list of accounts is not empty, only those accounts
-- may be used as inputs.  If this list is empty and an account from
-- the input wallet is selected, this account will be used as input.
-- Otherwise inputs will be selected from all accounts in the wallet.
txFee ::
       {-(HasConfigurations)
    => -}PassiveWalletLayer IO
    -> WalletFace
    -> CardanoFace
    -> IORef (Maybe WalletSelection)
    -> (Doc -> IO ())
    -> WalletReference
    -> [LocalAccountReference]
    -> InputSelectionPolicy
    -> NonEmpty TxOut
    -> IO Coin
txFee pwl WalletFace {..} CardanoFace {..} walletSelRef printAction walletRef accRefs isp outs = do
    -- TODO: call newPending here
    let NT runCardanoMode = cardanoRunCardanoMode
    walletDb <- pwlGetDBSnapshot pwl
    let wallets = walletDb ^. dbHdWallets
    (walletRootId, walletAccounts) <-
        resolveWalletRefThenRead walletSelRef walletRef
        walletDb readAccountsByRootId
    accountIds <- getSuitableAccounts walletRootId walletAccounts
    let filteredAccounts :: IxSet HdAccount
        filteredAccounts = maybe identity filterAccounts accountIds walletAccounts

        filterAccounts :: NonEmpty HdAccountId -> IxSet HdAccount -> IxSet HdAccount
        filterAccounts ids accounts = accounts @+ toList ids

    runCardanoMode $
        txFeeDo wallets walletRootId filteredAccounts =<< cardanoGetDiffusion
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
            maybeThrow (SendTxAccountIndexOutOfRange rootId uiIdx) $
            accountsList ^? ix (fromIntegral uiIdx) . hdAccountId

    txFeeDo ::
           HdWallets
        -> HdRootId
        -> IxSet HdAccount
        -> Diffusion CardanoMode
        -> CardanoMode Coin
    txFeeDo wallets walletRootId accountsToUse diffusion = do
        -- Wallet creation and deletion is organized in such way that
        -- the absence of a key is not possible.
        esk <- liftIO $ fromMaybe
            (error "Bug: Keystore has no such key.")
            <$> pwlLookupKeystore pwl walletRootId

        let signersMap :: HashMap Address SafeSigner
            -- safe due to passphrase check above
            signersMap = walletSigners esk wallets pp accountsToUse
        let getSigner :: Address -> Maybe SafeSigner
            getSigner addr = signersMap ^. at addr
        ourAddresses <-
            maybeThrow
                (SendTxNoAddresses walletRootId)
                (nonEmpty $ HM.keys signersMap)
        -- We pick one of the accounts to generate change address in it.
        ourAccount <-
            maybeThrow
                (SendTxNoAccounts walletRootId)
                (toList accountsToUse ^? ix 0)
        let ourAccountId = ourAccount ^. hdAccountId
        let newChangeAddress = getFakeChangeAddress
        (txAux, _) <-
            prepareMTx
                cardanoProtocolMagic
                getSigner
                mempty
                isp
                ourAddresses
                (map TxOutAux outs)
                newChangeAddress
        let tx = taTx txAux
        let txId = hash tx
        txId <$ submitTxRaw diffusion txAux

        TxFee fee <- eitherToThrow =<< runTxCreator isp (computeTxFee cardanoProtocolMagic pendingAddrs utxo outputs)
        return fee

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
        let derPath = encodeBip44DerivationPathToAccount accIdx
        in case deriveHDSecretKeyByPath shouldNotCheck pp rootSK derPath of
            Nothing -> invalidPassphrase
            Just key -> key

    deriveAddressKey :: EncryptedSecretKey -> HdAddressChain -> HdAddressIx -> EncryptedSecretKey
    deriveAddressKey accKey addrChain addrIx =
        let derPath = encodeBip44DerivationPathFromAccount addrChain addrIx
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
