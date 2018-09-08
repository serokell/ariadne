module Dscp.Wallet.Knit where

import Prelude hiding (preview)

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Exception
import Control.Lens
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BSL
import Data.Scientific (toBoundedInteger)
import qualified Serokell.Util.Base64 as Base64
import Text.Earley

import Dscp.Core (GTx (..), Tx (..), TxId, TxInAcc (..), TxOut (..), addrFromText, coinToInteger, toTxId)
import Dscp.Crypto (decrypt, emptyPassPhrase, encrypt, fromByteArray, mkPassPhrase)
import Dscp.Util (toHex)
import Dscp.Wallet.Face
import Dscp.Witness.Web.Types

import Knit

-- Component type for Knit
data Wallet

data instance ComponentValue _ Wallet
  = ValueAddress Address
  | ValueCoin Coin
  | ValueTx TxId Address Coin [TxOut]
  | ValueTxOut TxOut
  | ValueCryptoKey ByteString
  deriving (Eq, Ord, Show)

makePrisms 'ValueAddress

instance
  ( Elem components Core
  , Elem components Wallet
  , AllConstrained (ComponentInflate components) components
  ) => ComponentInflate components Wallet where
  componentInflate = \case
      ValueAddress a -> ExprLit $ toLit (LitAddress a)
      ValueCoin c -> ExprLit $ toLit (LitNumber . fromIntegral . coinToInteger $ c)
      ValueTx txId inAddr inValue outs -> componentInflate . ValueList $
        [ toValue . ValueString . toHex $ txId
        , toValue . ValueAddress $ inAddr
        , toValue . ValueCoin $ inValue
        , toValue . ValueList $ toValue . ValueTxOut <$> outs
        ]
      ValueTxOut txOut -> ExprProcCall $ ProcCall "tx-out" $ txOutToArgs txOut
      ValueCryptoKey sk -> ExprLit $ toLit (LitString . Base64.encode $ sk)
    where
      txOutToArgs :: TxOut -> [Arg (Expr CommandId components)]
      txOutToArgs TxOut{..} = map ArgPos $
        [ componentInflate $ ValueAddress txOutAddr
        , componentInflate $ ValueCoin txOutValue
        ]

data instance ComponentLit Wallet
  = LitAddress Address
  deriving (Eq, Ord, Show)

data instance ComponentToken Wallet
  = TokenAddress Address
  deriving (Eq, Ord, Show)

makePrisms 'TokenAddress

instance Elem components Wallet => ComponentTokenizer components Wallet where
  componentTokenizer =
      [ toToken . TokenAddress <$> pAddress
      ]
    where
      pAddress :: Tokenizer Address
      pAddress = do
        str <- pSomeAlphaNum
        either (fail . show) return $ addrFromText str

instance ComponentDetokenizer Wallet where
  componentTokenRender = \case
    TokenAddress a -> pretty a

instance Elem components Wallet => ComponentLitGrammar components Wallet where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitAddress <$> tok (_Token . uprism . _TokenAddress)
      ]

instance ComponentPrinter Wallet where
  componentPpLit = \case
    LitAddress x -> text (componentTokenRender (TokenAddress x))
  componentPpToken = \case
    TokenAddress _ -> "address"

data instance ComponentCommandRepr components Wallet
  = CommandAction (WalletFace -> IO (Value components))
  | CommandReturn (Value components)

instance ComponentLitToValue components Wallet where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x

data instance ComponentExecContext _ _ Wallet =
  WalletExecCtx WalletFace

instance MonadIO m => ComponentCommandExec m components Wallet where
  componentCommandExec (WalletExecCtx walletFace) (CommandAction act) =
    liftIO $ act walletFace
  componentCommandExec _ (CommandReturn val) = return val

instance AllConstrained (Elem components) '[Wallet, Core] => ComponentCommandProcs components Wallet where
  componentCommandProcs =
    [
      CommandProc
        { cpName = "gen-key-pair"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            passString <- getArgOpt tyString "pass"
            mName <- getArgOpt tyString "name"
            pure (passString, mName)
        , cpRepr = \(passString, mName) -> CommandAction $ \WalletFace{..} -> do
            mPassPhrase <- forM passString $ either throwIO return . mkPassPhrase . encodeUtf8
            account <- walletGenKeyPair mName mPassPhrase
            return . renderAccount $ account
        , cpHelp = "Generate a key pair and save into storage."
        }
    , CommandProc
        { cpName = "restore-key"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            secretKeyString <- getArg tyCryptoKey "secretkey"
            passString <- getArgOpt tyString "pass"
            mName <- getArgOpt tyString "name"
            pure (secretKeyString, passString, mName)
        , cpRepr = \(secretKeyString, passString, mName) -> CommandAction $ \WalletFace{..} -> do
            (eSecretKey, mPassPhrase) <- parseAccount secretKeyString passString
            walletRestoreKey mName eSecretKey mPassPhrase
            return . toValue $ ValueUnit
        , cpHelp = "Generate a key pair and save into storage."
        }
    , CommandProc
        { cpName = "list-keys"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \WalletFace{..} -> do
            accounts <- walletListKeys
            let values = renderAccount <$> accounts
            return . toValue . ValueList $ values
        , cpHelp = "List all keys in storage."
        }
    , CommandProc
        { cpName = "tx-out"
        , cpArgumentPrepare = map
            $ typeDirectedKwAnn "addr" tyAddress
            . typeDirectedKwAnn "value" tyCoin
        , cpArgumentConsumer = do
            txOutAddr <- getArg tyAddress "addr"
            txOutValue <- getArg tyCoin "value"
            return TxOut{..}
        , cpRepr = CommandReturn . toValue . ValueTxOut
        , cpHelp = "Construct a transaction output"
        }
    , CommandProc
        { cpName = "send-tx"
        , cpArgumentPrepare = map
            $ typeDirectedKwAnn "out" tyTxOut
        , cpArgumentConsumer = do
            secretKeyString <- getArg tyCryptoKey "secretkey"
            passString <- getArgOpt tyString "pass"
            outs <- getArgSome tyTxOut "out"
            pure (passString, secretKeyString, outs)
        , cpRepr = \(passString, secretKeyString, outs) -> CommandAction $ \WalletFace{..} -> do
            (eSecretKey, mPassPhrase) <- parseAccount secretKeyString passString
            toValue . ValueString . toHex . toTxId <$> walletSendTx eSecretKey mPassPhrase outs
        , cpHelp = "Send a transaction."
        }
    , CommandProc
        { cpName = "get-balance"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            address <- getArg tyAddress "address"
            pure (address)
        , cpRepr = \(address) -> CommandAction $ \WalletFace{..} -> do
            BlocksOrMempool{..} <- walletGetBalance address
            return . toValue . ValueList $
              map (toValue . ValueCoin) [bmConfirmed, bmTotal]
        , cpHelp = "Get address balance."
        }
    , CommandProc
        { cpName = "tx-history"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            address <- getArg tyAddress "address"
            pure (address)
        , cpRepr = \(address) -> CommandAction $ \WalletFace{..} -> do
            txs <- walletGetTxHistory address
            return . toValue . ValueList . catMaybes $ renderTx <$> txs
        , cpHelp = "Get transaction history for an address."
        }
    ]

renderTx
  :: AllConstrained (Elem components) '[Wallet, Core]
  => GTx
  -> Maybe (Value components)  -- TODO: remove Maybe when we support publication txs
renderTx (GPublicationTx _) = Nothing  -- TODO: we don't display publication txs yet
renderTx (GMoneyTx tx@Tx{..}) = Just . toValue $ ValueTx (toTxId tx) (tiaAddr txInAcc) txInValue txOuts

renderAccount
  :: AllConstrained (Elem components) '[Wallet, Core]
  => Account
  -> Value components
renderAccount Account{..} = toValue . ValueList $
  [ toValue . ValueString $ fromMaybe "" accountName
  , fromRight (toValue . ValueCryptoKey . BSL.toStrict . serialise $ accountSecretKey) $
      toValue . ValueCryptoKey . BA.convert <$> decrypt emptyPassPhrase accountSecretKey
  , toValue . ValueCryptoKey . BA.convert $ accountPublicKey
  , toValue . ValueAddress $ accountAddress
  ]

parseAccount :: ByteString -> Maybe Text -> IO (Encrypted SecretKey, Maybe PassPhrase)
parseAccount secretKeyString passString =
  case fromByteArray @SecretKey secretKeyString of
    Right secretKey | Nothing <- passString ->
      return (encrypt emptyPassPhrase secretKey, Nothing)
    _ -> do
      mPassPhrase <- forM passString $ either throwIO return . mkPassPhrase . encodeUtf8
      eSecretKey <- either throwIO return . deserialiseOrFail . BSL.fromStrict $ secretKeyString
      return (eSecretKey, mPassPhrase)

----------------------------------------------------------------------------
-- Type projections
----------------------------------------------------------------------------

tyAddress :: Elem components Wallet => TyProjection components Address
tyAddress = TyProjection "Address" (preview _ValueAddress <=< fromValue)

tyCoin :: (Elem components Core, Elem components Wallet) => TyProjection components Coin
tyCoin = TyProjection "Coin" (\v -> fromValueCoin v <|> fromValueNumber v)
  where
    fromValueCoin = preview _ValueCoin <=< fromValue
    fromValueNumber = return . Coin <=< toBoundedInteger <=< preview _ValueNumber <=< fromValue

tyTxOut :: Elem components Wallet => TyProjection components TxOut
tyTxOut = TyProjection "TxOut" (preview _ValueTxOut <=< fromValue)

tyCryptoKey :: (Elem components Core, Elem components Wallet) => TyProjection components ByteString
tyCryptoKey = TyProjection "Base64" (\v -> fromValueCryptoKey v <|> fromValueString v)
  where
    fromValueCryptoKey = preview _ValueCryptoKey <=< fromValue
    fromValueString = (rightToMaybe . Base64.decode) <=< preview _ValueString <=< fromValue
