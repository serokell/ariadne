module Ariadne.Cardano.Knit where

import Universum hiding (preview)

import Control.Lens hiding (parts)
import Data.List as List
import Data.Scientific
import Formatting (sformat, (%))
import IiExtras
import Pos.Core
import Pos.Core.Txp (TxOut)
import Pos.Crypto
import Pos.Util.Util (toParsecError)
import Text.Earley
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Ariadne.Cardano.Face

import Knit

-- Component type for Knit
data Cardano

data instance ComponentValue _ Cardano
  = ValueAddress Address
  | ValuePublicKey PublicKey
  | ValueCoin Coin
  | ValueTxOut TxOut
  | ValueHash AHash
  deriving (Eq, Ord, Show)

makePrisms 'ValueAddress

instance
  ( Elem components Cardano
  , Elem components Core
  , AllConstrained (ComponentInflate components) components
  ) => ComponentInflate components Cardano
  where
    componentInflate =
      \case
        ValueAddress a ->
          ExprLit $ toLit (LitAddress a)
        ValuePublicKey pk ->
          ExprLit $ toLit (LitPublicKey pk)
        ValueTxOut txOut ->
          ExprProcCall $ ProcCall "tx-out" $ txOutToArgs txOut
        ValueCoin coin ->
          ExprLit $ toLit (LitCoin coin)
        ValueHash h ->
          ExprLit $ toLit (LitHash h)
      where
        txOutToArgs :: TxOut -> [Arg (Expr CommandId components)]
        txOutToArgs TxOut {..} = List.map ArgPos $
          [ componentInflate $ ValueAddress txOutAddress
          , componentInflate $ ValueCoin txOutValue
          ]


data instance ComponentLit Cardano
  = LitAddress Address
  | LitPublicKey PublicKey
  | LitHash AHash
  | LitCoin Coin
  deriving (Eq, Ord, Show)

data instance ComponentToken Cardano
  = TokenAddress Address
  | TokenPublicKey PublicKey
  | TokenHash AHash
  | TokenCoin Scientific
  deriving (Eq, Ord, Show)

makePrisms 'TokenAddress

instance Elem components Cardano => ComponentTokenizer components Cardano where
  componentTokenizer =
      [ toToken . TokenAddress <$> pAddress
      , toToken . TokenPublicKey <$> pPublicKey
      , toToken . TokenHash <$> pHash
      , toToken . TokenCoin <$> pCoin
      ]
    where
      pAddress :: Tokenizer Address
      pAddress = do
        str <- pSomeAlphaNum
        toParsecError $ decodeTextAddress str

      pPublicKey :: Tokenizer PublicKey
      pPublicKey = do
        str <- (<>) <$> P.takeP (Just "base64") 86 <*> P.string "=="
        toParsecError $ parseFullPublicKey str

      pHash :: Tokenizer AHash
      pHash = do
        void $ P.char '#'
        str <- pSomeAlphaNum
        toParsecError . fmap unsafeCheatingHashCoerce $ decodeAbstractHash str
      pCoin :: Tokenizer Scientific
      pCoin = do
        n <- P.signed (return ()) P.scientific
        u <- (adaMultiplier <$ P.string' "ADA") <|> (1 <$ P.string' "Lovelace")

        return (u * n)

instance ComponentDetokenizer Cardano where
  componentTokenRender = \case
    TokenAddress a -> pretty a
    TokenPublicKey pk -> sformat fullPublicKeyF pk
    TokenHash h -> sformat ("#"%hashHexF) (getAHash h)
    TokenCoin c -> let (amount, unit) = showScientificCoin c in amount <> unit

instance Elem components Cardano => ComponentLitGrammar components Cardano where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitAddress <$> tok (_Token . uprismElem . _TokenAddress)
      , toLit . LitPublicKey <$> tok (_Token . uprismElem . _TokenPublicKey)
      , toLit . LitHash <$> tok (_Token . uprismElem . _TokenHash)
      , toLit . LitCoin <$> tok (_Token . uprismElem . _TokenCoin . adaPrism)
      ]

    where
      -- This prism will focus on an ADA value in a Scientific if it is valid
      -- Left side of prism (Coin -> Scientific) will never actually be called
      adaPrism = prism' (fromIntegral . unsafeGetCoin) $
        fmap fromPreCoin . toBoundedInteger <=< scientificToCoinValue

instance ComponentPrinter Cardano where
  componentPpLit = \case
    LitAddress x -> text (componentTokenRender (TokenAddress x))
    LitPublicKey x -> text (componentTokenRender (TokenPublicKey x))
    LitHash x -> text (componentTokenRender (TokenHash x))
    LitCoin x -> text (componentTokenRender (TokenCoin $ fromIntegral $ unsafeGetCoin x))
  componentPpToken = \case
    TokenAddress _ -> "address"
    TokenPublicKey _ -> "public key"
    TokenHash _ -> "hash"
    TokenCoin c -> if isJust $ scientificToCoinValue c then "coin" else "invalid coin"

data instance ComponentCommandRepr components Cardano
  = CommandAction (CardanoMode (Value components))
  | CommandError  (forall m. MonadThrow m => m (Value components))
  | CommandReturn (Value components)

instance ComponentLitToValue components Cardano where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x
    LitPublicKey x -> ValuePublicKey x
    LitHash x -> ValueHash x
    LitCoin x -> ValueCoin x

newtype instance ComponentExecContext _ _ Cardano =
  CardanoExecCtx (CardanoMode ~> IO)

instance MonadIO m => ComponentCommandExec m components Cardano where
  componentCommandExec (CardanoExecCtx runCardanoMode) (CommandAction act) =
    liftIO $ runCardanoMode act
  componentCommandExec _ (CommandError action) = liftIO $ action
  componentCommandExec _ (CommandReturn val) = return val

instance (AllConstrained (Elem components) '[Cardano, Core]) => ComponentCommandProcs components Cardano where
  componentCommandProcs =
    [ CommandProc
        { cpName = txOutCommandName
        , cpArgumentPrepare = List.map
          $ typeDirectedKwAnn "addr" tyAddress
        , cpArgumentConsumer = do
            txOutAddress <- getArg tyAddress "addr"
            txOutValue <- getArg tyCoin "value"
            return TxOut{..}
        , cpRepr = CommandReturn . toValue . ValueTxOut
        , cpHelp = "construct a transaction output"
        }
    ]

txOutCommandName :: CommandId
txOutCommandName = "tx-out"

class ToLeft m a b where
    toLeft :: Either a b -> m a

instance (Monad m, ToLeft m a b, ToLeft m b c) => ToLeft m a (Either b c) where
    toLeft = toLeft <=< traverse toLeft

-- | Small hack to use 'toBoundedInteger' for 'Coin'.
newtype PreCoin = PreCoin { getPreCoin :: Word64 }
    deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Bounded PreCoin where
    minBound = PreCoin . unsafeGetCoin $ minBound
    maxBound = PreCoin . unsafeGetCoin $ maxBound

fromPreCoin :: PreCoin -> Coin
fromPreCoin = mkCoin . getPreCoin

tyAddress :: Elem components Cardano => TyProjection components Address
tyAddress = TyProjection "Address" (preview _ValueAddress <=< fromValue)

tyPublicKey :: Elem components Cardano => TyProjection components PublicKey
tyPublicKey = TyProjection "PublicKey" (preview _ValuePublicKey <=< fromValue)

tyTxOut :: Elem components Cardano => TyProjection components TxOut
tyTxOut = TyProjection "TxOut" (preview _ValueTxOut <=< fromValue)

tyCoin :: (Elem components Core, Elem components Cardano) => TyProjection components Coin
tyCoin = TyProjection "ADA" (\v -> tyCoinAda v <|> tyCoinLovelace v)
  where
    tyCoinAda = fmap fromPreCoin . toBoundedInteger <=< adaToCoin <=< preview _ValueNumber <=< fromValue
    tyCoinLovelace = preview _ValueCoin <=< fromValue

adaExponent :: Int
adaExponent = 6

adaMultiplier :: Scientific
adaMultiplier = scientific 1 adaExponent

adaToCoin :: Scientific -> Maybe Scientific
adaToCoin (normalize -> normalized) =
  if base10Exponent normalized > adaExponent
     then Nothing
     else Just $ normalized * adaMultiplier

scientificToCoinValue :: Scientific -> Maybe Scientific
scientificToCoinValue c = if isInteger c then Just c else Nothing

showCoin :: Coin -> (Text, Text)
showCoin = showScientificCoin . fromIntegral . unsafeGetCoin

showScientificCoin :: Scientific -> (Text, Text)
showScientificCoin c =
  case floatingOrInteger ada of
    Left (_ :: Double) ->
      if ada >= 1
         then (show ada, "ADA")
         else (componentTokenRender $ TokenNumber c, "Lovelace")
    Right (n :: Integer) -> (show n, "ADA")
  where
    ada :: Scientific
    ada = c / adaMultiplier
