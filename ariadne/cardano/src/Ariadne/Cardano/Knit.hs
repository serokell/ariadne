module Ariadne.Cardano.Knit
       ( Cardano
       , Currency(..)

       , ComponentValue(..)
       , ComponentInflate(..)
       , ComponentLit(..)
       , ComponentToken(..)
       , ComponentTokenizer(..)
       , ComponentDetokenizer(..)
       , ComponentTokenToLit(..)
       , ComponentPrinter(..)
       , ComponentCommandRepr(..)
       , ComponentLitToValue(..)
       , ComponentExecContext(..)
       , ComponentCommandExec(..)
       , ComponentCommandProcs(..)

       , adaToCoin
       , adaMultiplier
       , maxCoin
       , showCoin
       , txOutCommandName
       , tyCoin
       , tyPublicKey
       , tyTxOut
       ) where

import Control.Lens (makePrisms, prism')
import Control.Natural (type (~>))
import Data.List as List
import Data.Scientific
import Formatting (sformat, (%))
import Pos.Core
import Pos.Core.Txp (TxOut)
import Pos.Crypto
import Pos.Util.Util (toParsecError)
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
          ExprLit NoExt $ toLit (LitAddress a)
        ValuePublicKey pk ->
          ExprLit NoExt $ toLit (LitPublicKey pk)
        ValueTxOut txOut ->
          ExprProcCall NoExt $ ProcCall NoExt "tx-out" $ txOutToArgs txOut
        ValueCoin coin ->
          ExprLit NoExt $ toLit (LitCoin coin)
        ValueHash h ->
          ExprLit NoExt $ toLit (LitHash h)
      where
        txOutToArgs :: TxOut -> [Arg NoExt (Expr NoExt CommandId components)]
        txOutToArgs TxOut {..} = List.map (ArgPos NoExt) $
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
        n <- P.signed pass P.scientific
        u <- (adaMultiplier <$ P.string' "ADA") <|> (1 <$ P.string' "Lovelace")

        return (u * n)

instance ComponentDetokenizer Cardano where
  componentTokenRender = \case
    TokenAddress a -> pretty a
    TokenPublicKey pk -> sformat fullPublicKeyF pk
    TokenHash h -> sformat ("#"%hashHexF) (getAHash h)
    TokenCoin c -> let (amount, unit) = showScientificCoin c in amount <> show unit

instance Elem components Cardano => ComponentTokenToLit components Cardano where
  componentTokenToLit t =
    asum
      [ (toLit . LitAddress) <$> preview (_Token . uprism . _TokenAddress) t
      , (toLit . LitPublicKey) <$> preview (_Token . uprism . _TokenPublicKey) t
      , (toLit . LitHash) <$> preview (_Token . uprism . _TokenHash) t
      , (toLit . LitCoin) <$> preview (_Token . uprism . _TokenCoin . adaPrism) t
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

-- maxCoinVal from Pos.Core.Types
maxCoin :: Scientific
maxCoin = normalize (scientific (toInteger maxCoinVal) 0)

adaToCoin :: Scientific -> Maybe Scientific
adaToCoin (normalize -> normalized) =
  if base10Exponent normalized < -adaExponent || normalized < 0 || normalized * adaMultiplier > maxCoin
     then Nothing
     else Just $ normalized * adaMultiplier

scientificToCoinValue :: Scientific -> Maybe Scientific
scientificToCoinValue c = if isInteger c then Just c else Nothing

data Currency = ADA | Lovelace deriving (Eq, Show)

showCoin :: Coin -> (Text, Currency)
showCoin = showScientificCoin . fromIntegral . unsafeGetCoin

showScientificCoin :: Scientific -> (Text, Currency)
showScientificCoin c =
  case floatingOrInteger ada of
    Left (_ :: Double) ->
      if ada >= 1
        -- Use formatScientific Fixed to not show exponential notation
        then (fromString $ formatScientific Fixed Nothing ada, ADA)
        else (componentTokenRender $ TokenNumber c, Lovelace)
    Right (n :: Integer) -> (show n, ADA)
  where
    ada :: Scientific
    ada = c / adaMultiplier
