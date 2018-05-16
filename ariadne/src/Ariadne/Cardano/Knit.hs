module Ariadne.Cardano.Knit where

import Universum hiding (preview)

import Control.Lens hiding (parts)
import Data.Coerce (coerce)
import Data.Fixed
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Scientific
import Data.Time.Units
import Formatting (sformat)
import IiExtras
import Pos.Core
import Pos.Core.Txp (TxOut)
import Pos.Crypto
import Pos.Update (BlockVersionData, BlockVersionModifier, SystemTag)
import Pos.Util.Util (eitherToThrow, toParsecError)
import Serokell.Data.Memory.Units (Byte, fromBytes)
import Text.Earley
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Ariadne.Cardano.Face

import Knit

data AddrDistrPart = AddrDistrPart
    { adpStakeholderId :: !StakeholderId
    , adpCoinPortion   :: !CoinPortion
    } deriving (Eq, Ord, Show, Generic)

-- | Parameters for 'ProposeUpdate' command.
data ProposeUpdateParams = ProposeUpdateParams
    { puSecretKeyIdx         :: !Int -- the node that creates/signs the proposal
    , puVoteAll              :: !Bool
    , puBlockVersion         :: !BlockVersion
    , puSoftwareVersion      :: !SoftwareVersion
    , puBlockVersionModifier :: !BlockVersionModifier
    , puUpdates              :: ![ProposeUpdateSystem]
    } deriving (Show)

data RollbackParams = RollbackParams
    { rpNum      :: !Word
    , rpDumpPath :: !FilePath
    } deriving (Show)

data ProposeUpdateSystem = ProposeUpdateSystem
    { pusSystemTag     :: SystemTag
    , pusInstallerPath :: Maybe FilePath
    , pusBinDiffPath   :: Maybe FilePath
    } deriving (Eq, Ord, Show)

data GenBlocksParams = GenBlocksParams
    { bgoBlockN :: !Word32
    -- ^ Number of blocks to generate.
    , bgoSeed   :: !(Maybe Int)
    -- ^ Generating seed.
    } deriving (Show)

-- | Parameters of `add-key` command.
data AddKeyParams = AddKeyParams
    { akpFile    :: !FilePath
    -- ^ Path to 'UserSecret'.
    , akpPrimary :: !Bool
    -- ^ If 'True', then primary key will be added.
    }

-- Component type for Knit
data Cardano

data instance ComponentValue _ Cardano
  = ValueAddress Address
  | ValuePublicKey PublicKey
  | ValueTxOut TxOut
  | ValueStakeholderId StakeholderId
  | ValueHash AHash
  | ValueBlockVersion BlockVersion
  | ValueSoftwareVersion SoftwareVersion
  | ValueBlockVersionModifier BlockVersionModifier
  | ValueBlockVersionData BlockVersionData
  | ValueProposeUpdateSystem ProposeUpdateSystem
  | ValueAddrDistrPart AddrDistrPart
  | ValueAddrStakeDistribution AddrStakeDistribution
  | ValueSoftforkRule SoftforkRule
  | ValueTxFeePolicy TxFeePolicy
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
        ValueStakeholderId sId ->
          ExprLit $ toLit (LitStakeholderId sId)
        ValueHash h ->
          ExprLit $ toLit (LitHash h)
        ValueBlockVersion v ->
          ExprLit $ toLit (LitBlockVersion v)
        ValueSoftforkRule sr ->
          ExprProcCall $ ProcCall "softfork-rule" (srToArgs sr)
        ValueTxFeePolicy tfp ->
          ExprProcCall $ tfpToProcCall tfp
        ValueBlockVersionModifier bvm ->
          ExprProcCall $ ProcCall "bvm" (bvmToArgs bvm)
        ValueBlockVersionData bvd ->
          ExprProcCall $ ProcCall "bvd-read" (bvdToArgs bvd)
        ValueProposeUpdateSystem pus ->
          ExprProcCall $ ProcCall "upd-bin" (pusToArgs pus)
        ValueAddrDistrPart adp ->
          ExprProcCall $ ProcCall "dp" (adpToArgs adp)
        ValueAddrStakeDistribution asd ->
          ExprProcCall (asdToProcCall asd)
        ValueSoftwareVersion sv ->
          ExprProcCall $ ProcCall "software-version" (svToArgs sv)
      where
        txOutToArgs :: TxOut -> [Arg (Expr CommandName components)]
        txOutToArgs TxOut {..} = List.map ArgPos $
          [ componentInflate $ ValueAddress txOutAddress
          , componentInflate $ ValueNumber (fromIntegral $ unsafeGetCoin txOutValue)
          ]

        coinPortionToScientific :: CoinPortion -> Scientific
        coinPortionToScientific (getCoinPortion -> num) =
          (fromIntegral num) / (fromIntegral coinPortionDenominator)

        srToArgs SoftforkRule {..} = List.map ArgPos $
          [ componentInflate $ ValueNumber (coinPortionToScientific srInitThd)
          , componentInflate $ ValueNumber (coinPortionToScientific srMinThd)
          , componentInflate $ ValueNumber (coinPortionToScientific srThdDecrement)
          ]

        tfpToProcCall = \case
          TxFeePolicyTxSizeLinear (TxSizeLinear a b) ->
            ProcCall "tx-fee-policy-tx-size-linear"
              [ ArgPos $ componentInflate $
                  ValueNumber (realToFrac @Nano @Scientific $ coerce a)
              , ArgPos $ componentInflate $
                  ValueNumber (realToFrac @Nano @Scientific $ coerce b)
              ]
          TxFeePolicyUnknown v bs ->
            ProcCall "tx-fee-policy-unknown"
              [ ArgPos $ componentInflate $
                  ValueNumber (fromIntegral v)
              , ArgPos $ componentInflate $
                  ValueString (decodeUtf8 bs) -- FIXME: ValueBytes (Vector Word8)
              ]

        millisecToSec :: Millisecond -> Scientific
        millisecToSec = (/ 1e3) . fromIntegral

        bvmToArgs :: BlockVersionModifier -> [Arg (Expr CommandName components)]
        bvmToArgs BlockVersionModifier {..} = catMaybes
          [ ArgKw "script-version" . componentInflate . ValueNumber . fromIntegral <$> bvmScriptVersion
          , ArgKw "slot-duration" . componentInflate . ValueNumber . millisecToSec <$> bvmSlotDuration
          , ArgKw "max-block-size" . componentInflate . ValueNumber . fromIntegral <$> bvmMaxBlockSize
          , ArgKw "max-header-size" . componentInflate . ValueNumber . fromIntegral <$> bvmMaxHeaderSize
          , ArgKw "max-tx-size" . componentInflate . ValueNumber . fromIntegral <$> bvmMaxTxSize
          , ArgKw "max-proposal-size" . componentInflate . ValueNumber . fromIntegral <$> bvmMaxProposalSize
          , ArgKw "mpc-thd" . componentInflate . ValueNumber . coinPortionToScientific <$> bvmMpcThd
          , ArgKw "heavy-del-thd" . componentInflate . ValueNumber . coinPortionToScientific <$> bvmHeavyDelThd
          , ArgKw "update-vote-thd" . componentInflate . ValueNumber . coinPortionToScientific <$> bvmUpdateVoteThd
          , ArgKw "update-proposal-thd" . componentInflate . ValueNumber . coinPortionToScientific <$> bvmUpdateProposalThd
          , ArgKw "update-implicit" . componentInflate . ValueNumber . fromIntegral <$> bvmUpdateImplicit
          , ArgKw "softfork-rule" . ExprProcCall . ProcCall "softfork-rule" . srToArgs <$> bvmSoftforkRule
          , ArgKw "tx-fee-policy" . ExprProcCall . tfpToProcCall <$> bvmTxFeePolicy
          , ArgKw "unlock-stake-epoch" . componentInflate . ValueNumber . fromIntegral <$> bvmUnlockStakeEpoch
          ]

        bvdToArgs :: BlockVersionData -> [Arg (Expr CommandName components)]
        bvdToArgs BlockVersionData {..} =
          [ ArgKw "script-version" $ componentInflate $ ValueNumber $ fromIntegral bvdScriptVersion
          , ArgKw "slot-duration" $ componentInflate $ ValueNumber $ millisecToSec bvdSlotDuration
          , ArgKw "max-block-size" $ componentInflate $ ValueNumber $ fromIntegral bvdMaxBlockSize
          , ArgKw "max-header-size" $ componentInflate $ ValueNumber $ fromIntegral bvdMaxHeaderSize
          , ArgKw "max-tx-size" $ componentInflate $ ValueNumber $ fromIntegral bvdMaxTxSize
          , ArgKw "max-proposal-size" $ componentInflate $ ValueNumber $ fromIntegral bvdMaxProposalSize
          , ArgKw "mpc-thd" $ componentInflate $ ValueNumber $ coinPortionToScientific bvdMpcThd
          , ArgKw "heavy-del-thd" $ componentInflate $ ValueNumber $ coinPortionToScientific bvdHeavyDelThd
          , ArgKw "update-vote-thd" $ componentInflate $ ValueNumber $ coinPortionToScientific bvdUpdateVoteThd
          , ArgKw "update-proposal-thd" $ componentInflate $ ValueNumber $ coinPortionToScientific bvdUpdateProposalThd
          , ArgKw "update-implicit" $ componentInflate $ ValueNumber $ fromIntegral bvdUpdateImplicit
          , ArgKw "softfork-rule" $ ExprProcCall $ ProcCall "softfork-rule" $ srToArgs bvdSoftforkRule
          , ArgKw "tx-fee-policy" $ ExprProcCall $ tfpToProcCall bvdTxFeePolicy
          , ArgKw "unlock-stake-epoch" $ componentInflate $ ValueNumber $ fromIntegral bvdUnlockStakeEpoch
          ]

        pusToArgs :: ProposeUpdateSystem -> [Arg (Expr CommandName components)]
        pusToArgs ProposeUpdateSystem {..} = catMaybes
          [ Just $ ArgPos $ componentInflate $ ValueString $ getSystemTag pusSystemTag
          , ArgKw "installer-path" . componentInflate . ValueFilePath <$> pusInstallerPath
          , ArgKw "bin-diff-path"  . componentInflate . ValueFilePath <$> pusBinDiffPath
          ]

        adpToArgs :: AddrDistrPart -> [Arg (Expr CommandName components)]
        adpToArgs AddrDistrPart {..} =
          [ ArgPos $ componentInflate $ ValueStakeholderId adpStakeholderId
          , ArgPos $ componentInflate $ ValueNumber $ coinPortionToScientific adpCoinPortion
          ]

        asdToProcCall
          :: AddrStakeDistribution
          -> ProcCall CommandName (Expr CommandName components)
        asdToProcCall =
          \case
            BootstrapEraDistr -> ProcCall "boot" []
            SingleKeyDistr sId ->
              ProcCall "distr" [ArgPos $ adpToExpr $ AddrDistrPart sId maxBound]
            UnsafeMultiKeyDistr mkd ->
              ProcCall "distr" $
                List.map (\(sId, cp) -> ArgPos $ adpToExpr $ AddrDistrPart sId cp) $
                  Map.toList mkd
          where
            adpToExpr :: AddrDistrPart -> Expr CommandName components
            adpToExpr = componentInflate . ValueAddrDistrPart

        svToArgs
          :: SoftwareVersion
          -> [Arg (Expr CommandName components)]
        svToArgs SoftwareVersion{..} =
          [ ArgKw "name" $ componentInflate $ ValueString (getApplicationName svAppName)
          , ArgKw "n" $ componentInflate $ ValueNumber (fromIntegral svNumber)
          ]

data instance ComponentLit Cardano
  = LitAddress Address
  | LitPublicKey PublicKey
  | LitStakeholderId StakeholderId
  | LitHash AHash
  | LitBlockVersion BlockVersion
  deriving (Eq, Ord, Show)

data instance ComponentToken Cardano
  = TokenAddress Address
  | TokenPublicKey PublicKey
  | TokenStakeholderId StakeholderId
  | TokenHash AHash
  | TokenBlockVersion BlockVersion
  deriving (Eq, Ord, Show)

makePrisms 'TokenAddress

instance Elem components Cardano => ComponentTokenizer components Cardano where
  componentTokenizer =
      [ toToken . TokenAddress <$> pAddress
      , toToken . TokenPublicKey <$> pPublicKey
      , toToken . TokenStakeholderId <$> pStakeholderId
      , toToken . TokenHash <$> pHash
      , toToken . TokenBlockVersion <$> pBlockVersion
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

      pStakeholderId :: Tokenizer StakeholderId
      pStakeholderId = do
        str <- pSomeAlphaNum
        toParsecError $ decodeAbstractHash str

      pHash :: Tokenizer AHash
      pHash = do
        str <- pSomeAlphaNum
        toParsecError . fmap unsafeCheatingHashCoerce $ decodeAbstractHash str

      pBlockVersion :: Tokenizer BlockVersion
      pBlockVersion = do
        bvMajor <- P.decimal
        void $ P.char '.'
        bvMinor <- P.decimal
        void $ P.char '.'
        bvAlt <- P.decimal
        P.notFollowedBy $ P.char '.'
        return BlockVersion{..}

instance ComponentDetokenizer Cardano where
  componentTokenRender = \case
    TokenAddress a -> pretty a
    TokenPublicKey pk -> sformat fullPublicKeyF pk
    TokenStakeholderId sId -> sformat hashHexF sId
    TokenHash h -> sformat hashHexF (getAHash h)
    TokenBlockVersion v -> pretty v

instance Elem components Cardano => ComponentLitGrammar components Cardano where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitAddress <$> tok (_Token . uprismElem . _TokenAddress)
      , toLit . LitPublicKey <$> tok (_Token . uprismElem . _TokenPublicKey)
      , toLit . LitStakeholderId <$> tok (_Token . uprismElem . _TokenStakeholderId)
      , toLit . LitHash <$> tok (_Token . uprismElem . _TokenHash)
      , toLit . LitBlockVersion <$> tok (_Token . uprismElem . _TokenBlockVersion)
      ]

instance ComponentPrinter Cardano where
  componentPpLit = \case
    LitAddress x -> text (componentTokenRender (TokenAddress x))
    LitPublicKey x -> text (componentTokenRender (TokenPublicKey x))
    LitStakeholderId x -> text (componentTokenRender (TokenStakeholderId x))
    LitHash x -> text (componentTokenRender (TokenHash x))
    LitBlockVersion x -> text (componentTokenRender (TokenBlockVersion x))
  componentPpToken = \case
    TokenAddress _ -> "address"
    TokenPublicKey _ -> "public key"
    TokenStakeholderId _ -> "stakeholder id"
    TokenHash _ -> "hash"
    TokenBlockVersion _ -> "block version"

data instance ComponentCommandRepr components Cardano
  = CommandAction (CardanoMode (Value components))
  | CommandError  (forall m. MonadThrow m => m (Value components))
  | CommandReturn (Value components)

instance ComponentLitToValue components Cardano where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x
    LitPublicKey x -> ValuePublicKey x
    LitStakeholderId x -> ValueStakeholderId x
    LitHash x -> ValueHash x
    LitBlockVersion x -> ValueBlockVersion x

newtype instance ComponentExecContext _ _ Cardano =
  CardanoExecCtx (CardanoMode ~> IO)

instance MonadIO m => ComponentCommandExec m components Cardano where
  componentCommandExec (CardanoExecCtx runCardanoMode) (CommandAction act) =
    liftIO $ runCardanoMode act
  componentCommandExec _ (CommandError action) = liftIO $ action
  componentCommandExec _ (CommandReturn val) = return val

instance (AllConstrained (Elem components) '[Cardano, Core]) => ComponentCommandProcs components Cardano where
  componentCommandProcs =
    [
      CommandProc
        { cpName = bootCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = const . CommandReturn . toValue . ValueAddrStakeDistribution $ BootstrapEraDistr
        , cpHelp = "bootstrap era stake distribution constant"
        }
    , CommandProc
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
    , CommandProc
        { cpName = softforkRuleCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            srInitThd <- getArg tyCoinPortion "init-thd"
            srMinThd <- getArg tyCoinPortion "min-thd"
            srThdDecrement <- getArg tyCoinPortion "thd-decrement"
            return SoftforkRule{..}
        , cpRepr = CommandReturn . toValue . ValueSoftforkRule
        , cpHelp = "construct a softfork rule"
        }
    , CommandProc
        { cpName = bvmCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            bvmScriptVersion     <- getArgOpt tyScriptVersion "script-version"
            bvmSlotDuration      <- getArgOpt tySecond "slot-duration"
            bvmMaxBlockSize      <- getArgOpt tyByte "max-block-size"
            bvmMaxHeaderSize     <- getArgOpt tyByte "max-header-size"
            bvmMaxTxSize         <- getArgOpt tyByte "max-tx-size"
            bvmMaxProposalSize   <- getArgOpt tyByte "max-proposal-size"
            bvmMpcThd            <- getArgOpt tyCoinPortion "mpc-thd"
            bvmHeavyDelThd       <- getArgOpt tyCoinPortion "heavy-del-thd"
            bvmUpdateVoteThd     <- getArgOpt tyCoinPortion "update-vote-thd"
            bvmUpdateProposalThd <- getArgOpt tyCoinPortion "update-proposal-thd"
            bvmUpdateImplicit    <- getArgOpt tyWord64 "update-implicit"
            bvmSoftforkRule      <- getArgOpt tySoftforkRule "softfork-rule"
            bvmTxFeePolicy       <- getArgOpt tyTxFeePolicy "tx-fee-policy"
            bvmUnlockStakeEpoch  <- getArgOpt tyEpochIndex "unlock-stake-epoch"
            pure BlockVersionModifier{..}
        , cpRepr = CommandReturn . toValue . ValueBlockVersionModifier
        , cpHelp = "construct a BlockVersionModifier"
        }
    , CommandProc
        { cpName = bvdReadCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            bvdScriptVersion     <- getArg tyScriptVersion "script-version"
            bvdSlotDuration      <- getArg tySecond "slot-duration"
            bvdMaxBlockSize      <- getArg tyByte "max-block-size"
            bvdMaxHeaderSize     <- getArg tyByte "max-header-size"
            bvdMaxTxSize         <- getArg tyByte "max-tx-size"
            bvdMaxProposalSize   <- getArg tyByte "max-proposal-size"
            bvdMpcThd            <- getArg tyCoinPortion "mpc-thd"
            bvdHeavyDelThd       <- getArg tyCoinPortion "heavy-del-thd"
            bvdUpdateVoteThd     <- getArg tyCoinPortion "update-vote-thd"
            bvdUpdateProposalThd <- getArg tyCoinPortion "update-proposal-thd"
            bvdUpdateImplicit    <- getArg tyWord64 "update-implicit"
            bvdSoftforkRule      <- getArg tySoftforkRule "softfork-rule"
            bvdTxFeePolicy       <- getArg tyTxFeePolicy "tx-fee-policy"
            bvdUnlockStakeEpoch  <- getArg tyEpochIndex "unlock-stake-epoch"
            pure BlockVersionData {..}
        , cpRepr = CommandReturn . toValue . ValueBlockVersionData
        , cpHelp = "Construct a ValueBlockVersionData"
        }
    , CommandProc
        { cpName = updBinCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            pusSystemTag <- getArg tySystemTag "system"
            pusInstallerPath <- getArgOpt tyFilePath "installer-path"
            pusBinDiffPath <- getArgOpt tyFilePath "bin-diff-path"
            pure ProposeUpdateSystem{..}
        , cpRepr = CommandReturn . toValue . ValueProposeUpdateSystem
        , cpHelp = "construct a part of the update proposal for binary update"
        }
    , CommandProc
        { cpName = dpCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            adpStakeholderId <- getArg (tyStakeholderId `tyEither` tyPublicKey) "s"
            adpCoinPortion <- getArg tyCoinPortion "p"
            return (adpStakeholderId, adpCoinPortion)
        , cpRepr = \(sId', cp) -> CommandReturn . runIdentity $ do
            sId <- toLeft sId'
            return . toValue $ ValueAddrDistrPart (AddrDistrPart sId cp)
        , cpHelp = "construct an address distribution part"
        }
    , CommandProc
        { cpName = txFeePolicyTxSizeLinearCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            a <- getArg tyCoeff "a"
            b <- getArg tyCoeff "b"
            pure $ TxFeePolicyTxSizeLinear $ TxSizeLinear (a) b
        , cpRepr = CommandReturn . toValue . ValueTxFeePolicy
        , cpHelp = "construct a TxFeePolicy that is linearly proportional to tx size"
        }
    , CommandProc
        { cpName = distrCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = getArgSome tyAddrDistrPart "dp"
        , cpRepr = \parts -> CommandError $ do
            distr <- case parts of
                AddrDistrPart sId coinPortion :| []
                  | coinPortion == maxBound ->
                    return $ SingleKeyDistr sId
                _ -> eitherToThrow $
                    mkMultiKeyDistr . Map.fromList $
                    List.map (\(AddrDistrPart s cp) -> (s, cp)) $
                    NonEmpty.toList parts
            return . toValue $ ValueAddrStakeDistribution distr
        , cpHelp = "construct an address distribution (use 'dp' for each part)"
        }

    , CommandProc
        { cpName = softwareCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            appName <- getArg tyApplicationName "name"
            number <- getArg tyWord32 "n"
            pure (appName, number)
        , cpRepr = \(svAppName, svNumber) -> CommandReturn . toValue $ ValueSoftwareVersion SoftwareVersion{..}
        , cpHelp = "Construct a software version from application name and number"
        }
    ]

bootCommandName :: CommandName
bootCommandName = "boot"

txOutCommandName :: CommandName
txOutCommandName = "tx-out"

softforkRuleCommandName :: CommandName
softforkRuleCommandName = "softfork-rule"

bvmCommandName :: CommandName
bvmCommandName = "bvm"

bvdReadCommandName :: CommandName
bvdReadCommandName = "bvd-read"

updBinCommandName :: CommandName
updBinCommandName = "upd-bin"

dpCommandName :: CommandName
dpCommandName = "dp"

txFeePolicyTxSizeLinearCommandName :: CommandName
txFeePolicyTxSizeLinearCommandName = "tx-fee-policy-tx-size-linear"

distrCommandName :: CommandName
distrCommandName = "distr"

softwareCommandName :: CommandName
softwareCommandName = "software"

class ToLeft m a b where
    toLeft :: Either a b -> m a

instance (Monad m, ToLeft m a b, ToLeft m b c) => ToLeft m a (Either b c) where
    toLeft = toLeft <=< traverse toLeft

instance Monad m => ToLeft m StakeholderId PublicKey where
    toLeft = return . either identity addressHash

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

tyAddrStakeDistr :: Elem components Cardano => TyProjection components AddrStakeDistribution
tyAddrStakeDistr = TyProjection "AddrStakeDistribution" (preview _ValueAddrStakeDistribution <=< fromValue)

tyCoin :: Elem components Core => TyProjection components Coin
tyCoin = fromPreCoin <$> TyProjection "Coin" (toBoundedInteger <=< preview _ValueNumber <=< fromValue)

coinPortionFromDouble :: Double -> Maybe CoinPortion
coinPortionFromDouble a
    | a >= 0, a <= 1 = Just $ unsafeCoinPortionFromDouble a
    | otherwise = Nothing

tyCoinPortion :: Elem components Core => TyProjection components CoinPortion
tyCoinPortion = TyProjection "CoinPortion" (coinPortionFromDouble . toRealFloat <=< preview _ValueNumber <=< fromValue)

sciToInteger :: Scientific -> Maybe Integer
sciToInteger = either (const Nothing) Just . floatingOrInteger @Double @Integer

tyByte :: Elem components Core => TyProjection components Byte
tyByte = fromBytes <$> TyProjection "Byte" (sciToInteger <=< preview _ValueNumber <=< fromValue)

tyScriptVersion :: Elem components Core => TyProjection components ScriptVersion
tyScriptVersion = TyProjection "ScriptVersion" (toBoundedInteger <=< preview _ValueNumber <=< fromValue)

tyStakeholderId :: Elem components Cardano => TyProjection components StakeholderId
tyStakeholderId = TyProjection "StakeholderId" (preview _ValueStakeholderId <=< fromValue)

tyAddrDistrPart :: Elem components Cardano => TyProjection components AddrDistrPart
tyAddrDistrPart = TyProjection "AddrDistrPart" (preview _ValueAddrDistrPart <=< fromValue)

tyEpochIndex :: Elem components Core => TyProjection components EpochIndex
tyEpochIndex = TyProjection "EpochIndex" (toBoundedInteger <=< preview _ValueNumber <=< fromValue)

tyHash :: Elem components Cardano => TyProjection components (Hash a)
tyHash = getAHash <$> TyProjection "Hash" (preview _ValueHash <=< fromValue)

tyBlockVersion :: Elem components Cardano => TyProjection components BlockVersion
tyBlockVersion = TyProjection "BlockVersion" (preview _ValueBlockVersion <=< fromValue)

tySoftwareVersion :: Elem components Cardano => TyProjection components SoftwareVersion
tySoftwareVersion = TyProjection "SoftwareVersion" (preview _ValueSoftwareVersion <=< fromValue)

tyBlockVersionModifier :: Elem components Cardano => TyProjection components BlockVersionModifier
tyBlockVersionModifier = TyProjection "BlockVersionModifier" (preview _ValueBlockVersionModifier <=< fromValue)

tySoftforkRule :: Elem components Cardano => TyProjection components SoftforkRule
tySoftforkRule = TyProjection "SoftforkRule" (preview _ValueSoftforkRule <=< fromValue)

tyTxFeePolicy :: Elem components Cardano => TyProjection components TxFeePolicy
tyTxFeePolicy = TyProjection "TxFeePolicy" (preview _ValueTxFeePolicy <=< fromValue)

tyProposeUpdateSystem :: Elem components Cardano => TyProjection components ProposeUpdateSystem
tyProposeUpdateSystem = TyProjection "ProposeUpdateSystem" (preview _ValueProposeUpdateSystem <=< fromValue)

tySystemTag :: Elem components Core => TyProjection components SystemTag
tySystemTag = TyProjection "SystemTag" (mkSystemTag' <=< preview _ValueString <=< fromValue)

tyApplicationName :: Elem components Core => TyProjection components ApplicationName
tyApplicationName = TyProjection "ApplicationName" (mkApplicationName' <=< preview _ValueString <=< fromValue)

mkSystemTag' :: Text -> Maybe SystemTag
mkSystemTag' str =
    let res = SystemTag str
     in case checkSystemTag res of
            Left _ -> Nothing
            Right _ -> Just res

mkApplicationName' :: Text -> Maybe ApplicationName
mkApplicationName' str =
    let res = ApplicationName str
     in case checkApplicationName res of
            Left _ -> Nothing
            Right _ -> Just res

tySecond :: (TimeUnit a, Elem components Core) => TyProjection components a
tySecond = secToTimeUnit <$> TyProjection "Second" (toDouble <=< preview _ValueNumber <=< fromValue)
  where
    toDouble :: Scientific -> Maybe Double
    toDouble = rightToMaybe . toBoundedRealFloat

    -- Using microseconds here because that's how time-units stores times internally.
    secToTimeUnit :: TimeUnit a => Double -> a
    secToTimeUnit = convertUnit @Microsecond . fromMicroseconds . round . (* 1e6)

tyCoeff :: Elem components Core => TyProjection components Coeff
tyCoeff = Coeff . realToFrac <$> TyProjection "Coeff" (preview _ValueNumber <=< fromValue)
