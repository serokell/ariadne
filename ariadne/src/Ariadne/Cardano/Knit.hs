module Ariadne.Cardano.Knit where

import Universum

import Data.Vinyl.TypeLevel

import Pos.Core
import Pos.Core.Txp (TxOut)
import Pos.Update (BlockVersionData, BlockVersionModifier, SystemTag)
import Pos.Crypto
import Pos.Util.Util (toParsecError)
import Mockable (runProduction)
import Text.Earley
import Control.Lens
import Data.List as List
import Data.Map as Map
import Data.Scientific
import Data.Fixed
import Data.Time.Units
import Data.Coerce (coerce)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import qualified Ariadne.Cardano.Mode as Auxx

import Knit.Value
import Knit.Procedure
import Knit.Eval
import Knit.Syntax
import Knit.Core
import Knit.Tokenizer
import Knit.Parser
import Knit.Inflate
import Knit.Utils

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
          , componentInflate $ ValueNumber (fromIntegral $ getCoin txOutValue)
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

instance Elem components Cardano => ComponentLitGrammar components Cardano where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitAddress <$> tok (_Token . uprismElem . _TokenAddress)
      , toLit . LitPublicKey <$> tok (_Token . uprismElem . _TokenPublicKey)
      , toLit . LitStakeholderId <$> tok (_Token . uprismElem . _TokenStakeholderId)
      , toLit . LitHash <$> tok (_Token . uprismElem . _TokenHash)
      , toLit . LitBlockVersion <$> tok (_Token . uprismElem . _TokenBlockVersion)
      ]

data instance ComponentCommandRepr components Cardano
  = CommandAction (Auxx.AuxxMode (Value components))

instance ComponentLitToValue components Cardano where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x
    LitPublicKey x -> ValuePublicKey x
    LitStakeholderId x -> ValueStakeholderId x
    LitHash x -> ValueHash x
    LitBlockVersion x -> ValueBlockVersion x

newtype instance ComponentExecContext Cardano =
  CardanoExecCtx (IO Auxx.AuxxContext)

instance (MonadIO m, Show (Value components)) => ComponentCommandExec m components Cardano where
  componentCommandExec (CardanoExecCtx getAuxxContext) (CommandAction auxxModeAction) = do
    auxxContext <- liftIO getAuxxContext
    liftIO . runProduction . usingReaderT auxxContext $ auxxModeAction

instance (AllConstrained (Elem components) '[Cardano, Core]) => ComponentCommandProcs components Cardano where
  componentCommandProcs =
    [
      CommandProc
        { cpName = "cardano"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ return (toValue (ValueBool True))
        , cpHelp = "test cardano infra"
        }
    ]
