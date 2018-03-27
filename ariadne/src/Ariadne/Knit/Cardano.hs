module Ariadne.Knit.Cardano where

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
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Mode as Auxx -- TODO: FIXME

import Knit.Value
import Knit.Procedure
import Knit.Eval
import Knit.Syntax
import Knit.Core
import Knit.Tokenizer
import Knit.Parser
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

deriving instance Eq (Value components) => Eq (ComponentValue components Cardano)
deriving instance Ord (Value components) => Ord (ComponentValue components Cardano)
deriving instance Show (Value components) => Show (ComponentValue components Cardano)

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
