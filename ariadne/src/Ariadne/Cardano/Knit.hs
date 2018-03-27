module Ariadne.Cardano.Knit where

import Universum

import Data.Vinyl.TypeLevel

import Pos.Core (AddrStakeDistribution, Address, BlockVersion, CoinPortion,
                 SoftwareVersion, StakeholderId)
import Pos.Core.Txp (TxOut)
import Pos.Crypto (AHash, PublicKey)
import Pos.Update (BlockVersionData, BlockVersionModifier, SystemTag)
import Mockable (runProduction)
import Text.Earley
import qualified Ariadne.Cardano.Mode as Auxx

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

deriving instance Eq (Value components) => Eq (ComponentValue components Cardano)
deriving instance Ord (Value components) => Ord (ComponentValue components Cardano)
deriving instance Show (Value components) => Show (ComponentValue components Cardano)

data instance ComponentLit Cardano
  = LitAddress Address
  | LitPublicKey PublicKey
  | LitStakeholderId StakeholderId
  | LitHash AHash
  | LitBlockVersion BlockVersion
  | LitSoftwareVersion SoftwareVersion
  deriving (Eq, Ord, Show)

data instance ComponentToken Cardano

deriving instance Eq (ComponentToken Cardano)
deriving instance Ord (ComponentToken Cardano)
deriving instance Show (ComponentToken Cardano)

instance ComponentTokenizer components Cardano where
  componentTokenizer = empty

instance ComponentLitGrammar components Cardano where
  componentLitGrammar = rule empty

data instance ComponentCommandRepr components Cardano
  = CommandAction (Auxx.AuxxMode (Value components))

instance ComponentLitToValue components Cardano where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x
    LitPublicKey x -> ValuePublicKey x
    LitStakeholderId x -> ValueStakeholderId x
    LitHash x -> ValueHash x
    LitBlockVersion x -> ValueBlockVersion x
    LitSoftwareVersion x -> ValueSoftwareVersion x

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
