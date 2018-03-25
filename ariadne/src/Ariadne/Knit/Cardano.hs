module Ariadne.Knit.Cardano where

import           Universum

import           Pos.Core (AddrStakeDistribution, Address, BlockVersion, CoinPortion,
                           SoftwareVersion, StakeholderId)
import           Pos.Core.Txp (TxOut)
import           Pos.Crypto (AHash, PublicKey)
import           Pos.Update (BlockVersionData, BlockVersionModifier, SystemTag)

import           Knit.Value
import           Knit.Procedure
import           Knit.Eval
import           Knit.Syntax

-- TODO
type AuxxMode = Identity

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

data instance ComponentLit Cardano
  = LitAddress Address
  | LitPublicKey PublicKey
  | LitStakeholderId StakeholderId
  | LitHash AHash
  | LitBlockVersion BlockVersion
  | LitSoftwareVersion SoftwareVersion
  deriving (Eq, Ord, Show)

data instance ComponentCommandRepr components Cardano
  = CommandAction (AuxxMode (Value components))

instance ComponentLitToValue components Cardano where
  componentLitToValue = \case
    LitAddress x -> ValueAddress x
    LitPublicKey x -> ValuePublicKey x
    LitStakeholderId x -> ValueStakeholderId x
    LitHash x -> ValueHash x
    LitBlockVersion x -> ValueBlockVersion x
    LitSoftwareVersion x -> ValueSoftwareVersion x
