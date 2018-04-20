module Ariadne.Cardano.Face
       ( CardanoContext
       , CardanoMode
       , CardanoStatusUpdate (..)
       , CardanoEvent (..)
       , CardanoFace (..)

       -- * Re-exports from Cardano
       , EpochOrSlot (..)
       , EpochIndex (..)
       , SlotId (..)
       , HeaderHash
       , Address
       , TxId
       , TxOut (..)
       , PassPhrase
       , HasConfigurations
       , HasCompileInfo

       , AccountData (..)
       , WalletData (..)
       , UserSecret
       , usWallets
       ) where

import Universum

import Control.Monad.Trans.Reader (ReaderT)
import Data.Constraint (Dict(..))
import IiExtras
import Mockable (Production)
import Pos.Communication.Protocol (SendActions)
import Pos.Core
  (Address, EpochIndex(..), EpochOrSlot(..), HeaderHash, SlotId(..), TxId,
  TxOut(..))
import Pos.Crypto (PassPhrase)
import Pos.Launcher (HasConfigurations)
import Pos.Util.CompileInfo (HasCompileInfo)
import Pos.Util.UserSecret
  (AccountData(..), UserSecret, WalletData(..), usWallets)
import Pos.WorkMode (EmptyMempoolExt, RealModeContext)

data CardanoStatusUpdate = CardanoStatusUpdate
  { tipHeaderHash :: HeaderHash
  , tipEpochOrSlot :: EpochOrSlot
  , currentSlot :: SlotId
  , isInaccurate :: Bool
  }

-- | Events as generated by the Cardano node. They will be translated into
-- UI-compatible events in the 'Glue' module. They must be independent from the
-- UI and capture /what the backend can generate/, not what the frontend can
-- handle.
data CardanoEvent
  = CardanoLogEvent Text
  | CardanoStatusUpdateEvent CardanoStatusUpdate

type CardanoContext = RealModeContext EmptyMempoolExt

type CardanoMode = ReaderT CardanoContext Production

data CardanoFace = CardanoFace
    { cardanoRunCardanoMode :: CardanoMode :~> IO
    , cardanoConfigurations :: Dict HasConfigurations
    , cardanoCompileInfo :: Dict HasCompileInfo
    , cardanoGetSendActions :: CardanoMode (SendActions CardanoMode)
    }
