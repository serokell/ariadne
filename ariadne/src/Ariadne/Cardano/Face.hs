module Ariadne.Cardano.Face
       ( CardanoContext
       , CardanoMode
       , CardanoStatusUpdate (..)
       , CardanoEvent (..)
       , CardanoFace (..)

       -- * Re-exports from Cardano
       , Coin
       , EpochOrSlot (..)
       , EpochIndex (..)
       , SlotId (..)
       , LocalSlotIndex (..)
       , HeaderHash
       , Address
       , TxId
       , TxOut (..)
       , PassPhrase
       , HasConfigurations
       , HasCompileInfo
       , decodeTextAddress

       , UserSecret
       , usWallets
       ) where

import Universum

import Control.Monad.Trans.Reader (ReaderT)
import Data.Constraint (Dict(..))
import IiExtras
import Mockable (Production)
import Pos.Core
  (Address, Coin, EpochIndex(..), EpochOrSlot(..), HeaderHash,
  LocalSlotIndex(..), SlotId(..), TxId, TxOut(..), decodeTextAddress)
import Pos.Crypto (PassPhrase)
import Pos.Infra.Diffusion.Types (Diffusion)
import Pos.Launcher (HasConfigurations)
import Pos.Util.CompileInfo (HasCompileInfo)
import Pos.Util.UserSecret (UserSecret, usWallets)
import Pos.WorkMode (EmptyMempoolExt, RealModeContext)

data CardanoStatusUpdate = CardanoStatusUpdate
  { tipHeaderHash :: HeaderHash
  , tipEpochOrSlot :: EpochOrSlot
  , currentSlot :: SlotId
  , isInaccurate :: Bool
  , syncProgress :: Maybe Rational
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
    , cardanoGetDiffusion :: CardanoMode (Diffusion CardanoMode)
    }
