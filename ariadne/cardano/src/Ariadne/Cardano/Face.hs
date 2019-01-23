{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Ariadne.Cardano.Face
       ( BListenerHandle (..)
       , CardanoContext (..)
       , CardanoMode (..)
       , CardanoModeMonad
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
       ) where

import Control.Lens (makeLensesWith)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (withReaderT)
import Control.Monad.Trans.Resource (transResourceT)
import Control.Natural ((:~>))
import Crypto.Random (MonadRandom)
import Data.Conduit (transPipe)
import Data.Constraint (Dict(..))
import Pos.Chain.Block
  (Blund, HasSlogContext(..), HasSlogGState(..), HeaderHash)
import qualified Pos.Chain.Genesis as Genesis
import Pos.Chain.Txp (TxId, TxOut(..))
import Pos.Context (HasNodeContext(..), HasPrimaryKey(..), HasSscContext(..))
import Pos.Core
  (Address, Coin, EpochIndex(..), EpochOrSlot(..), LocalSlotIndex(..),
  SlotId(..), decodeTextAddress)
import Pos.Core.Chrono (NE, NewestFirst, OldestFirst)
import Pos.Core.JsonLog (CanJsonLog(..))
import Pos.Core.Reporting (HasMisbehaviorMetrics(..))
import Pos.Core.Slotting (HasSlottingVar(..), MonadSlotsData)
import Pos.Crypto (PassPhrase)
import Pos.DB (MonadGState(..))
import Pos.DB.Block (MonadBListener(..))
import Pos.DB.Class (MonadDB(..), MonadDBRead(..))
import Pos.DB.Txp (MempoolExt, MonadTxpLocal(..))
import Pos.Infra.Diffusion.Types (Diffusion)
import Pos.Infra.Network.Types (HasNodeType(..))
import Pos.Infra.Reporting (MonadReporting(..))
import Pos.Infra.Shutdown (HasShutdownContext(..))
import Pos.Infra.Slotting.Class (MonadSlots(..))
import Pos.Infra.Util.JsonLog.Events (HasJsonLogConfig(..), jsonLogDefault)
import Pos.Launcher (HasConfigurations)
import Pos.Util.CompileInfo (HasCompileInfo)
import Pos.Util.LoggerName (HasLoggerName'(..))
import Pos.Util.Util (HasLens(..))
import Pos.Util.Wlog (CanLog, HasLoggerName(..))
import Pos.WorkMode (EmptyMempoolExt, RealMode, RealModeContext(..))

import Ariadne.Util

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


data BListenerHandle
  = BListenerHandle
  { bhOnApply :: OldestFirst NE Blund -> IO ()
  , bhOnRollback :: NewestFirst NE Blund -> IO ()
  }

data CardanoContext
  = CardanoContext
  { ccBListenerHandle :: BListenerHandle
  , ccRealModeContext :: RealModeContext EmptyMempoolExt
  }
makeLensesWith postfixLFields ''CardanoContext

type CardanoModeMonad = ReaderT CardanoContext IO
newtype CardanoMode a
  = CardanoMode
  { unwrapCardanoMode :: CardanoModeMonad a
  }
  deriving
    ( HasLoggerName
    , CanLog
    , Functor
    , Applicative
    , Monad
    , MonadReader CardanoContext
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadBase IO
    , MonadBaseControl IO
    , MonadRandom
    , MonadUnliftIO
    )

instance MonadBListener CardanoMode where
  onApplyBlocks _ b = do
    CardanoContext{..} <- ask
    liftIO $ bhOnApply ccBListenerHandle $ b
    pure mempty
  onRollbackBlocks _ _ b = do
    CardanoContext{..} <- ask
    liftIO $ bhOnRollback ccBListenerHandle $ b
    pure mempty

-- ♫ E-E-G-G-E-E-G Mr. Boilerplate!
-- Amazing, with Mr. Boilerplate my code is now twice as long!!!

instance HasNodeType CardanoContext where
    getNodeType = getNodeType . ccRealModeContext

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext EmptyMempoolExt) r =>
    HasLens tag CardanoContext r
  where
    lensOf = ccRealModeContextL . lensOf @tag

instance HasSscContext CardanoContext where
    sscContext = ccRealModeContextL . sscContext

instance HasPrimaryKey CardanoContext where
    primaryKey = ccRealModeContextL . primaryKey

instance HasMisbehaviorMetrics CardanoContext where
    misbehaviorMetrics = ccRealModeContextL . misbehaviorMetrics

instance HasShutdownContext CardanoContext where
    shutdownContext = ccRealModeContextL . shutdownContext

instance HasSlottingVar CardanoContext where
    slottingTimestamp = ccRealModeContextL . slottingTimestamp
    slottingVar = ccRealModeContextL . slottingVar

instance HasSlogContext CardanoContext where
    slogContext = ccRealModeContextL . slogContext

instance HasSlogGState CardanoContext where
    slogGState = ccRealModeContextL . slogGState

instance HasNodeContext CardanoContext where
    nodeContext = ccRealModeContextL . nodeContext

instance HasLoggerName' CardanoContext where
    loggerName = ccRealModeContextL . loggerName

instance HasJsonLogConfig CardanoContext where
    jsonLogConfig = ccRealModeContextL . jsonLogConfig

realModeToCardanoMode :: RealMode EmptyMempoolExt a -> CardanoMode a
realModeToCardanoMode m = CardanoMode $ withReaderT ccRealModeContext m

instance {-# OVERLAPPING #-} CanJsonLog CardanoMode where
    jsonLog = jsonLogDefault

instance (MonadSlotsData ctx CardanoMode)
      => MonadSlots ctx CardanoMode
  where
    getCurrentSlot = realModeToCardanoMode ... getCurrentSlot
    getCurrentSlotBlocking = realModeToCardanoMode ... getCurrentSlotBlocking
    getCurrentSlotInaccurate = realModeToCardanoMode ... getCurrentSlotInaccurate
    currentTimeSlotting = realModeToCardanoMode ... currentTimeSlotting

instance MonadGState CardanoMode where
    gsAdoptedBVData = realModeToCardanoMode ... gsAdoptedBVData

instance MonadDBRead CardanoMode where
    dbGet = realModeToCardanoMode ... dbGet
    dbIterSource tag p =
      transPipe (transResourceT realModeToCardanoMode) (dbIterSource tag p)
    dbGetSerBlock = realModeToCardanoMode ... dbGetSerBlock
    dbGetSerUndo = realModeToCardanoMode ... dbGetSerUndo
    dbGetSerBlund = realModeToCardanoMode ... dbGetSerBlund

instance MonadDB CardanoMode where
    dbPut = realModeToCardanoMode ... dbPut
    dbWriteBatch = realModeToCardanoMode ... dbWriteBatch
    dbDelete = realModeToCardanoMode ... dbDelete
    dbPutSerBlunds = realModeToCardanoMode ... dbPutSerBlunds

instance MonadTxpLocal CardanoMode where
    txpNormalize = realModeToCardanoMode ... txpNormalize
    txpProcessTx = realModeToCardanoMode ... txpProcessTx

instance MonadReporting CardanoMode where
    report = realModeToCardanoMode ... report

type instance MempoolExt CardanoMode = EmptyMempoolExt

data CardanoFace = CardanoFace
    { cardanoRunCardanoMode :: CardanoMode :~> IO
    , cardanoConfigurations :: Dict HasConfigurations
    , cardanoCompileInfo :: Dict HasCompileInfo
    , cardanoGetDiffusion :: CardanoMode (Diffusion CardanoMode)
    , cardanoGenesisConfig :: !Genesis.Config
    }
