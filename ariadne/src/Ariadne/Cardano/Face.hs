module Ariadne.Cardano.Face
       ( CardanoContext
       , CardanoMode

       -- * Re-exports from Cardano
       , WalletUserSecret (..)
       , UserSecret
       , usWallet
       ) where

import Control.Monad.Trans.Reader (ReaderT)
import Mockable (Production)
import Pos.Util.UserSecret (UserSecret, WalletUserSecret(..), usWallet)
import Pos.WorkMode (EmptyMempoolExt, RealModeContext)

-- | Events as generated by the Cardano node. They will be translated into
-- UI-compatible events in the 'Glue' module. They must be independent from the
-- UI and capture /what the backend can generate/, not what the frontend can
-- handle.
data CardanoEvent =
  -- Report the latest known slot, chain difficulty, etc.
  CardanoEventMock

type CardanoContext = RealModeContext EmptyMempoolExt

type CardanoMode = ReaderT CardanoContext Production
