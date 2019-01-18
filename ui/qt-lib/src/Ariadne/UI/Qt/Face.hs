module Ariadne.UI.Qt.Face
       ( Qt
       , FrontendEvent (..)
       , FrontendCommand (..)
       , UiNewAddressArgs (..)
       , FrontendCommandResult (..)
       , UiCalcTotalCommandResult (..)
       , UiNewAddressCommandResult (..)
       , UiBackendExceptionEvent (..)
       , UiWalletFace (..)
       , Currency (..)
       , UiCurrency (..)
       , module CommonFace
       ) where

import Data.Scientific (Scientific)

import Ariadne.UI.Common.Face as CommonFace
import Serokell.Data.Memory.Units (Byte)


data Qt

-- | Events as perceived by the UI. They will be generated from backend-specific
-- events in the 'Glue' module. They must be independent from the backends and
-- capture /what the UI can handle/, not what the backends can generate.
data instance FrontendEvent Qt
  = UiBackendExceptionEvent UiBackendExceptionEvent

data instance FrontendCommandEvent Qt = NoEvent Void

-- | Commands issued by the UI widgets
data instance FrontendCommand Qt
  = UiCalcTotal [Scientific]
  | UiNewAddress UiNewAddressArgs

data UiNewAddressArgs = UiNewAddressArgs
  { unadaWalletIdx :: !Word
  , unadaAccountIdx :: !Word
  }

data instance FrontendCommandResult Qt
  = UiCalcTotalCommandResult UiCalcTotalCommandResult
  | UiNewAddressCommandResult UiNewAddressCommandResult

data UiCalcTotalCommandResult
  = UiCalcTotalCommandSuccess (Text, Text)
  | UiCalcTotalCommandFailure Text

data UiNewAddressCommandResult
  = UiNewAddressCommandSuccess Word Word Text
  | UiNewAddressCommandFailure Text

-- | Ui event to handle backend exceptions
data UiBackendExceptionEvent = UiBackendException SomeException

-- Interface for the wallet
data UiWalletFace =
  UiWalletFace
    { uiGenerateMnemonic :: Byte -> IO [Text]
    , uiDefaultEntropySize :: Byte
    , uiValidateAddress :: Text -> Maybe Text
    , uiValidateCoin :: Scientific -> Bool
    , uiCoinPrecision :: Int
    }

data Currency = ADA | Lovelace

data instance UiCurrency Qt = UiCurrency Text Currency
