module Ariadne.UI.Qt.Face
       ( UiCommandId (..)
       , UiCommandEvent (..)
       , UiWalletEvent (..)
       , UiBackendStatusUpdate (..)
       , UiBackendExceptionEvent (..)
       , UiBackendEvent (..)
       , UiEvent (..)
       , UiCommand (..)
       , UiCommandResult (..)
       , UiSendOutput (..)
       , UiSendArgs (..)
       , UiSendCommandResult (..)
       , UiCalcTotalCommandResult (..)
       , UiNewWalletCommandResult (..)
       , UiRestoreWalletCommandResult (..)
       , UiNewAccountCommandResult (..)
       , UiNewAddressCommandResult (..)
       , UiChangePasswordCommandResult (..)
       , UiLangFace (..)
       , UiWalletFace (..)
       , UiHistoryFace (..)
       , UiPasswordEvent (..)
       , UiConfirmEvent (..)
       , UiConfirmationType (..)
       , UiConfirmSendInfo (..)
       , UiDeletingItem (..)
       , UiFace (..)

       , UiWalletTreeItem (..)
       , UiWalletTree
       , UiWalletTreeSelection(..)
       , TreePath
       , UiCurrency(..)
       , UiWalletInfo(..)
       , UiAccountInfo(..)
       , UiAddressInfo(..)
       , UiSelectionInfo(..)
       ) where

import qualified Control.Concurrent.Event as CE
import Data.Loc.Span (Span)
import Data.Scientific (Scientific)
import Data.Tree (Tree)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.UX.PasswordManager
import Serokell.Data.Memory.Units (Byte)

data UiCommandId =
  UiCommandId
  { -- This field is used to compare whether two command identifiers are equal.
    -- The mapping from actual command identifiers to these integers must be
    -- injective.
    cmdIdEqObject :: Natural
  , -- This field is the visual representation of a command identifier. The
    -- mapping from actual command identifiers to text need not be injective,
    -- but it would be very unfair to the user, as different command identifiers
    -- would appear the same to her.
    cmdTaskIdRendered :: Maybe Text
    -- Task identifier object.
  , cmdTaskId :: Maybe Natural
  }

instance Eq UiCommandId where
  a == b = cmdIdEqObject a == cmdIdEqObject b

-- A REPL command has either finished or sent some information.
data UiCommandEvent
  = UiCommandSuccess Doc
  | UiCommandFailure Doc
  | UiCommandOutput Doc

data UiBackendStatusUpdate = UiBackendStatusUpdate
  { syncProgress :: Maybe Text
  , blockchainLocal :: Text
  , blockchainNetwork :: Text
  }

-- Update current displayed slot, chain difficulty, etc
data UiBackendEvent
  = UiBackendLogEvent Text
  | UiBackendStatusUpdateEvent UiBackendStatusUpdate

data UiWalletEvent =
  UiWalletUpdate
    { wuTrees :: [UiWalletTree]
    , wuSelection :: Maybe UiWalletTreeSelection
    , wuSelectionInfo :: Maybe UiSelectionInfo
    }

-- | Events as perceived by the UI. They will be generated from backend-specific
-- events in the 'Glue' module. They must be independent from the backends and
-- capture /what the UI can handle/, not what the backends can generate.
data UiEvent
  = UiCommandEvent UiCommandId UiCommandEvent
  | UiCommandResult UiCommandId UiCommandResult
  | UiBackendEvent UiBackendEvent
  | UiWalletEvent UiWalletEvent
  | UiPasswordEvent UiPasswordEvent
  | UiConfirmEvent UiConfirmEvent
  | UiBackendExceptionEvent UiBackendExceptionEvent

-- | Commands issued by the UI widgets
data UiCommand
  = UiSelect [Word]
  | UiSend UiSendArgs
  | UiCalcTotal [Scientific]
  | UiChangePassword
  | UiNewWallet Text (Maybe Text) -- ^ Name, password
  | UiRestoreWallet Text (Maybe Text) Text -- ^ Name, password, mnemonic
  | UiNewAccount Text  -- ^ Name
  | UiNewAddress Word Word -- ^ Wallet index, account index
  | UiRename Text -- ^ New name
  | UiKill Natural
  | UiRemoveCurrentItem

data UiSendOutput = UiSendOutput
  { usoAmount :: Scientific
  , usoAddress :: Text
  }

data UiSendArgs = UiSendArgs
  { usaWalletIdx :: Word
  , usaAccounts :: [Word]
  , usaOutputs :: [UiSendOutput]
  }

-- | Results of commands issued by the UI widgets
data UiCommandResult
  = UiSendCommandResult UiSendCommandResult
  | UiCalcTotalCommandResult UiCalcTotalCommandResult
  | UiNewWalletCommandResult UiNewWalletCommandResult
  | UiRestoreWalletCommandResult UiRestoreWalletCommandResult
  | UiNewAccountCommandResult UiNewAccountCommandResult
  | UiNewAddressCommandResult UiNewAddressCommandResult
  | UiChangePasswordCommandResult UiChangePasswordCommandResult

data UiSendCommandResult
  = UiSendCommandSuccess Text
  | UiSendCommandFailure Text

data UiCalcTotalCommandResult
  = UiCalcTotalCommandSuccess (Text, Text)
  | UiCalcTotalCommandFailure Text

data UiNewWalletCommandResult
  = UiNewWalletCommandSuccess
  | UiNewWalletCommandFailure Text

data UiRestoreWalletCommandResult
  = UiRestoreWalletCommandSuccess
  | UiRestoreWalletCommandFailure Text

data UiNewAccountCommandResult
  = UiNewAccountCommandSuccess
  | UiNewAccountCommandFailure Text

data UiNewAddressCommandResult
  = UiNewAddressCommandSuccess Word Word Text
  | UiNewAddressCommandFailure Text

data UiChangePasswordCommandResult
  = UiChangePasswordCommandSuсcess
  | UiChangePasswordCommandFailure Text

-- | Ui event triggered by the password manager
data UiPasswordEvent
  = UiPasswordRequest PasswordRequestMode WalletId CE.Event

-- | Ui event to handle confirmations
data UiConfirmEvent
  = UiConfirmRequest (MVar Bool) UiConfirmationType

data UiConfirmationType
  = UiConfirmMnemonic [Text]          -- ^ mnemonic
  | UiConfirmRemove UiDeletingItem    -- ^ selection
  | UiConfirmSend [UiConfirmSendInfo] -- ^ lists of outputs

data UiConfirmSendInfo =
  UiConfirmSendInfo
    { csiAddress :: Text
    , csiAmount  :: Text
    , csiCoin    :: Text
    }

data UiDeletingItem
  = UiDelWallet (Maybe Text)
  | UiDelAccount (Maybe Text)
  | UiDelUnknownKeys Text
  | UiDelBrokenWallets Text
  deriving Eq

-- | Ui event to handle backend exceptions
data UiBackendExceptionEvent = UiBackendException SomeException

-- The backend language (Knit by default) interface as perceived by the UI.
data UiLangFace =
  forall err expr. UiLangFace
  { langPutCommand :: expr -> IO UiCommandId
  , langPutUiCommand :: UiCommand -> IO (Either Text UiCommandId)
  , langParse :: Text -> Either err expr
  , langPpExpr :: expr -> Doc
  , langPpParseError :: err -> Doc
  , langParseErrSpans :: err -> [Span]
  , langGetHelp :: [Doc]
  }

-- Interface for the wallet
data UiWalletFace =
  UiWalletFace
    { uiGenerateMnemonic :: Byte -> IO [Text]
    , uiDefaultEntropySize :: Byte
    , uiValidateAddress :: Text -> Maybe Text
    , uiValidateCoin :: Scientific -> Bool
    , uiCoinPrecision :: Int
    }

-- Interface for the command history
data UiHistoryFace =
  UiHistoryFace
    { historyAddCommand :: Text -> IO ()
    , historySetPrefix :: Text -> IO ()
    , historyNextCommand :: IO (Maybe Text)
    , historyPrevCommand :: IO (Maybe Text)
    }

-- API for the UI.
data UiFace = UiFace
    { putUiEvent :: UiEvent -> IO ()
    -- ^ Update the user interface with an event. Does not block unless the
    -- queue of events is full (should not normally happen).
    }

----------------------------------------------------------------------------
-- Wallet tree widget model
----------------------------------------------------------------------------

-- | A node in HD-wallet tree.
data UiWalletTreeItem = UiWalletTreeItem
    { wtiLabel :: !(Maybe Text)
    -- ^ Some text to display (e. g. wallet's name).
    , wtiPath :: ![Word]
    -- ^ Path to this node in the tree. Can be used as an identifier
    -- (hopefully).
    , wtiShowPath :: !Bool
    -- ^ Whether the path should be displayed.
    }

type UiWalletTree = Tree UiWalletTreeItem

-- | Path in a 'Tree'.
--
-- N.B. The head of this list is the index in root's children.
-- I find this order more intuitive, but if perfomance turns out
-- to be an issue, we may consider changing it.
type TreePath = [Word]

type NonEmptyPath = NonEmpty Word

data UiWalletTreeSelection =
  UiWalletTreeSelection
    { wtsWalletIdx :: Word
    , wtsPath :: TreePath
    }

data UiCurrency = ADA | Lovelace

-- Display info for entities on all HD-wallet tree levels
data UiWalletInfo = UiWalletInfo
  { uwiLabel :: !(Maybe Text)
  , uwiWalletId :: !Text
  , uwiWalletIdx :: !Word
  , uwiBalance :: !(Text, UiCurrency)
  , uwiAccounts :: ![UiAccountInfo]
  }

data UiAccountInfo = UiAccountInfo
  { uaciLabel :: !(Maybe Text)
  , uaciWalletIdx :: !Word
  , uaciPath :: !NonEmptyPath
  , uaciBalance :: !(Text, UiCurrency)
  , uaciAddresses :: ![UiAddressInfo]
  }

data UiAddressInfo = UiAddressInfo
  { uadiWalletIdx :: !Word
  , uadiPath :: !NonEmptyPath
  , uadiAddress :: !Text
  , uadiBalance :: !(Text, UiCurrency)
  }

-- | Info for currently selected tree item
data UiSelectionInfo
  = UiSelectionWallet !UiWalletInfo
  | UiSelectionAccount !UiAccountInfo
