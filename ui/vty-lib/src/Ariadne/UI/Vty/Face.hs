module Ariadne.UI.Vty.Face
       ( UiFeatures (..)

       , UiFace (..)
       , UiLangFace (..)
       , UiHistoryFace (..)

       , UiEvent (..)
       , UiCommandId (..)
       , UiCommandEvent (..)
       , UiCommandAction (..)
       , UiBackendEvent (..)
       , UiBackendStatusUpdate (..)
       , UiWalletEvent (..)
       , UiNewVersionEvent (..)
       , UiPasswordEvent (..)
       , UiConfirmEvent (..)
       , UiConfirmationType (..)
       , UiConfirmSendInfo (..)
       , UiDeletingItem (..)

       , UiCommand (..)
       , UiSendOutput (..)
       , UiSendArgs (..)
       , UiFeeArgs (..)
       , UiNewWalletArgs (..)
       , UiNewAccountArgs (..)
       , UiNewAddressArgs (..)
       , UiRestoreWalletArgs (..)
       , UiRenameArgs (..)

       , UiCommandResult (..)
       , UiBalanceCommandResult (..)
       , UiTxHistoryRowPart (..)
       , UiTxHistoryRow (..)
       , UiTxHistoryCommandResult (..)
       , UiSendCommandResult (..)
       , UiFeeCommandResult (..)
       , UiNewWalletCommandResult (..)
       , UiNewAccountCommandResult (..)
       , UiNewAddressCommandResult (..)
       , UiRestoreWalletCommandResult (..)
       , UiRenameCommandResult (..)
       , UiRemoveCommandResult (..)
       , UiExportCommandResult (..)

       , UiTreeItem (..)
       , UiTree
       , UiTreeSelection(..)
       , TreePath
       , UiWalletInfo(..)
       , UiAccountInfo(..)
       , UiAddressInfo(..)
       , UiSelectionInfo(..)
       ) where

import qualified Control.Concurrent.Event as CE
import Data.Loc (Loc, Span)
import Data.Tree (Tree)
import Data.Version (Version)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.UX.PasswordManager (WalletId)

-- | UI library settings for a particular currency implementation
-- Mostly boolean flags for enabled widgets
data UiFeatures = UiFeatures
  { featureStatus :: !Bool
  , featureExport :: !Bool
  , featureAccounts :: !Bool
  , featureTxHistory :: !Bool
  , featureSecretKeyName :: !Text  -- ^ "Secret key"/"Mnemonic"/etc
  }

----------------------------------------------------------------------------
-- Faces
----------------------------------------------------------------------------

-- API for the UI.
data UiFace = UiFace
  { -- Update the user interface with an event. Does not block unless the
    -- queue of events is full (should not normally happen).
    putUiEvent :: UiEvent -> IO ()
  }

-- The backend language (Knit by default) interface as perceived by the UI.
data UiLangFace = forall err expr. UiLangFace
  { langPutCommand :: expr -> IO UiCommandId
  , langPutUiCommand :: UiCommand -> IO (Either Text UiCommandId)
  , langPutUISilentCommand :: UiCommand -> IO (Either Text UiCommandId)
  , langParse :: Text -> Either err expr
  , langAutocomplete :: Loc -> Text -> [(Loc, Text)]
  , langPpExpr :: expr -> Doc
  , langPpParseError :: err -> Doc
  , langParseErrSpans :: err -> [Span]
  , langGetHelp :: [Doc]
  }

-- Interface for the command history
data UiHistoryFace = UiHistoryFace
  { historyAddCommand :: Text -> IO ()
  , historySetPrefix :: Text -> IO ()
  , historyNextCommand :: IO (Maybe Text)
  , historyPrevCommand :: IO (Maybe Text)
  }

----------------------------------------------------------------------------
-- UI events and their payloads
----------------------------------------------------------------------------

-- | Events as perceived by the UI. They will be generated from backend-specific
-- events in the 'Glue' module. They must be independent from the backends and
-- capture /what the UI can handle/, not what the backends can generate.
data UiEvent
  = UiCommandEvent UiCommandId UiCommandEvent
  | UiCommandResult UiCommandId UiCommandResult
  | UiCommandAction UiCommandAction
  | UiBackendEvent UiBackendEvent
  | UiWalletEvent UiWalletEvent
  | UiNewVersionEvent UiNewVersionEvent
  | UiPasswordEvent UiPasswordEvent
  | UiConfirmEvent UiConfirmEvent

data UiCommandId = UiCommandId
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
  | UiCommandWidget Doc

-- UI event triggered by REPL command
data UiCommandAction
  = UiCommandHelp
  | UiCommandLogs
  | UiCommandQuit

-- Update current displayed slot, chain difficulty, etc
data UiBackendEvent
  = UiBackendLogEvent Text
  | UiBackendStatusUpdateEvent UiBackendStatusUpdate

data UiBackendStatusUpdate = UiBackendStatusUpdate
  { syncProgress :: Maybe Text
  , blockchainLocal :: Text
  , blockchainNetwork :: Text
  }

-- Full Wallet update
data UiWalletEvent = UiWalletUpdate
  { wuTrees :: [UiTree]
  , wuSelection :: Maybe UiTreeSelection
  , wuSelectionInfo :: Maybe UiSelectionInfo
  }

data UiNewVersionEvent = UiNewVersion
  { nvVersion :: Version
  , nvUpdateURL :: Text
  }

-- | Ui event triggered by the password manager
data UiPasswordEvent
  = UiPasswordRequest WalletId CE.Event
  | UiPasswordSent

-- | Ui event to handle confirmations
data UiConfirmEvent
  = UiConfirmRequest (MVar Bool) UiConfirmationType
  | UiConfirmDone

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

----------------------------------------------------------------------------
-- UI commands
----------------------------------------------------------------------------

-- | Commands issued by the UI widgets
data UiCommand
  = UiSelect [Word]
  | UiBalance
  | UiTxHistory
  | UiSend UiSendArgs
  | UiFee UiFeeArgs
  | UiNewWallet UiNewWalletArgs
  | UiNewAccount UiNewAccountArgs
  | UiNewAddress UiNewAddressArgs
  | UiRestoreWallet UiRestoreWalletArgs
  | UiRename UiRenameArgs
  | UiRemove
  | UiExport
  | UiKill Natural

data UiSendOutput = UiSendOutput
  { usoAddress :: !Text
  , usoAmount :: !Text
  }

data UiSendArgs = UiSendArgs
  { usaWalletIdx :: !(Maybe Word)
  , usaAccounts :: ![Word32]
  , usaOutputs :: [UiSendOutput]
  , usaPassphrase :: !Text
  }

data UiFeeArgs = UiFeeArgs
  { ufaWalletIdx :: !(Maybe Word)
  , ufaAccounts :: ![Word32]
  , ufaOutputs :: [UiSendOutput]
  }

data UiNewWalletArgs = UiNewWalletArgs
  { unwaName :: !Text
  , unwaPassphrase :: !Text
  }

data UiNewAccountArgs = UiNewAccountArgs
  { unaaWalletIdx :: !(Maybe Word)
  , unaaName :: !Text
  }

data UiNewAddressArgs = UiNewAddressArgs
  { unadaWalletIdx :: !(Maybe Word)
  , unadaAccountIdx :: !(Maybe Word)
  }

data UiRestoreWalletArgs = UiRestoreWalletArgs
  { urwaName :: !Text
  , urwaMnemonic :: !Text
  , urwaPassphrase :: !Text
  }

data UiRenameArgs = UiRenameArgs
  { uraName :: !Text
  }

----------------------------------------------------------------------------
-- UI command results
----------------------------------------------------------------------------

-- | Results of commands issued by the UI widgets
data UiCommandResult
  = UiBalanceCommandResult UiBalanceCommandResult
  | UiTxHistoryCommandResult UiTxHistoryCommandResult
  | UiSendCommandResult UiSendCommandResult
  | UiFeeCommandResult UiFeeCommandResult
  | UiNewWalletCommandResult UiNewWalletCommandResult
  | UiNewAccountCommandResult UiNewAccountCommandResult
  | UiNewAddressCommandResult UiNewAddressCommandResult
  | UiRestoreWalletCommandResult UiRestoreWalletCommandResult
  | UiRenameCommandResult UiRenameCommandResult
  | UiRemoveCommandResult UiRemoveCommandResult
  | UiExportCommandResult UiExportCommandResult

data UiBalanceCommandResult
  = UiBalanceCommandSuccess Text
  | UiBalanceCommandFailure Text

data UiSendCommandResult
  = UiSendCommandSuccess Text
  | UiSendCommandFailure Text

data UiFeeCommandResult
  = UiFeeCommandSuccess Text
  | UiFeeCommandFailure Text

data UiTxHistoryRowPart = UiTxHistoryRowPart
  { uthrpAddress :: Text
  , uthrpAmount :: Text
  }
  deriving (Eq, Show)

data UiTxHistoryRow = UiTxHistoryRow
  { uthrId :: Text
  , uthrTotal :: Text
  , uthrFrom :: [UiTxHistoryRowPart]
  , uthrTo :: [UiTxHistoryRowPart]
  }
  deriving (Eq, Show)

data UiTxHistoryCommandResult
  = UiTxHistoryCommandSuccess [UiTxHistoryRow]
  | UiTxHistoryCommandFailure Text

data UiNewWalletCommandResult
  = UiNewWalletCommandSuccess [Text]
  | UiNewWalletCommandFailure Text

data UiNewAccountCommandResult
  = UiNewAccountCommandSuccess
  | UiNewAccountCommandFailure Text

data UiNewAddressCommandResult
  = UiNewAddressCommandSuccess
  | UiNewAddressCommandFailure Text

data UiRestoreWalletCommandResult
  = UiRestoreWalletCommandSuccess
  | UiRestoreWalletCommandFailure Text

data UiRenameCommandResult
  = UiRenameCommandSuccess
  | UiRenameCommandFailure Text

data UiRemoveCommandResult
  = UiRemoveCommandSuccess
  | UiRemoveCommandFailure Text

data UiExportCommandResult
  = UiExportCommandSuccess Text
  | UiExportCommandFailure Text

----------------------------------------------------------------------------
-- Wallet widget model
----------------------------------------------------------------------------

-- | A node in HD-wallet tree.
data UiTreeItem = UiTreeItem
  { wtiLabel :: !(Maybe Text)
  -- ^ Some text to display (e. g. wallet's name).
  , wtiPath :: ![Word]
  -- ^ Path to this node in the tree. Can be used as an identifier
  -- (hopefully).
  , wtiShowPath :: !Bool
  -- ^ Whether the path should be displayed.
  }

type UiTree = Tree UiTreeItem

-- | Path in a 'Tree'.
--
-- N.B. The head of this list is the index in root's children.
-- I find this order more intuitive, but if perfomance turns out
-- to be an issue, we may consider changing it.
type TreePath = [Word]

data UiTreeSelection = UiTreeSelection
  { wtsWalletIdx :: Word
  , wtsPath :: TreePath
  }

-- Display info for entities on all HD-wallet tree levels
data UiWalletInfo = UiWalletInfo
  { uwiLabel :: !(Maybe Text)
  , uwiId :: !Text
  , uwiWalletIdx :: !Word
  , uwiBalance :: !(Maybe Text)
  , uwiAccounts :: ![UiAccountInfo]
  }

instance Eq UiWalletInfo where
  a == b = uwiWalletIdx a == uwiWalletIdx b

data UiAccountInfo = UiAccountInfo
  { uaciLabel :: !(Maybe Text)
  , uaciWalletIdx :: !Word
  , uaciPath :: !TreePath
  , uaciBalance :: !(Maybe Text)
  , uaciAddresses :: ![UiAddressInfo]
  }

instance Eq UiAccountInfo where
  a == b =
    uaciWalletIdx a == uaciWalletIdx b &&
    uaciPath a == uaciPath b

data UiAddressInfo = UiAddressInfo
  { uadiWalletIdx :: !Word
  , uadiPath :: !TreePath
  , uadiAddress :: !Text
  , uadiBalance :: !(Maybe Text)
  }

instance Eq UiAddressInfo where
  a == b =
    uadiWalletIdx a == uadiWalletIdx b &&
    uadiPath a == uadiPath b

-- | Info for currently selected tree item
data UiSelectionInfo
  = UiSelectionWallet !UiWalletInfo
  | UiSelectionAccount !UiAccountInfo
