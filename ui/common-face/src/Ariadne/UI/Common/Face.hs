module Ariadne.UI.Common.Face
       ( UiLangFace (..)
       , UiFace (..)
       , UiHistoryFace (..)
       , FrontendEvent

       , UiEvent (..)
       , FrontendCommandEvent
       , UiCommandId (..)
       , UiCommandEvent (..)
       , UiBackendEvent (..)
       , UiBackendStatusUpdate (..)
       , UiWalletEvent (..)
       , UiPasswordEvent (..)
       , UiConfirmEvent (..)
       , UiConfirmationType (..)
       , UiConfirmSendInfo (..)
       , UiDeletingItem (..)

       , UiCommand (..)
       , FrontendCommand
       , UiSendOutput (..)
       , UiSendArgs (..)
       , UiFeeArgs (..)
       , UiNewWalletArgs (..)
       , UiNewAccountArgs (..)
       , UiNewAddressArgs (..)
       , UiRestoreWalletArgs (..)
       , UiRenameArgs (..)
       , UiChangePasswordArgs (..)

       , UiCommandResult (..)
       , FrontendCommandResult
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
       , UiChangePasswordCommandResult (..)

       , UiTreeItem (..)
       , UiTree
       , TreePath
       , NonEmptyPath
       , UiTreeSelection(..)
       , UiCurrency
       , UiWalletInfo(..)
       , UiAccountInfo(..)
       , UiAddressInfo(..)
       , UiSelectionInfo(..)
       ) where


import qualified Control.Concurrent.Event as CE
import qualified Data.Text.Buildable as Buildable
import Data.Loc (Loc, Span)
import Data.Tree (Tree)
import Formatting (bprint, int, (%))
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.UX.PasswordManager

----------------------------------------------------------------------------
-- Faces
----------------------------------------------------------------------------

-- The backend language (Knit by default) interface as perceived by the UI.
data UiLangFace frontend = forall err expr. UiLangFace
  { langPutCommand :: expr -> IO UiCommandId
  , langPutUiCommand :: UiCommand frontend -> IO (Either Text UiCommandId)
  , langPutUISilentCommand :: UiCommand frontend -> IO (Either Text UiCommandId)
  , langParse :: Text -> Either err expr
  , langAutocomplete :: Loc -> Text -> [(Loc, Text)]
  , langPpExpr :: expr -> Doc
  , langPpParseError :: err -> Doc
  , langParseErrSpans :: err -> [Span]
  , langGetHelp :: [Doc]
  }

-- API for the UI.
data UiFace frontend = UiFace
  { -- Update the user interface with an event. Does not block unless the
    -- queue of events is full (should not normally happen).
    putUiEvent :: UiEvent frontend -> IO ()
  }


-- Interface for the command history
data UiHistoryFace = UiHistoryFace
  { historyAddCommand :: Text -> IO ()
  , historySetPrefix :: Text -> IO ()
  , historyNextCommand :: IO (Maybe Text)
  , historyPrevCommand :: IO (Maybe Text)
  }

-- | Events as perceived by the UI. They will be generated from backend-specific
-- events in the 'Glue' module. They must be independent from the backends and
-- capture /what the UI can handle/, not what the backends can generate.
data UiEvent frontend
  = UiCommandEvent UiCommandId (UiCommandEvent frontend)
  | UiCommandResult UiCommandId (UiCommandResult frontend)
  | UiBackendEvent UiBackendEvent
  | UiWalletEvent (UiWalletEvent frontend)
  | UiPasswordEvent UiPasswordEvent
  | UiConfirmEvent UiConfirmEvent
  | UiFrontendEvent (FrontendEvent frontend)

-- UiEvent extension type
data family FrontendEvent frontend

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

instance Buildable UiCommandId where
    build UiCommandId {..} =
        case cmdTaskIdRendered of
            Just rendered -> Buildable.build rendered
            Nothing -> bprint ("EqObject:"%int) cmdIdEqObject

instance Eq UiCommandId where
  a == b = cmdIdEqObject a == cmdIdEqObject b

-- A REPL command has either finished or sent some information.
data UiCommandEvent frontend
  = UiCommandSuccess Doc
  | UiCommandFailure Doc
  | UiCommandOutput Doc
  | UiCommandWidget Doc
  | UiFrontendCommandEvent (FrontendCommandEvent frontend)

-- UiCommandEvent extension type
data family FrontendCommandEvent frontend

deriving instance Show (FrontendCommandEvent frontend) => Show (UiCommandEvent frontend)

-- Update current displayed slot, chain difficulty, etc
data UiBackendEvent
  = UiBackendLogEvent Text
  | UiBackendStatusUpdateEvent UiBackendStatusUpdate

data UiBackendStatusUpdate = UiBackendStatusUpdate
  { syncProgress :: Maybe Text
  , blockchainLocal :: Text
  , blockchainNetwork :: Text
  }

data UiWalletEvent frontend =
  UiWalletUpdate
    { wuTrees :: [UiTree]
    , wuSelection :: Maybe UiTreeSelection
    , wuSelectionInfo :: Maybe (UiSelectionInfo frontend)
    }

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

data UiCommand frontend
  = UiSelect [Word]
  | UiSend UiSendArgs
  | UiNewWallet UiNewWalletArgs
  | UiNewAccount UiNewAccountArgs
  | UiNewAddress UiNewAddressArgs
  | UiRestoreWallet UiRestoreWalletArgs
  | UiChangePassword UiChangePasswordArgs
  | UiRename UiRenameArgs
  | UiRemove
  | UiKill Natural
  | UiBalance
  | UiTxHistory
  | UiFee UiFeeArgs
  | UiExport
  | UiFrontendCommand (FrontendCommand frontend)

-- UiCommand extension
data family FrontendCommand frontend

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

data UiChangePasswordArgs = UiChangePasswordArgs

data UiRenameArgs = UiRenameArgs
  { uraName :: !Text
  }

-- | Results of commands issued by the UI widgets
data UiCommandResult frontend
  = UiSendCommandResult UiSendCommandResult
  | UiNewWalletCommandResult UiNewWalletCommandResult
  | UiRestoreWalletCommandResult UiRestoreWalletCommandResult
  | UiNewAccountCommandResult UiNewAccountCommandResult
  | UiNewAddressCommandResult UiNewAddressCommandResult
  | UiBalanceCommandResult UiBalanceCommandResult
  | UiTxHistoryCommandResult UiTxHistoryCommandResult
  | UiFeeCommandResult UiFeeCommandResult
  | UiRenameCommandResult UiRenameCommandResult
  | UiRemoveCommandResult UiRemoveCommandResult
  | UiExportCommandResult UiExportCommandResult
  | UiChangePasswordCommandResult UiChangePasswordCommandResult
  | UiFronendCommandResult (FrontendCommandResult frontend)

-- UiCommandResult extension
data family FrontendCommandResult frontend

data UiBalanceCommandResult
  = UiBalanceCommandSuccess Text
  | UiBalanceCommandFailure Text

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

data UiSendCommandResult
  = UiSendCommandSuccess Text
  | UiSendCommandFailure Text

data UiFeeCommandResult
  = UiFeeCommandSuccess Text
  | UiFeeCommandFailure Text

data UiNewWalletCommandResult
  = UiNewWalletCommandSuccess [Text]
  | UiNewWalletCommandFailure Text

data UiRestoreWalletCommandResult
  = UiRestoreWalletCommandSuccess
  | UiRestoreWalletCommandFailure Text

data UiNewAccountCommandResult
  = UiNewAccountCommandSuccess
  | UiNewAccountCommandFailure Text

data UiNewAddressCommandResult
  = UiNewAddressCommandSuccess
  | UiNewAddressCommandFailure Text

data UiRenameCommandResult
  = UiRenameCommandSuccess
  | UiRenameCommandFailure Text

data UiRemoveCommandResult
  = UiRemoveCommandSuccess
  | UiRemoveCommandFailure Text

data UiExportCommandResult
  = UiExportCommandSuccess Text
  | UiExportCommandFailure Text

data UiChangePasswordCommandResult
  = UiChangePasswordCommandSuccess
  | UiChangePasswordCommandFailure Text

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

type TreePath = [Word]

type NonEmptyPath = NonEmpty Word

data UiTreeSelection = UiTreeSelection
  { wtsWalletIdx :: Word
  , wtsPath :: TreePath
  }
  deriving Eq

-- | Representation of currency on the frontend
data family UiCurrency frontend

-- Display info for entities on all HD-wallet tree levels
data UiWalletInfo frontend = UiWalletInfo
  { uwiLabel :: !(Maybe Text)
  , uwiId :: !Text
  , uwiWalletIdx :: !Word
  , uwiBalance :: !(UiCurrency frontend)
  , uwiAccounts :: ![UiAccountInfo frontend]
  }

deriving instance Eq (UiCurrency frontend) => Eq (UiWalletInfo frontend)

data UiAccountInfo frontend = UiAccountInfo
  { uaciLabel :: !(Maybe Text)
  , uaciWalletIdx :: !Word
  , uaciPath :: !NonEmptyPath
  , uaciBalance :: !(UiCurrency frontend)
  , uaciAddresses :: ![UiAddressInfo frontend]
  }

deriving instance Eq (UiCurrency frontend) => Eq (UiAccountInfo frontend)

data UiAddressInfo frontend = UiAddressInfo
  { uadiWalletIdx :: !Word
  , uadiPath :: !NonEmptyPath
  , uadiAddress :: !Text
  , uadiBalance :: !(UiCurrency frontend)
  }

deriving instance Eq (UiCurrency frontend) => Eq (UiAddressInfo frontend)

-- | Info for currently selected tree item
data UiSelectionInfo frontend
  = UiSelectionWallet !(UiWalletInfo frontend)
  | UiSelectionAccount !(UiAccountInfo frontend)
