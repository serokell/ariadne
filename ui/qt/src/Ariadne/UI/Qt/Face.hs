module Ariadne.UI.Qt.Face
       ( UiCommandId (..)
       , UiCommandEvent (..)
       , UiWalletEvent (..)
       , UiCardanoStatusUpdate (..)
       , UiCardanoEvent (..)
       , UiEvent (..)
       , UiCommand (..)
       , UiCommandResult (..)
       , UiSendCommandResult (..)
       , UiNewWalletCommandResult (..)
       , UiNewAccountCommandResult (..)
       , UiNewAddressCommandResult (..)
       , UiLangFace (..)
       , UiHistoryFace (..)
       , UiFace (..)

       , UiWalletTreeItem (..)
       , UiWalletTree
       , UiWalletTreeSelection(..)
       , TreePath
       , UiWalletInfo(..)
       , UiAccountInfo(..)
       , UiAddressInfo(..)
       , UiSelectionInfo(..)
       ) where

import Universum

import Data.Loc.Span (Span)
import Data.Tree (Tree)
import Text.PrettyPrint.ANSI.Leijen (Doc)

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

data UiCardanoStatusUpdate = UiCardanoStatusUpdate
  { syncProgress :: Maybe Text
  , blockchainLocal :: Text
  , blockchainNetwork :: Text
  }

-- Update current displayed slot, chain difficulty, etc
data UiCardanoEvent
  = UiCardanoLogEvent Text
  | UiCardanoStatusUpdateEvent UiCardanoStatusUpdate

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
  | UiCardanoEvent UiCardanoEvent
  | UiWalletEvent UiWalletEvent

-- | Commands issued by the UI widgets
data UiCommand
  = UiSelect [Word]
  | UiSend Text Text  -- ^ Address, amount
  | UiNewWallet Text  -- ^ Name
  | UiNewAccount Text  -- ^ Name
  | UiNewAddress
  | UiKill Natural

-- | Results of commands issued by the UI widgets
data UiCommandResult
  = UiSendCommandResult UiSendCommandResult
  | UiNewWalletCommandResult UiNewWalletCommandResult
  | UiNewAccountCommandResult UiNewAccountCommandResult
  | UiNewAddressCommandResult UiNewAddressCommandResult

data UiSendCommandResult
  = UiSendCommandSuccess Text
  | UiSendCommandFailure Text

data UiNewWalletCommandResult
  = UiNewWalletCommandSuccess [Text]
  | UiNewWalletCommandFailure Text

data UiNewAccountCommandResult
  = UiNewAccountCommandSuccess
  | UiNewAccountCommandFailure Text

data UiNewAddressCommandResult
  = UiNewAddressCommandSuccess
  | UiNewAddressCommandFailure Text

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

data UiWalletTreeSelection =
  UiWalletTreeSelection
    { wtsWalletIdx :: Word
    , wtsPath :: TreePath
    }

-- Display info for entities on all HD-wallet tree levels
data UiWalletInfo = UiWalletInfo
  { uwiLabel :: !(Maybe Text)
  , uwiWalletIdx :: !Word
  , uwiBalance :: !Text
  , uwiAccounts :: ![UiAccountInfo]
  }

data UiAccountInfo = UiAccountInfo
  { uaciLabel :: !(Maybe Text)
  , uaciWalletIdx :: !Word
  , uaciPath :: !TreePath
  , uaciBalance :: !Text
  , uaciAddresses :: ![UiAddressInfo]
  }

data UiAddressInfo = UiAddressInfo
  { uadiWalletIdx :: !Word
  , uadiPath :: !TreePath
  , uadiAddress :: !Text
  , uadiBalance :: !Text
  }

-- | Info for currently selected tree item
data UiSelectionInfo
  = UiSelectionWallet !UiWalletInfo
  | UiSelectionAccount !UiAccountInfo
