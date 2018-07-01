module Ariadne.UI.Vty.Face
       ( UiCommandId (..)
       , UiCommandEvent (..)
       , UiWalletEvent (..)
       , UiCardanoStatusUpdate (..)
       , UiCardanoEvent (..)
       , UiCommandAction (..)
       , UiEvent (..)
       , UiCommand (..)
       , UiCommandResult (..)
       , UiBalanceCommandResult (..)
       , UiSendCommandResult (..)
       , UiNewWalletCommandResult (..)
       , UiRestoreWalletCommandResult (..)
       , UiSelectedItem (..)
       , UiLangFace (..)
       , UiHistoryFace (..)
       , UiFace (..)

       , UiTreeItem (..)
       , UiTree
       , UiTreeSelection(..)
       , UiWalletInfoType(..)
       , UiWalletInfo(..)
       , TreePath
       ) where

import Universum

import Data.Loc.Span (Span)
import Data.Tree (Tree)
import Data.Version (Version)
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
    { wuTrees :: [UiTree]
    , wuSelection :: Maybe UiTreeSelection
    , wuPaneInfoUpdate :: Maybe UiWalletInfo
    }

-- UI event triggered by REPL command
data UiCommandAction
  = UiCommandHelp
  | UiCommandLogs
  | UiCommandQuit

-- | Events as perceived by the UI. They will be generated from backend-specific
-- events in the 'Glue' module. They must be independent from the backends and
-- capture /what the UI can handle/, not what the backends can generate.
data UiEvent
  = UiCommandEvent UiCommandId UiCommandEvent
  | UiCommandResult UiCommandId UiCommandResult
  | UiCommandAction UiCommandAction
  | UiCardanoEvent UiCardanoEvent
  | UiWalletEvent UiWalletEvent
  | UiNewVersionEvent Version

-- | Commands issued by the UI widgets
data UiCommand
  = UiSelect [Word]
  | UiBalance
  | UiSend Text Text Text -- ^ Address, amount, passphrase
  | UiNewWallet Text Text  -- ^ Name, passphrase
  | UiRestoreWallet Text Text Text Bool  -- ^ Name, mnemonic, passphrase, full
  | UiKill Natural
  | UiCopySelection

-- | Results of commands issued by the UI widgets
data UiCommandResult
  = UiBalanceCommandResult UiBalanceCommandResult
  | UiSendCommandResult UiSendCommandResult
  | UiNewWalletCommandResult UiNewWalletCommandResult
  | UiRestoreWalletCommandResult UiRestoreWalletCommandResult

data UiBalanceCommandResult
  = UiBalanceCommandSuccess Text
  | UiBalanceCommandFailure Text

data UiSendCommandResult
  = UiSendCommandSuccess Text
  | UiSendCommandFailure Text

data UiNewWalletCommandResult
  = UiNewWalletCommandSuccess [Text]
  | UiNewWalletCommandFailure Text

data UiRestoreWalletCommandResult
  = UiRestoreWalletCommandSuccess
  | UiRestoreWalletCommandFailure Text

-- | Item which is currently selected by the backend.
data UiSelectedItem
    = UiNoSelection
    | UiSelectedWallet { uswWalletName :: !Text }
    | UiSelectedAccount { usaAccountName :: !Text }
    | UiSelectedAddress { usaAddress :: !Text }

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
data UiFace =
  UiFace
    {
      -- Update the user interface with an event. Does not block unless the
      -- queue of events is full (should not normally happen).
      putUiEvent :: UiEvent -> IO ()
    }

----------------------------------------------------------------------------
-- Wallet tree widget model
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

data UiTreeSelection =
  UiTreeSelection
    { wtsWalletIdx :: Word
    , wtsPath :: TreePath
    }

----------------------------------------------------------------------------
-- Wallet pane widget model
----------------------------------------------------------------------------

data UiWalletInfoType
  = UiWalletInfoWallet
  | UiWalletInfoAccount [Word32]

data UiWalletInfo
  = UiWalletInfo
    { wpiType :: !(Maybe UiWalletInfoType)
    , wpiLabel :: !(Maybe Text)
    , wpiWalletIdx :: !Word
    , wpiPath :: !TreePath
    , wpiAddresses :: ![(Word32, Text)]
    }
