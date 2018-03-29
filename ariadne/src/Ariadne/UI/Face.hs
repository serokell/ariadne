module Ariadne.UI.Face where

import Prelude
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Data.Text
import Data.Loc.Span (Span)

import Ariadne.CommandId

-- A REPL command has either finished or sent some information.
data UiCommandEvent
  = UiCommandSuccess Doc
  | UiCommandFailure Doc
  | UiCommandOutput Doc

-- Update current displayed slot, chain difficulty, etc
data UiCardanoEvent = UiCardanoEventMock

-- | Events as perceived by the UI. They will be generated from backend-specific
-- events in the 'Glue' module. They must be independent from the backends and
-- capture /what the UI can handle/, not what the backends can generate.
data UiEvent
  = UiCommandEvent CommandId UiCommandEvent
  | UiCardanoEvent UiCardanoEvent

-- The backend language (Knit by default) interface as perceived by the UI.
data UiLangFace =
  forall err expr. UiLangFace
  { langPutCommand :: expr -> IO CommandId
  , langParse :: Text -> Either err expr
  , langPpExpr :: expr -> Doc
  , langPpParseError :: err -> Doc
  , langParseErrSpans :: err -> [Span]
  }

-- API for the UI.
data UiFace =
  UiFace
    {
      -- Update the user interface with an event. Does not block unless the
      -- queue of events is full (should not normally happen).
      putUiEvent :: UiEvent -> IO ()
    }
