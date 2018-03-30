-- | Glue code between the frontend and the backends.

module Ariadne.Glue where

import Control.Exception (displayException)
import Data.Foldable
import Data.Text (pack)
import Data.Unique
import IiExtras
import Numeric
import Prelude
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.CommandId
import Ariadne.Knit.Face
import Ariadne.UI.Vty.Face

import qualified Knit

knitFaceToUI
  :: forall components.
     ( KnownSpine components
     , AllConstrained (Knit.ComponentTokenizer components) components
     , AllConstrained (Knit.ComponentLitGrammar components) components
     , AllConstrained Knit.ComponentPrinter components
     )
  => KnitFace components
  -> UiLangFace
knitFaceToUI KnitFace{..} =
  UiLangFace
    { langPutCommand = fmap commandIdToUI . putKnitCommand
    , langParse = Knit.parse
    , langPpExpr = Knit.ppExpr
    , langPpParseError = Knit.ppParseError
    , langParseErrSpans = Knit.parseErrorSpans
    }

commandIdToUI :: CommandId -> UiCommandId
commandIdToUI (CommandId u) =
  UiCommandId
    { cmdIdEqObject = fromIntegral i
    , cmdIdRendered = pack $ '<' : showIntAtBase 36 base36Char i ">"
    }
  where
    i = hashUnique u
    base36Char = (alphabet!!)
    alphabet = "0123456789" ++ ['a'..'z']

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
knitEventToUI
  :: forall components.
     ( AllConstrained Knit.ComponentPrinter components
     , AllConstrained (Knit.ComponentInflate components) components
     )
  => KnitEvent components
  -> Maybe UiEvent
knitEventToUI = \case
  KnitCommandResultEvent commandId commandResult ->
    Just $ UiCommandEvent (commandIdToUI commandId) $
      case commandResult of
        KnitCommandSuccess v ->
          UiCommandSuccess $ Knit.ppValue v
        KnitCommandEvalError e ->
          UiCommandFailure $ Knit.ppEvalError e
        KnitCommandProcError e ->
          UiCommandFailure $ Knit.ppResolveErrors e
        KnitCommandException e ->
          UiCommandFailure $ PP.text (displayException e)
  KnitCommandOutputEvent commandId doc ->
    Just $ UiCommandEvent (commandIdToUI commandId) (UiCommandOutput doc)

putKnitEventToUI
  :: forall components.
     ( AllConstrained Knit.ComponentPrinter components
     , AllConstrained (Knit.ComponentInflate components) components
     )
  => UiFace
  -> KnitEvent components
  -> IO ()
putKnitEventToUI UiFace{..} ev =
  traverse_ putUiEvent (knitEventToUI ev)
