-- | Glue code between the frontend and the backends.

module Ariadne.Glue where

import Prelude
import Data.Vinyl.TypeLevel
import Control.Exception (displayException)
import Data.Foldable
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Knit.Face
import Ariadne.UI.Face

import qualified Knit

knitFaceToUI
  :: forall components.
     ( Knit.KnownSpine components
     , AllConstrained (Knit.ComponentTokenizer components) components
     , AllConstrained (Knit.ComponentLitGrammar components) components
     , AllConstrained Knit.ComponentPrinter components
     )
  => KnitFace components
  -> UiLangFace
knitFaceToUI KnitFace{..} =
  UiLangFace
    { langPutCommand = putKnitCommand
    , langParse = Knit.parse
    , langPpExpr = Knit.ppExpr
    , langPpParseError = Knit.ppParseError
    , langParseErrSpans = Knit.parseErrorSpans
    }

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
    Just $ UiCommandEvent commandId $
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
    Just $ UiCommandEvent commandId (UiCommandOutput doc)

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
