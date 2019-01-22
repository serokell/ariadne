-- | Glue code between the frontend and the backends.

module Glue
       ( vtyToUiCurrency
       , putUpdateEventToUI
       , commandHandle
       , resultHandle
       , module CommonGlue
       ) where

import Data.Version (Version)
import NType (Elem)

import Ariadne.UI.Vty.Face
import Ariadne.Knit.Face
import Ariadne.UI.Common.Glue as CommonGlue

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

vtyToUiCurrency :: Text -> Knit.Currency -> UiCurrency Vty
vtyToUiCurrency amount unit = UiCurrency $ amount <> " " <> show unit

putUpdateEventToUI :: UiFace Vty -> Version -> Text -> IO ()
putUpdateEventToUI UiFace{..} ver updateURL = putUiEvent $ UiFrontendEvent $ UiNewVersionEvent $ UiNewVersion ver updateURL

commandHandle
  :: Elem components Knit.Core
  => FrontendCommand Vty -> Either Text (Knit.Expr Knit.NoExt Knit.CommandId components)
commandHandle = \case
  UiNewAddress UiNewAddressArgs{..} -> do
    Right $ exprProcCall
      (procCall Knit.newAddressCommandName $
        justOptNumber "wallet" unadaWalletIdx ++
        justOptNumber "account" unadaAccountIdx
      )
  where
    exprProcCall = Knit.ExprProcCall Knit.NoExt
    procCall = Knit.ProcCall Knit.NoExt

resultHandle :: KnitCommandResult components -> FrontendCommand Vty -> Maybe (UiCommandResult Vty)
resultHandle result = \case
  UiNewAddress{} ->
      Just . UiFrontendCommandResult . UiNewAddressCommandResult
    . either UiNewAddressCommandFailure (const UiNewAddressCommandSuccess) $
    fromResult result
