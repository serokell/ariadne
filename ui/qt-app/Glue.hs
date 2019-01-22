-- | Glue code between the frontend and the backends.

module Glue
       ( qtToUiCurrency
       , commandHandle
       , resultHandle
       , putBackendErrorToUI
       , module CommonGlue
       ) where

import NType (Elem)

import Ariadne.UI.Qt.Face
import Ariadne.Knit.Face
import Ariadne.UI.Common.Glue as CommonGlue

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

qtToUiCurrency :: Text -> Knit.Currency -> UiCurrency Qt
qtToUiCurrency amount unit = UiCurrency amount (unitToUI unit)
  where
    unitToUI Knit.ADA = ADA
    unitToUI Knit.Lovelace = Lovelace

commandHandle
  :: Elem components Knit.Core
  => FrontendCommand Qt -> Either Text (Knit.Expr Knit.NoExt Knit.CommandId components)
commandHandle = \case
  UiNewAddress UiNewAddressArgs{..} -> do
    Right $ exprProcCall
      (procCall Knit.newAddressCommandName $
        [ argKw "wallet" . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral $ unadaWalletIdx
        , argKw "account" . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral $ unadaAccountIdx
        ]
      )
  UiCalcTotal amounts -> do
    argAmounts <- forM amounts $ Right . (argKw "amount") . exprLit . Knit.toLit . Knit.LitNumber
    Right $ exprProcCall
      (procCall Knit.sumCoinsCommamdName argAmounts)
  where
    exprProcCall = Knit.ExprProcCall Knit.NoExt
    procCall = Knit.ProcCall Knit.NoExt
    argKw = Knit.ArgKw Knit.NoExt
    exprLit = Knit.ExprLit Knit.NoExt

resultHandle
  :: Elem components Knit.Cardano
  => KnitCommandResult components -> FrontendCommand Qt -> Maybe (UiCommandResult Qt)
resultHandle result = \case
  UiNewAddress UiNewAddressArgs{..} ->
      Just . UiFrontendCommandResult . UiNewAddressCommandResult
    . either UiNewAddressCommandFailure (UiNewAddressCommandSuccess unadaWalletIdx unadaAccountIdx) $
      fromResult result >>= fromValue >>= \case
        Knit.ValueAddress a -> Right $ pretty a
        _ -> Left "Unrecognized return value"
  UiCalcTotal {} ->
      Just . UiFrontendCommandResult . UiCalcTotalCommandResult
    . either UiCalcTotalCommandFailure UiCalcTotalCommandSuccess $
      fromResult result >>= fromValue >>= \case
        Knit.ValueCoin c ->
          let (amount, unit) = Knit.showCoin c in Right (amount, show unit)
        e -> Left $ "Unrecognized return value: " <> show e

putBackendErrorToUI :: UiFace Qt -> SomeException -> IO ()
putBackendErrorToUI UiFace{..} e = putUiEvent . UiFrontendEvent . UiBackendExceptionEvent $
  UiBackendException e
