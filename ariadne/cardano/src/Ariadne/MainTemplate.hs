-- | Template of the code that is supposed to be in @Main.hs@.

module Ariadne.MainTemplate
       ( MainSettings (..)
       , defaultMain
       ) where

import Control.Concurrent (forkIO, myThreadId, throwTo)
import Control.Concurrent.Async
  (Async(..), AsyncCancelled(..), ExceptionInLinkedThread(..), race_,
  waitCatch, withAsync)
import Control.Monad.Component (ComponentM, runComponentM)
import Control.Natural (($$))
import Data.Version (Version)
import Data.Vinyl.TypeLevel (type (++))
import NType (N(..), Rec)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Backend (CardanoBackend(..), createCardanoBackend)
import Ariadne.Cardano.Face (CardanoEvent, CardanoFace(..), decodeTextAddress)
import Ariadne.Cardano.Knit (adaToCoin)
import Ariadne.Config.Ariadne (AriadneConfig(..))
import Ariadne.Config.CLI (getConfig)
import Ariadne.Config.History (HistoryConfig(..))
import Ariadne.Config.Logging (LoggingConfig(..))
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Knit.Backend (Components, KnitFace, createKnitBackend)
import Ariadne.Logging (Logging, loggingComponent)
import Ariadne.TaskManager.Backend
import Ariadne.Update.Backend
import Ariadne.UX.CommandHistory
import Ariadne.UX.PasswordManager
import Ariadne.Wallet.Backend
import Ariadne.Wallet.Backend.KeyStorage (generateMnemonic)
import Ariadne.Wallet.Face (WalletEvent, WalletUIFace(..))

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

type NonUiComponents = '[Knit.Core, Knit.Cardano, Knit.Wallet, Knit.TaskManager]

type family AllComponents (uiComponents :: [*]) :: [*] where
   AllComponents uiComponents = NonUiComponents ++ uiComponents

-- | Everything needed for the 'main' function.
data MainSettings (uiComponents :: [*]) uiFace uiLangFace = MainSettings
    { msCommitHash :: !String
    -- ^ Commit hash of current revision. Should be passed to
    -- 'defaultMain' instead of being obtained there, because it
    -- involves TH and we want to run it only when we build
    -- executables, not when we build the library.
    , msCreateUI :: !(
        WalletUIFace ->
        Logging ->
        CommandHistory ->
        PutPassword ->
        ComponentM (uiFace, uiLangFace -> IO ())
      )
    , msPutWalletEventToUI :: !(uiFace -> WalletEvent -> IO ())
    , msPutCardanoEventToUI :: !(uiFace -> CardanoEvent -> IO ())
    , msPutUpdateEventToUI :: !(Maybe (uiFace -> Version -> Text -> IO ()))
    -- ^ Make UI process an update event if it's supported by UI. If
    -- it's not supported, this field can be 'Nothing'.
    , msPutPasswordEventToUI :: !(uiFace -> RequestPasswordToUi)
    -- ^ Make UI respond to a request from the password manager
    , msKnitFaceToUI :: !(
        uiFace ->
        KnitFace (AllComponents uiComponents) ->
        PutPassword ->
        uiLangFace
      )
    , msUiExecContext ::
        !(uiFace ->
        Rec (Knit.ComponentExecContext IO (AllComponents uiComponents)) uiComponents)
    , msPutBackendErrorToUI :: !(uiFace -> SomeException -> IO ())
    -- ^ Notify UI about exception, that occurs in backend.
    -- Not necessary for VTY application for now
    }

-- | Default implementation of the 'main' function.
defaultMain
    :: Components (AllComponents uiComponents)
    => MainSettings uiComponents uiFace uiLangFace -> IO ()
defaultMain settings = do
    ariadneConfig <- getConfig (msCommitHash settings)
    runComponentM "ariadne" (initializeEverything settings ariadneConfig) id

initializeEverything
    :: forall uiComponents uiFace uiLangFace.
       (Components (AllComponents uiComponents))
    => MainSettings uiComponents uiFace uiLangFace
    -> AriadneConfig
    -> ComponentM (IO ())
initializeEverything MainSettings {..}
                     AriadneConfig { acCardano = cardanoConfig
                                   , acWallet = walletConfig
                                   , acUpdate = updateConfig
                                   , acHistory = historyConfig
                                   , acLogging = loggingConfig
                                   } = do
  logging <- loggingComponent $ lcPath loggingConfig
  history <- openCommandHistory $ hcPath historyConfig
  let
    walletUIFace = WalletUIFace
      { walletGenerateMnemonic = generateMnemonic
      , walletDefaultEntropySize = wcEntropySize walletConfig
      , walletValidateAddress = leftToMaybe . decodeTextAddress
      , walletValidateCoin = isJust . adaToCoin
      , walletCoinPrecision = 6
      }
  PasswordManager {..} <- createPasswordManager

  (uiFace, mkUiAction) <- msCreateUI walletUIFace logging history putPassword
  CardanoBackend
    { cbFace = cardanoFace
    , cbMkAction = mkCardanoAction
    } <- createCardanoBackend cardanoConfig
  WalletPreface
    { wpBListener = bHandle
    , wpMakeWallet = mkWallet
    } <- createWalletBackend
        walletConfig
        cardanoFace
        (msPutWalletEventToUI uiFace)
        (getPasswordWithUI (msPutPasswordEventToUI uiFace))
        voidPassword
        logging
  let CardanoFace { cardanoRunCardanoMode = runCardanoMode
                  } = cardanoFace
  taskManagerFace <- createTaskManagerFace

  let
    mkWalletFace :: (Doc -> IO ()) -> WalletFace
    walletInitAction :: IO ()
    postInitAction :: IO ()
    (mkWalletFace, walletInitAction, postInitAction) = mkWallet

    knitExecContext ::
        (Doc -> IO ()) -> Knit.ExecContext IO (AllComponents uiComponents)
    knitExecContext putCommandOutput =
       Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) &:
       Knit.CardanoExecCtx (runCardanoMode $$) &:
       Knit.WalletExecCtx walletFace &:
       Knit.TaskManagerExecCtx taskManagerFace &:
       msUiExecContext uiFace
      where
        walletFace = mkWalletFace putCommandOutput
        a &: b = Step (a, b)
        infixr &:

    knitFace :: KnitFace (AllComponents uiComponents)
    knitFace = createKnitBackend knitExecContext taskManagerFace

    uiAction, cardanoAction :: IO ()
    uiAction = mkUiAction $ msKnitFaceToUI uiFace knitFace putPassword
    cardanoAction = mkCardanoAction bHandle (msPutCardanoEventToUI uiFace)

    raceWithUpdateCheckAction :: IO () -> IO ()
    raceWithUpdateCheckAction action =
      case msPutUpdateEventToUI of
        Nothing -> action
        Just putUpdateEventToUI ->
          let updateCheck =
                runUpdateCheck updateConfig (putUpdateEventToUI uiFace)
          in action `race_` updateCheck

    initAction :: IO ()
    initAction = walletInitAction

    serviceAction :: IO ()
    serviceAction =
      raceWithUpdateCheckAction cardanoAction

    mainAction :: IO ()
    mainAction = do
      initAction
      withAsync postInitAction $ \postInitActionThread -> do
        linkUI postInitActionThread $ msPutBackendErrorToUI uiFace
      -- Spawn backend actions in async thread, then run ui action in the main thread
      -- This is needed because some UI libraries (Qt) insist on livng in the main thread
        withAsync serviceAction $ \serviceThread -> do
          -- Make custom link to service thread for UI apps.
          -- It is going to call msPutBackendErrorToUI and rethrow exception, if something goes wrong
          linkUI serviceThread $ msPutBackendErrorToUI uiFace
          uiAction
  return mainAction

-- Similar to link from Control.Concurrent.Async, but calls finalizer on exception
linkUI :: Async a -> (SomeException -> IO ()) -> IO ()
linkUI thread finalizer = do
  me <- myThreadId
  void $ forkIO $ do
    r <- waitCatch thread
    case r of
      Left e | not $ isCancel e -> finalizer e >> throwTo me (ExceptionInLinkedThread thread e)
      _ -> pass
  where
    isCancel :: SomeException -> Bool
    isCancel e
      | Just AsyncCancelled <- fromException e = True
      | otherwise = False
