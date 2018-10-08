module Ariadne.UI.Vty.Knit
       ( UI
       , ComponentExecContext(..)
       ) where

import Ariadne.UI.Vty.Face
import Knit

data UI

data instance ComponentValue _ UI

deriving instance Eq (ComponentValue components UI)
deriving instance Ord (ComponentValue components UI)
deriving instance Show (ComponentValue components UI)

instance ComponentInflate components UI where
  componentInflate = \case{}

data instance ComponentLit UI

deriving instance Eq (ComponentLit UI)
deriving instance Ord (ComponentLit UI)
deriving instance Show (ComponentLit UI)

data instance ComponentToken UI

deriving instance Eq (ComponentToken UI)
deriving instance Ord (ComponentToken UI)
deriving instance Show (ComponentToken UI)

instance ComponentTokenizer components UI where
  componentTokenizer = []

instance ComponentDetokenizer UI where
  componentTokenRender = \case{}

instance ComponentTokenToLit components UI where
  componentTokenToLit _ = asum []

instance ComponentPrinter UI where
  componentPpLit = \case{}
  componentPpToken = \case{}

data instance ComponentCommandRepr components UI
  = CommandAction (UiFace -> IO (Value components))

instance ComponentLitToValue components UI where
  componentLitToValue = \case{}

data instance ComponentExecContext _ _ UI =
  UiExecCtx UiFace

instance MonadIO m => ComponentCommandExec m components UI where
  componentCommandExec (UiExecCtx uiFace) (CommandAction act) =
    liftIO $ act uiFace

instance (AllConstrained (Elem components) '[UI, Core]) => ComponentCommandProcs components UI where
  componentCommandProcs =
    [
      CommandProc
        { cpName = "help"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pass
        , cpRepr = \() -> CommandAction $ \UiFace{..} -> do
            putUiEvent $ UiCommandAction UiCommandHelp
            return $ toValue ValueUnit
        , cpHelp = "Show help screen"
        }
    , CommandProc
        { cpName = "logs"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pass
        , cpRepr = \() -> CommandAction $ \UiFace{..} -> do
            putUiEvent $ UiCommandAction UiCommandLogs
            return $ toValue ValueUnit
        , cpHelp = "Show logs screen"
        }
    ]
