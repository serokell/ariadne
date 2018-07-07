module Ariadne.UI.Vty.Knit where

import Universum hiding (preview)

import IiExtras
import Text.Earley

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

instance ComponentLitGrammar components UI where
  componentLitGrammar = rule empty

instance ComponentPrinter UI where
  componentPpLit = \case{}
  componentPpToken = \case{}

data instance ComponentCommandRepr components UI
  = CommandAction (UiFace -> IO UiSelectedItem -> IO (Value components))

instance ComponentLitToValue components UI where
  componentLitToValue = \case{}

data instance ComponentExecContext _ _ UI =
  UiExecCtx UiFace (IO UiSelectedItem)

instance MonadIO m => ComponentCommandExec m components UI where
  componentCommandExec (UiExecCtx uiFace uiGetSelected) (CommandAction act) =
    liftIO $ act uiFace uiGetSelected

instance (AllConstrained (Elem components) '[UI, Core]) => ComponentCommandProcs components UI where
  componentCommandProcs =
    [
      CommandProc
        { cpName = "help"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \UiFace{..} _ -> do
            putUiEvent $ UiCommandAction UiCommandHelp
            return $ toValue ValueUnit
        , cpHelp = "Show help screen"
        }
    , CommandProc
        { cpName = "logs"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \UiFace{..} _ -> do
            putUiEvent $ UiCommandAction UiCommandLogs
            return $ toValue ValueUnit
        , cpHelp = "Show logs screen"
        }
    ]
