module Ariadne.TaskManager.Knit where

import Universum hiding (preview)

import Control.Lens
import Data.Vinyl.TypeLevel
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Earley
import Formatting hiding (text)

import IiExtras
import Ariadne.TaskManager.Face

import Knit

data TaskManager

data instance ComponentValue _ TaskManager
  = ValueTaskId TaskId
  deriving (Eq, Show, Ord)

makePrisms 'ValueTaskId

instance
  ( Elem components TaskManager
  , Elem components Core , AllConstrained (ComponentInflate components) components
  ) => ComponentInflate components TaskManager
  where
    componentInflate (ValueTaskId cid)
      = ExprLit $ toLit (LitTaskId cid)

data instance ComponentCommandRepr components TaskManager
  = CommandPure (Value components)
  | CommandAction (TaskManagerM (Value components) (Value components))

newtype instance ComponentExecContext components TaskManager =
  TaskManagerExecCtx (TaskManagerM (Value components) ~> IO)

data instance ComponentLit TaskManager
  = LitTaskId TaskId
  deriving (Eq, Show, Ord)

data instance ComponentToken TaskManager
  = TokenTaskId TaskId

makePrisms 'TokenTaskId

instance ComponentLitToValue components TaskManager where
  componentLitToValue = \case
    LitTaskId cid -> ValueTaskId cid

instance Elem components TaskManager => ComponentTokenizer components TaskManager where
  componentTokenizer = [ toToken . TokenTaskId <$> pTaskId ]
    where
      pTaskId :: Tokenizer TaskId
      pTaskId = do
        void $ P.char '<'
        cid <- P.decimal
        void $ P.char '>'
        return $ TaskId cid

instance ComponentDetokenizer TaskManager where
  componentTokenRender = \case
    TokenTaskId (TaskId cid) -> sformat ("<"%build%">") cid

instance Elem components TaskManager => ComponentLitGrammar components TaskManager where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitTaskId <$> tok (_Token . uprismElem . _TokenTaskId)
      ]

instance ComponentPrinter TaskManager where
  componentPpLit = \case
    LitTaskId x -> text . componentTokenRender . TokenTaskId $ x
  componentPpToken = \case
    TokenTaskId _ -> "task id"

instance (MonadIO m, Show (Value components)) => ComponentCommandExec m components TaskManager where
  componentCommandExec (TaskManagerExecCtx natTransform) = \case
    CommandPure val -> return val
    CommandAction action -> liftIO . natTransform $ action

instance
  ( AllConstrained (Elem components) '[TaskManager, Core]
  ) => ComponentCommandProcs components TaskManager
  where
    componentCommandProcs =
      [ CommandProc
          { cpName = "kill"
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyTaskId "id"
          , cpRepr = \tid -> CommandAction . TaskManagerM $ do
              TaskManagerContext {..} <- ask
              liftIO $ do
                mTask <- lookupTask tid
                whenJust mTask cancel
              return . toValue $ ValueUnit
          , cpHelp = "kill a task with a specified id"
          }
      , CommandProc
          { cpName = "wait"
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyTaskId "id"
          , cpRepr = \tid -> CommandAction . TaskManagerM $ do
              TaskManagerContext{..} <- ask
              ret <- liftIO $ do
                mTask <- lookupTask tid
                case mTask of
                  Nothing -> do
                    mCache <- lookupCache tid
                    maybe (throwIO NoTaskException) (either (\_ -> throwIO NoTaskException) return) mCache
                  Just task -> wait task
              return ret
          , cpHelp = "wait for a specific task to finish"
          }
      , CommandProc
          { cpName = "sleep"
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyInt "time"
          , cpRepr = \ms -> CommandAction . TaskManagerM $ do
              liftIO $ threadDelay ms
              return . toValue $ ValueUnit
          , cpHelp = "delay the execution for specified amount of microseconds"
          }
      ]

tyTaskId :: Elem components TaskManager => TyProjection components TaskId
tyTaskId = TyProjection "TaskId" (preview _ValueTaskId <=< fromValue)
