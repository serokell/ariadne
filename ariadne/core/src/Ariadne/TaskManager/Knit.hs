module Ariadne.TaskManager.Knit where

import Universum hiding (preview)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Formatting hiding (text)
import Text.Earley
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Ariadne.TaskManager.Face

import Knit

data TaskManager

data instance ComponentValue _ TaskManager
  = ValueTaskId TaskId
  deriving (Eq, Show, Ord)

makePrisms 'ValueTaskId

instance Elem components TaskManager => ComponentInflate components TaskManager where
  componentInflate (ValueTaskId cid) =
    ExprLit NoExt $ toLit (LitTaskId cid)

data instance ComponentCommandRepr components TaskManager
  = CommandAction (TaskManagerFace (Value components) -> IO (Value components))

newtype instance ComponentExecContext _ components TaskManager =
  TaskManagerExecCtx (TaskManagerFace (Value components))

data instance ComponentLit TaskManager
  = LitTaskId TaskId
  deriving (Eq, Show, Ord)

data instance ComponentToken TaskManager
  = TokenTaskId TaskId
  deriving (Eq, Ord, Show)

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
    TokenTaskId (TaskId cid) -> sformat ("<"%build%">") (toInteger cid)

instance Elem components TaskManager => ComponentLitGrammar components TaskManager where
  componentLitGrammar =
    rule $ asum
      [ toLit . LitTaskId <$> tok (_Token . uprism . _TokenTaskId)
      ]

instance ComponentPrinter TaskManager where
  componentPpLit = \case
    LitTaskId x -> text . componentTokenRender . TokenTaskId $ x
  componentPpToken = \case
    TokenTaskId _ -> "task id"

instance MonadIO m => ComponentCommandExec m components TaskManager where
  componentCommandExec (TaskManagerExecCtx face) = \case
    CommandAction action -> liftIO $ action face

instance
  ( AllConstrained (Elem components) '[TaskManager, Core]
  ) => ComponentCommandProcs components TaskManager
  where
    componentCommandProcs =
      [ CommandProc
          { cpName = killCommandName
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyTaskId "id"
          , cpRepr = \tid -> CommandAction $ \TaskManagerFace{..} -> do
              mTask <- lookupTask tid
              whenJust mTask cancel
              return . toValue $ ValueUnit
          , cpHelp = "kill a task with a specified id"
          }
      , CommandProc
          { cpName = waitCommandName
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyTaskId "id"
          , cpRepr = \tid -> CommandAction $ \TaskManagerFace{..} -> do
              mTask <- lookupTask tid
              case mTask of
                Nothing -> do
                  mCache <- lookupCache tid
                  maybe (throwIO NoTaskException) (either throwIO return) mCache
                Just task -> wait task
          , cpHelp = "wait for a specific task to finish"
          }
      , CommandProc
          { cpName = sleepCommandName
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyInt "time"
          , cpRepr = \ms -> CommandAction . const $ do
              threadDelay ms
              return . toValue $ ValueUnit
          , cpHelp = "delay the execution for specified amount of microseconds"
          }
      ]

killCommandName :: CommandId
killCommandName = "kill"

waitCommandName :: CommandId
waitCommandName = "wait"

sleepCommandName :: CommandId
sleepCommandName = "sleep"

tyTaskId :: Elem components TaskManager => TyProjection components TaskId
tyTaskId = TyProjection "TaskId" (preview _ValueTaskId <=< fromValue)
