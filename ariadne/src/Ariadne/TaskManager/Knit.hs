module Ariadne.TaskManager.Knit where

import Universum hiding (preview)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Data.Vinyl.TypeLevel
import Formatting hiding (text)
import Text.Earley
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

import Ariadne.TaskManager.Face
import IiExtras

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
  | CommandAction (TaskManagerFace (Value components) -> IO (Value components))

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
      [ toLit . LitTaskId <$> tok (_Token . uprismElem . _TokenTaskId)
      ]

instance ComponentPrinter TaskManager where
  componentPpLit = \case
    LitTaskId x -> text . componentTokenRender . TokenTaskId $ x
  componentPpToken = \case
    TokenTaskId _ -> "task id"

instance (MonadIO m, Show (Value components)) => ComponentCommandExec m components TaskManager where
  componentCommandExec (TaskManagerExecCtx face) = \case
    CommandPure val -> return val
    CommandAction action -> liftIO $ action face

instance
  ( AllConstrained (Elem components) '[TaskManager, Core]
  ) => ComponentCommandProcs components TaskManager
  where
    componentCommandProcs =
      [ CommandProc
          { cpName = "kill"
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyTaskId "id"
          , cpRepr = \tid -> CommandAction $ \TaskManagerFace{..} -> do
              mTask <- lookupTask tid
              whenJust mTask cancel
              return . toValue $ ValueUnit
          , cpHelp = "kill a task with a specified id"
          }
      , CommandProc
          { cpName = "wait"
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
          { cpName = "sleep"
          , cpArgumentPrepare = identity
          , cpArgumentConsumer = getArg tyInt "time"
          , cpRepr = \ms -> CommandAction . const $ do
              threadDelay ms
              return . toValue $ ValueUnit
          , cpHelp = "delay the execution for specified amount of microseconds"
          }
      ]

tyTaskId :: Elem components TaskManager => TyProjection components TaskId
tyTaskId = TyProjection "TaskId" (preview _ValueTaskId <=< fromValue)
