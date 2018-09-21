module Ariadne.UX.CommandHistory
       ( CommandHistory
       , openCommandHistory
       , addCommand
       , setPrefix
       , toNextCommand
       , toPrevCommand
       ) where

import Control.Monad.Component (ComponentM, buildComponent_)
import qualified Data.Text as T
import Database.SQLite.Simple

data Row = Row Int T.Text deriving (Show)
instance FromRow Row where
  fromRow = Row <$> field <*> field

data Direction = Previous | Next
  deriving Eq

data CommandHistory =
    CommandHistory
    { historyFile :: FilePath
    , currentCommandId :: IORef Int
    , currentPrefix :: IORef Text
    }

openCommandHistory :: FilePath -> ComponentM CommandHistory
openCommandHistory historyFile = buildComponent_ "Command history" $ do
    currentPrefix <- newIORef ""
    currentCommandId <- newIORef 0
    withConnection historyFile $
      \conn -> execute_ conn "CREATE TABLE IF NOT EXISTS history (id INTEGER PRIMARY KEY, command TEXT)"
    let ch = CommandHistory historyFile currentCommandId currentPrefix
    resetCurrentCommandId ch
    return ch

toPrevCommand :: CommandHistory -> IO (Maybe Text)
toPrevCommand ch =
    changeCommand ch Previous

toNextCommand :: CommandHistory -> IO (Maybe Text)
toNextCommand ch =
    changeCommand ch Next

changeCommand :: CommandHistory -> Direction -> IO (Maybe Text)
changeCommand ch direction = do
    prefix <- readIORef (currentPrefix ch)
    let prefix' = prefix <> "%"
    currId <- readIORef (currentCommandId ch)

    queryResult <- withConnection (historyFile ch) $ \conn ->
      case direction of
        Previous ->
          query conn "SELECT * FROM history WHERE command LIKE ? AND id < ? ORDER BY id DESC LIMIT 1" (prefix', currId)
        Next ->
          query conn "SELECT * FROM history WHERE command LIKE ? AND id > ? ORDER BY id ASC LIMIT 1" (prefix', currId)

    case queryResult of
      [Row rowId cmd] -> do
        writeIORef (currentCommandId ch) rowId
        return $ Just cmd
      _ ->
        case direction of
          Previous -> do
            result <- withConnection (historyFile ch) $ \conn ->
              query conn "SELECT * FROM history WHERE id=?" [currId]
            return $ Just $
              case result of
                [Row _ command] -> command
                _ -> prefix
          Next -> do
            resetCurrentCommandId ch
            return $ Just prefix

setPrefix :: CommandHistory -> Text -> IO ()
setPrefix ch (T.strip -> prefix) = do
    resetCurrentCommandId ch
    writeIORef (currentPrefix ch) prefix

addCommand :: CommandHistory -> Text -> IO ()
addCommand ch (T.strip -> command) = do
    writeIORef (currentPrefix ch) ""
    unless (null command) $
      withConnection (historyFile ch) $
        \conn -> execute conn "INSERT INTO history (command) VALUES (?)" (Only command)
    resetCurrentCommandId ch

resetCurrentCommandId :: CommandHistory -> IO ()
resetCurrentCommandId ch =
  withConnection (historyFile ch) $ \conn -> do
    queryResult <- query_ conn "SELECT COALESCE(MAX(id), 0) FROM history" :: IO [[Int]]
    case queryResult of
      [[maxId]] -> writeIORef (currentCommandId ch) (maxId + 1)
      _ -> pass
