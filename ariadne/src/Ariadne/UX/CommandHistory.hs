module Ariadne.UX.CommandHistory
    ( CommandHistory
    , openCommandHistory
    , addCommand
    , setPrefix
    , toNextCommand
    , toPrevCommand
    ) where

import Universum

import Database.SQLite.Simple
import qualified Data.Text as T

data Row = Row Int T.Text deriving (Show)
instance FromRow Row where
  fromRow = Row <$> field <*> field

data CommandHistory =
    CommandHistory
    { historyFile :: FilePath
    , counter :: IORef Int
    , currentPrefix :: IORef Text
    }

openCommandHistory :: FilePath -> IO CommandHistory
openCommandHistory historyFile = do
    currentPrefix <- newIORef ""
    currentCounter <- newIORef 0
    withConnection historyFile $
      \conn -> execute_ conn "CREATE TABLE IF NOT EXISTS history (id INTEGER PRIMARY KEY, command TEXT)"
    let ch = CommandHistory historyFile currentCounter currentPrefix
    resetCounter ch
    return ch

toPrevCommand :: CommandHistory -> IO (Maybe Text)
toPrevCommand ch =
    changeCommand ch 1

toNextCommand :: CommandHistory -> IO (Maybe Text)
toNextCommand ch =
    changeCommand ch (-1)

changeCommand :: CommandHistory -> Int -> IO (Maybe Text)
changeCommand ch counterChange = do
    newCounter <- (+counterChange) <$> readIORef (counter ch)
    resetCounter ch
    prefix <- readIORef (currentPrefix ch)
    let comp = if counterChange < 0 then "<=" else ">=" :: Text
    let order = if counterChange < 0 then "ASC" else "DESC" :: Text

    withConnection (historyFile ch) $ \conn -> do
      queryResult <- query conn "SELECT * FROM history WHERE command LIKE '?%' AND id ? ? ORDER BY id ? LIMIT 1" (prefix, comp, newCounter, order)
      case queryResult of
        [Row _ cmd] -> return $ Just cmd
        _ -> do
          resetCounter ch
          return Nothing

setPrefix :: CommandHistory -> Text -> IO ()
setPrefix ch (T.strip -> prefix) = do
    resetCounter ch
    writeIORef (currentPrefix ch) prefix

addCommand :: CommandHistory -> Text -> IO ()
addCommand ch (T.strip -> command) = do
    resetCounter ch
    writeIORef (currentPrefix ch) ""

    unless (null command) $
      withConnection (historyFile ch) $
        \conn -> execute conn "INSERT INTO history (command) VALUES (?)" (Only command)

resetCounter :: CommandHistory -> IO ()
resetCounter ch =
  withConnection (historyFile ch) $ \conn -> do
    queryResult <- query_ conn "SELECT MAX(id) FROM history"
    case queryResult of
      [[maxId]] -> writeIORef (counter ch) maxId
      _ -> return ()
