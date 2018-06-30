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

data CounterChange = Previous | Next
  deriving Eq

data CommandHistory =
    CommandHistory
    { historyFile :: FilePath
    , counter :: IORef Int
    , currentPrefix :: IORef Text
    , lastCommand :: IORef Text
    }

openCommandHistory :: FilePath -> IO CommandHistory
openCommandHistory historyFile = do
    currentPrefix <- newIORef ""
    lastCommand <- newIORef ""
    currentCounter <- newIORef 0
    withConnection historyFile $
      \conn -> execute_ conn "CREATE TABLE IF NOT EXISTS history (id INTEGER PRIMARY KEY, command TEXT)"
    let ch = CommandHistory historyFile currentCounter currentPrefix lastCommand
    resetCounter ch
    return ch

toPrevCommand :: CommandHistory -> IO (Maybe Text)
toPrevCommand ch =
    changeCommand ch Previous

toNextCommand :: CommandHistory -> IO (Maybe Text)
toNextCommand ch =
    changeCommand ch Next

direction :: CounterChange -> Int
direction Previous = (-1)
direction Next = 1

changeCommand :: CommandHistory -> CounterChange -> IO (Maybe Text)
changeCommand ch counterChange = do
    prefix <- readIORef (currentPrefix ch)
    let prefix' = prefix <> "%"

    count <- readIORef (counter ch)
    writeIORef (counter ch) (count + direction counterChange)

    queryResult <- withConnection (historyFile ch) $ \conn ->
      case counterChange of
        Previous ->
          query conn "SELECT * FROM history WHERE command LIKE ? AND id < ? ORDER BY id DESC LIMIT 1" (prefix', count)
        Next ->
          query conn "SELECT * FROM history WHERE command LIKE ? AND id > ? ORDER BY id ASC LIMIT 1" (prefix', count)

    case queryResult of
      [Row rowId cmd] -> do
        writeIORef (counter ch) rowId
        writeIORef (lastCommand ch) cmd
        return $ Just cmd
      _ ->
        case counterChange of
          Previous -> do
            lastCmd <- readIORef (lastCommand ch)
            return $ Just lastCmd
          Next -> do
            resetCounter ch
            return $ Just prefix

setPrefix :: CommandHistory -> Text -> IO ()
setPrefix ch (T.strip -> prefix) = do
    resetCounter ch
    writeIORef (currentPrefix ch) prefix

addCommand :: CommandHistory -> Text -> IO ()
addCommand ch (T.strip -> command) = do
    writeIORef (currentPrefix ch) ""
    unless (null command) $
      withConnection (historyFile ch) $
        \conn -> execute conn "INSERT INTO history (command) VALUES (?)" (Only command)
    resetCounter ch

resetCounter :: CommandHistory -> IO ()
resetCounter ch =
  withConnection (historyFile ch) $ \conn -> do
    queryResult <- query_ conn "SELECT COALESCE(MAX(id), 0) FROM history" :: IO [[Int]]
    case queryResult of
      [[maxId]] -> writeIORef (counter ch) (maxId + 1)
      _ -> return ()
