module Ariadne.UX.CommandHistory
    ( CommandHistory
    , openCommandHistory
    , addCommand
    , setPrefix
    , toNextCommand
    , toPrevCommand
    ) where

import Universum

import Control.Lens (ix)
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
    -- starts at -1 because we're "one away" from the last command entered
    counter <- newIORef (-1)
    currentPrefix <- newIORef ""

    withConnection historyFile $
      \conn -> execute_ conn "CREATE TABLE IF NOT EXISTS history (id INTEGER PRIMARY KEY, command TEXT)"

    return CommandHistory{..}

toPrevCommand :: CommandHistory -> IO (Maybe Text)
toPrevCommand ch =
    changeCommand ch 1

toNextCommand :: CommandHistory -> IO (Maybe Text)
toNextCommand ch =
    changeCommand ch (-1)

changeCommand :: CommandHistory -> Int -> IO (Maybe Text)
changeCommand ch counterChange = do
    newCounter <- (+counterChange) <$> readIORef (counter ch)
    prefix <- readIORef (currentPrefix ch)

    -- do as little logic in sql as possible
    withConnection (historyFile ch) $ \conn -> do
      history <- query_ conn "SELECT * FROM history ORDER BY id DESC" :: IO [Row]

      let commands = [ cmd | Row _ cmd <- history ]
      let result = filter (prefix `T.isPrefixOf`) commands ^? ix newCounter

      when (isJust result) $ writeIORef (counter ch) newCounter
      return result

setPrefix :: CommandHistory -> Text -> IO ()
setPrefix ch (T.strip -> prefix) = do
    writeIORef (counter ch) (-1)
    writeIORef (currentPrefix ch) prefix

addCommand :: CommandHistory -> Text -> IO ()
addCommand ch (T.strip -> command) = do
    writeIORef (counter ch) (-1)
    writeIORef (currentPrefix ch) ""

    unless (null command) $
      withConnection (historyFile ch) $
        \conn -> execute conn "INSERT INTO history (command) VALUES (?)" (Only command)
