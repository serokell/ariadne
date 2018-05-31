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
import System.Directory (doesFileExist, renameFile)

import qualified Data.Text as T

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

    unlessM (doesFileExist historyFile) $ writeFile historyFile ""

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

    history <- lines <$> readFile (historyFile ch)
    let result = filter (prefix `T.isPrefixOf`) history ^? ix newCounter

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

    -- write command to first line of history file
    unless (null command) $ do
      file <- readFile $ historyFile ch
      let temp = historyFile ch ++ ".tmp"
      writeFile temp $ command <> "\n" <> toText file
      renameFile temp $ historyFile ch
