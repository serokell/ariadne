module Ariadne.UI.Vty.CommandHistory
    ( CommandHistory
    , openCommandHistory
    , setCurrCommand
    , startNewCommand
    , toNextCommand
    , toPrevCommand
    ) where

import Universum

import Control.Lens (ix)
import System.Directory (doesFileExist, renameFile)

import qualified Data.Text as T

data CommandHistory =
    CommandHistory
    { currCommand :: IORef Text
    , historyFile :: FilePath
    , counter :: IORef Int
    }

openCommandHistory :: FilePath -> IO CommandHistory
openCommandHistory path = do
    currCommandRef <- newIORef ""
    -- starts at -1 because we're "one away" from the last command entered
    counterRef <- newIORef (-1)
    fileExists <- doesFileExist path
    unless fileExists $ writeFile path ""
    return
        CommandHistory
        { currCommand = currCommandRef
        , historyFile = path
        , counter = counterRef
        }

toPrevCommand :: CommandHistory -> IO (Maybe Text)
toPrevCommand ch =
    changeCommand ch 1

toNextCommand :: CommandHistory -> IO (Maybe Text)
toNextCommand ch =
    changeCommand ch (-1)

changeCommand :: CommandHistory -> Int -> IO (Maybe Text)
changeCommand ch counterChange = do
    newCounter <- (+counterChange) <$> readIORef (counter ch)
    prefix <- T.strip <$> readIORef (currCommand ch)

    history <- lines <$> readFile (historyFile ch)
    let result = filter (prefix `T.isPrefixOf`) history ^? ix newCounter

    when (isJust result) $ writeIORef (counter ch) newCounter

    return result

setCurrCommand :: CommandHistory -> Text -> IO ()
setCurrCommand ch cmd = do
    writeIORef (counter ch) (-1)
    writeIORef (currCommand ch) cmd

startNewCommand :: CommandHistory -> IO ()
startNewCommand ch = do
    writeIORef (counter ch) (-1)
    command <- readIORef $ currCommand ch
    writeIORef (currCommand ch) ""

    -- write command to first line of history file
    unless (command == "\n") $ do
      file <- readFile $ historyFile ch
      let temp = historyFile ch ++ ".tmp"
      writeFile temp $ command <> toText file
      renameFile temp $ historyFile ch
