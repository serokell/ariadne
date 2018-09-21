module Ariadne.UI.Cli
       ( UiFace(..)
       , createAriadneUI
       ) where

import Control.Monad.Component (ComponentM, buildComponent_)
import Data.Text (strip)

import qualified System.Console.Haskeline as Haskeline

import Ariadne.UI.Cli.Face

type Repl = Haskeline.InputT IO

type UiAction = UiLangFace -> IO ()
type PrintAction = Text -> IO ()

createAriadneUI :: ComponentM (UiFace, UiAction)
createAriadneUI = buildComponent_ "UI-CLI" $ do
    printActionVar :: MVar PrintAction <- newEmptyMVar
    let uiFace = mkUiFace printActionVar
    return (uiFace, runUI printActionVar uiFace)

mkUiFace :: MVar PrintAction -> UiFace
mkUiFace printActionVar =UiFace{..}
  where
    putUiEvent = handleEvent printActionVar

runUI :: MVar PrintAction -> UiFace -> UiLangFace -> IO ()
runUI printActionVar _uiFace langFace = Haskeline.runInputT settings $ do
    printAction <- Haskeline.getExternalPrint
    putMVar printActionVar (liftIO . printAction . toString)
    greetings
    loop (prompt langFace)
  where
    greetings = outputTextLn "Welcome to Ariadne wallet REPL"
    settings = Haskeline.Settings
        { Haskeline.complete       = Haskeline.noCompletion
        , Haskeline.historyFile    = Nothing
        , Haskeline.autoAddHistory = True
        }

prompt :: UiLangFace -> Repl LoopDecision
prompt UiLangFace{..} = do
    getInputTextLine "knit> " >>= \case
        Nothing -> return Stop
        Just line
            | isQuitCommand line -> return Stop
            | isHelpCommand line -> do
                outputTextLn (unlines $ show <$> langGetHelp)
                return Continue
            | otherwise -> Continue <$ case langParse line of
                Left err -> do
                    outputTextLn $ show $ langPpParseError err
                Right expr -> do
                    void $ liftIO $ langPutCommand expr

handleEvent :: MVar PrintAction -> UiEvent -> IO ()
handleEvent printActionVar event = do
    c <- readMVar printActionVar
    case event of
        UiCommandEvent _cid result -> do
            case result of
                UiCommandSuccess doc -> liftIO $ c $ show doc
                UiCommandFailure doc -> liftIO $ c $ show doc
                UiCommandOutput doc -> liftIO $ c $ show doc

isQuitCommand :: Text -> Bool
isQuitCommand t = strip t `elem` ["quit", "q", ":quit", ":q"]

isHelpCommand :: Text -> Bool
isHelpCommand t = strip t == "help"

outputTextLn :: Text -> Repl ()
outputTextLn = Haskeline.outputStrLn . toString

getInputTextLine :: Text -> Repl (Maybe Text)
getInputTextLine = (fmap.fmap) toText . Haskeline.getInputLine . toString

data LoopDecision = Continue | Stop

loop :: Monad m => m LoopDecision -> m ()
loop step = fix $ \go ->
    step >>= \case
        Continue -> go
        Stop -> return ()
