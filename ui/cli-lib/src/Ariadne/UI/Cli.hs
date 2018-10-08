module Ariadne.UI.Cli
       ( UiFace(..)
       , createAriadneUI
       ) where

import qualified Control.Concurrent.Event as CE
import Control.Monad.Component (ComponentM, buildComponent_)
import Data.Text (strip)

import qualified System.Console.Haskeline as Haskeline

import Ariadne.UI.Cli.Face
import Ariadne.UX.PasswordManager

type Repl = Haskeline.InputT IO

type UiAction = UiLangFace -> IO ()

data AppState = AppState
    { loopDecision :: !LoopDecision
    -- ^ tracks if it has to stop the loop or keep going
    , passwordMode :: !(Maybe (WalletId, CE.Event))
    -- ^ tracks if it's in password mode and the info it needs to communicate
    -- to the password manager when the password gets inserted
    }

data LoopDecision = Continue | Stop

createAriadneUI :: PutPassword -> ComponentM (UiFace, UiAction)
createAriadneUI putPass = buildComponent_ "UI-CLI" $ do
    uiEventVar :: MVar UiEvent <- newEmptyMVar
    let uiFace = mkUiFace uiEventVar
    return (uiFace, runUI uiEventVar putPass uiFace)

mkUiFace :: MVar UiEvent -> UiFace
mkUiFace uiEventVar = let putUiEvent = putMVar uiEventVar in UiFace{..}

runUI :: MVar UiEvent -> PutPassword -> UiFace -> UiLangFace -> IO ()
runUI uiEventVar putPass _uiFace langFace = Haskeline.runInputT settings $ do
    outputTextLn "Welcome to Ariadne wallet REPL"
    loop (prompt putPass uiEventVar langFace) initState
  where
    settings = Haskeline.Settings
        { Haskeline.complete       = Haskeline.noCompletion
        , Haskeline.historyFile    = Nothing
        , Haskeline.autoAddHistory = True
        }
    initState = AppState
        { loopDecision = Continue
        , passwordMode = Nothing
        }
    loop stepFunc appState = case loopDecision appState of
        Stop -> pass
        Continue -> loop stepFunc =<< stepFunc appState

prompt :: PutPassword -> MVar UiEvent -> UiLangFace -> AppState -> Repl AppState
prompt putPass uiEventVar UiLangFace{..} appState = case passwordMode appState of
    Just (walletRef, cEvent) -> getTextPassword >>= \case
        Nothing -> stop
        Just line -> do
            liftIO $ putPass walletRef line (Just cEvent)
            waitResult
    Nothing -> getInputTextLine >>= \case
        Nothing -> stop
        Just line
            | isQuitCommand line -> stop
            | isHelpCommand line -> do
                outputTextLn (unlines $ show <$> langGetHelp)
                continue
            | otherwise -> case langParse line of
                Left err -> do
                    outputTextLn $ show $ langPpParseError err
                    continue
                Right expr -> do
                    void . liftIO $ langPutCommand expr
                    waitResult
  where
    stop, continue, waitResult :: Repl AppState
    stop = return $ appState {loopDecision = Stop}

    continue = return appState

    waitResult = takeMVar uiEventVar >>= \case
        UiCommandEvent _cid result -> do
            outputTextLn . show $ case result of
                UiCommandSuccess doc -> doc
                UiCommandFailure doc -> doc
                UiCommandOutput doc -> doc
            return $ appState {passwordMode = Nothing}
        UiPasswordEvent (UiPasswordRequest walletId cEvent) ->
            return $ appState {passwordMode = Just (walletId, cEvent)}

isQuitCommand :: Text -> Bool
isQuitCommand t = strip t `elem` ["quit", "q", ":quit", ":q"]

isHelpCommand :: Text -> Bool
isHelpCommand t = strip t == "help"

outputTextLn :: Text -> Repl ()
outputTextLn = Haskeline.outputStrLn . toString

getInputTextLine :: Repl (Maybe Text)
getInputTextLine = getUsingText Haskeline.getInputLine "knit> "

getTextPassword :: Repl (Maybe Text)
getTextPassword = getUsingText (Haskeline.getPassword Nothing) "Password: "

getUsingText :: (String -> Repl (Maybe String)) -> Text -> Repl (Maybe Text)
getUsingText getFunc = (fmap.fmap) toText . getFunc . toString
