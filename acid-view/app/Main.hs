module Main where

import AcidView (loadState, printDB)
import CmdParse (CmdOptions(..), parsedCMDOptions)

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (Snapshot(..), defDB)
import Data.Acid (query)

main :: IO ()
main = do
  CmdOptions{..} <- parsedCMDOptions
  stateE <- loadState path defDB position
  case stateE of
    Left err -> putStrLn err
    Right (acidDB, eventTag) -> do
      db <- liftIO $ query acidDB Snapshot
      printDB output fileToSave db
      let appliedMethod = "\nLast applied method: " <> decodeUtf8 eventTag <> "\n"
      case fileToSave of
        Nothing   -> putText appliedMethod
        Just file -> appendFile file appliedMethod
