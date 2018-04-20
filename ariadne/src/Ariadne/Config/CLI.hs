module Ariadne.Config.CLI (getConfig) where

import Universum

import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.DhallUtil (fromDhall)
import Control.Exception (displayException)
import Data.Text (pack)
import qualified Data.Text.Lazy as LT
import qualified Options.Applicative as O

getConfig :: IO AriadneConfig
getConfig = do
    mbConfigPath <- O.execParser opts
    case mbConfigPath of
        Nothing -> return defaultAriadneConfig
        (Just path) -> catch ((fromDhall . LT.pack) path) (\(e :: SomeException) -> do
            putText "Default configuration file will be used due to the following Error:"
            putText $ pack $ displayException e
            return defaultAriadneConfig)
  where
    opts = O.info (parseOptions <**> O.helper)
        ( O.fullDesc
        <> O.progDesc "Runs Ariadne CLI"
        <> O.header "Ariadne CLI" )

parseOptions :: O.Parser (Maybe FilePath)
parseOptions = O.optional $ O.strOption
          ( O.long "config"
         <> O.metavar "FILEPATH"
         <> O.help "Path to ariadne .dhall configuration file" )
