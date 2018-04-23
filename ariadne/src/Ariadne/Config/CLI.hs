module Ariadne.Config.CLI (getConfig) where

import Universum

import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.DhallUtil (fromDhall)
import qualified Data.Text.Lazy.IO as LTIO
import Formatting (sformat, string, (%))
import qualified Options.Applicative as O
import System.Directory (doesFileExist)

getConfig :: IO AriadneConfig
getConfig = do
    configPath <- O.execParser opts

    ifM (doesFileExist configPath)
        (do
            -- Dhall will throw well formatted colourful error message
            -- if something goes wrong
            configDhall <- LTIO.readFile configPath
            fromDhall configDhall)
        (do
            putText $ sformat ("File "%string%" not found. Default config will be used.") configPath
            return defaultAriadneConfig)

  where
    opts = O.info (parseOptions <**> O.helper)
        ( O.fullDesc
        <> O.progDesc "Runs Ariadne CLI"
        <> O.header "Ariadne CLI" )

parseOptions :: O.Parser FilePath
parseOptions = O.strOption
          ( O.long "config"
         <> O.metavar "FILEPATH"
         <> O.value "config/ariadne-config.dhall"
         <> O.help "Path to ariadne .dhall configuration file" )
