module Dscp.Wallet.CLI
       ( WalletCLIParams (..)
       , getWalletCLIParams
       ) where

import Options.Applicative (execParser, fullDesc, help, helper, info, long,
                            metavar, optional, progDesc, strOption)

import Dscp.CommonCLI
import Dscp.Web

data WalletCLIParams = WalletCLIParams
    { wpWitness :: BaseUrl
    , wpKnitCommand :: Maybe Text
    }

getWalletCLIParams :: IO WalletCLIParams
getWalletCLIParams = do
    let parser = do
            wpWitness <- clientAddressParser "witness" "Address of a witness node to communicate with"
            wpKnitCommand <- optional $ strOption $
                long "knit" <>
                metavar "COMMAND" <>
                help "Execute provided knit command and exit."
            return WalletCLIParams{..}
    execParser $
        info (helper <*> versionOption <*> parser) $
        fullDesc <> progDesc "Ariadne wallet for Disciplina"
