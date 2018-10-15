module CmdParse where

import Options.Applicative
import System.Directory (getHomeDirectory)

data TypeOfOutput = Wallets | Accounts | Addresses | AllData
  deriving Show

data CmdOptions = CmdOptions
    {  path       :: FilePath
    ,  position   :: Int
    ,  output     :: TypeOfOutput
    ,  fileToSave :: Maybe FilePath
    }


cmdArgToTypeOfOutput :: String -> Either String TypeOfOutput
cmdArgToTypeOfOutput output = case output of
    "wal"  -> Right Wallets
    "acc"  -> Right Accounts
    "addr" -> Right Addresses
    "all"  -> Right AllData
    _      -> Left "Can't parse type of output"

opts :: FilePath -> ParserInfo CmdOptions
opts path = info ((fileOptions path) <**> helper)
      ( fullDesc
     <> progDesc "Show acid state db contents"
     <> header "Show acid-state DB contents" )

parsedCMDOptions :: IO CmdOptions
parsedCMDOptions = do
  home <- getHomeDirectory
  execParser $ opts home

fileOptions :: FilePath -> Parser CmdOptions
fileOptions home = CmdOptions
  <$> strOption
        ( long "dbpath"
       <> metavar "DBPath"
       <> showDefault
       <> value (home ++ "/.local/share/ariadne/wallet-db/")
       <> help "Path to Acid DB.")
  <*> option auto
        ( long "position"
       <> short 'p'
       <> metavar "Steps"
       <> showDefault
       <> value 0
       <> help "Position in history of events (from the head).")
  <*> option (eitherReader cmdArgToTypeOfOutput)
       ( long "show"
      <> short 's'
      <> metavar "TypeOfOutput"
      <> showDefault
      <> value AllData
      <> help "Which part of the DB should be printed. Available options are:\
                \(\"wal\"(Wallets) | \"acc\"(Accounts) | \"addr\"(Addresses) | \"all\"(AllData)).")
  <*> (optional $ strOption
       ( long    "file"
      <> short   'f'
      <> metavar "FileToSave"
      <> help    "(Optional) Path to file to save the data to. If not provided, DB contents will be \
                 \printed to stdout." ))
