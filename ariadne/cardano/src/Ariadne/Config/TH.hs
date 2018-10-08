module Ariadne.Config.TH
       ( getCommitHash
       ) where

import Data.Char (isSpace)
import Formatting (sformat, string, (%))
import Language.Haskell.TH (ExpQ, litE, runIO, stringL)
import System.Process (readProcess)

getCommitHash :: ExpQ
getCommitHash = do
  hash <- runIO $
    -- `tryAny` here will both catch exit code and other exceptions like bad file descriptor
    -- which can occur if git executable was not found, for example
    tryAny (readProcess "git" ["rev-parse", "--verify", "HEAD"] "") >>= \case
      Right out -> return out
      Left err -> do
        putStrLn $ sformat ("Failed to get git revision: "%string) (show err)
        return "unknown"
  litE . stringL $ strip hash
  where
    -- This is not exactly fast, but it is okay for this function to not be fast
    -- given that it is executed once compile-time and the string length is 40
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
