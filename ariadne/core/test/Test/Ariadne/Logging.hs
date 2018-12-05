-- | Logging tests.

module Test.Ariadne.Logging
       ( spec
       ) where

import Control.Monad.Component (runComponentM)
import Data.Text (isInfixOf)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Expectation, Spec, describe, it, shouldSatisfy)

import Ariadne.Logging (Logging, logDebug, logError, loggingComponent)

spec :: Spec
spec = describe "Logging" $ do
  it "Can write messages to 'ariadne.log' in given directory" loggingUnitTest

loggingUnitTest :: Expectation
loggingUnitTest =
    withSystemTempDirectory name $ \dir -> do
        runComponentM name (loggingComponent dir) doLogging
        doCheck dir
  where
    name :: IsString s => s
    name = "ariadne-core-logging-test"

    doLogging :: Logging -> IO ()
    doLogging logging = do
      logDebug logging "vanya"
      logError logging "vanя"

    doCheck :: FilePath -> Expectation
    doCheck dir = do
      line0:line1:_ <- lines <$> readFile (dir </> "ariadne.log")
      "vanya" `shouldSatisfy` (`isInfixOf` line0)
      "vanя" `shouldSatisfy` (`isInfixOf` line1)
