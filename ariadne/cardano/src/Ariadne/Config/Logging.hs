-- | Configuration of logging in Ariadne.

module Ariadne.Config.Logging
       ( defaultLoggingConfig
       , loggingFieldModifier
       , LoggingConfig (..)
       , lcPathL
       ) where

import Control.Lens (makeLensesWith)
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import System.FilePath ((</>))

import Ariadne.Config.DhallUtil (injectFilePath, interpretFilePath, parseField)
import Ariadne.Util (postfixLFields)

defaultLoggingConfig :: FilePath -> LoggingConfig
defaultLoggingConfig dataDir =
    LoggingConfig {lcPath = dataDir </> "logs"}

parseFieldLogging ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldLogging = parseField loggingFieldModifier

loggingFieldModifier :: D.Text -> D.Text
loggingFieldModifier = f
  where
    f "lcPath" = "path"
    f x = x

data LoggingConfig = LoggingConfig
  { lcPath :: FilePath
  } deriving (Eq, Show)

makeLensesWith postfixLFields ''LoggingConfig

instance D.Interpret LoggingConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        lcPath <- parseFieldLogging fields "lcPath" interpretFilePath
        return LoggingConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [(loggingFieldModifier "lcPath", D.expected interpretFilePath)])

instance D.Inject LoggingConfig where
    injectWith _ = injectLoggingConfig

injectLoggingConfig :: D.InputType LoggingConfig
injectLoggingConfig = D.InputType {..}
  where
      embed LoggingConfig {..} = RecordLit
          (Map.fromList
              [ (loggingFieldModifier "lcPath",
                D.embed injectFilePath lcPath)
              ])

      declared = Record
          (Map.fromList
              [ (loggingFieldModifier "lcPath", D.declared injectFilePath)
              ])
