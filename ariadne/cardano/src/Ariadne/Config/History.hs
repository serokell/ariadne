module Ariadne.Config.History
  ( defaultHistoryConfig
  , historyFieldModifier
  , HistoryConfig (..)
  , hcPathL
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

defaultHistoryConfig :: FilePath -> HistoryConfig
defaultHistoryConfig dataDir =
    HistoryConfig {hcPath = dataDir </> "ariadne_history.db"}

parseFieldHistory ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldHistory = parseField historyFieldModifier

historyFieldModifier :: D.Text -> D.Text
historyFieldModifier = f
  where
    f "hcPath" = "path"
    f x = x

data HistoryConfig = HistoryConfig
  { hcPath :: FilePath
  } deriving (Eq, Show)

makeLensesWith postfixLFields ''HistoryConfig

instance D.Interpret HistoryConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        hcPath <- parseFieldHistory fields "hcPath" interpretFilePath
        return HistoryConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [(historyFieldModifier "hcPath", D.expected interpretFilePath)])

instance D.Inject HistoryConfig where
    injectWith _ = injectHistoryConfig

injectHistoryConfig :: D.InputType HistoryConfig
injectHistoryConfig = D.InputType {..}
  where
      embed HistoryConfig {..} = RecordLit
          (Map.fromList
              [ (historyFieldModifier "hcPath",
                D.embed injectFilePath hcPath)
              ])

      declared = Record
          (Map.fromList
              [ (historyFieldModifier "hcPath", D.declared injectFilePath)
              ])
