module Ariadne.Config.History
  ( defaultHistoryConfig
  , historyFieldModifier
  , HistoryConfig (..)) where

import Universum

import Ariadne.Config.DhallUtil (parseField)
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)

defaultHistoryConfig :: HistoryConfig
defaultHistoryConfig = HistoryConfig "ariadne_history.db"

parseFieldHistory ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldHistory = parseField historyFieldModifier

historyFieldModifier :: D.Text -> D.Text
historyFieldModifier = f
  where
    f "hcPath" = "path"
    f x = x

data HistoryConfig = HistoryConfig
  { hcPath :: Text
  } deriving (Eq, Show)

instance D.Interpret HistoryConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        hcPath <- parseFieldHistory fields "hcPath" D.strictText
        return HistoryConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [(historyFieldModifier "hcPath", D.expected D.strictText)])
