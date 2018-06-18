module Ariadne.Config.Update
  ( defaultUpdateConfig
  , updateFieldModifier
  , UpdateConfig (..)) where

import Universum

import Ariadne.Config.DhallUtil (interpretInt, parseField)
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)

defaultUpdateConfig :: UpdateConfig
defaultUpdateConfig = UpdateConfig "https://ariadnewallet.io" 3600

parseFieldUpdate ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldUpdate = parseField updateFieldModifier

updateFieldModifier :: D.Text -> D.Text
updateFieldModifier = f
  where
    f "ucVersionCheckUrl" = "version-check-url"
    f "ucCheckDelay" = "check-delay"
    f x = x

data UpdateConfig = UpdateConfig
  { ucVersionCheckUrl :: Text
  , ucCheckDelay :: Int
  } deriving (Eq, Show)

instance D.Interpret UpdateConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        ucVersionCheckUrl <- parseFieldUpdate fields "ucVersionCheckUrl" D.strictText
        ucCheckDelay <- parseFieldUpdate fields "ucCheckDelay" interpretInt
        return UpdateConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [(updateFieldModifier "ucVersionCheckUrl", D.expected D.strictText)
                ,(updateFieldModifier "ucCheckDelay", D.expected interpretInt)])
