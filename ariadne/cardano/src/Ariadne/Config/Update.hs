module Ariadne.Config.Update
       ( defaultUpdateConfig
       , updateFieldModifier
       , UpdateConfig (..)
       ) where

import Ariadne.Config.DhallUtil (interpretInt, injectInt, parseField)
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)

defaultUpdateConfig :: UpdateConfig
defaultUpdateConfig = UpdateConfig "https://serokell.io/ariadne/version" "https://serokell.io/ariadne/" 3600

parseFieldUpdate ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldUpdate = parseField updateFieldModifier

updateFieldModifier :: D.Text -> D.Text
updateFieldModifier = f
  where
    f "ucVersionCheckUrl" = "version-check-url"
    f "ucUpdateUrl" = "update-url"
    f "ucCheckDelay" = "check-delay"
    f x = x

data UpdateConfig = UpdateConfig
  { ucVersionCheckUrl :: Text
  , ucUpdateUrl :: Text
  , ucCheckDelay :: Int
  } deriving (Eq, Show)

instance D.Interpret UpdateConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        ucVersionCheckUrl <- parseFieldUpdate fields "ucVersionCheckUrl" D.strictText
        ucUpdateUrl <- parseFieldUpdate fields "ucUpdateUrl" D.strictText
        ucCheckDelay <- parseFieldUpdate fields "ucCheckDelay" interpretInt
        return UpdateConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [(updateFieldModifier "ucVersionCheckUrl", D.expected D.strictText)
                ,(updateFieldModifier "ucUpdateUrl", D.expected D.strictText)
                ,(updateFieldModifier "ucCheckDelay", D.expected interpretInt)])

instance D.Inject UpdateConfig where
    injectWith _ = injectUpdateConfig

injectUpdateConfig :: D.InputType UpdateConfig
injectUpdateConfig = D.InputType {..}
  where
      embed UpdateConfig {..} = RecordLit
          (Map.fromList
              [ (updateFieldModifier "ucVersionCheckUrl",
                D.embed (D.inject @Text) ucVersionCheckUrl)
              , (updateFieldModifier "ucUpdateUrl",
                D.embed (D.inject @Text) ucUpdateUrl)
              , (updateFieldModifier "ucCheckDelay",
                D.embed injectInt ucCheckDelay)
              ])

      declared = Record
          (Map.fromList
              [ (updateFieldModifier "ucVersionCheckUrl",
                D.declared (D.inject @Text))
              , (updateFieldModifier "ucUpdateUrl",
                D.declared (D.inject @Text))
              , (updateFieldModifier "ucCheckDelay",
                D.declared injectInt)
              ])
