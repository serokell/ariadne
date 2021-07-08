module Ariadne.Config.UI where

import Control.Lens (makeLensesWith)
import qualified Dhall as D
import Dhall.Core (Expr(..))
import qualified Data.HashMap.Strict.InsOrd as Map
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)

import Ariadne.Config.DhallUtil

import Ariadne.Util (postfixLFields)

defaultUIConfig :: UIConfig
defaultUIConfig = UIConfig
  { ucNoConfirm = False
  }

parseFieldUI :: Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldUI = parseField uiFieldModifier

uiFieldModifier :: D.Text -> D.Text
uiFieldModifier = f
  where
    f "ucNoConfirm" = "no-confirm"
    f x = x

data UIConfig = UIConfig
  { ucNoConfirm :: !Bool
  } deriving (Eq, Show)

makeLensesWith postfixLFields ''UIConfig

instance D.Interpret UIConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        ucNoConfirm <- parseFieldUI fields "ucNoConfirm" D.bool
        return UIConfig {..}
      extractOut _ = Nothing

      expectedOut = Record $ Map.fromList
        [ (uiFieldModifier "ucNoConfirm", D.expected D.bool)
        ]

instance D.Inject UIConfig where
  injectWith _ = injectUIConfig

injectUIConfig :: D.InputType UIConfig
injectUIConfig = D.InputType {..}
  where
    embed UIConfig {..} = RecordLit
      (Map.fromList
        [ ( uiFieldModifier "ucNoConfirm", D.embed injectBool ucNoConfirm)
        ]
      )

    declared = Record
      (Map.fromList
        [ (uiFieldModifier "ucNoConfirm", D.declared injectBool)
        ]
      )

    injectBool = D.inject :: D.InputType Bool
