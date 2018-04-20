module Ariadne.Config.Ariadne
  ( defaultAriadneConfig
  , AriadneConfig (..)) where

import Universum

import Ariadne.Config.Cardano
import qualified Data.Map as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)

-- default Ariadne config with Cardano mainnet config
defaultAriadneConfig :: AriadneConfig
defaultAriadneConfig = AriadneConfig defaultCardanoConfig

parseFieldAriadne :: Map D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldAriadne dhallRec name type_ = Map.lookup (ariadneFieldModifier name) dhallRec >>= D.extract type_

ariadneFieldModifier :: D.Text -> D.Text
ariadneFieldModifier = f
  where
    f "acCardano" = "cardano"
    f x = x

-- dhall representation of AriadneConfig is a record
-- so it wroth to make a Haskell representation a record too.
-- It will be a Map Text Config if dhall representation
-- is changed to a List
data AriadneConfig = AriadneConfig
  { acCardano :: CardanoConfig } deriving (Eq, Show)

instance D.Interpret AriadneConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        acCardano <- parseFieldAriadne fields "acCardano" (D.auto @CardanoConfig)
        return AriadneConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [(ariadneFieldModifier "acCardano", D.expected (D.auto @CardanoConfig))])
