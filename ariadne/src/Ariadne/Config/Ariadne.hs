module Ariadne.Config.Ariadne
  ( defaultAriadneConfig
  , AriadneConfig (..)) where

import Universum

import Ariadne.Config.Cardano
import Ariadne.Config.DhallUtil (parseField)
import Ariadne.Config.Wallet
import qualified Data.Map as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)

-- default Ariadne config with Cardano mainnet config
defaultAriadneConfig :: AriadneConfig
defaultAriadneConfig = AriadneConfig defaultCardanoConfig defaultWalletConfig

parseFieldAriadne :: Map D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldAriadne = parseField ariadneFieldModifier

ariadneFieldModifier :: D.Text -> D.Text
ariadneFieldModifier = f
  where
    f "acCardano" = "cardano"
    f "acWallet" = "wallet"
    f x = x

-- dhall representation of AriadneConfig is a record
-- so it wroth to make a Haskell representation a record too.
-- It will be a Map Text Config if dhall representation
-- is changed to a List
data AriadneConfig = AriadneConfig
  { acCardano :: CardanoConfig
  , acWallet :: WalletConfig
  } deriving (Eq, Show)

instance D.Interpret AriadneConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        acCardano <- parseFieldAriadne fields "acCardano" (D.auto @CardanoConfig)
        acWallet <- parseFieldAriadne fields "acWallet" (D.auto @WalletConfig)
        return AriadneConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [ (ariadneFieldModifier "acCardano", D.expected (D.auto @CardanoConfig))
                , (ariadneFieldModifier "acWallet", D.expected (D.auto @WalletConfig))])
