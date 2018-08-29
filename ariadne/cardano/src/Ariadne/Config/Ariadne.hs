module Ariadne.Config.Ariadne
       ( defaultAriadneConfig
       , AriadneConfig (..)
       , acCardanoL
       , acWalletL
       , acUpdateL
       , acHistoryL
       ) where

import Universum

import Control.Lens (makeLensesWith)
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)

import Ariadne.Config.Cardano
import Ariadne.Config.DhallUtil (parseField)
import Ariadne.Config.History
import Ariadne.Config.Update
import Ariadne.Config.Wallet
import Ariadne.Util

-- default Ariadne config with Cardano mainnet config
defaultAriadneConfig :: FilePath -> AriadneConfig
defaultAriadneConfig dataDir =
    AriadneConfig
        { acCardano = defaultCardanoConfig dataDir
        , acWallet = defaultWalletConfig
        , acUpdate = defaultUpdateConfig
        , acHistory = defaultHistoryConfig dataDir
        }

parseFieldAriadne ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldAriadne = parseField ariadneFieldModifier

ariadneFieldModifier :: D.Text -> D.Text
ariadneFieldModifier = f
  where
    f "acCardano" = "cardano"
    f "acWallet" = "wallet"
    f "acUpdate" = "update"
    f "acHistory" = "history"
    f x = x

-- dhall representation of AriadneConfig is a record
-- so it wroth to make a Haskell representation a record too.
-- It will be a Map Text Config if dhall representation
-- is changed to a List
data AriadneConfig = AriadneConfig
  { acCardano :: CardanoConfig
  , acWallet :: WalletConfig
  , acUpdate :: UpdateConfig
  , acHistory :: HistoryConfig
  } deriving (Eq, Show)

makeLensesWith postfixLFields ''AriadneConfig

instance D.Interpret AriadneConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        acCardano <- parseFieldAriadne fields "acCardano" (D.auto @CardanoConfig)
        acWallet <- parseFieldAriadne fields "acWallet" (D.auto @WalletConfig)
        acUpdate <- parseFieldAriadne fields "acUpdate" (D.auto @UpdateConfig)
        acHistory <- parseFieldAriadne fields "acHistory" (D.auto @HistoryConfig)
        return AriadneConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [ (ariadneFieldModifier "acCardano", D.expected (D.auto @CardanoConfig))
                , (ariadneFieldModifier "acWallet", D.expected (D.auto @WalletConfig))
                , (ariadneFieldModifier "acUpdate", D.expected (D.auto @UpdateConfig))
                , (ariadneFieldModifier "acHistory", D.expected (D.auto @HistoryConfig))
                ])
