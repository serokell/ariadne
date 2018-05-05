module Ariadne.Config.Wallet
  ( defaultWalletConfig
  , walletFieldModifier
  , WalletConfig (..)) where

import Universum

import Ariadne.Config.DhallUtil (interpretByte, parseField)
import qualified Data.Map as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import Serokell.Data.Memory.Units (Byte)

defaultWalletConfig :: WalletConfig
defaultWalletConfig = WalletConfig 16

parseFieldWallet :: Map D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldWallet = parseField walletFieldModifier

walletFieldModifier :: D.Text -> D.Text
walletFieldModifier = f
  where
    f "wcEntropySize" = "entropy-size"
    f x = x

data WalletConfig = WalletConfig
  { wcEntropySize :: Byte
  } deriving (Eq, Show)

instance D.Interpret WalletConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        wcEntropySize <- parseFieldWallet fields "wcEntropySize" interpretByte
        return WalletConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [(walletFieldModifier "wcEntropySize", D.expected interpretByte)])
