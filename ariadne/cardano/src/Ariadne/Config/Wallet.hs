module Ariadne.Config.Wallet
  ( defaultWalletConfig
  , wcAcidDBPathL
  , wcEntropySizeL 
  , walletFieldModifier
  , WalletConfig (..)) where

import Universum

import Ariadne.Config.DhallUtil (interpretByte, interpretFilePath, parseField)
import Ariadne.Util (postfixLFields)
import Control.Lens (makeLensesWith)
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import Serokell.Data.Memory.Units (Byte)
import System.FilePath ((</>))

defaultWalletConfig :: FilePath -> WalletConfig
defaultWalletConfig dataDir = WalletConfig 16 $ dataDir </> ".wallet-db"

parseFieldWallet ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldWallet = parseField walletFieldModifier

walletFieldModifier :: D.Text -> D.Text
walletFieldModifier = f
  where
    f "wcEntropySize" = "entropy-size"
    f "wcAcidDBPath"  = "wallet-db-path"
    f x = x

data WalletConfig = WalletConfig
  { wcEntropySize :: Byte
  , wcAcidDBPath  :: FilePath 
  } deriving (Eq, Show)

makeLensesWith postfixLFields ''WalletConfig

instance D.Interpret WalletConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        wcEntropySize <- parseFieldWallet fields "wcEntropySize" interpretByte
        wcAcidDBPath  <- parseFieldWallet fields "wcAcidDBPath"  interpretFilePath
        return WalletConfig {..}
      extractOut _ = Nothing

      expectedOut =
        Record
            (Map.fromList
                [(walletFieldModifier "wcEntropySize", D.expected interpretByte)
                 ,(walletFieldModifier "wcAcidDBPath", D.expected interpretFilePath)])
