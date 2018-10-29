module Ariadne.Config.Wallet
       ( defaultWalletConfig
       , wcEntropySizeL
       , wcKeyfilePathL
       , wcAcidDBPathL
       , wcNumStoredArchivesL
       , wcDBCleanupPerionL
       , walletFieldModifier
       , WalletConfig (..)
       ) where

import Control.Lens (makeLensesWith)
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Dhall as D
import Dhall.Core (Expr(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import Serokell.Data.Memory.Units (Byte)
import System.FilePath ((</>))

import Ariadne.Config.DhallUtil
  (injectByte, injectFilePath, injectInt, interpretByte, interpretFilePath,
  interpretInt, parseField)
import Ariadne.Util (postfixLFields)

defaultWalletConfig :: FilePath -> WalletConfig
defaultWalletConfig dataDir = WalletConfig
    { wcEntropySize       = 16
    , wcKeyfilePath       = dataDir </> "secret-mainnet.key"
    , wcAcidDBPath        = dataDir </> "wallet-db"
    , wcNumStoredArchives = 10
    , wcDBCleanupPerion   = 600
    }

parseFieldWallet ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldWallet = parseField walletFieldModifier

walletFieldModifier :: D.Text -> D.Text
walletFieldModifier = f
  where
    f "wcEntropySize"       = "entropy-size"
    -- name is identical to the one used in Cardano for UserSecret
    f "wcKeyfilePath"       = "keyfile"
    f "wcAcidDBPath"        = "wallet-db-path"
    f "wcNumStoredArchives" = "stored-db-archives"
    f "wcDBCleanupPerion"   = "db-cleanup-period"
    f x = x

data WalletConfig = WalletConfig
  { wcEntropySize        :: !Byte
  , wcKeyfilePath        :: !FilePath
  , wcAcidDBPath         :: !FilePath
  , wcNumStoredArchives  :: !Int
  , wcDBCleanupPerion    :: !Int
  } deriving (Eq, Show)

makeLensesWith postfixLFields ''WalletConfig

instance D.Interpret WalletConfig where
  autoWith _ = D.Type extractOut expectedOut
    where
      extractOut (RecordLit fields) = do
        wcEntropySize        <- parseFieldWallet fields "wcEntropySize"       interpretByte
        wcKeyfilePath        <- parseFieldWallet fields "wcKeyfilePath"       interpretFilePath
        wcAcidDBPath         <- parseFieldWallet fields "wcAcidDBPath"        interpretFilePath
        wcNumStoredArchives  <- parseFieldWallet fields "wcNumStoredArchives" interpretInt
        wcDBCleanupPerion    <- parseFieldWallet fields "wcDBCleanupPerion"   interpretInt
        return WalletConfig {..}
      extractOut _ = Nothing

      expectedOut = Record $ Map.fromList
        [ (walletFieldModifier "wcEntropySize", D.expected interpretByte)
        , (walletFieldModifier "wcKeyfilePath", D.expected interpretFilePath)
        , (walletFieldModifier "wcAcidDBPath", D.expected interpretFilePath)
        , (walletFieldModifier "wcNumStoredArchives", D.expected interpretInt)
        , (walletFieldModifier "wcDBCleanupPerion", D.expected interpretInt)
        ]

instance D.Inject WalletConfig where
    injectWith _ = injectWalletConfig

injectWalletConfig :: D.InputType WalletConfig
injectWalletConfig = D.InputType {..}
  where
      embed WalletConfig {..} = RecordLit
          (Map.fromList
              [ (walletFieldModifier "wcEntropySize",
                D.embed injectByte wcEntropySize)
              , (walletFieldModifier "wcKeyfilePath",
                D.embed injectFilePath wcKeyfilePath)
              , (walletFieldModifier "wcAcidDBPath",
                D.embed injectFilePath wcAcidDBPath)
              , (walletFieldModifier "wcNumStoredArchives",
                D.embed injectInt wcNumStoredArchives)
              , (walletFieldModifier "wcDBCleanupPerion",
                D.embed injectInt wcDBCleanupPerion)
              ])

      declared = Record
        (Map.fromList
          [ (walletFieldModifier "wcEntropySize", D.declared injectByte)
          , (walletFieldModifier "wcKeyfilePath", D.declared injectFilePath)
          , (walletFieldModifier "wcAcidDBPath", D.declared injectFilePath)
          , (walletFieldModifier "wcNumStoredArchives", D.declared injectInt)
          , (walletFieldModifier "wcDBCleanupPerion", D.declared injectInt)
          ])
