module Ariadne.Wallet.Cardano.Kernel.Keystore.Types
       ( StorageDecodingError (..)
       , InternalStorage (..)
       , Versioned (..)
       , isWallets
       , getInternalStorageKeys
       ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
  (FromJSON, ToJSON, Value(..), object, parseJSON, toJSON, withObject, (.:),
  (.=))
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Default (Default, def)
import Data.Map (Map)
import Formatting (bprint, build)

import Pos.Binary.Class (decodeFull', serialize')
import Pos.Core (AddressHash)
import Pos.Crypto (EncryptedSecretKey(..), PublicKey)

import Ariadne.Wallet.Cardano.Kernel.Keystore.Util (esksToMap)

import qualified Data.Map as Map (elems, empty)
import qualified Data.Vector as V
import qualified Formatting.Buildable as Buildable
import qualified Serokell.Util.Base16 as S (decode, encode)

-- | InternalStorage data containing wallet keys that gets saved to file
data InternalStorage
  = InternalStorage
  { _isWallets  :: !(Map (AddressHash PublicKey) EncryptedSecretKey)
  } deriving (Show)

makeLenses ''InternalStorage

instance Default InternalStorage where
  def = InternalStorage { _isWallets = Map.empty }

newtype Version = Version Int
  deriving (Eq, Show, Generic)

-- | Attaches a version to serialised JSON
newtype Versioned a = Versioned a
    deriving (Eq, Show)

newtype StorageDecodingError = StorageDecodingError Text
    deriving (Show)

instance Exception StorageDecodingError

instance Buildable.Buildable StorageDecodingError where
    build (StorageDecodingError msg) =
        "Failed to decode stored json: " <> bprint build msg

instance ToJSON InternalStorage where
  toJSON istorage =
    object [ "keys" .= (fmap eskToJSON $ getInternalStorageKeys istorage) ]

instance FromJSON InternalStorage where
  parseJSON =
      withObject "InternalStorage" $ \v ->
          let pEskList = traverse eskFromJson . V.toList =<< v .: "keys"
          in InternalStorage . esksToMap <$> pEskList

-- | We have only one version, till the moment we integrate JSON versioning
theVersion :: Version
theVersion = Version 0

instance FromJSON Version

instance ToJSON Version

instance ToJSON a => ToJSON (Versioned a) where
    toJSON (Versioned content) = object
        [ "version" .= theVersion
        , "content" .= content
        ]

instance FromJSON a => FromJSON (Versioned a) where
    parseJSON = withObject "Versioned content" $ \o -> do
        ver <- o .: "version"
        unless (ver == theVersion) $
            fail $ "Only " <> show theVersion <> " version is supported"

        content <- o .: "content"
        return $ Versioned content

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

getInternalStorageKeys :: InternalStorage -> [EncryptedSecretKey]
getInternalStorageKeys istorage = Map.elems (istorage ^. isWallets)

eskToJSON :: EncryptedSecretKey -> Value
eskToJSON = String . S.encode . serialize'

eskFromJson :: Value -> Parser EncryptedSecretKey
eskFromJson (String encryptedText) =
    let textFail = fail . toString
        eitherDecode = either textFail pure . decodeFull'
    in either textFail eitherDecode $ S.decode encryptedText

eskFromJson val = typeMismatch "EncryptedSecretKey" val
