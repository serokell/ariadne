module AcidView
       ( loadState
       , printDB
       ) where

import CmdParse

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet

import Data.Acid (AcidState(..), IsAcidic)
import Data.Acid.Local (getState)
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder, toLazyText)

import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Builder.Int as BI

-- | Loads Acid DB state from @pos@ events ago.
loadState :: (IsAcidic st) => FilePath -> st -> Int -> IO (Either String (AcidState st, BL.ByteString))
loadState path initState pos = getState path initState pos True


-- | Prints given part of Acid DB to terminal or file.
printDB :: TypeOfOutput -> Maybe FilePath -> DB -> IO ()
printDB whatToPrint fileM db = do
    let wallets     = db ^. dbHdWallets . hdWalletsRoots
    let accounts    = db ^. dbHdWallets . hdWalletsAccounts
    let addresses   = db ^. dbHdWallets . hdWalletsAddresses
    let infoToPrint = case whatToPrint of
                        Wallets   -> build wallets
                        Accounts  -> build accounts
                        Addresses -> build addresses
                        AllData   -> build db
    let additionalInfo = "\n\nTotal wallets: " <> (ixSize wallets) <> "\nTotal accounts: "
                            <> (ixSize accounts) <> "\nTotal addresses: " <> (ixSize addresses)
    case fileM of
      Nothing   -> putLText . toLazyText $ infoToPrint <> additionalInfo
      Just file -> writeFile file . toStrict . toLazyText $ infoToPrint <> additionalInfo
    where
      ixSize :: IxSet.IxSet a -> Builder
      ixSize = BI.decimal . IxSet.size
