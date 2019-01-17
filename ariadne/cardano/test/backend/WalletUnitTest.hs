-- | Wallet unit tests
module Main (main) where

import Formatting (build, sformat)
import Test.Hspec (Spec, describe, hspec)

import UTxO.Bootstrap (bootstrapTransaction)
import UTxO.Context (Addr, TransCtxt)
import UTxO.DSL (GivenHash, Transaction)
import UTxO.Translate (runTranslateNoErrors, withConfig)

import qualified Test.Spec.Accounts
import qualified Test.Spec.AcidState
import qualified Test.Spec.CreateAddress
import qualified Test.Spec.Fee
import qualified Test.Spec.Kernel
import qualified Test.Spec.Keystore
import qualified Test.Spec.Models
import qualified Test.Spec.Translation
import qualified Test.Spec.Wallets
import qualified Test.Spec.WalletWorker
import TxMetaStorageSpecs (txMetaStorageSpecs)

{-------------------------------------------------------------------------------
  Main test driver
-------------------------------------------------------------------------------}

main :: IO ()
main = runTranslateNoErrors $ withConfig $ return $ hspec tests

-- | Debugging: show the translation context
_showContext :: IO ()
_showContext = do
    putStrLn $ runTranslateNoErrors $ withConfig $
      sformat build <$> ask
    putStrLn $ runTranslateNoErrors $
      let bootstrapTransaction' :: TransCtxt -> Transaction GivenHash Addr
          bootstrapTransaction' = bootstrapTransaction
      in sformat build . bootstrapTransaction' <$> ask

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "Wallet unit tests" $ do
    Test.Spec.Translation.spec
    Test.Spec.Models.spec
    Test.Spec.Kernel.spec
    Test.Spec.WalletWorker.spec
    txMetaStorageSpecs
    Test.Spec.Keystore.spec
    Test.Spec.CreateAddress.spec
    Test.Spec.Wallets.spec
    Test.Spec.Accounts.spec
    Test.Spec.AcidState.spec
    Test.Spec.Fee.spec
