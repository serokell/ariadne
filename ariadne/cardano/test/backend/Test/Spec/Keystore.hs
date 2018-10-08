{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Spec.Keystore (
    spec
  ) where

import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (IOError)

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary)
import Test.QuickCheck.Monadic (forAllM, monadicIO, pick, run)

import Pos.Crypto (EncryptedSecretKey, hash, safeKeyGen)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (eskToHdRootId)
import Ariadne.Wallet.Cardano.Kernel.Keystore (DeletePolicy(..), Keystore)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

import Util.Buildable (ShowThroughBuild(..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Creates and operate on a keystore. The 'Keystore' is created in a temporary
-- directory and garbage-collected from the Operating System.
withKeystore :: (Keystore -> IO a) -> IO a
withKeystore = Keystore.bracketTestKeystore

genKeypair :: Gen ( ShowThroughBuild WalletId
                  , ShowThroughBuild EncryptedSecretKey
                  )
genKeypair = do
    (_, esk) <- arbitrary >>= safeKeyGen
    return $ bimap STB STB (WalletIdHdSeq . eskToHdRootId  $ esk, esk)

nukeKeystore :: FilePath -> IO ()
nukeKeystore fp =
    removeFile fp `catch` (\(_ :: IOError) -> pass)

getKeystorePath :: MonadIO m => m FilePath
getKeystorePath = do
    tempDir <- liftIO getTemporaryDirectory
    pure $ tempDir </> "test_keystore.key"

spec :: Spec
spec =
    describe "Keystore to store UserSecret(s)" $ do
        it "creating a brand new one works" $ do
            keystorePath <- getKeystorePath
            nukeKeystore keystorePath
            Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \_ks ->
                pass
            doesFileExist keystorePath `shouldReturn` True

        it "destroying a keystore (completely) works" $ do
            keystorePath <- getKeystorePath
            nukeKeystore keystorePath
            Keystore.bracketKeystore RemoveKeystoreIfEmpty keystorePath $ \_ks ->
                pass
            doesFileExist keystorePath `shouldReturn` False

        prop "lookup of keys works" $ monadicIO $ do
            forAllM genKeypair $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    mbKey <- Keystore.lookup wid ks
                    (fmap hash mbKey) `shouldBe` (Just (hash esk))

        prop "Inserts are persisted after releasing the keystore" $ monadicIO $ do
            keystorePath <- getKeystorePath
            (STB wid, STB esk) <- pick genKeypair
            run $ do
                nukeKeystore keystorePath
                Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \keystore1 ->
                    Keystore.insert wid esk keystore1
                Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \keystore2 -> do
                    mbKey <- Keystore.lookup wid keystore2
                    (fmap hash mbKey) `shouldBe` (Just (hash esk))

        prop "deletion of keys works" $ monadicIO $ do
            forAllM genKeypair $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    Keystore.delete wid ks
                    mbKey <- Keystore.lookup wid ks
                    (fmap hash mbKey) `shouldBe` Nothing

        prop "Deletion of keys are persisted after releasing the keystore" $ monadicIO $ do
            keystorePath <- getKeystorePath
            (STB wid, STB esk) <- pick genKeypair
            run $ do
                nukeKeystore keystorePath
                Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \keystore1 -> do
                    Keystore.insert wid esk keystore1
                    Keystore.delete wid keystore1
                Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \keystore2 -> do
                    mbKey <- Keystore.lookup wid keystore2
                    (fmap hash mbKey) `shouldBe` Nothing
