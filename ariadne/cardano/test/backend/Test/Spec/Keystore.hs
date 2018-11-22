{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Spec.Keystore (
    spec
  ) where

import qualified Universum.Unsafe as Unsafe (fromJust)

import Data.Aeson (encode, decode)
import qualified Data.Map as Map (elems)
import System.Directory (doesFileExist, removeFile)
import System.IO.Error (IOError)

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Hspec.QuickCheck (prop)
import Test.Pos.Core.Arbitrary ()
import Test.QuickCheck (Gen, arbitrary)
import Test.QuickCheck.Monadic (forAllM, monadicIO, pick, run)

import Pos.Crypto (EncryptedSecretKey, hash, safeKeyGen)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (eskToHdRootId)
import Ariadne.Wallet.Cardano.Kernel.Keystore (DeletePolicy(..), Keystore)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))
import Ariadne.Wallet.Cardano.Kernel.Keystore.Persistence (getTempKeystorePath)
import Ariadne.Wallet.Cardano.Kernel.Keystore.Types
  (InternalStorage(..), isWallets)
import Ariadne.Wallet.Cardano.Kernel.Keystore.Util (esksToMap)

import Util.Buildable (ShowThroughBuild(..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Creates and operate on a keystore. The 'Keystore' is created in a temporary
-- directory and garbage-collected from the Operating System.
withKeystore :: (Keystore -> IO a) -> IO a
withKeystore = Keystore.bracketTestKeystore

arbitraryEsk :: Gen EncryptedSecretKey
arbitraryEsk = arbitrary >>= safeKeyGen >>= pure . snd

genKeypair :: Gen ( ShowThroughBuild WalletId
                  , ShowThroughBuild EncryptedSecretKey
                  )
genKeypair = do
    esk <- arbitraryEsk
    return $ bimap STB STB (WalletIdHdSeq . eskToHdRootId  $ esk, esk)

genInternalStorage :: Gen InternalStorage
genInternalStorage = do
    esk <- arbitraryEsk
    return $ InternalStorage (esksToMap [esk])

nukeKeystore :: FilePath -> IO ()
nukeKeystore fp =
    removeFile fp `catch` (\(_ :: IOError) -> pass)

spec :: Spec
spec =
    describe "Keystore to store UserSecret(s)" $ do
        it "creating a brand new one works" $ do
            keystorePath <- getTempKeystorePath
            nukeKeystore keystorePath
            Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \_ks ->
                pass
            doesFileExist keystorePath `shouldReturn` True

        it "destroying a keystore (completely) works" $ do
            keystorePath <- getTempKeystorePath
            nukeKeystore keystorePath
            Keystore.bracketKeystore RemoveKeystoreIfEmpty keystorePath $ \_ks ->
                pass
            doesFileExist keystorePath `shouldReturn` False

        prop "fromJSON is inverse of toJSON for InternalStorage" $ monadicIO $ do
            forAllM genInternalStorage $ \istorage -> run $ do
                let decodedIstorge    = Unsafe.fromJust $ decode (encode istorage)
                    decodedEskHashes  = hash <$> Map.elems (decodedIstorge ^. isWallets)
                    originalEskHashes = hash <$> Map.elems (istorage ^. isWallets)
                originalEskHashes `shouldBe` decodedEskHashes

        prop "lookup of keys works" $ monadicIO $ do
            forAllM genKeypair $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    mbKey <- Keystore.lookup wid ks
                    (fmap hash mbKey) `shouldBe` (Just (hash esk))

        prop "Inserts are persisted after releasing the keystore" $ monadicIO $ do
            keystorePath <- getTempKeystorePath
            (STB wid, STB esk) <- pick genKeypair
            run $ do
                nukeKeystore keystorePath
                Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \keystore1 -> do
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
            keystorePath <- getTempKeystorePath
            (STB wid, STB esk) <- pick genKeypair
            run $ do
                nukeKeystore keystorePath
                Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \keystore1 -> do
                    Keystore.insert wid esk keystore1
                    Keystore.delete wid keystore1
                Keystore.bracketKeystore KeepKeystoreIfEmpty keystorePath $ \keystore2 -> do
                    mbKey <- Keystore.lookup wid keystore2
                    (fmap hash mbKey) `shouldBe` Nothing
