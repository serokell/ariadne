-- | Transaction metadata conform the wallet specification
module Ariadne.Wallet.Cardano.Kernel.DB.TxMeta
       ( -- * Transaction metadata
         module Types

         -- * Handy re-export to not leak our current choice of storage backend.
       , openMetaDB
       ) where

import qualified Ariadne.Wallet.Cardano.Kernel.DB.Sqlite as ConcreteStorage
import Ariadne.Wallet.Cardano.Kernel.DB.TxMeta.Types as Types

-- Concrete instantiation of 'MetaDBHandle'

openMetaDB :: FilePath -> IO MetaDBHandle
openMetaDB fp = do
    conn <- ConcreteStorage.newConnection fp
    return MetaDBHandle {
          closeMetaDB   = ConcreteStorage.closeMetaDB conn
        , migrateMetaDB = ConcreteStorage.unsafeMigrateMetaDB conn
        , getTxMeta     = ConcreteStorage.getTxMeta conn
        , putTxMeta     = ConcreteStorage.putTxMeta conn
        , getTxMetas    = ConcreteStorage.getTxMetas conn
        }
