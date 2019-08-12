module DB (insert, getAll, query, download) where

import Prelude

import Control.Parallel (parTraverse_)
import Data.Maybe (Maybe(..))
import Database.IndexedDB.Core (Database, Transaction, TransactionMode(..), class IDBObjectStore, class IDBIndex) as IDBCore
import Database.IndexedDB.IDBDatabase as IDBDatabase
import Database.IndexedDB.IDBFactory as IDBFactory
import Database.IndexedDB.IDBObjectStore as IDBObjectStore
import Database.IndexedDB.IDBTransaction as IDBTransaction
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Expense (Expense, ExpenseDB, fromModelToDB, fromDBToModel)

dbName :: String
dbName = "expensesDB"

storeName :: String
storeName = "expenses"

-- IndexedDB methods
onUpgradeNeeded ::
  IDBCore.Database -> IDBCore.Transaction -> { oldVersion :: Int } -> Effect Unit
onUpgradeNeeded db _ _ = launchAff_ do
  store <- IDBDatabase.createObjectStore
             db storeName {keyPath: [], autoIncrement: true}
  _     <- IDBObjectStore.createIndex
             store "type" ["type"] IDBObjectStore.defaultParameters
  _     <- IDBObjectStore.createIndex
             store "date" ["date"] IDBObjectStore.defaultParameters
  pure unit

openDB :: Aff IDBCore.Database
openDB = IDBFactory.open
           dbName Nothing { onBlocked: Nothing
                          , onUpgradeNeeded: Just onUpgradeNeeded
                          }

insertOne :: forall s. IDBCore.IDBObjectStore s => s -> Expense -> Aff Unit
insertOne s e = do
  let eDB = fromModelToDB e
  _ <- IDBObjectStore.add s eDB (Nothing :: Maybe Int)
  pure unit

insert :: Array Expense -> Aff Unit
insert exps = do
  db <- openDB
  tx <- IDBDatabase.transaction db [storeName] IDBCore.ReadWrite
  store <- IDBTransaction.objectStore tx storeName
  parTraverse_ (\exp -> insertOne store exp) exps


allFromIndex ::
  forall index. IDBCore.IDBIndex index => index -> Aff (Array ExpenseDB)
allFromIndex = fromEffectFnAff <<< _getAll

getAll :: Aff (Array Expense)
getAll = do
  db <- openDB
  tx <- IDBDatabase.transaction db [storeName] IDBCore.ReadOnly
  store <- IDBTransaction.objectStore tx storeName
  index <- IDBObjectStore.index store "date"
  values <- allFromIndex store
  pure $ map fromDBToModel values

-- Imported methods from JS implementation
foreign import _getAll ::
  forall index. IDBCore.IDBIndex index => index -> EffectFnAff (Array ExpenseDB)

foreign import _download :: Effect Unit
download :: Effect Unit
download = _download

query :: Aff (Array Expense)
query = pure []
