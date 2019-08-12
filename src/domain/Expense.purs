module Expense where

import Prelude

import Data.Enum (class Enum, enumFromTo)
import Data.Maybe (Maybe(..))
import Data.String.Read (class Read, class Zero)

data ExpenseType
  = Casa
  | Carro
  | Comida
  | Saídas
  | Misc
derive instance eqExpenseType :: Eq ExpenseType
derive instance ordExpenseType :: Ord ExpenseType

instance showExpenseType :: Show ExpenseType where
  show Casa = "Casa"
  show Carro = "Carro"
  show Comida = "Comida"
  show Saídas = "Saídas"
  show Misc = "Misc"

instance readExpenseType :: Read ExpenseType where
  read = case _ of
    "Casa" -> Just Casa
    "Carro" -> Just Carro
    "Comida" -> Just Comida
    "Saídas" -> Just Saídas
    "Misc" -> Just Misc
    _ -> Nothing

instance zeroExpenseType :: Zero ExpenseType where
  zero = Misc

instance enumExpenseType :: Enum ExpenseType where
  succ Casa = Just Carro
  succ Carro = Just Comida
  succ Comida = Just Saídas
  succ Saídas = Just Misc
  succ Misc = Nothing

  pred Misc = Just Saídas
  pred Saídas = Just Comida
  pred Comida = Just Carro
  pred Carro = Just Casa
  pred Casa = Nothing

allExpenseTypes :: Array ExpenseType
allExpenseTypes = enumFromTo Casa Misc

type Expense =
  { value :: Number
  , date :: Maybe Number
  , expenseType :: ExpenseType
  , extra :: Boolean
  , description :: Maybe String
  }

defaultExpense :: Expense
defaultExpense =
  { value: 7.0
  , date: Nothing
  , expenseType: Comida
  , extra: false
  , description: Just "Iguarias"
  }
