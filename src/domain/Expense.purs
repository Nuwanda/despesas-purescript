module Expense where

import Data.Maybe (Maybe(..))

data ExpenseType
  = Casa
  | Carro
  | Comida
  | Saídas
  | Misc

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
