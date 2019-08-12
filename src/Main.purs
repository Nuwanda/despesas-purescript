module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Array ((:), deleteAt)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Expense (Expense, defaultExpense)
import ExpenseForm (expenseForm)
import ExpenseTable (expenseTable)

data Actions
  = Add Expense
  | Remove Int

wrapper :: forall a. Array Expense ->  Widget HTML a
wrapper expenses = do
  res <-
    D.div [P.className "container"]
    [ D.div [P.className "columns"]
      [ D.div_ [P.className "column col-6 col-md-12 col-mx-auto p-2"]
        (expenseForm defaultExpense) <#> Add
      , D.div_ [P.className "column col-6 col-md-12 col-mx-auto p-2"]
        (expenseTable expenses) <#> Remove
      ]
    ]
  case res of
    Add exp -> wrapper (exp : expenses)
    Remove idx -> wrapper $ fromMaybe [] (deleteAt idx expenses)

main :: Effect Unit
main = runWidgetInDom "root" $ wrapper []
