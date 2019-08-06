module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import ExpenseForm (expenseForm)
import Expense (defaultExpense)

dummyHandler :: forall a. Widget HTML a
dummyHandler = do
  _ <- expenseForm defaultExpense
  D.div [P.className "text-center"] [D.text "Form Submitted"]

wrapper :: forall a. Widget HTML a
wrapper = do
  D.div [P.className "container"]
    [ D.div [P.className "columns"]
      [ D.div_ [P.className "column col-6 col-mx-auto"]
        dummyHandler]]

main :: Effect Unit
main = runWidgetInDom "root" wrapper
