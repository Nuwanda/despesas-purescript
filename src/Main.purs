module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import ExpenseForm (defaultExpense, expenseForm)

wrapper :: forall a. Widget HTML a
wrapper = do
  result <-
    D.div [P.className "container"]
      [ D.div [P.className "columns"]
        [ D.div [P.className "column col-6 col-mx-auto"]
          [expenseForm defaultExpense]]]
  D.div [P.className "container"]
    [ D.div [P.className "columns"]
      [ D.div [P.className "column col-12 text-center"]
        [D.text "Form submitted"]]]

main :: Effect Unit
main = runWidgetInDom "root" wrapper
