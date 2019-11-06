module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import DB as DB
import DOM as DOM
import Data.Array (deleteAt, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Expense (Expense, ExpenseForm, defaultExpense, fromFormToModel)
import ExpenseForm (expenseForm)
import ExpenseTable (expenseTable)
import Control.Alternative ((<|>))

data Actions
  = Add ExpenseForm
  | Remove Int
  | Save


saveButton :: Widget HTML Unit
saveButton =
  D.button [ P.onClick $> unit
           , P.className "btn btn-lg btn-primary float-right mt-2"
           , P._type "button"
           ]
    [D.text "Save"]

app :: forall a. Maybe ExpenseForm -> Array Expense ->  Widget HTML a
app exp expenses = do
  let current = case exp of
                     Nothing -> defaultExpense
                     Just e -> e
  res <-
    D.div [P.className "container"]
    [ D.div [P.className "columns"]
        [ D.div_ [P.className "column col-6 col-md-12 col-mx-auto p-2"]
          (expenseForm current) <#> Add
        , D.div
          [ P.className "column col-6 col-md-12 col-mx-auto p-2 table-container"]
          [ (expenseTable expenses) <#> Remove
          , saveButton $> Save
          ]
        ]
    ]
  case res of
    Add e -> do
      isValid <- liftEffect $ DOM.reportFormValidity "expenseForm"
      case isValid of
        true -> app Nothing ((fromFormToModel e) : expenses)
        false -> app (Just e) expenses
    Remove idx -> app Nothing $ fromMaybe [] (deleteAt idx expenses)
    Save -> do
      liftAff $ DB.insert expenses
      app Nothing []

main :: Effect Unit
main = do
  runWidgetInDom "root" $ app Nothing []
