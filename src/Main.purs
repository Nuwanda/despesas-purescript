module Main where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import DB as DB
import Data.Array (deleteAt, (:))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Console as Console
import Effect.Exception (Error)
import Expense (ExpenseForm, Expense, defaultExpense, fromFormToModel)
import ExpenseForm (expenseForm)
import ExpenseTable (expenseTable)

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

wrapper :: forall a. Array Expense ->  Widget HTML a
wrapper expenses = do
  res <-
    D.div [P.className "container"]
    [ D.div [P.className "columns"]
        [ D.div_ [P.className "column col-6 col-md-12 col-mx-auto p-2"]
          (expenseForm defaultExpense) <#> Add
        , D.div
          [ P.className "column col-6 col-md-12 col-mx-auto p-2 table-container"]
          [ (expenseTable expenses) <#> Remove
          , saveButton $> Save
          ]
        ]
    ]
  case res of
    Add exp -> wrapper ((fromFormToModel exp) : expenses)
    Remove idx -> wrapper $ fromMaybe [] (deleteAt idx expenses)
    Save -> do
      liftAff $ DB.insert expenses
      wrapper []

printValues :: Either Error (Array Expense) -> Effect Unit
printValues res =
  case res of
    Left e -> Console.errorShow e
    Right v -> Console.logShow v

main :: Effect Unit
main = do
  runAff_ printValues DB.getAll
  runWidgetInDom "root" $ wrapper []
