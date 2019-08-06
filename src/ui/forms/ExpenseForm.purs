module ExpenseForm (expenseForm) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Maybe (Maybe(..), fromMaybe)
import DateInput (dateInput)
import NumberInput (numberInput)
import CheckboxInput (checkboxInput)
import Expense (Expense, ExpenseType, defaultExpense)

data FormAction
  = Value (Maybe Number)
  | Date (Maybe Number)
  | Type ExpenseType
  | Extra Boolean
  | Description String
  | Submit

formGroup :: forall a. String -> Widget HTML a -> Widget HTML a
formGroup label child =
  D.div [P.className "form-group"]
    [ D.div [P.className "col-3 col-sm-12"]
        [D.label [P.className "form-label"] [D.text label]]
    , D.div [P.className "col-9 col-sm-12"] [child]
    ]

checkboxFormGroup :: String -> Boolean -> Widget HTML Boolean
checkboxFormGroup label value =
  D.div [P.className "form-group"]
  [ D.div [P.className "col-9 col-sm-12 col-ml-auto"]
    [checkboxInput label value]]

textarea :: Maybe String -> Widget HTML String
textarea value =
  D.textarea
  [ P.unsafeTargetValue <$> P.onBlur
  , P.defaultValue $ fromMaybe "" value
  , P.className "form-input"
  ]
  []

submitButton :: Widget HTML Unit
submitButton =
  D.button
  [ P.onClick $> unit
  , P.className "btn btn-lg btn-primary float-right"
  ]
  [D.text "Submit"]

expenseForm :: Expense -> Widget HTML Expense
expenseForm exp = do
  res <- D.form [P.className "form-horizontal"]
         [ formGroup "Value" (numberInput $ Just exp.value) <#> Value
         , formGroup "Date" (dateInput exp.date) <#> Date
         , checkboxFormGroup "Extra" exp.extra <#> Extra
         , formGroup "Description" (textarea exp.description)  <#> Description
         , submitButton $> Submit
    ]
  -- Handle Action and recur
  case res of
    Value s -> expenseForm $ exp {value = fromMaybe 0.0 s}
    Extra b -> expenseForm $ exp {extra = b}
    Date s -> expenseForm $ exp {date = s}
    Description d -> expenseForm $ exp {description = Just d}
    Submit -> pure exp
    _ -> expenseForm exp
