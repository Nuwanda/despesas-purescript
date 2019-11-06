module ExpenseForm (expenseForm) where

import Prelude

import CheckboxInput (checkboxInput)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Read (readDefault)
import DateInput (dateInput)
import Effect (Effect)
import Effect.Class (liftEffect)
import Expense (ExpenseForm, ExpenseType, allExpenseTypes)
import NumberInput (numberInput)
import React.SyntheticEvent (SyntheticMouseEvent, preventDefault, stopPropagation)

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
  [ P.unsafeTargetValue <$> P.onChange
  , P.value $ fromMaybe "" value
  , P.className "form-input"
  ]
  []

optionType :: forall a. ExpenseType -> Widget HTML a
optionType t = D.option [P.value $ show t] [D.text $ show t]

selectType :: ExpenseType -> Widget HTML ExpenseType
selectType t = do
  readDefault <<< P.unsafeTargetValue <$>
    D.select [P.className "form-select", P.value $ show t, P.onChange]
    (map optionType allExpenseTypes)

handleSubmitClick :: SyntheticMouseEvent -> Effect Unit
handleSubmitClick e = do
  preventDefault e
  stopPropagation e

submitButton :: Widget HTML Unit
submitButton =
  liftEffect =<< D.button
    [ P.onClick <#> handleSubmitClick
    , P.className "btn btn-lg btn-primary float-right mt-2"
    , P._type "submit"
    ]
    [D.text "Submit"]

data FormAction
  = Value String
  | Date String
  | Type ExpenseType
  | Extra Boolean
  | Description String
  | Submit

expenseForm :: ExpenseForm -> Widget HTML ExpenseForm
expenseForm exp = do
  res <- D.form [P.className "form-horizontal", P._id "expenseForm"]
           [ formGroup "Value" (numberInput exp.value) <#> Value
           , formGroup "Date" (dateInput exp.date) <#> Date
           , formGroup "Type" (selectType exp.expenseType) <#> Type
           , checkboxFormGroup "Extra" exp.extra <#> Extra
           , formGroup "Description" (textarea exp.description)  <#> Description
           , submitButton $> Submit
           ]
  -- Handle Action and recur
  case res of
    Value s -> expenseForm $ exp {value = s}
    Extra b -> expenseForm $ exp {extra = b}
    Date s -> expenseForm $ exp {date = s}
    Description d -> expenseForm $ exp {description = Just d}
    Type t -> expenseForm $ exp {expenseType = t}
    Submit -> pure exp
