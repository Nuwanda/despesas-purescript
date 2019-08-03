module ExpenseForm (Expense, ExpenseType, expenseForm, defaultExpense) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Maybe (Maybe(..), fromMaybe)
import DateInput (dateInput)
import NumberInput (numberInput)

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
    , D.div_ [P.className "col-9 col-sm-12"] child
    ]

expenseForm :: Expense -> Widget HTML Expense
expenseForm form = do
  -- This is like Elm's view function
  res <- D.form [P.className "form-horizontal"]
         [ Value <$> formGroup "Value" (numberInput $ Just form.value)
         , Date <$> formGroup "Date" (dateInput form.date)
         , Extra (not form.extra) <$ D.div [P.className "form-group"]
           [ D.div [P.className "col-9 col-sm-12 col-ml-auto"]
             [D.label [P.className "form-switch"]
              [ D.input
                [ P._type "checkbox"
                , P.checked form.extra
                , P.onChange
                ]
              , D.i [P.className "form-icon"] []
              , D.text "Extra"
              ]
             ]
           ]
         , Description <$> formGroup "Description" (D.textarea
           [ P.unsafeTargetValue <$> P.onBlur
           , P.defaultValue $ fromMaybe "" form.description
           , P.className "form-input"
           ]
           [])
         , Submit <$ D.button [P.onClick] [D.text "Adicionar"]
    ]
   -- This is like Elm's update function
  case res of
    Value s -> expenseForm $ form {value = fromMaybe 0.0 s}
    Extra b -> expenseForm $ form {extra = b}
    Date s -> expenseForm $ form {date = s}
    Description d -> expenseForm $ form {description = Just d}
    Submit -> pure form
    _ -> expenseForm form

defaultExpense :: Expense
defaultExpense =
  { value: 7.0
  , date: Nothing
  , expenseType: Comida
  , extra: false
  , description: Just "Iguarias"
  }
