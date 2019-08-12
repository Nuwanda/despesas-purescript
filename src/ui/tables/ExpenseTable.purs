module ExpenseTable (expenseTable) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Array (mapWithIndex)
import Data.Maybe (fromMaybe)

import Expense (Expense)

row :: Int -> Expense -> Widget HTML Int
row index exp =
  index <$ D.tr'
       [ D.td' [D.number exp.value]
       , D.td' [D.text $ show exp.expenseType]
       , D.td' [D.number $ fromMaybe 0.0 exp.date]
       , D.td' [D.text $ show exp.extra]
       , D.td' [D.text $ fromMaybe "" exp.description]
       , D.td' [D.button [P.onClick, P.className "btn btn-primary btn-action"]
                [D.i [P.className "icon icon-delete"] []]]
       ]

expenseTable :: Array Expense -> Widget HTML Int
expenseTable expenses =
  D.table [P.className "table table-striped table-hover"]
  [ D.thead'
    [D.tr'
     [ D.th' [D.text "Value"]
     , D.th' [D.text "Type"]
     , D.th' [D.text "Date"]
     , D.th' [D.text "Extra"]
     , D.th' [D.text "Description"]
     , D.th' []
     ]
    ]
  , D.tbody'
    (mapWithIndex row expenses)
  ]
