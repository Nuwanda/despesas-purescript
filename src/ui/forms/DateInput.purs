module DateInput (dateInput) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P

dateInput :: String -> Widget HTML String
dateInput date =
  D.input
    [ P._type "date"
    , P.value date
    , P.unsafeTargetValue <$> P.onChange
    , P.className "form-input"
    , P.required true
    ]
