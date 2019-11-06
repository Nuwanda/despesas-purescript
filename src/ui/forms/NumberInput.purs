module NumberInput (numberInput) where

import Prelude
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P

numberInput :: String -> Widget HTML String
numberInput value = do
  D.input [ P._type "number"
          , P.value value
          , P.onChange <#> P.unsafeTargetValue
          , P.className "form-input"
          , P.min "0"
          , P.step "0.01"
          , P.required true
          ]

