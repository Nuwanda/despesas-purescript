module CheckboxInput (checkboxInput) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P

checkboxInput :: String -> Boolean -> Widget HTML Boolean
checkboxInput label value =
  D.label [P.className "form-switch"]
  [ D.input
    [ P._type "checkbox"
    , P.checked value
    , P.onChange $> (not value)
    ]
  , D.i [P.className "form-icon"] []
  , D.text label
  ]
