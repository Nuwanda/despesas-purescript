module NumberInput (numberInput) where

import Prelude
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Maybe (Maybe(..))
import Data.Number (fromString)

numberInput :: Maybe Number -> Widget HTML (Maybe Number)
numberInput n =
  let value = case n of
        Just v -> show v
        Nothing -> ""
  in fromString <$> D.input [ P._type "number"
                            , P.defaultValue value
                            , P.unsafeTargetValue <$> P.onBlur
                            , P.className "form-input"
                            ]

