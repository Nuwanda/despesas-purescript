module DateInput (dateInput) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Array.NonEmpty (singleton, toArray)
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.JSDate (fromTime, getDate, getFullYear, getMonth, getTime, jsdate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Text.Format (format, width, zeroFill)

dateRegex :: Regex
dateRegex =
  case regex "^([0-9]{4})-([0-9]{2})-([0-9]{2})$" noFlags of
    Right rx -> rx
    Left _ -> unsafeThrow "Regex compiled fine, this should never throw"

timeFromDateParts :: Number -> Number -> Number -> Number
timeFromDateParts year month day =
  let date = jsdate { year
                    , month
                    , day
                    , hour: 0.0
                    , minute: 0.0
                    , second: 0.0
                    , millisecond: 0.0
                    }
  in getTime date

timeFromISOString :: String -> Maybe Number
timeFromISOString s =
  let matches = fromMaybe (singleton $ Just "0") $ match dateRegex s
      maybeStringToNumber = fromMaybe "0" >>> fromString >>> fromMaybe 0.0
      dateParts = maybeStringToNumber <$> (toArray matches)
  in case dateParts of
    [_, year, month, day] -> Just $ timeFromDateParts year month day
    _ -> Nothing

isoStringFromTime :: Maybe Number -> Effect String
isoStringFromTime time =
  case time of
    Nothing -> pure ""
    Just t -> do
      month <- getMonth date
      year <- getFullYear date
      day <- getDate date
      pure $ toDatePart 4 year <> "-"
        <> toDatePart 2 month <> "-"
        <> toDatePart 2 day
      where date = fromTime t
            toDatePart w =
              fromNumber >>> fromMaybe 0 >>> format (zeroFill <> width w)

dateInput :: Maybe Number -> Widget HTML (Maybe Number)
dateInput time = do
  date <- liftEffect $ isoStringFromTime time
  timeFromISOString <$> D.input
      [ P._type "date"
      , P.defaultValue date
      , P.unsafeTargetValue <$> P.onBlur
      , P.className "form-input"
      ]
