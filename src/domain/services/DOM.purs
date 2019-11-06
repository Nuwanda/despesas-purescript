module DOM (reportFormValidity, checkFormValidity) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode as Node
import Web.HTML as HTML
import Web.HTML.HTMLDocument as D
import Web.HTML.HTMLFormElement as F
import Web.HTML.Window as W

getElementById :: String -> Effect (Maybe Element)
getElementById elementId = do
  document <- W.document =<< HTML.window
  Node.getElementById elementId (D.toNonElementParentNode document)

callOnForm :: (F.HTMLFormElement -> Effect Boolean) -> String -> Effect Boolean
callOnForm f formId = do
  maybeEle <- getElementById formId
  case maybeEle of
    Nothing -> pure false
    Just ele ->
      let maybeForm = F.fromElement ele
      in case maybeForm of
        Nothing -> pure false
        Just form -> f form

checkFormValidity :: String -> Effect Boolean
checkFormValidity = callOnForm F.checkValidity

reportFormValidity :: String -> Effect Boolean
reportFormValidity = callOnForm F.reportValidity
