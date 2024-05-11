module DOM where

import Prelude

import Effect (Effect)
import Web.DOM (Element)

foreign import clearHTML :: Element -> Effect Unit