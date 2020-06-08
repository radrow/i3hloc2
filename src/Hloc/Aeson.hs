module Hloc.Aeson where

import Data.Char
import Data.Aeson
import Data.Aeson.Casing

deriveOptions :: Options
deriveOptions = (aesonPrefix snakeCase)
  { sumEncoding = UntaggedValue
  , omitNothingFields = True
  , constructorTagModifier = (camelTo2 '_')
  }
