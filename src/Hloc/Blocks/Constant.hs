module Hloc.Blocks.Constant(constant) where

import Data.Text
import Data.String
import Hloc.Block

data Constant = Constant
  { meta :: !BlockMeta
  , content :: !Text
  }

constant :: BlockMeta -> Text -> Block
constant m t = Block $ Constant m t

instance IsBlock Constant where
  serialize b = [(serializationBase b){ i3bFullText = content b }]
  update = pure
  waitTime _ = maxBound
  getMeta = Just . meta
