module Hloc.Blocks.Constant(constant) where

import Hloc.Block

newtype Constant = Constant String

constant :: String -> Constant
constant = Constant

instance IsBlock Constant where
  serialize (Constant s) = pure defaultBlock { fullText = s }
  update = pure
  waitTime _ = maxBound
