{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Hloc.Block
  ( module Hloc.I3Bar
  , IsBlock(..)
  , Block(..)
  ) where

import Hloc.I3Bar (I3BarBlock(..), defaultBlock)


class IsBlock b where
  serialize :: b -> [I3BarBlock]
  update :: b -> IO b
  waitTime :: b -> Int

data Block where
  Block :: IsBlock b => b -> Block

instance IsBlock Block where
  serialize (Block b) = serialize b
  update (Block b) = Block <$> update b
  waitTime (Block b) = waitTime b
