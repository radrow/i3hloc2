{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Hloc.Block
  ( module Hloc.I3Bar
  , IsBlock(..)
  , Block(..)
  , BlockMeta(..)
  , serializationBase
  , defaultMeta
  ) where

import Data.Text

import Hloc.Color
import Hloc.I3Bar (I3BarBlock(..), Align(..), defaultBlock)


class IsBlock b where
  serialize :: b -> [I3BarBlock]
  update :: b -> IO b
  waitTime :: b -> Int
  getMeta :: b -> Maybe BlockMeta
  getMeta = const Nothing

data Block where
  Block :: IsBlock b => b -> Block

instance IsBlock Block where
  serialize (Block b) = serialize b
  update (Block b) = Block <$> update b
  waitTime (Block b) = waitTime b
  getMeta (Block b) = getMeta b

data BlockMeta = BlockMeta
  { bmColor :: Maybe Color
  , bmBackground :: Maybe Color
  , bmBorder :: Maybe Color
  , bmBorderBottom :: Int
  , bmBorderTop :: Int
  , bmBorderRight :: Int
  , bmBorderLeft :: Int
  , bmMinWidth :: Maybe Int
  , bmAlign :: Align
  , bmName :: Maybe Text
  , bmInstance :: Maybe Text
  , bmUrgent :: Bool
  , bmSeparator :: Bool
  , bmSeparatorBlockWidth :: Int
  }

{-# INLINE serializeMeta #-}
serializeMeta :: BlockMeta -> I3BarBlock
serializeMeta bm = defaultBlock
  { i3bColor = bmColor bm
  , i3bBackground = bmBackground bm
  , i3bBorder = bmBorder bm
  , i3bBorderBottom = bmBorderBottom bm
  , i3bBorderTop = bmBorderTop bm
  , i3bBorderRight = bmBorderRight bm
  , i3bBorderLeft = bmBorderLeft bm
  , i3bMinWidth = bmMinWidth bm
  , i3bAlign = bmAlign bm
  , i3bName = bmName bm
  , i3bInstance = bmInstance bm
  , i3bUrgent = bmUrgent bm
  , i3bSeparator = bmSeparator bm
  , i3bSeparatorBlockWidth = bmSeparatorBlockWidth bm
  }

{-# INLINE serializationBase #-}
serializationBase :: IsBlock b => b -> I3BarBlock
serializationBase = maybe defaultBlock serializeMeta . getMeta


defaultMeta :: BlockMeta
defaultMeta = BlockMeta
  { bmColor = Nothing
  , bmBackground = Nothing
  , bmBorder = Nothing
  , bmBorderBottom = 1
  , bmBorderTop = 1
  , bmBorderRight = 1
  , bmBorderLeft = 1
  , bmMinWidth = Nothing
  , bmAlign = AlignLeft
  , bmName = Nothing
  , bmInstance = Nothing
  , bmUrgent = False
  , bmSeparator = True
  , bmSeparatorBlockWidth = 9
  }
