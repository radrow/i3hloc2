module Hloc.Blocks.SystemCommand(systemCommandSimple) where

import Data.Text
import Hloc.Block
import System.Process

data SCBlockSimple = SCBlockSimple
  { meta    :: !BlockMeta
  , command :: !String
  , args    :: ![String]
  , delay   :: !Int
  , output  :: !String
  }

systemCommandSimple :: BlockMeta -> String -> [String] -> Int -> Block
systemCommandSimple m c a d = Block $ SCBlockSimple
  { meta = m
  , command = c
  , args = a
  , delay = d
  , output = ""
  }

instance IsBlock SCBlockSimple where
  serialize b = [(serializationBase b){ i3bFullText = pack $ output b }]
  update b = do
    (_r, o, _e) <- readProcessWithExitCode (command b) (args b) ""
    return $ b{output = o}
  waitTime = delay
  getMeta = Just . meta

