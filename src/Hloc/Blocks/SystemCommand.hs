module Hloc.Blocks.SystemCommand(systemCommandSimple) where

import Hloc.Block
import System.Process

data SCBlockSimple = SCBlockSimple
  { command :: String
  , args :: [String]
  , delay :: Int
  , output :: String
  }

systemCommandSimple :: String -> [String] -> Int -> SCBlockSimple
systemCommandSimple c a d = SCBlockSimple
  { command = c
  , args = a
  , delay = d
  , output = ""
  }

instance IsBlock SCBlockSimple where
  serialize b = pure defaultBlock { fullText = output b }
  update b = do
    (_r, o, _e) <- readProcessWithExitCode (command b) (args b) ""
    return $ b{output = o}
  waitTime = delay

