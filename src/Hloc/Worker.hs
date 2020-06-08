{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hloc.Worker where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import Control.Monad
import Data.Text(Text)
import Data.Typeable

import Hloc.Block
import Hloc.I3Bar

data Worker = Worker
  { hlocLock   :: MVar ()
  , blockRef   :: MVar Block
  }

newtype CrashedBlock = CrashedBlock { uncrashBlock :: Block }

instance IsBlock CrashedBlock where
  update b = CrashedBlock <$> update (uncrashBlock b)
  waitTime = waitTime . uncrashBlock
  serialize b =
    let s = serialize (uncrashBlock b)

        errorNote :: Text
        errorNote =
          "<span foreground=\"black\" background=\"red\">[\xf0e7\xf0e7\xf0e7]</span>"

        withErrorNote :: I3BarBlock -> I3BarBlock
        withErrorNote i3b = i3b
          { i3bFullText = errorNote <> i3bFullText i3b
          , i3bShortText = fmap (errorNote <>) (i3bShortText i3b)
          , i3bMarkup = Pango
          }
    in map withErrorNote s

crashed :: Block -> Block
crashed bAny@(Block b) = case cast b :: Maybe CrashedBlock of
  Just cb -> Block cb
  Nothing -> Block $ CrashedBlock bAny

runWorker :: MonadIO m => Worker -> m ThreadId
runWorker worker = liftIO $ forkIO $ loop worker

loop :: Worker -> IO ()
loop worker = forever $ do
  prevBlock <- takeMVar (blockRef worker)
  block <- update prevBlock `catch` \(_ :: SomeException) ->
           return (crashed prevBlock)
  putMVar (blockRef worker) block
  void $ tryPutMVar (hlocLock worker) ()
  threadDelay (waitTime block)
