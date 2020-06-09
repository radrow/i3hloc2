{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hloc.Worker where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import Control.Monad.State.Strict
import Data.Text(Text)
import Data.Typeable
import Data.Aeson as Aeson

import Hloc.Block
import Hloc.I3Bar

data Worker = Worker
  { hlocLock   :: MVar ()
  , jsonsRef   :: MVar [Aeson.Value]
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

runWorker :: MonadIO m => Worker -> Block -> m ThreadId
runWorker worker block = liftIO $ forkIO $ evalStateT (loop worker) block

loop :: Worker -> StateT Block IO ()
loop worker = forever $ do
  prevBlock <- get
  block <- liftIO $ do
    block <- modifyMVar (jsonsRef worker) $ \_ -> do
      block <- update prevBlock `catch` \(_ :: SomeException) -> return (crashed prevBlock)
      return (map toJSON $ serialize block, block)
    void $ tryPutMVar (hlocLock worker) ()
    threadDelay (waitTime block)
    return block
  put block
