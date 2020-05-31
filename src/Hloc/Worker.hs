module Hloc.Worker where

import Control.Concurrent
import Control.Monad.Trans
import Control.Monad


import Hloc.Block

data Worker = Worker
  { hlocLock :: MVar ()
  , blockRef :: MVar Block
  }

runWorker :: MonadIO m => Worker -> m ThreadId
runWorker worker = liftIO $ forkIO $ loop worker -- FIXME forkFinally?

loop :: Worker -> IO ()
loop worker = forever $ do
  block <- takeMVar (blockRef worker) >>= update
  putMVar (blockRef worker) block
  void $ tryPutMVar (hlocLock worker) ()
  threadDelay (waitTime block)
