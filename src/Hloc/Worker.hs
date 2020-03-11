module Hloc.Worker where

import Control.Concurrent.QSem
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad


import Hloc.Block

data Worker = Worker
  { hloc :: QSem
  , blockRef :: MVar Block
  }

runWorker :: MonadIO m => Worker -> m ThreadId
runWorker worker = liftIO $ forkIO $ loop worker -- FIXME forkFinally?

loop :: Worker -> IO ()
loop worker = forever $ do
  block <- takeMVar (blockRef worker) >>= update
  putMVar (blockRef worker) block
  signalQSem (hloc worker)
  threadDelay (waitTime block)
