{-# LANGUAGE OverloadedStrings #-}
module Hloc.Hloc where

import           Control.Concurrent
import           Control.Monad.State
import           Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.IO

import           Hloc.Block
import           Hloc.I3Bar
import           Hloc.Worker


data Hloc = Hloc
  { semaphore   :: MVar ()
  , workers     :: [(ThreadId, MVar [Aeson.Value])]
  }

type HlocM = StateT Hloc IO

createWorker :: Block -> HlocM ()
createWorker block = do
  me <- gets semaphore
  jsonsMV <- liftIO $ newMVar []
  let worker = Worker
        { hlocLock   = me
        , jsonsRef   = jsonsMV
        }
  threadId <- runWorker worker block
  modify (\s -> s{workers = (threadId, jsonsMV):workers s})


runHloc :: I3BarHeader -> [Block] -> IO ()
runHloc header inits = do
  s <- newMVar ()
  let initState = Hloc
        { semaphore = s
        , workers = []
        }
  flip evalStateT initState $ do
    forM_ inits createWorker
    liftIO $ do
      BS.putStrLn $ encode (toJSON header)
      BS.putStrLn $ BS.pack "["
      hFlush stdout
    loopHloc

loopHloc :: HlocM ()
loopHloc = forever $ do
  ws <- gets workers
  blocks <- fmap concat $ forM ws $ \(_, block) ->
    liftIO . readMVar $ block
  printBlocks blocks
  s <- gets semaphore
  liftIO $ takeMVar s

printBlocks :: [Aeson.Value] -> HlocM ()
printBlocks blocks = do
  let encoded = encode $ toJSON blocks
  liftIO $ do
    BS.putStrLn $ encoded <> BS.pack ","
    hFlush stdout
