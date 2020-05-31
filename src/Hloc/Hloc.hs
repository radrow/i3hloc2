{-# LANGUAGE OverloadedStrings #-}
module Hloc.Hloc where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map as M
import           Data.Text(Text)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.IO

import           Hloc.Block
import           Hloc.Color
import           Hloc.I3Bar
import           Hloc.Worker

type PostProcess = M.Map Text (
  Maybe Text, I3BarBlock -> I3BarBlock)

data Hloc = Hloc
  { semaphore   :: MVar ()
  , workers     :: [(ThreadId, MVar Block)]
  , postProcess :: PostProcess
  }

type HlocM = StateT Hloc IO

createWorker :: Block -> HlocM ()
createWorker block = do
  me <- gets semaphore
  blockMV <- liftIO $ newMVar block
  let worker = Worker
        { hlocLock = me
        , blockRef = blockMV
        }
  threadId <- runWorker worker
  modify (\s -> s{workers = (threadId, blockMV):workers s})


runHloc :: I3BarHeader -> [Block] -> IO ()
runHloc header inits = do
  s <- newMVar ()
  let initState = Hloc
        { semaphore = s
        , workers = []
        , postProcess = M.empty
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
  blocks <- forM ws $ \(_, block) ->
    liftIO . readMVar $ block
  pp <- gets postProcess
  printBlocks pp blocks
  s <- gets semaphore
  liftIO $ takeMVar s

printBlocks :: PostProcess -> [Block] -> HlocM ()
printBlocks pp blocks = do
  let jsons = concatMap (map (applyPostProcess pp) . serialize) blocks
      encoded = encode $ toJSON jsons
  liftIO $ do
    BS.putStrLn $ encoded <> BS.pack ","
    hFlush stdout

applyPostProcess :: PostProcess -> I3BarBlock -> I3BarBlock
applyPostProcess pp b =
  case i3bName b of
    Nothing -> case M.lookup "" pp of
      Nothing -> b
      Just (_, f) -> f b
    Just n -> case M.lookup n pp of
      Nothing -> b
      Just (_, f) -> f b
