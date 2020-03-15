module Hloc.Hloc where

import           Control.Concurrent
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.IO

import           Hloc.Block
import           Hloc.I3Bar
import           Hloc.Worker


data Hloc = Hloc
  { semaphore :: QSem
  , workers   :: [(ThreadId, MVar Block)]
  }

type HlocM = StateT Hloc IO

createWorker :: Block -> HlocM ()
createWorker block = do
  me <- gets semaphore
  blockMV <- liftIO $ newMVar block
  let worker = Worker
        { hloc = me
        , blockRef = blockMV
        }
  threadId <- runWorker worker
  modify (\s -> s{workers = (threadId, blockMV):workers s})


runHloc :: I3BarHeader -> [Block] -> IO ()
runHloc header inits = do
  s <- newQSem 0
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
  blocks <- forM ws $ \(_, block) ->
    liftIO . readMVar $ block
  printBlocks blocks
  s <- gets semaphore
  liftIO $ waitQSem s

printBlocks :: [Block] -> HlocM ()
printBlocks blocks = do
  let jsons = concatMap serialize blocks
      encoded = encode $ toJSON jsons
  liftIO $ do
    BS.putStrLn $ encoded <> BS.pack ","
    hFlush stdout
