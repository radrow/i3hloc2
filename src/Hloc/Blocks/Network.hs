module Hloc.Blocks.Network
  ( network
  , networkDefault
  , NetworkEntity(..)
  , DataUnit(..)
  ) where

import Data.Functor((<&>))
import Data.List(intersperse)
import System.FilePath
import System.Directory
import System.Clock

import Hloc.Block


data NetworkEntity
  = PlainText String
  | UpSpeed
  | DownSpeed
  | Status
  | StatusNoUp
  | IP

data DataUnit
  = Bytes
  | KiloBytes
  | KibiBytes
  | MegaBytes
  | MebiBytes
  | GigaBytes
  | GibiBytes
  | PetaBytes
  | PebiBytes
  | AutoISO
  | AutoBin

data State = Dormant | Up | Down | Unknown | Missing

data TransferStats = TransferStats
  { lastUpBytes   :: Int
  , lastDownBytes :: Int
  }
data SpeedStats = SpeedStats
  { lastUpSpeed   :: Int
  , lastDownSpeed :: Int
  }

data Network = Network
  { format        :: [NetworkEntity]
  , dataUnit      :: DataUnit
  , displayUnit   :: Bool
  , delay         :: Int
  , lastUpdate    :: TimeSpec
  , transferStats :: Maybe TransferStats
  , speedStats    :: Maybe SpeedStats
  , state         :: State
  , interface     :: String
  , interfaceDir  :: String
  }

network :: Int -> String -> FilePath -> DataUnit -> Bool -> [NetworkEntity] -> IO Network
network d i cp du disp f = do
  timeNow <- getTime Realtime
  let path = cp </> i
  s <- getState path
  transfer <- case s of
    Up -> fmap Just $ TransferStats <$> getUp path <*> getDown path
    _ -> return Nothing
  return Network
    { format = f
    , dataUnit = du
    , displayUnit = disp
    , delay = d
    , lastUpdate = timeNow
    , transferStats = transfer
    , speedStats = Nothing
    , state = s
    , interface = i
    , interfaceDir = path
    }

networkDefault :: String -> IO Network
networkDefault int =
  network 1000000 int "/sys/class/net/" AutoBin True [Status, DownSpeed, UpSpeed]

getState :: FilePath -> IO State
getState path = do
  let file = path </> "operstate"
  exists <- doesFileExist file
  if exists
    then filter (/= '\n') <$> readFile file <&> \case
    "up" -> Up
    "down" -> Down
    "dormant" -> Dormant
    _ -> Unknown
    else return Missing


getUp :: FilePath -> IO Int
getUp path = do
  let file = path </> "statistics" </> "tx_bytes"
  readFile file <&> read


getDown :: FilePath -> IO Int
getDown path = do
  let file = path </> "statistics" </> "rx_bytes"
  readFile file <&> read

instance IsBlock Network where
  waitTime = delay
  serialize b =
    let s = state b
        unit = dataUnit b
        dispUnit = displayUnit b
        out = unwords $ map printEntity (format b) where
          printEntity = \case
            PlainText str -> str
            UpSpeed ->
              let ms = showInUnit unit dispUnit . lastUpSpeed <$> speedStats b
              in maybe "" (++"↑")ms
            DownSpeed ->
              let ms = showInUnit unit dispUnit . lastDownSpeed <$> speedStats b
              in maybe "" (++"↓") ms
            Status -> showState s
            StatusNoUp -> case s of
              Up -> ""
              _ -> showState s
    in pure defaultBlock {fullText = out}
  update b = do
    let path = interfaceDir b
    s <- getState path
    timeNow <- getTime Realtime
    case s of
      Up -> do
        newUp <- getUp path
        newDown <- getDown path
        case transferStats b of
          Just TransferStats{lastUpBytes = oldUp, lastDownBytes = oldDown} -> do
            let timeDelta :: Int
                timeDelta = fromInteger $ toNanoSecs (timeNow - lastUpdate b)
                deltaUp = newUp - oldUp
                deltaDown = newDown - oldDown
                upSpeed = (deltaUp * 1000000000) `quot` timeDelta
                downSpeed = (deltaDown * 1000000000) `quot` timeDelta
            return b
              { state = s
              , transferStats =
                  Just TransferStats{lastUpBytes = newUp, lastDownBytes = newDown}
              , speedStats =
                  Just SpeedStats{lastUpSpeed = upSpeed, lastDownSpeed = downSpeed}
              , lastUpdate = timeNow
              }
          Nothing ->
            return b
            { state = s
            , transferStats =
              Just TransferStats{lastUpBytes = newUp, lastDownBytes = newDown}
            , speedStats = Nothing
            , lastUpdate = timeNow
            }
      _ ->
        return b
        { state = s
        , transferStats = Nothing
        , speedStats = Nothing
        , lastUpdate = timeNow
        }

showInUnit :: DataUnit -> Bool -> Int -> String
showInUnit unit dispUnit val =
  let optShow :: Int -> String -> String
      optShow v s = show v ++ if dispUnit then ' ':s else ""
      showInUnit' u = case u of
        Bytes -> optShow val "B"
        KiloBytes -> optShow (val `quot` 1000) "kB"
        KibiBytes -> optShow (val `quot` 1024) "kiB"
        MegaBytes -> optShow (val `quot` (1000^(2::Int))) "MB"
        MebiBytes -> optShow (val `quot` (1024^(2::Int))) "MiB"
        GigaBytes -> optShow (val `quot` (1000^(3::Int))) "GB"
        GibiBytes -> optShow (val `quot` (1024^(3::Int))) "GiB"
        PetaBytes -> optShow (val `quot` (1000^(4::Int))) "PB"
        PebiBytes -> optShow (val `quot` (1024^(4::Int))) "PiB"
        AutoISO -> if
          | val < 2*1000 -> showInUnit' Bytes
          | val < 2*1000^(2::Int) -> showInUnit' KiloBytes
          | val < 2*1000^(3::Int) -> showInUnit' MegaBytes
          | val < 2*1000^(4::Int) -> showInUnit' GigaBytes
          | otherwise -> showInUnit' PetaBytes
        AutoBin -> if
          | val < 2*1024 -> showInUnit' Bytes
          | val < 2*1024^(2::Int) -> showInUnit' KibiBytes
          | val < 2*1024^(3::Int) -> showInUnit' MebiBytes
          | val < 2*1024^(4::Int) -> showInUnit' GibiBytes
          | otherwise -> showInUnit' PebiBytes
  in showInUnit' unit

showState :: State -> String
showState = \case
  Up -> "up"
  Down -> "down"
  Dormant -> "disconnected"
  Missing -> "missing"
  Unknown -> "unknown"
