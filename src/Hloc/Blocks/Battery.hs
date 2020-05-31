{-# LANGUAGE OverloadedStrings #-}
module Hloc.Blocks.Battery
  ( battery
  , batteryDefault
  , BatteryFormat( Status
                 , Health
                 , Charge
                 , ETA
                 )
  ) where

import           Data.Functor     ((<&>))
import           Data.Maybe       (fromMaybe)
import           Data.String      (IsString (..))
import qualified Data.ByteString as B
import qualified Data.Text        as T
import           Data.Text        (Text)
import           System.Clock
import           System.Directory
import           System.FilePath

import           Hloc.Block

data Health
  = HealthUnknown
  | Good
  | Overheat
  | Dead
  | OverVoltage
  | UnspecifiedFailure
  | Cold
  | WatchdogTimerExpire
  | SafetyTimerExpire
  | OverCurrent
  deriving Eq

data Status
  = StatusUnknown
  | Charging
  | Discharging
  | NotCharging
  | Full
  | Missing
  deriving Eq

data Battery = Battery
  { meta      :: !BlockMeta
  , status    :: !Status
  , format    :: ![BatteryFormat]
  , batName   :: !String
  , classPath :: !FilePath
  , delay     :: !Int
  , etaDelay  :: !Int
  , batInfo   :: !(Maybe BatteryInfo)
  }

data BatteryInfo = BatteryInfo
  { health    :: Health
  , chargeMax :: Int
  , charge    :: Int
  , etaChecks :: [(TimeSpec, Int)]
  }

data BatteryFormat
  = Text !Text
  | Status
  | Health
  | Charge
  | ETA

instance IsString BatteryFormat where
  fromString = Text . T.pack

battery :: BlockMeta -> Int -> Int -> String -> [BatteryFormat] -> IO Block
battery m del edel n f = do
  let path = "/sys/class/power_supply" </> n
  Block <$> updateBatteryInfo Battery
    { meta = m
    , status = StatusUnknown
    , format = f
    , batName = n
    , classPath = path
    , delay = del
    , etaDelay = edel
    , batInfo = Nothing
    }

batteryDefault :: BlockMeta -> String -> [BatteryFormat] -> IO Block
batteryDefault m = battery m 10000000 1200000000

updateBatteryInfo :: Battery -> IO Battery
updateBatteryInfo b = do
  let path = classPath b
  sts <- getStatus path
  newBatInfo <- case sts of
    Missing -> return Nothing
    _ -> do
      tnow <- getTime Realtime
      hlth <- getHealth path
      chr <- getCharge path
      chrMax <- getChargeMax path
      case batInfo b of
        Nothing -> return $ Just
          BatteryInfo
          { health = hlth
          , chargeMax = chrMax
          , charge = chr
          , etaChecks = [(tnow, chr)]
          }
        Just oldBatInfo -> do
          let statusChanged = status b /= sts
          return $ Just
            BatteryInfo
            { health = hlth
            , chargeMax = chrMax
            , charge = chr
            , etaChecks = if statusChanged
                          then [(tnow, chr)]
                          else (tnow,chr):
                               filter
                               (\(t, _) -> t >= tnow - microsecToTimeSpec
                                           (delay b + etaDelay b))
                               (etaChecks oldBatInfo)
            }
  return b{batInfo = newBatInfo, status = sts}

getStatus :: FilePath -> IO Status
getStatus path = do
  let file = path </> "status"
  batExists <- doesDirectoryExist path
  statExists <- doesFileExist file
  if batExists
    then
    if statExists then ({-# SCC "bat_status_read" #-} B.readFile file) <&> \case
      "Charging\n" -> Charging
      "Discharging\n" -> Discharging
      "Full\n" -> Full
      "Unknown\n" -> StatusUnknown
      "Not charging\n" -> NotCharging
      _ -> StatusUnknown
    else return StatusUnknown
  else return Missing

getHealth :: FilePath -> IO Health
getHealth path = do
  let file = path </> "health"
  exists <- doesFileExist file
  if exists
    then B.readFile file <&> \case
    "Unknown\n" -> HealthUnknown
    "Good\n" -> Good
    "Overheat\n" -> Overheat
    "Dead\n" -> Dead
    "Over voltage\n" -> OverVoltage
    "Unspecified failure\n" -> UnspecifiedFailure
    "Cold\n" -> Cold
    "Watchdog timer expire\n" -> WatchdogTimerExpire
    "Safety timer expire\n" -> SafetyTimerExpire
    "Over current\n" -> OverCurrent
    _ -> HealthUnknown
    else return HealthUnknown

getCharge :: FilePath -> IO Int
getCharge path = readFile (path </> "charge_now") <&> read


getChargeMax :: FilePath -> IO Int
getChargeMax path = readFile (path </> "charge_full") <&> read

instance IsBlock Battery where
  waitTime = delay
  serialize b =
    let out = T.unwords $ map printFormat (format b) where
          printFormat = \case
            Text str -> str
            Status -> showStatus b
            Health -> showHealth b
            Charge -> showCapacity b
            ETA -> showEta b
        cap = fromMaybe 100 (getCapacity b)
    in [(serializationBase b){i3bFullText = out, i3bUrgent = cap < 10}]
  update = updateBatteryInfo

microsecToTimeSpec :: Int -> TimeSpec
microsecToTimeSpec m =
  fromNanoSecs (toInteger (m * 1000))

getEta :: Battery -> Maybe Int
getEta Battery{batInfo = Just bi, status = st} =
  case etaChecks bi of
    []  -> Nothing
    [_] -> Nothing
    e   -> case st of
      Discharging ->
        let (lastT, lastC) = head e
            (firstT, firstC) = last e -- irony..
            chrDelta = firstC - lastC
            timeDelta :: Int
            timeDelta = fromInteger (toNanoSecs lastT - toNanoSecs firstT) `quot` 1000
        in
          -- trace ("cd: " <> show chrDelta <> ", td: " <> show timeDelta <> ", chr: " <> show (charge bi)) $
           if chrDelta == 0
           then Nothing
           else Just $ timeDelta * charge bi `div` chrDelta
      Charging ->
        let (lastT, lastC) = head e
            (firstT, firstC) = last e -- irony..
            chrDelta = lastC - firstC
            timeDelta :: Int
            timeDelta = fromInteger (toNanoSecs lastT - toNanoSecs firstT) `quot` 1000
        in if chrDelta == 0
           then Nothing
           else Just $ timeDelta * (chargeMax bi - charge bi) `div` chrDelta
      _ -> Nothing
getEta Battery{batInfo = Nothing} = Nothing

showStatus :: Battery -> Text
showStatus Battery{status = s} = case s of
  StatusUnknown -> ""
  Charging      -> "chr"
  Discharging   -> "dis"
  NotCharging   -> "nchr"
  Full          -> "full"
  Missing       -> "MISSING"

showHealth :: Battery -> Text
showHealth Battery{batInfo = Just BatteryInfo{health = h}} = case h of
  HealthUnknown       -> ""
  Good                -> "good"
  Overheat            -> "overheat"
  Dead                -> "dead"
  OverVoltage         -> "over-v"
  UnspecifiedFailure  -> "fail"
  Cold                -> "cold"
  WatchdogTimerExpire -> "watchdog expire"
  SafetyTimerExpire   -> "safety time expire"
  OverCurrent         -> "over-c"
showHealth _ = ""

getCapacity :: Battery -> Maybe Int
getCapacity Battery{batInfo = Just BatteryInfo{charge = chr, chargeMax = mchr}} =
  if mchr == 0 then Nothing
  else Just $ 100 * chr `quot` mchr
getCapacity _ = Nothing

showCapacity :: Battery -> Text
showCapacity = maybe "" ((<>"%") . T.pack . show) . getCapacity


showEta :: Battery -> Text
showEta b = case getEta b of
  Nothing -> ""
  Just eta ->
    let secs = eta `quot` 1000000
        mins = secs `quot` 60
        hours = mins `quot` 60
        days = hours `quot` 24

        secStr = twoDigitFront . T.pack . show $ secs `rem` 60
        minStr = twoDigitFront . T.pack . show $ mins `rem` 60
        hourStr = twoDigitFront . T.pack . show $ hours `rem` 24
        daysStr = T.pack $ show days
    in T.concat [ if days == 0 then "" else daysStr <> "d "
                , hourStr, ":", minStr, ":", secStr]


twoDigitFront :: Text -> Text
twoDigitFront t = case T.length t of
  0 -> "00"
  1 -> "1" <> t
  _ -> t
