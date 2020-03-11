module Hloc.Blocks.Date(dateBlock, DateTimeEntity(..)) where

import Hloc.Block
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import Data.List
import Data.String

twoDigitFront :: String -> String
twoDigitFront s | null s        = "00"
                | length s == 1 = '0':s
                | otherwise     = take 2 s

twoDigitBack :: String -> String
twoDigitBack s | null s        = "00"
               | length s == 1 = ['0', last s]
               | otherwise     = drop (length s - 2) s

dayOfWeekStr :: Int -> String
dayOfWeekStr n = case n of
  1 -> "Monday"
  2 -> "Tuesday"
  3 -> "Wednesday"
  4 -> "Thursday"
  5 -> "Friday"
  6 -> "Saturday"
  7 -> "Sunday"
  _ -> dayOfWeekStr (n `mod` 7 + 1)

data DateTimeEntity
  = Plaintext String
  | Sec
  | Minute
  | Hour24
  | Hour12
  | Day
  | Month
  | Year
  | WeekDayFull
  | WeekDayShort
  | AmPm
  | CenturyYear

instance IsString DateTimeEntity where
  fromString = Plaintext


renderDateTimeEntity :: LocalTime -> DateTimeEntity -> String
renderDateTimeEntity t s =
  let timeOfDay = localTimeOfDay t
      date = localDay t
      (year, month, day) = toGregorian date
      hour = todHour timeOfDay
      minute = todMin timeOfDay
      sec = todSec timeOfDay
      wday = (\(_,_,n) -> dayOfWeekStr n) $ toWeekDate date
  in case s of
    Sec ->  twoDigitFront . takeWhile (/='.') . show $ sec
    Minute ->  twoDigitFront . show $ minute
    Hour24 ->  twoDigitFront . show $ hour
    Hour12 ->  twoDigitFront . show $ hour `mod` 12
    AmPm -> if hour < 12 then "am" else "pm"
    Day ->  twoDigitFront . show $ day
    Month ->  twoDigitFront . show $ month
    Year ->  show year
    WeekDayFull ->  wday
    WeekDayShort -> take 3 wday
    CenturyYear -> twoDigitBack . show $ year `mod` 100
    Plaintext se -> se

data DateBlock = DateBlock
  { format :: [DateTimeEntity]
  , delay :: Int
  , localtime :: LocalTime
  }

getLocalTime :: IO LocalTime
getLocalTime = do
  utcTime <- getCurrentTime
  timeZone <- getCurrentTimeZone
  return $ utcToLocalTime timeZone utcTime

dateBlock :: [DateTimeEntity] -> IO DateBlock
dateBlock form = do
  time <- getLocalTime
  return $ DateBlock
    { format = form
    , delay = calcOptimDelay form
    , localtime = time
    }

calcOptimDelay :: [DateTimeEntity] -> Int
calcOptimDelay = foldl' (\prev e -> min prev (entityDelay e)) alot where
  alot :: Int
  alot = 30 * 1000 * 1000 -- 30 s
  entityDelay = \case
    Sec -> 500 * 1000 -- 0.5s
    Minute -> 5 * 10000 * 1000 -- 5s
    Hour24 -> 5 * 10000 * 1000 -- 5s
    Hour12 -> 5 * 10000 * 1000 -- 5s
    AmPm -> alot
    Day -> alot
    Month -> alot
    Year -> alot
    WeekDayFull -> alot
    WeekDayShort -> alot
    CenturyYear -> alot
    Plaintext _ -> alot

instance IsBlock DateBlock where
  serialize DateBlock{localtime = lt, format = ft} =
    pure defaultBlock { fullText = concatMap (renderDateTimeEntity lt) ft }
  update c = do
    time <- getLocalTime
    return c{localtime = time}
  waitTime = delay
