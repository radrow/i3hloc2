{-# LANGUAGE OverloadedStrings #-}
module Hloc.Blocks.Date
  ( dateBlock
  , DateTimeFormat( Sec
                  , Minute
                  , Hour24
                  , Hour12
                  , Day
                  , Month
                  , Year
                  , WeekDayFull
                  , WeekDayShort
                  , AmPm
                  , CenturyYear
                  )
  ) where

import Hloc.Block
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import Data.List
import Data.String
import Data.Text(Text)
import qualified Data.Text as T

twoDigitFront :: Text -> Text
twoDigitFront t = case T.length t of
  0 -> "00"
  1 -> "0" <> t
  _ -> T.take 2 t

twoDigitBack :: Text -> Text
twoDigitBack t = case T.length t of
  0 -> "00"
  1 -> t <> "0"
  _ -> T.takeEnd 2 t

dayOfWeekStr :: Int -> Text
dayOfWeekStr n = case n of
  1 -> "Monday"
  2 -> "Tuesday"
  3 -> "Wednesday"
  4 -> "Thursday"
  5 -> "Friday"
  6 -> "Saturday"
  7 -> "Sunday"
  _ -> dayOfWeekStr (n `mod` 7 + 1)

data DateTimeFormat
  = Text !Text
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

instance IsString DateTimeFormat where
  fromString = Text . T.pack


renderDateTimeFormat :: LocalTime -> DateTimeFormat -> Text
renderDateTimeFormat t s =
  let timeOfDay = localTimeOfDay t
      date = localDay t
      (year, month, day) = toGregorian date
      hour = todHour timeOfDay
      minute = todMin timeOfDay
      sec = todSec timeOfDay
      wday = (\(_,_,n) -> dayOfWeekStr n) $ toWeekDate date
  in case s of
    Sec ->  twoDigitFront . T.pack . takeWhile (/='.') . show $ sec
    Minute ->  twoDigitFront . T.pack . show $ minute
    Hour24 ->  twoDigitFront . T.pack . show $ hour
    Hour12 ->  twoDigitFront . T.pack . show $ hour `mod` 12
    AmPm -> if hour < 12 then "am" else "pm"
    Day ->  twoDigitFront . T.pack . show $ day
    Month ->  twoDigitFront . T.pack . show $ month
    Year ->  T.pack . show $ year
    WeekDayFull ->  wday
    WeekDayShort -> T.take 3 wday
    CenturyYear -> twoDigitBack . T.pack . show $ year `mod` 100
    Text se -> se

data DateBlock = DateBlock
  { meta      :: !BlockMeta
  , format    :: ![DateTimeFormat]
  , delay     :: !Int
  , localtime :: !LocalTime
  }

getLocalTime :: IO LocalTime
getLocalTime = do
  utcTime <- getCurrentTime
  timeZone <- getCurrentTimeZone
  return $ utcToLocalTime timeZone utcTime

dateBlock :: BlockMeta -> [DateTimeFormat] -> IO Block
dateBlock m form = do
  time <- getLocalTime
  return $ Block $ DateBlock
    { meta = m
    , format = form
    , delay = calcOptimDelay form
    , localtime = time
    }

calcOptimDelay :: [DateTimeFormat] -> Int
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
    Text _ -> alot

instance IsBlock DateBlock where
  serialize b@DateBlock{localtime = lt, format = ft} =
    [(serializationBase b){ i3bFullText = T.concat (map (renderDateTimeFormat lt) ft) }]
  update c = do
    time <- getLocalTime
    return c{localtime = time}
  waitTime = delay
  getMeta = Just . meta
