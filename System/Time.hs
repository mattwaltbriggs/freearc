{-# LANGUAGE DeriveDataTypeable #-}
module System.Time
  ( ClockTime(..)
  , CalendarTime(..)
  , TimeDiff(..)
  , Month(..)
  , Day(..)
  , getClockTime
  , toCalendarTime
  , toClockTime
  , addToClockTime
  , diffClockTimes
  , calendarTimeToString
  , formatCalendarTime
  ) where

import Data.Data (Data, Typeable)
import qualified Data.Time as DT
import Data.Time (UTCTime(..), NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime, fromGregorian, toGregorian, utctDay)
import System.IO.Unsafe (unsafePerformIO)

data ClockTime = TOD Integer Integer
  deriving (Eq, Ord, Show, Data, Typeable)

data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
           deriving (Eq, Ord, Show, Read, Data, Typeable, Enum)

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
         deriving (Eq, Ord, Show, Read, Data, Typeable)

data CalendarTime = CalendarTime
  { ctYear    :: Int
  , ctMonth   :: Month
  , ctDay     :: Int
  , ctHour    :: Int
  , ctMinute  :: Int
  , ctSecond  :: Int
  , ctPicosec :: Integer
  , ctWDay    :: Day
  , ctYDay    :: Int
  , ctTZName  :: String
  , ctTZ      :: Int
  , ctIsDST   :: Bool
  } deriving (Eq, Ord, Show, Data, Typeable)

data TimeDiff = TimeDiff
  { tdYear  :: Int
  , tdMonth :: Int
  , tdDay   :: Int
  , tdHour  :: Int
  , tdMin   :: Int
  , tdSec   :: Int
  , tdPicosec :: Integer
  } deriving (Eq, Ord, Show, Data, Typeable)

monthNamesList :: [Month]
monthNamesList = [January, February, March, April, May, June,
              July, August, September, October, November, December]

dayNamesList :: [Day]
dayNamesList = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]

getClockTime :: IO ClockTime
getClockTime = do
  now <- getCurrentTime
  let epoch = UTCTime (fromGregorian 1970 1 1) 0
      diff = diffUTCTime now epoch
      secs = floor diff :: Integer
  return (TOD secs 0)

toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime (TOD secs _pico) = do
  let diff = fromIntegral secs :: NominalDiffTime
      epoch = UTCTime (fromGregorian 1970 1 1) 0
      ut = addUTCTime diff epoch
      (y, m, d) = toGregorian (utctDay ut)
      mIdx = max 0 (min 11 (m - 1))
  return CalendarTime
    { ctYear = fromInteger y
    , ctMonth = monthNamesList !! mIdx
    , ctDay = d
    , ctHour = 0
    , ctMinute = 0
    , ctSecond = 0
    , ctPicosec = 0
    , ctWDay = Sunday
    , ctYDay = 0
    , ctTZName = ""
    , ctTZ = 0
    , ctIsDST = False
    }

toClockTime :: CalendarTime -> ClockTime
toClockTime _ = TOD 0 0

addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime td (TOD s p) = TOD (s + fromIntegral (tdSec td) + fromIntegral (tdHour td) * 3600 + fromIntegral (tdDay td) * 86400) p

diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes (TOD s1 _) (TOD s2 _) = TimeDiff 0 0 0 0 0 (fromIntegral (s1 - s2)) 0

calendarTimeToString :: CalendarTime -> String
calendarTimeToString = show

formatCalendarTime :: a -> String -> CalendarTime -> String
formatCalendarTime _ _ = show
