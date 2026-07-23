module System.Locale
  ( TimeLocale(..)
  , defaultTimeLocale
  , iso8601DateFormat
  ) where

data TimeLocale = TimeLocale
  { wDays  :: [String]
  , months :: [String]
  , amPm   :: (String, String)
  , dateTimeFmt :: String
  , dateFmt :: String
  , timeFmt :: String
  , time12Fmt :: String
  } deriving (Show)

defaultTimeLocale :: TimeLocale
defaultTimeLocale = TimeLocale
  { wDays = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]
  , months = ["January","February","March","April","May","June",
              "July","August","September","October","November","December"]
  , amPm = ("AM","PM")
  , dateTimeFmt = "%a %b %d %H:%M:%S %Y"
  , dateFmt = "%m/%d/%y"
  , timeFmt = "%H:%M:%S"
  , time12Fmt = "%I:%M:%S %p"
  }

iso8601DateFormat :: Maybe String -> String
iso8601DateFormat Nothing = "%Y-%m-%d"
iso8601DateFormat (Just fmt) = "%Y-%m-%dT" ++ fmt
