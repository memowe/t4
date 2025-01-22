module T4.Data where

import Data.Char
import qualified Data.Text as T
import Data.List.Extra
import Data.Time
import Data.Aeson
import Data.Aeson.TH

type Cat    = String
type Tag    = String

data Config = Config  { allCats :: [Cat]
                      , allTags :: [Tag]
                      }
              deriving (Show, Eq)

$(deriveJSON defaultOptions ''Config)

newtype SimpleLocalTime = SLT {getLocalTime :: LocalTime}
                          deriving (Eq, Show, FromJSON)

simpleLocalTime :: Int -> Int -> Int -> Int -> Int -> Int -> SimpleLocalTime
simpleLocalTime y m d h i s =
  let day = fromGregorian (fromIntegral y) m d
      tod = TimeOfDay h i (fromIntegral s)
  in  SLT (LocalTime day tod)

instance ToJSON SimpleLocalTime where
  toJSON = String . T.pack . fmt . getLocalTime
    where fmt = formatTime defaultTimeLocale "%F %T"

data Clock  = In  { time  :: SimpleLocalTime
                  , cat   :: Cat
                  , tags  :: [Tag]
                  }
            | Out { time  :: SimpleLocalTime
                  }
              deriving (Show, Eq)

$(deriveJSON defaultOptions
  { constructorTagModifier  = map toLower
  , sumEncoding             = ObjectWithSingleField
  } ''Clock)

getDay :: Clock -> Day
getDay = localDay . getLocalTime . time

dayGroups :: [Clock] -> [[Clock]]
dayGroups = groupOn getDay
