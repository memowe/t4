module T4.Data where

import Data.Char
import Data.Function
import Data.Maybe
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Data.Aeson
import Data.Aeson.TH

newtype SimpleLocalTime = SLT {getLocalTime :: LocalTime}
                          deriving (Eq, Show, Ord, FromJSON)

simpleLocalTime :: Int -> Int -> Int -> Int -> Int -> Int -> SimpleLocalTime
simpleLocalTime y m d h i s =
  let day = fromGregorian (fromIntegral y) m d
      tod = TimeOfDay h i (fromIntegral s)
  in  SLT (LocalTime day tod)

formatSLT :: String -> SimpleLocalTime -> String
formatSLT fmt = formatTime defaultTimeLocale fmt . getLocalTime

dateString :: SimpleLocalTime -> String
dateString = formatSLT "%F"

timeString :: SimpleLocalTime -> String
timeString = formatSLT "%T"

sltString :: SimpleLocalTime -> String
sltString = formatSLT "%F %T"

instance ToJSON SimpleLocalTime where
  toJSON = String . T.pack . sltString

type Category = String
type Tag      = String

data Clock  = In  { time      :: SimpleLocalTime
                  , category  :: Maybe Category
                  , tags      :: Set Tag
                  }
            | Out { time      :: SimpleLocalTime
                  }
              deriving (Show, Eq)

instance Ord Clock where
  (<=) = (<=) `on` time

$(deriveJSON defaultOptions
  { constructorTagModifier  = map toLower
  , sumEncoding             = ObjectWithSingleField
  } ''Clock)

isIn :: Clock -> Bool
isIn (In {}) = True; isIn _ = False

isOut :: Clock -> Bool
isOut (Out {}) = True; isOut _ = False

getDay :: Clock -> Day
getDay = localDay . getLocalTime . time

summary :: Clock -> String
summary (Out t)       = "OUT (" ++ sltString t ++ ")"
summary (In t mc ts)  = "IN (" ++ sltString t ++ ")" ++ catStr ++ tagsStr
  where catStr  = maybe "" ((" [" ++) . (++ "]")) mc
        tagsStr = concatMap (" #" ++) ts

type Clocks = Set Clock

dayGroups :: Clocks -> Map Day Clocks
dayGroups = foldr combine M.empty
  where combine = M.insertWith S.union <$> getDay <*> S.singleton

allCategories :: Clocks -> Set Category
allCategories = S.fromList . mapMaybe category . S.toList . S.filter isIn

allTags :: Clocks -> Set Tag
allTags = S.unions . S.map tags . S.filter isIn
