module Util where

import T4.Data (SimpleLocalTime(SLT))
import Data.List
import Data.Foldable
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time

durations :: (Ord a, Show a, Foldable f)
          => (entry -> (Set a, LocalTime))
          -> f entry
          -> Map a NominalDiffTime
durations extract xs =
  let entries = sortOn snd $ extract <$> toList xs
      durs    = zipWith pairDur entries (drop 1 entries)
  in  foldr (M.unionWith (+)) M.empty durs
  where pairDur (ys, t1) (_, t2) = M.fromSet (const $ diffLocalTime t2 t1) ys

newtype DurationConfig  = DurConf { units :: [DurationUnit] }
                                  deriving (Eq, Show)
data    DurationUnit    = DurUnit { long  :: String
                                  , short :: String
                                  , size  :: Integer
                                  } deriving (Eq, Show)

naturalDurationConfig :: DurationConfig
naturalDurationConfig = DurConf
  [ DurUnit "seconds" "s"   60
  , DurUnit "minutes" "mi"  60
  , DurUnit "hours"   "h"   24
  , DurUnit "days"    "d"   30
  , DurUnit "months"  "mo"  12
  , DurUnit "years"   "y"   (fromIntegral (maxBound :: Int))
  ]

manDurationConfig :: DurationConfig
manDurationConfig = DurConf
  [ DurUnit "seconds"     "s"   60
  , DurUnit "minutes"     "mi"  60
  , DurUnit "hours"       "h"   8
  , DurUnit "man-days"    "d"   20
  , DurUnit "man-months"  "mo"  12
  , DurUnit "years"       "y"   (fromIntegral (maxBound :: Int))
  ]

maxDuration :: DurationConfig -> NominalDiffTime
maxDuration = secondsToNominalDiffTime . fromIntegral
            . pred . product . map size . units

splitDiffTime :: DurationConfig -> NominalDiffTime -> [(Integer, DurationUnit)]
splitDiffTime dc time = fst $ foldl step ([], floor time) (units dc)
  where step (xs, i) du@(DurUnit _ _ s) =
          let (q, r) = i `quotRem` s
          in  ((r,du):xs, q)

showDiffTimeSplits :: [(Integer, DurationUnit)] -> String
showDiffTimeSplits = unwords . map showPair . onlySignificant
  where onlySignificant   = dropWhile $ (== 0) . fst
        showPair (i, du)  = show i ++ short du

showDiffTime :: DurationConfig -> NominalDiffTime -> String
showDiffTime dc = showDiffTimeSplits . splitDiffTime dc

showRoughDiffTime :: DurationConfig -> NominalDiffTime -> String
showRoughDiffTime dc = showDiffTimeSplits . init . splitDiffTime dc

getCurrentSLT :: IO SimpleLocalTime
getCurrentSLT = SLT . zonedTimeToLocalTime <$> getZonedTime
