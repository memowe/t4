module T4.Report where

import T4.Data
import Util
import Data.List
import Data.Map
import Data.Time

categoryDurations :: [Clock] -> Map Category NominalDiffTime
categoryDurations = durations select
  where select (In t (Just c) _)  = ([c], getLocalTime t)
        select c                  = ([],  getLocalTime $ time c)

tagDurations :: [Clock] -> Map Tag NominalDiffTime
tagDurations = durations select
  where select (In t _ ts)  = (ts,  getLocalTime t)
        select c            = ([],  getLocalTime $ time c)

showDurMap :: Bool -> Bool -> Bool -> Map String NominalDiffTime -> [String]
showDurMap bySnd natural secs m =
  fmap (\(x, ndt) -> x ++ ": " ++ showDT durConf ndt) (ordPairs $ toList m)
  where ordPairs  = if bySnd    then sortOn snd else sortOn fst
        showDT    = if secs     then showDiffTime else showRoughDiffTime
        durConf   = if natural  then naturalDurationConfig
                                else manDurationConfig
