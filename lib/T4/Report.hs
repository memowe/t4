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

showDurMap :: Int -> Bool -> Bool -> Bool -> Map String NominalDiffTime -> [String]
showDurMap indent bySnd natural secs m =
  fmap formatLine (ordPairs $ toList m)
  where ordPairs  = if bySnd    then sortOn snd else sortOn fst
        showDT    = if secs     then showDiffTime else showRoughDiffTime
        durConf   = if natural  then naturalDurationConfig
                                else manDurationConfig
        formatLine (x, ndt) = replicate indent ' '
                              ++ x ++ ": " ++ showDT durConf ndt
