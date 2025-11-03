module T4.Report where

import T4.Data
import Util
import Data.List
import qualified Data.Set as S
import Data.Map
import Data.Time

categoryDurations :: Clocks -> Map Category NominalDiffTime
categoryDurations = durations select
  where select (In t (Just c) _)  = (S.singleton c, getLocalTime t)
        select c                  = (S.empty,       getLocalTime $ time c)

tagDurations :: Clocks -> Map Tag NominalDiffTime
tagDurations = durations select
  where select (In t _ ts)  = (ts,      getLocalTime t)
        select c            = (S.empty, getLocalTime $ time c)

showDurMap :: Bool -> Bool -> Bool -> Map String NominalDiffTime -> [String]
showDurMap bySnd natural secs m =
  fmap (\(x, ndt) -> x ++ ": " ++ showDT durConf ndt) (ordPairs $ toList m)
  where ordPairs  = if bySnd    then sortOn snd else sortOn fst
        showDT    = if secs     then showDiffTime else showRoughDiffTime
        durConf   = if natural  then naturalDurationConfig
                                else manDurationConfig
