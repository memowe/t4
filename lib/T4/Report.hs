module T4.Report where

import T4.Data
import Util
import Data.Map (Map)
import Data.Time

categoryDurations :: [Clock] -> Map Category NominalDiffTime
categoryDurations = durations select
  where select (In t (Just c) _)  = ([c], getLocalTime t)
        select c                  = ([],  getLocalTime $ time c)

tagDurations :: [Clock] -> Map Tag NominalDiffTime
tagDurations = durations select
  where select (In t _ ts)  = (ts,  getLocalTime t)
        select c            = ([],  getLocalTime $ time c)
