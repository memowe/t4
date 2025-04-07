module Util where

import Data.List
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time

durations :: (Ord a, Show a, Foldable f)
          => (entry -> ([a], LocalTime))
          -> f entry
          -> Map a NominalDiffTime
durations extract xs =
  let entries = sortOn snd $ extract <$> toList xs
      durs    = concat $ zipWith pairDuration entries (drop 1 entries)
  in  foldr (uncurry $ M.insertWith (+)) M.empty durs
  where pairDuration (ys, t1) (_, t2) = (, diffLocalTime t2 t1) <$> ys
